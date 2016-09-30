{-# LANGUAGE TemplateHaskell, RecordWildCards, PatternSynonyms, ViewPatterns,
             LambdaCase #-}

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Data.Time
import Data.SafeCopy
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Data.Char (isNumber, toLower)
import Data.List (find, isPrefixOf)
import Data.Maybe (maybeToList)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Network.Curl ( CurlCode(..), CurlOption(..)
                    , CurlResponse_(..), curlGetResponse_ )

import Search.Xapian
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), takeFileName)

import qualified Search.Xapian.Query.Combinators as Q

import Text.Read (readMaybe)
import Text.HTML.Scalpel hiding (curlOpts)

-- ^ Global configuration

-- Directories
dbDir       = "/z/booru/"
imageDir    = dbDir </> "images"
xapianDir   = dbDir </> "xapian"

-- Used when downloading images
curlOpts    = [CurlFollowLocation True]

-- ^ Types and instances

-- This is defined via SafeCopy so I can migrate to a newer version
-- in the future

data FileInfo = FileInfo { fileName :: !T.Text } deriving Show

deriveSafeCopy 0 'base ''FileInfo

-- These types are only used within the scraper, and don't need to be stored

data Post = Post
    { siteID   :: !Int
    , booru    :: !String
    , rating   :: !Rating
    , uploader :: !String
    , score    :: !Int
    , source   :: !(Maybe String)
    , fileURL  :: !String
    , tags     :: ![Tag]
    } deriving Show

data Rating = Safe | Questionable | Explicit
    deriving (Show, Read)

data Tag = Tag
    { tagType :: !TagType
    , tagName :: !String
    } deriving Show

data TagType = General | Character | Artist | Work
    deriving (Show, Read)

data SiteScraper = SiteScraper
    { siteName    :: String
    , scrapeID    :: Int -> IO (Maybe Post)
    , scrapeMax   :: IO (Maybe Int)
    }

-- ^ Scraper definitions

-- Helper functions
extractID :: URL -> Maybe Int
extractID = readMaybe . dropWhile (not . isNumber)

getRating :: String -> Maybe Rating
getRating = chomp "Rating: " >=> readMaybe

chomp :: String -> String -> Maybe String
chomp [] ys = Just ys
chomp _  [] = Nothing
chomp (x:xs) (y:ys)
    | x == y    = chomp xs ys
    | otherwise = Nothing

-- More useful scraper primitives
textMaybe :: Selectable s => s -> Scraper String (Maybe String)
textMaybe s = Just <$> text s <|> pure Nothing

textFirst :: Selectable s => (String -> Maybe a) -> s -> Scraper String a
textFirst f s = texts s >>= go
    where go [] = mzero
          go ((f -> Just x):xs) = pure x
          go (_:xs) = go xs

textRead :: (Selectable s, Read a) => s -> Scraper String a
textRead = maybe mzero pure . readMaybe <=< text

(@^=) :: String -> String -> AttributePredicate
k @^= v = match $ \k' v' -> k == k' && v `isPrefixOf` v'

-- Gelbooru scraper
gelbooru :: SiteScraper
gelbooru = SiteScraper{..}
    where siteName = "gelbooru"

          scrapeMax = scrapeURL rssUrl $ do
                newestID <- fmap extractID . text $ "item" // "link"
                case newestID of
                    Just id -> return id
                    Nothing -> mzero

          -- NOTE: gelbooru has a programmable API which would make
          -- most of this easier, but unfortunately it has some limitations
          -- so we're stuck with manually parsing the website if we want
          -- all of the information..
          scrapeID postID = scrapeURL (postUrl ++ show postID) $ do
                let siteID  = postID
                    booru = siteName

                -- Extract the stats by parsing the list on the left
                let stats = "div" @: ["id" @= "stats"] // "li"
                uploader <- text $ stats // "a" -- luckily the first link
                rating   <- textFirst getRating stats
                score    <- textRead  $ stats // "span" @: ["id" @^= "psc"]
                source   <- textMaybe $ stats // "a" @: ["rel" @= "nofllow"]

                -- This is the easiest way to match the "Original image" link
                fileURL <- attr "href" $ "div" // "li" // "a"
                                       @: ["style" @= "font-weight: bold;"]

                let tag = do Just tc <- getTagType <$> attr "class" "li"
                             [_, tn] <- texts $ "li" // "a"
                             return Tag { tagType = tc, tagName = sanitize tn }

                    getTagType "tag-type-general"   = Just General
                    getTagType "tag-type-character" = Just Character
                    getTagType "tag-type-artist"    = Just Artist
                    getTagType "tag-type-copyright" = Just Work
                    getTagType _ = Nothing

                    sanitize = map (\case ' ' -> '_'; c -> c)

                tags <- chroots ("ul" @: ["id" @= "tag-sidebar"] // "li") tag

                return Post{..}

          rssUrl  = "http://gelbooru.com/index.php?page=cooliris"
          postUrl = "http://gelbooru.com/index.php?page=post&s=view&id="

-- ^ Image downloading and storing

fetchImage :: URL -> ExceptT String IO FileInfo
fetchImage url = do
    -- Since we're working with direct links, we can extract the "filename"
    -- from the URL as if it were a path. Kludgy, but works..
    let fileName = takeFileName url
        filePath = imageDir </> fileName
        fileInfo = FileInfo { fileName = T.pack fileName }

    e   <- liftIO $ doesFileExist filePath
    unless e $ download url filePath

    return fileInfo

download :: URL -> FilePath -> ExceptT String IO ()
download url path = do
    res <- liftIO $ curlGetResponse_ url curlOpts
    case res :: CurlResponse_ [(String, String)] LBS.ByteString of
        CurlResponse { respCurlCode = CurlOK, respBody = img }
            -> liftIO $ LBS.writeFile path img

        CurlResponse { respCurlCode = err }
            -> throwE $ "Error downloading " ++ url ++ ": " ++ show err


-- ^ Xapian database integration

addTag :: Document -> String -> XapianM ()
addTag doc str = addTerm (UTF8.fromString $ sanitize str) doc
    where sanitize (p:str) = p : map toLower str

showLower :: Show a => a -> String
showLower = map toLower . show

xapianStore :: WritableDatabase db => db -> Post -> FileInfo -> XapianM ()
xapianStore db Post{..} fi = do
    doc <- emptyDocument
    setData (runPut $ safePut fi) doc

    addTag doc $ siteIDPrefix   : show siteID
    addTag doc $ booruPrefix    : booru
    addTag doc $ ratingPrefix   : showLower rating
    addTag doc $ uploaderPrefix : uploader
    addTag doc $ scorePrefix    : show score
    addTag doc $ urlPrefix      : fileURL
    forM_ source $ \s       -> addTag doc (sourcePrefix : s)
    forM_ tags   $ \Tag{..} -> addTag doc (tagPrefix tagType : tagName)

    addDocument db doc
    commit db

xapianRW :: ExceptT String IO ReadWriteDB
xapianRW = requireRight $ openReadWrite CreateOrOpen xapianDir

parseQuery :: String -> Query
parseQuery str = Q.queryAll [ parseTerm t | t <- words str ]

xapianRO :: ExceptT String IO ReadOnlyDB
xapianRO = requireRight $ openReadOnly xapianDir

runXM :: XapianM a -> ExceptT String IO a
runXM = liftIO . runXapian

xapianSearch :: ReadableDatabase db => db -> Query -> QueryRange -> XapianM [Document]
xapianSearch db query range = snd <$> search db query range

allResults :: QueryRange
allResults = QueryRange 0 maxBound

-- ^ Front-end routines for the primitives above

processID :: WritableDatabase db => db -> SiteScraper -> Int -> ExceptT String IO Post
processID db SiteScraper{..} id = do
    post <- requireJust "Scraper failed to scrape post" $ scrapeID id
    file <- fetchImage (fileURL post)
    runXM $ xapianStore db post file
    return post

listFiles :: ReadableDatabase db => db -> Query -> ExceptT String IO [FileInfo]
listFiles db q = do
    ds <- runXM $ xapianSearch db q allResults >>= mapM getData
    forM ds $ ExceptT . pure . runGet safeGet

-- Misc helpers

requireJust :: String -> IO (Maybe a) -> ExceptT String IO a
requireJust err = maybe (throwE err) pure <=< liftIO

requireRight :: Show e => IO (Either e a) -> ExceptT String IO a
requireRight = either (throwE . show) pure <=< liftIO

-- Prefix mapping

tagPrefix :: TagType -> Char
tagPrefix General   = 'G'
tagPrefix Character = 'C'
tagPrefix Artist    = 'A'
tagPrefix Work      = 'W'

siteIDPrefix   = 'I'
booruPrefix    = 'B'
ratingPrefix   = 'R'
uploaderPrefix = 'U'
scorePrefix    = 'S'
sourcePrefix   = 'R'
urlPrefix      = 'L'

prefixFriendly :: [(Char, [String])]
prefixFriendly =
    [ (siteIDPrefix,   ["id:"])
    , (booruPrefix,    ["booru:", "scraper:", "site:"])
    , (ratingPrefix,   ["rating:"])
    , (uploaderPrefix, ["uploader:", "creator:"])
    , (scorePrefix,    ["score:"])
    , (sourcePrefix,   ["source:"])
    , (urlPrefix,      ["url:"])

    , (tagPrefix Character, ["character:", "char:"])
    , (tagPrefix Artist,    ["artist:", "author:"])
    , (tagPrefix Work,      ["work:", "copyright:"])

    -- This must be the last entry
    , (tagPrefix General, [""])
    ]

parseTerm :: String -> String
parseTerm str = head [ p:r | (p, ls) <- prefixFriendly
                           , l <- ls
                           , r <- maybeToList $ chomp l str
                     ]

-- TODO: thumbnail generation (?), frontend
