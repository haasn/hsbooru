{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module HsBooru.Xapian.FFI
    ( XapianM
    , runXM_
    -- * Document
    , Document
    , ValueNumber
    , newDoc
    , addValStr
    , addValDouble
    , addTerm
    -- * Database
    , XapianDB
    , localDB
    , memoryDB
    , DocId
    , addDocument
    , addSynonym
    , txBegin
    , txCommit
    ) where

import Control.Arrow
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text.Foreign as T (withCStringLen)

import Foreign
import Foreign.C hiding (withCStringLen)
import System.IO.Unsafe (unsafePerformIO)

-- | Xapian calls are hidden behind a newtype because access to xapian
-- is not thread safe. This allows the library to ensure correct locking.
newtype XapianM a = XapianM (ExceptT String IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runXM_ :: XapianM a -> IO (Either String a)
runXM_ (XapianM a) = withLock $ runExceptT a
    where withLock = bracket (takeMVar lock) (putMVar lock) . const

lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

withCStringLen :: Text -> ContT r IO (CString, CSize)
withCStringLen = fmap (id *** fromIntegral) . ContT . T.withCStringLen

-- Constants

foreign import ccall unsafe "db_create_or_open"
    db_create_or_open :: CInt

foreign import ccall unsafe "db_backend_inmemory"
    db_backend_inmemory :: CInt

-- Documents and related functions

data CXapianDoc
type Document = ForeignPtr CXapianDoc

foreign import ccall unsafe "doc_new"
    cx_doc_new :: IO (Ptr CXapianDoc)

foreign import ccall unsafe "&doc_delete"
    cx_doc_del :: FunPtr (Ptr CXapianDoc -> IO ())

newDoc :: XapianM Document
newDoc = liftIO $ cx_doc_new >>= newForeignPtr cx_doc_del

type ValueNumber = CUInt

foreign import ccall unsafe "doc_add_val_str"
    cx_doc_add_val_str :: Ptr CXapianDoc -> CUInt -> CString -> CSize -> IO ()

addValStr :: Document -> ValueNumber -> Text -> XapianM ()
addValStr doc valueno val = liftIO . evalContT $ do
    pdoc <- ContT $ withForeignPtr doc
    (pval, len) <- withCStringLen val
    liftIO $ cx_doc_add_val_str pdoc valueno pval len

foreign import ccall unsafe "doc_add_val_dbl"
    cx_doc_add_val_dbl :: Ptr CXapianDoc -> CUInt -> CDouble -> IO ()

addValDouble :: Document -> ValueNumber -> Double -> XapianM ()
addValDouble doc valueno val = liftIO . withForeignPtr doc $ \pdoc -> do
    cx_doc_add_val_dbl pdoc valueno (CDouble val)

foreign import ccall unsafe "doc_add_term"
    cx_doc_add_term :: Ptr CXapianDoc -> CString -> CSize -> IO ()

addTerm :: Document -> Text -> XapianM ()
addTerm doc tag = liftIO . evalContT $ do
    pdoc <- ContT $ withForeignPtr doc
    (ptag, len) <- withCStringLen tag
    liftIO $ cx_doc_add_term pdoc ptag len

-- Database and related functions

data CXapianDB
type XapianDB = ForeignPtr CXapianDB
type Flags = CInt

wrapError :: Eq a => a -> (Ptr CString -> IO a) -> (a -> IO b)
          -> ContT r IO (Either String b)
wrapError badVal action good = do
    err <- ContT alloca
    res <- liftIO $ action err
    liftIO $ if res == badVal
        then peek err >>= (\e -> Left <$> peekCString e <* free e)
        else Right <$> good res

foreign import ccall unsafe "db_open"
    cx_db_open :: CString -> Flags -> Ptr CString -> IO (Ptr CXapianDB)

foreign import ccall unsafe "&db_delete"
    cx_db_delete :: FunPtr (Ptr CXapianDB -> IO ())

openDB :: Flags -> FilePath -> IO (Either String XapianDB)
openDB flags path = evalContT $ do
    cst <- ContT $ withCString path
    wrapError nullPtr (cx_db_open cst flags) (newForeignPtr cx_db_delete)

localDB :: FilePath -> IO (Either String XapianDB)
localDB = openDB db_create_or_open

memoryDB :: IO (Either String XapianDB)
memoryDB = openDB db_backend_inmemory ""

foreign import ccall unsafe "db_add_doc"
    cx_db_add_doc :: Ptr CXapianDB -> Ptr CXapianDoc -> Ptr CString -> IO CUInt

type DocId = CUInt

addDocument :: XapianDB -> Document -> XapianM DocId
addDocument db doc = XapianM . ExceptT . evalContT $ do
    pdb  <- ContT $ withForeignPtr db
    pdoc <- ContT $ withForeignPtr doc
    wrapError 0 (cx_db_add_doc pdb pdoc) return

foreign import ccall unsafe "db_add_synonym"
    cx_db_add_synonym :: Ptr CXapianDB -> CString -> CSize -> CString -> CSize
                      -> Ptr CString -> IO CUInt

addSynonym :: XapianDB -> Text -> Text -> XapianM ()
addSynonym db tag1 tag2 = XapianM . ExceptT . evalContT $ do
    pdb <- ContT $ withForeignPtr db
    (ptag1, len1) <- withCStringLen tag1
    (ptag2, len2) <- withCStringLen tag2
    wrapError 0 (cx_db_add_synonym pdb ptag1 len1 ptag2 len2) (\_ -> pure ())

foreign import ccall unsafe "db_tx_begin"
    cx_db_tx_begin :: Ptr CXapianDB -> Ptr CString -> IO CUInt

foreign import ccall unsafe "db_tx_commit"
    cx_db_tx_commit :: Ptr CXapianDB -> Ptr CString -> IO CUInt

txBegin :: XapianDB -> XapianM ()
txBegin db = XapianM . ExceptT . evalContT $ do
    pdb <- ContT $ withForeignPtr db
    wrapError 0 (cx_db_tx_begin pdb) (\_ -> pure ())

txCommit :: XapianDB -> XapianM ()
txCommit db = XapianM . ExceptT . evalContT $ do
    pdb <- ContT $ withForeignPtr db
    wrapError 0 (cx_db_tx_commit pdb) (\_ -> pure ())
