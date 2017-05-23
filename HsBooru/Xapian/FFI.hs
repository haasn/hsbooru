{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module HsBooru.Xapian.FFI
    ( XapianM
    , runXM
    -- * Document
    , Document
    , ValueNumber
    , newDoc
    , addValStr
    , addValDouble
    , addTerm
    -- * Database
    , XapianDB
    , xapianDB
    , addDocument
    , commit
    ) where

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text.Foreign as T (withCStringLen)

import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import HsBooru.Util

-- | Xapian calls are hidden behind a newtype because access to xapian
-- is not thread safe. This allows the library to ensure correct locking.
newtype XapianM a = XapianM { runXapianM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

runXM :: XapianM a -> ExceptT String IO a
runXM = ioCatch . withLock . runXapianM
    where withLock = bracket (takeMVar lock) (putMVar lock) . const

lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

-- Documents and related functions

data CXapianDoc
type Document = ForeignPtr CXapianDoc

foreign import ccall unsafe "doc_new"
    cx_doc_new :: IO (Ptr CXapianDoc)

foreign import ccall unsafe "&doc_delete"
    cx_doc_del :: FunPtr (Ptr CXapianDoc -> IO ())

newDoc :: XapianM Document
newDoc = io $ cx_doc_new >>= newForeignPtr cx_doc_del

type ValueNumber = CUInt

foreign import ccall unsafe "doc_add_val_str"
    cx_doc_add_val_str :: Ptr CXapianDoc -> CUInt -> CString -> CSize -> IO ()

addValStr :: Document -> ValueNumber -> Text -> XapianM ()
addValStr doc valueno val = io . evalContT $ do
    pdoc <- ContT $ withForeignPtr doc
    (pval, len) <- ContT $ T.withCStringLen val
    io $ cx_doc_add_val_str pdoc valueno pval (fromIntegral len)

foreign import ccall unsafe "doc_add_val_dbl"
    cx_doc_add_val_dbl :: Ptr CXapianDoc -> CUInt -> CDouble -> IO ()

addValDouble :: Document -> ValueNumber -> Double -> XapianM ()
addValDouble doc valueno val = io . withForeignPtr doc $ \pdoc -> do
    cx_doc_add_val_dbl pdoc valueno (CDouble val)

foreign import ccall unsafe "doc_add_term"
    cx_doc_add_term :: Ptr CXapianDoc -> CString -> CSize -> IO ()

addTerm :: Document -> Text -> XapianM ()
addTerm doc tag = io . evalContT $ do
    pdoc <- ContT $ withForeignPtr doc
    (ptag, len) <- ContT $ T.withCStringLen tag
    io $ cx_doc_add_term pdoc ptag (fromIntegral len)

-- Database and related functions

data CXapianDB
type XapianDB = ForeignPtr CXapianDB

foreign import ccall unsafe "db_open"
    cx_db_open :: CString -> Ptr CString -> IO (Ptr CXapianDB)

foreign import ccall unsafe "&db_delete"
    cx_db_delete :: FunPtr (Ptr CXapianDB -> IO ())

xapianDB :: FilePath -> ExceptT String IO XapianDB
xapianDB path = ExceptT . evalContT $ do
    cst <- ContT $ withCString path
    err <- ContT alloca
    res <- io $ cx_db_open cst err
    io $ if res == nullPtr
                then Left  <$> (peekCString =<< peek err)
                else Right <$> newForeignPtr cx_db_delete res

foreign import ccall unsafe "db_add_doc"
    cx_db_add_doc :: Ptr CXapianDB -> Ptr CXapianDoc -> IO ()

addDocument :: XapianDB -> Document -> XapianM ()
addDocument db doc = io . evalContT $ do
    pdb  <- ContT $ withForeignPtr db
    pdoc <- ContT $ withForeignPtr doc
    io $ cx_db_add_doc pdb pdoc

foreign import ccall unsafe "db_commit"
    cx_db_commit :: Ptr CXapianDB -> IO ()

commit :: XapianDB -> XapianM ()
commit db = io $ withForeignPtr db cx_db_commit
