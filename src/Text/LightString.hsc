{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances #-}

module Text.LightString where

import Prelude hiding (length)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.ForeignPtr
import Data.String
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO.Unsafe (unsafePerformIO)

#include "lightstring.h"

data LightString_
type LightString = Ptr LightString_

foreign import ccall unsafe "lightstring.h init"
    c_init :: CUInt -> CUInt -> IO ()

init bytes strings_count = c_init (fromIntegral bytes) (fromIntegral strings_count)

foreign import ccall unsafe "lightstring.h fini"
    fini :: IO ()


foreign import ccall unsafe "lightstring.h from_c_string"
    c_from_c_string :: CString -> IO LightString

lsFromString str = unsafePerformIO $ withCString str c_from_c_string

instance IsString LightString where
    fromString = lsFromString


foreign import ccall unsafe "lightstring.h get_length"
    c_get_length :: LightString -> IO CUInt

getLength ls = fromIntegral <$> c_get_length ls

length ls = unsafePerformIO $ getLength ls

foreign import ccall unsafe "lightstring.h write_c_string"
    c_write_c_string :: LightString -> CString -> IO ()

toString ls = unsafePerformIO $ do
    len <- (1+) <$> getLength ls
    allocaBytes len (\cstr -> c_write_c_string ls cstr >> peekCString cstr)

foreign import ccall unsafe "lightstring.h concat"
    c_concat :: LightString -> LightString -> IO LightString

instance Monoid LightString where
    mempty = lsFromString ""
    mappend x y = unsafePerformIO $ c_concat x y

