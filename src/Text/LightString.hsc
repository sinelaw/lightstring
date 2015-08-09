{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances #-}

module Text.LightString where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.ForeignPtr
import Data.String
import Foreign.Marshal.Alloc (allocaBytes)

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

fromString str = withCString str c_from_c_string

foreign import ccall unsafe "lightstring.h get_length"
    c_get_length :: LightString -> IO CUInt

getLength ls = fromIntegral <$> c_get_length ls

foreign import ccall unsafe "lightstring.h write_c_string"
    c_write_c_string :: LightString -> CString -> IO ()


toString ls = do
    len <- (1+) <$> getLength ls
    allocaBytes len (\cstr -> c_write_c_string ls cstr >> peekCString cstr)
