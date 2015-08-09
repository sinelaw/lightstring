{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Text.LightString where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr


#include "lightstring.h"

foreign import ccall unsafe "lightstring.h init"
    c_init :: CInt -> CInt -> IO ()

init bytes strings_count = c_init (fromIntegral bytes) (fromIntegral strings_count)

foreign import ccall unsafe "lightstring.h fini"
    fini :: IO ()


