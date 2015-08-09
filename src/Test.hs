module Main where
import Text.LightString
main = do
    Text.LightString.init 100 100
    ls <- fromString "hello there"
    ls2 <- fromString " foo"
    print ls
    toString ls >>= print
    toString ls2 >>= print
    ls3 <- c_concat ls ls2
    toString ls3 >>= print
    return ()
