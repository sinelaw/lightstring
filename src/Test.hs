module Main where
import Text.LightString
main = do
    Text.LightString.init 100 100
    ls <- fromString "hello there"
    print ls
    str <- toString ls
    putStrLn str
    return ()
