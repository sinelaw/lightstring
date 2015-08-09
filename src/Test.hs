module Main where
import Data.String(IsString(..))
import Data.Monoid((<>))
import Text.LightString
main = do
    Text.LightString.init 100 100
    let ls = fromString "hello there"
        ls2 = fromString " foo"
    print ls
    print $ toString ls
    print $ toString ls2
    print $ toString (ls <> ls2)
    print $ toString $ fromString "hi momo"
    return ()
