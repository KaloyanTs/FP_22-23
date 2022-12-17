import System.IO  
import Control.Monad

main = do  
        let list = []
        handle <- openFile "prolog/test.pl" ReadMode
        contents <- hGetContents handle
        hClose handle   
        let singlewords = words contents
            list = f singlewords
        print list
        --todo

f :: [String] -> [String]
f = map read