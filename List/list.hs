import Control.Exception
import Data.Char (isSpace)
import Prelude hiding ((++), (!!), all, and, any, break, concat, concatMap,
                       drop, dropWhile, elem, filter, foldl, foldr, init,
                       last, length, lines, map, minimum, maximum, notElem,
                       or, product, reverse, scanl, scanr, span, splitAt,
                       sum, take, takeWhile, unlines, until, unwords, unzip,
                       unzip3, words, zip, zip3, zipWith, zipWith3)
import qualified Prelude ((++), concat, concatMap, map, take)



length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs


