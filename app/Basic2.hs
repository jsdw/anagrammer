module Main where

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString       as ByteString8
import qualified Data.HashMap.Strict   as Map
import qualified Data.List             as List
import qualified Data.Set              as Set
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')
import Data.ByteString.Char8 (ByteString)
import Control.Monad (forM_)
import System.Environment (getArgs)

main :: IO ()
main = do
    -- read the filename:
    args <- getArgs

    -- load the file:
    file <- ByteString.readFile (if null args then "shakespeare.txt" else head args)

    -- parse the file, find anagrams and output stats:
    outputStats (anagrams $ ByteString.splitWith (== '\n') file)

-- tokenise a line into words (all lowercase)
tokenise :: ByteString -> [ByteString]
tokenise = filter (/= "") . ByteString.splitWith notLowercaseAlpha . ByteString8.map lowerCase
  where
    notLowercaseAlpha  c = c < 'a' || c > 'z'
    lowerCase c = if c > 64 && c < 91 then c+32 else c

-- take in lines of words, and output groups of anagram lines:
anagrams :: [ByteString] -> [[ByteString]]
anagrams = filter ( (>1) . length )
         . map dedupe
         . Map.elems
         . Map.fromListWith (++)
         . map mappify'
  where
    mappify' orig = (key, [(wkey, orig)])
      where
        !toks = tokenise orig
        !wkey = List.sort toks
        !key  = ByteString.sort $ ByteString.concat toks

-- output stats:
outputStats :: [[ByteString]] -> IO ()
outputStats matches = do
    forM_ (List.sort matches) $ \ms -> do
        forM_ ms ByteString.putStrLn
        ByteString.putStr "\n"
    putStrLn $ (show $ length matches) ++ " anagram sets found."

-- take key-value pairs and return values, deduping by keys
dedupe :: Ord key => [(key,val)] -> [val]
dedupe lark = run lark Set.empty [] where
    run [] _ !out = out
    run ((key,val):as) !s !out = if Set.member key s
        then run as s out
        else run as (Set.insert key s) (val:out)