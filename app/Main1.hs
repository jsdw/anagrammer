module Main where

import qualified Data.ByteString.Char8            as ByteString
import qualified Data.Map                         as Map
import qualified Data.List                        as List
import qualified Data.Set                         as Set
import Data.Maybe (fromMaybe)
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.ByteString.Char8 (ByteString)
import Control.Monad (forM_)
import System.Environment (getArgs, withArgs)

mainWith :: String -> IO ()
mainWith s = withArgs [s] main

main :: IO ()
main = do
    -- read the filename:
    (fileName : _) <- getArgs

    -- load the file:
    file <- ByteString.readFile fileName

    -- parse the file, find anagrams and output stats:
    outputStats (anagrams $ ByteString.splitWith (== '\n') file)

-- tokenise a line into words (all lowercase)
tokenise :: ByteString -> [ByteString]
tokenise = fmap (ByteString.map lowerCase) . filter (/= "") . ByteString.splitWith notAlpha
  where
    notAlpha  c = let nc = ord c in nc < 65 || nc > 122 || (nc > 90 && nc < 97)
    lowerCase c = let nc = ord c in if nc > 64 && nc < 91 then toEnum (nc+32) else c

-- take in lines of words, and output groups of anagram lines:
anagrams :: [ByteString] -> [[ByteString]]
anagrams = foldl' collapse [] . foldl' mappify Map.empty . fmap (\b -> (tokenise b, b))
  where
    -- step 1: collect into a map:
    mappify !m ([],_)      = m
    mappify !m (toks,orig) =
        let !wkey = List.sort toks
            !key  = ByteString.sort $ ByteString.concat toks
            !cur  = fromMaybe [] (Map.lookup key m)
        in  Map.insert key ((wkey,orig) : cur) m

    -- step 2: reduce into groups:
    collapse list ws = case dedupe ws of
        deduped@(_ : _ : _) -> deduped : list
        _ -> list

-- output stats:
outputStats :: [[ByteString]] -> IO ()
outputStats matches = do
    forM_ matches $ \ms -> do
        forM_ ms ByteString.putStrLn
        ByteString.putStrLn "\n"

-- take key-value pairs and return values, deduping by keys
dedupe :: Ord key => [(key,val)] -> [val]
dedupe lark = run lark Set.empty [] where
    run [] _ !out = out
    run ((key,val):as) !s !out = if Set.member key s
        then run as s out
        else run as (Set.insert key s) (val:out)