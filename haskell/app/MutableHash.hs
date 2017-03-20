module Main where

import qualified Data.ByteString.Char8    as ByteString
import qualified Data.ByteString          as ByteString8
import qualified Data.HashTable.ST.Cuckoo as Hash
import qualified Data.List                as List
import qualified Data.Set                 as Set
import Control.Monad.ST.Strict (runST)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (ByteString)
import Control.Monad (forM_)
import System.Environment (getArgs)

--
-- We swap out the map for a mutable HashTable, although actually this
-- seems to be slower, not faster, than Main1.
--

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
tokenise = map (ByteString.filter isLetter) . filter (/= "") . ByteString.splitWith (== ' ') . ByteString8.map lowerCase
  where
    isLetter c = c >= 'a' && c <= 'z'
    lowerCase c = if c > 64 && c < 91 then c+32 else c

-- take in lines of words, and output groups of anagram lines:
anagrams :: [ByteString] -> [[ByteString]]
anagrams ls = runST $ do
    hash <- Hash.newSized 10000
    forM_ ls $ \orig -> do
        let !toks = tokenise orig
            !wkey = List.sort toks
            !key  = ByteString.sort $ ByteString.concat toks
        cur <- fmap (fromMaybe []) (Hash.lookup hash key)
        if toks /= []
            then Hash.insert hash key ((wkey,orig) : cur)
            else return ()
    Hash.foldM (\a (_,vs) -> return $ collapse a vs) [] hash
  where
    -- reduce into groups:
    collapse list ws = case dedupe ws of
        deduped@(_ : _ : _) -> deduped : list
        _ -> list

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