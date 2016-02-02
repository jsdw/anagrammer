{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.ByteString.Char8       as ByteString
import qualified Data.ByteString             as ByteString8
import qualified Data.HashMap.Strict         as Map
import qualified Data.List                   as List
import qualified Data.Set                    as Set
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Bits                   as Bit
import Data.ByteString.Char8 (ByteString)
import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.Word
import Data.Hashable

main :: IO ()
main = do
    -- read the filename:
    args <- getArgs

    -- load the file:
    file <- ByteString.readFile (if null args then "shakespeare.txt" else head args)

    -- parse the file, find anagrams and output stats:
    outputStats (anagrams $ filter (not . ByteString.null) $ ByteString.splitWith (== '\n') file)

newtype FreqCount = FreqCount { getFreqCount :: V.Vector Word8 }
                    deriving (Eq)

instance Hashable FreqCount where
    hashWithSalt s = V.foldl' (\h n -> (h `Bit.rotateR` 5) `Bit.xor` (fromIntegral n)) s . getFreqCount
    {-# INLINABLE hashWithSalt #-}

-- tokenise a line into words (all lowercase)
tokenise :: ByteString -> [ByteString]
tokenise = filter (/= "") . ByteString.splitWith notLowercaseAlpha . ByteString8.map lowerCase
  where
    notLowercaseAlpha  c = c < 'a' || c > 'z'
    lowerCase c = if c > 64 && c < 91 then c+32 else c

-- take in lines of words, and output groups of anagram lines:
anagrams :: [ByteString] -> [[ByteString]]
anagrams = filter subSingleton
         . map dedupe
         . filter subSingleton
         . Map.elems
         . Map.fromListWith (++)
         . map mappify'
  where
    subSingleton = not . null . drop 1
    mappify' orig = (FreqCount key, [orig])
      where
        -- wkey = List.sort $ tokenise orig
        !key  = V.modify populateVector (V.replicate 26 (0 :: Word8))
        populateVector mv = mapM_ (popVec mv) (ByteString8.unpack orig)
        popVec mv c | c > 96 && c < 123 = modvec 97
                    | c > 64 && c < 91  = modvec 65
                    | otherwise = return ()
            where modvec x = MV.unsafeModify mv succ (fromIntegral c - x)

-- output stats:
outputStats :: [[ByteString]] -> IO ()
outputStats matches = do
    forM_ (List.sort matches) $ \ms -> do
        forM_ ms ByteString.putStrLn
        ByteString.putStr "\n"
    putStrLn $ (show $ length matches) ++ " anagram sets found."

-- take key-value pairs and return values, deduping by keys
dedupe :: [ByteString] -> [ByteString]
dedupe lark = run lark Set.empty [] where
    run [] _ !out = out
    run (val:as) !s !out = let key = List.sort $ tokenise val
                           in  if Set.member key s
                                   then run as s out
                                   else run as (Set.insert key s) (val:out)
