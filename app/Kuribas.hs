module Main where

import qualified Data.ByteString.Char8       as ByteString
import qualified Data.ByteString             as ByteString8
import qualified Data.HashMap.Strict         as Map
import qualified Data.List                   as List
import qualified Data.Set                    as Set
import qualified Data.Bits                   as Bit
import Data.ByteString.Char8 (ByteString)
import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.Word

main :: IO ()
main = do
    -- read the filename:
    args <- getArgs

    -- load the file:
    file <- ByteString.readFile (if null args then "shakespeare.txt" else head args)

    -- parse the file, find anagrams and output stats:
    outputStats (anagrams $ filter (not . ByteString.null) $ ByteString.splitWith (== '\n') file)

-- tokenise a line into words (all lowercase)
tokenise :: ByteString -> [ByteString]
tokenise = filter (not . ByteString.null)
         . ByteString.splitWith (not . lowercaseAlpha)
         . ByteString8.map toLower

lowercaseAlpha :: Char -> Bool
lowercaseAlpha c = c >= 'a' && c <= 'z'

toLower :: Word8 -> Word8
toLower c = if c > 64 && c < 91 then c+32 else c

-- take in lines of words, and output groups of anagram lines:
anagrams :: [ByteString] -> [[ByteString]]
anagrams = filter subSingleton
         . concatMap (splitCollision . dedupe)
         . filter subSingleton
         . Map.elems
         . Map.fromListWith (++)
         . map mappify'
  where
    subSingleton = not . null . drop 1
    mappify' orig = (key, [orig])
      where
        key = ByteString8.foldl' addChar 0 orig
        addChar :: Int -> Word8 -> Int
        addChar acc c
          |c > 96 && c < 123 =
             acc + Bit.shiftL 1 (fromIntegral c - 91) + 1
          | c > 64 && c < 91  =
              acc + Bit.shiftL 1 (fromIntegral c - 59) + 1
          | otherwise = acc

-- split collided sentences
splitCollision :: [ByteString] -> [[ByteString]]
splitCollision sentences =
    Map.elems $
    Map.fromListWith (++)
    [(ByteString.sort $
      ByteString.filter lowercaseAlpha $
      ByteString8.map toLower s,
      [s])
    | s <- sentences]

-- output stats:
outputStats :: [[ByteString]] -> IO ()
outputStats matches = do
    forM_ (List.sort matches) $ \ms -> do
        forM_ ms ByteString.putStrLn
        putStr "\n"
    putStrLn $ (show $ length matches) ++ " anagram sets found."

-- take key-value pairs and return values, deduping by keys
dedupe :: [ByteString] -> [ByteString]
dedupe lark = run lark Set.empty [] where
    run [] _ !out = out
    run (val:as) !s !out
      | Set.member key s = run as s out
      | otherwise = run as (Set.insert key s) (val:out)
      where key = List.sort $ tokenise val