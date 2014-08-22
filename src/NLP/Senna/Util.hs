module NLP.Senna.Util
  (
    getPosTags
  , getNerTags
  , getChkTags
  , getSrlTags
  , getWords
  , deducePositions
  , deducePhrases
  , dropNothing
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.List (span)
import Data.Maybe (fromJust, isJust)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import NLP.Senna.Foreign
import NLP.Senna.Tags
import NLP.Senna.Types

-- Convert a Ptr to an array.
ptrToArray1 :: (Storable a) => (a -> b) -> Int -> Ptr a -> IO [b]
ptrToArray1 f n ptr =
  map f <$> peekArray n ptr

ptrToArray2 :: (Storable a) => (a -> b) -> Int -> Int -> Ptr (Ptr a) -> IO [[b]]
ptrToArray2 f n m ptr =
  peekArray n ptr >>= mapM (ptrToArray1 f m)

getEnums :: (Enum a) => CSenna -> Ptr CInt -> IO [a]
getEnums ctx ptr = do
  len <- c_senna_get_length ctx
  ptrToArray1 (toEnum . fromIntegral) (fromIntegral len) ptr

getPosTags :: CSenna -> IO [CPosTag]
getPosTags ctx =
  c_senna_get_pos ctx >>= getEnums ctx

getNerTags :: CSenna -> IO [CNerTag]
getNerTags ctx =
  c_senna_get_ner ctx >>= getEnums ctx

getChkTags :: CSenna -> IO [CChkTag]
getChkTags ctx =
  c_senna_get_chk ctx >>= getEnums ctx

getSrlTags :: CSenna -> IO [[CSrlTag]]
getSrlTags ctx = do
   len <- fromIntegral <$> c_senna_get_length ctx
   vrb <- fromIntegral <$> c_senna_get_verbs ctx
   c_senna_get_srl ctx >>= ptrToArray2 (toEnum . fromIntegral) vrb len

getWords :: CSenna -> IO [String]
getWords ctx = do
  arr <- c_senna_get_words ctx
  len <- c_senna_get_length ctx
  ptrToArray1 peekCString (fromIntegral len) arr >>= sequence

-- Counts the number of elements until we reach the given item in list.
countUntil :: (Eq a) => [a] -> a -> Int
countUntil s x =
  let (h,t) = span (x /=) s in 1 + length h

-- Convert Spanning foreign tags to Spans.
deducePositions :: (Eq a, Convertable a b, Spanning a) => [a] -> [(b,Position)]
deducePositions x =
    deducePositions x 0
  where
    deducePositions [] _ = []
    deducePositions xs@(x:xs') i =
      case end x of
        Just y -> (convert x, (i,l)) : deducePositions (drop l xs) (i + l) where l = countUntil xs y
        Nothing -> (convert x, (i,1)) : deducePositions xs' (i + 1)

-- Removes all '(Nothing, _)' tuples from the list and drops the Just from all
-- remaining elements.
dropNothing :: [(Maybe a,b)] -> [(a,b)]
dropNothing =
  map (first fromJust) . filter (isJust . fst)

deducePhrases :: [String] -> [(a,Position)] -> [(a,Phrase)]
deducePhrases words [] = []
deducePhrases words (x:xs) =
  (v, take l words) : deducePhrases (drop l words) xs
  where
    (v, (_,l)) = x
