module NLP.Senna.Util
  (
    getPosTags
  , getNerTags
  , getChkTags
  , getSrlTags
  , getTokens
  , deducePositions
  , deducePhrases
  , dropNothing
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Maybe (fromJust, isJust)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import NLP.Senna.Foreign
import NLP.Senna.Tags
import NLP.Senna.Types

-- | Convert a @Ptr@ to a 1D array.
ptrToArray1 :: (Storable a) => (a -> b) -> Int -> Ptr a -> IO [b]
ptrToArray1 f n ptr =
  map f <$> peekArray n ptr

-- | Convert a @Ptr Ptr@ to a 2D array.
ptrToArray2 :: (Storable a) => (a -> b) -> Int -> Int -> Ptr (Ptr a) -> IO [[b]]
ptrToArray2 f n m ptr =
  peekArray n ptr >>= mapM (ptrToArray1 f m)

-- | Convert a @Ptr CInt@ returned by one of the low-level
-- "NLP.Senna.Foreign.Functions" to an 'Enum' list.
getEnums :: (Enum a) => CSenna -> Ptr CInt -> IO [a]
getEnums ctx ptr = do
  len <- fromIntegral <$> c_senna_get_number_of_tokens ctx
  ptrToArray1 (toEnum . fromIntegral) len ptr

-- | Returns a list of low-level 'NLP.Senna.Foreign.Tags.CPosTag' tags.
getPosTags :: CSenna -> IO [CPosTag]
getPosTags ctx =
  c_senna_get_pos ctx >>= getEnums ctx

-- | Returns a list of low-level 'NLP.Senna.Foreign.Tags.CNerTag' tags.
getNerTags :: CSenna -> IO [CNerTag]
getNerTags ctx =
  c_senna_get_ner ctx >>= getEnums ctx

-- | Returns a list of low-level 'NLP.Senna.Foreign.Tags.CChkTag' tags.
getChkTags :: CSenna -> IO [CChkTag]
getChkTags ctx =
  c_senna_get_chk ctx >>= getEnums ctx

-- | Returns a list of low-level 'NLP.Senna.Foreign.Tags.CSrlTag' tag lists.
getSrlTags :: CSenna -> IO [[CSrlTag]]
getSrlTags ctx = do
   len <- fromIntegral <$> c_senna_get_number_of_tokens ctx
   vrb <- fromIntegral <$> c_senna_get_number_of_verbs ctx
   c_senna_get_srl ctx >>= ptrToArray2 (toEnum . fromIntegral) vrb len

-- | Returns a list of tokens.
getTokens :: CSenna -> IO [String]
getTokens ctx = do
  arr <- c_senna_get_tokens ctx
  len <- c_senna_get_number_of_tokens ctx
  ptrToArray1 peekCString (fromIntegral len) arr >>= sequence

-- | Counts the number of elements until a given item is reached.
countUntil :: (Eq a) => [a] -> a -> Int
countUntil s x =
  let (h,t) = span (x /=) s in
    if t /= []
      then length h + 1
      else length h

-- | Converts low-level 'NLP.Senna.Foreign.Tags.Spanning' tags to high-level
-- "NLP.Senna.Tags" and 'NLP.Senna.Types.Position' tuples.
--
-- This function is used to produce 'NLP.Senna.Processor.process' results.
deducePositions :: (Eq a, Convertable a b, Spanning a) => [a] -> [(b,Position)]
deducePositions x =
    deducePositions x 0
  where
    deducePositions [] _ = []
    deducePositions xs@(x:xs') i =
      case end x of
        Just y -> (convert x, (i,l)) : deducePositions (drop l xs) (i + l) where l = countUntil xs y
        Nothing -> (convert x, (i,1)) : deducePositions xs' (i + 1)

-- | Removes @(Nothing, _)@ tuples from list and applies 'Data.Maybe.fromJust'
-- to all remaining elements.
--
-- This function works on tuples because this is what
-- 'NLP.Senna.Processor.process' results look like.
dropNothing :: [(Maybe a,b)] -> [(a,b)]
dropNothing =
  map (first fromJust) . filter (isJust . fst)

-- | Takes a list of tokens acquired by 'getTokens' and converts
-- 'NLP.Senna.Types.Position' to 'NLP.Senna.Types.Phrase'.
--
-- This function works on tuples because this is what
-- 'NLP.Senna.Processor.process' results look like.
deducePhrases :: [String] -> [(a,Position)] -> [(a,Phrase)]
deducePhrases tokens [] = []
deducePhrases tokens (x:xs) =
  (v, take l tokens) : deducePhrases (drop l tokens) xs
  where
    (v, (_,l)) = x
