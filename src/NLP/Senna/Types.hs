-- | This module contains all type definitions used by "NLP.Senna".
module NLP.Senna.Types
  (
    Word
  , Sentence
  , Position
  , Phrase
  , Context
  ) where

import NLP.Senna.Foreign (CSenna)

-- | Context is a wrapper around the C library state passed to all
-- underlying C functions.
type Context = CSenna

-- | Word is a single word.
type Word = String

-- | Sentence is a single sentence.
type Sentence = String

-- | Phrase is a list of consecutive words. It's used for tags which may span
-- multiple words.
type Phrase = [Word]

-- | Position is a tuple of (index, length) Ints. It's used to represent
-- the position of tags in the word array of a tokenized sentence.
type Position = (Int, Int)
