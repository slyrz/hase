-- | This module contains all type definitions used by "NLP.Senna".
module NLP.Senna.Types
  (
    Token
  , Sentence
  , Position
  , Phrase
  , Context
  ) where

import NLP.Senna.Foreign (CSenna)

-- | Context is a wrapper around the C library state passed to all
-- underlying C functions.
type Context = CSenna

-- | Token is a word or a symbol.
type Token = String

-- | Sentence is a single sentence.
type Sentence = String

-- | Phrase is a list of consecutive tokens. It's used for tags which may span
-- multiple tokens, like the 'PER' tag of 'NLP.Senna.Tags.NER' might span
-- /Simon Peyton Jones/ in the sentence \"Simon Peyton Jones is a major
-- contributor to the design of the Haskell programming language.\",
-- resulting in the phrase @[\"Simon\", \"Peyton\", \"Jones\"]@ tagged as 'PER'.
type Phrase = [Token]

-- | Position is a tuple of (index, length) Ints. It's used to represent
-- the /token-level/ position of tags.
type Position = (Int, Int)
