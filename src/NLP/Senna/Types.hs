module NLP.Senna.Types
  (
    Word
  , Sentence
  , Position
  , Phrase
  , Context
  ) where

import NLP.Senna.Foreign (CSenna)

type Word = String

type Phrase = [Word]

type Sentence = String

type Position = (Int, Int)

type Context = CSenna
