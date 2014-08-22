{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module NLP.Senna.Processor where

import Control.Applicative ((<$>), (<*>))
import NLP.Senna.Types
import NLP.Senna.Tags
import NLP.Senna.Util

class Processor a where
  process :: Context -> IO [a]

instance  Processor Word where
  process = getWords

instance Processor (Maybe POS) where
  process ctx =
    map convert <$> getPosTags ctx

instance Processor (Maybe NER, Position) where
  process ctx =
    deducePositions <$> getNerTags ctx

instance Processor (Maybe CHK, Position) where
  process ctx =
    deducePositions <$> getChkTags ctx

instance Processor (Maybe NER, Phrase) where
  process ctx =
    deducePhrases <$> getWords ctx <*> process ctx

instance Processor (Maybe CHK, Phrase) where
  process ctx =
    deducePhrases <$> getWords ctx <*> process ctx

instance Processor (NER, Position) where
  process ctx =
    dropNothing <$> process ctx

instance Processor (NER, Phrase) where
  process ctx =
    dropNothing <$> process ctx

instance Processor (CHK, Position) where
  process ctx =
    dropNothing <$> process ctx

instance Processor (CHK, Phrase) where
  process ctx =
    dropNothing <$> process ctx

instance  Processor [(Maybe SRL, Position)] where
  process ctx =
    map deducePositions <$> getSrlTags ctx

instance  Processor [(Maybe SRL, Phrase)] where
  process ctx =
    mapDeduce <$> getWords ctx <*> process ctx
    where
      mapDeduce words = map (deducePhrases words)

instance  Processor [(SRL, Position)] where
  process ctx =
    map dropNothing <$> process ctx

instance  Processor [(SRL, Phrase)] where
  process ctx =
    map dropNothing <$> process ctx

