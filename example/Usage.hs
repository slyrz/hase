module Main where

import NLP.Senna

-- Sentences from random Wikipedia articles.
sentences =
  [ "Queen City was laid out by Doctor George W. Wilson in May 1867."
  , "Bethel moved the ship to Bimini, using it as a warehouse for alcohol during the era of Prohibition."
  , "French language is a minor language in Jordan."
  , "The inhabitants belonged to Mohammad Khanlu tribe."
  , "The Hornet's Nest is a 2014 American documentary film about the Afghanistan war."
  , "The Zeze is a river in western Albania."
  ]

printTags ctx sentence = do
  tokenize ctx sentence

  putStrLn sentence
  print =<< (process ctx :: IO [Token])
  print =<< (process ctx :: IO [Maybe POS])
  print =<< (process ctx :: IO [(NER, Phrase)])
  print =<< (process ctx :: IO [(CHK, Phrase)])
  print =<< (process ctx :: IO [[(SRL, Phrase)]])

main =
  -- We use the explicit 'withContextFromPath' here so you can run this
  -- example without installing the package. Once you decide to install
  -- hase, just use 'withContext' and hase will load the data from your
  -- cabal install directory.
  withContextFromPath "foreign/senna" $ \ctx -> mapM_ (printTags ctx) sentences
