module NLP.Senna.Functions
  (
    createContext
  , createContextFromPath
  , freeContext
  , withContext
  , withContextFromPath
  , tokenize
  ) where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Foreign
import Foreign.C.String (newCString)
import NLP.Senna.Foreign
import NLP.Senna.Types
import NLP.Senna.Util
import Paths_hase
import System.FilePath

-- The directory where Cabal dropped the data-files.
dataDir :: IO String
dataDir =
  (</> "foreign/senna") <$> getDataDir

-- | Creates a new 'Context' by loading all data from Cabal's
-- install path.
--
-- The returned handle should be released with 'freeContext'.
createContext :: IO Context
createContext =
  dataDir >>= createContextFromPath

-- | Creates a new 'Context' by loading all data from a given
-- directory.
--
-- The returned handle should be released with 'freeContext'.
createContextFromPath :: FilePath -> IO Context
createContextFromPath path =
  newCString (addTrailingPathSeparator path) >>= c_senna_new

-- | Releases all memory used by 'Context', thus renders 'Context'
-- invalid.
freeContext :: Context -> IO ()
freeContext =
  c_senna_free

-- | Provides a bracket which automatically creates and frees
-- a 'Context' passed to a user-defined function.
withContext :: (Context -> IO a) -> IO a
withContext =
  bracket createContext freeContext

-- | Like 'withContext', but allows users to specify a data directory.
withContextFromPath :: FilePath -> (Context -> IO a) -> IO a
withContextFromPath path =
  bracket (createContextFromPath path) freeContext

-- | This functions sets up 'Context' to process the given
-- sentence. After calling this function, 'NLP.Senna.Processor.process' may be used to
-- perform NLP tasks on sentence.
tokenize :: Context -> Sentence -> IO ()
tokenize ctx sentence =
  newCString sentence >>= c_senna_tokenize ctx

