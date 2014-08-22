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

createContext :: IO Context
createContext =
  dataDir >>= createContextFromPath

createContextFromPath :: FilePath -> IO Context
createContextFromPath path =
  newCString (addTrailingPathSeparator path) >>= c_senna_new

freeContext :: Context -> IO ()
freeContext =
  c_senna_free

withContext :: (Context -> IO a) -> IO a
withContext =
  bracket createContext freeContext

withContextFromPath :: FilePath -> (Context -> IO a) -> IO a
withContextFromPath path =
  bracket (createContextFromPath path) freeContext

tokenize :: Context -> Sentence -> IO ()
tokenize ctx sentence =
  newCString sentence >>= c_senna_tokenize ctx

