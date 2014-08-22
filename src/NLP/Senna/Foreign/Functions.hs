{-# LANGUAGE ForeignFunctionInterface #-}
module NLP.Senna.Foreign.Functions where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import NLP.Senna.Foreign.Types

foreign import ccall "foreign/senna.h senna_new" c_senna_new :: CString -> IO CSenna
foreign import ccall "foreign/senna.h senna_free" c_senna_free :: CSenna -> IO ()
foreign import ccall "foreign/senna.h senna_tokenize" c_senna_tokenize :: CSenna -> CString -> IO ()
foreign import ccall "foreign/senna.h senna_get_error" c_senna_get_errror :: CSenna -> IO CInt
foreign import ccall "foreign/senna.h senna_get_length" c_senna_get_length :: CSenna -> IO CInt
foreign import ccall "foreign/senna.h senna_get_verbs" c_senna_get_verbs  :: CSenna -> IO CInt
foreign import ccall "foreign/senna.h senna_get_words" c_senna_get_words  :: CSenna -> IO (Ptr CString)
foreign import ccall "foreign/senna.h senna_get_pos" c_senna_get_pos :: CSenna -> IO (Ptr CInt)
foreign import ccall "foreign/senna.h senna_get_chk" c_senna_get_chk :: CSenna -> IO (Ptr CInt)
foreign import ccall "foreign/senna.h senna_get_ner" c_senna_get_ner :: CSenna -> IO (Ptr CInt)
foreign import ccall "foreign/senna.h senna_get_srl" c_senna_get_srl :: CSenna -> IO (Ptr (Ptr CInt))
