module Text.Escape.Encoding.C99.UniversalCharacterNames where

import Text.Escape.Encoding

data C99_UniversalCharacterNames = C99_UniversalCharacterNames

instance Encoding C99_UniversalCharacterNames where
  isReserved _ '\\' = True
  isReserved _ _    = False