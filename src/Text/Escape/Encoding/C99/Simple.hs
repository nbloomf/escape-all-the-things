module Text.Escape.Encoding.C99.Simple where

import Text.Escape.Encoding

data C99_Simple = C99_Simple

instance Encoding C99_Simple where
  isReserved _ '\\' = True
  isReserved _ '\"' = True
  isReserved _ _    = False

  isAllowed _ c = elem c $ concat
    [ "abcdefghijklmnopqrstuvwxyz"
    , "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    , "0123456789"
    , "!\"#%&'()*+,-./:;<=>?[\\]^_{|}~"
    ]

  isCodeWord _ x = case x of
    "\\\\" -> True
    "\\\"" -> True
    "\\'"  -> True
    "\\?"  -> True
    "\\a"  -> True
    "\\b"  -> True
    "\\f"  -> True
    "\\n"  -> True
    "\\r"  -> True
    "\\t"  -> True
    "\\v"  -> True
    _      -> False

  stripCodeWord t cs = case cs of
    [] ->
      Nothing

    [c] ->
      if isAllowed t c
        then Just ([c], [])
        else Nothing

    c:d:rest -> case c of
      '\\' -> case isCodeWord t [c,d] of
        True -> Just ([c,d], rest)
        False -> Nothing

      otherwise -> Just ([c], d:rest)
