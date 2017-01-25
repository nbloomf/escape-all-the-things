module Text.Escape.Encoding where


class Encoding t where
  isReserved :: t -> Char -> Bool

  isAllowed :: t -> Char -> Bool

  isCodeWord :: t -> [Char] -> Bool

  stripCodeWord :: t -> [Char] -> Maybe ([Char], [Char])

  encodeChar :: t -> Char -> [Char]

  decodeChar :: t -> [Char] -> Char


toCodeWords :: (Encoding t) => t -> [Char] -> ([[Char]], [Char])
toCodeWords t cs = toCodeWords' [] cs
  where
    toCodeWords' xs [] = (reverse xs, [])
    toCodeWords' xs as = case stripCodeWord t as of
      Nothing        -> (reverse xs, as)
      Just (x, rest) -> toCodeWords' (x:xs) rest
