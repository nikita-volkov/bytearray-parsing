module ByteArray.Parsing.ShortByteString
where

import ByteArray.Parsing.Prelude
import qualified Data.ByteString.Short.Internal as A
import qualified ByteArray.Parsing.Parse as B
import qualified Data.Primitive.ByteArray as C


parse :: ShortByteString -> B.Parse parsed -> (Int -> result) -> (Text -> result) -> (Int -> parsed -> result) -> result
parse (A.SBS unboxedArray) (B.Parse parse) needMoreResult errorResult successResult =
  parse (C.ByteArray unboxedArray) 0 needMoreResult errorResult successResult
