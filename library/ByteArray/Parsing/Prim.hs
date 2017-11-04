module ByteArray.Parsing.Prim
where

import ByteArray.Parsing.Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Internal as A
import qualified Data.ByteString.Short.Internal as E
import qualified GHC.ForeignPtr as B
import qualified Data.Primitive.ByteArray as C
import qualified Data.Primitive.Addr as D


{-# INLINE byteArraySliceToByteString #-}
byteArraySliceToByteString :: Int -> Int -> C.ByteArray -> ByteString
byteArraySliceToByteString offset size byteArray =
  unsafeDupablePerformIO $ do
    pinnedSlice <- C.newPinnedByteArray size
    C.copyByteArray pinnedSlice 0 byteArray offset size
    let
      D.Addr addr# = C.mutableByteArrayContents pinnedSlice
      C.MutableByteArray mba# = pinnedSlice
      fp = B.ForeignPtr addr# (B.PlainPtr mba#)
      in return (A.PS fp 0 size)

{-# INLINE byteArraySlice #-}
byteArraySlice :: Int -> Int -> C.ByteArray -> C.ByteArray
byteArraySlice offset size byteArray =
  unsafeDupablePerformIO $ do
    slice <- C.newByteArray size
    C.copyByteArray slice 0 byteArray offset size
    C.unsafeFreezeByteArray slice

{-# INLINE byteArraySliceToShortByteString #-}
byteArraySliceToShortByteString :: Int -> Int -> C.ByteArray -> ShortByteString
byteArraySliceToShortByteString offset size byteArray =
  case byteArraySlice offset size byteArray of
    C.ByteArray byteArray# -> E.SBS byteArray#
