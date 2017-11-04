module ByteArray.Parsing.Parse
where

import ByteArray.Parsing.Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Internal as A
import qualified GHC.ForeignPtr as B
import qualified Data.Primitive.ByteArray as C
import qualified Data.Primitive.Addr as D
import qualified ByteArray.Parsing.Prim as E


newtype Parse a =
  Parse (forall x. C.ByteArray -> Int -> (Int -> x) -> (Text -> x) -> (Int -> a -> x) -> x)

instance Functor Parse where
  {-# INLINE fmap #-}
  fmap mapping (Parse parse) =
    Parse $ \ array index needMoreResult errorResult successResult ->
    {-# SCC "fmap" #-} 
    parse array index needMoreResult errorResult $ \ newIndex parsed ->
    successResult newIndex (mapping parsed)

instance Applicative Parse where
  {-# INLINE pure #-}
  pure x =
    Parse $ \ _ index _ _ successResult -> successResult index x
  {-# INLINE (<*>) #-}
  (<*>) (Parse parseLeft) (Parse parseRight) =
    Parse $ \ array index needMoreResult errorResult successResult ->
    {-# SCC "<*>" #-} 
    parseLeft array index needMoreResult errorResult $ \ leftConsumed leftParsed ->
    parseRight array leftConsumed needMoreResult errorResult $ \ rightConsumed rightParsed ->
    successResult rightConsumed (leftParsed rightParsed)

instance Alternative Parse where
  {-# INLINE empty #-}
  empty =
    Parse $ \ _ _ needMoreResult _ _ -> needMoreResult 0
  {-# INLINE (<|>) #-}
  (<|>) (Parse parseLeft) (Parse parseRight) =
    Parse $ \ array index needMoreResult errorResult successResult ->
    {-# SCC "<|>" #-} 
    parseLeft array index
      (\ _ -> parseRight array index needMoreResult errorResult successResult)
      errorResult
      successResult

instance Monad Parse where
  {-# INLINE return #-}
  return =
    pure
  {-# INLINE (>>=) #-}
  (>>=) (Parse parseLeft) rightK =
    Parse $ \ array index needMoreResult errorResult successResult ->
    {-# SCC ">>=" #-} 
    parseLeft array index needMoreResult errorResult $ \ leftConsumed leftParsed ->
    case rightK leftParsed of
      Parse parseRight ->
        parseRight array leftConsumed needMoreResult errorResult successResult

instance MonadPlus Parse where
  mzero = empty
  mplus = (<|>)

{-# INLINE fail #-}
fail :: Text -> Parse a
fail message =
  Parse $ \ _ _ _ errorResult _ ->
  errorResult message

{-# INLINE word8 #-}
word8 :: Parse Word8
word8 =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "word8" #-} 
  if C.sizeofByteArray array >= index
    then successResult (succ index) (C.indexByteArray array index)
    else needMoreResult 1

{-# INLINE beNum #-}
beNum :: (Bits a, Num a) => Int -> Parse a
beNum size =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "beNum" #-} 
  let
    arrayLength = C.sizeofByteArray array
    nextIndex = index + size
    in if arrayLength >= size
      then let
        fold !index !state =
          if index < nextIndex
            then fold (succ index) (shiftL state 8 .|. fromIntegral (C.indexByteArray array index :: Word8))
            else state
        in successResult (nextIndex) (fold index 0)
      else needMoreResult (nextIndex - arrayLength)

{-# INLINE beWord16 #-}
beWord16 :: Parse Word16
beWord16 =
  {-# SCC "beWord16" #-} 
  beNum 2

{-# INLINE beWord32 #-}
beWord32 :: Parse Word32
beWord32 =
  {-# SCC "beWord32" #-} 
  beNum 4

{-# INLINE beWord64 #-}
beWord64 :: Parse Word64
beWord64 =
  {-# SCC "beWord64" #-} 
  beNum 8

{-# INLINE shortByteString #-}
shortByteString :: Int -> Parse ShortByteString
shortByteString size =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "shortByteString" #-} 
  let
    arrayLength = C.sizeofByteArray array
    nextIndex = index + size
    in if arrayLength >= nextIndex
      then successResult nextIndex (E.byteArraySliceToShortByteString index size array)
      else needMoreResult (nextIndex - arrayLength)

{-# INLINE byteString #-}
byteString :: Int -> Parse ByteString
byteString size =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "byteString" #-} 
  let
    arrayLength = C.sizeofByteArray array
    nextIndex = index + size
    in if arrayLength >= nextIndex
      then successResult nextIndex (E.byteArraySliceToByteString index size array)
      else needMoreResult (nextIndex - arrayLength)

{-# INLINE remainingByteString #-}
remainingByteString :: Parse ByteString
remainingByteString =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "remainingByteString" #-} 
  let
    arrayLength =
      C.sizeofByteArray array
    size =
      arrayLength - index
    in successResult arrayLength (E.byteArraySliceToByteString index (arrayLength - index) array)

{-# INLINE nullTerminatedByteString #-}
nullTerminatedByteString :: Parse ByteString
nullTerminatedByteString =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "nullTerminatedBytes" #-}
  let
    findNullIndex !index =
      if (C.indexByteArray array index :: Word8) == 0
        then index
        else findNullIndex (succ index)
    nullIndex =
      findNullIndex index
    nextIndex =
      succ nullIndex
    size =
      nullIndex - index
    in successResult nextIndex (E.byteArraySliceToByteString index size array)

{-# INLINE byteStringWhile #-}
byteStringWhile :: (Word8 -> Bool) -> Parse ByteString
byteStringWhile predicate =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "byteStringWhile" #-} 
  let
    iterate !index =
      if predicate (C.indexByteArray array index)
        then iterate (succ index)
        else index
    nextIndex =
      iterate index
    size =
      nextIndex - index
    in successResult nextIndex (E.byteArraySliceToByteString index size array)

{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> Parse ()
skipWhile predicate =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "skipWhile" #-} 
  let
    iterate !index =
      if predicate (C.indexByteArray array index)
        then iterate (succ index)
        else index
    nextIndex =
      iterate index
    size =
      nextIndex - index
    in successResult nextIndex ()

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> Parse state
foldWhile predicate step start =
  Parse $ \ array index needMoreResult errorResult successResult ->
  {-# SCC "foldWhile" #-} 
  let
    iterate !index !state =
      let
        byte =
          C.indexByteArray array index
        in if predicate byte
          then iterate (succ index) (step state byte)
          else successResult index state
    in iterate index start

-- |
-- Unsigned integral number encoded in ASCII.
{-# INLINE unsignedASCIIIntegral #-}
unsignedASCIIIntegral :: Integral a => Parse a
unsignedASCIIIntegral =
  {-# SCC "unsignedASCIIIntegral" #-} 
  foldWhile byteIsDigit step 0
  where
    byteIsDigit byte =
      byte - 48 <= 9
    step !state !byte =
      state * 10 + fromIntegral byte - 48
