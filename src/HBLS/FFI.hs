{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

module HBLS.FFI
  ( -- * FFI wrappers

    -- ** Types
    BLSResult
      ( BLSSuccess,
        BLSBadEncoding,
        BLSPointNotOnCurve,
        BLSPointNotInGroup,
        BLSAggrTypeMismatch,
        BLSVerifyFail,
        BLSPKIsInfinity,
        BLSBadScalar
      ),
    C_G1,
    C_G1Affine,
    C_G2,
    C_G2Affine,
    CScalar,

    -- ** Constants
    g1Size,
    g1CompressedSize,
    g1AffineSize,
    g2Size,
    g2CompressedSize,
    g2AffineSize,

    -- ** Functions
    withNewG1,
    withNewG1Affine,
    withNewG2,
    withNewG2Affine,
    withNewScalar,

    -- ** Bindings

    -- *** G1
    cG1Add,
    cG1Neg,
    cG1ScalarMul,
    cG1Equal,
    cG1HashToGroup,
    cG1Compress,
    cG1Uncompress,
    cG1MultPippenger,

    -- *** G2
    cG2Add,
    cG2Neg,
    cG2ScalarMul,
    cG2Equal,
    cG2HashToGroup,
    cG2Compress,
    cG2Uncompress,
    cG2MultPippenger,

    -- *** Scalars
    cScalarFromBendian,
    cScalarFromLendian,
  )
where

import Control.Category ((>>>))
import Control.Monad.Primitive (PrimState)
import Data.Kind (Type)
import Data.Primitive.ByteArray
  ( ByteArray,
    ByteArray#,
    MutableByteArray,
    MutableByteArray#,
    newPinnedByteArray,
    withMutableByteArrayContents,
  )
import Data.Word (Word64, Word8)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Foreign.C.Types (CBool (CBool), CInt (CInt), CSize (CSize))
import Foreign.Ptr (Ptr, castPtr)

data {-# CTYPE "blst.h" "blst_p1" #-} C_G1

data {-# CTYPE "blst.h" "blst_p1_affine" #-} C_G1Affine

data {-# CTYPE "blst.h" "blst_p2" #-} C_G2

data {-# CTYPE "blst.h" "blst_p2_affine" #-} C_G2Affine

data {-# CTYPE "blst.h" "blst_scalar" #-} CScalar

newtype {-# CTYPE "blst.h" "BLST_ERROR" #-} BLSResult = BLSResult CInt

pattern BLSSuccess :: BLSResult
pattern BLSSuccess = BLSResult 0

pattern BLSBadEncoding :: BLSResult
pattern BLSBadEncoding = BLSResult 1

pattern BLSPointNotOnCurve :: BLSResult
pattern BLSPointNotOnCurve = BLSResult 2

pattern BLSPointNotInGroup :: BLSResult
pattern BLSPointNotInGroup = BLSResult 3

pattern BLSAggrTypeMismatch :: BLSResult
pattern BLSAggrTypeMismatch = BLSResult 4

pattern BLSVerifyFail :: BLSResult
pattern BLSVerifyFail = BLSResult 5

pattern BLSPKIsInfinity :: BLSResult
pattern BLSPKIsInfinity = BLSResult 6

pattern BLSBadScalar :: BLSResult
pattern BLSBadScalar = BLSResult 7

{-# COMPLETE
  BLSSuccess,
  BLSBadEncoding,
  BLSPointNotOnCurve,
  BLSPointNotInGroup,
  BLSAggrTypeMismatch,
  BLSVerifyFail,
  BLSPKIsInfinity,
  BLSBadScalar
  #-}

g1Size :: Int
g1Size = 144

g1CompressedSize :: Int
g1CompressedSize = 48

g1AffineSize :: Int
g1AffineSize = 96

g2Size :: Int
g2Size = 288

g2CompressedSize :: Int
g2CompressedSize = 96

g2AffineSize :: Int
g2AffineSize = 192

withNewG1 ::
  forall (a :: Type).
  (Ptr C_G1 -> IO a) ->
  IO (MutableByteArray (PrimState IO), a)
withNewG1 f = do
  ba <- newPinnedByteArray g1Size
  res <- withMutableByteArrayContents ba (castPtr >>> f)
  pure (ba, res)

withNewG1Affine ::
  forall (a :: Type).
  (Ptr C_G1Affine -> IO a) ->
  IO (MutableByteArray (PrimState IO), a)
withNewG1Affine f = do
  ba <- newPinnedByteArray g1AffineSize
  res <- withMutableByteArrayContents ba (castPtr >>> f)
  pure (ba, res)

withNewG2 ::
  forall (a :: Type).
  (Ptr C_G2 -> IO a) ->
  IO (MutableByteArray (PrimState IO), a)
withNewG2 f = do
  ba <- newPinnedByteArray g2Size
  res <- withMutableByteArrayContents ba (castPtr >>> f)
  pure (ba, res)

withNewG2Affine ::
  forall (a :: Type).
  (Ptr C_G2Affine -> IO a) ->
  IO (MutableByteArray (PrimState IO), a)
withNewG2Affine f = do
  ba <- newPinnedByteArray g2AffineSize
  res <- withMutableByteArrayContents ba (castPtr >>> f)
  pure (ba, res)

withNewScalar ::
  forall (a :: Type).
  (Ptr CScalar -> IO a) ->
  IO a
withNewScalar f = do
  ba <- newPinnedByteArray 32
  withMutableByteArrayContents ba (f . castPtr)

foreign import capi unsafe "blst.h blst_p1_add_or_double"
  cG1Add :: Ptr C_G1 -> ConstPtr C_G1 -> ConstPtr C_G1 -> IO ()

foreign import capi unsafe "blst.h blst_p2_add_or_double"
  cG2Add :: Ptr C_G2 -> ConstPtr C_G2 -> ConstPtr C_G2 -> IO ()

foreign import capi unsafe "blst.h blst_p1_cneg"
  cG1Neg :: Ptr C_G1 -> CBool -> IO ()

foreign import capi unsafe "blst.h blst_p2_cneg"
  cG2Neg :: Ptr C_G2 -> CBool -> IO ()

foreign import capi unsafe "blst.h blst_p1_mult"
  cG1ScalarMul :: Ptr C_G1 -> ConstPtr C_G1 -> ConstPtr Word8 -> CSize -> IO ()

foreign import capi unsafe "blst.h blst_p2_mult"
  cG2ScalarMul :: Ptr C_G2 -> ConstPtr C_G2 -> ConstPtr Word8 -> CSize -> IO ()

foreign import capi unsafe "blst.h blst_p1_is_equal"
  cG1Equal :: ConstPtr C_G1 -> ConstPtr C_G1 -> IO CBool

foreign import capi unsafe "blst.h blst_p2_is_equal"
  cG2Equal :: ConstPtr C_G2 -> ConstPtr C_G2 -> IO CBool

foreign import capi unsafe "blst.h blst_p1_compress"
  cG1Compress :: MutableByteArray# (PrimState IO) -> ConstPtr C_G1 -> IO ()

foreign import capi unsafe "blst.h blst_p2_compress"
  cG2Compress :: MutableByteArray# (PrimState IO) -> ConstPtr C_G2 -> IO ()

foreign import capi unsafe "blst.h blst_p1_uncompress"
  cG1Uncompress :: Ptr C_G1Affine -> ByteArray# -> IO BLSResult

foreign import capi unsafe "blst.h blst_p2_uncompress"
  cG2Uncompress :: Ptr C_G2Affine -> ByteArray# -> IO BLSResult

foreign import capi unsafe "blst.h blst_hash_to_g1"
  cG1HashToGroup ::
    Ptr C_G1 ->
    ByteArray# ->
    CSize ->
    ByteArray# ->
    CSize ->
    ByteArray# ->
    CSize ->
    IO ()

foreign import capi unsafe "blst.h blst_hash_to_g2"
  cG2HashToGroup ::
    Ptr C_G2 ->
    ByteArray# ->
    CSize ->
    ByteArray# ->
    CSize ->
    ByteArray# ->
    CSize ->
    IO ()

foreign import capi unsafe "blst.h blst_p1s_mult_pippenger"
  cG1MultPippenger ::
    Ptr C_G1 ->
    ConstPtr (ConstPtr C_G1Affine) ->
    CSize ->
    ConstPtr ByteArray ->
    CSize ->
    Ptr Word64 ->
    IO ()

foreign import capi unsafe "blst.h blst_p2s_mult_pippenger"
  cG2MultPippenger ::
    Ptr C_G2 ->
    ConstPtr (ConstPtr C_G2Affine) ->
    CSize ->
    ConstPtr ByteArray ->
    CSize ->
    Ptr Word64 ->
    IO ()

foreign import capi unsafe "blst.h blst_scalar_from_bendian"
  cScalarFromBendian ::
    Ptr CScalar ->
    ConstPtr Word8 ->
    IO ()

foreign import capi unsafe "blst.h blst_scalar_from_lendian"
  cScalarFromLendian ::
    Ptr CScalar ->
    ConstPtr Word8 ->
    IO ()
