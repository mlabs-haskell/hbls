{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HBLS
  ( G1Point,
    G2Point,
  )
where

import Control.Monad.ST (runST)
import Data.Bits (unsafeShiftR)
import Data.Foldable (traverse_)
import Data.Group (Group (invert, pow, (~~)))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Primitive.ByteArray
  ( ByteArray,
    copyByteArrayToPtr,
    fillByteArray,
    newPinnedByteArray,
    unsafeFreezeByteArray,
    withByteArrayContents,
    withMutableByteArrayContents,
  )
import Data.Semigroup (Semigroup (sconcat, stimes))
import Data.Word (Word8)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)
import HBLS.FFI
  ( C_G1,
    cG1Add,
    cG1Equal,
    cG1Neg,
    cG1ScalarMul,
    cG2Equal,
    cScalarFromLendian,
    g1Size,
    withNewG1,
    withNewScalar,
  )
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | @since 1.0.0
newtype G1Point = G1Point ByteArray

-- | @since 1.0.0
instance Eq G1Point where
  {-# INLINEABLE (==) #-}
  G1Point p1 == G1Point p2 =
    unsafeDupablePerformIO . withByteArrayContents p1 $ \p1Ptr ->
      withByteArrayContents p2 $ \p2Ptr -> do
        let p1Ptr' = ConstPtr . castPtr $ p1Ptr
        let p2Ptr' = ConstPtr . castPtr $ p2Ptr
        (== 0) <$> cG1Equal p1Ptr' p2Ptr'

-- | = Important note
--
-- If 'stimes' is called with an exponent larger than 32 bytes in size, it will
-- be reduced modulo @2^256@; that is, only the least significant 32 bytes of
-- the exponent (at most) will be considered. This is a limitation imposed by
-- @blst@ itself, and in practice doesn't matter anyway, as useful exponents for
-- @G1@ points are significantly smaller.
--
-- @since 1.0.0
instance Semigroup G1Point where
  {-# INLINEABLE (<>) #-}
  G1Point p1 <> G1Point p2 =
    unsafeDupablePerformIO . withByteArrayContents p1 $ \p1Ptr ->
      withByteArrayContents p2 $ \p2Ptr -> do
        let p1Ptr' = ConstPtr . castPtr $ p1Ptr
        let p2Ptr' = ConstPtr . castPtr $ p2Ptr
        (mba, _) <- withNewG1 $ \resPtr -> cG1Add resPtr p1Ptr' p2Ptr'
        G1Point <$> unsafeFreezeByteArray mba
  {-# INLINEABLE sconcat #-}
  sconcat xs = unsafeDupablePerformIO $ do
    (mba, _) <- withNewG1 $ \resPtr -> traverse_ (go resPtr) xs
    G1Point <$> unsafeFreezeByteArray mba
    where
      go :: Ptr C_G1 -> G1Point -> IO ()
      go resPtr (G1Point ba) = withByteArrayContents ba $ \p -> do
        let p' = ConstPtr . castPtr $ p
        let constP = ConstPtr resPtr
        cG1Add resPtr constP p'
  {-# INLINEABLE stimes #-}
  stimes e p = case compare e 0 of
    LT -> error "stimes: negative exponent"
    EQ -> mempty
    GT -> unsafeDupablePerformIO . doExp p (toInteger e) $ const (pure ())

-- | @since 1.0.0
instance Monoid G1Point where
  {-# INLINEABLE mempty #-}
  -- According to the implementation, the all-zero vector is the 'canonical'
  -- zero value. We can construct that directly, using `ST` instead.
  --
  -- We have to use this rather manual method as `createByteArray` makes an
  -- _un_pinned `ByteArray`, which isn't suitable for our uses.
  mempty = G1Point $ runST $ do
    mba <- newPinnedByteArray g1Size
    fillByteArray mba 0 g1Size 0
    unsafeFreezeByteArray mba
  {-# INLINEABLE mconcat #-}
  mconcat = \case
    [] -> mempty
    (x : xs) -> sconcat (x :| xs)

-- | = Important note
--
-- The same caveat as for 'stimes' applies to this instance as well. This
-- applies regardless of sign: that is, the 32-byte limit is considered for the
-- absolute value of the exponent.
--
-- @since 1.0.0
instance Group G1Point where
  {-# INLINEABLE invert #-}
  invert (G1Point ba) = unsafeDupablePerformIO $ do
    (mba, _) <- withNewG1 $ \resPtr -> do
      copyByteArrayToPtr (castPtr @_ @Word8 resPtr) ba 0 g1Size
      -- The `1` here is the `CBool` true. Any nonzero value would technically
      -- work too. Needed because BLS negation is conditional: `True` means
      -- 'negate'.
      cG1Neg resPtr 1
    G1Point <$> unsafeFreezeByteArray mba
  {-# INLINEABLE (~~) #-}
  G1Point ba1 ~~ G1Point ba2 = unsafeDupablePerformIO . withByteArrayContents ba1 $ \p1 -> do
    (mba, _) <- withNewG1 $ \resPtr -> do
      copyByteArrayToPtr (castPtr @_ @Word8 resPtr) ba2 0 g1Size
      cG1Neg resPtr 1
      cG1Add resPtr (ConstPtr . castPtr $ p1) (ConstPtr resPtr)
    G1Point <$> unsafeFreezeByteArray mba
  {-# INLINEABLE pow #-}
  pow p e = case compare e 0 of
    LT -> unsafeDupablePerformIO . doExp p (abs . toInteger $ e) $ \ptr -> cG1Neg ptr 1
    EQ -> mempty
    GT -> unsafeDupablePerformIO . doExp p (toInteger e) $ const (pure ())

-- | @since 1.0.0
newtype G2Point = G2Point ByteArray

-- | @since 1.0.0
instance Eq G2Point where
  {-# INLINEABLE (==) #-}
  G2Point p1 == G2Point p2 =
    unsafeDupablePerformIO . withByteArrayContents p1 $ \p1Ptr ->
      withByteArrayContents p2 $ \p2Ptr -> do
        let p1Ptr' = ConstPtr . castPtr $ p1Ptr
        let p2Ptr' = ConstPtr . castPtr $ p2Ptr
        (== 0) <$> cG2Equal p1Ptr' p2Ptr'

-- Helpers

-- NOTE: This function expects the `Integer` input to be positive.
doExp :: G1Point -> Integer -> (Ptr C_G1 -> IO ()) -> IO G1Point
doExp (G1Point ba) e f = withByteArrayContents ba $ \p -> do
  scalarMba <- newPinnedByteArray 32
  (mba, _) <- withMutableByteArrayContents scalarMba $ \scalarP -> do
    integerToScalar (toInteger e) scalarP
    withNewScalar $ \actualScalarP -> do
      cScalarFromLendian actualScalarP (ConstPtr scalarP)
      withNewG1 $ \resPtr -> do
        -- We use the number of _bits_ in the scalar, rather than the number of
        -- _bytes_, as this is what the C API wants.
        cG1ScalarMul resPtr (ConstPtr . castPtr $ p) (ConstPtr . castPtr $ actualScalarP) 256
        f resPtr
  G1Point <$> unsafeFreezeByteArray mba

-- NOTE: This function expects the `Integer` input to be positive. Furthermore,
-- it expects the `Ptr Word8` to point to the beginning of a block of memory
-- _exactly_ 32 bytes long.
integerToScalar :: Integer -> Ptr Word8 -> IO ()
integerToScalar i ptr = do
  fillBytes ptr 0 32
  go 0 i
  where
    go :: Int -> Integer -> IO ()
    go ix = \case
      0 -> pure ()
      remaining -> do
        let digit :: Word8 = fromIntegral remaining
        let remaining' = remaining `unsafeShiftR` 8
        pokeElemOff ptr ix digit
        go (ix + 1) remaining'
