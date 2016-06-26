{-# LANGUAGE DeriveGeneric #-}

module Huffman (BinT (..), huffmanEncode, huffmanDecode, vecToLeafList, runLengthEncode, runLengthDecode, rle, rld) where

import Data.List (sort, sortBy, group)
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.Bits (setBit, testBit, (.|.))
import Data.Binary (Binary (..))
import qualified Data.Binary as Bin (encode, decode)
import GHC.Generics (Generic)
import Data.Vector.Binary ()
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Vector as V
import Data.Vector.Unboxed (Unbox, Vector (..))
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Algorithms.Intro as VAI
import Control.Monad.ST
import Data.Ord
import Debug.Trace
import Test.QuickCheck
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Serialize (Serialize (..), Get, Putter)
import qualified Data.Serialize as Cereal (encode, decode, get, put)
import Data.Vector.Serialize
import Data.Either
import Text.PrettyPrint.ANSI.Leijen (Doc (..), text, (<+>), int, indent, putDoc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- binary tree data structure for huffman encoding
data BinT a = Branch {count :: Int, lTree :: BinT a, rTree :: BinT a} | Leaf { count :: Int, dec :: a } | Empty
   deriving (Show, Generic, Eq)
-- Ord instance for BinT a to be able to sort
instance Ord a => Ord (BinT a) where
   compare = cmpTrees

pprintBinT :: (Show a) => BinT a -> Doc
pprintBinT Empty = text "Empty"
pprintBinT (Leaf c d) = text "Leaf :" <+> int c <+> text (show d)
pprintBinT (Branch c l r) = text "Branch :" <+> int c PP.<$> indent 1 (pprintBinT l) PP.<$> indent 1 (pprintBinT r)

data SparseBinT a = SBranch {sLeft :: SparseBinT a, sRight :: SparseBinT a} | SLeaf {sdec :: a} | SparseEmpty
  deriving (Show, Generic)

pprintSparse :: (Show a) => SparseBinT a -> Doc
pprintSparse SparseEmpty = text "Empty"
pprintSparse (SLeaf d) = text "Leaf :" <+> text (show d)
pprintSparse (SBranch l r) = text "Branch" PP.<$> indent 1 (pprintSparse l) PP.<$> indent 1 (pprintSparse r)

instance Monoid (BinT a) where
  mempty = Empty
  mappend = mergeBinT

mergeBinT :: BinT a -> BinT a -> BinT a
mergeBinT a b = Branch {count = count a + count b, lTree = a, rTree = b}

instance Binary a => Binary (BinT a)

-- even when Serialize is derived => same output size
instance (Serialize a) => Serialize (SparseBinT a)
  where
    get = getSparseBinT Cereal.get
    put = putSparseBinT Cereal.put

getSparseBinT :: Get a -> Get (SparseBinT a)
getSparseBinT ga = do
  w <- Cereal.get :: Get Word8
  case w of
    (-1) -> return SparseEmpty
    0 -> do
      d <- ga
      return (SLeaf d)
    1 -> do
      l <- getSparseBinT ga
      r <- getSparseBinT ga
      return (SBranch l r)

putSparseBinT :: Putter a -> Putter (SparseBinT a)
putSparseBinT _ SparseEmpty = Cereal.put ((-1) :: Word8)
putSparseBinT pa (SLeaf d) = do
  Cereal.put (0 :: Word8)
  pa d
putSparseBinT pa (SBranch l r) = do
  Cereal.put (1 :: Word8)
  putSparseBinT pa l
  putSparseBinT pa r

type HuffBit = Word8

type Key = Seq.Seq HuffBit

type BitEncoded = [HuffBit]

toSparse :: BinT a -> SparseBinT a
toSparse Empty = SparseEmpty
toSparse (Leaf _ d) = SLeaf d
toSparse (Branch _ l r) = SBranch (toSparse l) (toSparse r)

toLeafList :: (Ord a, Binary a) => [a] -> [BinT a]
-- (sort [a] group [a]) -> take lergth -> (Leaf length a)
toLeafList [] = []
toLeafList xs = sort [Leaf {count = length ys, dec = head ys} | ys <- (group . sort) xs]

-- TODO : implement with UV.partition instead
vecToLeafList :: (Ord a, Binary a, Unbox a) => Vector a -> [BinT a]
vecToLeafList v = V.toList $ V.map (\(f, e) -> Leaf { count = f, dec = e }) freq
   where
      vSort vv = runST $ do
         mv <- UV.thaw vv
         VAI.sort mv
         UV.freeze mv
      vSortBy compFunc vv = runST $ do
        mv <- V.thaw vv
        VAI.sortBy compFunc mv
        V.freeze mv
      freq = (vSortBy (comparing (Down . fst)) . V.fromList . vecGroupSorted . vSort) v

vecGroupSorted :: (Ord a, Binary a, Unbox a) => Vector a -> [(Int, a)]
vecGroupSorted v
   | v == UV.empty = []
   | otherwise = (UV.length h, elem) : vecGroupSorted t
   where
     elem = UV.head v
     (h, t) = UV.span (== elem) v

cmpTrees :: (Ord a) => BinT a -> BinT a -> Ordering
cmpTrees a b = compare (count a) (count b)

huffmanTree :: (Ord a, Binary a, Unbox a) => Vector a -> BinT a
huffmanTree = constructHuffmanTree . vecToLeafList

constructHuffmanTree :: (Ord a, Binary a) => [BinT a] -> BinT a
constructHuffmanTree [] = Empty
constructHuffmanTree [x] = x
constructHuffmanTree (x:y:xs) = constructHuffmanTree (sort (mergeBinT x y : xs))

-- TODO : create and use a Traversable instance
createMapping :: (Ord a, Binary a) => BinT a -> Key -> Map Key a -> Map Key a
createMapping Empty _ _ = Map.empty
createMapping (Leaf _ decoded) keyBits hmap = Map.insert keyBits decoded hmap
createMapping (Branch _ l r) keyBits hmap = createMapping l (keyBits Seq.|> 0) (createMapping r (keyBits Seq.|> 1) hmap)

createInverseMapping :: (Ord a, Binary a) => BinT a -> Key -> Map a Key -> Map a Key
createInverseMapping Empty _ _ = Map.empty
createInverseMapping (Leaf _ decoded) keyBits hmap = Map.insert decoded keyBits hmap
createInverseMapping (Branch _ l r) keyBits hmap = createInverseMapping l (keyBits Seq.|> 0) (createInverseMapping r (keyBits Seq.|> 1) hmap)

--invertMap :: (Ord a, Binary a) => Map a [HuffBit] -> Map [HuffBit] a
--invertMap mapping = Map.fromList (zip (Map.elems mapping) (Map.keys mapping))

huffmanEncodeFromMapping :: (Ord a, Binary a) => Map a Key -> [a] -> [HuffBit]
huffmanEncodeFromMapping m = concatMap (\a -> (toList . fromJust) (Map.lookup a m))

vecHuffmanEncodeFromMapping :: (Ord a, Binary a, Unbox a) => Map a Key -> Vector a -> Vector HuffBit
vecHuffmanEncodeFromMapping m = UV.concatMap (localLookup m)
   where
      localLookup :: (Ord a) => Map a Key -> a -> Vector HuffBit
      localLookup mp e = (UV.fromList . toList . fromJust) $ Map.lookup e mp

huffmanEncode :: (Ord a, Serialize a, Binary a, Unbox a, Show a) => Vector a -> S.ByteString
huffmanEncode vec
  | UV.length encoded == 0 = Cereal.encode (toSparse htree, UV.length vec, UV.empty :: Vector Word8)
  | otherwise = Cereal.encode (toSparse htree, UV.length encoded, hencodedToWordVec encoded)
  -- if there is only 1 element repeated length is the length of the unencoded values
  -- otherwise length is the length of the encoded but not packed bits
  where
    invMapping = createInverseMapping htree Seq.empty Map.empty
    mapping = createMapping htree Seq.empty Map.empty
    htree = huffmanTree vec
    encoded = vecHuffmanEncodeFromMapping invMapping vec

hencodedToWordVec :: Vector HuffBit -> Vector Word8
hencodedToWordVec vec = UV.fromList $ map packToWord (chop8 vec)

chop8 :: Vector Word8 -> [Vector Word8]
chop8 vec
   | UV.length vec <= 8 = [vec]
   | otherwise = UV.take 8 vec : chop8 (UV.drop 8 vec)

packToWord :: Vector HuffBit -> Word8
packToWord v
   | v == UV.empty = 0
   | otherwise = foldl1 (.|.) (map (setCond v 0) [0 .. (UV.length v - 1)] :: [Word8])

setCond :: Vector HuffBit -> Word8 -> Int -> Word8
setCond v w i
   | v UV.! i == 0 = w
   | otherwise = setBit w i

huffmanDecode :: (Ord a, Binary a, Serialize a, Show a, Unbox a) => S.ByteString -> Vector a
huffmanDecode bs = case sparseHTree of
  SparseEmpty -> UV.empty
  -- if the tree is a leaf there are no encoded bits thus encList is empty
  SLeaf d -> UV.replicate len d
  SBranch {} -> (UV.fromList . huffmanDecodeFromTree sparseHTree) (wordListToHEncoded len encList)
  where
    (sparseHTree, len, encList) = huffmanDeserialize bs

huffmanDeserialize :: (Serialize a) => S.ByteString -> (SparseBinT a, Int, Vector Word8)
huffmanDeserialize bs = case Cereal.decode bs of
      (Left errString) -> (SparseEmpty, 0, UV.empty)
      (Right tuple) -> tuple

huffmanProp :: [Int] -> Bool
huffmanProp s = (UV.toList . huffmanDecode . huffmanEncode . UV.fromList) s == s

qcHuffman :: IO ()
qcHuffman = quickCheckWith stdArgs { maxSuccess = 500} huffmanProp

wordListToHEncoded :: Int -> Vector Word8 -> [HuffBit]
wordListToHEncoded len v
   | (UV.length v == 1) && (len `mod` 8 == 0) = reverse (unpackFromWord8 (UV.head v))
   | UV.length v == 1 = take (len `mod` 8) $ reverse $  unpackFromWord8 (UV.head v)
   | otherwise  = reverse (unpackFromWord8 (UV.head v)) ++ wordListToHEncoded len (UV.tail v)

unpackFromWord8 :: Word8 -> [HuffBit]
unpackFromWord8 = unpackFromWord8' 7

-- iBit is the Int representation of the Bit being processed
unpackFromWord8' :: Int -> Word8 -> [HuffBit]
unpackFromWord8' (-1) _ = []
unpackFromWord8' i w = iBit : unpackFromWord8' (i - 1) w
   where
      iBit
         | testBit w i = 1
         | otherwise = 0

-- MAP -> (partial) KEY -> rest of ENCODED
huffmanDecodeFromMapping :: (Ord a, Binary a) => Map Key a -> Key -> [HuffBit] -> [a]
huffmanDecodeFromMapping m k [] = case Map.lookup k m of
   Nothing -> []
   Just a -> [a]
huffmanDecodeFromMapping m k (i:is) = case Map.lookup k m of
   Nothing -> huffmanDecodeFromMapping m (k Seq.|> i) is
   Just a -> a : huffmanDecodeFromMapping m Seq.empty (i:is)

huffmanDecodeFromTree :: (Ord a, Binary a) => SparseBinT a -> [HuffBit] -> [a]
huffmanDecodeFromTree _ [] = []
huffmanDecodeFromTree t lbi = symbol : huffmanDecodeFromTree t rest
   where
      (symbol, rest) = decodePrefix t lbi
      decodePrefix (SLeaf s) l = (s, l)
      decodePrefix (SBranch l _) (0:xs) = decodePrefix l xs
      decodePrefix (SBranch _ r) (1:xs) = decodePrefix r xs

-- MAP -> splitIndex -> rest of ENCODED
vecHuffmanDecodeFromMapping :: (Ord a, Binary a, Unbox a) => Map Key a -> Key -> [HuffBit] -> Vector a
vecHuffmanDecodeFromMapping m k [] = case Map.lookup k m of
   Nothing -> UV.empty
   Just a -> UV.fromList [a]
vecHuffmanDecodeFromMapping m k (i:is) = case Map.lookup k m of
   Nothing -> vecHuffmanDecodeFromMapping m (k Seq.|> i) is
  --TODO : fix cons building up chunks
   Just a -> UV.cons a $ vecHuffmanDecodeFromMapping m Seq.empty (i:is)

vecHuffmanDecodeFromMapping' :: (Ord a, Binary a, Unbox a) => Map (Vector HuffBit) a -> Int -> Vector HuffBit -> Vector a
vecHuffmanDecodeFromMapping' m spl input
   | UV.length input == spl = case Map.lookup input m of
      Nothing -> UV.empty
      Just a -> UV.cons a UV.empty
   | otherwise = case Map.lookup key m of
      Nothing -> vecHuffmanDecodeFromMapping' m (spl + 1) input
      Just a -> UV.cons a $ vecHuffmanDecodeFromMapping' m 1 rest
   where
      key = UV.take spl input
      rest = UV.drop spl input

-- run length encoder
rle :: (Eq a, Unbox a) => Vector a -> Vector (Int, a)
rle is = UV.fromList $ eqGroup is

eqGroup :: (Eq a, Unbox a) => Vector a -> [(Int, a)]
eqGroup v
   | v == UV.empty = []
   | otherwise = (UV.length g, UV.head g) : eqGroup rest
   where
         (g, rest) = spanpre v

spanpre :: (Eq a, Unbox a) => Vector a -> (Vector a, Vector a)
spanpre viv = UV.span (== UV.head viv) viv

rld :: (Unbox a) => Vector (Int, a) -> Vector a
rld = UV.concatMap (uncurry UV.replicate)

runLengthProp :: [Int] -> Bool
runLengthProp l = (rld . rle) v == v
   where
      v = UV.fromList l

qcRunLength :: IO ()
qcRunLength = quickCheckWith stdArgs { maxSuccess = 500} runLengthProp

runLengthEncode :: (Eq a, Binary a, Unbox a) => Vector a -> L.ByteString
runLengthEncode = Bin.encode . rle

runLengthDecode :: (Unbox a, Binary a) => L.ByteString -> Vector a
runLengthDecode = rld . Bin.decode

-- TODO : create a run length enc/decoder for ByteString
runLengthEncodeBS :: S.ByteString -> S.ByteString
runLengthEncodeBS = undefined

runLengthDecodeBS :: S.ByteString -> S.ByteString
runLengthDecodeBS = undefined
