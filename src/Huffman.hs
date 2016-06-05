{-# LANGUAGE DeriveGeneric #-}
module Huffman (huffmanEncode, huffmanDecode, runLengthEncode, runLengthDecode, rle, rld) where

import Data.List (sort, sortBy, group)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.Bits (setBit, testBit, (.|.))
import Data.Binary (Binary (..), encode, decode, Get)
import GHC.Generics (Generic)
import Data.Vector.Binary ()
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Algorithms.Intro as VAI
import Control.Monad.ST
import Data.Ord
import Debug.Trace
import Test.QuickCheck

-- TODO : add some criterion benchmarks
-- binary tree data structure for huffman encoding
data BinT a = Branch {count :: Int, rTree :: BinT a, lTree :: BinT a} | Leaf { count :: Int, dec :: a } | Empty
   deriving (Show, Generic, Eq)
-- Ord instance for BinT a to be able to sort
instance Ord a => Ord (BinT a) where
   compare = cmpTrees

instance Binary a => Binary (BinT a) 

type HuffBit = Word8

mergeBint :: BinT a -> BinT a -> BinT a
mergeBint a b = Branch {count = count a + count b, rTree = a, lTree = b}

toLeafList :: (Ord a, Binary a) => [a] -> [BinT a]
-- (sort [a] goup [a]) -> take lergth -> (Leaf length a)
toLeafList [] = []
toLeafList xs = sort [Leaf {count = length ys, dec = head ys} | ys <- (group . sort) xs]

-- TODO : implement with UV.partition instead
vecToLeafList :: (Ord a, Binary a, UV.Unbox a) => UV.Vector a -> [BinT a]
vecToLeafList v = V.toList $ V.map (\(f, e) -> Leaf { count = f, dec = e }) freq
   where
      vsortBy comp vv = runST $ do
         mv <- V.thaw vv
         VAI.sortBy comp mv
         V.freeze mv
      freq = vsortBy (comparing fst) (V.fromList $ vecGroup v)

vecGroup :: (Ord a, Binary a, UV.Unbox a) => UV.Vector a -> [(Int, a)]
vecGroup v
   | v == UV.empty = []
   | otherwise = (UV.length h, UV.head h) : vecGroup t
   where
      (h, t) = UV.unstablePartition (== UV.head v) v

cmpTrees :: (Ord a) => BinT a -> BinT a -> Ordering
cmpTrees a b = compare (count a) (count b)

huffmanTree :: (Ord a, Binary a, UV.Unbox a) => UV.Vector a -> BinT a
huffmanTree = constructHuffmanTree . vecToLeafList

constructHuffmanTree :: (Ord a, Binary a) => [BinT a] -> BinT a
constructHuffmanTree [] = Empty
constructHuffmanTree [x] = x
constructHuffmanTree (x:y:xs) = constructHuffmanTree (sort (mergeBint x y : xs))

createMapping :: (Ord a, Binary a) => BinT a -> [HuffBit] -> Map.Map a (UV.Vector HuffBit) -> Map.Map a (UV.Vector HuffBit)
createMapping Empty _ _ = Map.empty
createMapping (Leaf _ decoded) ints hmap = Map.insert decoded (UV.fromList $ reverse ints) hmap
createMapping (Branch _ l r) ints hmap = createMapping l (0:ints) (createMapping r (1:ints) hmap)

--invertMap :: (Ord a, Binary a) => Map.Map a [HuffBit] -> Map.Map [HuffBit] a
--invertMap mapping = Map.fromList (zip (Map.elems mapping) (Map.keys mapping))

huffmanEncodeFromMapping :: (Ord a, Binary a) => Map.Map a [HuffBit] -> [a] -> [HuffBit]
huffmanEncodeFromMapping m [x] = fromJust (Map.lookup x m)
huffmanEncodeFromMapping m (x:xs) = fromJust (Map.lookup x m) ++ huffmanEncodeFromMapping m xs

vecHuffmanEncodeFromMapping :: (Ord a, Binary a, UV.Unbox a) => Map.Map a (UV.Vector HuffBit) -> UV.Vector a -> UV.Vector HuffBit
vecHuffmanEncodeFromMapping m = UV.concatMap (locallookup m)
   where
      locallookup :: (Ord a) => Map.Map a (UV.Vector HuffBit) -> a -> UV.Vector HuffBit
      locallookup mp e = fromJust $ Map.lookup e mp

huffmanEncode :: (Ord a, Binary a, UV.Unbox a, Show a) => UV.Vector a -> L.ByteString
huffmanEncode vec
  -- if there is only 1 element repeated length is the length of the unencoded values
  -- TODO : investigate removing special case
  | Map.size mapping == 1 = encode (htree, UV.length vec, hencodedToWordVec encoded)
  -- otherwise length is the length of the encoded but not packed bits
  | otherwise = encode (htree, UV.length encoded, hencodedToWordVec encoded)
  where 
    mapping = createMapping htree [] Map.empty
    htree = huffmanTree vec
    encoded = vecHuffmanEncodeFromMapping mapping vec

hencodedToWordVec :: UV.Vector HuffBit -> UV.Vector Word8
hencodedToWordVec vec = UV.fromList $ map packToWord (chop8 vec)

chop8 :: UV.Vector Word8 -> [UV.Vector Word8]
chop8 vec
   | UV.length vec <= 8 = [vec]
   | otherwise = UV.take 8 vec : chop8 (UV.drop 8 vec)

packToWord :: UV.Vector HuffBit -> Word8
packToWord v
   | v == UV.empty = 0
   | otherwise = foldl1 (.|.) (map (setCond v 0) [0 .. (UV.length v - 1)] :: [Word8])

setCond :: UV.Vector HuffBit -> Word8 -> Int -> Word8
setCond v w i
   | v UV.! i == 0 = w
   | otherwise = setBit w i

huffmanDecode :: (Ord a, Binary a, Show a, UV.Unbox a) => L.ByteString -> UV.Vector a
huffmanDecode bs = case htree of
      Empty -> UV.empty
      (Leaf _ d) -> UV.replicate len d
      Branch {} -> UV.fromList $ huffmanDecodeFromTree htree (wordListToHEncoded len encList)
   where 
      --imapping = Map.fromList $ zip (map UV.fromList vals) ks
      (htree, len, encList) = decode bs :: (Binary a) => (BinT a, Int, UV.Vector Word8) 

cdTest :: [Int] -> Bool
cdTest s = (UV.toList . huffmanDecode . huffmanEncode . UV.fromList) s == s

qcHuffman :: IO ()
qcHuffman = quickCheckWith stdArgs { maxSuccess = 500} cdTest

wordListToHEncoded :: Int -> UV.Vector Word8 -> [HuffBit]
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
huffmanDecodeFromMapping :: (Ord a, Binary a) => Map.Map [HuffBit] a -> [HuffBit] -> [HuffBit] -> [a]
huffmanDecodeFromMapping m k [] = case Map.lookup k m of
   Nothing -> []
   Just a -> [a]
huffmanDecodeFromMapping m k (i:is) = case Map.lookup k m of
   Nothing -> huffmanDecodeFromMapping m (k ++ [i]) is
   Just a -> a : huffmanDecodeFromMapping m [] (i:is)

huffmanDecodeFromTree :: (Ord a, Binary a) => BinT a -> [HuffBit] -> [a]
huffmanDecodeFromTree _ [] = []
huffmanDecodeFromTree t lbi = symbol : huffmanDecodeFromTree t rest
   where
      (symbol, rest) = decodePrefix t lbi
      decodePrefix (Leaf _ s) l = (s, l)
      decodePrefix (Branch _ l _) (0:xs) = decodePrefix l xs
      decodePrefix (Branch _ _ r) (1:xs) = decodePrefix r xs

-- MAP -> splitIndex -> rest of ENCODED
vecHuffmanDecodeFromMapping :: (Ord a, Binary a, UV.Unbox a) => Map.Map [HuffBit] a -> [HuffBit] -> [HuffBit] -> UV.Vector a
vecHuffmanDecodeFromMapping m k [] = case Map.lookup k m of
   Nothing -> UV.empty
   Just a -> UV.fromList [a]
vecHuffmanDecodeFromMapping m k (i:is) = case Map.lookup k m of
   Nothing -> vecHuffmanDecodeFromMapping m (k ++ [i]) is
   Just a -> UV.cons a $ vecHuffmanDecodeFromMapping m [] (i:is)

vecHuffmanDecodeFromMapping' :: (Ord a, Binary a, UV.Unbox a) => Map.Map (UV.Vector HuffBit) a -> Int -> UV.Vector HuffBit -> UV.Vector a
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
rle :: UV.Vector Int -> UV.Vector Int
rle is = UV.fromList $ eqGroup is

eqGroup :: UV.Vector Int -> [Int]
eqGroup v
   | v == UV.empty = []
   | otherwise = UV.length g : UV.head g : eqGroup rest
   where
         (g, rest) = spanpre v

spanpre :: UV.Vector Int -> (UV.Vector Int, UV.Vector Int)
spanpre viv = UV.span (== UV.head viv) viv

rld :: UV.Vector Int -> UV.Vector Int
rld is = UV.concat $ lrep is
   where
   lrep v
      | UV.length v >= 2 = UV.replicate (v UV.! 0) (v UV.! 1) : lrep (UV.drop 2 v)
      | otherwise = []

qcRleRld :: [Int] -> Bool
qcRleRld l = (rld . rle) v == v
   where
      v = UV.fromList l

runLengthEncode :: UV.Vector Int -> L.ByteString
runLengthEncode = encode . rle

runLengthDecode :: L.ByteString -> UV.Vector Int
runLengthDecode = rld . decode
