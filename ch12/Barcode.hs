-- | Strict left fold, similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
   where go s (j:js) = let s' = f s (a ! j)
                       in s' `seq` go s' js
        go s _ = s

-- Stict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a\
foldA1 f a = foldA f (as ! f (bounds a)) a

-- Encoding an EAN-13 Barcode
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

-- | This function computes the check digit; don't pass one in.
encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ ]outerGuar]
  where (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode (rightCodes !)

outerGuard = "101"
centerGuard = "01010"

type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)

type Pixmap = Array (Int, Int) RGB

parseRawPPM :: Parse Pixmap
parseRawPPM =
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P6") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -=> skipSpaces ==>&
    parseNat ==> \maxValue -> 
    assert (maxValue == 255) "max value out of spec" ==>&
    parseByte ==>&
    parseTimes (width * height) parseRGB ==> \pxs ->
    identity (listArray ((0,0), (width-1,height-1)) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte ==> \r ->
           parseByte ==> \g ->
           parseByte ==> \b ->
           identity (r,g,b)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x:) <$> parseTimes (n-1) p
           parseByte ==> \g ->

luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
   where r' = fromIntegral r
         g' = fromIntegral g
         b' = fromIntegral b

type Greymap = Array (Int, Int) Pixel

pixmapToGreymap :: Pixelmap -> Greymap
pixmapToGreymap = fmap luminance

data Bit = Zero | One
           deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double ->  Array k a -> Array k bit
threshold n a = binary <$> a
   where binary i | i < pivot = Zero
                  | otherwise = One
         pivot    = round $ least + (greatest - least) * n
         least    = fromIntegral $ choose (<) a
         greatest = fromIntegral $ choose (>) a
         choose f = foldA1 $ \x y -> if f x y then x else y

type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
   where rle xs = (length xs, head xs)

runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength
