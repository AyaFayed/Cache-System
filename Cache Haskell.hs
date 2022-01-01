import Data.Foldable (Foldable)

data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

splitEvery :: (Num a1, Ord a1) => a1 -> [a] -> [[a]]
splitEvery a (x:xs) = (splitEveryhelper a a (x:xs) [])

splitEveryhelper _ _ [] (c:b) = [(c:b)]
splitEveryhelper a curr (x:xs) y = if curr /= 0 then splitEveryhelper a (curr-1) xs (y++[x]) else [y]++(splitEveryhelper a a (x:xs) [])


logBase2 :: Floating a => a -> a
logBase2 x = logBase 2 x 

fillZeros :: (Eq a, Num a) => [Char] -> a -> [Char]
fillZeros (x:xs) a = if a /= 0 then fillZeros (['0']++(x:xs)) (a-1) else (x:xs)


convertBinToDec :: Integral a => a -> a
convertBinToDec x = convertBinToDechelper x 0
convertBinToDechelper x a = if x /= 0 then  ((mod x 10)*(power2 a)) + convertBinToDechelper (div x 10) (a+1) else 0

power2 :: Integral a => a -> a
power2 0 = 1
power2 x =  2 * (power2 (x-1))

intDivision :: Integral a => a -> a -> a
intDivision x s = if mod x s == 0 then div x s else (div x s) + 1

replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]		
replaceIthItem z (x:xs) a = replaceIthItemhelper z (x:xs) a 0 []
replaceIthItemhelper z (x:xs) a curr y = if a == curr then y++[z]++xs else (replaceIthItemhelper z xs a (curr+1) (y++[x]))


getNumBits :: (Floating a1, Integral a, RealFrac a1, Foldable t) => a1 -> [Char] -> t a2 -> a

getNumBits _ "fullyAssoc" _ = 0

getNumBits x "setAssoc" _ = ceiling (logBase2 x)

getNumBits x "directMap" l = ceiling (logBase2 x)

convertStringToInt :: String -> Int
convertStringToInt a = read a::Int

tenMultiplyer :: Integral a => a -> a
tenMultiplyer 0 = 1
tenMultiplyer a = (tenMultiplyer (a-1)) * 10

-- ****************** To get the index and tag individually from convertAddress ******************
getIndex :: (Int,Int) -> Int
getIndex (_,b) = b

getTag :: (Int,Int) -> Int 
getTag (a,_) = a

-- ****************** convertAddress for directMap and setAssoc ******************

convertAddress address n _ =((div address (10^n)),(mod address (10^n))) 

-- ************************** Direct Mapping **************************

checkVal t (It (T tag) (D a) b _) | t==tag && b==True = True
                                  | otherwise = False
getAns (It (T tag) (D a) True _) = Out ( a , 0 )
getDataFromCache address mem "directMap" n | (getIndex (convertAddress (convertStringToInt(address)) n "directMap")) > length mem = NoOutput
                                           |  checkVal (getTag (convertAddress (convertStringToInt(address)) n "directMap")) ( mem !! (getIndex (convertAddress (convertStringToInt(address)) n "directMap"))) = getAns ( mem !! (getIndex (convertAddress (convertStringToInt(address)) n "directMap")))
                                          |otherwise = NoOutput

-- ************************** Set Associative **************************
--getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a
getDataFromCache address arr "setAssoc" bitsNum  = searchInSet (getTag (convertAddress (convertStringToInt address) (bitsNum) "setAssoc"))
 ((splitEvery ((intDivision (length arr) (power2 bitsNum))) arr)!!(convertBinToDec (getIndex (convertAddress (convertStringToInt address) (bitsNum) "setAssoc")))) 0  

searchInSet :: Int -> [Item a] -> Int -> Output a
searchInSet _ [] _  =  NoOutput
searchInSet s ((It (T tag) (D d) boo ord ):xs) hops | s == tag = (if boo == True then Out (d, hops) else searchInSet s xs (hops+1))
													  | otherwise = searchInSet s xs (hops+1)



-- ****************** Not the same type like the mentioned one in the project ******************

--replaceInCache :: Integral a => Int -> a -> [b] -> [Item b] -> String -> Int -> (b,[Item b])

replaceInCache tag idx mem cache cacheType bitsNum | cacheType == "directMap" = ( (mem!!(merge tag idx bitsNum)) , (replaceIthItem (It (T tag) (D (mem!!(merge tag idx bitsNum))) True 0) cache (convertBinToDec idx)))
												   | cacheType == "fullyAssoc" = ( (mem!!(convertBinToDec tag)) , (replaceIthItem (It (T tag) (D (mem!!(convertBinToDec tag))) True 0) (updateOrd cache) (searchIdx cache)))
	     										   | otherwise = ((mem!!(merge tag idx bitsNum)), flatten (replaceIthItem (replaceInSet tag (mem!!(merge tag idx bitsNum)) (getSet idx cache (power2 bitsNum))) (splitEvery (intDivision (length cache) (power2 bitsNum)) cache) (convertBinToDec idx)) [] )

												   
merge tag idx numBits = convertBinToDec (convertStringToInt ((show tag)++(fillZeros (show idx) (numBits - length (show idx)))))

replaceInSet tag d set = (replaceIthItem (It (T tag) (D d) True 0) (updateOrd set) (searchIdx set))
getSet idx cache numOfsets = (splitEvery (intDivision (length cache) numOfsets) cache) !! (convertBinToDec idx) 

updateOrd [] = []
updateOrd ((It x y True ord ):xs) = ((It x y True (ord+1) ):(updateOrd xs))
updateOrd ((It x y False ord ):xs) = ((It x y False (ord) ):(updateOrd xs))

flatten [] acc = acc 
flatten (xs:ys) acc = flatten ys (acc ++ xs)

searchIdx l = if (containsFalse l 0) == (-1) then searchMax l 0 0 0 else containsFalse l 0

containsFalse [] _ = (-1)
containsFalse ((It _ _ False _ ):xs) curr= curr
containsFalse ((It _ _ True _ ):xs) curr= containsFalse xs (curr + 1)
 
searchMax [] _ _ maxIdx = maxIdx
searchMax ((It _ _ _ ord ):xs) maxSoFar curr maxIdx = if ord > maxSoFar then searchMax (xs) ord (curr+1) curr else searchMax (xs) maxSoFar (curr+1) maxIdx



getData stringAddress cache memory cacheType bitsNum
 | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
 | otherwise = (getX x, cache)
 where
 x = getDataFromCache stringAddress cache cacheType bitsNum
 address = read stringAddress:: Int
 (tag, index) = convertAddress address bitsNum cacheType
 getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numSets =
 ((d:prevData), finalCache)
 where
 bitsNum = round (logBase2 numSets)
 (d, updatedCache) = getData addr cache memory cacheType bitsNum
 (prevData, finalCache) = runProgram xs updatedCache memory cacheType numSets





