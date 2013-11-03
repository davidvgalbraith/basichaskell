import Data.List

doubleMe x = x + x

doubleUs x y = 2 * x + 2 * y

doubleSmallNumber x = if x > 100
		      	 then x
			 else x * 2

ftp = "Man, Arthur the chimpanzee!1"

bears x = [q | q <- x, q `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]

capital::String -> String
capital [] = "Walter the chimpanzee!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

zw  _ [] _ = []
zw  _ _ [] = []
zw f (x:xs) (y: ys) = (f x y): (zw f xs ys)

flp f = g where g x y = f y x

mapp f [] = []
mapp f (x: xs) = f x : mapp f xs

filterr _ [] = []
filterr p (x:xs)
       | p x = x : filterr p xs
       | otherwise = filterr p xs

chain 1 = [1]
chain x 
      | odd x = x: chain (3 * x + 1)
      | otherwise = x : chain (x `div` 2)

maxine [a] = a
maxine (x:xs)
       | x > maxine xs = x
       | otherwise = maxine xs

rtg x y = if x > y then x else y

maxion xs acc = foldl rtg acc xs

maxio xs = maxion xs 0

ravers = foldl (flip (:)) []

inters _ [] = []
inters x (y:[]) = (y:[])
inters x (y:xs) = y:x:inters x xs

interc _ [] = []
interc xs (y: []) = y
interc xs (y: ys) = y++xs++interc xs ys

gurk _ [] = []
gurk x (y:ys) 
     | x < length y = y !! x : gurk x ys
     |otherwise = gurk x ys

transpate x xs 
	  | x >= maxlen xs = []
	  | otherwise = (gurk x xs) : transpate (x + 1) xs

maxlen xs = maximum $ map length xs

transpos [] = []
transpos xs = transpate 0 xs

conqat [] = []
conqat (x:xs) = x ++ conqat xs

andd [] = True
andd (x:xs) = x && (andd xs) 

iteraate f x = x : (iteraate f $ f x)

splat 0 x = ["", x]
splat n xs
      | n < 0 = splat 0 xs
      | n > length xs = [xs, ""]
      | otherwise = [a, b]
      where a = frstn (n-1) xs
      	    b = bfn n xs

frstn 0 (x:xs) = x : []
frstn n (x:xs) = x : frstn (n-1) xs

bfn 0 xs = xs
bfn n (x:xs) = bfn (n-1) xs

takwl boo [] = []
takwl boo (x:xs)
      | boo x = x : takwl boo xs
      | otherwise = []

dropwl boo [] = []
dropwl boo (x:xs)
       | boo x = dropwl boo xs
       | otherwise = xs

bananas boo d (x:xs)
	| boo x = ((reverse d), (x:xs))
	| otherwise = bananas boo (x:d) xs
bananas boo d [] = (reverse d, [])
brake boo xs = bananas boo [] xs

srot [] = []
srot (x:xs) = garbage ++ [x] ++ trash
     where garbage = srot $ filter (<= x) xs
     	   trash = srot $ filter (> x) xs

grp [] = []
grp xs = gruup [] xs
gruup [] [] = []
gruup d (x:xs) 
      | d == [] = gruup [x] xs
      | head d == x = gruup (x:d) xs
      | xs == [] = [d, [x]]
      | otherwise = d: (gruup [] (x:xs))

ints d [] = [d]
ints d (x:xs) = d:ints (x:d) xs
iniits xs = map reverse $ ints [] xs

shave (x:[]) = []
shave (x:xs) = x:shave xs
tales [] = []
tales xs = xs : tales (shave xs)
tailles xs = conqat $ tales xs : [[[]]]

surch n (h:hs)
      | length hs < (length n) -1 = False
      | h : (take ((length n) -1) hs) == n = True
      | otherwise = surch n hs

parton a b = map ravers $ partin [] [] a b
partin a b [] _ = [a, b]
partin a b (x:xs) cond 
       | cond x = partin (x:a) b xs cond
       | otherwise = partin a (x:b) xs cond

noob x [] = x
noob x (y:ys) 
     | elem y x = noob x ys
     | otherwise = noob (y:x) ys
nuub x = ravers(noob [] x)

dalete x [] = []
dalete x (y:ys) = if y == x then ys else y : (dalete x ys)

crunch a [] = a
crunch as (b:bs) = crunch (dalete b as) bs

onion x [] = x
onion x (y:ys) = srot $ if (elem y x) then onion x ys else onion (y:x) ys

inersec a [] c = c
inersec a (x:xs) c = if (elem x a) then inersec a xs (x : c) else inersec a xs c
intersec a b = srot $ nuub $ inersec a b []

data Vector a = Vector a a a deriving (Show)

data BST a = Emty | Node a (BST a) (BST a) deriving (Show, Read, Eq)

singelton x = Node x Emty Emty

insurt x Emty = singelton x
insurt x (Node a lft rght) 
       | x == a = Node x lft rght 
       | x > a = Node a lft (insurt x rght) 
       | otherwise = Node a (insurt x lft) rght

contanes x Emty = False
contanes x (Node a lft rght) 
	 | x == a = True
	 | x > a = contanes x rght
	 | otherwise = contanes x lft

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
     deriving (Eq, Show, Read, Ord, Bounded, Enum)

data TrafficLight = Green | Yellow | Red deriving (Read)
instance Eq TrafficLight where
	 Red == Red = True
	 Green == Green = True
	 Yellow == Yellow = True
	 _ == _ = false
instance Show TrafficLight where
	 show Red = "Walter the Police!"
	 show Green = "Green light!! GUN IT BEFORE IT GOSE YELLO"
	 show Yellow = "Man, Arthur the chimpanzee, that Macbeth was yellow."
true = True
false = False
nothing = Nothing
class Boo a where
      boo :: a -> Bool

instance Boo Int where
	 boo 0 = False	
	 boo _ = True

class YesNo a where
      yesno :: a -> Bool

instance YesNo Int where
	 yesno 0 = False
	 yesno _ = True

wreck 0 = "Walter the chimpanzee"
wreck 1 = "I hate the Arthuring chimpanzee"

treetolist Emty = []
treetolist (Node a x y) = a : (treetolist x ++ (treetolist y))

class Tofu t where
      tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
	 tofu x = Frank x

data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
	 fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

main = do 
     putStrLn "Walter the chimpanzee!"
     garbage <- getLine
     putStrLn garbage
isop :: String -> Bool
isop x = x == "+" || x == "-" || x == "*"
garph :: [(String, Int -> Int -> Int)]
garph = [("+", plus), ("-", minus), ("*", times)]
plus a b = a + b
minus a b = a - b
times a b = a * b
slash a b = a / b
insirt :: Int -> String -> [String] -> [String]
insirt 0 x xs = x:xs
insirt n x (y:ys) = y:(insirt (n-1) x ys)
getop x = lookup x garph
polish :: String -> Int
polish x = read $ head $ beam $ words x
beam::[String] -> [String]
beam (x:[]) = (x:[])
beam (x:y:z:xs) = if isop z then
                    let a = ra $ getop z
                        b = read x
                        c = read y
                    in
                     beam ((show (a b c)) : xs)
                  else
                    beam (x:(beam (y:z:xs)))
beam x = x
ra (Just x) = x
rpn :: ( Num a , Read a ) => String -> a
rpn = head . foldl f [] . words 
  where
    f (x:y:xs) "+" = (x+y):xs
    f (x:y:xs) "*" = (x*y):xs
    f (x:y:xs) "-" = (y-x):xs
    f x num = read num:x
  
solveRPN :: ( Num a , Read a ) => String -> a
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction ( x : y : ys ) "*" = ( x * y ): ys
    foldingFunction ( x : y : ys ) "+" = ( x + y ): ys
    foldingFunction ( x : y : ys ) "-" = ( y - x ): ys
    foldingFunction xs numberString = read numberString : xs
    
rtl z y = if z > y then y else z
sp [] = 0
sp x = rtl (sph x 0 1) (sph x 0 2)
sph [] n _ = n
sph (x:y:z:zs) n 1 = rtl (sph zs (n + x) 1) (sph zs (n + x + z) 2)
sph (x:y:z:zs) n 2 = rtl (sph zs (n + y) 2) (sph zs (n + y + z) 1)
b = [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0]
c = [1, 2, 3]