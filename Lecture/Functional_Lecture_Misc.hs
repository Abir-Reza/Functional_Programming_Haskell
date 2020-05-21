{-# LANGUAGE GADTSyntax #-}

module Functional.Lecture.Misc where

reverseString :: String -> String
reverseString ""      = ""
reverseString (c:str) = reverseString str ++ [c]

reverseString2 :: String -> String
reverseString2 str = reverse str
-- reverse :: [] a -> [] a


-- const :: a -> b -> a
-- const x _ = x

-- id :: a -> a
-- id x = x

{-

  $> const [True,False] 42
           ^^^^^^^^^^^  ^^^
            :: [Bool]    :: Int
     ^^^^^^^^^^^^^^^^^^^^^^
         :: [Bool]
-}

-- foldr :: (a -> b -> b) -> b -> [] a -> b
-- they all compute the same value
ex1 = foldr (\c res -> if c == 'b' then res else c:res) [] "abcb"
ex1' = foldr (\c res -> (if c == 'b' then id else (\list -> c:list)) res) [] "abcb"
ex1'' = foldr (\c res -> (if c == 'b' then id else (c:)) res) [] "abcb"
{-

foldr f e [] = e
foldr f e (x:xs) = f x (foldr f e xs)

   foldr (\c res -> (if c == 'b' then id else (c:)) res) [] "ab"
 = (if 'a' == 'b' then id else ('a':))
    (foldr (\c res -> (if c == 'b' then id else (c:)) res) [] "b")
 = (if 'a' == 'b' then id else ('a':)) ((if 'b' == 'b' then id else ('b':)) [])
 = ('a':) ((if 'b' == 'b' then id else ('b':)) [])
 = (\list -> 'a': list) ((if 'b' == 'b' then id else ('b':)) [])
 = 'a' : ((if 'b' == 'b' then id else ('b':)) [])
 = 'a' : (id [])
 = 'a' : []
 = "a"

-}

ex2 = foldr (\c res -> if c == 'b' then "" else c:res) [] "acbcb"
ex2' = foldr (\c res -> (if c == 'b' then const "" else (\list -> c:list)) res) [] "abcb"
ex2'' = foldr (\c res -> (if c == 'b' then const "" else (c:)) res) [] "abcb"

plus1 x = x + 1
onePlus x = 1 + x


loop = loop

{-

(+) :: Int -> (Int -> Int)
map :: (a  ->      b)       -> [] a -> [] b

:t map (+)
    a ~ Int
    b ~ (Int -> Int)

[] Int -> [] (Int -> Int)

:t map (+) [1,2,3,4,5]
[] (Int -> Int)

map (+) [1,2,3,4,5]
~ [(+) 1, (+) 2, (+) 3, (+) 4, (+) 5]
-}

exInt :: Int
exInt = 42

doSomething :: ([] (Int -> Int)) -> Int -> [] Int
doSomething [] int                 = []
doSomething (funInt : funInts) int = funInt int : doSomething funInts int

someFunctions :: [] (Int -> Bool)
someFunctions = [\x -> True, (> 4), (== 2), (>) 4,          (4 >)]
                         -- \x -> x > 5    \x -> (>) 4 x    \x -> 4 > x
                         --                \x -> 4 > x
-- conjunction : )      -- and   "Ver'und'ung"    und
-- disjunction : )      -- or    "Ver'oder'ung"  oder

andList :: [Bool] -> Bool
andList []        = True
andList (b:bools) = b && andList bools

fulfills :: Int -> [] (Int -> Bool) -> Bool
fulfills int conditions = andList (map (\pred -> pred int) conditions)

data Color where
  Red   :: Color
  Blue  :: Color
  Green :: Color
 deriving Show

type IntMap = [] ((,) Int Color)

intMapEx1 :: IntMap
intMapEx1 = [ (4, Red), (42, Blue), (13, Green) ]

lookupColor :: IntMap -> Int -> Maybe Color
lookupColor [] _ = Nothing
lookupColor ((intVal, color) : intMap) searchVal =
  if intVal == searchVal
     then Just color
     else lookupColor intMap searchVal

type IntMapFun = Int -> Maybe Color

intMapFunEx1 :: IntMapFun
-- intMapFunEx1 :: Int -> Maybe Color
intMapFunEx1 4  = Just Red
intMapFunEx1 42 = Just Blue
intMapFunEx1 13 = Just Green
intMapFunEx1 _  = Nothing

lookupFunColor :: IntMapFun -> Int -> Maybe Color
lookupFunColor intMap int = intMap int
          -- intMap :: IntMapFun
          -- intMap :: Int -> Maybe Color

insertMapFun :: IntMapFun -> Int -> Color -> IntMapFun
-- insertMapFun :: IntMapFun -> Int -> Color -> (Int -> Maybe Color)
insertMapFun intMap newVal newColor =
  \val -> if val == newVal then Just newColor
                           else intMap val

deleteMapFun :: IntMapFun -> Int -> IntMapFun
-- insertMapFun :: IntMapFun -> Int -> Color -> (Int -> Maybe Color)
deleteMapFun intMap delVal =
  \val -> if val == delVal then Nothing
                           else intMap val
