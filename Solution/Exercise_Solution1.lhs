>{-# LANGUAGE GADTSyntax #-}

> module Exercise_Solution1 where
> import Prelude hiding (Left, Right)

> data Direction where
>    Up    :: Direction
>    Down  :: Direction
>    Left  :: Direction
>    Right :: Direction
>   deriving Show


> data Coordinate where
>   XYAxis :: Integer -> Integer -> Coordinate
>  deriving Show


1) Define two values of type `XYAxis`


> xyAxis1 :: Coordinate
> xyAxis1 = XYAxis 4 6

> xyAxis2 :: Coordinate
> xyAxis2 = XYAxis 23 6


2) Implement a function `eqDirection :: Direction -> Direction -> Bool` 
   that yields `True` if the both arguments are the same and `False` otherwise.
   The data type `Bool` is predefined as follows in Haskell.

> eqDirection :: Direction -> Direction -> Bool
> eqDirection Up Up        = True
> eqDirection Down Down    = True
> eqDirection Left left    = True
> eqDirection Right Right  = True
> eqDirection _ _         = False


3) Implement a function `isVertical :: Direction -> Bool`. Such functions with result type `Bool` are often called predicates. 
   The function shoud yield `True` for vertical direction and `False` otherwise.

> isVertical :: Direction -> Bool
> isVertical Up = True
> isVertical Down = True 
> isVertical _ = False


4) Declare a data type for an imaginary token tile that might be moved on a coordination system. 
   The data type `Token` should be defined analogue to `Direction` defined in the lecture: 
   the data type `Token` should have at least four different nullary constructors 
   and can represent a total of four values only.
   Also write a function `prettyToken :: Token -> String` 
   that yields a pretty(!) string representation for a token.

> data Token where
>   Dollar   :: Token
>   Question :: Token
>   Plus     :: Token
>   Minus    :: Token
>  deriving Show

> prettyToken :: Token -> String
> prettyToken Dollar   = "$"                          --error "prettyToken: Implement me!"
> prettyToken Question = "?"
> prettyToken Plus     = "+"
> prettyToken Minus    = "-"