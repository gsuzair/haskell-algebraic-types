-- 6.1.1.  The simplest sort of algebraic type is defined by enumeration. For example:
-- data Season = Spring | Summer | Autumn | Winter
-- deriving (Eq, Show, Enum)
-- Program a constant function theSeasons of type [Season] containing all of the seasons in order,
-- starting with Spring

-- Enum a => a -> [a] -> on hoogle -> enumFrom :: a -> [a] can be used for this question

main :: IO ()
main = do
  let result = theSeasons Spring  
  print result

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Show, Enum)
              
theSeasons :: Season -> [Season]
theSeasons n = enumFrom n


-- 6.1.2  Give a function seasonsFrom that receives a season as an input and returns a list of seasons starting
-- from the input parameter and finishing in Winter. For example, seasonsFrom Autumn produces
-- [Autumn, Winter] and seasonsFrom Summer computes [Summer, Autumn, Winter].
-- Solve the problem two times, once using the function you found on Hoogle earlier, and once using
-- the syntactic sugar for arithmetic sequences (like [1..100]).

main :: IO ()
main = do
  let result = theSeasons Autumn  
  print result

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Show, Enum)
              
theSeasons :: Season -> [Season]
theSeasons n = enumFrom n

-- second approach:
-- theSeasons n = [n ..]

-- third approach:
main :: IO ()
main = do
  let result = toEnumSeason 12 
  print result

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Show)
              
instance Enum Season where 
  toEnum 0 = Winter 
  toEnum 1 = Autumn 
  toEnum 2 = Summer
  toEnum 3 = Spring
  toEnum n = toEnum (n `mod` 4)

  fromEnum Winter = 0 
  fromEnum Autumn = 1
  fromEnum Summer = 2
  fromEnum Spring = 3
  
toEnumSeason:: Int -> Season
toEnumSeason = toEnum

-- Would toEnumSeason work without fromEnum?
-- Yes, it would work because toEnumSeason only depends on toEnum. However, defining fromEnum makes your implementation more complete and allows you to use season values in numerical operations or other functions that rely on Enum.
-- So, while fromEnum is not directly needed for your specific function, it's useful for consistency and future flexibility.

-- 6.1.3 Give a function mapSeasonsFrom that receives a list of seasons and returns a list of a list of seasons
-- where each list is computed using the function seasonsFrom starting from each of the elements of
-- the input parameter.
-- Some examples follow:
-- > mapSeasonsFrom [Summer] ==> [[Summer, Autumn, Winter]]
-- > mapSeasonsFrom [Winter, Summer] ==> [[Winter], [Summer, Autumn, Winter]]
-- > mapSeasonsFrom [Autumn, Winter, Summer]
-- ==> [[Autumn, Winter], [Winter], [Summer, Autumn, Winter]]


main :: IO ()
main = do
  let result = mapSeasonsFrom [Spring, Autumn, Summer, Winter]
  print result

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Show, Enum)
  
mapSeasonsFrom:: [Season] -> [[Season]]
mapSeasonsFrom [] = []
mapSeasonsFrom (x:xs) = [enumFrom x] ++ mapSeasonsFrom xs

-- 6.1.4 Define the type of months as a Haskell algebraic type. The elements of this type are Jan, Feb,
-- ..., Dec. Derive instances for Eq, Ord, Show and Enum.
-- What type would the function have that converts a month to the integer representing its number
-- in the year (e.g. 6 for June)? Use Hoogle to find the general function that converts an instance of
-- Enum into an integer, and the function that converts an integer to an instance of Enum. (Be careful
-- with the latter, it throws an exception if you specify an integer outside of the range!)

-- enum to int
main :: IO ()
main = do
  let result = enumToInt April
  print result

data Months = January | February | March | April | May | June | July | August | September | October | November | December
              deriving (Eq, Show, Enum, Ord)

enumToInt :: Months -> Int
enumToInt month = fromEnum month

-- int to enum
main :: IO ()
main = do
  let result = enumToInt 3
  print result

data Months = January | February | March | April | May | June | July | August | September | October | November | December
              deriving (Eq, Show, Enum, Ord)

enumToInt :: Int -> Months
enumToInt month = toEnum month

-- 6.1.5. Implement a function monthFromTo that takes two months as arguments and returns all months
-- in the interval, including the arguments. Write it in the same syntactic sugar you know from
-- arithmetic sequences (e.g. [3..8]).

main :: IO ()
main = do
  let result = monthFromTo January June
  print result

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Show, Enum)
data Months = January | February | March | April | May | June | July | August | September | October | November | December
              deriving (Eq, Show, Enum, Ord)

monthFromTo :: Months -> Months -> [Months]
monthFromTo a b = [a .. b]

-- 6.1.6 Write a function monthToSeason which takes a month to its appropriate season. Assume that Mar,
-- Apr, May correspond to Spring; Jun, Jul, Aug correspond to Summer; Sep, Oct, Nov correspond
-- to Autumn and Jan, Feb, Dec correspond to Winter.
-- For example,
-- > monthToSeason Jan ==> Winter
-- > monthToSeason Jul ==> Summer

main :: IO ()
main = do
  let result = monthToSeason January
  print result  -- Expected Output: Winter

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq, Show, Enum)

data Months = January | February | March | April | May | June | July | August | September | October | November | December
              deriving (Eq, Show, Enum, Ord)

monthToSeason :: Months -> Season
monthToSeason month
  | month `elem` [March, April, May] = Spring
  | month `elem` [June, July, August] = Summer
  | month `elem` [September, October, November] = Autumn
  | otherwise = Winter  -- Covers December, January, February

-- 6.1.7. The Boolean type can be defined as follows:
-- data MyBoolean = MyFalse | MyTrue
-- deriving Show
-- Present a function boolToMyBoolean that given a built-in Boolean returns the corresponding result
-- of MyBoolean type. That is, if the input parameter is False the function produces MyFalse and if
-- the input parameter is True the function returns MyTrue.
-- Also define the inverse of this function, myBooleanToBool.

main :: IO ()
main = do
  let result = boolToMyBoolean True
  print result  -- Expected Output: MyTrue

data MyBoolean = MyFalse | MyTrue
                 deriving Show

boolToMyBoolean :: Bool -> MyBoolean
boolToMyBoolean True  = MyTrue
boolToMyBoolean False = MyFalse

-- 6.1.8. Define operators &:& and |:| that perform respectively the logical “and” and the logical “or” on
-- the MyBoolean type. Use underscores (_) for a very concise definition.

main :: IO ()
main = do
  let result = MyTrue &:& MyFalse |:| MyTrue
  print result  -- Expected Output: MyTrue
  
data MyBoolean = MyFalse | MyTrue
                 deriving Show

infixr 3 &:&
infixl 6 |:|

(&:&) :: MyBoolean -> MyBoolean -> MyBoolean
(&:&) MyTrue MyTrue = MyTrue
_      &:& _        = MyFalse

(|:|) :: MyBoolean -> MyBoolean -> MyBoolean
MyFalse |:| MyFalse = MyFalse
_       |:| _       = MyTrue

-- 6.1.9. Implement the functions myAnd and myOr that perform respectively the logical “and” and the logical
-- “or” on a list of MyBoolean. myAnd of an empty list should be myTrue and myOr of an empty list is
-- myFalse.

main :: IO ()
main = do
  let result = myOr [MyTrue, MyFalse, MyTrue, MyTrue]
  print result  -- Expected Output: MyTrue
  
data MyBoolean = MyFalse | MyTrue
                 deriving Show

myAnd::[MyBoolean] -> MyBoolean
myAnd [] = MyTrue
myAnd (MyTrue : myBooleans) =  myAnd myBooleans
myAnd (MyFalse : _)         =  MyFalse

myOr::[MyBoolean] -> MyBoolean
myOr [] = MyFalse
myOr (MyFalse : myBooleans) =  myOr myBooleans
myOr (MyTrue : _)         =  MyTrue

-- with map:
-- myAnd' bs =  boolToMyBoolean (and (map myBooleanToBool bs))

-- with folding:
-- myAnd'' :: [MyBoolean] -> MyBoolean
-- myAnd'' =  foldr (&:&) MyTrue

-- myOr'' :: [MyBoolean] -> MyBoolean
-- myOr'' =  foldr (|:|) MyFalse

-- 6.1.10 Define an algebraic type for bits (called Bit), such that the 0 is represented by the letter O (i.e.,
-- capital o) and the 1 is represented by the letter I (i.e., capital i). Arrange the constructors in the
-- definition such that fromEnum gives just the right integer value. Then define a function bitsToInt
-- of type [Bit] -> Int that translates the list of bits into the equivalent integer representation.
-- For instance,
-- > bitsToInt [] ==> 0
-- > bitsToInt [I,O,I] ==> 5
-- > bitsToInt [O,I,I,I] ==> 7

data Bit = O | I
    deriving (Show, Enum)

bitsToInt :: [Bit] -> Int
bitsToInt =  bitsToInt' 0
  where
    bitsToInt' n []     = n
    bitsToInt' n (b:bs) = bitsToInt' (2*n + fromEnum b) bs

-- bitsToInt' 0 [O, I, I, O]
-- bitsToInt' (2*0 + 0) [I, I, O]  -- n = 0
-- bitsToInt' (2*0 + 1) [I, O]     -- n = 1
-- bitsToInt' (2*1 + 1) [O]        -- n = 3
-- bitsToInt' (2*3 + 0) []         -- n = 6

-- 6.2.1  Consider the sum abstract data type:
-- data Number = Exact Int | Approx Float
-- Define a function rounded that receives a Number and returns an integer. If the input parameter
-- is an exact number (i.e., it has been constructed using Exact) the integer value corresponding to
-- this number is returned. Otherwise, if the input parameter is an approximate number (i.e., it has
-- been constructed using Approx) the value returned is the result of applying the built-in function
-- round to the real number employed in the construction.
-- For example,
-- > rounded (Exact 10) ==> 10
-- > rounded (Approx 10.9) ==> 11
-- > rounded (Approx (-23)) ==> -23

main :: IO ()
main = do
  let result = rounded (Approx 1.9)
  print result

data Number = Exact Int | Approx Float
  deriving (Show)

rounded :: Number -> Int
rounded (Exact i)  = i
rounded (Approx f) = round f

-- 6.2.2 Instead of using a tuple we can define a type with a number of components, often called a product
-- type. For example, consider the product types:
-- data Age = Years Int
-- deriving Show
-- data Name = Name String String
-- deriving Show
-- data Person = Person Name Age
-- deriving Show
-- and the function:
-- firstName :: Person -> String
-- firstName (Person (Name first family) _) = first
-- thus:
-- > firstName (Person (Name "Ada" "Lovelace") (Years 36)) ==> "Ada"
-- Note that the name for the type (e.g. Person) can coincide with the name for the constructor (also
-- Person). This is because Haskell will always be able to tell from the context whether you mean
-- the type or the constructor. Thus it is not only valid Haskell code to give type and constructor
-- the same name in case of a product type, but it is also usual and recommended. We will therefore
-- keep this convention for the rest of the exercise sheet. 
-- Define the functions howOld and addAges (and their type signatures) that respectively return the
-- age in Years of a given person and compute the sum of the ages of two persons. For the function
-- addAges is required the result to be of type Age.
-- For example,
-- > howOld (Person (Name "Haskell" "Curry") (Years 81)) ==> Years 81
-- > addAges (Person (Name "A" "L") (Years 10))
-- (Person (Name "X" "Y") (Years 12)) ==> Years 22

main :: IO ()
main = do
  let result = addAges (Person (Name "A" "L") (Years 10)) (Person (Name "X" "Y") (Years 12))
  print result

data Age = Years Int
           deriving Show
data Name = Name String String
            deriving Show
data Person = Person Name Age
              deriving Show

howOld :: Person -> Age
howOld (Person _ age) = age

addAges :: Person -> Person -> Age
addAges (Person _ (Years age1)) (Person _ (Years age2)) = Years (age1 + age2)

-- 6.3.1  Handling Geometric Shapes (A long exercise for self-study): Now we combine
-- Sum and Product types. Let us say that a Shape is either a Circle or a Rectangle. Thus, there are
-- two ways of building an element of type Shape. One form is to supply the radius of a Circle, the other
-- alternative is to give the length of the two sides of a Rectangle.
-- Define the type Shape as an algebraic type.
-- 6.3.2 Give a predicate isRound of type Shape -> MyBoolean that returns True if the input parameter
-- is a Circle and False otherwise (The type MyBoolean was defined in Exercise 4.1.7).

main :: IO ()
main = do
  let result = isRound (Circle pi)
  print result

data Shape = Circle Float 
           | Rectangle Float Float
           deriving Show

data Point = Point Float Float
            deriving Show

data Slope = Value Float | Infinite
            deriving Show

data YIntercept = Intercept Float
                  deriving Show

data Figure = Figure Shape Point
            deriving Show


isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle m o)
      | m == 0 = True 
      | otherwise = False

-- 6.3.3 Program a function to give the area of a geometrical object of type Shape.
-- For instance,
-- > getArea (Rectangle 16.9 68) ==> 1149.2
      
getArea :: Shape -> Float
getArea (Circle r) = pi * r^2
getArea (Rectangle l w) = l * w

-- 6.3.4 Points in the Cartesian plane (i.e., R^2) can be defined as follows:
-- data Point = Point Float Float
-- deriving Show
-- Give a function dist that given two points in R^2
-- computes the length of the shortest line between them. Recall that for two points (x1, y1) and (x2, y2) this distance can be computed by the formula:
-- (y2 − y1)
-- 2 + (x2 − x1)
-- 2.
-- For instance,
-- > dist (Point 4 6) (Point 9 25) ==> 19.64688

dist ::  Point -> Point -> Float
dist (Point x1 y1) (Point x2 y2) = sqrt((y2^2 - y1^2) + (x2^2 - x1^2))

-- 6.3.5 The slope of a line in the plane containing the x and y axes is generally represented by the letter
-- m, and is defined as the change in the y-coordinate divided by the corresponding change in the
-- x-coordinate, between two distinct points on the line. Given two points (x1, y1) and (x2, y2), the
-- change in x from one to the other is x2 − x1, while the change in y is y2 − y1. Thus:
-- m =
-- y2 − y1
-- x2 − x1
-- Observe that the slope of two points can be infinite when the x-coordinate of both points is the
-- same (i.e., when x1 = x2).

getSlope :: Point -> Point -> Slope
getSlope (Point x1 y1) (Point x2 y2) 
  | x1 == x2 = Infinite
  | otherwise = Value ((y2 - y1) / (x2 - x1))

-- 6.3.6 Lines in a Cartesian plane can be described algebraically by linear equations and linear functions. In
-- two dimensions, the characteristic equation is often given by the slope-intercept form: y = mx + b,
-- where m is the slope of the line, b is the y-intercept of the line and x is the independent variable
-- of the function.
-- The problem is that of finding the y-intercept from a given point (x, y) and slope m, which clearly
-- can be obtained by: b = y − mx if the slope is finite. Otherwise, if the slope is infinite the line
-- defined by (x, y) and m is parallel to the y-axis. Define an algebraic type YIntercept that either
-- represents a value in the y-axis when the Slope is finite (i.e., it has the form Intercept v for
-- some v of type Float) or it is Undefined when the Slope is Infinite. Then program the function
-- getYIntercept of type Point -> Slope -> YIntercept for computing the y-intercept.
-- For instance,
-- > getYIntercept (Point 5 5) (Value 1) ==> Intercept 0.0
-- > getYIntercept (Point (-5) 25) (Value 0.5) ==> Intercept 27.5
-- > getYIntercept (Point 1 1) Infinite ==> Undefined


getYIntercept::Point -> Slope -> YIntercept
getYIntercept (Point x y) (Value m) = Intercept (y - m * x)
getYIntercept _ Infinite = error "Cannot compute y-intercept for a vertical line"

-- 6.3.7 Define an algebraic type Figure for geometrical objects that extends the information contained the
-- type Shape. Thus, a Figure can be either a circle or a rectangle that in addition to the constructor’s
-- parameters defined in Shape (i.e., the radius for the circle and the length of the two sides for a rectangle), must contain the center of the object represented as a Point. Call the constructor of
-- this example Figure as well. Include deriving Show in your definition.
-- The type Figure does not take account of the orientation of an object, so let us assume from now
-- on that all rectangles lie with the longest of its two sides parallel to the x-axis, and the other side
-- parallel to the y-axis. If the two sides are of the same length, obviously, the rectangle lies with its
-- sides parallel to the axes.
-- Define a function move of type Float -> Float -> Figure -> Figure which moves a geometrical
-- object of type Figure by the two offsets given.
-- For example,
-- > move 1 2 (Figure (Circle pi) (Point 10 10))
-- ==> Figure (Circle 3.141593) (Point 11.0 12.0)
-- > move 10 (-10) (Figure (Rectangle 3 4) (Point 0 0))
-- ==> Figure (Rectangle 3.0 4.0) (Point 10.0 (-10.0))

move :: Float -> Float -> Figure -> Figure
move f1 f2 (Figure (Circle r) (Point x y)) = Figure (Circle r) (Point (x + f1) (y + f2))
move f1 f2 (Figure (Rectangle l w) (Point x y)) = Figure (Rectangle l w) (Point (x + f1) (y + f2))

-- 6.3.8 Define a function overlap to test whether two objects of type Figure overlap.
-- Hint: For this you may need to define some auxiliary functions. The case for two circles is easy.
-- Defining overlap for rectangles is a bit harder.
-- You can try the following approach:
-- Let’s define overlapping for two rectangles. Define a function leftOf, that takes the data of two
-- rectangles (height, width and position), and determines whether the first rectangle is completely
-- to the left of the second rectangle, i.e. whether all points of the first have a smaller x-coordinate
-- than the smallest x-coordinate of the second. Define functions rightOf, above and below in a
-- corresponding way and note that two rectangles overlap if the first one is neither left of, right of,
-- above or below the other rectangle.
-- To define overlapping for circles and rectangles, first divide the plane into 9 parts by extending the
-- sides of the rectangle: Interiour, the infinite strip directly above, the quarter plane above left, the
-- infinite strip directly to the left, and so on. Find out, in which of the parts the centre of the circle
-- is. For each part, there is an easy overlapping detection function which you should be able to find.
-- For example,
-- > overlap (Figure (Circle 1.414213) (Point 0 0))
-- (Figure (Circle 1.414213) (Point 2 2)) ==> False
-- > overlap (Figure (Rectangle 2 8) (Point 5 0))
-- (Figure (Circle 1) (Point 0 0)) ==> True
-- > overlap (Figure (Circle 1.414214) (Point 0 0))
-- (Figure (Circle 1.414214) (Point 2 2)) ==> True

overlap :: Figure -> Figure -> Bool
overlap (Figure (Circle r1) (Point x1 y1)) (Figure (Circle r2) (Point x2 y2))
    | dist (Point x1 y1) (Point x2 y2) <= r1 + r2 = True
    | otherwise = False
    
overlap (Figure (Rectangle w1 h1) (Point x1 y1)) (Figure (Rectangle w2 h2) (Point x2 y2))
    | x1 + w1 < x2  || x2 + w2 < x1  = False  -- One rectangle is completely to the left of the other
    | y1 + h1 < y2  || y2 + h2 < y1  = False  -- One rectangle is completely below the other
    | otherwise = True   

-- 6.4.1 In the lecture the following algebraic type has been introduced:
-- data Expr = Lit Int |
-- Add Expr Expr |
-- Sub Expr Expr
-- Also in the lecture the following function for evaluating expressions has been presented:
-- eval :: Expr -> Int
-- eval (Lit n) = n
-- eval (Add e1 e2) = (eval e1) + (eval e2)
-- eval (Sub e1 e2) = (eval e1) - (eval e2)
-- Define a function size which counts the number of operations in an expression.
-- For instance,
-- > size (Lit 10) ==> 0
-- > size (Sub (Add (Lit 10) (Lit 5)) (Lit 1)) ==> 2
--  Add the operations of multiplication and integer division to the type Expr, and redefine the functions eval and size to include the new cases.

main :: IO ()
main = do
  let result =   size (Sub (Add (Lit 10) (Lit 5)) (Lit 1))
  print result
  
data Expr = Lit Int |
  Add Expr Expr |
  Sub Expr Expr |
  Div Expr Expr |
  Mul Expr Expr

size :: Expr -> Int
size (Lit _) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Div e1 e2) = 1 + size e1 + size e2

-- 6.4.3 Instead of adding extra constructors to the Expr type (as in the previous question) it is possible to
-- factor the definition as follows:
-- data Expr = Lit Int
-- | OpExp Op Expr Expr
-- deriving Eq
-- data Op = Add | Sub | Mul | Div
-- deriving Eq
-- Show how the functions eval and size are defined for this type. Then add the operation Mod.

main :: IO ()
main = do
  let result =   size (OpExp Sub (OpExp Add (Lit 10) (Lit 5)) (Lit 1))
  print result
  
data Expr = Lit Int
  | OpExp Op Expr Expr
  deriving Eq

data Op = Add | Sub | Mul | Div
  deriving Eq
  
eval :: Expr -> Int
eval (Lit n) = n
eval (OpExp Add e1 e2) = eval e1 + eval e2
eval (OpExp Sub e1 e2) = eval e1 - eval e2
eval (OpExp Mul e1 e2) = eval e1 * eval e2
eval (OpExp Div e1 e2) = eval e1 `div` eval e2 


size :: Expr -> Int
size (Lit _) = 0
size (OpExp _ e1 e2) = 1 + size e1 + size e2

-- 6.5.1 Mutually Recursive Types: It is possible to extend the type Expr so that it contains
-- conditional expressions, IF b e1 e2, where e1 and e2 are expressions, and b is a Boolean expression.
-- Thus:
-- data IExpr = ILit Int |
-- ADD IExpr IExpr |
-- SUB IExpr IExpr |
-- MUL IExpr IExpr |
-- MOD IExpr IExpr |
-- IF BExp IExpr IExpr
-- The expression IF b e1 e2 evaluates to the value of e1 if b is True and otherwise it has the value of e2.
-- 1. First the syntax. Define an algebraic type for BExp such that:
--  Boolean literals are BLit True and BLit False.
--  The conjunction of two Boolean expressions (AND) is True if both expressions have the value
-- True and it is False otherwise.
--  The disjunction of two Boolean expressions (OR) is False if both expressions have the value
-- False and it is True otherwise.
--  The negation of a Boolean expression (NOT) is True if the expression has the value False and
-- it is True otherwise.
--  The comparison of two integer expressions (EQUAL) is True when the two numerical expressions
-- have equal values and it is False otherwise.

main :: IO ()
main = do
  print $ bEval (Equal (ILit 1) (SUB (ILit 2) (ILit 1))) -- True
  print $ iEval (IF (Equal (ILit 10) (ILit 10)) (ADD (ILit 1) (ILit 2)) (ILit 0)) -- 3
  print $ bEval (Not (Equal (ILit 10) (SUB (ILit 11) (ILit 1)))) -- False
  print $ iEval (IF (Not (Equal (ILit 3) (SUB (ILit 4) (ILit 1)))) (ILit 1) (ILit 10)) -- 10

  
-- Integer Expressions
data IExpr
  = ILit Int
  | ADD IExpr IExpr
  | SUB IExpr IExpr
  | MUL IExpr IExpr
  | MOD IExpr IExpr
  | IF BExpr IExpr IExpr
  deriving Show

-- Boolean Expressions
data BExpr
  = BLit Bool
  | And BExpr BExpr
  | Or BExpr BExpr
  | Not BExpr
  | Equal IExpr IExpr
  deriving Show

-- Evaluating Integer Expressions
iEval :: IExpr -> Int
iEval (ILit n) = n
iEval (ADD e1 e2) = iEval e1 + iEval e2
iEval (SUB e1 e2) = iEval e1 - iEval e2
iEval (MUL e1 e2) = iEval e1 * iEval e2
iEval (MOD e1 e2) = iEval e1 `mod` iEval e2
iEval (IF cond e1 e2) =
  if bEval cond
    then iEval e1
    else iEval e2

-- Evaluating Boolean Expressions
bEval :: BExpr -> Bool
bEval (BLit b) = b
bEval (And b1 b2) = bEval b1 && bEval b2
bEval (Or b1 b2) = bEval b1 || bEval b2
bEval (Not b) = not (bEval b)
bEval (Equal e1 e2) = iEval e1 == iEval e2


-- 6.6.1 Polymorphic Types: Algebraic type definitions can contain type variables (e.g., a, b
-- and so on), defining polymorphic types. The definitions are as before, with the type variable used in the
-- definition appearing after the type name on the left-hand side of the definition.
-- 1. Consider the following polymorphic algebraic type for pairs:
-- data Pair a = Pair a a
-- Define functions to swap the two components of a pair (swapPair) and to test the equality of the
-- two components (eqPair).
-- For example,
-- > swapPair (Pair ’a’ ’b’) ==> Pair ’b’ ’a’
-- > swapPair (Pair (iEval (ILit 1)) (iEval (ILit 2))) ==> Pair 2 1
-- > eqPair (Pair "Haskell" "Type") ==> False
-- > eqPair (Pair False False) ==> True

data Pair a = Pair a a
              deriving Show

swapPair :: Pair a -> Pair a
swapPair (Pair a b) = Pair b a

eqPair :: Eq a => Pair a -> Bool
eqPair (Pair a b) = a == b

-- 6.6.2 Consider the following polymorphic definition for lists:
-- data List a = EmptyList | Cons a (List a)
-- deriving (Eq,Ord,Show,Read)
-- Define functions to test whether a given list is empty (isEmpty) and to compute the length (in
-- number of elements) of a list (lengthOfList).
-- For example,
-- > isEmpty EmptyList ==> True
-- > isEmpty (Cons 2 (Cons 1 EmptyList)) ==> False
-- > lengthOfList (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 EmptyList))))) ==> 5

main :: IO ()
main = do
  let result =  lengthOfList (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 EmptyList)))))
  print result

data List a = EmptyList | Cons a (List a)
    deriving (Eq, Ord, Show, Read)

isEmpty :: List a    -> Bool
isEmpty    EmptyList =  True
isEmpty    _         =  False

lengthOfList :: List a    -> Int
lengthOfList EmptyList =  0
lengthOfList (Cons _ list) =  1 + lengthOfList list

-- 6.7.1 Polymorphic Binary Trees: In the lecture, binary trees have been defined as follows:
-- data Tree a = Empty |
-- Leaf a |
-- Node a (Tree a) (Tree a)
-- deriving Show
-- 1. Define a function howMany for computing the number of elements of a tree.
-- For example,
-- > howMany Empty ==> 0
-- > howMany (Node ’a’ (Node ’b’ (Leaf ’c’) Empty) (Leaf ’d’)) ==> 4

main :: IO ()
main = do
  let result = howMany (Node 'a' (Node 'b' (Leaf 'c') Empty) (Leaf 'd'))
  print result

data Tree a = Empty |
              Leaf a |
              Node a (Tree a) (Tree a)
              deriving Show

howMany :: Tree a              -> Int
howMany    Empty               =  0
howMany   (Leaf _)             =  1
howMany   (Node _ tree1 tree2) =  1 + howMany tree1 + howMany tree2

-- max (depth tree1) (depth tree2) ensures that we take the larger depth between the two subtrees

-- 6.7.2 Define a function depth which obtains the depth of a given tree. Recall that the depth of an empty
-- tree is 0. The depth of a node is the length of the (shortest) path from the root to that node. The
-- depth of a tree is the maximum depth of a node of the tree.
-- For example,
-- > depth (Leaf 100) ==> 1
-- > depth (Node ’a’ (Node ’b’ (Leaf ’c’) Empty) (Leaf ’d’)) ==> 3

depth :: Tree a -> Int
depth    Empty  =  0
depth   (Leaf _) = 1
depth   (Node _ tree1 tree2) = 1 + max (depth tree1) (depth tree2)

-- 6.7.3 A tree is reflected by swapping left and right subtrees recursively. Define a function reflect to
-- reflect a tree.
-- For instance,
-- > reflect (Node ’a’ (Node ’b’ (Leaf ’c’) Empty) (Leaf ’d’))
-- ==> Node ’a’ (Leaf ’d’) (Node ’b’ Empty (Leaf ’c’))

reflect :: Tree a -> Tree a
reflect Empty = Empty
reflect (Leaf n) = Leaf n
reflect (Node a tree1 tree2) = Node a (reflect tree2) (reflect tree1)

-- 6.7.4 In the lecture a function for tree traversal has been defined as follows:
-- traversal :: Tree a -> [a]
-- traversal Empty = []
-- traversal (Leaf x) = [x]
-- traversal (Node x l r) = traversal l ++ [x] ++ traversal r
-- This is done by visiting the elements of the tree ‘in-order’, that is visiting first the left subtree, the
-- the node (root) itself, then the right subtree.
-- Program functions post and pre for traversing trees in ‘post-order’ and ‘pre-order’ respectively.
-- Recall that in post-order first the left subtree is traversed (in post-order), then the right subtree is
-- visited (in post-order) and finally the root node is considered. In pre-order first the root node is
-- visited, then the left subtree is traversed (in pre-order) and finally the right subtree is visited (in
-- pre-order).
-- For example,
-- > post (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)) ==> [3,4,2,5,1]
-- > post (reflect (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))) ==> [5,4,3,2,1]
-- > pre (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)) ==> [1,2,3,4,5]
-- > pre (reflect (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))) ==> [1,5,2,4,3]


traversal :: Tree a -> [a]
traversal Empty = []
traversal (Leaf x) = [x]
traversal (Node x l r) = traversal l ++ [x] ++ traversal r

post :: Tree a -> [a]
post Empty = []
post (Leaf x) = [x]
post (Node x l r) = post l ++ post r ++ [x]

pre :: Tree a -> [a]
pre Empty = []
pre (Leaf x) = [x]
pre (Node x l r) = [x] ++ pre l ++ pre r

-- 6.7.5 Equivalent trees can be represented in various manners due to the redundancy in the constructors.
-- For instance, each of the following pairs of trees are equivalent:
-- > Node 1 Empty Empty ==> Leaf 1
-- > Node ’a’ (Node ’b’ Empty (Node ’c’ Empty Empty)) (Node ’d’ Empty Empty)
-- ==> Node ’a’ (Node ’b’ Empty (Leaf ’c’)) (Node ’d’ Empty Empty)
-- > Node ’a’ (Node ’b’ Empty (Leaf ’c’)) (Node ’d’ Empty Empty)
-- ==> Node ’a’ (Node ’b’ Empty (Leaf ’c’)) (Leaf ’d’)
-- > Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 Empty Empty)
-- ==> Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)
-- The most economic way of representing a tree is by substituting all the occurrences of the form
-- (Node x Empty Empty) by (Leaf x) for some arbitrary x. Define a function normalise for doing
-- this tree normalisation.
-- For instance,
-- > normalise (Node ’a’ Empty Empty) ==> Leaf ’a’
-- > normalise (Node ’a’ (Node ’b’ Empty (Leaf ’c’)) (Node ’d’ Empty Empty))
-- ==> Node ’a’ (Node ’b’ Empty (Leaf ’c’)) (Leaf ’d’)
-- > normalise (Node 1 (Node 2 (Node 3 Empty Empty) (Leaf 4)) (Leaf 5))
-- ==> Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)
-- > normalise (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
-- ==> Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)

normalise :: Tree a -> Tree a
normalise    Empty  =  Empty
normalise   (Leaf a) = Leaf a
normalise (Node a Empty Empty) = Leaf a
normalise (Node a tree1 tree2) = Node a (normalise tree1) (normalise tree2)

-- 6.7.6  Give a higher-order function mapTree that applies a given input function to all the elements of a
-- tree. For instance, considering the functions
-- natural :: Int -> Bool
-- natural n = (n >= 0)
-- plusOne :: Int -> Int
-- plusOne x = x + 1
-- we get:
-- > mapTree natural (Node 1 (Node (-2) (Leaf 3) (Leaf 4)) (Leaf (-5)))
-- ==> Node True (Node False (Leaf True) (Leaf True)) (Leaf False)
-- mapTree plusOne (Node 1 (Node (-2) (Leaf 3) (Leaf 4)) (Leaf (-5)))
-- ==> Node 2 (Node (-1) (Leaf 4) (Leaf 5)) (Leaf (-4))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree     _          Empty   = Empty
mapTree     func         (Leaf a) = Leaf $ func a
mapTree     func         (Node a tree1 tree2) =
    Node (func a) (mapTree func tree1) (mapTree func tree2)

-- One list is a prefix of another if the elements of the first occur in the second at the
-- beginning and in the same order. For instance, [], [10], [10, 17], [10, 17, 9]
-- and [10, 17, 9, 28] are all prefixes of [10, 17, 9, 28]. Note that the empty
-- list is considered to be a prefix of any other list and that every list is a prefix of itself.
-- Define a Haskell function (predicate) isaprefix together with the most general possible type, which takes two lists and evaluates whether or not (returning True or
-- False, respectively) the first list is a prefix of the second.
-- As an illustration, consider the following execution scenarios,
-- Main> isaprefix [] [1 .. 10]
-- True :: Bool
-- Main> isaprefix [5,6,7] [5,6,2,6,7]
-- False :: Bool
-- Main> isaprefix ["Hello", "Haskell"] ["Hello", "Haskell", "F2"]
-- True :: Bool

main :: IO ()
main = do
  let result = isaprefix [5,6,7] [5,6,2,6,7]
  print result

isaprefix :: Eq a => [a] -> [a] -> Bool
isaprefix [] _ = True  -- An empty list is a prefix of any list
isaprefix _ [] = False -- A non-empty list cannot be a prefix of an empty list
isaprefix (f:fs) (s:sc)
    | f == s    = isaprefix fs sc  -- If elements match, continue checking
    | otherwise = False            -- If elements don't match, not a prefix

-- One list is a sublist of another if the elements of the first occur in the second, in
-- the same order as a contiguous subsequence (i.e., without gaps). Using the function
-- isaprefix or otherwise define a Haskell function (predicate) isasublist with the
-- most general possible type, which takes two lists of the same type and decides whether
-- or not (returning True or False, respectively) the first list is a sublist of the second.
-- Recall that the empty list is a sublist of any other list.
-- As an illustration, consider the following execution scenarios,
-- Main> isasublist [1,2,3] [0,1,2,3,4]
-- True :: Bool
-- Main> isasublist [1,2,3] [1,2,10,2,3,11]
-- False :: Bool
-- Main> isasublist "Chip" "Fish&Chips"
-- True :: Bool

isasublist :: Eq a => [a] -> [a] -> Bool
isasublist [] _ = True
isasublist _ [] = False
isasublist (f:fs) (s:sc)
    | isaprefix (f:fs) (s:sc) = True
    | otherwise = isasublist (f:fs) sc