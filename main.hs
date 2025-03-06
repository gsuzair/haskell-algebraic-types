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

