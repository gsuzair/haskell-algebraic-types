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

