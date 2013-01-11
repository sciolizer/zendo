{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module English where

import Prelude hiding ((.), id, all, sum, last)
import Control.Category ((.), id)
import Control.Monad (forever)
import Data.String
import Text.Boomerang
import Text.Boomerang.Strings hiding (digit)
import Text.Boomerang.TH
import System.IO (hFlush, stdout)

import Zendo

$(derivePrinterParsers ''Digit)
$(derivePrinterParsers ''Parity)
$(derivePrinterParsers ''DigitAttribute)
$(derivePrinterParsers ''DigitFactor)
$(derivePrinterParsers ''Flip)
$(derivePrinterParsers ''GroupAttribute)
$(derivePrinterParsers ''LimitedComparison)
$(derivePrinterParsers ''GroupFactor)

word x = x . eos

line = digitAttribute . word "X"

digitFactor = rDigitFactor . (rIs <> rIsNot . word "not") . digitAttribute

-- A sequence has the buddha nature if
{- no pairs are equal
 - all numbers are different
 - all pairs (of numbers) are different/unequal
 - no pair of numbers differs by 5
 - 
 - different/unequal
 - equal/the same
 - pairs / pair of numbers / 
 - increasing
 - decreasing
 - ordered from smallest to largest
 - ordered from largest to smallest
 - consecutive pairs
 -}

groupFactor = rGroupFactor . (rIs <> rIsNot . notTheCase) . groupAttribute where
  notTheCase = word "it" . word "is" . word "not" . word "the" . word "case" . word "that"

groupAttribute =
     rAll . all . digitFactor
  <> rAtLeastOne . atLeastOne . digitFactor
  <> rNone . none . digitFactor -- todo: suppors "fives" and "5s"
  <> rSum . sum . parity
  <> rSmallest . smallest . digitFactor
  <> rLargest . largest . digitFactor
  <> rFirst . first . digitFactor
  <> rLast . last . digitFactor
  <> rNoPairs . noPairs
  <> rAtLeastOnePair . atLeastOnePair
  <> rAllPairs . allPairs

all = eachAndEvery . word "number" . word "is"
    <> word "all" . word "numbers" . word "are"
none = word "none" . word "of" . word "the" . word "numbers" . word "are"
    <> word "no" . word "number" . word "is"
atLeastOne = opt (word "at" . word "least") . word "one" . (word "of" . word "the" . word "numbers" <> word "number") . "is"
  <> (word "a" <> word "any") . word "number" . word "is"
sum = word "the" . (word "sum" <> word "total") . opt (word "of" . word "the" . word "numbers") . word "is"
smallest = word "the" . (word "smallest" <> word "minimum" <> word "least") . opt (word "number") . word "is"
largest = word "the" . (word "largest" <> word "biggest" <> word "maximum") . opt (word "number") . word "is"
first = word "the" . word "first" . opt (word "number") . word "is"
last = word "the" . word "last" . opt (word "number") . word "is"

eachAndEvery = word "each" . opt (word "and" . word "every") <> word "every"

ofNumbers = word "of" . word "numbers"

allPairs = eachAndEvery . word "pair" . opt ofNumbers . limited Pair
  <> word "all" . word "pairs" . opt ofNumbers . limited Pairs
atLeastOnePair =
     word "any" . word "pair" . opt ofNumbers . limited Pair
  <> word "at" . word "least" . (word "one" <> word "a") . word "pair" . opt ofNumbers . limited Pair
  <> opt (word "at" . word "least") . word "two" . word "numbers" . limited Numbers
noPairs =
     word "no" . word "pair" . ofNumbers . limited Pair
  <> word "no" . word "two" . word "numbers" . limited Numbers

data ToEachOther = Numbers | Pair | Pairs
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- no two numbers are equal to each other
-- no pairs of numbers are equal
-- pairEqual = word "equal" <> word "the" . word "same"
-- the same as each other
-- equal to each other
-- pair is unequal
limited Pair =
     rEqual . word "is" . word "equal"
  -- <> rUnequal . word "is" . word "unequal"
  -- <> rAbsoluteDifferenceOf . word "differs" . word "by" . opt (word "exactly") . thenumber [1..9]
  <> rAbsoluteDifferenceOf . word "has" . word "a" . word "difference" . word "of" . thenumber [1..9]
limited Pairs =
     rEqual . word "are" . word "equal"
  -- <> rUnequal . word "are" . word "unequal"
  <> rAbsoluteDifferenceOf . word "have" . word "a" . word "difference" . word "of" . thenumber [1..9]
limited Numbers =
     rEqual . word "are" . (word "equal" . opt toEachOther <> word "the" . word "same" . opt asEachOther)
  <> rUnequal . word "are" . (word "unequal" . rNothing . opt toEachOther <> word "different" . rMaybe fromEachOther)
  -- <> rUnequal . word "differ"
  <> rAbsoluteDifferenceOf . word "differ" . word "by" . opt (word "exactly") . thenumber [1..9]
  <> rAbsoluteDifferenceOf . word "have" . word "a" . word "difference" . word "of" . opt (word "exactly") . thenumber [1..9] where
  toEachOther = word "to" . word "each" . word "other"
  fromEachOther = rUnit . word "from" . word "each" . word "other"
  asEachOther = word "as" . word "each" . word "other"

digitAttribute =
     rTheDigit . thedigits [0..9]
  <> rDLessThan . less . word "than" . thedigits [1..9]
  <> rDGreaterThan . more . word "than" . thedigits [0..8]
  <> rParity . parity
  <> rDLessThanOrEqual . less . word "than" . word "or" . eq . thedigits [0..8]
  <> rDGreaterThanOrEqual . more . word "than" . word "or" . eq . thedigits [1..9]

eq = word "equal" . word "to" <> word "the" . word "same" . word "as"
less = word "less" <> word "smaller" <> word "lower"
more = word "more" <> word "bigger" <> word "larger" <> word "greater" <> word "higher"

parity = rEven . word "even" <> rOdd . word "odd"

thenumber ds = opt (word "the" . word "number") . foldl1 (<>) (map number ds)

number i = [zero, one, two, three, four, five, six, seven, eight, nine] !! i where
  zero = undefined
  one = undefined
  two = undefined
  three = undefined
  four = undefined
  five = undefined
  six = undefined
  seven = undefined
  eight = undefined
  nine = undefined

thedigits ds = opt (word "the" . word "number") . digits ds

digits ds = foldl1 (<>) [digit i . word (fromString $ show i) | i <- ds]

digit i = [rD0, rD1, rD2, rD3, rD4, rD5, rD6, rD7, rD8, rD9] !! i

{-
data Foo
  = Bar
  | Baz Int Char
  deriving (Eq, Ord, Read, Show)

data Att = Att Bool Digit
  deriving (Eq, Ord, Read, Show)

data Digit = D0 | D1
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

$(derivePrinterParsers ''Foo)
$(derivePrinterParsers ''Att)
$(derivePrinterParsers ''Digit)

rDigit = rD0 . "0" <> rD1 . "1"

type StringsPrinterParser = PrinterParser StringsError [String]

at :: StringsPrinterParser () (Att :- ())
at = rAtt . "is" . eos . (rFalse . "not" . eos <> rTrue) . rDigit
-}
{-
foo :: StringPrinterParser () (Foo :- ())
foo = rBar . "bar" <> rBaz . "baz-" . int . "-" . alpha

test1 = parseString foo "baz-2-c"

test2 = parseString foo ""

test3 = parseString foo "baz-2-3"

test4 = unparseString foo (Baz 1 'z')

testInvert :: String -> IO ()
testInvert str =
  case parseString foo str of
    (Left e) -> print e
    (Right f') -> do
      putStrLn $ "Parsed: " ++ show f'
      case unparseString foo f' of
        Nothing -> putStrLn "unparseString failed to produce a value."
        (Just s) -> putStrLn $ "Pretty: " ++ s

main = forever $ do
  putStr "Enter a string to parse: "
  hFlush stdout
  l <- getLine
  testInvert l
  -}
-- main = print "hi"
     
