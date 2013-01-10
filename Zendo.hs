{-# LANGUAGE NoMonomorphismRestriction #-}
module Zendo where

import Control.Applicative
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data DigitFactor = DigitFactor Flip DigitAttribute
  deriving (Eq, Ord, Read, Show)

instance Arbitrary DigitFactor where
  arbitrary = DigitFactor <$> arbitrary <*> arbitrary

data DigitAttribute
  = TheDigit Digit
  | DLessThan Digit -- 1 to 9
  | DGreaterThan Digit -- 0 to 8
  | Parity Parity
  deriving (Eq, Ord, Read, Show)

instance Arbitrary DigitAttribute where
  arbitrary = oneof [ TheDigit <$> arbitrary,
                      DLessThan <$> arbitrary,
                      DGreaterThan <$> arbitrary,
                      Parity <$> arbitrary ]
{-
data NumberAttribute
  = Parity Parity
  -}

data Parity = Even | Odd
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary Parity where arbitrary = arbitraryBoundedEnum

{-
data Primality = Prime | Composite
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
  -}

data Flip = Is | IsNot
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary Flip where arbitrary = arbitraryBoundedEnum

data Digit
  = D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary Digit where arbitrary = arbitraryBoundedEnum

data GroupAttribute
  = All DigitFactor
  -- | AtLeastOne DigitAttribute -- not all
  -- | AtLeastTwo NumberAttribute
  | None DigitFactor
  | Sum Parity
  | Smallest DigitFactor
  | Largest DigitFactor
  | First DigitFactor
  | Last DigitFactor
  -- | Product NumberAttribute
  | NoPairs LimitedComparison
  -- | AtLeastOnePair Comparison -- not all
  | AllPairs LimitedComparison
  | ConsecutivePairs Comparison
  {-
  | LastR RelativeAttribute
  | First NumberAttribute
  | Range NumberAttribute
  | TwoSixes
  -- | Alternate... probably not
  -}
  deriving (Eq, Ord, Read, Show)

instance Arbitrary GroupAttribute where
  arbitrary = oneof $
    [ All <$> arbitrary,
      None <$> arbitrary,
      Sum <$> arbitrary,
      Smallest <$> arbitrary,
      Largest <$> arbitrary,
      First <$> arbitrary,
      Last <$> arbitrary,
      NoPairs <$> arbitrary,
      AllPairs <$> arbitrary,
      ConsecutivePairs <$> arbitrary ]

data GroupFactor = GroupFactor Flip GroupAttribute
  deriving (Eq, Ord, Read, Show)

instance Arbitrary GroupFactor where
  arbitrary = GroupFactor <$> arbitrary <*> arbitrary

data LimitedComparison
  = Equal
  | Unequal
  | AbsoluteDifferenceOf Int -- 1 to 9
  deriving (Eq, Ord, Read, Show)

instance Arbitrary LimitedComparison where
  arbitrary = oneof [ pure Equal,
                      pure Unequal,
                      AbsoluteDifferenceOf <$> oneof (map pure [1..9]) ]

data Comparison
  = CLessThan
  | CGreaterThan
  | DifferenceOf Int -- 1 to 9 and -1 to -9
  | LimitedComparison LimitedComparison
  deriving (Eq, Ord, Read, Show)

instance Arbitrary Comparison where
  arbitrary = oneof [ pure CLessThan
                    , pure CGreaterThan
                    , DifferenceOf <$> oneof (map pure ([(-1)..(-9)] ++ [1..9])) ]

class English e where
  english :: e -> String

instance English GroupFactor where
  english (GroupFactor f ga) = prefix ++ english ga where
    prefix = case f of { Is -> ""; IsNot -> "it is not the case that " }

instance English GroupAttribute where
  english ga =
    case ga of
      All da -> "all numbers " ++ english da
      None da -> "none of the numbers " ++ english da
      Sum p -> "the sum of the numbers " ++ english p
      Smallest da -> "the smallest number " ++ english da
      Largest da -> "the largest number " ++ english da
      First da -> "the first number " ++ english da
      Last da -> "the last number " ++ english da
      NoPairs Equal -> "no pairs are equal"
      NoPairs Unequal -> "no pairs are unequal"
      NoPairs (AbsoluteDifferenceOf i) -> "no pairs differ by " ++ show i
      AllPairs Equal -> "all numbers are equal"
      AllPairs Unequal -> "all numbers are distinct"
      AllPairs (AbsoluteDifferenceOf i) -> "all pairs differ by " ++ show i
      ConsecutivePairs CLessThan -> "the numbers are in ascending order"
      ConsecutivePairs CGreaterThan -> "the numbers are in decreasing order"
      ConsecutivePairs (LimitedComparison Equal) -> "all numbers are equal"
      ConsecutivePairs (LimitedComparison Unequal) -> "adjacent numbers are distinct"
      ConsecutivePairs (DifferenceOf i) -> "adjacent numbers increase by " ++ show i
      ConsecutivePairs (LimitedComparison (AbsoluteDifferenceOf i)) -> "adjacent numbers differ by " ++ show i

{-
instance English Comparison where
  english c =
    case c of
      CLessThan -> 
      -}
instance English Parity where
  english Even = "is even"
  english Odd = "is odd"
instance English DigitFactor where
  english (DigitFactor f d) = prefix ++ english d where
    prefix = case f of { Is -> "is "; IsNot -> "is not " }
instance English DigitAttribute where
  english da =
    case da of
      TheDigit d -> english d
      DLessThan d -> "less than " ++ english d
      DGreaterThan d -> "greater than " ++ english d
      Parity p -> english p
instance English Digit where
  english d =
    case d of
      D0 -> "0"
      D1 -> "1"
      D2 -> "2"
      D3 -> "3"
      D4 -> "4"
      D5 -> "5"
      D6 -> "6"
      D7 -> "7"
      D8 -> "8"
      D9 -> "9"
