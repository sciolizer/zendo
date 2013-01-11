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
  | DLessThanOrEqual Digit -- 0 to 8
  | DGreaterThan Digit -- 0 to 8
  | DGreaterThanOrEqual Digit -- 1 to 9
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
  | AtLeastOne DigitFactor -- not all
  -- | AtLeastTwo NumberAttribute
  | None DigitFactor
  | Sum Parity
  | Smallest DigitFactor
  | Largest DigitFactor
  | First DigitFactor
  | Last DigitFactor
  -- | Product NumberAttribute
  | NoPairs LimitedComparison
  | AtLeastOnePair LimitedComparison -- Comparison -- not all
  | AllPairs LimitedComparison
  -- | ConsecutivePairs Comparison
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
      AllPairs <$> arbitrary ]
      -- ConsecutivePairs <$> arbitrary ]

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
  -- todo: change this to a list of strings so that
  -- we can classify into something multiple ways
  english :: e -> Flip -> [String]

class Englishes e where
  englishes :: e -> Flip -> Plurality -> [String]

xor Is Is = Is
xor Is IsNot = IsNot
xor IsNot Is = IsNot
xor IsNot IsNot = Is

data Plurality = Singular | Plural
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

{-
instance English GroupFactor where
  english (GroupFactor f ga) f' = english 
    case f `xor` f' of
      Is -> english Is ga
      IsNot -> undistributed ++ distributed where
        undistributed = ("it is not the case that " ++ ) <$> english ga Is
        distributed = english ga IsNot

notAttributed ga =
  case ga of
    All df -> atLeast ++ notAll where
      atLeast = ("at least one number " ++) <$> digitFactor Singular (negated df)
      notAll = ("not all numbers " ++) <$> digitFactor Plural df
    None df -> ("at least one number " ++) <$> digitFactor Singular df
    Sum p -> ("the sum of the numbers " ++) <$> parity Singular (negated p)
    Smallest df -> do
      k <- ["smallest number ", "minimum "]
      ("the " ++ k ++) <$> digitFactor Singular (negated df)
    Largest df -> do
      k <- ["largest number ", "maximum "]
      ("the " ++ k ++) <$> digitFactor Singular (negated df)
    First df -> ("the first number " ++) <$> digitFactor Singlar
    -- "begins with"

instance English GroupAttribute where
  english ga f =
    case ga of
      All df ->
        let q f' = all ++ each
            all = ("all numbers " ++) <$> englishes df f' Plural
            each = do
              e <- ["each", "every"]
              (e ++ " number " ++) <$> englishes df f' Singular in
        case f of
          Is -> q Is
          IsNot -> undistributed ++ distributed where
            undistributed = all ++ each
            all = ("not all numbers " ++) <$> englishes df Is Plural
            each = ("not every number ") <$> englishes df Is Singular
            distributed = q IsNot
      -- next:
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
      -}
