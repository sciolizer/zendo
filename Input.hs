module Main where

import Control.Monad
import Data.Char
import System.IO
import Text.Boomerang
import Text.Boomerang.Strings

import English
import Zendo

main = forever $ do
  putStr "The number is "
  hFlush stdout
  -- debugging ask: if you input "The number is 8", it still gives
  -- a list of things to parse. Why? Why is it getting an error?
  l <- getLine
  -- todo: try both with and without the X so we know if continuing
  -- is possible
  case parseStrings digitAttribute (words (map toLower l) ++ ["X"]) of
    -- todo: check position to find out if error is at end or earlier
    -- alternatively, see if "unexpected end of input" is one of the messages
    (Left p@(ParserError _ msgs)) -> do
      print p
      putStrLn . unwords . map read . concatMap expected $ msgs
    (Right x) -> print x

expected (Expect s) = [s]
expected _ = []
