{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Prelude hiding ((.), id)

import Control.Category ((.), id)
import Control.Monad
import Data.Char
import Data.List
import Debug.Trace
import Safe
import System.IO
import Text.Boomerang
import Text.Boomerang.Strings
import UI.NCurses

import English
import Zendo

type PP a = PrinterParser StringsError [String] () (a :- ())

prompt
  :: PP a
  -> (Int, Int)
  -> Int
  -> Int
  -> Curses a
prompt pp rc@(r,c) start end = promptS "" where
  promptS s = do
    w <- defaultWindow
    let (done, right, options) = completions pp s
    updateWindow w $ do
      moveCursor (toInteger r) (toInteger c)
      drawString s
      forM_ (zip ((if done then ("<enter>":) else id) options) [1..]) $ \(op,rd) -> do
        moveCursor (toInteger r + rd) (toInteger c)
        drawString op
    e <- getEvent w Nothing
    case e of
      Just (EventCharacter c) | c >= ' ' && c <= '~' -> promptS (s ++ [c])
      Just (EventSpecialKey KeyBackspace) -> promptS (initSafe s)
      Just (EventCharacter '\n') ->
        case parseStrings pp (words (map toLower s)) of
          Left _ -> promptS s
          Right x -> return x
      _ -> promptS s

completions
  :: PP a
  -> String
  -- | EOL ok, num valid characters, and candidate completions
  -> (Bool, Int, [String])
completions pp input = z where
  z =
    case onParser pp of
      Just (correct, cands) -> (False, correct, cands)
      Nothing ->
        case onParser (pp . lit "X" . eos) of
          Just (correct, cands) -> (True, correct, filter (/= "X") cands)
          Nothing -> error "impossible case; X should have been filtered by toLower"
  seq = words (map toLower input)
  candidates = map read . concatMap expected
  expected (Expect s) = [s]
  expected _ = []
  onParser parser =
    case parseStrings parser seq of
      Left p@(ParserError (Just (MajorMinorPos w _)) msgs) ->
        let cands = candidates msgs
            -- todo: this is not quite correct.
            -- It is incorrect if unwords is not symmetric
            -- with words (such as when the input has multiple spaces)
            correct = length . unwords . take (fromIntegral w) $ seq
        in
        -- trace (show p) $
        case drop (fromIntegral w) seq of
          [] -> Just (correct, cands)
          (x:_) ->
            case filter (x `isPrefixOf`) cands of
              [] -> Just (correct, cands)
              fcs -> Just (correct, cands) --) fcs)
      Left p@(ParserError Nothing msgs) -> error $ "no position: " ++ show p
      Right ret -> Nothing
        
        

{-
waitFor :: Window -> (Event -> Bool) -> Curses [Event]
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

-}
