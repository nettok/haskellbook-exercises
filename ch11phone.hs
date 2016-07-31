{-# LANGUAGE FlexibleInstances #-}

module Chapter11Phone where

import qualified Data.Char as C
import qualified Data.Text as T


type NextInputCharIsUpper = Bool
type OutputChoices = () -> [Char]

data KeyBuffer = EmptyKeyBuffer | PressedKeyBuffer Char Int OutputChoices deriving (Show)

data TextpadST = TextpadST T.Text KeyBuffer NextInputCharIsUpper deriving (Show)


instance Show OutputChoices where
  show f = show (take 5 (f ()) ++ "...")


pressTextpadKey :: TextpadST -> Char -> TextpadST
pressTextpadKey tp key =
  case C.toUpper key of
    '1' -> appendChar (acceptKeyBuffer tp) '1'
    '2' -> pressKey tp '2' (\() -> cycle ['a'..'c'])
    '3' -> pressKey tp '3' (\() -> cycle ['d'..'f'])
    '4' -> pressKey tp '4' (\() -> cycle ['g'..'i'])
    '5' -> pressKey tp '5' (\() -> cycle ['j'..'l'])
    '6' -> pressKey tp '6' (\() -> cycle ['m'..'o'])
    '7' -> pressKey tp '7' (\() -> cycle ['p'..'s'])
    '8' -> pressKey tp '8' (\() -> cycle ['t'..'v'])
    '9' -> pressKey tp '9' (\() -> cycle ['w'..'z'])
    '*' -> toogleCase tp
    '0' -> pressKey tp '0' (\() -> cycle ['+', ' '])
    '#' -> pressKey tp '#' (\() -> cycle ['.', ','])
    'P' -> acceptKeyBuffer tp
    _   -> tp

pressTextpadKeys :: String -> TextpadST
pressTextpadKeys = foldl pressTextpadKey (TextpadST T.empty EmptyKeyBuffer False)

pressKey :: TextpadST -> Char -> OutputChoices -> TextpadST
pressKey (TextpadST txt EmptyKeyBuffer nextInputCharIsUpper) c oc = TextpadST txt (PressedKeyBuffer c 1 oc) nextInputCharIsUpper
pressKey tp@(TextpadST txt (PressedKeyBuffer c' count oc') nextInputCharIsUpper) c oc =
  if c == c'
    then TextpadST txt (PressedKeyBuffer c' (count + 1) oc') nextInputCharIsUpper
    else pressKey (acceptKeyBuffer tp) c oc

toogleCase :: TextpadST -> TextpadST
toogleCase (TextpadST txt keyBuffer nextInputCharIsUpper) = TextpadST txt keyBuffer (not nextInputCharIsUpper)

acceptKeyBuffer :: TextpadST -> TextpadST
acceptKeyBuffer (TextpadST txt EmptyKeyBuffer nextInputCharIsUpper) = TextpadST txt EmptyKeyBuffer nextInputCharIsUpper
acceptKeyBuffer tp@(TextpadST _ (PressedKeyBuffer _ count oc') _) = appendChar tp (oc' () !! (count - 1))

appendChar :: TextpadST -> Char -> TextpadST
appendChar (TextpadST txt _ nextInputCharIsUpper) c = TextpadST (T.snoc txt (setCase c)) EmptyKeyBuffer False
  where setCase = if nextInputCharIsUpper then C.toUpper else id
