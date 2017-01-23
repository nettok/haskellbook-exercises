{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( run
    , parseNumber
    , parseHeader
    , parseSection
    , sectionEx''
    , parseIni
    , parseSemVer
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta
import Text.Parser.Combinators

run :: IO ()
--run = print testParse123
--run = testParseFraction
--run = testParseNumber
run = iniParserTests

--

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneTwo = one >> char '2'
oneTwo' = oneTwo >> stop

eof' a = eof >> return a

parse :: Parser a -> String -> Result a
parse p = parseString p mempty

-- Exercises: Parsing Practice (page 891)
-- 1
parseEof = parse $ oneTwo >>= eof'
testParseEof = parseEof "123"

-- 2
parse123 :: String -> Result String
parse123 = parse $ (str "123" <|> str "12" <|> str "1") >>= eof'
testParse123 = parse123 "1234"

--3 (wrong?)
str :: String -> Parser String
str [] = return []
str (c:cs) = char c >>= (\c -> fmap (c:) (str cs))

------------------------------------
-- 24.4 Parsing Fractions (page 893)

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denominator cannot be zero"
    _ -> return (numerator % denominator)

testParseFraction :: IO ()
testParseFraction = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction

-------------------------------
-- Exercise: Try Try (page 912)

parseNumber :: Parser (Either Integer Rational)
parseNumber = try ((Left <$> decimal) >>= (\n -> eof >> return n)) <|> ((Right <$> parseFraction) >>= (\n -> eof >> return n))

testParseNumber :: IO ()
testParseNumber = do
  print $ parseString parseNumber mempty "1/0"
  print $ parseString parseNumber mempty "1/5"
  print $ parseString parseNumber mempty "42"

----------------------------------------------
-- 24.7 Parsing configuration files (page 912)

-- ; comment
-- [section]
-- host=wikipedia.org
-- alias=claw

-- INI examples

headerEx :: ByteString
headerEx = "[blah]"

assignmentEx :: ByteString
assignmentEx = "woot=1"

commentEx :: ByteString
commentEx =
  "; last modified 1 April \
  \ 2001 by Calabaza"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n  \n;hah"

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]

-- INI parser

newtype Header = Header String deriving (Eq, Ord, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany $ do
  _ <- char ';' <|> char '#'
  skipMany (noneOf "\n")
  skipEOL

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections

-- INI parser tests

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

iniParserTests :: IO ()
iniParserTests = hspec $ do

  describe "Assignment Parsing" $
    it "can parse a single assignment" $ do
      let m = parseByteString parseAssignment mempty assignmentEx
          r = maybeSuccess m
      print m
      r `shouldBe` Just ("woot", "1")

  describe "Header Parsing" $
    it "can parse a single header" $ do
      let m = parseByteString parseHeader mempty headerEx
          r = maybeSuccess m
      print m
      r `shouldBe` Just (Header "blah")

  describe "Comment Parsing" $
    it "can skip a comment before a header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r = maybeSuccess m
      print m
      r `shouldBe` Just (Header "blah")

  describe "Section Parsing" $
    it "can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx
          r = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
          expected = Just (Section (Header "states") states)
      print m
      r `shouldBe` expected

  describe "INI Parsing" $
    it "can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r = maybeSuccess m
          sectionValues = M.fromList [("alias", "claw"), ("host", "wikipedia.org")]
          whatisitValues = M.fromList [("red", "intoothandclaw")]
          expected = Just (Config (M.fromList [(Header "section", sectionValues), (Header "whatisit", whatisitValues)]))
      print m
      r `shouldBe` expected

-------------------------------------
-------------------------------------
-- 24.11 Chapter Exercises (page 941)

data NumberOrString =
    NOSN Integer
  | NOSS String
  deriving (Show, Eq, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq, Ord)

skipDot :: Parser ()
skipDot = void $ char '.'

skipHyphen :: Parser ()
skipHyphen = void $ char '-'

parseNonMajor :: Parser Integer
parseNonMajor = option 0 $ skipDot >> integer

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = choice [NOSN <$> integer, NOSS <$> some letter]

parseExtra :: Parser [NumberOrString]
parseExtra = option [] $ skipHyphen >> sepEndBy1 parseNumberOrString skipDot

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  minor <- parseNonMajor
  patch <- parseNonMajor
  release  <- parseExtra
  metadata <- parseExtra
  return $ SemVer major minor patch release metadata
