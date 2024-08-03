{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char
import System.Random
import Text.Printf

-- Maximum length for repeated elements.
-- Adjust to get longer strings and lists.
maxLen :: Int
maxLen = 2

-- Used by randomFromListIOBiased. Adjust as desired
-- (to a number > 0 and <= 1)
weight :: Float
weight = 0.75

-- Adjust as desired
numTests :: Int
numTests = 100

space, htab, cr, lf :: String
space = " "
htab = [toEnum 9]
cr = [toEnum 13]
lf = [toEnum 10]

join :: [IO String] -> IO String
join [] = return ""
join (x:xs) = do
  head <- x
  tail <- join xs
  return $ head ++ tail

optionalWhitespaceBefore :: IO String -> IO String
optionalWhitespaceBefore m = join [optional generateWhitespace, m]

requiredWhitespaceBefore :: IO String -> IO String
requiredWhitespaceBefore m = join [generateWhitespace, m]

-- Simplified for readability. Uncomment various ranges
-- to test all possible characters
contentChars = map (\ x -> [x]) $ map toEnum $
  -- n.b. control characters need to be escaped for JSON
  -- and this code doesn't currently do that
 {- [0x1..0x8] ++ [0x0b..0x0c] ++ [0x0e..0x1f] ++ -} -- [0x21..0x2d]
  [0x23..0x2d]
  ++ [0x2f..0x3f] ++ [0x41..0x5b] ++ [0x5d..0x7a]
  -- ++ [0x7e..0x2fff]
  -- ++ [0x3001..0xd7ff] ++ [0xe000..0x10ffff]

generateContentChar :: IO String
generateContentChar = randomFromList contentChars

randomFromList :: [a] -> IO a
randomFromList xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i

randomFromListIO :: [IO a] -> IO a
randomFromListIO xs = do
  i <- randomRIO (0, length xs - 1)
  xs !! i

-- Pick the first argument with `weight` probability
randomFromListIOBiased :: IO a -> [IO a] -> IO a
randomFromListIOBiased x xs = do
  pickFirst :: Float <- randomRIO (0, 1)
  if (pickFirst <= weight) then
    x
  else do
    i <- randomRIO (0, length xs - 1)
    xs !! i

optional :: IO String -> IO String
optional m = do
  omit :: Bool <- randomIO
  if (omit) then return "" else m

bounded :: IO String -> IO String
bounded m = bounded' maxLen m
  where bounded' n m | n == 0 = m
        bounded' n m = do
          s <- m
          rest <- bounded' (n - 1) m
          return $ s ++ rest

-- Uncomment for less readable/more exhaustive test cases
generateWhitespaceChar :: IO String
generateWhitespaceChar = return space -- randomFromList [space, htab, cr, lf {- , "\x3000" -}]

-- Uncomment call to `bounded` for longer whitespace strings
generateWhitespace :: IO String
generateWhitespace = {- bounded -} generateWhitespaceChar

-- TODO -- this has 50% chance of an empty list -- maybe not ideal
maybeEmptyList :: IO String -> IO String
maybeEmptyList m = randomFromListIO [bounded m, return ""]

nonEmptyList = bounded

-- TODO: Doesn't include all `name-start` characters,
-- for readability
generateNameStart :: IO String
generateNameStart = randomFromList (alpha ++ ["_"])

-- TODO: Also simplified for readability
generateNameChar :: IO String
generateNameChar = randomFromListIO [generateNameStart, generateDigit, return "-", return "."]

generatePrivateStart :: IO String
generatePrivateStart = randomFromList ["^", "&"]

generateName :: IO String
generateName = join [generateNameStart, maybeEmptyList generateNameChar]

generateNamespace = generateName

generateIdentifier :: IO String
generateIdentifier = join [
  optional (join [generateNamespace, return ":"]),
  generateName]

generateSign = randomFromList ["-", "+"]

alpha :: [String]
alpha = map (\ x -> [x]) (['a'..'z'] ++ ['A'..'Z'])

nonZeroDigits :: [String]
nonZeroDigits = map (\ x -> [x]) ['1'..'9']

generateNonzeroDigit :: IO String
generateNonzeroDigit = randomFromList nonZeroDigits

generateDigit :: IO String
generateDigit = randomFromList $ ("0":nonZeroDigits)

generateNumberLiteral :: IO String
generateNumberLiteral = join [
  optional (return "-"),
  randomFromListIO [return "0",
                    join [generateNonzeroDigit,
                           maybeEmptyList generateDigit]],
  optional $ join [return ".", nonEmptyList generateDigit],
  optional $ join [randomFromList ["e", "E"], optional generateSign, nonEmptyList generateDigit]
  ]

generateQuotedChar :: IO String
generateQuotedChar =
  randomFromListIO [generateContentChar,
                    generateWhitespace,
                    return ".",
                    return "@",
                    return "{",
                    return "}"]

generateQuotedLiteral :: IO String
generateQuotedLiteral = join [return "|",
                              maybeEmptyList (randomFromListIO [generateQuotedChar, generateEscapedChar]),
                              return "|"]

generateUnquotedLiteral :: IO String
generateUnquotedLiteral = randomFromListIO [generateName, generateNumberLiteral]

generateLiteral :: IO String
generateLiteral = randomFromListIO [generateQuotedLiteral, generateUnquotedLiteral]

generateVariable :: IO String
generateVariable = join $ [return "$", generateName]

generateLiteralExpression :: IO String
generateLiteralExpression = generateLiteralOrVariableExpression generateLiteral

generateVariableExpression :: IO String
generateVariableExpression = generateLiteralOrVariableExpression generateVariable

generateLiteralOrVariableExpression :: IO String -> IO String
generateLiteralOrVariableExpression operand =
  join [
  return "{",
  optional generateWhitespace,
  operand,
  optional (join [generateWhitespace, generateAnnotation]),
  generateAttributeList,
  optional generateWhitespace,
  return "}"
  ]

generateOption :: IO String
generateOption =
  join [
  generateIdentifier,
  optional generateWhitespace,
  return "=",
  optional generateWhitespace,
  randomFromListIO [generateLiteral, generateVariable]
  ]

generateAttribute :: IO String
generateAttribute =
  join [
  return "@",
  generateIdentifier,
  optional (join [
            optional generateWhitespace,
            return "=",
            optional generateWhitespace,
            randomFromListIO [generateLiteral, generateVariable]])]

generateAttributeList = maybeEmptyList (join [generateWhitespace, generateAttribute])

generateOptionList = maybeEmptyList (join [generateWhitespace, generateOption])

generateFunction :: IO String
generateFunction = join [
  return ":",
  generateIdentifier,
  generateOptionList
  ]

generateReservedChar :: IO String
generateReservedChar = randomFromListIOBiased generateContentChar [return "."]

generateReservedBodyPart :: IO String
generateReservedBodyPart =
  randomFromListIO [generateReservedChar, generateEscapedChar, generateQuotedLiteral]

generateReservedBody :: IO String
generateReservedBody =
  join [generateReservedBodyPart,
        maybeEmptyList (optionalWhitespaceBefore generateReservedBodyPart)]

generatePrivateUseAnnotation :: IO String
generatePrivateUseAnnotation =
  join [generatePrivateStart,
        optional (optionalWhitespaceBefore generateReservedBody)]

generateReservedAnnotationStart :: IO String
generateReservedAnnotationStart =
  randomFromList ["!", "%", "*", "+", "<", ">", "?", "~"]

generateReservedKeyword :: IO String
generateReservedKeyword = join [return ".", generateName]

generateReservedAnnotation :: IO String
generateReservedAnnotation =
  join [generateReservedAnnotationStart,
        optional (optionalWhitespaceBefore generateReservedBody)]

generateAnnotation :: IO String
generateAnnotation = randomFromListIOBiased generateFunction $ [generatePrivateUseAnnotation, generateReservedAnnotation]

generateAnnotationExpression :: IO String
generateAnnotationExpression = join [
  return "{",
  optional generateWhitespace,
  generateAnnotation,
  generateAttributeList,
  optional generateWhitespace,
  return "}"
  ]

generateOpenOrStandaloneMarkup :: IO String
generateOpenOrStandaloneMarkup =
  join
  [
    return "{",
    optional generateWhitespace,
    return "#",
    generateIdentifier,
    generateOptionList,
    generateAttributeList,
    optional generateWhitespace,
    optional (return "/"),
    return "}"
    ]

generateCloseMarkup :: IO String
generateCloseMarkup =
  join
  [
    return "{",
    optional generateWhitespace,
    return "/",
    generateIdentifier,
    generateOptionList,
    generateAttributeList,
    optional generateWhitespace,
    return "}"
  ]

generateMarkup :: IO String
generateMarkup = randomFromListIO [generateOpenOrStandaloneMarkup, generateCloseMarkup]

generateExpression :: IO String
generateExpression = randomFromListIO [generateLiteralExpression, generateVariableExpression, generateAnnotationExpression]

generatePlaceholder :: IO String
generatePlaceholder = randomFromListIO [generateExpression, generateMarkup]

generateSimpleStartChar :: IO String
generateSimpleStartChar =
  randomFromListIO [generateContentChar, generateWhitespace, return "@", return "|"]

generateTextChar :: IO String
generateTextChar =
  randomFromListIO [generateContentChar, generateWhitespace, return ".", return "@", return "|"]

generateEscapedChar :: IO String
generateEscapedChar = randomFromList ["\\\\", "\\{", "\\|", "\\}"]

generateSimpleStart :: IO String
generateSimpleStart =
  randomFromListIO [generateSimpleStartChar, generateEscapedChar, generatePlaceholder]

generatePatternPart :: IO String =
  randomFromListIOBiased generatePlaceholder $ [generateTextChar, generateEscapedChar]

generatePattern :: IO String
generatePattern = bounded generatePatternPart

generateLocalDeclaration :: IO String
generateLocalDeclaration =
  join [return ".local",
        generateWhitespace,
        generateVariable,
        optional generateWhitespace,
        return "=",
        optional generateWhitespace,
        generateExpression]

generateInputDeclaration :: IO String
generateInputDeclaration =
  join [return ".input",
        optional generateWhitespace,
        generateVariableExpression]

generateReservedStatement :: IO String
generateReservedStatement =
  join [generateReservedKeyword,
        optional (requiredWhitespaceBefore generateReservedBody),
        nonEmptyList (optionalWhitespaceBefore generateExpression)]

generateDeclarations :: IO String
generateDeclarations = randomFromListIOBiased generateLocalDeclaration [generateInputDeclaration, generateReservedStatement]

generateKey :: IO String
generateKey = randomFromListIO [generateLiteral, return "*"]

generateVariant :: IO String
generateVariant =
  join [generateKey,
        maybeEmptyList (requiredWhitespaceBefore generateKey),
        optional generateWhitespace,
        generateQuotedPattern]

generateSelector = generateExpression
generateMatchStatement :: IO String
generateMatchStatement =
  join [return ".match",
        nonEmptyList (optionalWhitespaceBefore generateSelector)]

generateMatcher :: IO String
generateMatcher =
  join [generateMatchStatement,
        nonEmptyList (optionalWhitespaceBefore generateVariant)]

generateQuotedPattern :: IO String
generateQuotedPattern = join [return "{{", generatePattern, return "}}"]

generateComplexBody :: IO String
generateComplexBody = randomFromListIO [generateQuotedPattern, generateMatcher]

generateComplexMessage :: IO String
generateComplexMessage =
  join [generateDeclarations,
        generateComplexBody,
        optional generateWhitespace]

-- Generates a non-empty message
-- (it wouldn't be very interesting to generate an empty one)
generateSimpleMessage :: IO String
generateSimpleMessage = do
    startChar <- generateSimpleStart
    pat       <- generatePattern
    return $ startChar ++ pat

generateMessage :: IO String
generateMessage = do
  simple :: Bool <- randomIO
  if (simple) then
     generateSimpleMessage
  else
     generateComplexMessage

repeatM :: Int -> IO a -> IO [a]
repeatM n _ | n == 0 = return []
repeatM n m = do
  result <- m
  rest <- repeatM (n - 1) m
  return $ result:rest

escapeChar :: Char -> String
escapeChar '\\'  = "\\\\"
escapeChar '\n' = "\\n"
escapeChar '\t' = "\\t"
escapeChar '\r' = "\\r"
escapeChar c = (printf "\\u%.4x") $ fromEnum c

escape :: String -> String
escape = concatMap (\ c -> if (c == '\\' || isControl c) then (escapeChar c) else [c])

formatTest :: String -> String
formatTest s = "{ \"src\" : \"" ++ (escape s) ++ "\" },\n"

formatTests :: [String] -> String
formatTests = concatMap formatTest

main = do
  tests <- repeatM numTests generateMessage
  putStrLn $ formatTests tests
