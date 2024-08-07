{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char
import System.Random
import Text.Printf

{-
  Messages are constructed to avoid most data model errors.
  The exceptions are "duplicate declaration" and "duplicate option name",
  where we rely on probability to avoid duplicate names.
-}

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

-- Set to true to get spaces only when generating whitespace
spacesOnly :: Bool
spacesOnly = True

-- Set to true to limit which characters appear in names
readableNames :: Bool
readableNames = False

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

contentChars = map (\ x -> [x]) $ map toEnum $
  [0x1..0x8] ++ [0x0b..0x0c] ++ [0x0e..0x1f] ++ [0x21..0x2d]
  ++ [0x23..0x2d]
  ++ [0x2f..0x3f] ++ [0x41..0x5b] ++ [0x5d..0x7a]
  ++ [0x7e..0x2fff]
  ++ [0x3001..0xd7ff] ++ [0xe000..0x10ffff]

nameStartChars :: [Char]
nameStartChars = alpha ++ ['_'] ++
   (if readableNames then [] else (map toEnum $
       [0xc0..0xd6] ++ [0xd8..0xf6] ++ [0xf8..0x2ff]
    ++ [0x370..0x37d] ++ [0x37f..0x1fff] ++ [0x200c..0x200d]
    ++ [0x2070..0x218f] ++ [0x2c00..0x2fef] ++ [0x3001..0xd7ff]
    ++ [0xf900..0xfdcf] ++ [0xfdf0..0xfffc] ++ [0x10000..0xeffff]))

nameChars :: [Char]
nameChars =
  nameStartChars ++ digits ++ ['-'] ++ ['.'] ++
  (if readableNames then [] else
     map toEnum $ [0xb7] ++ [0x300..0x36f] ++ [0x203f..0x2040])

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
bounded = listWithLen maxLen

listWithLen :: Int -> IO String -> IO String
listWithLen n m = bounded' n m
  where bounded' :: Int -> IO String -> IO String
        bounded' n m | n == 0 = return ""
        bounded' n m = do
          s <- m
          rest <- bounded' (n - 1) m
          return $ s ++ rest

-- Uncomment for less readable/more exhaustive test cases
generateWhitespaceChar :: IO String
generateWhitespaceChar =
  if spacesOnly then
    return space
  else
    randomFromList [space, htab, cr, lf, "\x3000" ]

-- Uncomment call to `bounded` for longer whitespace strings
generateWhitespace :: IO String
generateWhitespace = bounded generateWhitespaceChar

maybeEmptyList :: IO String -> IO String
maybeEmptyList m = randomFromListIOBiased (bounded m) [return ""]

nonEmptyList = bounded

generateNameStart :: IO String
generateNameStart = randomFromList $ map (\ x -> [x]) nameStartChars

generateNameChar :: IO String
generateNameChar = randomFromList $ map (\ x -> [x]) nameChars

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

alpha :: [Char]
alpha = (['a'..'z'] ++ ['A'..'Z'])

nonZeroDigits :: [Char]
nonZeroDigits = ['1'..'9']

digits :: [Char]
digits = '0':nonZeroDigits

generateNonzeroDigit :: IO String
generateNonzeroDigit = randomFromList $ map (\ x -> [x]) nonZeroDigits

generateDigit :: IO String
generateDigit = randomFromList $ map (\ x -> [x]) digits

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
                              maybeEmptyList (randomFromListIOBiased generateQuotedChar [generateEscapedChar]),
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
-- For now, never generate fallback keys because fallbackVariant takes care of that.
-- This avoids a "duplicate variant" error at the expense of not testing cases like
-- "* foo" and "foo *"
generateKey = generateLiteral

generateVariant :: Int -> IO String
generateVariant numKeys =
  join [generateKey,
        listWithLen (numKeys - 1) (requiredWhitespaceBefore generateKey),
        optional generateWhitespace,
        generateQuotedPattern]

fallbackVariant :: Int -> IO String
fallbackVariant numKeys =
  join [return (concat $ take numKeys (repeat "* ")),
        optional generateWhitespace,
        generateQuotedPattern]

generateSelector :: IO String
generateSelector =
-- In order to avoid a "missing selector annotation" data model error,
-- require an expression with an annotation
-- This is stricter than the actual requirement, but simpler to generate
  join [
    return "{",
    optional generateWhitespace,
    randomFromListIO [generateLiteral, generateVariable],
    -- To avoid ICU4J "unknown selector type" error, always use :string
    join [generateWhitespace, return ":string"],
    generateAttributeList,
    optional generateWhitespace,
    return "}"
    ]

generateMatchStatement :: Int -> IO String
generateMatchStatement numSelectors =
  join [return ".match",
        listWithLen numSelectors (optionalWhitespaceBefore generateSelector)]

generateMatcher :: IO String
generateMatcher = do
  -- Avoid a "variant key mismatch" data model error
  -- by constraining the selector list and variant list to be the same length
  numSelectors <- randomRIO (1, maxLen)
  join [generateMatchStatement numSelectors,
        nonEmptyList (optionalWhitespaceBefore (generateVariant numSelectors)),
        -- Avoid a "missing fallback variant" data model error by including
        -- a variant with all '*' keys
        optionalWhitespaceBefore (fallbackVariant numSelectors)]

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
