module LambdaCompilerTests.LexerTests (spec) where

import Control.Applicative
import Control.Monad
import Data.Char
import Lexer.Internal
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "statement" $ do
    it "can parse the same number of blocks" $ do
      property
        ( \(Lsc src blocks) ->
            case runParser (many statement) ((0, 0), src) of
              Left _ -> False
              Right (statms, _, _) -> length statms == length blocks
        )
    it "parses the same number of non-whitespace characters" $ do
      property
        ( \(Lsc src blocks) ->
            case runParser (many statement) ((0, 0), src) of
                  Left _ -> False
                  Right (statms, _, _) ->
                    countNonWhitespaceChars (map snd statms)
                      == countNonWhitespaceChars blocks
        )

countNonWhitespaceChars :: [String] -> Int
countNonWhitespaceChars str = sum $ map (length . filter (not . isSpace)) str

data LexicallySaneCode = Lsc
  { lscSrc :: String
  , lscBlocks :: [String]
  }
  deriving (Show)

instance Arbitrary LexicallySaneCode where
  arbitrary = do
    NonNegative blockCount' <- arbitrary
    statements <- replicateM blockCount' $ do
      NonNegative spaceBeforeCount <- arbitrary
      spacingBefore <- replicateM spaceBeforeCount $ do
        ind <- (`mod` 4) . abs <$> arbitrary
        return $ "\n\r \t" !! ind
      let spacingBefore' = spacingBefore ++ "\n"
      NonNegative spaceAfterCount <- arbitrary
      spacingAfter <- replicateM spaceAfterCount $ do
        ind <- (`mod` 4) . abs <$> arbitrary
        return $ "\n\r \t" !! ind
      Positive firstLineLength <- arbitrary
      let firstLine = replicate firstLineLength 'a'
      NonNegative followingLineCount <- arbitrary
      followingLines <- replicateM followingLineCount $ do
        NonNegative spaceCount <- arbitrary
        spaces <- replicateM spaceCount $ do
          ind <- (`mod` 4) . abs <$> arbitrary
          return $ "\n\r\t " !! ind
        let spaces' = spaces ++ " "
        Positive lineLength <- arbitrary
        let line = replicate lineLength 'a'
        return $ spaces' ++ line
      let statement' = firstLine ++ "\n" ++ unlines followingLines
      return (spacingBefore' ++ statement' ++ spacingAfter, statement')
    return $ Lsc (concatMap fst statements) $ map snd statements
