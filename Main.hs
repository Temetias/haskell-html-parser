module Main where

import Data.Char
import Control.Applicative

data HTMLElement
    = HTMLDiv [HTMLElement]
    | HTMLSpan [HTMLElement]
    | HTMLInput
    deriving (Show)

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser parser) =
        Parser $ \str -> do
            (str', x) <- parser str
            Just (str', f x)

instance Applicative Parser where
    pure x = Parser $ \str -> Just (str, x)
    (Parser parser1) <*> (Parser parser2) =
        Parser $ \str -> do
            (str', f) <- parser1 str
            (str'', a) <- parser2 str'
            Just (str'', f a)

charParser :: Char -> Parser Char
charParser x = Parser f
    where
        f (s:str)
            | s == x = Just (str, x)
            | otherwise = Nothing
        f [] = Nothing

stringParser :: String -> Parser String
stringParser = sequenceA . map charParser

inputParser :: Parser HTMLElement
inputParser = (\_ -> HTMLInput) <$> stringParser "<input/>"

htmlParser :: Parser HTMLElement
htmlParser = undefined

main :: IO ()
main = undefined