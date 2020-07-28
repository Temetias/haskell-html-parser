module Main where

import Data.Char
import Control.Applicative

data HTMLElement
    = HTMLNonVoidElement [HTMLElement]
    | HTMLVoidElement String
    | HTMLText String
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

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser parser1) <|> (Parser parser2) =
        Parser $ \str -> parser1 str <|> parser2 str

charParser :: Char -> Parser Char
charParser x = Parser f
    where
        f (s:str)
            | s == x = Just (str, x)
            | otherwise = Nothing
        f [] = Nothing

stringParser :: String -> Parser String
stringParser = sequenceA . map charParser

whiteSpaceParser :: Parser String
whiteSpaceParser = Parser $ \str ->
    let (whiteSpaceCharacters, rest) = span isSpace str
        in Just (rest, whiteSpaceCharacters)
    
tagParser :: Parser String
tagParser = Parser $ \str ->
    let (word, rest) = span isLetter str
        in Just (rest, word)

whiteSpaceWrap :: Parser a -> Parser a
whiteSpaceWrap parser = whiteSpaceParser *> parser <* whiteSpaceParser 

voidElementParser :: Parser HTMLElement
voidElementParser = (\tag -> HTMLVoidElement tag) <$> (stringParser "<" *> whiteSpaceWrap tagParser <* stringParser "/>")

nonVoidElementParser :: Parser HTMLElement
nonVoidElementParser = (\els -> HTMLNonVoidElement els) <$> (stringParser "<div>" *> innerParser <* stringParser "</div>")
    where
        innerParser = whiteSpaceWrap $ many htmlParser

htmlParser :: Parser HTMLElement
htmlParser = whiteSpaceWrap $ nonVoidElementParser <|> voidElementParser

main :: IO ()
main = undefined