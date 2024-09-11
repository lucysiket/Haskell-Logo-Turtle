{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Hurtle.Parser where

-- You'll probably want to refer to 
-- https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.

import Hurtle.Types
import Text.Megaparsec
import Data.Char (isDigit)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

-- Parses every line in the hogo file until the end of file. Stops parsing on empty and commented lines --
parseHogo :: Parser HogoProgram
parseHogo = between stopParse eof $ parseLine `sepEndBy` stopParse

-- Consumes empty lines and commented lines --
stopParse :: Parser ()
stopParse = skipMany (choice [skipLineComment ";", space1])

-- Line parser ------------------------------------------------------------------------------------------------------------------------------------------
parseLine :: Parser HogoCode
parseLine = choice [parseMovement, parseOther, parseRepeat, parseColour]
    where

        -- Parses movement commands that require a floating point value --
        parseMovement :: Parser HogoCode
        parseMovement = do
            -- A HogoCode value is returned depending on the command the user has input --
            command <- parseForward <|> parseBackward <|> parseLeft <|> parseRight
            hspace
            command <$> parseNumber  -- Checks that the input number is either a float or an integer
            where
                parseForward = string "forward" >> pure GoForward 
                parseBackward = string "back" >> pure GoBackward   
                parseLeft = string "left" >> pure TurnLeft
                parseRight = string "right" >> pure TurnRight

        -- Handles repeat statements --
        parseRepeat :: Parser HogoCode
        parseRepeat = do
            string "repeat"
            hspace
            num <- many $ satisfy isDigit   -- Checks an integer is given for the number of times to repeat --
            hspace
            char '['
            -- Parses each line of hogocode within the repeat statement until the repeat is closed with ] --
            code <- between stopParse (char ']') $ parseLine `sepEndBy` stopParse
            hspace
            pure (Repeat (read num) code)

        -- Parses commands to change the pen's colour. This requires a string --
        parseColour :: Parser HogoCode
        parseColour = do
            string "colour"
            hspace
            -- Returns a string of a colour depending on the colour the user has input --
            colour <- parseRed <|> parseBlue <|> parseGreen <|> parseBlack <|> parsePink <|> parsePurple
            hspace
            pure (PenColour colour)
            where
                parseRed = string "red" >> pure "red"
                parseBlue = string "blue" >> pure "blue"
                parseGreen = string "green" >> pure "green"
                parseBlack = string "black" >> pure "black"
                parsePink = string "pink" >> pure "pink"
                parsePurple = string "purple" >> pure "purple"

        -- Parses other commands that only require the keyword --
        parseOther :: Parser HogoCode
        parseOther = do
            command <- parseHome <|> parsePenup <|> parsePendown <|> parseClearscreen
            hspace
            pure command
            where
                parseHome = string "home" >> pure GoHome
                parsePenup = string "penup" >> pure PenUp
                parsePendown = string "pendown" >> pure PenDown
                parseClearscreen = string "clearscreen" >> pure ClearScreen

-- Checks user input is either a float or an integer --
parseNumber :: Parser Float
parseNumber = try parseFloat <|> parseInt  -- Attempts to parse a float first, then attempts to parse an integer
    where
        -- Parses an integer --
        parseInt :: Parser Float
        parseInt = do
            x <- some $ satisfy isDigit
            pure $ read x

        -- To satisfy being a float, the input must contain digits around a single decimal point --
        parseFloat :: Parser Float
        parseFloat = do
            whole <- many $ satisfy isDigit
            _ <- char '.'
            fraction <- many $ satisfy isDigit
            pure $ read $ whole <> "." <> fraction






