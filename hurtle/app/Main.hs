-- This is the main entry point for your Hurtle viewer.
import System.IO
import Text.Megaparsec
import Control.Monad (forever)
import Hurtle.Parser (parseHogo)
import Hurtle.Types
import Graphics.Gloss
import Graphics.Gloss.Juicy

window :: Display
window = InWindow "CS141 Hogocode" (700, 700) (10, 10)

background :: Color
background = white

-- The image of the 'turtle' --
turtlePosition :: IO (Maybe Picture)
turtlePosition = loadJuicy "assets/ant.png"

-- Stores information about the turtle's current state --
data TurtleState = TurtleState {
    position :: (Float, Float),
    angle :: Float,
    penDown :: Bool,
    linesDrawnSoFar :: [((Float, Float), (Float,Float))],
    penColor :: String
} 

-- Main -----------------------------------------------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = forever $ do
    putStrLn "Please enter a hogo code file path: "
    hFlush stdout   
    file <- getLine
    contents <- readFile file
    -- Checks that the user has input a valid file path --
    case parse parseHogo file contents of
         Left err -> putStrLn $ errorBundlePretty err  
         Right code -> do
            putStrLn "This is a valid hogocode file"
            picture <- turtlePosition   -- Obtains the image of the turtle --
            case picture of
                -- Opens a gloss window to display the results of the hogocode: the turtle's final position and the lines it drew --
                Just turtlePic -> display window background (linesResult code <> translatedTurtle)
                    where
                        translatedTurtle = translate (getX code) (getY code) rotatedTurtle
                        rotatedTurtle = rotate (getAngle code) scaledTurtle
                        scaledTurtle = (Scale 0.1 0.1 turtlePic)   
                Nothing -> print "Image of turtle could not be loaded"

-- Turtle drawing ----------------------------------------------------------------------------------------------------------------------------------------------------

-- Obtains every line to be drawn at the end of the hogo program (as type Picture) --
linesResult :: HogoProgram -> Picture
linesResult code = color (lineColour) (combineLines (linesResultBody (getLinesAtEnd $ applyHogocode code) []))
    where
        -- Uses recursion to convert every pair of points in a list into a list of lines --
        linesResultBody :: [((Float, Float), (Float,Float))] -> [Picture] -> [Picture]
        linesResultBody [] result = result
        linesResultBody (((a, b), (c, d)):xs) result = linesResultBody xs ((line [(a, b), (c, d)]):result)

        -- Obtains the list of all lines in the program, from the turtle state value --
        getLinesAtEnd :: TurtleState -> [((Float, Float), (Float,Float))]
        getLinesAtEnd (TurtleState (_, _) _ _ lines _) = lines

        -- Obtains the string value for 'colour' from a turtle state value, and returns the corresponding value as type 'colour' -- 
        getFinalColour :: TurtleState -> Color
        getFinalColour (TurtleState (_, _) _ _ _ colour) = case colour of
            "red" -> red
            "green" -> green
            "blue" -> blue
            "black" -> black
            "pink" -> rose
            "purple" -> violet

        -- Uses recursion to combine all lines in the list of lines into one picture --
        combineLines :: [Picture] -> Picture
        combineLines [] = Blank
        combineLines (x:xs) = x <> combineLines xs

        -- Obtains the colour of the drawing -- 
        lineColour = getFinalColour $ applyHogocode code

-- Turtle image ---------------------------------------------------------------------------------------------------------------------------------------------------

-- Obtains the x co-ordinate of the turtle after the drawing has been complete --
getX :: HogoProgram -> Float
getX code = getFinalPosX (applyHogocode code)
    where
        getFinalPosX :: TurtleState -> Float
        getFinalPosX (TurtleState (x, _) _ _ _ _) = x

-- Obtains the y co-ordinate of the turtle after the drawing has been complete --
getY :: HogoProgram -> Float
getY code = getFinalPosY (applyHogocode code)
    where
        getFinalPosY :: TurtleState -> Float
        getFinalPosY (TurtleState (_, y) _ _ _ _) = y

-- Obtains the angle the turtle is facing after the drawing has been complete --
getAngle :: HogoProgram -> Float
getAngle code = getFinalAngle (applyHogocode code)
    where
        getFinalAngle :: TurtleState -> Float
        getFinalAngle (TurtleState (_, _) angle _ _ _) = angle



-- Apply hogocode to turtle ---------------------------------------------------------------------------------------------------------------------------------

-- Calls the recursive function applyHogocodeBody to apply every line of hogocode from the turtle's initial state --
applyHogocode :: HogoProgram -> TurtleState
applyHogocode code = applyHogocodeBody code turtleStart
    where

        -- Uses recursion to apply every line of hogocode, and obtain the final turtle state value --
        applyHogocodeBody :: HogoProgram -> TurtleState -> TurtleState
        applyHogocodeBody [] state = state   -- Base case: Every hogocode instruction has been applied so return the final turtle state
        applyHogocodeBody (x:xs) state = case x of
            -- Repeat statements - handled the same except the state passed into the recursive call is the result of applying
            -- every line of hogocode within the repeat statement to the state at this point
            Repeat numTimes code -> applyHogocodeBody (xs) (applyHogocodeBody (repeatList numTimes code []) (state))
            a -> applyHogocodeBody (xs) (applyToState state x)  

        -- Represents the starting position of the turtle at the origin, when no lines have been drawn --
        turtleStart :: TurtleState
        turtleStart = TurtleState (0, 0) 0 True [] "black"

        -- Using recursion, repeats the hogocode in a hogoprogram a given number of times, and returns this as a new hogoprogram --
        repeatList :: Int -> HogoProgram -> HogoProgram -> HogoProgram
        repeatList 0 originalList newList = newList
        repeatList x originalList newList = repeatList (x - 1) originalList (originalList ++ newList)  

-- Applies each hogocode command -----------------------------------------------------------------------------------------------------------------------------

 -- Applies hogocode to a turtle state, and returns the new turtle state --
applyToState :: TurtleState -> HogoCode -> TurtleState
applyToState (TurtleState (x, y) angle penStatus lines colour) code = case code of

    -- Returns the state of the turtle after the command forward x has been applied (Its coords have changed) --
    (GoForward a) -> TurtleState newForwardCoords angle penStatus (line:lines) colour
        where
            -- Calculates the new position of the turtle, after it has moved forward (based on its angle and distance moved) --
            newForwardCoords = (x + (a * (sin $ radians angle)), y + (a * (cos $ radians angle)))   
            line = newLine penStatus (x, y) newForwardCoords  -- Adds the new line endpoints to the list of line endpoints

    -- Returns the state of the turtle after the command back x has been applied (Its coords have changed) --
    (GoBackward a) -> TurtleState newBackwardCoords angle penStatus (line:lines) colour
        where
            -- Calculates the new position of the turtle, after it has moved backwards (based on its angle and distance moved) --
            newBackwardCoords = (x - (a * (sin $ radians angle)), y - (a * (cos $ radians angle)))  
            line = newLine penStatus (x, y) newBackwardCoords  -- Adds the new line endpoints to the list of line endpoints
    
    -- Returns the state of the turtle after it has turned either left or right (Its angle has changed) --
    (TurnLeft a) -> TurtleState (x, y) (angle - a) penStatus lines colour
    (TurnRight a) -> TurtleState (x, y) (angle + a) penStatus lines colour

    -- Returns state of turtle after the command 'home'. This sets places it back at the origin and sets it to face north --
    (GoHome) -> TurtleState (0, 0) 0 penStatus lines colour

    -- Returns state of turtle after the command 'clearscreen'. This behaves the same as 'home' but also clears the list of line endpoints --
    (ClearScreen) -> TurtleState (0, 0) 0 penStatus [] colour
    
    (PenDown) -> TurtleState (x, y) angle True lines colour   -- Pen down -> Sets the pen to be used (true)
    (PenUp) -> TurtleState (x, y) angle False lines colour    -- Pen up -> Sets the pen not to be used (false)

    -- Changes the colour of the pen --
    (PenColour newColour) -> TurtleState (x, y) angle penStatus lines newColour



-- Converts a value in degrees to radians --
radians :: Float -> Float
radians x = x * (pi / 180)

-- When the pen is down, this function returns the end points of a line, based on the turtle's previous and new position -- 
newLine :: Bool -> (Float, Float) -> (Float, Float) -> ((Float, Float), (Float,Float))
newLine penStatus oldCoords newCoords =
    if penStatus then
        (oldCoords, newCoords)
    else
        ((0, 0), (0, 0))  -- The pen is up so do not return a line

