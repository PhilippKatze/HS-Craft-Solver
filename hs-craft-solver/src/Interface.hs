module Interface
    ( startup
    ) where

import Ingredients
import Data.List
import Data.Char
import Text.Read
import Data.Maybe
import Solver
import System.Console.ANSI
import qualified Data.Vector as V

startup :: IO ()
startup = do 

    allIngredients <- fetchIngredients
    allReceips <- fetchReceipts

    putStrLn "-------------Haskell Craft Solver-------------"
    printOption "please select the piece you want to optimate" --Helmet
    piece <- validateInput (=="WOODWORKING")

    printOption "please select the Level of the crafted piece" --100-103
    level <- validateInput $ isJust . (readMaybe :: (String -> Maybe Int))

    printOption "please select the minimum durability" --200 
    durability <- validateInput $ isJust . (readMaybe :: (String -> Maybe Int))

    let attributenames = nub $  V.toList $ V.concatMap (V.map (map toLower . fst) . identifications) $ V.fromList allIngredients
    --let skillss = nub $  V.toList $ V.concatMap skills $ V.fromList allIngredients --debug

    printOption "please select the wanted stats out of the following list" --xpbonus 
    putStrLn $ "Possible stats are:" ++ show attributenames
    --putStrLn $ "All Skills:" ++ show skillss
    print allReceips
    stats <- validateInput $ all (`elem` attributenames) . words

    solve piece (words $ map toUpper stats) (read durability) (read level) allIngredients

    return ()

-- TODO validate inputs for each case
validateInput :: (String -> Bool) -> IO String
validateInput cond = do
    inputStr <- getLine
    if cond inputStr 
        then return inputStr 
        else putStrLn "Your input was not valid" >> validateInput cond


printOption :: String -> IO ()
printOption str = do
    clearScreen
    putStrLn "-----------------------------------------------------------------------"
    putStrLn str
    putStrLn "-----------------------------------------------------------------------"
    putStrLn " "

    

