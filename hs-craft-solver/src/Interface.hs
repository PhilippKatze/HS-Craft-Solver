module Interface
    ( startup
    ) where

import Ingredients
import Data.List
import Data.Char
import Text.Read
import Data.Maybe
import Solver
import Data.Ix
import System.Console.ANSI
import qualified Data.Vector as V

startup :: IO ()
startup = do 

    allIngredients <- fetchIngredients
    allReceips <- fetchReceipts
    let allSkills = zip [0..] (nub $ map skil allReceips) 

    putStrLn "-------------Haskell Craft Solver-------------"
    printOption "please select the piece you want to optimate" --ARMOURING
    putStrLn $ "Type your number for the choosen Profession: " ++ concatMap (\(number, prof) -> "[" ++show number ++"]" ++ show prof ++ ";  " ) allSkills
    choosenNumber <- validateInput (`elem` map (show.fst) allSkills)
    let skilt = snd $ fromJust $ find ((== choosenNumber). show .fst) allSkills

    let receipesFromSkill = filter ((== skilt) . skil) allReceips
    let possibleLevels = nub $ concatMap (range . levels) receipesFromSkill
    let possibleTypes =  zip [0..] $ nub $ map typ receipesFromSkill

    printOption "please select the Type of the crafted piece" --BOOTS
    putStrLn $ "Type your number for the choosen Type: " ++ concatMap (\(number, typp) -> "[" ++show number ++"]" ++ show typp ++ ";  " ) possibleTypes
    typNumber <- validateInput (`elem` map (show.fst) possibleTypes)
    let typString = snd $ fromJust $ find ((== typNumber). show .fst) possibleTypes


    printOption "please select the Level of the crafted piece" --100-103
    level <- validateInput (`elem` map show possibleLevels)

    let choosenReceips = fromJust $ find (\ing -> typString == typ ing && inRange (levels ing) (read level)) receipesFromSkill


    printOption "please select the minimum durability" --200 
    durabilityChoosen <- validateInput $ isJust . (readMaybe :: (String -> Maybe Int))
    let durability = floor $ (1.4 *  fromIntegral ( snd (dura choosenReceips)) ) - read durabilityChoosen 

    let attributenames = nub $  V.toList $ V.concatMap (V.map (map toLower . fst) . identifications) $ V.fromList allIngredients

    printOption "please select the wanted stats out of the following list" --xpbonus 
    putStrLn $ "Possible stats are: " ++ show attributenames
    stats <- validateInput $ all (`elem` attributenames) . words

    solve skilt (words $ map toUpper stats) durability (read level) allIngredients

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

    

