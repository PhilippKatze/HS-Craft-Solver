module Interface
    ( startup
    ) where

import Ingredients

startup :: IO ()
startup = do 

    allIngredients <- fetchIngredients
    let effectivenessIngredients = filter hasEffectivness allIngredients
    let skillReqIngredients = filter hasSkillPointReq allIngredients

    putStrLn "-------------Haskell Craft Solver-------------"
    putStrLn "please select the piece you want to optimate" --Helmet
    piece <- validateInput

    putStrLn "please select the Level of the crafted piece" --100-103
    level <- validateInput

    putStrLn "please select the minimum durability" --xpbonus
    durability <- validateInput

    putStrLn "please select the wanted stats out of the following list" --xpbonus
    stats <- validateInput

    return ()

-- TODO validate inputs for each case
validateInput :: IO String
validateInput = do
    inputStr <- getLine
    if inputStr == "test" 
        then return inputStr 
        else putStrLn "Your input was not valid" >> validateInput
    

