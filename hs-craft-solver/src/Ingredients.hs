{-# LANGUAGE OverloadedStrings, LambdaCase  #-}

module Ingredients
    ( Ingredient, 
    identifications,
    skills,
    name,
    level,
    durability,
    effectiveness,
    fetchIngredients, 
    hasEffectivness, 
    hasSkillPointReq,
    getURLJSON
    ) where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import System.Exit
import System.Console.ANSI

data Ingredient = Ingredient {
                    name::String,
                    tier::Int,
                    level::Int,
                    skills::[String],
                    identifications::[(String, (Int, Int))],
                    --items only
                    durability::Int,
                    requirements:: [(String, Int)],
                    --consumable only
                    charges::Int,
                    duration::Int,
                    -------------------------------
                    effectiveness:: [(String, Int)]} deriving (Show, Eq)

newtype IngredientList = IngredientList [Ingredient] deriving Show


instance FromJSON Ingredient where
    parseJSON = withObject "Ingredient" $ \i -> do
        name <- i .: "name"
        tier <- i .: "tier"
        level <- i .: "level"
        skills <- parseJSONList =<< i .: "skills"
        itemOnlyIDs <- i .: "itemOnlyIDs" 
        durability <- itemOnlyIDs .: "durabilityModifier"

        req <- (i .: "itemOnlyIDs":: Parser (M.Map String Int))
        let requirements = filter ((/= "durabilityModifier") . fst) $ M.toList req

        --Kinda buggy? not alway set
        consumOnlyIDs <- i .:? "consumableOnlyIDs" .!= i
        charges <- consumOnlyIDs .:? "charges" .!= 0
        duration <- consumOnlyIDs .:? "duration" .!= 0

        effect <- (i .: "ingredientPositionModifiers":: Parser (M.Map String Int))
        let effectivness = M.toList effect

        ident <- (i .: "identifications":: Parser (M.Map String Object))
        identifications <- mapM (\j -> do
            min <- snd j .: "minimum"
            max <- snd j .: "maximum"
            return (fst j, (min, max))) 
            $ M.toList ident

        return $ Ingredient name tier level skills identifications durability requirements charges duration effectivness

instance FromJSON IngredientList where
    parseJSON = \case
        Object o -> (o .: "data") >>= fmap IngredientList . parseJSON
        x -> fail $ "could not parse Ingredient List " ++ show x

--https://api.wynncraft.com/v2/ingredient/search/identifications/&xpbonus%3C;%3E

--https://api.wynncraft.com/v2/ingredient/search/tier/{level} --way to go?

--https://api.wynncraft.com/v2/recipe/list

getURLJSON :: String -> IO BL.ByteString
getURLJSON url = do
    request <- parseRequest url
    response <- httpLBS request
    return (getResponseBody response)


fetchIngredients :: IO [Ingredient]
fetchIngredients = do
    star0 <- fetchIngredientsUrl "https://api.wynncraft.com/v2/ingredient/search/tier/0"
    star1 <- fetchIngredientsUrl "https://api.wynncraft.com/v2/ingredient/search/tier/1"
    star2 <- fetchIngredientsUrl "https://api.wynncraft.com/v2/ingredient/search/tier/2"
    star3 <- fetchIngredientsUrl "https://api.wynncraft.com/v2/ingredient/search/tier/3"
    let allstar = concat [star0,star1,star2,star3]
    return allstar


fetchIngredientsUrl :: String -> IO [Ingredient]
fetchIngredientsUrl url = do
    json <- getURLJSON url
    let ingrList =  (decode json :: Maybe IngredientList)
    case ingrList of
        Nothing -> putStrLn "Parsing the Wynn API failed" >> die "Parsing Error" 
        Just (IngredientList x) -> return x

hasEffectivness :: Ingredient -> Bool
hasEffectivness = any ((>0) . snd) . effectiveness

hasSkillPointReq :: Ingredient -> Bool
hasSkillPointReq = any ((>0) . snd) . requirements