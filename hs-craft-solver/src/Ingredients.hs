{-# LANGUAGE OverloadedStrings, LambdaCase  #-}

module Ingredients
    ( Ingredient, 
    fetchIngredients, 
    hasEffectivness, 
    hasSkillPointReq,
    getURLJSON
    ) where

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import Data.Map
import Data.Maybe
import qualified Data.Vector as V
import Data.ByteString.Lazy as BL
import System.Exit

data Ingredient = Ingredient {
                    name::String,
                    tier::Int,
                    level::Int,
                    skills::[String],
                    identifications::Map String (Int, Int),
                    durability::Int,
                    charges::Int,
                    requirements::Map String Int,
                    effictivness::Map String Int} deriving Show

newtype IngredientList = IngredientList [Ingredient] deriving Show


instance FromJSON Ingredient where
    parseJSON = withObject "Ingredient" $ \i -> do
        name <- i .: "name"
        tier <- i .: "tier"
        level <- i .: "level"
        itemOnlyIDs <- i .: "itemOnlyIDs" 
        durability <- itemOnlyIDs .: "durabilityModifier"

        return $ Ingredient name tier level skills Data.Map.empty durability 0 Data.Map.empty Data.Map.empty

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
    json <- getURLJSON "https://api.wynncraft.com/v2/ingredient/search/tier/0"
    let ingrList =  (decode json :: Maybe IngredientList)
    case ingrList of
        Nothing -> putStrLn "Parsing the Wynn API failed" >> die "Parsing Error" 
        Just (IngredientList x) -> return x

hasEffectivness :: Ingredient -> Bool
hasEffectivness i = True

hasSkillPointReq :: Ingredient -> Bool
hasSkillPointReq i = True