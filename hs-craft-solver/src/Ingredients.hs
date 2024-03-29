{-# LANGUAGE OverloadedStrings, LambdaCase  #-}

module Ingredients
    ( Ingredient (..), 
    Receipe (..),
    fetchIngredients, 
    hasSkillPointReq,
    getURLJSON,
    fetchReceipts
    ) where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import System.Exit ( die )
import System.Cached.JSON

data Ingredient = Ingredient {
                    name::String,
                    tier::Int,
                    level::Int,
                    skills::V.Vector String,
                    identifications::V.Vector (String, (Int, Int)),
                    --items only
                    durability::Int,
                    requirements:: V.Vector (String, Int),
                    --consumable only
                    charges::Int,
                    duration::Int,
                    -------------------------------
                    isEffectniss::Bool,
                    effectiveness:: V.Vector (V.Vector Int)} deriving (Show, Eq)

data Receipe = Receipe {
                    typ::String,
                    skil::String,
                    levels::(Int, Int),
                    materials::V.Vector (String,Int),
                    healthOrDamage::(Int,Int),
                    dura::(Int,Int)} deriving (Show, Eq)


newtype IngredientList = IngredientList [Ingredient] deriving Show
newtype ReceipeList = ReceipeList [Receipe] deriving Show


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
        let effectivness = V.fromList $ M.toList effect

        ident <- (i .: "identifications":: Parser (M.Map String Object))
        identifications <- mapM (\j -> do
            min <- snd j .: "minimum"
            max <- snd j .: "maximum"
            return (fst j, (min, max))) 
            $ M.toList ident

        let isEffect = not $ all ((== 0). snd) effectivness
        --effects :: Int -> (String,Int) -> V.Vector Int
        let allEffectivenesses = V.map (\pos -> V.foldl (\input value-> V.zipWith (+) (effects pos value) input) (V.replicate 6 0) effectivness) $ V.generate 6 id

        return $ Ingredient name tier level (V.fromList skills) (V.fromList identifications) durability (V.fromList requirements) charges duration isEffect allEffectivenesses

instance FromJSON Receipe where
    parseJSON = withObject "Receipe" $ \i -> do
        type' <- i .: "type"
        skil' <- i .: "skill"
        _ <- i .: "id"::Parser String
        levelSection <- i .: "level" 
        minLevel <- levelSection .: "minimum" 
        maxLevel <- levelSection .: "maximum" 
        hodSection <- i .: "healthOrDamage" 
        minhod <- hodSection .: "minimum" 
        maxhod <- hodSection .: "maximum" 
        duraSection <- i .:? "durability" .!= i
        mindura <- duraSection .:? "minimum" .!= 0
        maxdura <- duraSection .:? "maximum" .!= 0
        duratSection <- i .:? "duration" .!= i
        mindurat <- duratSection .:? "minimum" .!= 0
        maxdurat <- duratSection .:? "maximum" .!= 0
        mats <- (parseJSONList =<< i .: "materials":: Parser [Object])
        matList <- mapM (\j -> do
            min <- (j .:? "item" .!= "" :: Parser String)
            max <- (j .:? "amount" .!= 0 :: Parser Int)
            return (min, max)) 
            mats
        return $ Receipe type' skil' (minLevel,maxLevel) (V.fromList matList) (minhod,maxhod) (mindura + mindurat,maxdura + maxdurat)


instance FromJSON IngredientList where
    parseJSON = \case
        Object o -> (o .: "data") >>= fmap IngredientList . parseJSON
        x -> fail $ "could not parse Ingredient List " ++ show x

instance FromJSON ReceipeList where
    parseJSON = \case
        Object o -> (o .: "data") >>= fmap ReceipeList . parseJSON
        x -> fail $ "could not parse Receipe List " ++ show x

getURLJSON :: String -> IO BL.ByteString
getURLJSON url = do
    request <- parseRequest url
    response <- httpLBS request
    return (getResponseBody response)


fetchIngredients :: IO [Ingredient]
fetchIngredients = do
    star0 <- fetchIngredientsUrl "ingr-t0" "https://api.wynncraft.com/v2/ingredient/search/tier/0"
    star1 <- fetchIngredientsUrl "ingr-t1" "https://api.wynncraft.com/v2/ingredient/search/tier/1"
    star2 <- fetchIngredientsUrl "ingr-t2" "https://api.wynncraft.com/v2/ingredient/search/tier/2"
    star3 <- fetchIngredientsUrl "ingr-t3" "https://api.wynncraft.com/v2/ingredient/search/tier/3"
    let allstar = concat [star0,star1,star2,star3]
    return allstar

fetchReceipts :: IO [Receipe]
fetchReceipts = do
    concat <$> mapM (\skil -> fetchReceiptUrl ("receipt-"++skil) ("https://api.wynncraft.com/v2/recipe/search/skill/"++skil)) ["TAILORING","ALCHEMISM","COOKING","SCRIBING","WEAPONSMITHING","ARMOURING","WOODWORKING","JEWELING"]
  
fetchReceiptUrl :: String -> String -> IO [Receipe]
fetchReceiptUrl name url = do
    jsonCached <- (getCachedJSON "craft-solver-cache" (name++".json") url (60*24*7) :: IO (Maybe Object))
    let jsonstr = fromJust jsonCached
    let ingrList = (decode $ encode jsonstr) :: Maybe ReceipeList
    case ingrList of
        Nothing -> putStrLn "Parsing the Wynn API failed" >> die "Parsing Error" 
        Just (ReceipeList x) -> return x


fetchIngredientsUrl :: String -> String -> IO [Ingredient]
fetchIngredientsUrl name url = do
    jsonCached <- (getCachedJSON "craft-solver-cache" (name++".json") url (60*24*7) :: IO (Maybe Object))
    let jsonstr = fromJust jsonCached
    let ingrList =  (decode $ encode jsonstr) :: Maybe IngredientList
    case ingrList of
        Nothing -> putStrLn "Parsing the Wynn API failed" >> die "Parsing Error" 
        Just (IngredientList x) -> return x

hasSkillPointReq :: Ingredient -> Bool
hasSkillPointReq = any ((>0) . snd) . requirements

effects :: Int -> (String,Int) -> V.Vector Int
effects _ (_,0) = V.replicate 6 0 
effects pos ("above",perc) = V.map (\ind -> if (ind<pos) && even (ind-pos) then perc else 0 ) $ V.generate 6 id
effects pos ("under",perc) = V.map (\ind -> if (ind>pos) && even (ind-pos) then perc else 0 ) $ V.generate 6 id
effects pos ("right",perc) = V.map (\ind -> if even pos && ind == pos+1 then perc else 0 ) $ V.generate 6 id
effects pos ("left",perc) = V.map (\ind  -> if odd pos && ind == pos-1 then perc else 0 ) $ V.generate 6 id
effects pos ("touching",perc) = V.map (\ind  -> 
    if (even pos && (ind-pos == -2 || ind-pos == 2 || ind-pos == 1)) 
        || (odd pos && (ind-pos == 2 || ind-pos == -2 || ind-pos == -1)) 
        then perc else 0 ) $ V.generate 6 id
effects pos ("notTouching",perc) = V.map (\ind  -> 
    if ind /= pos && (even pos && (ind-pos /= -2 && ind-pos /= 2 && ind-pos /= 1)) 
        || (odd pos && (ind-pos /= 2 && ind-pos /= -2 && ind-pos /= -1)) 
        then perc else 0 ) $ V.generate 6 id
effects _ _ = error "error with reading the effectiveness"