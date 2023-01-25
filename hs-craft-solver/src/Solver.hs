{-# LANGUAGE DeriveGeneric  #-}
module Solver
    ( solve
    ) where

import Ingredients
    ( Ingredient(Ingredient, effectiveness, skills, level, durability, name, identifications, isEffectniss),
      hasSkillPointReq )
import Control.Monad ( unless, foldM, when )
import Data.List.Split ( chunksOf )
import Data.List ( find, nub, sortOn )
import Debug.Trace (trace)
import Control.Concurrent.Async ( mapConcurrently )
import Control.DeepSeq ( NFData(..), force )
import Control.Exception (evaluate)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics ( Generic )
import Control.Parallel ()
import Data.Ord ( Down(Down) )
import qualified Data.Vector as V


data RecipeResult = RecipeResult {itemnames::V.Vector String, stats::V.Vector (String,Int), finalDurability::Int} deriving (Show, Generic)
instance NFData RecipeResult where rnf = genericRnf

instance Eq RecipeResult where
   (==) (RecipeResult _ attributes dur) (RecipeResult _ attributes2 dur2) = attributes == attributes2 && dur == dur2


solve :: String -> [String] -> Int -> Int -> [Ingredient] -> IO ()
solve skill attributes mindurability lvl allIngredients = do
    let allSkillIngredients = filter (elem skill . skills) allIngredients
    let allSkillIngredientsInLevelRange = filter ((<=lvl) . level) allSkillIngredients
        
    let durabilityIngredients = filter ((>0) .durability) allSkillIngredientsInLevelRange
    let effectivenessIngredients = filter isEffectniss allSkillIngredientsInLevelRange
    let skillReqIngredients = filter hasSkillPointReq allSkillIngredientsInLevelRange
    let statIngredients = filter (any ((`elem` attributes) . fst) . identifications) allSkillIngredientsInLevelRange --TODO filter effectiveness items


    let allIngrd = nub $ concat [effectivenessIngredients,durabilityIngredients,statIngredients] -- adding skill req items?

    --remove unused attributes
    let setAttributes attrub (Ingredient nam tie lvel skil _ dur req char durat isEff effe) = Ingredient nam tie lvel skil attrub dur req char durat isEff effe --TODO replace with lenses

    let ingWithAjustedStats = map (\ing -> setAttributes (V.fromList (map (\att -> (att, (0,findIngridientAttribute att ing))) attributes)) ing) allIngrd

    ------DEBUG-------
    putStrLn $ "All Effectivness Ingredients: " ++ show (map name effectivenessIngredients)
    putStrLn ""
    putStrLn $ "All Stat Ingredients: " ++ show (map name statIngredients)
    putStrLn ""
    putStrLn $ "All Durability Ingredients: " ++ show (map name durabilityIngredients)
    putStrLn ""
    putStrLn $ "All Skill Requirement Ingredients: " ++ show (map name skillReqIngredients)
    putStrLn ""
    putStrLn "Calculation starting..."

    --Alle Rezepte mit ohne effectivness items müssen performanter klappen als zu bruteforcen (heuristik? search algorithms?)
    
    -- mpi support ? 
    let multiChunks = chunksOf (div (length ingWithAjustedStats) 16) ingWithAjustedStats --how many threads?
    putStrLn $ "Threads started: " ++ show (length multiChunks)
    resultDList <- mapConcurrently (solvePart attributes mindurability ingWithAjustedStats) multiChunks
    let results = concat resultDList

    finalSkyline <- foldM (\list value -> testForSkyline (V.fromList attributes) list (fst value + 1, length results) (snd value)) [] $ zip [0..] $ map return results

    print "Threadresults"
    print results
    print "final skyline"

    --sort finalSkyline
    let sortedFinalSkyline = sortOn (Down . findReceiptAttribute 0) finalSkyline
    mapM_ print sortedFinalSkyline
    --return ()

solvePart :: [String] -> Int -> [Ingredient] -> [Ingredient] -> IO [RecipeResult]
solvePart attributes mindurability allIng partialIng = do
    foldM (\list value -> testForSkyline attributesVec list (fst value + 1, allpermutationsAmount)$ createReceipe mindurability attributesVec $ V.fromList $ snd value) [] $ zip [0..] allpermutations
    where
      attributesVec =  V.fromList attributes
      allpermutations = [[p,a1,a2,a3,a4,a5] | p <- partialIng, a1 <- allIng, a2 <- allIng, a3 <- allIng, a4 <- allIng, a5 <- allIng]
      allpermutationsAmount = length partialIng * (length allIng ^ 5)
    

--TODO cleanup? xD
--first calc durability and discard, when too low
createReceipe :: Int -> V.Vector String -> V.Vector Ingredient -> Maybe RecipeResult
createReceipe mindurability attr ingr
  | dura + mindurability < 0 = Nothing
  | otherwise = Just $ RecipeResult 
                (V.map name ingr) 
                (V.generate (length attr) (\att -> ((V.! att) attr, 
                    V.foldl (\li va -> (+ li) $ scaledValue va $ snd $ snd ((V.!) (identifications $ (V.! va) ingr) att)) 0 $ V.generate 6 id))) 
                dura
    where
        dura :: Int
        dura = V.foldl (\li va -> durability va + li) 0 ingr --sum over durability of ingr
        scaledValue :: Int -> Int -> Int
        scaledValue pos i = (`div` 100) (((effList V.! pos) + 100)*i)
        effList :: V.Vector Int
        effList = effectiveList ingr


effectiveList :: V.Vector Ingredient -> V.Vector Int
effectiveList ingrednients
  | someEffective = V.generate 6 (\index -> V.foldl (\li va -> (+ li) $ (V.! index)$ (V.! va) $ effectiveness $ (V.! va) ingrednients) 0 $ V.generate 6 id) --(V.! pos) $ effectiveness ing) --0 filteredEffectivnessList
  | otherwise = V.replicate 6 0
  where
    someEffective = V.any isEffectniss ingrednients


testForSkyline :: V.Vector String -> [RecipeResult] -> (Int,Int) -> Maybe RecipeResult -> IO [RecipeResult]
testForSkyline _ currentSkyline progress Nothing = return currentSkyline
testForSkyline attributes currentSkyline progress (Just receipt) = do
    let gotDominated = any (dominating attributes receipt) currentSkyline --rezept wird von dominiert -> discard
    let filteredskylinePoints = if not gotDominated then filter (\sky -> not $ dominating attributes sky receipt) currentSkyline else currentSkyline
    let noncompareable = incompareable attributes receipt filteredskylinePoints
    let newSkyline = if noncompareable && not gotDominated then receipt:filteredskylinePoints else filteredskylinePoints -- && notElem receipt filteredskylinePoints

    when (mod (fst progress) 1000000 == 0) (print $ "Progress " ++ show ( 100 * (/) (fromIntegral $ fst progress) (fromIntegral $ snd progress)))

    _ <- evaluate (force newSkyline)
    return newSkyline

dominating :: V.Vector String -> RecipeResult -> RecipeResult -> Bool
dominating attributes first dominat = all (\att -> findReceiptAttribute att dominat >= findReceiptAttribute att first) (V.generate (length attributes) id) -- && finalDurability dominat >= finalDurability first)
                                        -- && (any (\att -> findReceiptAttribute att dominat > findReceiptAttribute att first) (V.generate (length attributes) id) )-- || finalDurability dominat > finalDurability first)

incompareable :: V.Vector String -> RecipeResult -> [RecipeResult] -> Bool
incompareable _ _ [] = True
incompareable attributes receip othereceips = any (\att -> any (\recei -> findReceiptAttribute att receip > findReceiptAttribute att recei) othereceips)  (V.generate (length attributes) id)


findReceiptAttribute :: Int -> RecipeResult -> Int
findReceiptAttribute index receipt = snd $ (V.!) (stats receipt) index


findIngridientAttribute :: String -> Ingredient -> Int
findIngridientAttribute attr ingri = foundAttribute $ find ((== attr) . fst) $ identifications ingri
  where
    foundAttribute :: Maybe (String,(Int,Int)) -> Int
    foundAttribute = maybe 0 (snd . snd)
    --foundAttribute (Just (_, (_,x)))= x
    --foundAttribute Nothing = 0