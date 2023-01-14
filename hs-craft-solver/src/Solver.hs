{-# LANGUAGE DeriveGeneric  #-}
module Solver
    ( solve
    ) where

import Ingredients
    ( Ingredient(effectiveness, skills, level, durability, name, identifications, isEffectniss),
      hasSkillPointReq )
import Control.Monad ( unless, foldM, when )
import Data.List.Split ( chunksOf )
import Data.List ( find, nub )
import Debug.Trace ()
import Control.Concurrent.Async ( mapConcurrently )
import Control.DeepSeq ( NFData(..) )
import Control.Exception ()
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics ( Generic )
import Control.Parallel ()
import qualified Data.Vector as V


data RecipeResult = RecipeResult {itemnames::V.Vector String, stats::V.Vector (String,Int), finalDurability::Int} deriving (Show, Generic)
instance NFData RecipeResult where rnf = genericRnf



solve :: String -> [String] -> Int -> Int -> [Ingredient] -> IO ()
solve skill attributes mindurability lvl allIngredients = do
    let allSkillIngredients = filter (elem skill . skills) allIngredients
    let allSkillIngredientsInLevelRange = filter ((<=lvl) . level) allSkillIngredients
        
    let durabilityIngredients = filter ((>0) .durability) allSkillIngredientsInLevelRange
    let effectivenessIngredients = filter isEffectniss allSkillIngredientsInLevelRange
    let skillReqIngredients = filter hasSkillPointReq allSkillIngredientsInLevelRange
    let statIngredients = filter (any ((`elem` attributes) . fst) . identifications) allSkillIngredientsInLevelRange --TODO filter effectiveness items

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

    --let tests = filter ((`elem` ["Elephelk Trunk","Green Foot","Major's Badge","Accursed Effigy"]) . name) allIngredients
    --let test2 = [tests!!2,tests!!0,tests!!3,tests!!1,tests!!3,tests!!1]
    --mapM_ print test2
    --print $ effectiveList $ V.fromList test2
    --mapM_ (\(ing,pos) -> print $ map (effects pos) (V.toList $ effectiveness ing)) $ zip test2 [0..]
    --print $ foldl (zipWith (+)) [0,0,0,0,0,0] $ concatMap (\(ing,pos) -> map (V.toList . effects pos) (V.toList $ effectiveness ing)) $ zip test2 [0..]


    --effects :: Int -> (String,Int) -> V.Vector Int

    --Alle Rezepte mit ohne effectivness items müssen performanter klappen als zu bruteforcen (heuristik? search algorithms?)
    
    -- mpi support ? 
    let allIngrd = nub $ concat [effectivenessIngredients,durabilityIngredients,statIngredients] -- adding skill req items?
    let multiChunks = chunksOf (div (length allIngrd) 16) allIngrd --how many threads?
    putStrLn $ "Threads started: " ++ show (length multiChunks)
    results <- mapConcurrently (solvePart attributes mindurability allIngrd) multiChunks

    print "wait done"
    print results
    return ()

solvePart :: [String] -> Int -> [Ingredient] -> [Ingredient] -> IO [RecipeResult]
solvePart attributes mindurability allIng partialIng = do
    foldM (\list value -> testForSkyline attributesVec list $ createReceipe mindurability attributesVec $ V.fromList value) [] allpermutations
    where
      attributesVec =  V.fromList attributes
      allpermutations = [[p,a1,a2,a3,a4,a5] | p <- partialIng, a1 <- allIng, a2 <- allIng, a3 <- allIng, a4 <- allIng, a5 <- allIng]
    

--TODO cleanup? xD
--first calc durability and discard, when too low
createReceipe :: Int -> V.Vector String -> V.Vector Ingredient -> Maybe RecipeResult
createReceipe mindurability attr ingr
  | dura + mindurability < 0 = Nothing
  | otherwise = Just $ RecipeResult 
                (V.map name ingr) 
                (V.map (\att -> (att, sum $ V.zipWith(\ ing ind -> scaledValue ind $ foundAttribute (V.find ((== att) . fst) $ identifications ing)) ingr (V.generate (length ingr) id)))attr) 
                dura
    where
        dura = sum $ V.map durability ingr
        scaledValue pos i = (`div` 100) ((effList V.! pos) + 100)*i
        effList = effectiveList ingr
        foundAttribute :: Maybe (String,(Int,Int)) -> Int
        foundAttribute (Just (_,(_,x))) = x
        foundAttribute Nothing = 0


effectiveList :: V.Vector Ingredient -> V.Vector Int
effectiveList ingrednients
  | someEffective ingrednients = addLists $ V.map (\(ing,pos) -> (V.! pos) $ effectiveness ing) filteredEffectivnessList
  | otherwise = V.replicate 6 0
  where
    filteredEffectivnessList = V.filter (isEffectniss . fst) $ V.zip ingrednients $ V.generate (length ingrednients) id 
    someEffective = V.or . V.map isEffectniss
    addLists = V.foldl (V.zipWith (+)) (V.replicate 6 0)


testForSkyline :: V.Vector String -> [RecipeResult] -> Maybe RecipeResult -> IO [RecipeResult]
testForSkyline _ currentSkyline  Nothing = return currentSkyline
testForSkyline attributes currentSkyline (Just receipt) = do
    let gotDominated = any (dominating attributes receipt) currentSkyline --rezept wird von dominiert -> discard
    let filteredskylinePoints = if not gotDominated then filter (\sky -> not $ dominating attributes sky receipt) currentSkyline else currentSkyline
    let noncompareable = incompareable attributes receipt filteredskylinePoints
    let newSkyline = if noncompareable then receipt:filteredskylinePoints else filteredskylinePoints 
    when noncompareable (putStrLn "not comparable")
    when noncompareable (print receipt) 
    when noncompareable (print currentSkyline) 
    when noncompareable (print newSkyline)

    unless gotDominated (putStrLn "not gotDominated")
    unless gotDominated (print receipt) 
    unless gotDominated (print currentSkyline)
    unless gotDominated (print newSkyline)

    return newSkyline

dominating :: V.Vector String -> RecipeResult -> RecipeResult -> Bool
dominating attributes first dominat = all (\att -> fi att dominat >= fi att first) attributes && any (\att -> fi att dominat > fi att first) attributes
  where
    fi attr receipt = foundAttribute $ find ((== attr) . fst) $ stats receipt
    foundAttribute :: Maybe (String,Int) -> Int
    foundAttribute (Just (_,x)) = x
    foundAttribute Nothing = error "Receipt with wrong attributes"

incompareable :: V.Vector String -> RecipeResult -> [RecipeResult] -> Bool
incompareable _ _ [] = True
incompareable attributes receip othereceips = any (\att -> all (\recei -> fi att receip > fi att recei) othereceips) attributes
  where
    fi attr receipt = foundAttribute $ find ((== attr) . fst) $ stats receipt
    foundAttribute :: Maybe (String,Int) -> Int
    foundAttribute (Just (_,x)) = x
    foundAttribute Nothing = error "Receipt with wrong attributes"
