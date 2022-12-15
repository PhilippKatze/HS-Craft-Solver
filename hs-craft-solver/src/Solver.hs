{-# LANGUAGE BangPatterns #-}
module Solver
    ( solve,
    effects
    ) where

import Ingredients
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace
import Control.Concurrent.Async


data RecipeResult = RecipeResult {itemnames::[String], stats::[(String,Int)], finalDurability::Int} deriving Show


solve :: String -> [String] -> Int -> Int -> [Ingredient] -> IO ()
solve skill attributes mindurability lvl allIngredients = do
    let allSkillIngredients = filter (elem skill . skills) allIngredients
    let allSkillIngredientsInLevelRange = filter ((<=lvl) . level) allSkillIngredients
        
    let durabilityIngredients = filter ((>0) .durability) allSkillIngredientsInLevelRange
    let effectivenessIngredients = filter hasEffectivness allSkillIngredientsInLevelRange
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

    --Alle Rezepte mit ohne effectivness items müssen performanter klappen als zu bruteforcen (heuristik? search algorithms?)
    
    --Alle Rezepte mit effectivness müssen grebruteforced werden 
    -- Liste mit  5er Permutation mit jeweils allen anderen items kombiniert der liste der effectiveness items
    -- multi threaded
    -- mpi support ? 
    skyline <- newMVar []

    -- First try dont differ between items just bruteforce all
    let allIngrd = nub $ concat [effectivenessIngredients,durabilityIngredients,statIngredients] -- adding skill req items?
    let multiChunks = chunksOf 16 allIngrd --how many threads?
    !ids <- (mapM  (\part -> async $ solvePart skyline attributes mindurability allIngrd part) multiChunks)
    mapM_ wait ids --required to trigger lazyness
    --takeMVar handle
    print "wait done"
    listReceips <- readMVar skyline
    print listReceips
    return ()

solvePart :: MVar [RecipeResult] -> [String] -> Int -> [Ingredient] -> [Ingredient] -> IO ()
solvePart skyline attributes mindurability allIng partialIng = do
    forM_ partialIng 
        (\p -> forM_ allIng 
            (\a1 -> forM_ allIng 
                (\a2 -> forM_ allIng 
                    (\a3 -> forM_ allIng 
                        (\a4 -> forM_ allIng 
                            (\a5 -> testForSkyline attributes skyline $ createReceipe attributes [p,a1,a2,a3,a4,a5]))))))
    --tes <- mapM (testForSkyline attributes skyline) allReceipResults 
    --print allReceipResults

--TODO cleanup? xD
createReceipe :: [String] -> [Ingredient] -> RecipeResult
createReceipe attr ingr = RecipeResult (map name ingr) 
                                  (map (\att ->
                                     (att, 
                                     sum $ map (\(ing, ind)-> (`div` 100) $ ((effList!!ind) + 100) * (foundAttribute $ find ((== att) . fst) $ identifications ing)) $ zip ingr [0,1..]) 
                                  )attr)
                                  (sum $ map durability ingr)
    where
        effList = effectiveList ingr
        foundAttribute :: Maybe (String,(Int,Int)) -> Int
        foundAttribute (Just (_,(_,x))) = x
        foundAttribute Nothing = 0



effectiveList :: [Ingredient] -> [Int]
effectiveList = addLists . map (addLists . map (\(ind, effType) -> effects ind effType) . zip [0,1..] . effectiveness)
  where
    addLists = foldl (zipWith (+)) [0,0,0,0,0,0]

--TODO MEMORIZE WOULD BE EFFICIENT
effects :: Int -> (String,Int) -> [Int]
effects pos (_,0) = [0,0,0,0,0,0]
effects pos ("above",perc) = map (\ind -> if (ind<pos) && even (ind-pos) then perc else 0 ) [0,1,2,3,4,5]
effects pos ("under",perc) = map (\ind -> if (ind>pos) && even (ind-pos) then perc else 0 ) [0,1,2,3,4,5]
effects pos ("right",perc) = map (\ind -> if even pos && odd ind then perc else 0 ) [0,1,2,3,4,5]
effects pos ("left",perc) = map (\ind  -> if odd pos && even ind then perc else 0 ) [0,1,2,3,4,5]
effects pos ("touching",perc) = map (\ind  -> 
    if (even pos && (ind-pos == -2 || ind-pos == 2 || ind-pos == 1)) 
        || (odd pos && (ind-pos == 2 || ind-pos == -2 || ind-pos == -1)) 
        then perc else 0 ) [0,1,2,3,4,5]
effects pos ("notTouching",perc) = map (\ind  -> 
    if (even pos && (ind-pos /= -2 && ind-pos /= 2 && ind-pos /= 1)) 
        || (odd pos && (ind-pos /= 2 && ind-pos /= -2 && ind-pos /= -1)) 
        then perc else 0 ) [0,1,2,3,4,5]
effects _ _ = error "error with reading the effectiveness"


testForSkyline :: [String] -> MVar [RecipeResult] -> RecipeResult -> IO ()
testForSkyline attributes skyline receipt = do
    currentSkyline <- readMVar skyline
    --print receipt
    let gotDominated = any ((True==) . dominating attributes receipt) currentSkyline --rezept wird von dominiert -> discard
    let filteredskylinePoints = filter (\sky -> not $ dominating attributes sky receipt) currentSkyline
    !_ <- if gotDominated then swapMVar skyline filteredskylinePoints else swapMVar skyline (receipt:filteredskylinePoints)
    return ()

dominating :: [String] -> RecipeResult -> RecipeResult -> Bool
dominating attributes first dominating = and $ map (\att -> (fi att dominating) > (fi att first)) attributes
  where
    fi attr receipt = foundAttribute $ find ((== attr) . fst) $ stats receipt
    foundAttribute :: Maybe (String,Int) -> Int
    foundAttribute (Just (_,x)) = x
    foundAttribute Nothing = minBound :: Int