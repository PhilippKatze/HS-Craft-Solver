{-# LANGUAGE BangPatterns, DeriveGeneric, ParallelListComp  #-}
module Solver
    ( solve,
    effects
    ) where

import Ingredients
import Control.Monad
import Data.List.Split
import Data.List
import Debug.Trace
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
import Control.Concurrent.MVar.Strict
import Control.Parallel.Strategies
import qualified Data.Vector as V


data RecipeResult = RecipeResult {itemnames::V.Vector String, stats::V.Vector (String,Int), finalDurability::Int} deriving (Show, Generic)
instance NFData RecipeResult where rnf = genericRnf



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
    skyline <- newMVar V.empty

    -- First try dont differ between items just bruteforce all
    let allIngrd = V.fromList $ nub $ concat [effectivenessIngredients,durabilityIngredients,statIngredients] -- adding skill req items?
    let multiChunks = map V.fromList $ chunksOf (div (length allIngrd) 16) $ V.toList allIngrd --how many threads?
    putStrLn $ "Threads started: " ++ show (length multiChunks)
    mapConcurrently (solvePart skyline (V.fromList attributes) mindurability allIngrd) multiChunks
    --let t = using (map (solvePart skyline attributes mindurability allIngrd) multiChunks) (parListChunk 3 rseq)
    --mapM_ wait ids 

    print "wait done"
    listReceips <- readMVar skyline
    print listReceips
    return ()

solvePart :: MVar (V.Vector RecipeResult) -> V.Vector String -> Int -> V.Vector Ingredient -> V.Vector Ingredient -> IO ()
solvePart skyline attributes mindurability allIng partialIng = 
    V.forM_ partialIng 
        (\p -> V.forM_ allIng 
            (\a1 -> V.forM_ allIng 
                (\a2 -> V.forM_ allIng 
                    (\a3 -> V.forM_ allIng 
                        (\a4 -> V.forM_ allIng 
                            (\a5 -> testForSkyline attributes skyline $ createReceipe attributes (V.fromList[p,a1,a2,a3,a4,a5])))))))
--TODO cleanup? xD
--first calc durability and discard, when too low
createReceipe :: V.Vector String -> V.Vector Ingredient -> RecipeResult
createReceipe attr ingr = RecipeResult 
                (V.map name ingr) 
                (V.map (\att -> (att, sum $ V.zipWith(\ ing ind -> (`div` 100) $ ((effList V.! ind) + 100)* foundAttribute (V.find ((== att) . fst) $ identifications ing)) ingr (V.generate (length ingr) id)))attr) 
                (sum $ V.map durability ingr)
    where
        effList = effectiveList ingr
        foundAttribute :: Maybe (String,(Int,Int)) -> Int
        foundAttribute (Just (_,(_,x))) = x
        foundAttribute Nothing = 0

effectiveList :: V.Vector Ingredient -> V.Vector Int
effectiveList = addLists . V.map (\(pos, ingr) -> addLists $ V.map (effects pos) $ effectiveness ingr) . V.zip (V.replicate 6 0)
  where
    --addLists = V.map sum . transpose
    addLists = V.foldl (V.zipWith (+)) (V.replicate 6 0)

--TODO MEMORIZE COULD BE EFFICIENT
effects :: Int -> (String,Int) -> V.Vector Int
effects pos (_,0) = V.replicate 6 0 
effects pos ("above",perc) = V.map (\ind -> if (ind<pos) && even (ind-pos) then perc else 0 ) $ V.generate 6 id
effects pos ("under",perc) = V.map (\ind -> if (ind>pos) && even (ind-pos) then perc else 0 ) $ V.generate 6 id
effects pos ("right",perc) = V.map (\ind -> if even pos && odd ind then perc else 0 ) $ V.generate 6 id
effects pos ("left",perc) = V.map (\ind  -> if odd pos && even ind then perc else 0 ) $ V.generate 6 id
effects pos ("touching",perc) = V.map (\ind  -> 
    if (even pos && (ind-pos == -2 || ind-pos == 2 || ind-pos == 1)) 
        || (odd pos && (ind-pos == 2 || ind-pos == -2 || ind-pos == -1)) 
        then perc else 0 ) $ V.generate 6 id
effects pos ("notTouching",perc) = V.map (\ind  -> 
    if (even pos && (ind-pos /= -2 && ind-pos /= 2 && ind-pos /= 1)) 
        || (odd pos && (ind-pos /= 2 && ind-pos /= -2 && ind-pos /= -1)) 
        then perc else 0 ) $ V.generate 6 id
effects _ _ = error "error with reading the effectiveness"


testForSkyline :: V.Vector String -> MVar (V.Vector RecipeResult) -> RecipeResult -> IO ()
testForSkyline attributes skyline receipt = do

    --evaluate (force currentSkyline)
    modifyMVar_ skyline (\currentSkyline -> do
        let gotDominated = V.any ((True==) . dominating attributes receipt) currentSkyline --rezept wird von dominiert -> discard
        let filteredskylinePoints = V.filter (\sky -> not $ dominating attributes sky receipt) currentSkyline-- `using` evalList r0
        unless gotDominated (print receipt) 
        return $ if gotDominated then filteredskylinePoints else V.snoc filteredskylinePoints receipt)

    return ()

dominating :: V.Vector String -> RecipeResult -> RecipeResult -> Bool
dominating attributes first dominat = all (\att -> fi att dominat > fi att first) attributes
  where
    fi attr receipt = foundAttribute $ find ((== attr) . fst) $ stats receipt
    foundAttribute :: Maybe (String,Int) -> Int
    foundAttribute (Just (_,x)) = x
    foundAttribute Nothing = minBound :: Int