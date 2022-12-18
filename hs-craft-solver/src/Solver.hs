{-# LANGUAGE BangPatterns, DeriveGeneric, ParallelListComp  #-}
module Solver
    ( solve,
    effects
    ) where

import Ingredients
import Data.IORef.Strict
import System.IO.Strict.Internals
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
import qualified Data.Vector.Mutable as VM
import Control.Parallel.Strategies
import Control.Parallel
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

    --let tests = filter ((`elem` ["Elephelk Trunk","Green Foot","Major's Badge","Accursed Effigy"]) . name) allIngredients
    --let test2 = [tests!!2,tests!!0,tests!!3,tests!!1,tests!!3,tests!!1]
    --mapM_ print test2
    --print $ effectiveList $ V.fromList test2
    --mapM_ (\(ing,pos) -> print $ map (effects pos) (V.toList $ effectiveness ing)) $ zip test2 [0..]
    --print $ foldl (zipWith (+)) [0,0,0,0,0,0] $ concatMap (\(ing,pos) -> map (V.toList . effects pos) (V.toList $ effectiveness ing)) $ zip test2 [0..]


    --effects :: Int -> (String,Int) -> V.Vector Int

    --Alle Rezepte mit ohne effectivness items müssen performanter klappen als zu bruteforcen (heuristik? search algorithms?)
    
    --Alle Rezepte mit effectivness müssen grebruteforced werden 
    -- Liste mit  5er Permutation mit jeweils allen anderen items kombiniert der liste der effectiveness items
    -- multi threaded
    -- mpi support ? 
    skyline <- newMVar V.empty
    let allIngrd = V.fromList $ nub $ concat [effectivenessIngredients,durabilityIngredients,statIngredients] -- adding skill req items?
    let multiChunks = map V.fromList $ chunksOf (div (length allIngrd) 16) $ V.toList allIngrd --how many threads?
    putStrLn $ "Threads started: " ++ show (length multiChunks)
    results <- mapConcurrently (solvePart skyline (V.fromList attributes) mindurability allIngrd) multiChunks

    print "wait done"
    listReceips <- readMVar skyline
    print listReceips
    return ()

solvePart :: MVar (V.Vector RecipeResult) -> V.Vector String -> Int -> V.Vector Ingredient -> V.Vector Ingredient -> IO (IORef [RecipeResult])
solvePart skyline attributes mindurability allIng partialIng = do
    localSkyline <- run $ newIORef [] -- excuse me, change it later
    V.forM_ partialIng 
            (\p -> V.forM_ allIng 
                (\a1 -> V.forM_ allIng 
                    (\a2 -> V.forM_ allIng 
                        (\a3 -> V.forM_ allIng 
                            (\a4 -> V.forM_ allIng 
                                (\a5 -> testForSkyline attributes localSkyline $  createReceipe mindurability attributes (V.fromList[p,a1,a2,a3,a4,a5])))))))
    return localSkyline
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


-- print $ foldl (zipWith (+)) [0,0,0,0,0,0] $ concatMap (\(ing,pos) -> map (V.toList . effects pos) (V.toList $ effectiveness ing)) $ zip test2 [0..]
effectiveList :: V.Vector Ingredient -> V.Vector Int
effectiveList ingrednients
  | someEffective ingrednients = V.fromList $ addLists $ traceShowId $ concatMap (\(ing,pos) -> map (V.toList . effects pos) (V.toList $ effectiveness ing)) filteredEffectivnessList
  | otherwise = V.replicate 6 0
  where
    filteredEffectivnessList = filter (isEffectniss . fst) $ zip (V.toList ingrednients) [0..] 
    someEffective = V.or . V.map isEffectniss
    addLists = foldl (zipWith (+)) [0,0,0,0,0,0]

--TODO MEMORIZE COULD BE EFFICIENT
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


testForSkyline :: V.Vector String -> IORef [RecipeResult] -> Maybe RecipeResult -> IO ()
testForSkyline _ _ Nothing = return ()
testForSkyline attributes skyline (Just receipt) = do
    currentSkyline <- run $ readIORef skyline
    let gotDominated = any ((True==) . dominating attributes receipt) currentSkyline --rezept wird von dominiert -> discard
    let filteredskylinePoints = if not gotDominated then filter (\sky -> not $ dominating attributes sky receipt) currentSkyline else currentSkyline
    let newSkyline = if gotDominated then filteredskylinePoints else receipt:filteredskylinePoints 
    --unless gotDominated (print receipt) 
    --unless gotDominated (print newSkyline) 
    run $ writeIORef skyline newSkyline
    return ()

dominating :: V.Vector String -> RecipeResult -> RecipeResult -> Bool
dominating attributes first dominat = all (\att -> fi att dominat >= fi att first) attributes -- && any (\att -> fi att dominat > fi att first) attributes
  where
    fi attr receipt = foundAttribute $ find ((== attr) . fst) $ stats receipt
    foundAttribute :: Maybe (String,Int) -> Int
    foundAttribute (Just (_,x)) = x
    foundAttribute Nothing = error "Receipt with wrong attributes"