{-# LANGUAGE NamedFieldPuns #-}

module Tour (Tour(..), swapTwoCities, makeTour, makeTourShuffled) where

import Data.List 
import Data.Maybe
import Data.Ord (comparing)
import Data.Map (Map, adjust, fromList, elems, size, (!))
import qualified Data.Map as Map (map, fold, lookup)
import Data.HashMap.Lazy (insert, insertWith)
import qualified Data.HashMap.Lazy as HashMap
import System.Random (RandomGen, uniformR)

import Annealer (Solution, Cost, neighbor, cost)
import Graph (CityID, CityGraph, Graph, Distance, 
              naturalDistance, graphDistance, insertEdge, edge, (!?))
import Shuffle (shuffle)

type CityTour = Map Int CityID

-- | Representa una solución de una instancia del problema de TSP
-- `graph` es la subgráfica extendida local a las ciudades de la instancia.
data Tour = Tour {
  cities     :: CityTour,                -- recorrido de la solución
  tourCost   :: Cost,                    -- costo de la solución
  graph      :: Graph CityID Distance,   -- la subgráfica local extendida
  normalizer :: Double                   -- normalizador de la subgráfica
}

instance Show Tour where
  show t = "Tour {\ncities = " ++ (show $ cities t) ++ "\ntourCost = " ++ (show $ tourCost t) ++ "\n}"

makeCityTour = fromList . zip [0..]

-- | Construye un `Tour` inicial a partir de una gráfica y una lista de ciudades.
makeTour :: CityGraph -> [CityID] -> Tour
makeTour graph ids = Tour { cities, tourCost, graph=graph', normalizer }
  where
    cities = makeCityTour ids
    cityEdges = 
      [((id1, id2), e) | id1 <- ids, id2 <- ids, id1 /= id2, let e = graphDistance graph id1 id2]

    sorted      = reverse $ sort $ catMaybes $ map snd cityEdges
    normalizer  = sum $ take (length ids - 1) $ sorted
    maxDistance = head sorted
    graph'      = foldl insertEdge' HashMap.empty cityEdges

    insertEdge' g ((id1, id2), Just d) = insertEdge g (id1, id2) d
    insertEdge' g ((id1, id2), Nothing) = 
      insertEdge g (id1, id2) $ (naturalDistance graph id1 id2) * maxDistance

    tourCost = computeCost graph' cities normalizer


-- | Construye un `Tour` con el orden de las ciudades permutado aleatoriamente.
makeTourShuffled :: RandomGen g => g -> CityGraph -> [CityID] -> (Tour, g)
makeTourShuffled gen graph tour = (tour', gen')
  where
    (shuffled, gen') = shuffle gen tour
    tour' = makeTour graph shuffled

-- genera dos índices para intercambiar
genTwoIndexes :: RandomGen g => (Int, Int) -> g -> ((Int, Int), g)
genTwoIndexes range gen = ((lo', hi'), gen')
  where
    (i, g') = uniformR range gen
    (j, gen') = until ((/= i) . fst) (uniformR range . snd) (i, g')
    (lo', hi') | i < j = (i, j) | otherwise = (j, i)

    
-- | Genera una solución vecina a un `Tour` intercambiando dos ciudades.
swapTwoCities :: RandomGen g => g -> Tour -> (g, Tour)
swapTwoCities g t@(Tour {cities, tourCost, graph, normalizer}) = 
  (g', t { cities = swapped, tourCost = newCost })
  where
    ((i, j), g') = genTwoIndexes (0, (size cities)-1) g
    cityI   = cities ! i
    cityJ   = cities ! j
    swapped = adjust (const cityJ) i $ adjust (const cityI) j $ cities
    newCost = adjustCost t swapped i j

-- | Calcula el costo de un `Tour` desde cero (en tiempo O(n log n))
computeCost :: Graph CityID Distance -> CityTour -> Double -> Distance
computeCost graph tour normalizer = 
  (sum $ catMaybes $ map (graph !?) $ pairs $ elems tour)/normalizer
  where pairs = zip <*> tail


-- | Calcula el costo de un `Tour` según qué ciudades fueron permutadas de otro `Tour`
adjustCost :: Tour -> CityTour -> CityID -> CityID -> Distance
adjustCost (Tour { cities, graph, tourCost, normalizer }) newTour i j = adjustedCost
  where
    originalCost = tourCost * normalizer

    findEdges tour k = pairs $ catMaybes $ map (`Map.lookup` tour) $ [k-1, k, k+1]
    pairs = zip <*> tail

    removed = (findEdges cities i) ++ (findEdges cities j)
    added = (findEdges newTour i) ++ (findEdges newTour j)

    costRemoved = sum $ catMaybes $ map (graph !?) removed
    costAdded = sum $ catMaybes $ map (graph !?) added

    adjustedCost = (originalCost + costAdded - costRemoved)/normalizer

    
instance Solution Tour where
  neighbor = swapTwoCities
  cost = tourCost
