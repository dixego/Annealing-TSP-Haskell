{-# LANGUAGE NamedFieldPuns #-}

module Graph 
  (City(..), CityConnection(..), CityGraph(..), Graph, CityID, Distance, Latitude, Longitude, 
    makeGraph, graphDistance, naturalDistance, insertEdge, edge, (!?)) where

import Control.Monad (join)
import Control.Applicative ((<|>))
import Data.HashMap.Lazy hiding (foldl, map, (!?))
import Data.Map (Map)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import Data.Hashable (Hashable)

hLookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
hLookup = HashMap.lookup

-- Alias útiles para... la ~abstracción~
type CityID    = Int
type Distance  = Double
type Latitude  = Double
type Longitude = Double

data CityConnection = CityConnection CityID CityID Distance deriving (Show)
 
-- una gráfica es un diccionario de diccionarios, i.e. una matriz de adyacencia
type Graph v e = HashMap v (HashMap v e)

-- | Estructura que almacena la información de una ciudad.
data City = City {
  id         :: CityID,
  name       :: String,
  country    :: String,
  population :: Int,
  latitude   :: Latitude,
  longitude  :: Longitude
} deriving (Show)

-- | Estructura que representa la gráfica de ciudades como vértices y aristas.
-- Los vértices se guardan como un mapeo de `CityID` a `City` por eficiencia.
data CityGraph = CityGraph {
  cities :: Map CityID City,        -- Diccionario de ciudades con ids como llave
  edges  :: Graph CityID Distance   -- Matriz de adyacencia de ciudades
}

-- | Construye la gráfica a partir de una lista de ciudades y una lista de conexiones, tal
-- como aparecen en la base de datos.
makeGraph :: [City] -> [CityConnection] -> CityGraph
makeGraph cities connections = CityGraph { cities=cities', edges }
  where
    cities' = Map.fromList $ zip (map Graph.id cities) cities
    edges = foldl insertConnection HashMap.empty connections
    insertConnection m (CityConnection i j d) = insertEdge m (i, j) d

-- | Calcula la distancia natural entre dos ciudades utilizando la información geográfica
naturalDistance :: CityGraph -> CityID -> CityID -> Distance
naturalDistance CityGraph { cities } id1 id2 = radius * arc
  where
    (lat1, lon1) = 
          let City { latitude, longitude } = cities Map.! id1 in (rad latitude, rad longitude)
    (lat2, lon2) = 
          let City { latitude, longitude } = cities Map.! id2 in (rad latitude, rad longitude)
    rad g = (g * pi)/180.0
    radius = 6373000.0
    arc = 2.0 * atan2 (sqrt ang) (sqrt $ 1 - ang)
    ang = (sin $ (lat2 - lat1)/2)**2 + cos lat1 * cos lat2 * (sin $ (lon2 - lon1)/2)**2

-- | Extrae la distancia entre dos ciudades a partir de la gráfica
graphDistance :: CityGraph -> CityID -> CityID -> Maybe Distance
graphDistance CityGraph { edges } id1 id2 = edge edges id1 id2

-- | Extrae la arista entre dos vértices para una gráfica
edge :: (Eq v, Hashable v) => Graph v e -> v -> v -> Maybe e
edge graph v1 v2 = 
  join (hLookup <$> Just v1 <*> hLookup v2 graph) <|>
  join (hLookup <$> Just v2 <*> hLookup v1 graph) 

-- | `graph !? (v1, v2) == edge graph v1 v2`.
(!?) :: (Eq v, Hashable v) => Graph v e -> (v, v) -> Maybe e 
(!?) g (id1, id2) = edge g id1 id2

-- | Inserta una arista en la gráfica.
insertEdge :: (Eq v, Hashable v) => Graph v e -> (v, v) -> e -> Graph v e
insertEdge graph (v1, v2) e = graph''
  where graph' = HashMap.insertWith (insertNew v2 e) v1 (HashMap.singleton v2 e) graph
        graph'' = HashMap.insertWith (insertNew v1 e) v2 (HashMap.singleton v1 e) graph'
        insertNew k v _ old = HashMap.insert k v old
