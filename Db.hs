{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Db (getGraph) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HashMap

import Graph 

instance FromRow CityConnection where 
  fromRow = CityConnection <$> field <*> field <*> field

instance FromRow City where
  fromRow = City <$> field <*> field <*> field <*> field <*> field <*> field

-- | Extrae la información de la gráfica desde la base de datos
-- y construye la gráfica.
getGraph :: IO (CityGraph)
getGraph = do
  conn <- open "cities.db"
  connections <- query_ conn $ Query "Select * from Connections" :: IO [CityConnection]
  cities <- query_ conn $ Query "Select * from Cities" :: IO [City]
  close conn
  return $ makeGraph cities connections
