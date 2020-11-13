{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Data.Text hiding (head)
import Control.Monad.Parallel as Par (mapM)

import Db
import Annealer (initializeAnnealer, thresholdAccepting)
import Tour
import Data.Map (elems)
import Graph hiding (cities)

data Config = Config {
  cityIds :: [Int],
  seeds :: [Int],
  batchSize :: Int,
  acceptedRate :: Double,
  initialTemperature :: Double,
  minTemperature :: Double,
  coolingRate :: Double
} deriving (Show)

instance Y.FromJSON Config where
  parseJSON (Y.Object v) = Config 
    <$> (v .: "cityIds")
    <*> (v .: "seeds")
    <*> (v .: "batchSize")
    <*> (v .: "acceptedRate")
    <*> (v .: "initialTemperature")
    <*> (v .: "minTemperature")
    <*> (v .: "coolingRate")


eval :: CityGraph -> Tour -> Config -> Int -> [CityID]
eval graph tour res s = result
  where 
    annealer = 
      initializeAnnealer s (batchSize res) (acceptedRate res) tour (initialTemperature res) (minTemperature res) (coolingRate res)
    results = thresholdAccepting annealer
    result = elems $ cities $ head results


-- | Realiza el recocido simulado de un Tour con una semilla específica e imprime
-- los resultados
evalPrint :: CityGraph -> Tour -> Config -> Int -> IO ()
evalPrint graph tour res s = do
  let annealer = 
        initializeAnnealer s (batchSize res) (acceptedRate res) tour (initialTemperature res) (minTemperature res) (coolingRate res)
  let results = thresholdAccepting annealer
  let result = elems $ cities $ head results
  print result
  print $ tourCost $ head results
  putStrLn $ "seed: " ++ (show s)

main :: IO ()
main = do
  res <- Y.decodeFileThrow "config.yaml" :: IO Config
  print (res :: Config)
  g <- getGraph
  let tour = makeTour g (cityIds res)
  let seed = (seeds res)
  Par.mapM (evalPrint g tour res) seed
  return ()

