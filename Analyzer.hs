{-# LANGUAGE OverloadedStrings #-} 

import Tour
import Db
import System.Environment

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Data.Text hiding (head, map)

data Solutions = Solutions {
  solutions :: [[Int]]
} deriving (Show)

instance Y.FromJSON Solutions where
  parseJSON (Y.Object v) = Solutions
    <$> (v .: "solutions")

main :: IO ()
main = do
  sols <- Y.decodeFileThrow "analyzer.yaml" :: IO Solutions
  graph <- getGraph
  
  let tours = map (makeTour graph) $ solutions sols
  print tours
