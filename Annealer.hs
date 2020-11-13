{-# LANGUAGE NamedFieldPuns #-}
module Annealer where

import System.Random
import Data.List (find, iterate')
import Data.Maybe (fromMaybe, fromJust)

type Cost = Double

class Solution s where
  neighbor     :: RandomGen g => g -> s -> (g, s)
  cost         :: s -> Cost
  randomSolution :: RandomGen g => g -> s -> (g, s)
  randomSolution gen s = (iterate (uncurry neighbor) (gen, s)) !! 500 

type Temperature = Double
type Seed = Int
type BatchSize = Int
type AcceptedRate = Double
type CoolingRate = Double

-- | Estructura para guardar el estado del recocido
data Annealer s = Annealer {
  seed :: Seed,
  rng :: StdGen,
  batchSize :: BatchSize,
  acceptedRate :: AcceptedRate,
  initialSolution :: s,
  initialTemperature :: Temperature,
  minTemperature :: Temperature,
  coolingRate :: CoolingRate 
} deriving (Show)

-- | Determina si una solución es aceptada dada cierta temperatura y otra solución
accepted :: Solution s => Temperature -> s -> s -> Bool
accepted t s s' = cost s' < cost s + t

within :: Double -> Double -> Double -> Bool
within x y d = (abs (x - y)) < d

-- | Inicializa en estado del recocido con la configuración dada y aleatorizando la 
-- solución inicial
initializeAnnealer :: Solution s => Seed -> BatchSize -> AcceptedRate -> s 
                   -> Temperature -> Temperature -> CoolingRate -> Annealer s
initializeAnnealer seed bSize aRate sol iTemp mTemp cRate = 
  withInitialTemperature 
  $ Annealer {
    seed, rng, batchSize=bSize, acceptedRate=aRate, initialSolution=randomSol,
    initialTemperature=iTemp, minTemperature=mTemp, coolingRate=cRate}
      where (rng, randomSol) = randomSolution (mkStdGen seed) sol
    


-- | Toma un estado de recocidio inicializado y busca la temperatura inicial 
-- ideal con búsqueda binaria
withInitialTemperature :: Solution s => Annealer s -> Annealer s
withInitialTemperature an@(Annealer { rng, acceptedRate, initialTemperature, initialSolution }) 
  = an { rng = gen', initialTemperature = temp }
  where
    (currentRate, gen) = calculateAcceptedRate rng initialSolution initialTemperature

    (temp, gen')
      | within currentRate acceptedRate 0.01 = (initialTemperature, gen)
      | currentRate < acceptedRate = 
        let (hiTemp, g') = tempUp gen initialTemperature in 
            binarySearchTemp g' (hiTemp/2) hiTemp
      | otherwise = 
        let (loTemp, g') = tempDown gen initialTemperature in 
            binarySearchTemp g' loTemp (loTemp*2)

    tempUp gen temp
      | rate > acceptedRate = (temp, g)
      | otherwise = tempUp g (temp*2)
      where (rate, g) = calculateAcceptedRate gen initialSolution temp

    tempDown gen temp
      | rate < acceptedRate = (temp, g)
      | otherwise = tempDown g (temp/2)
      where (rate, g) = calculateAcceptedRate gen initialSolution temp

    binarySearchTemp gen lo hi
      | within hi lo 0.01 = (midPoint, gen)
      | within rate acceptedRate 0.01 = (midPoint, g)
      | rate < acceptedRate = binarySearchTemp g midPoint hi
      | otherwise = binarySearchTemp g lo midPoint
      where midPoint = (lo + hi)/2
            (rate, g) = calculateAcceptedRate gen initialSolution midPoint

-- | Algoritmo de recocido simulado por aceptación por umbrales. 
-- Toma un estado de recocido como configuración y devuelve una lista de
-- las mejores soluciones encontradas.
thresholdAccepting :: Solution s => Annealer s -> [s]
thresholdAccepting annealer@(Annealer { rng, batchSize, coolingRate, minTemperature, 
      initialTemperature, initialSolution }) 
  = step rng initialSolution [initialSolution] initialTemperature
  where
    step rng solution bests temp = 
      if temp < minTemperature 
         then bests
         else let 
              (rng', lastSolution, best) = tempStep 0 1e1000 rng solution (head bests) temp in
                  step rng' lastSolution (best:bests) (coolingRate * temp)

    tempStep p q rng sol best temp
      | p > q = (rng', last, best')
      | otherwise = tempStep p' p rng' last best' temp
      where 
        BatchResult{batchRng, finalSolution, bestSolution, avgCost} = 
          mkBatch rng sol batchSize temp
        (rng', last, best'', p') = (batchRng, finalSolution, bestSolution, avgCost)
        best' = if cost best <= cost best'' then best else best''
  

-- | Calcula el porcentaje de soluciones aceptadas generadas
-- a partir de una solución y una temperatura.
calculateAcceptedRate :: (RandomGen g, Solution s) => g -> s -> Temperature -> (Double, g)
calculateAcceptedRate gen solution temp = 
  ((fromIntegral $ sum $ map (fromEnum.(uncurry $ accepted temp).fst) $ proc)
    /(fromIntegral total), gen')
    where
      proc  = take total $ iterate step init
      gen'  = snd $ last $ proc 
      total = 500 
      init  = ((solution, solution), gen)
      step ((s, s'), gen) = let (gen', s'') = neighbor gen s' in ((s', s''), gen')


-- | Estado del cálculo de un lote.
data BatchState g s = BatchState g s s Cost Int
-- | Resultado del cálculo de un lote.
data BatchResult g s = BatchResult {
  batchRng :: g,
  finalSolution :: s,
  bestSolution :: s,
  avgCost :: Cost
} deriving (Show)

  
-- | Calcula un lote de soluciones aleatorias dada una solución inicial,
-- un tamaño de lote y una temperatura.
mkBatch :: (RandomGen g, Solution s) => g -> s -> BatchSize -> Temperature -> BatchResult g s
mkBatch g s batchSize temp =
  toBatchResult
  $ avgCost
  $ findWithDefault (accCountIs (==batchSize)) last
  $ take tryLimit
  $ iterate batchStep (BatchState g s s 0.0 0)
  where
    tryLimit = batchSize * 3
    accCountIs p (BatchState _ _ _ _ c) = p c
    avgCost (BatchState a b d c i) = BatchState a b d (c/(fromIntegral$batchSize)) i
    toBatchResult (BatchState a b c d _) = BatchResult a b c d
    applyIf p f | p = f | otherwise = id
    ($?) = applyIf
    findWithDefault p f l = 
      case find p l of
        Just a -> a
        Nothing -> f l


    batchStep (BatchState rng sol best totalCost accCount) =
      BatchState rng' sol' best' totalCost' accCount'
        where
          (rng', n)  = neighbor rng sol
          acc        = accepted temp sol n
          sol'       = acc $? (const n) $ sol
          best'      = (cost sol' < cost best) $? (const sol') $ best
          totalCost' = acc $? (+(cost sol')) $ totalCost
          accCount'  = acc $? (+1) $ accCount


