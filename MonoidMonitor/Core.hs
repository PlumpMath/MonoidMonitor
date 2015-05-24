module MonoidMonitor.Core (
  Timescale (..)
, StatVar (..)
, TimescaleKey (..)
, TimescaleValue (..)
, TransformRule (..)
, digestEvent
) where

import qualified Data.Map as M
import Data.Monoid ((<>))

-- Pueder ser cualquier escala, éstas serían las más comunes para agregar estadísticas
data Timescale
    = Minute { minuteYear :: Int, minuteMonth :: Int, minuteDay :: Int, minuteHour :: Int, minuteMinute :: Int }
    | Hour   {   hourYear :: Int,   hourMonth :: Int,   hourDay :: Int,   hourHour :: Int }
    | Day    {    dayYear :: Int,    dayMonth :: Int,    dayDay :: Int }
    | Month  {  monthYear :: Int,  monthMonth :: Int }
    | Year   {   yearYear :: Int }
      deriving (Eq, Ord)

-- Las estadísticas están formadas por valores interrelacionados que pueden ser almacenados o virtuales (intermedios)
data StatVar
    = StatVar { name    :: String
              , virtual :: Bool
              } deriving Show
instance Eq  StatVar where (StatVar a _) == (StatVar b _) = a == b
instance Ord StatVar where (StatVar a _) `compare` (StatVar b _) = a `compare` b
              
-- Ej. "número de páginas vistas durante el mes de junio de 2015"
data TimescaleKey
    = TimescaleKey { statVar   :: StatVar
                   , timescale :: Timescale
                   } deriving (Eq, Ord)

data TimescaleValue v
    = TimescaleValue { valueKey   :: TimescaleKey
                     , valueValue :: v }

-- Las reglas de transformación, saben convertir ciertos valores en otros que serán agregados
data TransformRule v
    = (:=>) { match     :: TimescaleKey -> Bool
            , propagate :: TimescaleValue v -> [TimescaleValue v] }

-- Actualiza una base de datos de estadísticas dado un evento de entrada y las reglas de transformación
digestEvent :: Monoid v => [TransformRule v] -> TimescaleValue v -> M.Map TimescaleKey v -> M.Map TimescaleKey v
digestEvent rs v database = foldr insertInto database propagatedValues
    where propagatedValues = concat [(propagate r) v | r <- rs, (match r) (valueKey v)]
          insertInto v'@(TimescaleValue k w) db = digestEvent rs v' db'
            where db' = if (virtual . statVar) k then db else M.insertWith (<>) k w db
