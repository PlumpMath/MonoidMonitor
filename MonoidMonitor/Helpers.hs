{-# LANGUAGE RecordWildCards #-}
module MonoidMonitor.Helpers (
  TimescaleType (..)
, timescaleType
, digestEvents
, reduced
, startWith
, (&&.)
) where

import Data.List (isPrefixOf)
import MonoidMonitor.Core
import Text.Printf

digestEvents rs vs db = foldr (digestEvent rs) db vs

instance Show Timescale where
    show (Minute y m d h i) = printf "%04i-%02i-%02i %02i:%02i" y m d h i
    show (Hour   y m d h  ) = printf "%04i-%02i-%02i %02i"      y m d h  
    show (Day    y m d    ) = printf "%04i-%02i-%02i"           y m d    
    show (Month  y m      ) = printf "%04i-%02i"                y m      
    show (Year   y        ) = printf "%04i"                     y        

data TimescaleType = Minutes | Hours | Days | Months | Years deriving (Eq, Show)

timescaleType :: Timescale -> TimescaleType
timescaleType (Minute {..}) = Minutes
timescaleType (Hour   {..}) = Hours
timescaleType (Day    {..}) = Days
timescaleType (Month  {..}) = Months
timescaleType (Year   {..}) = Years

reduce :: Timescale -> Timescale
reduce (Minute y m d h i) = Hour  y m d h
reduce (Hour   y m d h  ) = Day   y m d  
reduce (Day    y m d    ) = Month y m    
reduce (Month  y m      ) = Year  y
reduce _                  = error "Cannot reduce!"

reduce' :: TimescaleKey -> TimescaleKey
reduce' (TimescaleKey statvar timescale) = TimescaleKey statvar (reduce timescale)

reduced :: TimescaleValue v -> TimescaleValue v
reduced (TimescaleValue key value) = TimescaleValue (reduce' key) value

startWith :: String -> TimescaleKey -> Bool
startWith prefix key = prefix `isPrefixOf` (name . statVar) key

(&&.) :: (TimescaleKey -> Bool) -> (TimescaleKey -> Bool) -> TimescaleKey -> Bool
(&&.) f g k = f k && g k
