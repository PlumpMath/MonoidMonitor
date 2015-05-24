import qualified Data.Map as M
import Data.List
import Data.Function
import MonoidMonitor
import Control.Arrow
import System.Random

data MyValues
    = SumValue { unSumValue :: !Double }
    | AvgValue { avgSum :: !Double, avgCount :: !Double }
    | Empty

gvalue :: MyValues -> Double
gvalue Empty          = 0
gvalue (SumValue x)   = x
gvalue (AvgValue s c) = s / c

toAvgSum :: String -> TimescaleValue MyValues -> TimescaleValue MyValues
toAvgSum name (TimescaleValue (TimescaleKey _ t) (SumValue x)) = TimescaleValue (TimescaleKey (StatVar name False) t) (AvgValue x 0)
toAvgSum _ _ = error "`toAvgSum` require `SumValue`"

toAvgCount :: String -> TimescaleValue MyValues -> TimescaleValue MyValues
toAvgCount name (TimescaleValue (TimescaleKey _ t) (SumValue x)) = TimescaleValue (TimescaleKey (StatVar name False) t) (AvgValue 0 x)
toAvgCount _ _ = error "`toAvgCount` require `SumValue`"
    
instance Show MyValues where
    show = show . gvalue

instance Monoid MyValues where
    mempty = Empty
    x `mappend` Empty = x
    Empty `mappend` x = x
    (SumValue a) `mappend` (SumValue b) = SumValue (a + b)
    (AvgValue s1 c1) `mappend` (AvgValue s2 c2) = AvgValue (s1 + s2) (c1 + c2)
    _ `mappend` _ = error "Cannot `mappend` between monoid families"

-- Los eventos son estadísticas virtuales porque no se almacenan nunca, son digeridos en un sólo paso
-- Como ejemplo, un evento de "página web procesada" genera múltiples valores para estadísticas
event_web_page :: Timescale -> String -> Double -> Double -> Double -> Double -> Double -> Double -> [TimescaleValue MyValues]
event_web_page timespan resourceName responseSize responseTime cpuUsage netUsage ioUsage memUsage =
    [ mkVal "web.page.view" 1                           -- ésta página ha sido visitada
    , mkVal "web.page.size" responseSize                -- con un tamaño de respuesta
    , mkVal "web.page.time" responseTime                -- con un tiempo total de espera en respuesta
    , mkVal "web.page.cpu" cpuUsage                     -- con un consumo total en procesamiento
    , mkVal "web.page.net" netUsage                     -- con un consumo total de red
    , mkVal "web.page.io" ioUsage                       -- con un consumo total de disco
    , mkVal "web.page.mem" memUsage                     -- con un consumo total de memoria
    ]
    where mkVal name value = TimescaleValue (TimescaleKey (StatVar (name ++ "." ++ resourceName) False) timespan)
                                            (SumValue value)

web_server_rules =
    [
     -- Hace disponible cualquier contador en cualquier tiempo t' > t
     ((Years /=) . timescaleType . timescale) &&. startWith "web.page." :=> (return . reduced)
     
     -- Podemos generar el tamaño medio
    , startWith "web.page.size" :=> (return . toAvgSum   "stat.page.avg.size")
    , startWith "web.page.view" :=> (return . toAvgCount "stat.page.avg.size")

     -- O la velocidad media en que se generan los datos (útil para saber si el cuello está en nosotros o en el hardware)
    , startWith "web.page.size" :=> (return . toAvgSum   "stat.page.avg.speedgen")
    , startWith "web.page.time" :=> (return . toAvgCount "stat.page.avg.speedgen")
    ]

generateRandomEvent = do
    let page = ["home", "profile", "search"]
    d <- randomRIO (1, 10)          -- pongamos durante 10 días
    h <- randomRIO (0, 23)
    i <- randomRIO (0, 59)
    p <- randomRIO (0, length page - 1)
    rs <- randomRIO (256, 64 * 1024)
    rt <- randomRIO (0.001, 1.5)
    cpu <- randomRIO (rt * 0.5, 2 * rt)
    net <- randomRIO (rs * 0.5, 2 * rs)
    io  <- randomRIO (rs * 0.5, 2 * rs)
    mem <- randomRIO (rs * 0.5, 2 * rs)
    return $ event_web_page (Minute 2015 5 d h i) (page!!p) rs rt cpu net io mem
    
generateRandomEvents n = concat <$> mapM (const generateRandomEvent) [1..n]

graphTitle :: TimescaleKey -> String
graphTitle (TimescaleKey (StatVar name _) t) = "==[ " ++ name ++ " | " ++ show (timescaleType t) ++ " ]========="

printAllGraphs :: M.Map TimescaleKey MyValues -> IO ()
printAllGraphs = mapM_ printGraph . groupBy ((==) `on` (graphTitle . fst)) . M.assocs

printGraph :: [(TimescaleKey, MyValues)] -> IO ()
printGraph vs@((t,_):_) = do
    putStrLn $ graphTitle t
    mapM_ (\(TimescaleKey _ t, v) -> putStrLn $ show t ++ ": " ++ show v) vs

runTest = do
    es <- generateRandomEvents 50
    let db = digestEvents web_server_rules es M.empty
    printAllGraphs db
