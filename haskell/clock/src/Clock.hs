module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock {hour :: Int, minute :: Int} deriving (Eq, Show)

instance Num Clock where
    a + b = fromHourMin (clockHour a + clockHour b) (clockMin a + clockMin b)
    a - b = fromHourMin (clockHour a - clockHour b) (clockMin a - clockMin b)
    a * b = undefined
    negate x = fromHourMin (negate $ clockHour x) (negate $ clockMin x)
    fromInteger x = fromHourMin (fromInteger (x `div` 60)) (fromInteger (x `mod` 60))
    abs x = undefined
    signum x = undefined

clockHour :: Clock -> Int
clockHour = hour

clockMin :: Clock -> Int
clockMin = minute

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour + min `div` 60) `mod` 24) (min `mod` 60)

toNumber :: Clock -> Int
toNumber clock = 60 * clockHour clock + clockMin clock

toString :: Clock -> String
toString clock = padString (clockHour clock) ++ ":" ++ padString (clockMin clock)

padString :: Int -> String
padString x
    | x >= 10 = show x
    | otherwise = "0" ++ show x
