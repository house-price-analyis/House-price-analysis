module DataTypes where

import GHC.Int

data House = House {
   typeID :: Int64,
   street :: String,
   city :: String, 
   zip :: Double,
   state :: String,
   beds :: Double,
   baths :: Double,
   sq__ft :: Double,
   type1 :: String,
   sale_date :: String,
   price :: Double,
   latitude :: Double,
   longitude :: Double
} deriving (Eq, Show)
