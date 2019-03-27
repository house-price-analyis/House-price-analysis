module DataParser 
    ( convert
    , parse
    )where

import GHC.Int
import DataTypes
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as B
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Conduit (runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Control.Exception as E
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString as D

breakLine :: String -> String -> [String] -> [String]
breakLine [] y ys = (reverse y):ys
breakLine (',':xs) y ys = breakLine xs "" ((reverse y):ys)
breakLine (c:xs) y ys = breakLine xs (c:y) ys



convert :: Int64 -> String -> House
convert cia_id h = House cia_id street city (read zip) state (read beds) (read baths) (read sq__ft) type1 sale_date (read p) (read latitude) (read l) 
   where [street,city,zip,state,beds,baths,sq__ft,type1,sale_date,p,latitude,l] = reverse $ breakLine h "" []

liner :: String -> [String]
liner "" = []
liner h = let (l, h') = break (== '\n') h
          in  l : case h' of
                       []      -> []
                       (_:h'') -> liner h''


parse :: Int64 -> String -> [House]
parse cia_id h = map (convert cia_id) (tail $ liner h)


