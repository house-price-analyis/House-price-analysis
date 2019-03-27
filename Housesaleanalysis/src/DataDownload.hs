module DataDownload
    ( getsalesdata
    ) where


import DataTypes
import DataParser
import DataDb
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


type URL = String

past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)


getsalesdata :: URL -> IO L.ByteString
getsalesdata cia = do
   request' <- parseRequest $ "http://bhaskarsaikia.in/home/" ++ cia ++ ".csv"
   manager <- newManager tlsManagerSettings
   let request = request'
   response <- httpLbs request manager
   return $ responseBody response
