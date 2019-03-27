module DataDb
    ( database
    , initialiseDB
    , inserttypes
    , getCiaID
    , salessum
    , batchInsert
    ) where


import DataTypes
import Database.SQLite3

import GHC.Int
import Data.Text (pack)
import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
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

database :: IO Database
database = open $ pack "salesdata.db"

closeDB = close

initialiseDB :: Database -> IO ()
initialiseDB db = do
   exec db $ pack "CREATE TABLE IF NOT EXISTS types (\
            \ID INTEGER PRIMARY KEY AUTOINCREMENT, \
            \type1 VARCHAR(40) NOT NULL)"
   exec db $ pack "CREATE TABLE IF NOT EXISTS salesdata (\
            \typeID INTEGER NOT NULL, \
            \street VARCHAR(40) NOT NULL, \
            \city VARCHAR(40) NOT NULL, \
            \zip FLOAT DEFAULT NULL, \
            \state VARCHAR(40) NOT NULL, \
            \beds FLOAT DEFAULT NULL, \
            \baths FLOAT DEFAULT NULL, \
            \sq__ft FLOAT DEFAULT NULL, \
            \type1 VARCHAR(40) NOT NULL, \
            \sale_date VARCHAR(40) DEFAULT NULL, \
            \price FLOAT DEFAULT NULL, \
            \latitude FLOAT DEFAULT NULL, \
            \longitude FLOAT DEFAULT NULL,\
            \FOREIGN KEY (typeID) REFERENCES types(ID))"

inserttypes :: Database -> String -> IO Int64
inserttypes db types = do
   stmt <- prepare db (pack "INSERT INTO types (type1) VALUES (:t)")
   bindNamed stmt [ (pack ":t", SQLText (pack types)) ]
   step stmt
   stmt <- prepare db (pack $ "SELECT ID FROM types WHERE type1='" ++ types ++ "'")
   result <- step stmt
   lastInsertRowId db

insertsalesdata :: Database -> Int64 -> House -> IO ()
insertsalesdata db i house = do
   let f (House i street city zip state beds baths sq__ft type1 sale_date p latitude l) = [ (pack ":i", SQLInteger i)
                                                                                          , (pack ":street", SQLText (pack street))
                                                                                          , (pack ":city", SQLText (pack city))
                                                                                          , (pack ":zip", SQLFloat zip)
                                                                                          , (pack ":state",SQLText (pack state))
                                                                                          , (pack ":beds", SQLFloat beds)
                                                                                          , (pack ":baths", SQLFloat baths)
                                                                                          , (pack ":sq__ft", SQLFloat sq__ft)
                                                                                          , (pack ":type1", SQLText (pack type1))
                                                                                          , (pack ":sale_date", SQLText (pack sale_date))
                                                                                          , (pack ":p", SQLFloat p)
                                                                                          , (pack ":latitude", SQLFloat latitude) 
                                                                                          , (pack ":l", SQLFloat l)
                                                                                          ]
   let args = f house
   stmt <- prepare db (pack "INSERT INTO salesdata VALUES (:i,:street,:city,:zip,:state,:beds,:baths,:sq__ft,:type1,:sale_date,:p,:latitude,:l)")
   bindNamed stmt (f house)
   result <- step stmt
   print result

getCiaID :: Database -> String -> IO Int64
getCiaID db cia_name = do
   stmt <- prepare db (pack $ "SELECT (ID) FROM types WHERE type1=:cia")
   bindNamed stmt [ (pack ":cia", SQLText (pack cia_name)) ]
   result <- step stmt       -- one statement step
   cia_db <- column stmt 0   -- read how returned
   let readID (SQLInteger n) = n
   let cia_id = readID cia_db
   return cia_id

salessum :: Database -> String -> IO Double
salessum db cia_name = do
   cia_id <- getCiaID db cia_name
   stmt <- prepare db (pack "SELECT (price) FROM salesdata WHERE typeID=:cia")
   bindNamed stmt [ (pack ":cia", SQLInteger cia_id) ]
   let
       isFloat (SQLFloat _) = True
       isFloat _ = False
   let getFloat (SQLFloat f) = f
   let readPrice ps = do
         result <- step stmt       
         p <- column stmt 0        
         if isFloat p then
            readPrice (p:ps)
         else
            return ps
   ps <- readPrice []
   let fs = map getFloat ps
   return $ (sum fs) 

batchInsert :: Database -> Int64 -> [House] -> IO ()
batchInsert db cia_id salesdata = do
   mapM_ (insertsalesdata db cia_id) salesdata

