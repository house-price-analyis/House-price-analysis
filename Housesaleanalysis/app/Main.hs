{-# LANGUAGE OverloadedStrings #-}
module Main where

import DataTypes
import DataDb
import DataParser
import DataDownload
import Data.ByteString.Lazy.Char8 (unpack)

processCia db cia_name = do
   print $ "-> Processing " ++ cia_name
   cia_id <- inserttypes db cia_name
   response <- getsalesdata cia_name
   let salesdata = parse cia_id (unpack response)
   batchInsert db cia_id salesdata

query db cias = do
   putStrLn $ "Which Type of House property you would like to query? " ++ show(cias)
   putStrLn $ "(type anything else to quit)"
   cia_name <- getLine
   if cia_name `elem` cias then
      do
         average <- salessum db cia_name
         putStrLn $ "Total sum sales of   " ++ cia_name ++ " property is " ++ (show average)
         query db cias
   else
      putStrLn "Thank you for using the app"

main :: IO ()
main = do
   db <- database
   initialiseDB db
   let cias = ["housemultifamily", "housecondo","houseresidential"]
   mapM_ (processCia db) cias
   query db cias
