{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Result where

import Import
import Data.ByteString.Lazy (unpack)
import Yesod.Core.Handler
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import qualified Data.Text as DT
import qualified Data.List as DL (head)
import System.Directory   
import Settings.StaticFiles
import System.IO (readFile)
import Data.List.Split (splitOn)
import Control.Monad
import Yesod.Form.Bootstrap3    
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getResultR :: Handler Html
getResultR = do
    result <- getRequest
    let params = reqGetParams result
    let sessionIdjs = snd (DL.head params)
    let sessionId = DT.unpack sessionIdjs
    approot  <- fmap extraApproot getExtra
    outputPath <- fmap extraTempdir getExtra
    let temporaryDirectoryPath = DT.unpack (outputPath) ++ sessionId ++ "/"
    let tempDirectoryRootURL = "http://nibiru.tbi.univie.ac.at/rnalien_tmp/rnalien/"
    let tempDirectoryURL = tempDirectoryRootURL ++ sessionId ++ "/"
    --check if tempdir exists otherwise short circuit
    tempDirPresent <- liftIO (doesDirectoryExist temporaryDirectoryPath)         
    started <- liftIO (doesFileExist (temporaryDirectoryPath ++ "0" ++ ".log"))
    done <- liftIO (doesFileExist (temporaryDirectoryPath ++ "done"))  
    let unfinished = not done
    alienLog <- liftIO (readFile (temporaryDirectoryPath ++ "Log"))
    existentIterationLogs <- liftIO (filterM (\x -> doesFileExist (temporaryDirectoryPath ++ (show x) ++ ".log")) [0,1,2,3,4,5,6,7,8,9,10])
    iterationLogs <- liftIO (mapM (retrieveIterationLog temporaryDirectoryPath tempDirectoryURL) existentIterationLogs)
    let resultInsert = "<table><tr><td><a href=\"" ++ tempDirectoryURL ++ "result.cm\">Result CM</td></tr></table>"
    if started
       then do
         let iterationInsert = DT.pack (concat iterationLogs) 
         defaultLayout $ do
               aDomId <- newIdent
               setTitle "RNAlien Server - Results"
               $(widgetFile "result")
       else do
         let iterationInsert = DT.pack "<tr><td colspan=\"7\">Your job is queued</td></tr>"
         defaultLayout $ do
               aDomId <- newIdent
               setTitle "RNAlien Server - Results"
               $(widgetFile "result")

retrieveIterationLog :: String -> String -> Int -> IO String
retrieveIterationLog temporaryDirectoryPath tempDirectoryURL counter = do
      let logPath = temporaryDirectoryPath ++ (show counter) ++ ".log"
      iterationLog <- readFile logPath
      let alnlink = "<a href=\"" ++ tempDirectoryURL ++ show counter ++ "/" ++ "model.stockholm" ++ "\">stockholm-format</a>" 
      let cmlink = "<a href=\"" ++ tempDirectoryURL ++ show counter ++ "/" ++ "model.cm" ++ "\">covariance-model</a>" 
      let logfields = splitOn "," iterationLog
      status <- retrieveIterationStatus (temporaryDirectoryPath ++ show counter ++ "/")
      let iterationLine = "<tr><td>" ++ logfields !! 0 ++ "</td><td>" ++ logfields !! 1 ++ "</td><td>" ++ logfields !! 2 ++ "</td><td>" ++ logfields !! 3 ++ "</td><td>" ++ alnlink ++ "</td><td>" ++ cmlink ++ "</td><td>" ++ status ++ "</td></tr>"
      return iterationLine

retrieveIterationStatus :: String -> IO String
retrieveIterationStatus iterationDirectory = do
  searchStatus <- doesFileExist (iterationDirectory ++ "log")
  sequenceRetrievalStatus <- doesFileExist (iterationDirectory ++ "log/1_1blastOutput")  
  alignmentStatus <- doesFileExist (iterationDirectory ++ "1.fa")
  filteringStatusLocarna <- doesFileExist (iterationDirectory ++ "1.mlocarna")
  filteringStatusCMsearch <- doesFileExist (iterationDirectory ++ "1.cmsearch")
  let filteringStatus = or [filteringStatusLocarna,filteringStatusCMsearch]
  querySelectionStatus <- doesFileExist (iterationDirectory ++ "query.fa")
  calibrationStatus <- doesFileExist (iterationDirectory ++ "model.cm")
  doneStatus <- doesFileExist (iterationDirectory  ++ "model.cmcalibrate")
  let currentStatus = checkStatus searchStatus sequenceRetrievalStatus alignmentStatus filteringStatus calibrationStatus querySelectionStatus doneStatus
  return currentStatus

checkStatus :: Bool ->  Bool ->  Bool ->  Bool ->  Bool ->  Bool ->  Bool -> String
checkStatus searchStatus sequenceRetrievalStatus alignmentStatus filteringStatus calibrationStatus querySelectionStatus doneStatus
  | doneStatus = "<i class=\"green\">done</i>"
  | calibrationStatus = "<i class=\"blue\">model calibration</i>"
  | querySelectionStatus = "<i class=\"blue\">query selection</i>"                
  | filteringStatus = "<i class=\"orange\">candidate filtering</i>"
  | alignmentStatus = "<i class=\"orange\">candidate alignment</i>"
  | sequenceRetrievalStatus = "<i class=\"red\">sequence retrieval</i>"
  | searchStatus = "<i class=\"red\">sequence search</i>"
  | otherwise = "<i>loading</i>"
