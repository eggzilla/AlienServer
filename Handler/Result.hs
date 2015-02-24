{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Result where

import Import
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy.Char8 as L
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
import Data.Csv
import Data.Char
import qualified Data.Vector as V
import Data.Either.Unwrap
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
    tempDirPresent <- liftIO (doesDirectoryExist temporaryDirectoryPath)         
    started <- liftIO (doesFileExist (temporaryDirectoryPath ++ "0.log"))
    done <- liftIO (doesFileExist (temporaryDirectoryPath ++ "done"))  
    let unfinished = not done
    existentIterationLogs <- liftIO (filterM (\x -> doesDirectoryExist (temporaryDirectoryPath ++ (show x))) [0,1,2,3,4,5,6,7,8,9,10])
    iterationLogs <- liftIO (mapM (retrieveIterationLog temporaryDirectoryPath tempDirectoryURL) existentIterationLogs)
    resultInsert <- liftIO (retrieveResultCsv done temporaryDirectoryPath tempDirectoryURL approot)
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

retrieveResultCsv :: Bool -> String -> String -> Text -> IO String
retrieveResultCsv done temporaryDirectoryPath tempDirectoryURL approotURL = do
  if done
     then do
       let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord ';')
         }
       let alienCSVPath = temporaryDirectoryPath ++ "result.csv"
       inputCSV <- L.readFile alienCSVPath
       let decodedCsvOutput = V.toList (fromRight (decodeWith myOptions HasHeader (inputCSV) :: Either String (V.Vector (String,String,String))))
       let resultFamilyMemberTable = constructTaxonomyRecordsHtmlTable decodedCsvOutput
       let resultHeadline = "<h2>Results:</h2>"
       fastaPresent <- doesFileExist (temporaryDirectoryPath ++ "result.fa")
       stockholmPresent <- doesFileExist (temporaryDirectoryPath ++ "result.stockholm")
       cmPresent <- doesFileExist (temporaryDirectoryPath ++ "result.cm")
       let falink = fileStatusMessage fastaPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.fa\">Result Fasta</a>")
       let alnlink = fileStatusMessage stockholmPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.stockholm\">Result Alignment</a>")
       let cmlink = fileStatusMessage cmPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.cm\">Result CM</a>")
       let resultFilesTable = "<table><tr><td>" ++ falink ++ "</td><td>" ++ alnlink ++ "</td><td>" ++ cmlink ++ "</td></tr></table><br>"
       let cmcwsSendToField = "<img src=\"" ++ (DT.unpack approotURL) ++ "/static/images/cmcws_button.png\">"
       return (resultHeadline ++ resultFilesTable ++ resultFamilyMemberTable ++ cmcwsSendToField)
     else do
       return ""

retrieveIterationLog :: String -> String -> Int -> IO String
retrieveIterationLog temporaryDirectoryPath tempDirectoryURL counter = do
  let logPath = temporaryDirectoryPath ++ (show counter) ++ ".log"
  let iterationDirectoryPath = temporaryDirectoryPath ++ show counter ++ "/"
  iterationLog <- readFile logPath 
  stockholmPresent <- doesFileExist (iterationDirectoryPath ++ "model.stockholm")
  cmPresent <- doesFileExist (iterationDirectoryPath ++ "model.cm")
  let alnlink = fileStatusMessage stockholmPresent ("<a href=\"" ++ tempDirectoryURL ++ show counter ++ "/" ++ "model.stockholm" ++ "\">stockholm-format</a>")
  let cmlink = fileStatusMessage cmPresent ("<a href=\"" ++ tempDirectoryURL ++ show counter ++ "/" ++ "model.cm" ++ "\">covariance-model</a>")
  let logfields = splitOn "," iterationLog
  status <- retrieveIterationStatus iterationDirectoryPath
  let iterationLine = "<tr><td>" ++ logfields !! 0 ++ "</td><td><a href=\"http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=" ++ logfields !! 1  ++ "\">" ++ logfields !! 1 ++ "</a></td><td>" ++ (truncateThresholdField (logfields !! 2)) ++ "</td><td>" ++ logfields !! 3 ++ "</td><td>" ++ alnlink ++ "</td><td>" ++ cmlink ++ "</td><td>" ++ status ++ "</td></tr>"
  return iterationLine

fileStatusMessage :: Bool -> String -> String
fileStatusMessage filePresent message 
  | filePresent = message
  | otherwise = "loading"

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
  doneStatus <- doesFileExist (iterationDirectory  ++ "done")
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

constructTaxonomyRecordsHtmlTable :: [(String,String,String)] -> String
constructTaxonomyRecordsHtmlTable csv = recordtable
  where recordentries = concatMap (\(taxid,iteration,header) -> "<tr><td><a href=\"http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=" ++ taxid  ++ "\">" ++ taxid ++ "</a></td><td>" ++ iteration  ++ "</td><td>" ++ header ++ "</td></tr>") csv
        tableheader = "<h3>Included Sequences</h3><tr><th>Taxonomy Id</th><th>Included in Iteration</th><th>Entry Header</th></tr>"
        recordtable = "<table>" ++ tableheader ++ recordentries ++ "</table><br>"

truncateThresholdField :: String -> String
truncateThresholdField thresholdField
  | thresholdField == "not set" = "not set"
  | otherwise = (take 5 thresholdField)
