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
import System.Process
import System.Exit
import Bio.RNAzParser
import Text.ParserCombinators.Parsec 
import Yesod.Form.Bootstrap3    
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getResultR :: Handler Html
getResultR = do
    result <- getRequest
    let params = reqGetParams result
    let sessionIdjs = snd (DL.head params)
    let sessionId = DT.unpack sessionIdjs
    approot  <- fmap extraApproot getExtra
    taxDumpDirectoryPath <- fmap extraTaxDumpPath getExtra                
    outputPath <- fmap extraTempdir getExtra
    let temporaryDirectoryPath = DT.unpack (outputPath) ++ sessionId ++ "/"
    let tempDirectoryRootURL = "http://nibiru.tbi.univie.ac.at/rnalien_tmp/rnalien/"
    let tempDirectoryURL = tempDirectoryRootURL ++ sessionId ++ "/"
    let tempDirectoryURLjs = DT.pack ("../rnalien_tmp/rnalien/" ++ sessionId ++ "/")
    tempDirPresent <- liftIO (doesDirectoryExist temporaryDirectoryPath)         
    started <- liftIO (doesFileExist (temporaryDirectoryPath ++ "log/0.log"))
    done <- liftIO (doesFileExist (temporaryDirectoryPath ++ "done"))  
    let unfinished = not done
    existentIterationLogs <- liftIO (filterM (\x -> doesDirectoryExist (temporaryDirectoryPath ++ (show x))) [0..35])
    iterationLogs <- liftIO (mapM (retrieveIterationLog temporaryDirectoryPath tempDirectoryURL) existentIterationLogs)
    resultInsert <- liftIO (retrieveResultCsv done temporaryDirectoryPath tempDirectoryURL approot)
    if started
       then do
         let iterationInsert = DT.pack (concat iterationLogs)
         --liftIO (makeArchive done temporaryDirectoryPath)
         --liftIO (makeTaxonomicOverview done temporaryDirectoryPath taxDumpDirectoryPath)
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

--makeArchive :: Bool -> String -> IO ()
--makeArchive done temporaryDirectoryPath = do
--  if done
--     then do
--       archivePresent <- doesFileExist (temporaryDirectoryPath ++ "result.zip")
--       if archivePresent
--          then do
--            return ()
--          else do
--            _ <- system ("zip -9 -r " ++  temporaryDirectoryPath ++ "result.zip " ++ temporaryDirectoryPath)
--            return ()
--     else do
--       return ()
  
--makeTaxonomicOverview :: Bool -> String -> Text -> IO ()
--makeTaxonomicOverview done temporaryDirectoryPath taxDumpDirectoryPath = do
--  let taxOverviewDotFilePath = temporaryDirectoryPath ++ "taxonomy.dot"
--  let taxOverviewSvgFilePath = temporaryDirectoryPath ++ "taxonomy.svg"
--  let alienResultCsvFilePath = temporaryDirectoryPath ++ "result.csv"
--  let home = "/mnt/storage/home/egg"
--  if done
--     then do
--       overviewPresent <- doesFileExist taxOverviewSvgFilePath
--       if overviewPresent
--          then do
--            return ()
--          else do
--            _ <- system ("Ids2Tree -l 4 -i " ++ (DT.unpack taxDumpDirectoryPath) ++ " -o " ++ taxOverviewDotFilePath ++ " -r " ++ alienResultCsvFilePath) 
--           _ <- system ("dot -Tsvg " ++ taxOverviewDotFilePath ++ " -o " ++ taxOverviewSvgFilePath)
--            return ()
--     else do
--       return ()
              
retrieveResultCsv :: Bool -> String -> String -> Text -> IO String
retrieveResultCsv done temporaryDirectoryPath tempDirectoryURL approotURL = do
  if done
     then do
       let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord ';')
         }
       let alienCSVPath = temporaryDirectoryPath ++ "result.csv"
       let taxonomySvgPath = tempDirectoryURL ++ "taxonomy.svg"
       inputCSV <- L.readFile alienCSVPath
       let decodedCsvOutput = V.toList (fromRight (decodeWith myOptions HasHeader (inputCSV) :: Either String (V.Vector (String,String,String))))
       let resultFamilyMemberTable = constructTaxonomyRecordsHtmlTable decodedCsvOutput
       let resultHeadline = "<h2>Results:</h2>"
       logPresent <- doesFileExist (temporaryDirectoryPath ++ "Log")
       fastaPresent <- doesFileExist (temporaryDirectoryPath ++ "result.fa")
       stockholmPresent <- doesFileExist (temporaryDirectoryPath ++ "result.stockholm")
       cmPresent <- doesFileExist (temporaryDirectoryPath ++ "result.cm")
       rnazPresent <- doesFileExist (temporaryDirectoryPath ++ "result.rnaz")
       cmstatPresent <- doesFileExist (temporaryDirectoryPath ++ "result.cmstat")
       archivePresent <- doesFileExist (temporaryDirectoryPath ++ "result.zip")
       let loglink = fileStatusMessage logPresent ("<a href=\"" ++ tempDirectoryURL ++ "Log\">Log</a>")
       let falink = fileStatusMessage fastaPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.fa\">Fasta</a>")
       let alnlink = fileStatusMessage stockholmPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.stockholm\">Stockholm Alignment</a>")
       let cmlink = fileStatusMessage cmPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.cm\">Covariance Model</a>")
       let rnazlink = fileStatusMessage rnazPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.rnaz\">Rnaz Output</a>")
       let cmstatlink = fileStatusMessage cmstatPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.cmstat\">cmstat Output</a>")
       let archivelink = fileStatusMessage archivePresent ("<a href=\"" ++ tempDirectoryURL ++ "result.zip\">Zip Archive</a>")
       let resultFilesTable = "<table><tr><td>" ++ loglink ++ "</td><td>" ++ falink ++ "</td><td>" ++ alnlink ++ "</td><td>" ++ cmlink ++ "</td><td>" ++ rnazlink ++ "</td><td>" ++ cmstatlink ++ "</td><td>" ++ archivelink ++ "</td></tr></table><br>"
       evaluationResults <- constructEvaluationResults (length decodedCsvOutput) (temporaryDirectoryPath ++ "result.rnaz") (temporaryDirectoryPath ++ "result.cmstat")
       --let taxonomyOverview = "<table><tr><td>Taxonomic overview of alien hits</td></tr><tr><td><img src=\"" ++ taxonomySvgPath ++ "\" alt=\"loading\"></td></tr></table><br>"
       let taxonomyOverview = "<h3>Taxonomy overview:</h3><br>" ++ "<div id=\"tree-container\" style=\"width: 500px; height: 500px\" ></div>"
       let cmcwsSendToField = "<img src=\"" ++ (DT.unpack approotURL) ++ "/static/images/cmcws_button.png\">"
       return (resultHeadline ++ resultFilesTable ++ evaluationResults  ++ taxonomyOverview  ++ resultFamilyMemberTable ++ cmcwsSendToField)
     else do
       return ""

retrieveIterationLog :: String -> String -> Int -> IO String
retrieveIterationLog temporaryDirectoryPath tempDirectoryURL counter = do
  let logPath = temporaryDirectoryPath ++ "log/" ++ (show counter) ++ ".log"
  let iterationDirectoryPath = temporaryDirectoryPath ++ show counter ++ "/"
  iterationLog <- readFile logPath 
  stockholmPresent <- doesFileExist (iterationDirectoryPath ++ "model.stockholm")
  cmPresent <- doesFileExist (iterationDirectoryPath ++ "model.cm")
  let alnlink = fileStatusMessage stockholmPresent ("<a href=\"" ++ tempDirectoryURL ++ show counter ++ "/" ++ "model.stockholm" ++ "\">stockholm-format</a>")
  let cmlink = fileStatusMessage cmPresent ("<a href=\"" ++ tempDirectoryURL ++ show counter ++ "/" ++ "model.cm" ++ "\">covariance-model</a>")
  let logfields = splitOn "," iterationLog
  status <- retrieveIterationStatus iterationDirectoryPath
  let iterationLine = "<tr><td>" ++ logfields !! 0 ++ "</td><td><a href=\"http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=" ++ logfields !! 1  ++ "\">" ++ logfields !! 1 ++ "</a></td><td>" ++ logfields !! 2 ++ "</td><td>" ++ alnlink ++ "</td><td>" ++ cmlink ++ "</td><td>" ++ status ++ "</td></tr>"
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


constructEvaluationResults :: Int -> String -> String -> IO String
constructEvaluationResults entryNumber rnazPath cmStatPath = do
  inputcmStat <- readCMstat cmStatPath
  let cmstatString = cmstatHtmlOutput inputcmStat
  if (entryNumber > 1)
    then do 
      inputRNAz <- readRNAz rnazPath 
      let rnaZString = rnaZHtmlOutput inputRNAz
      return ("<h3>Evaluation Results:</h3><table><tr><td colspan=\"2\">CMstat statistics for result.cm</td></tr>" ++ cmstatString ++ "</table><table style=\"display: inline-block\"><tr><td colspan=\"2\">RNAz statistics for result alignment:</td></tr>" ++ rnaZString ++ "</table>")
    else do 
      return ("<h3>Evaluation Results:</h3><table><tr><td colspan=\"2\">CMstat statistics for result.cm</td></tr>" ++ cmstatString ++ "</table><table style=\"display: inline-block\"><tr><td colspan=\"2\">RNAlien could not find additional covariant sequences. Could not run RNAz statistics with a single sequence.</td></tr></table>")

cmstatHtmlOutput :: Either ParseError CMstat -> String 
cmstatHtmlOutput inputcmstat
  | isRight inputcmstat = cmstatString
  | otherwise = "<tr><td colspan=\"2\">" ++ show (fromLeft inputcmstat) ++ "</td></tr>"
    where cmStat = fromRight inputcmstat  
          cmstatString = "<tr><td>Sequence Number:</td><td>" ++ show (statSequenceNumber cmStat)++ "</td></tr>" ++ "<tr><td>Effective Sequences: " ++ show (statEffectiveSequences cmStat)++ "</td></tr>" ++ "<tr><td>Consensus length: " ++ show (statConsensusLength cmStat) ++ "</td></tr>" ++ "<tr><td>Expected maximum hit-length: " ++ show (statW cmStat) ++ "</td></tr>" ++ "<tr><td>Basepairs: " ++ show (statBasepairs cmStat)++ "</td></tr>" ++ "<tr><td>Bifurcations: " ++ show (statBifurcations cmStat) ++ "</td></tr>" ++ "<tr><td>Modeltype: " ++ show (statModel cmStat) ++ "</td></tr>" ++ "<tr><td>Relative Entropy CM: " ++ show (relativeEntropyCM cmStat) ++ "</td></tr>" ++ "<tr><td>Relative Entropy HMM: " ++ show (relativeEntropyHMM cmStat) ++ "</td></tr>"

rnaZHtmlOutput :: Either ParseError RNAz -> String 
rnaZHtmlOutput inputRNAz 
  | isRight inputRNAz = rnazString
  | otherwise = show (fromLeft inputRNAz)
    where rnaZ = fromRight inputRNAz
          rnazString = "<tr><td>Mean pairwise identity</td><td>" ++ show (meanPairwiseIdentity rnaZ) ++ "</td></tr><tr><td>Shannon entropy</td><td>" ++ show (shannonEntropy rnaZ) ++  "</td></tr><tr><td>GC content</td><td>" ++ show (gcContent rnaZ) ++ "</td></tr><tr><td>Mean single sequence minimum free energy</td><td>" ++ show (meanSingleSequenceMinimumFreeEnergy rnaZ) ++ "</td></tr><tr><td>Consensus minimum free energy</td><td>" ++ show (consensusMinimumFreeEnergy rnaZ) ++ "</td></tr><tr><td>Energy contribution</td><td>" ++ show (energyContribution rnaZ) ++ "</td></tr><tr><td>Covariance contribution</td><td>" ++ show (covarianceContribution rnaZ) ++ "</td></tr><tr><td>Combinations pair</td><td>" ++ show (combinationsPair rnaZ) ++ "</td></tr><tr><td>Mean z-score</td><td>" ++ show (meanZScore rnaZ) ++ "</td></tr><tr><td>Structure conservation index</td><td>" ++ show (structureConservationIndex rnaZ) ++ "</td></tr>Background model</td><td>" ++ backgroundModel rnaZ ++ "</td></tr><tr><td>Decision model</td><td>" ++ decisionModel rnaZ ++ "</td></tr><tr><td>SVM decision value</td><td>" ++ show (svmDecisionValue rnaZ) ++ "</td></tr><tr><td>SVM class propability</td><td>" ++ show (svmRNAClassProbability rnaZ) ++ "</td></tr><tr><td>Prediction</td><td>" ++ (prediction rnaZ)++ "</td></tr>"


-- | CMstat datastructure
data CMstat = CMstat
  { statIndex :: Int,
    statName :: String,
    statAccession :: String,
    statSequenceNumber :: Int,
    statEffectiveSequences :: Double,
    statConsensusLength :: Int,
    -- W The expected maximum length of a hit to the model.
    statW :: Int,
    statBasepairs :: Int,
    statBifurcations :: Int,
    statModel :: String,
    relativeEntropyCM :: Double,
    relativeEntropyHMM :: Double
  } deriving (Eq, Read) 

instance Show CMstat where
  show (CMstat _statIndex _statName _statAccession _statSequenceNumber _statEffectiveSequences _statConsensusLength _statW _statBasepairs _statBifurcations _statModel _relativeEntropyCM _relativeEntropyHMM) = a ++ b ++ c ++ d ++ e ++ f ++ g ++ h ++ i ++ j ++ k ++ l
    where a = "CMstat - covariance model statistics:\nIndex: " ++ show _statIndex ++ "\n" 
          b = "Name: " ++ show _statName ++ "\n" 
          c = "Accession: " ++ show _statAccession ++ "\n"
          d = "Sequence Number: " ++ show _statSequenceNumber ++ "\n"
          e = "Effective Sequences: " ++ show _statEffectiveSequences ++ "\n"
          f = "Consensus length: " ++ show _statConsensusLength ++ "\n"
          g = "Expected maximum hit-length: " ++ show _statW ++ "\n"
          h = "Basepairs: " ++ show _statBasepairs ++ "\n"
          i = "Bifurcations: " ++ show _statBifurcations ++ "\n"
          j = "Modeltype: " ++ show _statModel ++ "\n"
          k = "Relative Entropy CM: " ++ show _relativeEntropyCM ++ "\n"
          l = "Relative Entropy HMM: " ++ show _relativeEntropyHMM ++ "\n"

-- | parse from input filePath              
parseCMstat :: String -> Either ParseError CMstat
parseCMstat input = parse genParserCMstat "parseCMstat" input

-- | parse from input filePath                      
readCMstat :: String -> IO (Either ParseError CMstat)             
readCMstat filePath = do 
  parsedFile <- parseFromFile genParserCMstat filePath
  return parsedFile 
                      
genParserCMstat :: GenParser Char st CMstat
genParserCMstat = do
  string "# cmstat :: display summary statistics for CMs"
  newline
  string "# INFERNAL "
  many1 (noneOf "\n")
  newline       
  string "# Copyright (C) 201"
  many1 (noneOf "\n")
  newline       
  string "# Freely distributed under the GNU General Public License (GPLv3)."
  newline       
  string "# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
  newline
  char '#'
  many1 (char ' ')
  string "rel entropy"
  newline
  char '#'
  many1 (char ' ')
  many1 (char '-')
  newline
  char '#'
  many1 space 
  string "idx"
  many1 space        
  string "name"
  many1 space 
  string "accession"
  many1 space 
  string "nseq"
  many1 space  
  string "eff_nseq"
  many1 space 
  string "clen"
  many1 space 
  string "W"
  many1 space 
  string "bps"
  many1 space 
  string "bifs"
  many1 space 
  string "model"
  many1 space 
  string "cm"
  many1 space
  string "hmm"
  newline
  string "#"
  many1 (try (oneOf " -"))
  newline
  many1 space     
  _statIndex <- many1 digit
  many1 space
  _statName <- many1 letter
  many1 space                  
  _statAccession <- many1 (noneOf " ")
  many1 space             
  _statSequenceNumber <- many1 digit
  many1 space   
  _statEffectiveSequences <- many1 (oneOf "0123456789.e-")
  many1 space
  _statConsensusLength <- many digit
  many1 space                
  _statW <- many1 digit
  many1 space
  _statBasepaires <- many1 digit
  many1 space            
  _statBifurcations <- many1 digit
  many1 space              
  _statModel <- many1 letter
  many1 space          
  _relativeEntropyCM <- many1 (oneOf "0123456789.e-")
  many1 space                   
  _relativeEntropyHMM <- many1 (oneOf "0123456789.e-")
  newline
  char '#'
  newline
  eof  
  return $ CMstat (readInt _statIndex) _statName _statAccession (readInt _statSequenceNumber) (readDouble _statEffectiveSequences) (readInt _statConsensusLength) (readInt _statW) (readInt _statBasepaires) (readInt _statBifurcations) _statModel (readDouble _relativeEntropyCM) (readDouble _relativeEntropyHMM)

readDouble :: String -> Double
readDouble = read

readInt :: String -> Int
readInt = read

