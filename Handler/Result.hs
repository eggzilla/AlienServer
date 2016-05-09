
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Result where

import Import
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as DT
import qualified Data.List as DL (head)
import System.Directory   
import System.IO (readFile)
import Data.List.Split (splitOn)
import Control.Monad
import Data.Csv
import Data.Char
import qualified Data.Vector as V
import Data.Either.Unwrap
import Bio.RNAzParser
import qualified Bio.RNAcodeParser as RC
import Text.ParserCombinators.Parsec 

getResultR :: Handler Html
getResultR = do
    result <- getRequest
    let params = reqGetParams result
    let sessionIdjs = snd (DL.head params)
    let sessionId = DT.unpack sessionIdjs
    currentApproot  <- fmap extraApproot getExtra
    --taxDumpDirectoryPath <- fmap extraTaxDumpPath getExtra                
    outputPath <- fmap extraTempdir getExtra
    let temporaryDirectoryPath = DT.unpack (outputPath) ++ sessionId ++ "/"
    let tempDirectoryRootURL = "http://nibiru.tbi.univie.ac.at/rnalien_tmp/rnalien/"
    let tempDirectoryURL = tempDirectoryRootURL ++ sessionId ++ "/"
    let tempDirectoryURLjs = DT.pack ("../rnalien_tmp/rnalien/" ++ sessionId ++ "/")
    let returnLink = DT.pack ((DT.unpack currentApproot) ++ "/result?sessionId=" ++ sessionId)
    --tempDirPresent <- liftIO (doesDirectoryExist temporaryDirectoryPath)         
    started <- liftIO (doesFileExist (temporaryDirectoryPath ++ "log/0.log"))
    done <- liftIO (doesFileExist (temporaryDirectoryPath ++ "taxonomy.json"))  
    let unfinished = not done
    existentIterationLogs <- liftIO (filterM (\x -> doesDirectoryExist (temporaryDirectoryPath ++ (show x))) [0..35])
    iterationLogs <- liftIO (mapM (retrieveIterationLog temporaryDirectoryPath tempDirectoryURL) existentIterationLogs)
    resultInsert <-liftIO (retrieveResultCsv done sessionId temporaryDirectoryPath tempDirectoryURL currentApproot)
    if started
       then do
         if done
           then do
             let iterationInsert = DT.pack (concat iterationLogs)
             --liftIO (makeArchive done temporaryDirectoryPath)
             --liftIO (makeTaxonomicOverview done temporaryDirectoryPath taxDumpDirectoryPath)
             defaultLayout $ do
               aDomId <- newIdent
               setTitle "RNAlien Server - Results"
               $(widgetFile "result")
           else do
             let iterationInsert = DT.pack (concat iterationLogs)
             defaultLayout $ do
               aDomId <- newIdent
               setTitle "RNAlien Server - Results"
               $(widgetFile "progress")
       else do
         let iterationInsert = DT.pack "<tr><td colspan=\"7\">Your job is queued</td></tr>"
         defaultLayout $ do
               aDomId <- newIdent
               setTitle "RNAlien Server - Results"
               $(widgetFile "progress")

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
              
retrieveResultCsv :: Bool -> String -> String -> String -> Text -> IO String
retrieveResultCsv done sessionId temporaryDirectoryPath tempDirectoryURL approotURL = do
  if done
     then do
       let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord ';')
         }
       let alienCSVPath = temporaryDirectoryPath ++ "result.csv"
       let rnazPath = temporaryDirectoryPath ++ "result.rnaz"
       let rnacodePath = temporaryDirectoryPath ++ "result.rnacode"
       let cmstatPath = temporaryDirectoryPath ++ "result.cmstat"
       let rnacentralPath = temporaryDirectoryPath ++ "result.rnacentral"
       inputCSV <- L.readFile alienCSVPath
       let decodedCsvOutput = V.toList (fromRight (decodeWith myOptions HasHeader (inputCSV) :: Either String (V.Vector (String,String,String))))
       let resultFamilyMemberTable = constructTaxonomyRecordsHtmlTable decodedCsvOutput
       let resultHeadline = "<h2>Results:</h2>"
       logPresent <- doesFileExist (temporaryDirectoryPath ++ "Log")
       fastaPresent <- doesFileExist (temporaryDirectoryPath ++ "result.fa")
       stockholmPresent <- doesFileExist (temporaryDirectoryPath ++ "result.stockholm")
       cmPresent <- doesFileExist (temporaryDirectoryPath ++ "result.cm")
       rnazPresent <- doesFileExist rnazPath
       rnacodePresent <- doesFileExist rnacodePath
       cmstatPresent <- doesFileExist cmstatPath
       rnacentralPresent <- doesFileExist rnacentralPath
       archivePresent <- doesFileExist (temporaryDirectoryPath ++ "result.zip")
       started <- readFile (temporaryDirectoryPath ++  "starttime")
       ended <- readFile (temporaryDirectoryPath ++  "endtime")
       let loglink = fileStatusMessage logPresent ("<a href=\"" ++ tempDirectoryURL ++ "Log\">Log</a>")
       let falink = fileStatusMessage fastaPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.fa\">Fasta</a>")
       let alnlink = fileStatusMessage stockholmPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.stockholm\">Stockholm Alignment</a>")
       let cmlink = fileStatusMessage cmPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.cm\">Covariance Model</a>")
       let rnazlink = fileStatusMessage rnazPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.rnaz\">RNAz Output</a>")
       let rnacodelink = fileStatusMessage rnacodePresent ("<a href=\"" ++ tempDirectoryURL ++ "result.rnacode\">RNAcode Output</a>")
       let cmstatlink = fileStatusMessage cmstatPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.cmstat\">cmstat Output</a>")
       let rnacentrallink = fileStatusMessage rnacentralPresent ("<a href=\"" ++ tempDirectoryURL ++ "result.rnacentral\">RNAcentral Output</a>")
       let archivelink = fileStatusMessage archivePresent ("<a href=\"" ++ tempDirectoryURL ++ "result.zip\">Zip Archive</a>")
       let resultFilesTable = "<h3>Summary</h3><br><table><tr><td>Job started:</td><td>" ++ started ++ "</td></tr><tr><td>Job ended:</td><td>" ++ ended ++ "</td></tr></table><br><table><tr><td>" ++ loglink ++ "</td><td>" ++ falink ++ "</td><td>" ++ alnlink ++ "</td><td>" ++ cmlink ++ "</td><td>" ++ rnazlink ++ "</td><td>" ++ rnacodelink ++ "</td><td>" ++ cmstatlink ++ "</td><td>" ++ rnacentrallink ++ "</td><td>" ++ archivelink ++ "</td></tr></table><br>"
       evaluationResults <- constructEvaluationResults (length decodedCsvOutput) temporaryDirectoryPath tempDirectoryURL sessionId approotURL 
       --let taxonomyOverview = "<h3>Taxonomy overview</h3><br>" ++ "<div id=\"tree-container\" style=\"width: 500px; height: 500px\" ></div><br>"
       --let cmcwsSendToField = "<a href=\"http://nibiru.tbi.univie.ac.at/cgi-bin/cmcws/cmcws.cgi\"><img src=\"" ++ (DT.unpack approotURL) ++ "/static/images/cmcws_button.png\"></a>"
       --let cmcwsSendToField = "<form id=\"submit-form\" enctype=\"multipart/form-data\" method=\"post\" action=\"http://nibiru.tbi.univie.ac.at/cgi-bin/cmcws/cmcws.cgi/cmcws.cgi\">" ++
       --                       "<input id=\"select_slice\" type=\"hidden\" name=\"page\" value=\"All\">" ++
       --                       "<input id=\"page\" type=\"hidden\" name=\"page\" value=\"1\">" ++
       --                       "<input id=\"mode\" type=\"hidden\" name=\"mode\" value=\"1\">" ++
       --                       "<input id=\"uploaded_file\" type=\"hidden\" name=\"uploaded_file\" value=\"" ++ sessionId ++ "\">" ++
       --                       "<input type=\"image\" src=\"" ++ (DT.unpack approotURL) ++ "/static/images/cmcws_button.png\" value=\"Compare\">"
       return (resultHeadline ++ resultFilesTable ++ evaluationResults ++ resultFamilyMemberTable)
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
  if (logfields !! 1) == "not set"
    then do
      let iterationLine = "<tr><td>" ++ logfields !! 0 ++ "</td><td>" ++ logfields !! 1 ++ "</td><td>" ++ logfields !! 2 ++ "</td><td>-</td><td>-</td><td>" ++ status ++ "</td></tr>"
      return iterationLine
    else do
      if (logfields !! 2) =="0"
        then do
          let iterationLine = "<tr><td>" ++ logfields !! 0 ++ "</td><td><a href=\"http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=" ++ logfields !! 1  ++ "\">" ++ logfields !! 1 ++ "</a></td><td>" ++ logfields !! 2 ++ "</td><td>-</td><td>-</td><td>" ++ status ++ "</td></tr>"
          return iterationLine
        else do
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
  where recordentries = concatMap (\(taxid,iteration,fastaheader) -> "<tr><td><a href=\"http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=" ++ taxid  ++ "\">" ++ taxid ++ "</a></td><td>" ++ iteration  ++ "</td><td>" ++ fastaheader ++ "</td></tr>") csv
        tableheader = "<h3>Included Sequences</h3><tr><th>Taxonomy Id</th><th>Included in Iteration</th><th>Entry Header</th></tr>"
        recordtable = "<table>" ++ tableheader ++ recordentries ++ "</table><br>"

truncateThresholdField :: String -> String
truncateThresholdField thresholdField
  | thresholdField == "not set" = "not set"
  | otherwise = (take 5 thresholdField)

constructEvaluationResults :: Int -> String -> String -> String -> Text -> IO String
constructEvaluationResults entryNumber temporaryDirectoryPath tempDirectoryURL sessionId approotURL= do
  let rnazPath = temporaryDirectoryPath ++ "result.rnaz"
  let rnaCodePath = temporaryDirectoryPath ++ "result.rnacode"
  let cmStatPath = temporaryDirectoryPath ++ "result.cmstat"
  let rnaCentralPath = temporaryDirectoryPath ++ "result.rnacentral"
  let aliRNAjpeg = tempDirectoryURL ++ "alirna.jpg"
  inputcmStat <- readCMstat cmStatPath
  let cmStatString = cmstatHtml inputcmStat
  inputRNAcentralCSV <- L.readFile rnaCentralPath
  let myOptions = defaultDecodeOptions {
         decDelimiter = fromIntegral (ord '\t')
         }
  let decodedRNAcentralCSV = decodeWith myOptions HasHeader (inputRNAcentralCSV) :: Either String (V.Vector (String,String,String))
  let rnaCentralString = rnaCentralHtml decodedRNAcentralCSV inputRNAcentralCSV 
  if (entryNumber > 1)
    then do 
      inputRNAz <- readRNAz rnazPath
      inputRNAcode <- RC.readRNAcodeTabular rnaCodePath
      let rnaZString = rnaZHtml inputRNAz
      let rnaCodeString = rnaCodeHtml inputRNAcode
      let taxonomyOverview = "<br><h3>Taxonomy overview</h3><br>" ++ "<div id=\"tree-container\" style=\"width: 500px; height: 500px\" ></div><br>"
      let cmcwsSendToField = "<br><form id=\"submit-form\" enctype=\"multipart/form-data\" method=\"post\" action=\"http://nibiru.tbi.univie.ac.at/cgi-bin/cmcws/cmcws.cgi/cmcws.cgi\">" ++
                             "<input id=\"select_slice\" type=\"hidden\" name=\"page\" value=\"All\">" ++
                             "<input id=\"page\" type=\"hidden\" name=\"page\" value=\"1\">" ++
                             "<input id=\"mode\" type=\"hidden\" name=\"mode\" value=\"1\">" ++
                             "<input id=\"uploaded_file\" type=\"hidden\" name=\"uploaded_file\" value=\"" ++ sessionId ++ "\">" ++
                             "<input type=\"image\" src=\"" ++ (DT.unpack approotURL) ++ "/static/images/cmcws_button.png\" value=\"Compare\"><br>"
      return ("<h3>Evaluation Results</h3><br><table><tr><td colspan=\"2\">RNAz statistics for result alignment:</td></tr>" ++ rnaZString ++ "</table><br><table><tr><td colspan=\"2\">RNAalifold consensus structure</td></tr><tr><td><img src=\"" ++ aliRNAjpeg ++ "\" alt=\"RNAalifold consensus structure\" style=\"width: 500px; height: 500px\"></td></tr></table><br><table><tr><td colspan=\"2\">CMstat statistics for result.cm</td></tr>" ++ cmStatString ++ "</table><br><table><tr><td colspan=\"2\">RNAcode statistics for result alignment:</td></tr>" ++ rnaCodeString ++ "</table><br><table><tr><td colspan=\"2\">RNAcentral entries for found sequences</td></tr>" ++ rnaCentralString ++ "</table>" ++ taxonomyOverview ++ cmcwsSendToField)
    else do
      let taxonomyOverview = "<br><h3>Taxonomy overview</h3><br>RNAlien could not find additional covariant sequences.<br>"
      return ("<h3>Evaluation Results</h3><br><table><tr><td colspan=\"2\">CMstat statistics for result covariance model</td></tr>" ++ cmStatString ++ "</table><br><table><tr><td colspan=\"2\">RNAlien could not find additional covariant sequences.<br> Could not run RNAz and RNAcode statistics with a single sequence.</td></tr></table>" ++  taxonomyOverview)

rnaCentralHtml :: Either String (V.Vector (String,String,String)) -> L.ByteString -> String
rnaCentralHtml inputRNAcentral inputRNAcentralCSV
  | isRight inputRNAcentral = rnaCentralString
  | otherwise = "<tr><td>" ++ L.unpack inputRNAcentralCSV ++ "</td></tr>"
    where rnaCentralHits = V.toList (fromRight inputRNAcentral)
          rnaCentralHeader ="<tr><td>RNAcentral id</td><td>MD5 sequence checksum</td><td>Length</td>"
          rnaCentralBody = concatMap rnaCentralEntryHtml rnaCentralHits
          rnaCentralString = rnaCentralHeader ++ rnaCentralBody
             
rnaCentralEntryHtml :: (String,String,String) -> String
rnaCentralEntryHtml (centralid,centralmd5,centrallength) = "<tr><td><a href=\"http://rnacentral.org/api/v1/rna/" ++ centralid ++ "\">" ++ centralid ++ "</a></td><td>" ++ centralmd5 ++ "</td><td>" ++ centrallength ++"</td></tr>"

cmstatHtml :: Either ParseError CMstat -> String 
cmstatHtml inputcmstat
  | isRight inputcmstat = cmstatString
  | otherwise = "<tr><td colspan=\"2\">" ++ show (fromLeft inputcmstat) ++ "</td></tr>"
    where cmStat = fromRight inputcmstat  
          cmstatString = "<tr><td>Sequence Number</td><td>" ++ show (statSequenceNumber cmStat)++ "</td></tr>" ++ "<tr><td>Effective Sequences</td><td>" ++ show (statEffectiveSequences cmStat)++ "</td></tr>" ++ "<tr><td>Consensus length</td><td>" ++ show (statConsensusLength cmStat) ++ "</td></tr>" ++ "<tr><td>Expected maximum hit-length</td><td>" ++ show (statW cmStat) ++ "</td></tr>" ++ "<tr><td>Basepairs</td><td>" ++ show (statBasepairs cmStat)++ "</td></tr>" ++ "<tr><td>Bifurcations</td><td>" ++ show (statBifurcations cmStat) ++ "</td></tr>" ++ "<tr><td>Modeltype</td><td>" ++ show (statModel cmStat) ++ "</td></tr>" ++ "<tr><td>Relative Entropy CM</td><td>" ++ show (relativeEntropyCM cmStat) ++ "</td></tr>" ++ "<tr><td>Relative Entropy HMM</td><td>" ++ show (relativeEntropyHMM cmStat) ++ "</td></tr>"

rnaZHtml :: Either ParseError RNAz -> String 
rnaZHtml inputRNAz 
  | isRight inputRNAz = rnazString
  | otherwise = show (fromLeft inputRNAz)
    where rnaZ = fromRight inputRNAz
          rnazString = "<tr><td>Mean pairwise identity</td><td>" ++ show (meanPairwiseIdentity rnaZ) ++ "</td></tr><tr><td>Shannon entropy</td><td>" ++ show (shannonEntropy rnaZ) ++  "</td></tr><tr><td>GC content</td><td>" ++ show (gcContent rnaZ) ++ "</td></tr><tr><td>Mean single sequence minimum free energy</td><td>" ++ show (meanSingleSequenceMinimumFreeEnergy rnaZ) ++ "</td></tr><tr><td>Consensus minimum free energy</td><td>" ++ show (consensusMinimumFreeEnergy rnaZ) ++ "</td></tr><tr><td>Energy contribution</td><td>" ++ show (energyContribution rnaZ) ++ "</td></tr><tr><td>Covariance contribution</td><td>" ++ show (covarianceContribution rnaZ) ++ "</td></tr><tr><td>Combinations pair</td><td>" ++ show (combinationsPair rnaZ) ++ "</td></tr><tr><td>Mean z-score</td><td>" ++ show (meanZScore rnaZ) ++ "</td></tr><tr><td>Structure conservation index</td><td>" ++ show (structureConservationIndex rnaZ) ++ "</td></tr><tr><td>Background model</td><td>" ++ backgroundModel rnaZ ++ "</td></tr><tr><td>Decision model</td><td>" ++ decisionModel rnaZ ++ "</td></tr><tr><td>SVM decision value</td><td>" ++ show (svmDecisionValue rnaZ) ++ "</td></tr><tr><td>SVM class propability</td><td>" ++ show (svmRNAClassProbability rnaZ) ++ "</td></tr><tr><td>Prediction</td><td>" ++ (prediction rnaZ)++ "</td></tr>"

rnaCodeHtml :: Either ParseError RC.RNAcode -> String 
rnaCodeHtml inputRNAcode 
  | isRight inputRNAcode = rnaCodeString
  | otherwise = show (fromLeft inputRNAcode)
    where rnaCodeHits = RC.rnacodeHits (fromRight inputRNAcode)
          rnaCodeHeader ="<tr><td>Hss</td><td>Frame</td><td>Length</td><td>From</td><td>TO</td><td>Name</td><td>Start</td><td>End</td><td>Score</td><td>p-value</td></tr>"
          rnaCodeBody = concatMap rnaCodeHitHtml rnaCodeHits
          rnaCodeString = rnaCodeHeader ++ rnaCodeBody

rnaCodeHitHtml :: RC.RNAcodeHit -> String
rnaCodeHitHtml rnaCode = "<tr><td>" ++ show (RC.hss rnaCode) ++ "</td><td>" ++ show (RC.frame rnaCode) ++  "</td><td>" ++ show (RC.length rnaCode) ++ "</td><td>" ++ show (RC.from rnaCode) ++ "</td><td>" ++ show (RC.to rnaCode) ++ "</td><td>" ++ RC.name rnaCode ++ "</td><td>" ++ show (RC.start rnaCode) ++ "</td><td>" ++ show (RC.end rnaCode) ++ "</td><td>" ++ show (RC.score rnaCode) ++ "</td><td>" ++ show (RC.pvalue rnaCode) ++ "</td></tr>"


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

