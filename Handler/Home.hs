
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Control.Applicative
import Data.Maybe (fromJust)
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString as L
import System.Process
import System.Random
import System.Directory
import System.IO (writeFile)
import Data.Int (Int16)
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost inputForm
    (sampleWidget, sampleEnctype) <- generateFormPost sampleForm
   -- maybeToken <- fmap reqToken getRequest
 --   let token = fromJust maybeToken
    let samplefasta = DT.pack ">AARQ02000011.1/391-585\nAAUUGAAUAGAAGCGCCAGAACUGAUUGGGACGAAAAUGCUUGAAGGUGAAAUCCCUGAA\nAAGUAUCGAUCAGUUGACGAGGAGGAGAUUAAUCGAAGUUUCGGCGGGAGUCUCCCGGCU\nGUGCAUGCAGUCGUUAAGUCUUACUUACAAAUCAUUUGGGUGACCAAGUGGACAGAGUAG\nUAAUGAAACAUGCUU\n"
    --let failure = ""
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost inputForm
    ((sampleresult, _), _) <- runFormPost sampleForm
    let inputsubmission = case result of
            FormSuccess (fasta,taxid) -> Just (fasta,taxid)
            _ -> Nothing
    let samplesubmission = case sampleresult of
            FormSuccess (fasta,taxid) -> Just (DTE.encodeUtf8 fasta,taxid)
            _ -> Nothing
    if ((isJust inputsubmission) || (isJust samplesubmission))
      then do
       --Create tempdir and session-Id
       sessionId <- liftIO createSessionId
       revprox  <- fmap extraRevprox getExtra
       outputPath <- fmap extraTempdir getExtra
       geQueueName <- fmap extraGEqueuename getExtra
       let temporaryDirectoryPath = (DT.unpack outputPath) ++ sessionId ++ "/"  
       let alienLogPath = temporaryDirectoryPath ++ "Log"             
       liftIO (createDirectory temporaryDirectoryPath) 
       taxDumpDirectoryPath <- fmap extraTaxDumpPath getExtra
       let alienResultCsvFilePath = temporaryDirectoryPath ++ "result.csv"
       --Write input fasta file
       liftIO (writesubmissionData temporaryDirectoryPath inputsubmission samplesubmission)
       --liftIO (fileMove (fst (fromJust submission)) (temporaryDirectoryPath ++ "input.fa"))
       --liftIO (writeFile (temporaryDirectoryPath ++ "input.fa") (L.unpack (fst (fromJust submission))))
       let taxonomyId = extractTaxonomyId inputsubmission samplesubmission
       --Submit RNAlien Job to SGE
       let aliencommand = "RNAlien -i "++ temporaryDirectoryPath ++ "input.fa -c 5 -t " ++ (DT.unpack  taxonomyId) ++" -d "++ sessionId ++ " -o " ++ (DT.unpack outputPath) ++  " > " ++ alienLogPath ++ "\n"
       let ids2treecommand = "Ids2Tree -l 3 -f json -i " ++ (DT.unpack taxDumpDirectoryPath) ++ " -o " ++ temporaryDirectoryPath ++ " -r " ++ alienResultCsvFilePath  ++ "\n"
       let cmccommand = "cp " ++  temporaryDirectoryPath ++ "result.cm " ++ " /mnt/storage/tmp/cmcws/upload/" ++ sessionId ++ " \n"
       let archivecommand = "zip -9 -r " ++  temporaryDirectoryPath ++ "result.zip " ++ temporaryDirectoryPath ++ "\n"
        --sun grid engine settings
       let qsubLocation = "/usr/bin/qsub"
       let geErrorDir = temporaryDirectoryPath ++ "gelog"
       let geLogOutputDir = temporaryDirectoryPath ++ "gelog"
       let bashscriptpath = temporaryDirectoryPath ++ "qsub.sh"
       let home = "/mnt/storage/home/egg"
       let bashheader = "#!/bin/bash\n"
       let bashLDLibrary = "#$ -v LD_LIBRARY_PATH=" ++ home ++ "/Tools/locarna/lib\n"
       let bashmemrequest = "#$ -l mem_free=4G\n"
       let parallelenv = "#$ -pe para 5\n"
       let bashPath = "#$ -v PATH=" ++ home ++ "/Tools/bin:" ++ home ++  "/Tools/clustalo/bin:" ++ home ++ "/Tools/ViennaRNA/bin:" ++ home ++ "/Tools/locarna/bin:" ++ home ++ "/Tools/infernal/bin:" ++ home ++ "/.cabal/bin:/usr/bin/:/bin/:$PATH\n"
       let bashcontent = bashheader ++ bashLDLibrary ++ bashmemrequest ++ parallelenv ++ bashPath ++ aliencommand ++ ids2treecommand ++ cmccommand ++ archivecommand 
       let qsubcommand = qsubLocation ++ " -N " ++ sessionId ++ " -l h_vmem=12G " ++ " -q " ++ (DT.unpack geQueueName) ++ " -e " ++ geErrorDir ++ " -o " ++  geLogOutputDir ++ " " ++ bashscriptpath ++ " > " ++ temporaryDirectoryPath ++ "GEJobid"
       liftIO (writeFile geErrorDir "")
       liftIO (writeFile alienLogPath "")
       liftIO (writeFile bashscriptpath bashcontent)
       _ <- liftIO (runCommand (qsubcommand))
--    (formWidget, formEnctype) <- generateFormPost sampleForm
--    (sampleWidget, sampleEnctype) <- generateFormPost sampleForm
--    maybeToken <- fmap reqToken getRequest
--    let token = fromJust maybeToken
--    maybeToken <- fmap reqToken getRequest
--    let samplefasta = DT.pack ">AARQ02000011.1/391-585\nAAUUGAAUAGAAGCGCCAGAACUGAUUGGGACGAAAAUGCUUGAAGGUGAAAUCCCUGAA\nAAGUAUCGAUCAGUUGACGAGGAGGAGAUUAAUCGAAGUUUCGGCGGGAGUCUCCCGGCU\nGUGCAUGCAGUCGUUAAGUCUUACUUACAAAUCAUUUGGGUGACCAAGUGGACAGAGUAG\nUAAUGAAACAUGCUU\n"
--    defaultLayout $ do
--        aDomId <- newIdent
--        setTitle "Welcome To RNAlien!"
--       $(widgetFile "homepage")       
    --Render page
       defaultLayout $ do
         aDomId <- newIdent
         let approotjs = revprox
         let sessionIdInsert =  DT.pack sessionId
         let sessionIdjs = sessionId                       
         --let resultInsert = DT.unpack (fileName (fst (fromJust submission)))
         setTitle "Welcome To RNAlien!"
         $(widgetFile "calc")
      else do
        (formWidget, formEnctype) <- generateFormPost sampleForm
        (sampleWidget, sampleEnctype) <- generateFormPost sampleForm
       -- maybeToken <- fmap reqToken getRequest
     --   let token = fromJust maybeToken
        let samplefasta = DT.pack ">AARQ02000011.1/391-585\nAAUUGAAUAGAAGCGCCAGAACUGAUUGGGACGAAAAUGCUUGAAGGUGAAAUCCCUGAA\nAAGUAUCGAUCAGUUGACGAGGAGGAGAUUAAUCGAAGUUUCGGCGGGAGUCUCCCGGCU\nGUGCAUGCAGUCGUUAAGUCUUACUUACAAAUCAUUUGGGUGACCAAGUGGACAGAGUAG\nUAAUGAAACAUGCUU\n"
        --let failure = ""
        defaultLayout $ do
          aDomId <- newIdent
          setTitle "Welcome To RNAlien!"
          $(widgetFile "homepage")

         
inputForm :: Form (FileInfo, Text)
inputForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Upload a fasta sequence file"
    <*> areq textField (withSmallInput "Enter Taxonomy Id:") Nothing

writesubmissionData :: [Char] -> Maybe (FileInfo, b) -> Maybe (L.ByteString, b1) -> IO()
writesubmissionData temporaryDirectoryPath inputsubmission samplesubmission = do
  if isJust inputsubmission
     then do
       liftIO (fileMove (fst (fromJust inputsubmission)) (temporaryDirectoryPath ++ "input.fa"))
     else do
       liftIO (L.writeFile (temporaryDirectoryPath ++ "input.fa") ((fst (fromJust samplesubmission))))
--liftIO (fileMove (fst (fromJust submission)) (temporaryDirectoryPath ++ "input.fa"))
--liftIO (writeFile (temporaryDirectoryPath ++ "input.fa") (L.unpack (fst (fromJust submission))))


sampleForm :: Form (Text, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq hiddenField (withSmallInput "") (Just ">AARQ02000011.1/391-585\nAAUUGAAUAGAAGCGCCAGAACUGAUUGGGACGAAAAUGCUUGAAGGUGAAAUCCCUGAA\nAAGUAUCGAUCAGUUGACGAGGAGGAGAUUAAUCGAAGUUUCGGCGGGAGUCUCCCGGCU\nGUGCAUGCAGUCGUUAAGUCUUACUUACAAAUCAUUUGGGUGACCAAGUGGACAGAGUAG\nUAAUGAAACAUGCUU\n")
    <*> areq hiddenField (withSmallInput "") (Just "393124")

-- Auxiliary functions:
-- | Adds cm prefix to pseudo random number
randomid :: Int16 -> String
randomid number = "cm" ++ (show number)

createSessionId :: IO String                  
createSessionId = do
  randomNumber <- randomIO :: IO Int16
  let sessionId = randomid (abs randomNumber)
  return sessionId



--extractTaxonomyId :: Maybe (L.ByteString,Text) -> Maybe (L.ByteString,Text) -> Text
extractTaxonomyId inputsubmission samplesubmission
  | isJust inputsubmission = snd (fromJust inputsubmission)
  | otherwise = snd (fromJust samplesubmission)


--selectSubmission :: Maybe (L.ByteString,Text) -> Maybe (L.ByteString,Text) -> Maybe (L.ByteString,Text)
--selectSubmission inputsubmission samplesubmission
--  | isJust inputsubmission =  inputsubmission
--  | isJust samplesubmission = samplesubmission
--  | otherwise = Nothing
