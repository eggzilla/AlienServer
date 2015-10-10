
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Control.Applicative
import Data.Maybe (fromJust)
import qualified Data.Text as DT
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
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost inputForm
    let submission = case result of
            FormSuccess (fasta,taxid) -> Just (fasta,taxid)
            _ -> Nothing
    --Create tempdir and session-Id
    sessionId <- liftIO createSessionId
    revprox  <- fmap extraRevprox getExtra
    outputPath <- fmap extraTempdir getExtra
    geQueueName <- fmap extraGEqueuename getExtra            
    let temporaryDirectoryPath = (DT.unpack outputPath) ++ sessionId ++ "/"  
    let alienLogPath = temporaryDirectoryPath ++ "Log"             
    liftIO (createDirectory temporaryDirectoryPath)
  
    --Paths for rendering result taxonomy tree
    taxDumpDirectoryPath <- fmap extraTaxDumpPath getExtra
    let alienResultCsvFilePath = temporaryDirectoryPath ++ "result.csv"

    --Write input fasta file
    liftIO (fileMove (fst (fromJust submission)) (temporaryDirectoryPath ++ "input.fa"))
                  
    --Submit RNAlien Job to SGE
    let aliencommand = "RNAlien -i "++ temporaryDirectoryPath ++ "input.fa -c 5 -t " ++ (DT.unpack (snd (fromJust submission))) ++" -d "++ sessionId ++ " -o " ++ (DT.unpack outputPath) ++  " > " ++ alienLogPath ++ "\n"
    let ids2treecommand = "Ids2Tree -l 3 -f json -i " ++ (DT.unpack taxDumpDirectoryPath) ++ " -o " ++ temporaryDirectoryPath ++ " -r " ++ alienResultCsvFilePath  ++ "\n"
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
    let bashcontent = bashheader ++ bashLDLibrary ++ bashmemrequest ++ parallelenv ++ bashPath ++ aliencommand ++ ids2treecommand ++ archivecommand
    let qsubcommand = qsubLocation ++ " -N " ++ sessionId ++ " -l h_vmem=12G " ++ " -q " ++ (DT.unpack geQueueName) ++ " -e " ++ geErrorDir ++ " -o " ++  geLogOutputDir ++ " " ++ bashscriptpath ++ " > " ++ temporaryDirectoryPath ++ "GEJobid"
    liftIO (writeFile geErrorDir "")
    liftIO (writeFile alienLogPath "")
    liftIO (writeFile bashscriptpath bashcontent)
    _ <- liftIO (runCommand (qsubcommand))
           
    --Render page
    defaultLayout $ do
        aDomId <- newIdent
        let approotjs = revprox
        let sessionIdInsert =  DT.pack sessionId
        let sessionIdjs = sessionId                       
        --let resultInsert = DT.unpack (fileName (fst (fromJust submission)))
        setTitle "Welcome To RNAlien!"
        $(widgetFile "calc")
         
inputForm :: Form (FileInfo, Text)
inputForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Upload a fasta sequence file"
    <*> areq textField (withSmallInput "Enter Taxonomy Id:") Nothing

-- Auxiliary functions:
-- | Adds cm prefix to pseudo random number
randomid :: Int16 -> String
randomid number = "cm" ++ (show number)

createSessionId :: IO String                  
createSessionId = do
  randomNumber <- randomIO :: IO Int16
  let sessionId = randomid (abs randomNumber)
  return sessionId
