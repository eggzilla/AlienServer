{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.ByteString.Lazy (unpack)
import Yesod.Core.Handler
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import qualified Data.Text as DT
import Settings.StaticFiles
import System.Process
import System.Random
import System.Directory
import System.IO (writeFile)
import Data.Int (Int16)
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost inputForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost inputForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess (fasta,taxid) -> Just (fasta,taxid)
            _ -> Nothing
    --Create tempdir and session-Id
    sessionId <- liftIO createSessionId
    approot  <- fmap extraApproot getExtra
    revprox  <- fmap extraRevprox getExtra
    outputPath <- fmap extraTempdir getExtra
    geQueueName <- fmap extraGEqueuename getExtra  
    taxNodeFilePath <- fmap extraTaxNodeFilePath getExtra                
    let temporaryDirectoryPath = (DT.unpack outputPath) ++ sessionId ++ "/"  
    let alienLogPath = temporaryDirectoryPath ++ "Log"             
    liftIO (createDirectory temporaryDirectoryPath)
           
    --Write input fasta file
    liftIO (fileMove (fst (fromJust submission)) (temporaryDirectoryPath ++ "input.fa"))
                  
    --Submit RNAlien Job to SGE
    let aliencommand = "RNAlien -i "++ temporaryDirectoryPath ++ "input.fa -c 1 -t " ++ (DT.unpack (snd (fromJust submission))) ++" -d "++ sessionId ++ " -n " ++ (DT.unpack taxNodeFilePath)  ++ " -o " ++ (DT.unpack outputPath) ++  " > " ++ alienLogPath
    --sun grid engine settings
    let qsubLocation = "/usr/bin/qsub"
    let geErrorDir = temporaryDirectoryPath ++ "gelog"
    let geLogOutputDir = temporaryDirectoryPath ++ "gelog"
    let geRootDirectory = "/usr/share/gridengine"
    let bashscriptpath = temporaryDirectoryPath ++ "qsub.sh"
    let home = "/mnt/storage/home/egg"
    let bashheader = "#!/bin/bash\n"
    let bashLDLibrary = "#$ -v LD_LIBRARY_PATH=/mnt/storage/egg/Tools/lib\n"
    let bashPath = "#$ -v PATH=" ++ home ++ "/Tools/bin:" ++ home ++  "/Tools/clustalo/bin:" ++ home ++ "/Tools/ViennaRNA/bin:" ++ home ++ "/Tools/locarna/bin:" ++ home ++ "/Tools/infernal/bin:" ++ home ++ "/.cabal/bin:/usr/bin/:$PATH\n"
    let bashcontent = bashheader ++ bashLDLibrary ++ bashPath ++ aliencommand
    let qsubcommand = qsubLocation ++ " -N " ++ sessionId ++ " -l h_vmem=5G " ++ " -q " ++ (DT.unpack geQueueName) ++ " -e " ++ geErrorDir ++ " -o " ++  geLogOutputDir ++ " " ++ bashscriptpath ++ " > " ++ temporaryDirectoryPath ++ "GEJobid"
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
  let sessionId = randomid randomNumber
  return sessionId
