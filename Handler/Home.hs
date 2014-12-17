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
    outputPath <- fmap extraTempdir getExtra
    let temporaryDirectoryPath = (DT.unpack outputPath) ++ sessionId ++ "/"                     
    liftIO (createDirectory temporaryDirectoryPath)
           
    --Write input fasta file
    liftIO (fileMove (fst (fromJust submission)) (temporaryDirectoryPath ++ "input.fa"))
                  
    --Start RNAlien Job
    let aliencommand = "nohup RNAlien -i "++ temporaryDirectoryPath ++ "input.fa -c 3 -t " ++ (DT.unpack (snd (fromJust submission))) ++" -d "++ sessionId ++ " -o " ++ temporaryDirectoryPath ++  " > " ++ temporaryDirectoryPath ++ "alienserverLog"
    liftIO (writeFile (temporaryDirectoryPath ++ "alienserverCommand") aliencommand)
    _ <- liftIO (runCommand (aliencommand))
           
    --Render page
    defaultLayout $ do
        aDomId <- newIdent
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
