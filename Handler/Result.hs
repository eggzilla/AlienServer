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
    outputPath <- fmap extraTempdir getExtra
    let temporaryDirectoryPath = DT.unpack (outputPath) ++ sessionId ++ "/"
    --check if tempdir exists otherwise short circuit
    tempDirPresent <- liftIO (doesDirectoryExist temporaryDirectoryPath)
    --checkSessionId tempDirPresent             
    --retrieve alienoutput and check if done
    done <- liftIO (doesFileExist (temporaryDirectoryPath ++ "done"))
    let unfinished = not done        
    alienLog <- liftIO (readFile (temporaryDirectoryPath ++ "Log"))
    let iterationLogPaths = map (\x -> temporaryDirectoryPath ++ (show x) ++ ".log") [0,1,2,3,4,5,6,7,8,9,10]
    existentIterationLogs <- liftIO (filterM doesFileExist iterationLogPaths)
    iterationLogs <- liftIO (mapM retrieveIterationLog existentIterationLogs)
    
    let resultInsert = DT.pack (concat iterationLogs)
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "result")

retrieveIterationLog :: String -> IO String
retrieveIterationLog filePath = do
      iterationLog <- readFile filePath
      let logfields = splitOn "," iterationLog
      let iterationLine = "<tr><td>" ++ logfields !! 0 ++ "<td>" ++ logfields !! 1 ++ "<td>" ++ logfields !! 2 ++ "<td>" ++ logfields !! 3 ++"\n"
      return iterationLine

