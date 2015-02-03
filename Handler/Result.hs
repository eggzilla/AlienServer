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
    let resultInsert = DT.pack alienLog        
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "result")

--checkSessionId :: forall (m :: * -> *). MonadHandler m => Bool -> m ()
--checkSessionId tempDirPresent
--  | tempDirPresent == False = sendResponse (DT.pack "Invalid sessionId provided")
--  | otherwise = return ()
    
--buildResultInsert :: forall (m :: * -> *). a (Data.String.IsString a, Monad m) => Bool -> a -> m a
--buildResultInsert done sessionId = do
--  if done
--    then do
--      let resultInsert = ""
--      return resultInsert
--    else do
--      let resultInsert = sessionId
--      return resultInsert
