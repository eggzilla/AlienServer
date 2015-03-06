{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Help where

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
getHelpR :: Handler Html
getHelpR = do
    let handlerName = "getHelpR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "help")

