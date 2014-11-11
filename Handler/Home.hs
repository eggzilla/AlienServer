{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.ByteString.Lazy (unpack)
import Yesod.Core.Handler
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import qualified Data.Text as DT
import Settings.StaticFiles
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
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        let resultInsert = DT.unpack (fileName (fst (fromJust submission)))
        setTitle "Welcome To RNAlien!"
        $(widgetFile "result")
 
sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Upload a fasta sequence file"
    <*> areq textField (withSmallInput "Enter Taxonomy Id:") Nothing
