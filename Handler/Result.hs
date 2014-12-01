{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Result where

import Import
import Data.ByteString.Lazy (unpack)
import Yesod.Core.Handler
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import qualified Data.Text as DT
import qualified Data.List as DL (head)
    
import Settings.StaticFiles
import Yesod.Form.Bootstrap3    
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getResultR :: Handler Html
getResultR = do
    result <- getRequest
    let params = reqGetParams result
    let resultInsert = DT.unpack (snd (DL.head params))
 
            
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To RNAlien!"
        $(widgetFile "result")

        
