{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Help where

import Import
import Settings.StaticFiles

getHelpR :: Handler Html
getHelpR = do
    defaultLayout $ do
        setTitle "Welcome To RNAlien!"
        $(widgetFile "help")

