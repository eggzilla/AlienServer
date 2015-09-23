{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tool where

import Import
getToolR :: Handler Html
getToolR = do
    defaultLayout $ do
        setTitle "Welcome To RNAlien!"
        $(widgetFile "tool")

