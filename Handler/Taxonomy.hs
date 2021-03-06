{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Taxonomy where

import Import
import qualified Data.Text as DT
import qualified Data.Text.IO as DI
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )
import Yesod.Form.Jquery
import qualified Data.Vector as V
import Data.Maybe

getTaxonomyR :: Handler Value
getTaxonomyR = do
    inputorganism <-lookupGetParam (DT.pack "term")
    let organism = fromMaybe "" inputorganism
    -- cut -f 3  names.dmp | sort > organismslist
    organismString <- liftIO (DI.readFile "/mnt/storage/data/rnalien/organismslist")
    let organisms = V.fromList $ DT.lines $ organismString
    let hits = V.take 20 $ V.filter (DT.isInfixOf organism) organisms
    return $ Array (V.map String hits)
