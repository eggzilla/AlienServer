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
    --inputorganism <- do
    --    ((formResult, _), _) <- runFormPost inputForm
    --    case formResult of
    --        FormSuccess (inputorganism) -> return inputorganism
    --        _ -> return ( TaxonomyInput (DT.pack ""))
    --inputRequest <- getRequest
    inputorganism <-lookupGetParam (DT.pack "term")
    let organism = fromMaybe "" inputorganism
    --let test=DT.pack "test"
    -- cut -f 3  names.dmp | sort > organismslist
    organismString <- liftIO (DI.readFile "/mnt/storage/data/rnalien/organismslist")
    let organisms = V.fromList $ DT.lines $ organismString
    let hits = V.take 20 $ V.filter (DT.isInfixOf organism) organisms
    return $ Array (V.map String hits)
    --return $ Array (V.map String (V.fromList [organism,test]))

--data TaxonomyInput = TaxonomyInput
--    { inputOrganism :: DT.Text
--    }

--inputForm :: Form (TaxonomyInput)
--inputForm = renderBootstrap3 BootstrapBasicForm $ (TaxonomyInput)
--    <$> areq textField (withSmallInput "") Nothing 
--    <*> aopt ((jqueryAutocompleteField' 2) TaxonomyR) (withSmallInput "Enter Taxonomy Id:") Nothing

--inputForm :: Form (FileInfo, Maybe Text)
--inputForm = renderBootstrap3 BootstrapBasicForm $ (,)
--    <$> fileAFormReq "Upload a fasta sequence file"
--    <*> aopt ((jqueryAutocompleteField' 2) TaxonomyR) (withSmallInput "Enter Taxonomy Id:") Nothing
