{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Taxonomy where

import Import
import qualified Data.Text as DT
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )
import Yesod.Form.Jquery
import qualified Data.Vector as V

getTaxonomyR :: Handler Value
getTaxonomyR = do
    inputorganism <- do
        ((formResult, _), _) <- runFormPost inputForm
        case formResult of
            FormSuccess (_,inputorganism) -> return inputorganism
            _ -> return (Just $ DT.pack "")
    return $ Array (V.fromList [String "abc",String "def"]) 

inputForm :: Form (FileInfo, Maybe Text)
inputForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Upload a fasta sequence file"
    <*> aopt ((jqueryAutocompleteField' 2) TaxonomyR) (withSmallInput "Enter Taxonomy Id:") Nothing
