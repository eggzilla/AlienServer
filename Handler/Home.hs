{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Home where

import Import hiding ((<|>),many,optional)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
--import qualified Data.Text.IO as DTI
import qualified Data.ByteString.Char8 as B
import System.Process
import System.Random
import System.Directory
import System.IO (writeFile)
import Data.Int (Int16)
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput, withLargeInput )
import Text.Parsec
import Text.Parsec.ByteString
import Data.Either.Unwrap
import Data.List.Split hiding (oneOf)
import Data.List
import Yesod.Form.Jquery
import Yesod.Core (Route)
import Data.Maybe
import qualified Data.HashMap as HM
import Data.Tuple
import Control.Applicative ((<*>),(<$>))

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost inputForm
    (sampleWidget, sampleEnctype) <- generateFormPost sampleForm
    defaultLayout $ do
        aDomId <- newIdent
        let errorMsg = DT.pack ""
        setTitle "Welcome To RNAlien!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((formResult, _), _) <- runFormPost inputForm
    ((sampleResult, _), _) <- runFormPost sampleForm
    --Create tempdir and session-Id
    sessionId <- liftIO createSessionId
    revprox  <- fmap extraRevprox getExtra
    outputPath <- fmap extraTempdir getExtra
    geQueueName <- fmap extraGEqueuename getExtra
    let temporaryDirectoryPath = (DT.unpack outputPath) ++ sessionId ++ "/"
    let inputPath = temporaryDirectoryPath ++ "input.fa"
    liftIO (writesubmissionData formResult sampleResult temporaryDirectoryPath)
    uploadedFile <- liftIO (B.readFile inputPath)
    taxIdsOrganismsFile <- liftIO (B.readFile "/mnt/storage/data/rnalien/taxidsorganisms")
    --cut -f 1,3  names.dmp > taxidsorganisms
    let taxIdsOrganismsHash = HM.fromList $ catMaybes $ map pairs (map (B.split '\t') (B.lines taxIdsOrganismsFile))
    let taxonomyInfo = extractTaxonomyInfo formResult sampleResult
    let validatedInput = validateInput uploadedFile taxonomyInfo taxIdsOrganismsHash
    if (isRight validatedInput)
      then do  
       let alienLogPath = temporaryDirectoryPath ++ "Log"             
       liftIO (createDirectory temporaryDirectoryPath) 
       taxDumpDirectoryPath <- fmap extraTaxDumpPath getExtra
       let alienResultCsvFilePath = temporaryDirectoryPath ++ "result.csv"
       --Write input fasta file
       let taxonomySwitch = setTaxonomyId (fromRight validatedInput)
       --Submit RNAlien Job to SGE
       let aliencommand = "RNAlien -i "++ inputPath ++ " -c 5 " ++ taxonomySwitch ++" -d "++ sessionId ++ " -o " ++ (DT.unpack outputPath) ++  " > " ++ alienLogPath ++ "\n"
       let ids2treecommand = "Ids2Tree -l 3 -f json -i " ++ (DT.unpack taxDumpDirectoryPath) ++ " -o " ++ temporaryDirectoryPath ++ " -r " ++ alienResultCsvFilePath  ++ "\n"
       let cmccommand = "cp " ++  temporaryDirectoryPath ++ "result.cm " ++ " /mnt/storage/tmp/cmcws/upload/" ++ sessionId ++ " \n"
       let archivecommand = "zip -9 -r " ++  temporaryDirectoryPath ++ "result.zip " ++ temporaryDirectoryPath ++ "\n"
        --sun grid engine settings
       let qsubLocation = "/usr/bin/qsub"
       let geErrorDir = temporaryDirectoryPath ++ "gelog"
       let geLogOutputDir = temporaryDirectoryPath ++ "gelog"
       let bashscriptpath = temporaryDirectoryPath ++ "qsub.sh"
       let home = "/mnt/storage/home/egg"
       let bashheader = "#!/bin/bash\n"
       let bashLDLibrary = "#$ -v LD_LIBRARY_PATH=" ++ home ++ "/Tools/locarna/lib\n"
       let bashmemrequest = "#$ -l mem_free=4G\n"
       let parallelenv = "#$ -pe para 5\n"
       let bashPath = "#$ -v PATH=" ++ home ++ "/Tools/bin:" ++ home ++  "/Tools/clustalo/bin:" ++ home ++ "/Tools/ViennaRNA/bin:" ++ home ++ "/Tools/locarna/bin:" ++ home ++ "/Tools/infernal/bin:" ++ home ++ "/.cabal/bin:/usr/bin/:/bin/:$PATH\n"
       let bashcontent = bashheader ++ bashLDLibrary ++ bashmemrequest ++ parallelenv ++ bashPath ++ aliencommand ++ ids2treecommand ++ cmccommand ++ archivecommand 
       let qsubcommand = qsubLocation ++ " -N " ++ sessionId ++ " -l h_vmem=12G " ++ " -q " ++ (DT.unpack geQueueName) ++ " -e " ++ geErrorDir ++ " -o " ++  geLogOutputDir ++ " " ++ bashscriptpath ++ " > " ++ temporaryDirectoryPath ++ "GEJobid"
       liftIO (writeFile geErrorDir "")
       liftIO (writeFile alienLogPath "")
       liftIO (writeFile bashscriptpath bashcontent)
       _ <- liftIO (runCommand (qsubcommand))
       --Render page
       defaultLayout $ do
         aDomId <- newIdent
         let approotjs = revprox
         let sessionIdInsert =  DT.pack sessionId
         let sessionIdjs = sessionId                       
         setTitle "Welcome To RNAlien!"
         $(widgetFile "calc")
      else do
        (formWidget, formEnctype) <- generateFormPost inputForm
        (sampleWidget, sampleEnctype) <- generateFormPost sampleForm
        defaultLayout $ do
          aDomId <- newIdent
          setTitle "Welcome To RNAlien!"
          let parsingErrors = fromLeft validatedInput
          let errorMsg = DT.pack ("<div class=\"alert alert-danger\" role=\"alert\">" ++ parsingErrors ++ "</div><br>")
          $(widgetFile "homepage")

         
inputForm :: Form (FileInfo, Maybe Text)
inputForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Upload a fasta sequence file"
    <*> aopt ((jqueryAutocompleteField' 2) TaxonomyR) (withLargeInput "Enter Taxonomy Id or Name:") Nothing

sampleForm :: Form (Text, Maybe Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq hiddenField (withSmallInput "") (Just ">AARQ02000011.1/391-585\nAAUUGAAUAGAAGCGCCAGAACUGAUUGGGACGAAAAUGCUUGAAGGUGAAAUCCCUGAA\nAAGUAUCGAUCAGUUGACGAGGAGGAGAUUAAUCGAAGUUUCGGCGGGAGUCUCCCGGCU\nGUGCAUGCAGUCGUUAAGUCUUACUUACAAAUCAUUUGGGUGACCAAGUGGACAGAGUAG\nUAAUGAAACAUGCUU\n")
    <*> aopt hiddenField (withSmallInput "") (Just (Just (DT.pack "393124")))

-- Auxiliary functions:
-- | Adds cm prefix to pseudo random number
randomid :: Int16 -> String
randomid number = "cm" ++ (show number)

writesubmissionData :: FormResult (FileInfo,Maybe Text) -> FormResult (Text,Maybe Text) -> String -> IO()
writesubmissionData inputsubmission samplesubmission temporaryDirectoryPath = do
    case inputsubmission of
      FormSuccess (fasta,_) -> liftIO (fileMove fasta (temporaryDirectoryPath ++ "input.fa"))
      _ -> case samplesubmission of
        FormSuccess (fasta,_) -> (writeFile (temporaryDirectoryPath ++ "input.fa") (DT.unpack fasta))
        _ -> return ()

createSessionId :: IO String                  
createSessionId = do
  randomNumber <- randomIO :: IO Int16
  let sessionId = randomid (abs randomNumber)
  return sessionId

extractTaxonomyInfo :: FormResult (FileInfo,Maybe Text) -> FormResult (Text,Maybe Text) -> Maybe Text
extractTaxonomyInfo (FormSuccess (_,taxonomyInfo)) _ = taxonomyInfo
extractTaxonomyInfo _ (FormSuccess (_,taxonomyInfo)) = taxonomyInfo
extractTaxonomyInfo _ _ = Nothing

validateInput :: B.ByteString -> Maybe Text -> HM.HashMap B.ByteString Int -> Either String (Maybe Text)
validateInput fastaFileContent taxonomyInfo taxIdsOrganismsHash
  | (isRight checkedForm) && (isRight checkedTaxonomyInfo) = checkedTaxonomyInfo
  | otherwise = Left (convertErrorMessagetoHTML((unwrapEither checkedForm) ++ (unwrapEither checkedTaxonomyInfo)))
  where checkedForm =  either (\a -> Left (show a)) (\_ -> Right ("Input ok" :: String)) (parseFasta fastaFileContent)
        checkedTaxonomyInfo = checkTaxonomyInfo taxonomyInfo taxIdsOrganismsHash

checkTaxonomyInfo :: Maybe Text -> HM.HashMap B.ByteString Int -> Either String (Maybe Text)
checkTaxonomyInfo (Just taxonomyInfo) taxIdsOrganismsHash = parseTaxonomyInfo taxonomyInfo taxIdsOrganismsHash
checkTaxonomyInfo Nothing taxIdsOrganismsHash = Right Nothing

parseTaxonomyInfo :: Text -> HM.HashMap B.ByteString Int -> Either String (Maybe Text)
parseTaxonomyInfo taxonomyInfo taxIdsOrganismsHash
  | isRight parsedAsInteger = Right (Just taxonomyInfo)
  | isRight parsedAsOrganismName = Right (Just (DT.pack (show (fromRight parsedAsOrganismName))))
  | otherwise = Left (show (fromLeft parsedAsInteger) ++ "<br>" ++ unwrapEither parsedAsOrganismName)
  where parsedAsInteger = parse genParserTaxid "Error in TaxonomyId input:" (DTE.encodeUtf8 taxonomyInfo)
        parsedAsOrganismName = maybe (Left "Error in TaxonomyId input:<br>Provided organism not found<br>") (\a -> Right (DT.pack (show a))) (HM.lookup (DTE.encodeUtf8 taxonomyInfo) taxIdsOrganismsHash)


genParserTaxid :: GenParser B.ByteString st Int
genParserTaxid = do
  taxid <- many1 digit
  return (read taxid ::Int)

setTaxonomyId :: Maybe Text -> String
setTaxonomyId (Just taxonomyInfo) = " -t " ++ (DT.unpack taxonomyInfo) ++ " "
setTaxonomyId Nothing = ""

genParserFasta :: GenParser B.ByteString st Fasta
genParserFasta = do
  _ <- string (">") 
  _header <- many1 (noneOf "\n")                
  _ <- newline
  _sequence <- many genParserSequenceFragments
  eof
  return $ Fasta _header (concat _sequence)

genParserSequenceFragments :: GenParser B.ByteString st String
genParserSequenceFragments = do
  _sequencefragment <- many1 (oneOf "AaCcGgTtUuRrYyKkMmSsWwBbDdHhVvNn-")
  _ <- optional newline
  return $ _sequencefragment
  
-- | parse Fasta
parseFasta :: B.ByteString -> Either ParseError Fasta
parseFasta input = parse genParserFasta "Error in fasta input:" input

data Fasta = Fasta
  { 
    header :: String,
    seq :: String
  }
  deriving (Show, Eq)

unwrapEither :: Either String a -> String
unwrapEither eithervalue = either (\a -> (show a) ++ "<br>") (\_ -> ("" :: String)) eithervalue 

convertErrorMessagetoHTML :: String -> String
convertErrorMessagetoHTML errorMessage = htmlMessage
        where replacedquotes = intercalate "<br>" . splitOn "\\n" $ errorMessage
              replacedlinebreaks = intercalate " " . splitOn "\"" $ replacedquotes
              htmlMessage = intercalate " " . splitOn "\\" $ replacedlinebreaks

pairs :: [B.ByteString] -> Maybe (B.ByteString,Int)
pairs [v,k] = Just (k,fst (fromJust (B.readInt v)))
pairs [] = Nothing
pairs [_] = Nothing
