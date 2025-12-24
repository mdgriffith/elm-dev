{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ext.Test.Result
  ( TestId
  , Report(..)
  , TestRun(..)
  , TestResult(..)
  , Reason(..)
  , decodeReports
  , decodeReportsStrict
  , decodeReportsString
  ) where

import qualified Data.Aeson
import qualified Data.Aeson.Types
import           Data.Aeson ((.:), (.:?), withObject, (.!=))
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.UTF8
import qualified Data.Text


type TestId = String


data Report = Report
  { reportId   :: TestId
  , reportRuns :: [TestRun]
  , reportIsOnly :: Bool
  } deriving (Eq, Show)


data TestRun = TestRun
  { testRunLabel  :: [String]
  , testRunResult :: [TestResult]
  } deriving (Eq, Show)


data TestResult
  = Passed
  | Skipped
  | Failed
      { failureGiven       :: Maybe String
      , failureMessage     :: String
      , failureReason      :: Reason
      }
  deriving (Eq, Show)


data Reason
  = Custom String
  | Equality { equalityExpected :: String, equalityActual :: String }
  | Comparison { comparisonFirst :: String, comparisonSecond :: String }
  | TODO
  | Invalid String
  | ListDiff { listDiffExpected :: [String], listDiffActual :: [String] }
  | CollectionDiff
      { collectionExpected :: String
      , collectionActual   :: String
      , collectionExtra    :: [String]
      , collectionMissing  :: [String]
      }
  deriving (Eq, Show)



instance Data.Aeson.FromJSON Report where
  parseJSON = withObject "Report" $ \o -> do
    reportId <- o .: "id"
    reportRuns <- o .: "runs"
    reportIsOnly <- o .: "isOnly"
    pure Report{..}


instance Data.Aeson.FromJSON TestRun where
  parseJSON = withObject "TestRun" $ \o -> do
    testRunLabel <- o .: "label"
    testRunResult <- o .: "result"
    pure TestRun{..}


instance Data.Aeson.FromJSON TestResult where
  parseJSON = withObject "TestResult" $ \o -> do
    status <- (o .: "status" :: Data.Aeson.Types.Parser String)
    case status of
      "pass" -> pure Passed
      "skip" -> pure Skipped
      "fail" -> do
        given <- o .:? "given"
        message <- o .: "message"
        reason <- o .: "reason"
        -- If reason is Custom, replace it with Custom message
        let finalReason = case reason of
              Custom _ -> Custom message
              _ -> reason
        pure (Failed given message finalReason)
      _ -> fail ("Unknown test result status: " <> status)


instance Data.Aeson.FromJSON Reason where
  parseJSON v =
    withObject "Reason" (\o -> do
            typ <- (o .: "type" :: Data.Aeson.Types.Parser String)
            case typ of
              "Custom" -> pure (Custom "")
              "TODO" -> pure TODO
              "Invalid" -> Invalid <$> o .: "data"
              "Equality" -> Equality <$> o .: "expected" <*> o .: "actual"
              "Comparison" -> Comparison <$> o .: "first" <*> o .: "second"
              "ListDiff" -> ListDiff <$> o .: "expected" <*> o .: "actual"
              "CollectionDiff" -> CollectionDiff <$> o .: "expected" <*> o .: "actual" <*> o .: "extra" <*> o .: "missing"
              other -> fail ("Unknown reason type: " <> other)
         ) v






decodeReports :: Data.ByteString.Lazy.ByteString -> Either String [Report]
decodeReports = Data.Aeson.eitherDecode'


decodeReportsStrict :: Data.ByteString.ByteString -> Either String [Report]
decodeReportsStrict = Data.Aeson.eitherDecodeStrict'


decodeReportsString :: String -> Either String [Report]
decodeReportsString = Data.Aeson.eitherDecode' . Data.ByteString.Lazy.fromStrict . Data.ByteString.UTF8.fromString
