module System.Statusbar.Pomodoro.ProtocolTest (spec) where

import System.Statusbar.Pomodoro.Gen qualified as Gen

import Data.Aeson qualified as Aeson
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, tripping)

spec :: Spec
spec =
  describe "Statusbar.Pomodoro.Protocol" $ do
    describe "ProtoVersion" $ do
      it "round-trips through Aeson" $ hedgehog $ do
        ver <- forAll Gen.protoVersion
        tripping ver Aeson.encode Aeson.eitherDecode

    describe "RequestId" $ do
      it "round-trips through Aeson" $ hedgehog $ do
        reqId <- forAll Gen.requestId
        tripping reqId Aeson.encode Aeson.eitherDecode

    describe "Req" $ do
      it "round-trips through Aeson" $ hedgehog $ do
        req <- forAll Gen.req
        tripping req Aeson.encode Aeson.eitherDecode

    describe "ReqCommand" $ do
      it "round-trips through Aeson" $ hedgehog $ do
        cmd <- forAll Gen.reqCommand
        tripping cmd Aeson.encode Aeson.eitherDecode

    describe "ReqStatus" $ do
      it "round-trips through Aeson" $ hedgehog $ do
        st <- forAll Gen.respStatus
        tripping st Aeson.encode Aeson.eitherDecode

    describe "ErrorResponse" $ do
      it "round-trips through Aeson" $ hedgehog $ do
        err <- forAll Gen.errorResponse
        tripping err Aeson.encode Aeson.eitherDecode

    describe "StatusResponse" $ do
      it "round-trips through Aeson" $ hedgehog $ do
        let rangeSecs = Range.linear 0 Gen.upperBoundSecs

        resp <- forAll (Gen.statusResponse rangeSecs)
        tripping resp Aeson.encode Aeson.eitherDecode

    describe "RespTimerState" $ do
      it "round-trips though Aeson" $ hedgehog $ do
        st <- forAll Gen.respTimerState
        tripping st Aeson.encode Aeson.eitherDecode
