{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphStateMachine.Example.RiskManager where

import GraphStateMachine.StateMachine

-- singletons-base
import Data.Singletons.Base.TH

-- text
import Data.Text

newtype Name = Name Text

newtype Surname = Surname Text

newtype TaxCode = TaxCode Text

data UserData = UserData
  { name :: Name
  , surname :: Surname
  , taxCode :: TaxCode
  }

newtype Amount = EuroCents Int

newtype InstalmentsNumber = InstalmentsNumber Int

data LoanDetails = LoanDetails
  { amount      :: Amount
  , instalments :: InstalmentsNumber
  }

newtype MissedPaymentDeadlines = MissedPaymentDeadlines Int

data CreditBureauData = CreditBureauData
  { missedPaymentDeadlines :: MissedPaymentDeadlines
  , arrears                :: Amount
  }

data RiskCommand
  = RegisterUsedData UserData
  | ProvideLoanDetails LoanDetails
  | ProvideCreditBureauData CreditBureauData

data RiskEvent
  = UserDataRegistered UserData
  | LoanDetailsProvided LoanDetails
  | CreditBureauDataReceived CreditBureauData
  | NoOp

$(singletons [d|

  data RiskTag
    = NoDataTag
    | CollectedUserDataTag
    | CollectedLoanDetailsFirstTag
    | ReceivedCreditBureauDataFirstTag
    | CollectedAllDataTag
    deriving stock (Eq, Show)

  riskTopology :: Topology RiskTag
  riskTopology = MkTopology
    [ (NoDataTag, [NoDataTag, CollectedUserDataTag])
    , (CollectedUserDataTag, [CollectedUserDataTag, CollectedLoanDetailsFirstTag, ReceivedCreditBureauDataFirstTag])
    , (CollectedLoanDetailsFirstTag, [CollectedLoanDetailsFirstTag, CollectedAllDataTag])
    , (ReceivedCreditBureauDataFirstTag, [ReceivedCreditBureauDataFirstTag, CollectedAllDataTag])
    , (CollectedAllDataTag, [CollectedAllDataTag])
    ]

  |])

data RiskState (tag :: RiskTag) where
  NoData :: RiskState 'NoDataTag
  CollectedUserData :: UserData -> RiskState 'CollectedUserDataTag
  CollectedLoanDetailsFirst :: UserData -> LoanDetails -> RiskState 'CollectedLoanDetailsFirstTag
  ReceivedCreditBureauDataFirst :: UserData ->  CreditBureauData -> RiskState 'ReceivedCreditBureauDataFirstTag
  CollectedAllData :: UserData -> LoanDetails -> CreditBureauData -> RiskState 'CollectedAllDataTag

riskMachine :: StateMachine RiskTopology RiskState RiskCommand RiskEvent
riskMachine = MkStateMachine
  { initialState = MkInitialState NoData
  , action       = \case
    NoData -> \case
      (RegisterUsedData ud) -> MkActionResult (CollectedUserData ud) (UserDataRegistered ud)
      _ -> MkActionResult NoData NoOp
    CollectedUserData ud -> \case
      RegisterUsedData ud' -> MkActionResult (CollectedUserData ud') (UserDataRegistered ud')
      ProvideLoanDetails ld -> MkActionResult (CollectedLoanDetailsFirst ud ld) (LoanDetailsProvided ld)
      ProvideCreditBureauData cbd -> MkActionResult (ReceivedCreditBureauDataFirst ud cbd) (CreditBureauDataReceived cbd)
    CollectedLoanDetailsFirst ud ld -> \case
      RegisterUsedData ud' -> MkActionResult (CollectedLoanDetailsFirst ud' ld) (UserDataRegistered ud')
      ProvideLoanDetails ld' -> MkActionResult (CollectedLoanDetailsFirst ud ld') (LoanDetailsProvided ld')
      ProvideCreditBureauData cbd -> MkActionResult (CollectedAllData ud ld cbd) (CreditBureauDataReceived cbd)
    ReceivedCreditBureauDataFirst ud cbd -> \case
      RegisterUsedData ud' -> MkActionResult (ReceivedCreditBureauDataFirst ud' cbd) (UserDataRegistered ud')
      ProvideLoanDetails ld -> MkActionResult (CollectedAllData ud ld cbd) (LoanDetailsProvided ld)
      ProvideCreditBureauData cbd' -> MkActionResult (ReceivedCreditBureauDataFirst ud cbd') (CreditBureauDataReceived cbd')
    CollectedAllData ud ld cbd -> \case
      RegisterUsedData ud' -> MkActionResult (CollectedAllData ud' ld cbd) (UserDataRegistered ud')
      ProvideLoanDetails ld' -> MkActionResult (CollectedAllData ud ld' cbd) (LoanDetailsProvided ld')
      ProvideCreditBureauData cbd' -> MkActionResult (CollectedAllData ud ld cbd') (CreditBureauDataReceived cbd')
  }
