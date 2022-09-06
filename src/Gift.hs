module Gift (validator, valHash, scrAddress, runEmulator)
where

import           Prelude               hiding (($), (<$>))
import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)

import           Plutus.Contract    as Contract
import           PlutusTx           as PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    as PlutusTxPrelude hiding (Semigroup(..), unless, (.))

import           Ledger              as Ledger hiding (singleton)
import qualified Ledger.Constraints  as Constraints

import qualified Plutus.Script.Utils.V1.Scripts as UtilsScripts
import qualified Plutus.V1.Ledger.Scripts as LedgerScripts
import qualified Plutus.V1.Ledger.Api as LedgerApi
import           Ledger.Ada          as Ada

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Text.Printf         (printf)

import Plutus.Trace as PlutusTrace
import Wallet.Emulator as WalletEmulator
import qualified Control.Monad.Freer.Extras as Extras
import Data.Default
import qualified Data.Aeson as DataAeson


{-# OPTIONS_GHC -fno-warn-unused-imports #-}
--------------------------
-- On Chain Code
--------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
--mkValidator _ _ _ = ()
mkValidator _ _ _ = trace "Hello from on chain code"()

--------------------------
-- Helper Functions
--------------------------

validator :: LedgerScripts.Validator
validator = LedgerScripts.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: UtilsScripts.ValidatorHash
valHash = UtilsScripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptAddress validator

--------------------------
-- Off Chain Code - PAB
--------------------------

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = Constraints.mustPayToOtherScript valHash (LedgerApi.Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- Contract.submitTx tx
    void $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    --void $ Contract.toContract $ Contract.throwError @Text "" 
    Contract.logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- Contract.utxosAt scrAddress
    let orefs   = PlutusTxPrelude.fst <$> Map.toList utxos
        chainIndex = PlutusTxPrelude.snd <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: Constraints.TxConstraints Void Void
        tx      = PlutusTxPrelude.mconcat [Constraints.mustSpendScriptOutput oref $ LedgerApi.Redeemer $ Builtins.mkI 17 | oref <- orefs]

    Contract.logInfo @String $ "Beginning log TxOutRef"
    PlutusTxPrelude.sequence_ $ (logInfo @String . show) <$> orefs 
    PlutusTxPrelude.sequence_ $ logInfo @TxOutRef <$> orefs 
    Contract.logInfo @String $ "End log TxOutRef"

    Contract.logInfo @String $ "Beginning log chainIndex"
    PlutusTxPrelude.sequence_ $ (logInfo @String . show) <$> chainIndex 
    PlutusTxPrelude.sequence_ $ logInfo @ChainIndexTxOut <$> chainIndex 
    Contract.logInfo @String $ "End log chainIndex"

    ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    void $ Contract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    Contract.logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = Contract.awaitPromise (give' `select` grab') >> endpoints
  where
    give' = Contract.endpoint @"give" give
    grab' = Contract.endpoint @"grab" $ PlutusTxPrelude.const grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []

--------------------------
-- Emulator Trace
--------------------------

runEmulator = runEmulatorTraceIO' def def myTrace

myTrace = do
    handle1 <- PlutusTrace.activateContractWallet (WalletEmulator.knownWallet 1) endpoints
    handle2 <- PlutusTrace.activateContractWallet (WalletEmulator.knownWallet 2) endpoints
    callEndpoint @"give" handle1 10000000
    void $ PlutusTrace.waitUntilSlot 2
    PlutusTrace.callEndpoint @"grab" handle1 ()
    s <- PlutusTrace.waitNSlots 2
    Extras.logDebug $ "reached -> " <> show s
