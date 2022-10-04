module OffChainGift where

------------------
-- Non Plutus dependencies
------------------
import           Prelude                (Semigroup (..), Show (..), IO, String)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Control.Monad (void)
import qualified Data.Map               as Map
import           Text.Printf            (printf)
import qualified Control.Monad.Freer.Extras as Extras

------------------
-- Plutus dependencies
------------------
import           PlutusTx.Prelude hiding (Semigroup (..))
import qualified PlutusTx.Builtins
import           Plutus.V1.Ledger.Api (
  TxOutRef (..),
  Redeemer(..),
  Datum(..)
  )
import           Ledger (ChainIndexTxOut (..))
import           Plutus.Contract        as Contract -- disable qualified to import .\/ operator
import qualified Plutus.Trace           as Trace
import           Plutus.Trace (EmulatorTrace)
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Tx
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.Ada          as Ada

------------------
-- Project dependencies
------------------
import qualified OnChainGift

----------------------------
---- Off Chain Code
----------------------------
--
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = Constraints.mustPayToOtherScript OnChainGift.validatorHash (Datum $ PlutusTx.Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- Contract.submitTx tx
    void $ Contract.awaitTxConfirmed $ Ledger.Tx.getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- Contract.utxosAt OnChainGift.scriptAddress
    let orefs   = fst <$> Map.toList utxos
        chainIndex = snd <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.plutusV1OtherScript OnChainGift.validator
        tx :: Constraints.TxConstraints Void Void
        tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.Builtins.mkI 17 | oref <- orefs]

    Contract.logInfo @String $ "Beginning log TxOutRef"
    sequence_ $ (logInfo @String . show) <$> orefs 
    sequence_ $ logInfo @TxOutRef <$> orefs 
    Contract.logInfo @String $ "End log TxOutRef"

    Contract.logInfo @String $ "Beginning log chainIndex"
    sequence_ $ (logInfo @String . show) <$> chainIndex 
    sequence_ $ logInfo @ChainIndexTxOut <$> chainIndex 
    Contract.logInfo @String $ "End log chainIndex"

    ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    void $ Contract.awaitTxConfirmed $ Ledger.Tx.getCardanoTxId ledgerTx
    Contract.logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = Contract.awaitPromise (give' `select` grab') >> endpoints
  where
    give' = Contract.endpoint @"give" give
    grab' = Contract.endpoint @"grab" $ const grab

--------------------------
-- Emulator Trace
--------------------------
myTrace :: EmulatorTrace()
myTrace = do
    handle1 <- Trace.activateContractWallet (Wallet.knownWallet 1) endpoints
    handle2 <- Trace.activateContractWallet (Wallet.knownWallet 2) endpoints
    Trace.callEndpoint @"give" handle1 10000000
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"grab" handle2 ()
    s <- Trace.waitNSlots 2
    Extras.logDebug $ "reached -> " <> show s

runEmulator :: IO ()
runEmulator = Trace.runEmulatorTraceIO myTrace
