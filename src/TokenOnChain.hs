{-# LANGUAGE NumericUnderscores  #-}

module TokenOnChain
    ( tokenPolicy
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Plutus.V1.Ledger.Scripts        as PlutusScripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as UtilsScripts
import           Ledger.Value                as Value

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == amt
        _                -> False

tokenPolicy :: TxOutRef -> TokenName -> Integer -> UtilsScripts.MintingPolicy
tokenPolicy oref tn amt = PlutusScripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> UtilsScripts.mkUntypedMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

--tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
--tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn
