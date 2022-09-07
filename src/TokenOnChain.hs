{-# LANGUAGE NumericUnderscores  #-}

module TokenOnChain
    ( tokenPolicy
    , mkTokenPolicy
    , tokenPolicyScript
    , tokenPolicyValidator
    , tokenCurSymbol
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Plutus.V1.Ledger.Scripts        as PlutusScripts
import qualified Plutus.Script.Utils.V1.Scripts as UtilsScripts
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

--------------------------
-- Helper Functions
--------------------------

tokenPolicy :: TxOutRef -> TokenName -> Integer -> UtilsScripts.MintingPolicy
tokenPolicy oref tn amt = PlutusScripts.mkMintingPolicyScript $
    --Converts a custom redeemer from a minting policy function to an untyped minting policy function
    $$(PlutusTx.compile [|| \oref' tn' amt' -> UtilsScripts.mkUntypedMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenPolicyScript :: UtilsScripts.MintingPolicy -> PlutusScripts.Script
tokenPolicyScript = PlutusScripts.unMintingPolicyScript

tokenPolicyValidator :: PlutusScripts.Script -> PlutusScripts.Validator
tokenPolicyValidator = PlutusScripts.Validator

tokenCurSymbol :: UtilsScripts.MintingPolicy -> CurrencySymbol
tokenCurSymbol = UtilsScripts.scriptCurrencySymbol

-- λ> mintingPolicyHash $ tokenPolicy (TxOutRef {txOutRefId = "5cf29590d49121179118302e8dcf43169241aaac3059333ab1f9b51c2ac1dc02", txOutRefIdx = 3}) "test" 4
-- λ> mp = tokenPolicy (TxOutRef {txOutRefId = "5cf29590d49121179118302e8dcf43169241aaac3059333ab1f9b51c2ac1dc02", txOutRefIdx = 3}) "test" 4
-- λ> tpValidator = tokenPolicyValidator $ tokenPolicyScript mp
