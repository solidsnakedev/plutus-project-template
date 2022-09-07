module Compile where

import Prelude
import qualified Plutus.V1.Ledger.Scripts as LedgerScripts
import qualified Plutus.Script.Utils.V1.Scripts as UtilsScripts
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short  as SBS
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Cardano.Api as CardanoAPI
import           PlutusTx              
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import Ledger.Tx.CardanoAPI (toCardanoScriptData, toCardanoScriptHash)
import Data.Text 

--dataToScriptData :: Data -> ScriptData
--dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
--dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
--dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
--dataToScriptData (I n)         = ScriptDataNumber n
--dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
--writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData
writeJSON file = LBS.writeFile file .
                 encode . 
                 scriptDataToJson ScriptDataJsonDetailedSchema .
                 toCardanoScriptData .
                 PlutusTx.toBuiltinData

writeUnit :: IO ()
writeUnit = do
    exist <- doesDirectoryExist "output"
    createDirectoryIfMissing exist "output"
    writeJSON "output/unit.json" ()
    putStrLn "unit value saved in -> output/unit.json"

writeValidator :: FilePath -> LedgerScripts.Validator -> IO (Either (FileError ()) ())
writeValidator file validator = CardanoAPI.writeFileTextEnvelope file Nothing scriptSerialised
    where
        script :: LedgerScripts.Script
        script = LedgerScripts.unValidatorScript validator

        scriptShortBS :: SBS.ShortByteString
        scriptShortBS = SBS.toShort . LBS.toStrict $ serialise script
        
        scriptSerialised :: PlutusScript PlutusScriptV1
        scriptSerialised = PlutusScriptSerialised scriptShortBS

testnet_id :: CardanoAPI.NetworkId
testnet_id = Testnet $ NetworkMagic (fromInteger 1)

mkShelleyAddr :: CardanoAPI.PaymentCredential -> CardanoAPI.StakeAddressReference -> CardanoAPI.Address ShelleyAddr
mkShelleyAddr paymentCredential stakeAddressReference = makeShelleyAddress testnet_id paymentCredential stakeAddressReference

toScriptHash :: LedgerScripts.Validator -> CardanoAPI.ScriptHash
toScriptHash validator = 
    case toCardanoScriptHash validatorHash of
        Left e -> error (show e)
        Right scriptHash -> scriptHash
    where
        validatorHash = UtilsScripts.validatorHash validator

toBech32Addr :: LedgerScripts.Validator -> CardanoAPI.NetworkId -> Text
toBech32Addr validator testnetId = serialiseToBech32 shelleyAddress
    where
        shelleyAddress = makeShelleyAddress testnetId paymentCredentialByScript CardanoAPI.NoStakeAddress
        paymentCredentialByScript = CardanoAPI.PaymentCredentialByScript $ toScriptHash validator