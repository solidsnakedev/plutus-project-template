module ContractError (runTrace) where


import Prelude
import Data.Text (Text)
import Control.Monad
import Plutus.Contract  as Contract
import Plutus.Trace.Emulator as PlutusTrace
import Wallet.Emulator.Wallet as WalletEmulator


runTrace :: IO ()
runTrace = runEmulatorTraceIO $ void $ PlutusTrace.activateContractWallet (knownWallet 1) contract

contract :: Contract () Empty Text ()
contract = do

    void $ Contract.throwError @Text "This is an error"