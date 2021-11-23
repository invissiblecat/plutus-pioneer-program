module Test where 

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

test :: IO()
test = runEmulatorTraceIO testTrace

testTrace :: EmulatorTrace()
testTrace = do 
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams 
        {  mpTokenName = "myToken"
        ,  mpAmount = 517
        }
    void $ waitUntilSlot 20