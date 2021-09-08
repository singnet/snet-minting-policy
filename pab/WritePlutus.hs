import Cardano.Api
import Cardano.Api.Shelley
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
-- import OnChain.LockingScript
--   ( apiExampleUntypedPlutusLockingScript,
--     untypedLockingScriptAsShortBs,
--   )
import OnChain.MintingScript (apiExamplePlutusMintingScript)
import qualified Plutus.V1.Ledger.Api as Plutus
import Prelude
import System.Environment 

writePlutusScript :: FilePath -> IO ()
writePlutusScript filename = do
  result <- writeFileTextEnvelope filename Nothing apiExamplePlutusMintingScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

main :: IO ()
main = do
  args <- getArgs
  let argsLen = length args
  let filename = if argsLen > 0 then head args else "result.plutus"
  putStrLn $ "Writing output to " ++ filename
  writePlutusScript filename