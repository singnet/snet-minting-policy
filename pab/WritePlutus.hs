import Cardano.Api
import Cardano.Api.Shelley
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
-- import OnChain.LockingScript
--   ( apiExampleUntypedPlutusLockingScript,
--     untypedLockingScriptAsShortBs,
--   )
-- import OnChain.MintingScript (apiExamplePlutusMintingScript, curSymbol)
-- import OnChain.SimpleMintingScript (serialisedScript)
import OnChain.PrivateToken (serialisedScript, curSymbol)
import qualified Plutus.V1.Ledger.Api as Plutus
import System.Environment
import Prelude

writePlutusScript :: FilePath -> IO ()
writePlutusScript filename = do
  result <- writeFileTextEnvelope filename Nothing serialisedScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

main :: IO ()
main = do
  args <- getArgs
  let argsLen = length args
  let filename = if argsLen > 0 then head args else "private-token.plutus"
  putStrLn $ "Writing output to " ++ filename
  putStrLn $ "Currency Symbol " <> show curSymbol <> "."
  writePlutusScript filename
  putStrLn "Successfully written"