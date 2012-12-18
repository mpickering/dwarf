import Control.Monad (void)
import Distribution.Simple
import System.Cmd (system)

main = defaultMainWithHooks $ simpleUserHooks { runTests = runElfTests }

runElfTests a b pd lb = void $ system "runhaskell -i./src ./tests/Test.hs"
