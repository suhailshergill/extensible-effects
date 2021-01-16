import Test.Framework (defaultMain, Test)

import qualified Control.Eff.Test
import qualified Control.Eff.Coroutine.Test
import qualified Control.Eff.Example.Test
import qualified Control.Eff.Example.Fresh.Test
import qualified Control.Eff.Exception.Test
import qualified Control.Eff.Logic.NDet.Test
import qualified Control.Eff.Operational.Test
import qualified Control.Eff.Reader.Lazy.Test
import qualified Control.Eff.Reader.Strict.Test
import qualified Control.Eff.State.Lazy.Test
import qualified Control.Eff.State.OnDemand.Test
import qualified Control.Eff.State.Strict.Test
import qualified Control.Eff.Trace.Test
import qualified Control.Eff.Writer.Lazy.Test
import qualified Control.Eff.Writer.Strict.Test
import qualified Control.Eff.Scope
import DoctestRun (runDocTest)

main :: IO ()
main = do
  runDocTest
  defaultMain testGroups

testGroups :: [Test]
testGroups = []
             ++ Control.Eff.Test.testGroups
             ++ Control.Eff.Coroutine.Test.testGroups
             ++ Control.Eff.Example.Test.testGroups
             ++ Control.Eff.Example.Fresh.Test.testGroups
             ++ Control.Eff.Exception.Test.testGroups
             ++ Control.Eff.Logic.NDet.Test.testGroups
             ++ Control.Eff.Operational.Test.testGroups
             ++ Control.Eff.Reader.Lazy.Test.testGroups
             ++ Control.Eff.Reader.Strict.Test.testGroups
             ++ Control.Eff.State.Lazy.Test.testGroups
             ++ Control.Eff.State.OnDemand.Test.testGroups
             ++ Control.Eff.State.Strict.Test.testGroups
             ++ Control.Eff.Trace.Test.testGroups
             ++ Control.Eff.Writer.Lazy.Test.testGroups
             ++ Control.Eff.Writer.Strict.Test.testGroups
             ++ Control.Eff.Scope.testGroups
