module Device.ConcreteSpec where

import Test.Hspec
import Schedule.Core
import Device.Concrete
import Control.Concurrent
    
spec :: Spec
spec = describe "Concrete device" $
    it "Should start and then die" $ do
        device <- newDevice "http://foo.bar.baz"
        threadDelay 20000000
        stopDevice device