module Plugin.Debug.HUDSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  describe "Boolean calculation test for hspec usage check" $ do
    it "should be '0 == 0'" $
      "0" `shouldBe` "0"

    it "should not be '0 == 1'" $
      "0" `shouldNotBe` "1"
