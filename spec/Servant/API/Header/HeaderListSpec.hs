module Servant.API.Header.HeaderListSpec where

import Data.Proxy
import Servant.API.Header (Header')
import Servant.API.Modifiers
import Test.Hspec

import Servant.API.Header.HeaderList

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "HeaderList" do

    let emptyHeaders :: HeaderList 'Incoming '[]
        emptyHeaders = HeaderListNil

        exampleHeaders :: HeaderList 'Incoming '[ Header' '[Required, Strict] "Test" Int]
        exampleHeaders = HeaderListCons 5 HeaderListNil

    describe "Show" do

      it "shows an empty HeaderList" do
        show (emptyHeaders :: HeaderList 'Incoming '[])
          `shouldBe` "HeaderListNil"

      it "shows a populated HeaderList" do
        show exampleHeaders `shouldBe` "HeaderListCons 5 HeaderListNil"

    describe "GetHeaders" do

      it "returns the correct list of headers" do

        getHeaders exampleHeaders `shouldBe` [("Test", "5")]

    describe "ConstructHeaders" do

      it "constructs an empty list of headers" do
        constructHeaders (Proxy :: Proxy (HeaderList Incoming '[])) []
          `shouldBe` Right HeaderListNil

      describe "HeaderList 'Incoming '[ Header' '[Required, Strict] \"X-Test-Header\" String ]" do

        it "constructs a populated list of headers" do
          let proxy :: Proxy (HeaderList 'Incoming '[ Header' '[Required, Strict] "X-Test-Header" String ])
              proxy = Proxy

          constructHeaders proxy [("X-Test-Header", "test")]
            `shouldBe` Right (HeaderListCons "test" HeaderListNil)

      describe "HeaderList 'Incoming '[ Header' '[Required, Lenient] \"X-Test-Header\" String ]" do

        it "constructs a populated list of headers" do
          let proxy :: Proxy (HeaderList 'Incoming '[ Header' '[Required, Lenient] "X-Test-Header" String ])
              proxy = Proxy

          constructHeaders proxy [("X-Test-Header", "test")]
            `shouldBe` Right (HeaderListCons (Right "test") HeaderListNil)

      describe "HeaderList 'Incoming '[ Header' '[Required, Lenient] \"X-Test-Header\" Int ]" do

        it "constructs a populated list of headers" do
          let proxy :: Proxy (HeaderList 'Incoming '[ Header' '[Required, Lenient] "X-Test-Header" Int ])
              proxy = Proxy

          constructHeaders proxy [("X-Test-Header", "test")]
            `shouldBe` Right (HeaderListCons (Left "could not parse: `test' (input does not start with a digit)") HeaderListNil)

      describe "HeaderList 'Incoming '[ Header' '[Optional, Strict] \"X-Test-Header\" String ]" do

        it "constructs a populated list of headers" do
          let proxy :: Proxy (HeaderList 'Incoming '[ Header' '[Required, Strict] "X-Test-Header" String ])
              proxy = Proxy

          constructHeaders proxy [("X-Test-Header", "test")]
            `shouldBe` Right (HeaderListCons "test" HeaderListNil)

      describe "HeaderList 'Incoming '[ Header' '[Optional, Lenient] \"X-Test-Header\" String ]" do

        it "constructs a populated list of headers" do
          let proxy :: Proxy (HeaderList 'Incoming '[ Header' '[Required, Lenient] "X-Test-Header" String ])
              proxy = Proxy

          constructHeaders proxy [("X-Test-Header", "test")]
            `shouldBe` Right (HeaderListCons (Right "test") HeaderListNil)

      describe "HeaderList 'Incoming '[ Header' '[Optional, Lenient] \"X-Test-Header\" Int ]" do

        it "constructs a populated list of headers" do
          let proxy :: Proxy (HeaderList 'Incoming '[ Header' '[Required, Lenient] "X-Test-Header" Int ])
              proxy = Proxy

          constructHeaders proxy [("X-Test-Header", "test")]
            `shouldBe` Right (HeaderListCons (Left "could not parse: `test' (input does not start with a digit)") HeaderListNil)
