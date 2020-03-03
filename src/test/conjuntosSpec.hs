module ConjuntosSpec where

import Test.Hspec
import Conjuntos

main :: IO ()
main = hspec $ do
  describe "pertenece" $ do
    it "devuelve False pertenece cualquier elemento al conjunto vacio" $
      pertenece 1 vacio  `shouldBe` False

    it "devuelve False elemento no pertenece al conjunto" $ do
      let cxs = agregar 2 vacio
      pertenece 1 cxs  `shouldBe` False

    it "devuelve True elemento pertenece al conjunto" $ do
      let cxs = agregar 1 (agregar 2 vacio)
      pertenece 1 cxs  `shouldBe` True

  describe "agregar" $ do
    it "devuelve el conjunto con el elemento como unico elemento al agregar un elemento al conjunto vacio" $
      agregar 1 vacio  `shouldBe` [1]

    it "devuelve el conjunto con el elemento al agregar un elemento a un conjunto no vacio" $ do
      let cxs = agregar 2 vacio
      agregar 1 cxs  `shouldBe` [1, 2]

