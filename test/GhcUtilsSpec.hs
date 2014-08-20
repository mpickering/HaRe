{-# LANGUAGE ScopedTypeVariables #-}
module GhcUtilsSpec (main, spec) where

import           Test.Hspec

import           TestUtils

import qualified GHC     as GHC
import qualified NameSet as GHC
import qualified SrcLoc  as GHC

import qualified Data.Generics as SYB
import qualified GHC.SYB.Utils as SYB

import Data.Maybe

import Language.Haskell.Refact.Utils.Binds
import Language.Haskell.Refact.Utils.GhcVersionSpecific
import Language.Haskell.Refact.Utils.GhcUtils
import Language.Haskell.Refact.Utils.MonadFunctions
import Language.Haskell.Refact.Utils.TypeUtils
import Language.Haskell.TokenUtils.GHC.Layout

-- import TestUtils

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do

  describe "onelayerStaged" $ do
    it "only descends one layer into a structure" $ do
      -- let s = ([2,1,3,4,5],[6,7,8]) :: ([Int],[Int])
      let s' = (2,[3,4],5) :: (Int,[Int],Int)
      let -- worker (i :: Int)
          --  | i == 2 = ["f"]
          -- worker _ = []

          worker' (i::Int) = [i]
          -- worker'' (i::[Int]) = [head i]

      let g = onelayerStaged SYB.Renamer [] ([] `SYB.mkQ` worker') s'
      let g1 = SYB.gmapQ ([] `SYB.mkQ` worker') s'
      let g2 = SYB.gmapQl (++) [] ([] `SYB.mkQ` worker') s'

      (show g) `shouldBe` "[[2],[],[5]]"
      (show g1) `shouldBe` "[[2],[],[5]]"
      (show g2) `shouldBe` "[2,5]"

    -- ---------------------------------

    it "avoids GHC holes in the structure" $ do
      let s' = ([3,4],error "blowup",5,error "ptc",6,error "fix",7)
                :: ([Int],GHC.NameSet,Int,GHC.PostTcType,Int,GHC.Fixity,Int)
      let -- worker (i :: Int)
          --  | i == 2 = ["f"]
          -- worker _ = []

          worker' (i::Int) = [i]
          -- worker'' (i::[Int]) = [head i]

      let g = onelayerStaged SYB.Renamer [-1] ([-10] `SYB.mkQ` worker') s'
      let g1 = SYB.gmapQ ([-2] `SYB.mkQ` worker') s'
      let g2 = SYB.gmapQl (++) [-3] ([-30] `SYB.mkQ` worker') s'

      (show g) `shouldBe` "[[-10],[-10],[5],[-1],[6],[-10],[7]]"
      (show g1) `shouldBe` "[[-2],[-2],[5],[-2],[6],[-2],[7]]"
      (show g2) `shouldBe` "[-3,-30,-30,5,-30,6,-30,7]"

    -- ---------------------------------

    it "Finds a GHC.Name at top level only" $ do
      let
        comp = do
         (t, toks) <- parseSourceFileTest "./test/testdata/DupDef/Dd1.hs"
         putParsedModule t toks
         renamed <- getRefactRenamed

         let mn = locToName (4,1) renamed
         let (Just (ln@(GHC.L _ n))) = mn

         let mx = locToName (4,10) renamed
         let (Just (lx@(GHC.L _ x))) = mx

         let declsr = hsBinds renamed
             duplicatedDecls = definingDeclsNames [n] declsr True False

             res = findEntity ln duplicatedDecls
             res2 = findEntity n duplicatedDecls

             resx = findEntity lx duplicatedDecls
             resx2 = findEntity x duplicatedDecls


             worker (nn::GHC.Name) = [showGhc nn]
             g = onelayerStaged SYB.Renamer ["-1"] (["-10"] `SYB.mkQ` worker) duplicatedDecls

             worker2 ((GHC.L _ (GHC.FunBind (GHC.L _ n') _ _ _ _ _))::GHC.Located (GHC.HsBind GHC.Name))
               | n == n' = ["found"]
             worker2 _ = []
             g2 = onelayerStaged SYB.Renamer ["-1"] (["-10"] `SYB.mkQ` worker2) duplicatedDecls

         return (res,res2,resx,resx2,duplicatedDecls,g,g2,ln,lx)
      ((r,r2,rx,rx2,d,gg,gg2,_l,_x),_s) <- runRefactGhcState comp
      -- (SYB.showData SYB.Renamer 0 d) `shouldBe` ""

      (showGhc d) `shouldBe` "[DupDef.Dd1.toplevel x = DupDef.Dd1.c GHC.Num.* x]"
      (showGhc _l) `shouldBe` "DupDef.Dd1.toplevel"
      (showGhc _x) `shouldBe` "x"
      (show gg) `shouldBe` "[[\"-10\"],[\"-10\"]]"
      (show gg2) `shouldBe` "[[\"found\"],[\"-10\"]]"
      r `shouldBe` True
      r2 `shouldBe` True
      rx `shouldBe` False
      rx2 `shouldBe` True


  -- -----------------------------------

  describe "avoid landmines syb" $ do
    it "avoids landmines Kind" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Landmines/MineKind.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let renamed = fromJust $ GHC.tm_renamed_source t
      let typechecked = GHC.tm_typechecked_source t

      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""
      -- (SYB.showData SYB.TypeChecker 0 typechecked) `shouldBe` ""

      let
        gqs stage ast = SYB.everythingStaged stage (++) [] ([] `SYB.mkQ` worker) ast
        gq        ast = SYB.everything             (++)    ([] `SYB.mkQ` worker) ast

        worker (s@(GHC.RealSrcSpan _)) = [s]
        worker _ = []

      (length $ gqs SYB.Parser      parsed     ) `shouldBe` 46
      (length $ gqs SYB.Renamer     renamed    ) `shouldBe` 42
      (length $ gqs SYB.TypeChecker typechecked) `shouldBe` 0

      (putStrLn $ show $ length $ gq parsed     ) `shouldThrow` anyException
      (putStrLn $ show $ length $ gq renamed    ) `shouldThrow` anyException
      (show $ length $ gq typechecked) `shouldBe` "0"

    -- ---------------------------------

    it "avoids landmines Type" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Landmines/MineType.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let renamed = fromJust $ GHC.tm_renamed_source t
      let typechecked = GHC.tm_typechecked_source t

      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""
      -- (SYB.showData SYB.TypeChecker 0 typechecked) `shouldBe` ""

      let
        gqs stage ast = SYB.everythingStaged stage (++) [] ([] `SYB.mkQ` worker) ast
        gq        ast = SYB.everything             (++)    ([] `SYB.mkQ` worker) ast

        worker (s@(GHC.RealSrcSpan _)) = [s]
        worker _ = []

      (length $ gqs SYB.Parser      parsed     ) `shouldBe` 7
      (length $ gqs SYB.Renamer     renamed    ) `shouldBe` 7
      (length $ gqs SYB.TypeChecker typechecked) `shouldBe` 6

      (putStrLn $ show $ length $ gq parsed     ) `shouldThrow` anyException
      (putStrLn $ show $ length $ gq renamed    ) `shouldThrow` anyException
      (show $ length $ gq typechecked) `shouldBe` "6"

    -- ---------------------------------

    it "avoids landmines Fixity" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Landmines/MineFixity.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let renamed = fromJust $ GHC.tm_renamed_source t
      let typechecked = GHC.tm_typechecked_source t

      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""
      -- (SYB.showData SYB.TypeChecker 0 typechecked) `shouldBe` ""

      let
        gqs stage ast = SYB.everythingStaged stage (++) [] ([] `SYB.mkQ` worker) ast
        gq        ast = SYB.everything             (++)    ([] `SYB.mkQ` worker) ast

        worker (s@(GHC.RealSrcSpan _)) = [s]
        worker _ = []

      (length $ gqs SYB.Parser      parsed     ) `shouldBe` 9
      (length $ gqs SYB.Renamer     renamed    ) `shouldBe` 9
      (length $ gqs SYB.TypeChecker typechecked) `shouldBe` 6

      (putStrLn $ show $ length $ gq parsed     ) `shouldThrow` anyException
      (putStrLn $ show $ length $ gq renamed    ) `shouldThrow` anyException
      (show $ length $ gq typechecked) `shouldBe` "6"

    -- ---------------------------------

    it "avoids landmines NameSet" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Landmines/MineNames.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let renamed = fromJust $ GHC.tm_renamed_source t
      let typechecked = GHC.tm_typechecked_source t

      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""
      -- (SYB.showData SYB.TypeChecker 0 typechecked) `shouldBe` ""

      let
        gqs stage ast = SYB.everythingStaged stage (++) [] ([] `SYB.mkQ` worker) ast
        gq        ast = SYB.everything             (++)    ([] `SYB.mkQ` worker) ast

        worker (s@(GHC.RealSrcSpan _)) = [s]
        worker _ = []

      (length $ gqs SYB.Parser      parsed     ) `shouldBe` 11
      (length $ gqs SYB.Renamer     renamed    ) `shouldBe` 10
      (length $ gqs SYB.TypeChecker typechecked) `shouldBe` 6

      (putStrLn $ show $ length $ gq parsed     ) `shouldThrow` anyException
      (putStrLn $ show $ length $ gq renamed    ) `shouldThrow` anyException
      (show $ length $ gq typechecked) `shouldBe` "6"

-- ---------------------------------------------------------------------
