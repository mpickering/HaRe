{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Refact.Refactoring.Simple(removeBracket) where

import qualified Data.Generics         as SYB

import qualified GHC           as GHC

import qualified Language.Haskell.GhcMod as GM (Options(..))
import Language.Haskell.Refact.API

-- To be moved into HaRe API
import Language.Haskell.GHC.ExactPrint.Transform hiding (liftT)
import Language.Haskell.Refact.Utils.ExactPrint
import Language.Haskell.Refact.Utils.MonadFunctions

import Data.Maybe
-- import Debug.Trace

-- ---------------------------------------------------------------------

-- | Convert an if expression to a case expression
removeBracket :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> SimpPos -> IO [FilePath]
removeBracket settings opts fileName beginPos endPos =
  let applied = (:[]) . fst <$> applyRefac
                  (removeBracketTransform fileName beginPos endPos)
                  (RSFile fileName) in
  runRefacSession settings opts [Left fileName] applied

type HsExpr a = GHC.Located (GHC.HsExpr a)
pattern HsPar l s = GHC.L l (GHC.HsPar s)

removeBracketTransform  :: FilePath -> SimpPos -> SimpPos -> RefactGhc ()
removeBracketTransform fileName beginPos endPos = do
       getModuleGhc fileName
       parsed <- getRefactParsed
       let expr :: GHC.Located (GHC.HsExpr GHC.RdrName)
           expr = fromJust $ locToExp beginPos endPos parsed
           removePar :: HsExpr GHC.RdrName -> RefactGhc (HsExpr GHC.RdrName)
           removePar e@(HsPar _ s)
            | sameOccurrence e expr = do
              startAnns <- liftT $ getAnnsT
              let oldkey = mkKey e
                  newkey = mkKey s
                  newanns = fromMaybe startAnns $ replace oldkey newkey startAnns
              setRefactAnns newanns
              return s
           removePar e = return e
       p2 <- SYB.everywhereM (SYB.mkM removePar) parsed
       (liftT getAnnsT) >>= putRefactParsed p2
       logm $ "logm: after refactor\n" ++ showGhc p2



