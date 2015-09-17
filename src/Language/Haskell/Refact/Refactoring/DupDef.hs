{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.Refact.Refactoring.DupDef(duplicateDef) where

import qualified Data.Generics as SYB
import qualified GHC.SYB.Utils as SYB

import qualified GHC
import qualified RdrName               as GHC

import Data.List
import Data.Maybe

import qualified Language.Haskell.GhcMod as GM (Options(..))
import Language.Haskell.GhcMod.Internal as GM (mpPath)
import Language.Haskell.Refact.API

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Transform hiding (liftT)

-- ---------------------------------------------------------------------
-- | This refactoring duplicates a definition (function binding or
-- simple pattern binding) at the same level with a new name provided by
-- the user. The new name should not cause name clash/capture.
duplicateDef :: RefactSettings -> GM.Options -> FilePath -> String -> SimpPos -> IO [FilePath]
duplicateDef settings opts fileName newName (row,col) =
  runRefacSession settings opts [Left fileName] (comp fileName newName (row,col))

comp :: FilePath -> String -> SimpPos
     -> RefactGhc [ApplyRefacResult]
comp fileName newName (row, col) = do
  if isVarId newName
    then
      do
        getModuleGhc fileName
        renamed <- getRefactRenamed
        parsed  <- getRefactParsed
        targetModule <- getRefactTargetModule
        logm $ "DupDef.comp:got targetModule"

        let (Just (modName,_)) = getModuleName parsed
        let maybePn = locToName (row, col) renamed
        case maybePn of
          Just pn ->
            do
              logm $ "DupDef.comp:about to applyRefac for:pn=" ++ SYB.showData SYB.Parser 0 pn
              (refactoredMod@((_fp,_ismod),(_anns',_parsed')),(isDone,nn)) <- applyRefac (doDuplicating pn newName) (RSFile fileName)
              logm $ "DupDef.com:isDone=" ++ show isDone
              case isDone of
                DupDefFailed -> error "The selected identifier is not a function/simple pattern name, or is not defined in this module "
                DupDefLowerLevel -> return [refactoredMod]
                DupDefTopLevel -> do
                  if modIsExported modName renamed
                   then
                    do
                       logm $ "DupDef.comp:about to clientMods"
                       clients <- clientModsAndFiles targetModule
                       logm ("DupDef: clients=" ++ (showGhc clients)) -- ++AZ++ debug
                       refactoredClients <- mapM (refactorInClientMod (GHC.unLoc pn) modName nn)
                                                 clients
                       return $ refactoredMod:refactoredClients
                   else return [refactoredMod]
          Nothing -> error "Invalid cursor position!"
    else error $ "Invalid new function name:" ++ newName ++ "!"


data DupDefResult = DupDefFailed | DupDefTopLevel | DupDefLowerLevel
                  deriving (Eq,Show)

doDuplicating :: GHC.Located GHC.Name -> String
              -> RefactGhc (DupDefResult,GHC.Name)
doDuplicating pn newName = do
   logm $ "DupDef.comp:doDuplicating entered"
   inscopes <- getRefactInscopes
   logm $ "DupDef.comp:doDuplicating got inscopes"
   reallyDoDuplicating pn newName inscopes


reallyDoDuplicating :: GHC.Located GHC.Name -> String
              -> InScopes
              -> RefactGhc (DupDefResult,GHC.Name)
reallyDoDuplicating pn newName _inscopes = do
   clearRefactDone
   parsed <- getRefactParsed
   newNameGhc <- mkNewGhcName Nothing newName

   -- Check if it is a top level dup
   parsed2 <- dupInModule newNameGhc parsed
   d <- getRefactDone
   if d
     then do
       putRefactParsed parsed2 emptyAnns
       return (DupDefTopLevel,newNameGhc)
     else do
       parsed' <- SYB.everywhereMStaged SYB.Parser (
                                      SYB.mkM    (dupInMatch newNameGhc)
                                      `SYB.extM` (dupInPat newNameGhc)
                                      `SYB.extM` (dupInLet newNameGhc)
                                      `SYB.extM` (dupInLetStmt newNameGhc)
                                     ) parsed2
       putRefactParsed parsed' emptyAnns
       done <- getRefactDone
       if done then return (DupDefLowerLevel,newNameGhc)
               else return (DupDefFailed,newNameGhc)

        where
        --1. The definition to be duplicated is at top level.
        -- dupInMod :: (GHC.HsGroup GHC.Name)-> RefactGhc (GHC.HsGroup GHC.Name)
        -- dupInMod (grp :: (GHC.HsGroup GHC.Name))
        --   | not $ emptyList (findFunOrPatBind pn (hsBinds grp)) = doDuplicating' inscopes grp pn
        -- dupInMod grp = return grp
        dupInModule :: GHC.Name -> GHC.ParsedSource -> RefactGhc GHC.ParsedSource
        dupInModule newNameGhc p
          = do
              declsp <- liftT $ hsDecls p
              nm <- getRefactNameMap
              if not $ emptyList (findFunOrPatBind nm pn declsp)
                then doDuplicating' newNameGhc p pn
                else return p

        --2. The definition to be duplicated is a local declaration in a match
        dupInMatch newNameGhc (match::GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
          = do
              nm <- getRefactNameMap
              -- declsp <- liftT $ hsDecls rhs
              declsp <- liftT $ hsDecls match
              logm $ "dupInMatch:declsp=" ++ showGhc declsp
              if not $ emptyList (findFunOrPatBind nm pn declsp)
                then doDuplicating' newNameGhc match pn
                else return match

        --3. The definition to be duplicated is a local declaration in a pattern binding
        dupInPat newNameGhc (pat@(GHC.L _ (GHC.ValD (GHC.PatBind _p _rhs _typ _fvs _))) :: GHC.LHsDecl GHC.RdrName)
          = doDuplicating' newNameGhc pat pn
        --   | not $ emptyList (findFunOrPatBind pn (hsBinds rhs)) = doDuplicating' inscopes pat pn
        dupInPat _ pat = return pat

        --4: The defintion to be duplicated is a local decl in a Let expression
        dupInLet newNameGhc (letExp@(GHC.L _ (GHC.HsLet _ds _e)):: GHC.LHsExpr GHC.RdrName)
          = doDuplicating' newNameGhc letExp pn
        --   | not $ emptyList (findFunOrPatBind pn (hsBinds ds)) = doDuplicating' inscopes letExp pn
        dupInLet _ letExp = return letExp

        --5. The definition to be duplicated is a local decl in a case alternative.
        -- Note: The local declarations in a case alternative are covered in #2 above.

        --6.The definition to be duplicated is a local decl in a Let statement.
        dupInLetStmt newNameGhc (letStmt@(GHC.L _ (GHC.LetStmt _ds)):: GHC.LStmt GHC.RdrName (GHC.LHsExpr GHC.RdrName))
          = doDuplicating' newNameGhc letStmt pn
           -- was |findFunOrPatBind pn ds /=[]=doDuplicating' inscps letStmt pn
        --    |not $ emptyList (findFunOrPatBind pn (hsBinds ds)) = doDuplicating' inscopes letStmt pn
        dupInLetStmt _ letStmt = return letStmt


        findFunOrPatBind nm (GHC.L _ n) ds
          = filter (\d->isFunBindP d || isSimplePatDecl d) $ definingDeclsRdrNames nm [n] ds True False


        doDuplicating' :: (HasDecls t) => GHC.Name -> t -> GHC.Located GHC.Name -> RefactGhc t
        doDuplicating' newNameGhc t _ln = do
          logm $ "doDuplicating' entered"
          -- logm $ "doDuplicating' entered:t=" ++ SYB.showData SYB.Parser 0 t
          declsp <- liftT $ hsDecls t
          nm <- getRefactNameMap
          if not $ emptyList (findFunOrPatBind nm pn declsp)
            then doDuplicating'' newNameGhc t pn
            else return t


        doDuplicating'' :: (HasDecls t) => GHC.Name -> t -> GHC.Located GHC.Name
                       -> RefactGhc t
        doDuplicating'' newNameGhc parentr ln@(GHC.L _ n)
           = do
                logm $ "doDuplicating'' entered:ln" ++ showGhc ln
                declsp <- liftT $ hsDecls parentr
                nm <- getRefactNameMap
                let
                    -- declsr = hsBinds parentr

                    duplicatedDecls = definingDeclsRdrNames nm [n] declsp True False
                    -- (after,before)  = break (definesP pn) (reverse declsp)

                logm $ "doDuplicating'':duplicatedDecls=" ++ showGhc duplicatedDecls
                (f,d) <- hsFDNamesFromInsideRdr parentr
                    --f: names that might be shadowd by the new name,
                    --d: names that might clash with the new name

                logm $ "doDuplicating'':(f,d)=" ++ show (f,d)
                dv <- hsVisiblePNsRdr nm ln declsp --dv: names may shadow new name
                let vars        = nub (f `union` d `union` map showGhc dv)

                -- newNameGhc <- mkNewGhcName Nothing newName
                -- TODO: Where definition is of form tup@(h,t), test each element of it for clashes, or disallow
                nameAlreadyInScope <- isInScopeAndUnqualifiedGhc newName Nothing

                -- logm ("DupDef: nameAlreadyInScope =" ++ (show nameAlreadyInScope)) -- ++AZ++ debug
                -- logm ("DupDef: ln =" ++ (show ln)) -- ++AZ++ debug

                if elem newName vars || (nameAlreadyInScope && findEntity ln duplicatedDecls)
                   then error ("The new name'"++newName++"' will cause name clash/capture or ambiguity problem after "
                               ++ "duplicating, please select another name!")
                   else do
                           setRefactDone
                           newdecls <- duplicateDecl declsp n newNameGhc
                           -- logm $ "doDuplicating'':newdecls=" ++ showGhc newdecls
                           parentr' <- liftT $ replaceDecls parentr newdecls
                           return parentr'


-- | Do refactoring in the client module. That is to hide the
-- identifer in the import declaration if it will cause any problem in
-- the client module.
refactorInClientMod :: GHC.Name -> GHC.ModuleName -> GHC.Name -> TargetModule
                    -> RefactGhc ApplyRefacResult
refactorInClientMod oldPN serverModName newPName targetModule
  = do
       logm ("refactorInClientMod: (oldPN,serverModName,newPName)=" ++ (showGhc (oldPN,serverModName,newPName))) -- ++AZ++ debug
       -- void $ activateModule targetModule
       getTargetGhc targetModule

       -- let fileName = gfromJust "refactorInClientMod" $ GHC.ml_hs_file $ GHC.ms_location modSummary
       let fileName = GM.mpPath targetModule
{-
       -- modInfo@(t,ts) <- getModuleGhc fileName
       getModuleGhc fileName
-}
       renamed <- getRefactRenamed
       parsed <- getRefactParsed

       let modNames = willBeUnQualImportedBy serverModName renamed
       logm ("refactorInClientMod: (modNames)=" ++ (showGhc (modNames))) -- ++AZ++ debug

       -- if isJust modNames && needToBeHided (pNtoName newPName) exps parsed
       mustHide <- needToBeHided newPName renamed parsed
       logm ("refactorInClientMod: (mustHide)=" ++ (showGhc (mustHide))) -- ++AZ++ debug
       if isJust modNames && mustHide
        then do
                -- refactoredMod <- applyRefac (doDuplicatingClient serverModName [newPName]) (Just modInfo) fileName
                (refactoredMod,_) <- applyRefac (doDuplicatingClient serverModName [newPName]) (RSFile fileName)
                return refactoredMod
        else return ((fileName,RefacUnmodifed),(emptyAnns,parsed))
   where
     needToBeHided :: GHC.Name -> GHC.RenamedSource -> GHC.ParsedSource -> RefactGhc Bool
     needToBeHided name exps parsed = do
         let usedUnqual = usedWithoutQualR name parsed
         logm ("refactorInClientMod: (usedUnqual)=" ++ (showGhc (usedUnqual))) -- ++AZ++ debug
         return $ usedUnqual || causeNameClashInExports oldPN name serverModName exps

doDuplicatingClient :: GHC.ModuleName -> [GHC.Name]
              -> RefactGhc ()
doDuplicatingClient serverModName newPNames = do
  logm $ "doDuplicatingClient:newPNames=" ++ showGhc newPNames
  parsed  <- getRefactParsed
  parsed' <- addHiding serverModName parsed (map GHC.nameRdrName newPNames)
  putRefactParsed parsed' emptyAnns
  return ()



--Check here:
-- | get the module name or alias name by which the duplicated
-- definition will be imported automatically.
willBeUnQualImportedBy :: GHC.ModuleName -> GHC.RenamedSource -> Maybe [GHC.ModuleName]
willBeUnQualImportedBy modName (_,imps,_,_)
   = let
         ms = filter (\(GHC.L _ (GHC.ImportDecl _ (GHC.L _ modName1) _qualify _source _safe isQualified _isImplicit _as h))
                    -> modName == modName1
                       && not isQualified
                              && (isNothing h  -- not hiding
                                  ||
                                   (isJust h && ((fst (gfromJust "willBeUnQualImportedBy" h))==True))
                                  ))
                      imps
         in if (emptyList ms) then Nothing
                      else Just $ nub $ map getModName ms

         where getModName (GHC.L _ (GHC.ImportDecl _ _modName1 _qualify _source _safe _isQualified _isImplicit as _h))
                 = if isJust as then (fromJust as)
                                else modName
               -- simpModName (SN m loc) = m

