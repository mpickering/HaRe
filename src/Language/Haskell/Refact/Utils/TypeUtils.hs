{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- Module      : TypeUtils

-- Maintainer  : refactor-fp\@kent.ac.uk
-- |
--
-- This module contains a collection of program analysis and
-- transformation functions (the API) that work over the Type
-- Decorated AST. Most of the functions defined in the module are
-- taken directly from the API, but in some cases are modified to work
-- with the type decorated AST.
--
-- In particular some new functions have been added to make type
-- decorated AST traversals easier.
--
-- In HaRe, in order to preserve the comments and layout of refactored
-- programs, a refactoring modifies not only the AST but also the
-- token stream, and the program source after the refactoring is
-- extracted from the token stream rather than the AST, for the
-- comments and layout information is kept in the token steam instead
-- of the AST. As a consequence, a program transformation function
-- from this API modifies both the AST and the token stream (unless
-- explicitly stated). So when you build your own program
-- transformations, try to use the API to do the transformation, as
-- this can liberate you from caring about the token stream.
--
-- This type decorated API is still in development. Any suggestions
-- and comments are very much welcome.


--------------------------------------------------------------------------------
module Language.Haskell.Refact.Utils.TypeUtils
       (
 -- * Program Analysis
    -- ** Imports and exports
    inScopeInfo, isInScopeAndUnqualified, isInScopeAndUnqualifiedGhc, inScopeNames
   , isExported, isExplicitlyExported, modIsExported

    -- ** Property checking
    ,isVarId,isConId,isOperator,isTopLevelPN,isLocalPN,isNonLibraryName
    ,isQualifiedPN, isFunOrPatName, isTypeSig, isTypeSigDecl
    ,isFunBindP,isFunBindR,isPatBindP,isPatBindR,isSimplePatBind,isSimplePatDecl
    ,isComplexPatBind,isComplexPatDecl,isFunOrPatBindP,isFunOrPatBindR
    ,usedWithoutQualR,isUsedInRhs

    -- ** Getting
    ,findNameInRdr
    ,findPNT,findPN,findAllNameOccurences
    ,findPNs, findNamesRdr, findEntity, findEntity'
    ,findIdForName
    ,getTypeForName

    ,defines, definesP,definesTypeSig,definesTypeSigRdr
    -- ,HasModName(hasModName), HasNameSpace(hasNameSpace)
    ,sameBind,sameBindRdr
    {- ,usedByRhs -},UsedByRhs(..)

    -- ** Modules and files
    -- ,clientModsAndFiles,serverModsAndFiles,isAnExistingMod
    -- ,fileNameToModName, strToModName, modNameToStr
    , isMainModule
    , getModule

    -- ** Locations
    ,defineLoc, useLoc, locToExp
    ,locToName, locToRdrName
    ,getName

 -- * Program transformation
    -- ** Adding
    ,addDecl, addItemsToImport, addHiding
    ,addParamsToDecls, addActualParamsToRhs, addImportDecl, duplicateDecl
    -- ** Removing
    ,rmDecl, rmTypeSig, rmTypeSigs -- , commentOutTypeSig, rmParams
    -- ,rmItemsFromExport, rmSubEntsFromExport, Delete(delete)

    -- ** Updating
    -- ,Update(update)
    {- ,qualifyPName-},rmQualifier,qualifyToplevelName,{- renamePN, -} renamePN' {- ,replaceNameInPN -},autoRenameLocalVar

    -- * Miscellous
    -- ** Parsing, writing and showing
    {- ,parseSourceFile,writeRefactoredFiles-}, showEntities,showPNwithLoc -- , newProj, addFile, chase
    -- ** Locations
    -- ,toRelativeLocs, rmLocs
    -- ** Default values
   ,defaultPN {- ,defaultPNT -},defaultName {-,defaultModName-},defaultExp -- ,defaultPat, defaultExpUnTyped


    -- ** Identifiers, expressions, patterns and declarations
    ,ghcToPN,lghcToPN, expToName, expToNameRdr
    ,nameToString
    ,patToNameRdr
    {- ,expToPNT, expToPN, nameToExp,pNtoExp -},patToPNT {- , patToPN --, nameToPat -},pNtoPat

    -- ** Others
    , divideDecls
    , mkRdrName,mkNewGhcName,mkNewName,mkNewToplevelName

    -- The following functions are not in the the API yet.
    , causeNameClashInExports {- , inRegion , unmodified -}

    , removeOffset

    , declsSybTransform

    -- * Typed AST traversals (added by CMB)
    -- * Miscellous
    -- ,removeFromInts, getDataName, checkTypes, getPNs, getPN, getPNPats, mapASTOverTAST

    -- * Debug stuff
    -- , getDeclAndToks, getSigAndToks
    -- , getToksForDecl, removeToksOffset -- ++AZ++ remove this after debuggging
    , getParsedForRenamedLPat
    , getParsedForRenamedName
    , getParsedForRenamedLocated
    -- , allPNT
    --  , allPNTLens
    , rdrNameFromName
    , stripLeadingSpaces
    -- , lookupNameGhc
 ) where

import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Exception

import Language.Haskell.Refact.Utils.ExactPrint
import Language.Haskell.Refact.Utils.GhcUtils
import Language.Haskell.Refact.Utils.GhcVersionSpecific
import Language.Haskell.Refact.Utils.LocUtils
import Language.Haskell.Refact.Utils.Monad
import Language.Haskell.Refact.Utils.MonadFunctions
import Language.Haskell.Refact.Utils.TypeSyn
import Language.Haskell.Refact.Utils.Types
import Language.Haskell.Refact.Utils.Variables

import Language.Haskell.GHC.ExactPrint hiding (liftT)
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils


-- Modules from GHC
import qualified Bag           as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified Module        as GHC
import qualified Name          as GHC
-- import qualified Outputable    as GHC
import qualified RdrName       as GHC
import qualified Unique        as GHC
import qualified Var           as GHC

import qualified Data.Generics as SYB
import qualified GHC.SYB.Utils as SYB

import qualified Data.Map as Map

import Data.Generics.Strafunski.StrategyLib.StrategyLib hiding (liftIO,MonadPlus,mzero)

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- |Process the inscope relation returned from the parsing and module
-- analysis pass, and return a list of four-element tuples. Each tuple
-- contains an identifier name, the identifier's namespace info, the
-- identifier's defining module name and its qualifier name.
--
-- The same identifier may have multiple entries in the result because
-- it may have different qualifiers. This makes it easier to decide
-- whether the identifier can be used unqualifiedly by just checking
-- whether there is an entry for it with the qualifier field being
-- Nothing.
--
inScopeInfo :: InScopes                                      -- ^ The inscope relation .
           ->[(String, GHC.NameSpace, GHC.ModuleName, Maybe GHC.ModuleName)] -- ^ The result
inScopeInfo names = nub $  map getEntInfo $ names
  where
     getEntInfo name
       =(showGhc name,
         GHC.occNameSpace $ GHC.nameOccName name,
         GHC.moduleName $ GHC.nameModule name,
         getQualMaybe $ GHC.nameRdrName name)

     getQualMaybe rdrName = case rdrName of
       GHC.Qual modName _occName -> Just modName
       _                         -> Nothing



-- | Return True if the identifier is inscope and can be used without
-- a qualifier.
isInScopeAndUnqualified::String       -- ^ The identifier name.
                       ->InScopes     -- ^ The inscope relation
                       ->Bool         -- ^ The result.
isInScopeAndUnqualified n names
 = isJust $ find (\ (x, _,_, qual) -> x == n && isNothing qual ) $ inScopeInfo names

-- | Return True if the identifier is inscope and can be used without
-- a qualifier. The identifier name string may have a qualifier
-- already
-- NOTE: may require qualification based on name clash with an
-- existing identifier.
isInScopeAndUnqualifiedGhc ::
     String           -- ^ The identifier name.
  -> (Maybe GHC.Name) -- ^ Existing name, to be excluded from test, if
                      --   known
  -> RefactGhc Bool   -- ^ The result.
isInScopeAndUnqualifiedGhc n maybeExising = do
  names <- ghandle handler (GHC.parseName n)
  logm $ "isInScopeAndUnqualifiedGhc:(n,(maybeExising,names))=" ++ (show n) ++ ":" ++  (showGhc (maybeExising,names))
  ctx <- GHC.getContext
  logm $ "isInScopeAndUnqualifiedGhc:ctx=" ++ (showGhc ctx)
  let nameList = case maybeExising of
                  Nothing -> names
                  Just n' -> filter (\x -> (showGhcQual x) /= (showGhcQual n')) names
  logm $ "isInScopeAndUnqualifiedGhc:(n,nameList)=" ++ (show n) ++ ":" ++  (showGhc nameList)
  return $ nameList /= []

  where
    handler:: SomeException -> RefactGhc [GHC.Name]
    handler e = do
      logm $ "isInScopeAndUnqualifiedGhc.handler e=" ++ (show e)
      return []

-- ---------------------------------------------------------------------

inScopeNames :: String         -- ^ The identifier name.
             -> RefactGhc [GHC.Name] -- ^ The result.
inScopeNames n = do
  names <- ghandle handler (GHC.parseName n)
  logm $ "inScopeNames:(n,names)=" ++ (show n) ++ ":" ++  (showGhc names)
  return $ names

  where
    handler:: SomeException -> RefactGhc [GHC.Name]
    handler e = do
      logm $ "inScopeNames.handler e=" ++ (show e)
      return []

-- ---------------------------------------------------------------------
-- | Show a PName in a format like: 'pn'(at row:r, col: c).
showPNwithLoc:: GHC.Located GHC.Name -> String
showPNwithLoc pn@(GHC.L l _n)
  = let (r,c) = getGhcLoc l
    -- in  " '"++pNtoName pn++"'" ++"(at row:"++show r ++ ",col:" ++ show c ++")"
    in  " '"++showGhc pn++"'" ++"(at row:"++show r ++ ",col:" ++ show c ++")"

-- ---------------------------------------------------------------------

defaultPN :: PName
defaultPN = PN (mkRdrName "nothing")

defaultName :: GHC.Name
defaultName = n
  where
    un = GHC.mkUnique 'H' 0 -- H for HaRe :)
    n = GHC.localiseName $ GHC.mkSystemName un (GHC.mkVarOcc "nothing")

-- | Default expression.
defaultExp::HsExpP
-- defaultExp=Exp (HsId (HsVar defaultPNT))
defaultExp=GHC.HsVar $ mkRdrName "nothing"

mkRdrName :: String -> GHC.RdrName
mkRdrName s = GHC.mkVarUnqual (GHC.mkFastString s)

-- ---------------------------------------------------------------------

-- | Make a new GHC.Name, using the Unique Int sequence stored in the
-- RefactState.
mkNewGhcName :: Maybe GHC.Module -> String -> RefactGhc GHC.Name
mkNewGhcName maybeMod name = do
  s <- get
  u <- gets rsUniqState
  put s { rsUniqState = (u+1) }
  return (mkNewGhcNamePure 'H' (u + 1) maybeMod name)


-- ---------------------------------------------------------------------

mkNewToplevelName :: GHC.Module -> String -> GHC.SrcSpan -> RefactGhc GHC.Name
mkNewToplevelName modid name defLoc = do
  s <- get
  u <- gets rsUniqState
  put s { rsUniqState = (u+1) }

  let un = GHC.mkUnique 'H' (u+1) -- H for HaRe :)
      -- n = GHC.mkSystemName un (GHC.mkVarOcc name)
      -- n = GHC.localiseName $ GHC.mkSystemName un (GHC.mkVarOcc name)

        -- mkExternalName :: Unique -> Module -> OccName -> SrcSpan -> Name
      n = GHC.mkExternalName un modid (GHC.mkVarOcc name) defLoc
  return n

---------------------------------------------------------------------------


-- |Create a new name base on the old name. Suppose the old name is 'f', then
--  the new name would be like 'f_i' where 'i' is an integer.
mkNewName::String      -- ^ The old name
          ->[String]   -- ^ The set of names which the new name cannot take
          ->Int        -- ^ The posfix value
          ->String     -- ^ The result
mkNewName oldName fds suffix
  =let newName=if suffix==0 then oldName
                            else oldName++"_"++ show suffix
   in if elem newName fds
        then mkNewName oldName fds (suffix+1)
        else newName

-- ---------------------------------------------------------------------

-- | Return True if the current module is exported either by default
-- or by specifying the module name in the export.
modIsExported:: GHC.ModuleName       -- ^ The module name
               -> GHC.RenamedSource  -- ^ The AST of the module
               -> Bool               -- ^ The result
modIsExported modName (_g,_emps,mexps,_mdocs)
   = let
       modExported (GHC.L _ (GHC.IEModuleContents (GHC.L _ name))) = name == modName
       modExported _ = False

       moduleExports = filter modExported $ fromMaybe [] mexps

     in if isNothing mexps
           then True
           else (nonEmptyList moduleExports)

-- ---------------------------------------------------------------------

-- | Return True if an identifier is exported by the module currently
-- being refactored.
isExported :: GHC.Name -> RefactGhc Bool
isExported n = do
  typechecked <- getTypecheckedModule
  let modInfo = GHC.tm_checked_module_info typechecked
  return $ GHC.modInfoIsExportedName modInfo n

-- ---------------------------------------------------------------------

-- | Return True if an identifier is explicitly exported by the module.
isExplicitlyExported::GHC.Name           -- ^ The identifier
                     ->GHC.RenamedSource -- ^ The AST of the module
                     ->Bool              -- ^ The result
isExplicitlyExported pn (_g,_imps,exps,_docs)
  = findEntity pn exps

-- ---------------------------------------------------------------------


-- | Check if the proposed new name will conflict with an existing export
causeNameClashInExports::  GHC.Name          -- ^ The original name
                        -> GHC.Name          -- ^ The new name
                        -> GHC.ModuleName    -- ^ The identity of the module
                        -> GHC.RenamedSource -- ^ The AST of the module
                        -> Bool              -- ^ The result

-- Note that in the abstract representation of exps, there is no qualified entities.
causeNameClashInExports pn newName modName renamed@(_g,imps,maybeExps,_doc)
  = let exps = fromMaybe [] maybeExps
        varExps = filter isImpVar exps
        -- TODO: make withoutQual part of the API
        withoutQual n = showGhc $ GHC.localiseName n
        modNames=nub (concatMap (\(GHC.L _ (GHC.IEVar (GHC.L _ x)))->if withoutQual x== withoutQual newName
                                                        then [GHC.moduleName $ GHC.nameModule x]
                                                        else []) varExps)
        res = (isExplicitlyExported pn renamed) &&
               ( any (modIsUnQualifedImported renamed) modNames
                 || elem modName modNames)
    in res
 where
    isImpVar (GHC.L _ x) = case x of
      GHC.IEVar _ -> True
      _           -> False

    modIsUnQualifedImported _mod' modName'
     =let -- imps =hsModImports mod
       -- imp@(GHC.L _ (GHC.ImportDecl (GHC.L _ modName) qualify _source _safe isQualified _isImplicit as h))
      in isJust $ find (\(GHC.L _ (GHC.ImportDecl _ (GHC.L _ modName1) _qualify _source _safe isQualified _isImplicit _as _h))
                                -> modName1 == modName' && (not isQualified)) imps
      -- in isJust $ find (\(HsImportDecl _ (SN modName1 _) qualify  _ h) -> modName == modName1 && (not qualify)) imps


-- Original seems to be
--   1. pick up any module names in the export list with same unQual
     --   part as the new name
--   2. Check if the old is exported explicitly
--   3.  if so, if the new module is exported unqualified
--        or belongs to the current module
--       then it will cause a clash

-- ---------------------------------------------------------------------

-- | Given a RenamedSource LPAT, return the equivalent
-- ParsedSource part.
-- NOTE: returns pristine ParsedSource, since HaRe does not change it
getParsedForRenamedLPat :: GHC.ParsedSource -> GHC.LPat GHC.Name -> GHC.LPat GHC.RdrName
getParsedForRenamedLPat parsed lpatParam@(GHC.L l _pat) = r
  where
    mres = res parsed
    r = case mres of
      Just rr -> rr
      Nothing -> error $ "HaRe error: could not find Parsed LPat for"
                 ++ (SYB.showData SYB.Renamer 0 lpatParam)

    res t = SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` lpat) t

    lpat :: (GHC.LPat GHC.RdrName) -> (Maybe (GHC.LPat GHC.RdrName))
    lpat p@(GHC.L lp _)
       | lp == l = Just p
    lpat _ = Nothing

-- ---------------------------------------------------------------------

-- | Given a RenamedSource Located name, return the equivalent
-- ParsedSource part.
-- NOTE: returns pristine ParsedSource, since HaRe does not change it
getParsedForRenamedLocated :: ({- SYB.Typeable a, SYB.Data a, -} SYB.Typeable b {- , SYB.Data b -})
  => GHC.Located a -> RefactGhc (GHC.Located b)
getParsedForRenamedLocated (GHC.L l _n) = do
  parsed <- getRefactParsed
  let
    mres = res parsed
    r = case mres of
      Just rr -> rr
      Nothing -> error $ "HaRe error: could not find Parsed Location for"
                 ++ (showGhc l)

    res t = SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` lname) t

    lname :: (GHC.Located b) -> (Maybe (GHC.Located b))
    lname p@(GHC.L lp _)
       | lp == l = Just p
    lname _ = Nothing

  return r


-- | Given a RenamedSource Located name, return the equivalent
-- ParsedSource part.
-- NOTE: returns pristine ParsedSource, since HaRe does not change it
getParsedForRenamedName :: GHC.ParsedSource -> GHC.Located GHC.Name -> GHC.Located GHC.RdrName
getParsedForRenamedName parsed n@(GHC.L l _n) = r
  where
    mres = res parsed
    r = case mres of
      Just rr -> rr
      Nothing -> error $ "HaRe error: could not find Parsed LPat for"
                 ++ (SYB.showData SYB.Renamer 0 n)

    res t = SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` lname) t

    lname :: (GHC.Located GHC.RdrName) -> (Maybe (GHC.Located GHC.RdrName))
    lname p@(GHC.L lp _)
       | lp == l = Just p
    lname _ = Nothing

------------------------------------------------------------------------

-- | Return True if the identifier is unqualifiedly used in the given
-- syntax phrase.
-- usedWithoutQualR :: GHC.Name -> GHC.ParsedSource -> Bool
usedWithoutQualR ::  (SYB.Data t) => GHC.Name -> t -> Bool
usedWithoutQualR name parsed = fromMaybe False res
  where
     res = SYB.somethingStaged SYB.Parser Nothing
            (Nothing `SYB.mkQ` worker
            `SYB.extQ` workerBind
            `SYB.extQ` workerExpr
            ) parsed

     worker  (pname :: GHC.Located GHC.RdrName) =
       checkName pname

     workerBind (GHC.L l (GHC.VarPat n) :: (GHC.Located (GHC.Pat GHC.RdrName))) =
       checkName (GHC.L l n)
     workerBind _ = Nothing

     workerExpr ((GHC.L l (GHC.HsVar n)) :: (GHC.Located (GHC.HsExpr GHC.RdrName)))
       = checkName (GHC.L l n)
     workerExpr _ = Nothing

     -- ----------------

     checkName ((GHC.L l pn)::GHC.Located GHC.RdrName)
        | ((GHC.rdrNameOcc pn) == (GHC.nameOccName name)) &&
          -- isUsedInRhs pname parsed &&
          isUsedInRhs (GHC.L l name) parsed &&
          GHC.isUnqual pn     = Just True
     checkName _ = Nothing


-----------------------------------------------------------------------------

getModule :: RefactGhc GHC.Module
getModule = do
  typechecked <- getTypecheckedModule
  return $ GHC.ms_mod $ GHC.pm_mod_summary $ GHC.tm_parsed_module typechecked

-- ---------------------------------------------------------------------

-- | Return True if a string is a lexically  valid variable name.
isVarId :: String -> Bool
isVarId mid = isId mid && isSmall (ghead "isVarId" mid)
     where isSmall c=isLower c || c=='_'

-- | Return True if a string is a lexically valid constructor name.
isConId :: String -> Bool
isConId mid = isId mid && isUpper (ghead "isConId" mid)

-- | Return True if a string is a lexically valid operator name.
isOperator :: String -> Bool
isOperator mid = mid /= [] && isOpSym (ghead "isOperator" mid) &&
                isLegalOpTail (tail mid) && not (isReservedOp mid)
   where
    isOpSym mid' = elem mid' opSymbols
       where opSymbols = ['!', '#', '$', '%', '&', '*', '+','.','/','<','=','>','?','@','\'','^','|','-','~']

    isLegalOpTail tail' = all isLegal tail'
       where isLegal c = isOpSym c || c==':'

    isReservedOp mid' = elem mid' reservedOps
       where reservedOps = ["..", ":","::","=","\"", "|","<-","@","~","=>"]

-- | Returns True if a string lexically is an identifier.
-- *This function should not be exported.*
isId::String->Bool
isId mid = mid/=[] && isLegalIdTail (tail mid) && not (isReservedId mid)
  where
    isLegalIdTail tail' = all isLegal tail'
        where isLegal c=isSmall c|| isUpper c || isDigit c || c=='\''

    isReservedId mid' = elem mid' reservedIds
      where reservedIds=["case", "class", "data", "default", "deriving","do","else" ,"if",
                         "import", "in", "infix","infixl","infixr","instance","let","module",
                         "newtype", "of","then","type","where","_"]

    isSmall c=isLower c || c=='_'

-----------------------------------------------------------------------------

-- |Return True if a PName is a toplevel PName.
isTopLevelPN::GHC.Name -> RefactGhc Bool
isTopLevelPN n = do
  typechecked <- getTypecheckedModule
  let maybeNames = GHC.modInfoTopLevelScope $ GHC.tm_checked_module_info typechecked
  let names = fromMaybe [] maybeNames
  return $ n `elem` names


-- |Return True if a PName is a local PName.
isLocalPN::GHC.Name -> Bool
isLocalPN = GHC.isInternalName
-- isLocalPN (PN i (UniqueNames.S _)) = True
-- isLocalPN _ = False

-- |Return True if the name has a @GHC.SrcSpan@, i.e. is declared in
-- source we care about
isNonLibraryName :: GHC.Name -> Bool
isNonLibraryName n = case (GHC.nameSrcSpan n) of
  GHC.UnhelpfulSpan _ -> False
  _                   -> True


-- |Return True if a PName is a function\/pattern name defined in t.
isFunOrPatName::(SYB.Data t) => GHC.Name -> t -> Bool
isFunOrPatName pn
   =isJust . SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` worker)
     where
        -- worker (decl::HsDeclP)
        worker (decl::GHC.LHsBind GHC.Name)
           | defines pn decl = Just True
        worker _ = Nothing

-------------------------------------------------------------------------------
-- |Return True if a PName is a qualified PName.
--  AZ:NOTE: this tests the use instance, the underlying name may be qualified.
--           e.g. used name is zip, GHC.List.zip
--     NOTE2: not sure if this gives a meaningful result for a GHC.Name
isQualifiedPN :: GHC.Name -> RefactGhc Bool
isQualifiedPN name = return $ GHC.isQual $ GHC.nameRdrName name


-- | Return True if a declaration is a type signature declaration.
isTypeSig :: GHC.LSig a -> Bool
isTypeSig (GHC.L _ (GHC.TypeSig _ _ _)) = True
isTypeSig _ = False

-- | Return True if a declaration is a type signature declaration.
isTypeSigDecl :: GHC.LHsDecl a -> Bool
isTypeSigDecl (GHC.L _ (GHC.SigD (GHC.TypeSig _ _ _))) = True
isTypeSigDecl _ = False

-- | Return True if a declaration is a function definition.
isFunBindP::HsDeclP -> Bool
isFunBindP (GHC.L _ (GHC.ValD (GHC.FunBind _ _ _ _ _ _))) = True
isFunBindP _ =False

isFunBindR::GHC.LHsBind t -> Bool
isFunBindR (GHC.L _l (GHC.FunBind _ _ _ _ _ _)) = True
isFunBindR _ =False

-- | Returns True if a declaration is a pattern binding.
isPatBindP::HsDeclP->Bool
isPatBindP (GHC.L _ (GHC.ValD (GHC.PatBind _ _ _ _ _))) = True
isPatBindP _=False

isPatBindR::GHC.LHsBind t -> Bool
isPatBindR (GHC.L _ (GHC.PatBind _ _ _ _ _)) = True
isPatBindR _=False


-- | Return True if a declaration is a pattern binding which only
-- defines a variable value.
isSimplePatDecl :: (GHC.DataId t) => GHC.LHsDecl t-> Bool
isSimplePatDecl decl = case decl of
     (GHC.L _l (GHC.ValD (GHC.PatBind p _rhs _ty _fvs _))) -> hsPNs p /= []
     _ -> False

-- | Return True if a declaration is a pattern binding which only
-- defines a variable value.
isSimplePatBind :: (GHC.DataId t) => GHC.LHsBind t-> Bool
isSimplePatBind decl = case decl of
     (GHC.L _l (GHC.PatBind p _rhs _ty _fvs _)) -> hsPNs p /= []
     _ -> False

-- | Return True if a declaration is a pattern binding but not a simple one.
isComplexPatDecl::GHC.LHsDecl name -> Bool
isComplexPatDecl (GHC.L l (GHC.ValD decl)) = isComplexPatBind (GHC.L l decl)
isComplexPatDecl _ = False

-- | Return True if a LHsBin is a pattern binding but not a simple one.
isComplexPatBind::GHC.LHsBind name -> Bool
isComplexPatBind decl
  = case decl of
     (GHC.L _l (GHC.PatBind (GHC.L _ (GHC.VarPat _)) _rhs _ty _fvs _)) -> True
     _ -> False

-- | Return True if a declaration is a function\/pattern definition.
isFunOrPatBindP :: HsDeclP -> Bool
isFunOrPatBindP decl = isFunBindP decl || isPatBindP decl

-- | Return True if a declaration is a function\/pattern definition.
isFunOrPatBindR :: GHC.LHsBind t -> Bool
isFunOrPatBindR decl = isFunBindR decl || isPatBindR decl

-- ---------------------------------------------------------------------


-- | Returns True is a syntax phrase, say a, is part of another syntax
-- phrase, say b.
-- Expects to be at least Parser output
findEntity':: (SYB.Data a, SYB.Data b)
              => a -> b -> Maybe (SimpPos,SimpPos)
findEntity' a b = res
  where
    -- ++AZ++ do a generic traversal, and see if it matches.
    res = SYB.somethingStaged SYB.Parser Nothing worker b

    worker :: (SYB.Data c)
           => c -> Maybe (SimpPos,SimpPos)
    worker x = if SYB.typeOf a == SYB.typeOf x
                 -- then Just (getStartEndLoc b == getStartEndLoc a)
                 then Just (getStartEndLoc x)
                 else Nothing

--------------------------------------------------------------------------------

sameBindRdr :: NameMap -> GHC.LHsDecl GHC.RdrName -> GHC.LHsDecl GHC.RdrName -> Bool
sameBindRdr nm b1 b2 = (definedNamesRdr nm b1) == (definedNamesRdr nm b2)

sameBind :: GHC.LHsBind GHC.Name -> GHC.LHsBind GHC.Name -> Bool
sameBind b1 b2 = (definedPNs b1) == (definedPNs b2)

-- ---------------------------------------------------------------------

-- TODO: is this the same is isUsedInRhs?
class (SYB.Data t) => UsedByRhs t where

    -- | Return True if any of the GHC.Name's appear in the given
    -- syntax element
    usedByRhs :: t -> [GHC.Name] -> Bool
    usedByRhsRdr :: NameMap -> t -> [GHC.Name] -> Bool

instance UsedByRhs GHC.RenamedSource where

   -- Not a meaningful question at this level
   usedByRhs _renamed _pns = False
   usedByRhsRdr _ _ = assert False undefined

instance UsedByRhs (GHC.HsModule GHC.RdrName) where

   -- Not a meaningful question at this level
   usedByRhsRdr _ _parsed _pns = False
   usedByRhs _ _ = assert False undefined

-- -------------------------------------

instance (UsedByRhs a) => UsedByRhs (GHC.Located a) where
  usedByRhsRdr nm (GHC.L _ d) pns = usedByRhsRdr nm d pns
  -- usedByRhs _ _ = assert False undefined
  usedByRhs _ la = error $ "usedByRhs:Located a=" ++ SYB.showData SYB.Parser 0 la

-- -------------------------------------

instance UsedByRhs [GHC.LHsDecl GHC.RdrName] where
  usedByRhs _ _ = assert False undefined
  usedByRhsRdr nm ds pns = or $ map (\d -> usedByRhsRdr nm d pns) ds

-- -------------------------------------

instance UsedByRhs (GHC.HsDecl GHC.RdrName) where
  usedByRhs _ _ = assert False undefined
  usedByRhsRdr nm de pns =
   case de of
      GHC.TyClD d       -> f d
      GHC.InstD d       -> f d
      GHC.DerivD d      -> f d
      GHC.ValD d        -> f d
      GHC.SigD d        -> f d
      GHC.DefD d        -> f d
      GHC.ForD d        -> f d
      GHC.WarningD d    -> f d
      GHC.AnnD d        -> f d
      GHC.RuleD d       -> f d
      GHC.VectD d       -> f d
      GHC.SpliceD d     -> f d
      GHC.DocD d        -> f d
      GHC.RoleAnnotD d  -> f d
#if __GLASGOW_HASKELL__ < 711
      GHC.QuasiQuoteD d -> f d
#endif
     where
       f d' = usedByRhsRdr nm d' pns

-- -------------------------------------

instance UsedByRhs (GHC.TyClDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.InstDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.DerivDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.ForeignDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.WarnDecls GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.AnnDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.RoleAnnotDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.HsQuasiQuote GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.DefaultDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.SpliceDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.VectDecl GHC.RdrName) where
  usedByRhsRdr = assert False undefined
  usedByRhs _ _ = assert False undefined

instance UsedByRhs (GHC.RuleDecls GHC.RdrName) where
  usedByRhs _ _ = assert False undefined
  usedByRhsRdr = assert False undefined

instance UsedByRhs GHC.DocDecl where
  usedByRhs _ _ = assert False undefined
  usedByRhsRdr = assert False undefined

instance UsedByRhs (GHC.Sig GHC.RdrName) where
  usedByRhsRdr _ _ _ = False
  usedByRhs _ = assert False undefined

-- -------------------------------------

instance UsedByRhs (GHC.LHsBinds GHC.Name) where
  usedByRhs binds pns = or $ map (\b -> usedByRhs b pns) $ GHC.bagToList binds
  usedByRhsRdr _ _ = assert False undefined

instance UsedByRhs (GHC.HsValBinds GHC.Name) where
  usedByRhs (GHC.ValBindsIn binds _sigs) pns  = usedByRhs (GHC.bagToList binds) pns
  usedByRhs (GHC.ValBindsOut binds _sigs) pns = or $ map (\(_,b) -> usedByRhs b pns) binds
  usedByRhsRdr _ _ = assert False undefined

-- -------------------------------------

instance UsedByRhs (GHC.Match GHC.Name (GHC.LHsExpr GHC.Name)) where
  usedByRhs (GHC.Match _ _ _ (GHC.GRHSs rhs _)) pns -- = usedByRhs (hsValBinds rhs) pns
                                                 = findPNs pns rhs
  usedByRhsRdr _ _ = assert False undefined


instance UsedByRhs (GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  usedByRhsRdr nm (GHC.Match _ _ _ (GHC.GRHSs rhs _)) pns
    = findNamesRdr nm pns rhs

  usedByRhs _ _ = assert False undefined

-- -------------------------------------

instance UsedByRhs [GHC.LHsBind GHC.Name] where
  usedByRhs binds pns = or $ map (\b -> usedByRhs b pns) binds
  usedByRhsRdr _ _ = assert False undefined

instance UsedByRhs (GHC.HsBind GHC.Name) where
  usedByRhs (GHC.FunBind _ _ matches _ _ _) pns = findPNs pns matches
  usedByRhs (GHC.PatBind _ rhs _ _ _)       pns = findPNs pns rhs
  usedByRhs (GHC.VarBind _ rhs _)           pns = findPNs pns rhs
  usedByRhs (GHC.AbsBinds _ _ _ _ _)       _pns = False
  usedByRhs (GHC.PatSynBind _)             _pns = error "To implement: usedByRhs PaySynBind"
  usedByRhsRdr _ _ = assert False undefined

-- -------------------------------------

instance UsedByRhs (GHC.HsBind GHC.RdrName) where
  usedByRhs _ _ = assert False undefined
  usedByRhsRdr nm  (GHC.FunBind _ _ matches _ _ _)        pns = findNamesRdr nm pns matches
  usedByRhsRdr nm  (GHC.PatBind _ rhs _ _ _)              pns = findNamesRdr nm pns rhs
  usedByRhsRdr nm  (GHC.PatSynBind (GHC.PSB _ _ _ rhs _)) pns = findNamesRdr nm pns rhs
  usedByRhsRdr nm  (GHC.VarBind _ rhs _)                  pns = findNamesRdr nm pns rhs
  usedByRhsRdr _nm (GHC.AbsBinds _ _ _ _ _)              _pns = False

-- -------------------------------------

instance UsedByRhs (GHC.HsExpr GHC.Name) where
  usedByRhs (GHC.HsLet _lb e) pns = findPNs pns e
  usedByRhs e                _pns = error $ "undefined usedByRhs:" ++ (showGhc e)
  usedByRhsRdr _ _ = assert False undefined

instance UsedByRhs (GHC.HsExpr GHC.RdrName) where
  usedByRhsRdr nm (GHC.HsLet _lb e) pns = findNamesRdr nm pns e
  usedByRhsRdr _ e                 _pns = error $ "undefined usedByRhsRdr:" ++ (showGhc e)
  usedByRhs _ _ = assert False undefined

-- -------------------------------------

instance UsedByRhs (GHC.Stmt GHC.Name (GHC.LHsExpr GHC.Name)) where
  usedByRhs (GHC.LetStmt lb) pns = findPNs pns lb
  usedByRhs s               _pns = error $ "undefined usedByRhs:" ++ (showGhc s)
  usedByRhsRdr _ _ = assert False undefined

instance UsedByRhs (GHC.Stmt GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  usedByRhsRdr nm (GHC.LetStmt lb) pns = findNamesRdr nm pns lb
  usedByRhsRdr _ s               _pns = error $ "undefined usedByRhsRdr:" ++ (showGhc s)
  usedByRhs _ _ = assert False undefined

--------------------------------------------------------------------------------

-- |Find the identifier with the given name. This looks through the
-- given syntax phrase for the first GHC.Name which matches. Because
-- it is Renamed source, the GHC.Name will include its defining
-- location. Returns Nothing if the name is not found.

getName::(SYB.Data t)=> String           -- ^ The name to find
                     -> t                -- ^ The syntax phrase
                     -> Maybe GHC.Name   -- ^ The result
getName str t
  = res
       where
        res = SYB.somethingStaged SYB.Renamer Nothing
            (Nothing `SYB.mkQ` worker `SYB.extQ` workerBind `SYB.extQ` workerExpr) t

        worker ((GHC.L _ n) :: (GHC.Located GHC.Name))
          | showGhcQual n == str = Just n
        worker _ = Nothing

        workerBind (GHC.L _ (GHC.VarPat name) :: (GHC.Located (GHC.Pat GHC.Name)))
          | showGhcQual name == str = Just name
        workerBind _ = Nothing


        workerExpr ((GHC.L _ (GHC.HsVar name)) :: (GHC.Located (GHC.HsExpr GHC.Name)))
          | showGhcQual name == str = Just name
        workerExpr _ = Nothing

-- ---------------------------------------------------------------------

-- | Add identifiers to the export list of a module. If the second argument is
-- like: Just p, then do the adding only if p occurs in the export list, and the
-- new identifiers are added right after p in the export list. Otherwise the new
-- identifiers are add to the beginning of the export list. In the case that the
-- export list is emport, then if the third argument is True, then create an
-- explict export list to contain only the new identifiers, otherwise do
-- nothing.

addImportDecl ::
    GHC.ParsedSource
    -> GHC.ModuleName
    -> Maybe GHC.FastString -- ^qualifier
    -> Bool -> Bool -> Bool
    -> Maybe String         -- ^alias
    -> Bool
    -> [GHC.RdrName]
    -> RefactGhc GHC.ParsedSource
-- addImportDecl (groupedDecls,imp, b, c) modName pkgQual source safe qualify alias hide idNames
addImportDecl (GHC.L l p) modName pkgQual source safe qualify alias hide idNames
  = do
       let imp = GHC.hsmodImports p
       impDecl <- mkImpDecl
       newSpan <- liftT uniqueSrcSpanT
       let newImp = GHC.L newSpan impDecl
       liftT $ addSimpleAnnT newImp (DP (1,0)) [((G GHC.AnnImport),DP (0,0))]
       return (GHC.L l p { GHC.hsmodImports = (imp++[newImp])})
  where

     alias' = case alias of
                  Just stringName -> Just $ GHC.mkModuleName stringName
                  _               -> Nothing

     mkImpDecl = do
       newSpan1 <- liftT uniqueSrcSpanT
       newSpan2 <- liftT uniqueSrcSpanT
       newEnts <- mkNewEntList idNames
       let lNewEnts = GHC.L newSpan2 newEnts
       -- logm $ "addImportDecl.mkImpDecl:adding anns for:" ++ showGhc lNewEnts
       liftT $ addSimpleAnnT lNewEnts (DP (0,1)) [((G GHC.AnnHiding),DP (0,0)),((G GHC.AnnOpenP),DP (0,1)),((G GHC.AnnCloseP),DP (0,0))]
       let lmodname = GHC.L newSpan1 modName
       liftT $ addSimpleAnnT lmodname (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
       return $ GHC.ImportDecl
                        { GHC.ideclSourceSrc = Nothing
                        , GHC.ideclName      = lmodname
                        , GHC.ideclPkgQual   = pkgQual
                        , GHC.ideclSource    = source
                        , GHC.ideclSafe      = safe
                        , GHC.ideclQualified = qualify
                        , GHC.ideclImplicit  = False
                        , GHC.ideclAs        = alias'
                        , GHC.ideclHiding    =
                                      (if idNames == [] && hide == False then
                                            Nothing
                                       else
                                            (Just (hide, lNewEnts)))
                        }

-- ---------------------------------------------------------------------
{-
-- | Remove ImportDecl from the imports list, commonly returned from a RenamedSource type, so it can
-- be further processed.
--rmPreludeImports :: [GHC.Located (GHC.ImportDecl GHC.Name)] -> [GHC.Located (GHC.ImportDecl GHC.Name)]
rmPreludeImports ::
  [GHC.Located (GHC.ImportDecl GHC.Name)]
  -> [GHC.Located (GHC.ImportDecl GHC.Name)]
rmPreludeImports = filter isPrelude where
            isPrelude = (/="Prelude") . GHC.moduleNameString . GHC.unLoc . GHC.ideclName . GHC.unLoc
-}

-- ---------------------------------------------------------------------

-- | Adding a declaration to the declaration list of the given syntax
-- phrase. If the second argument is Nothing, then the declaration
-- will be added to the beginning of the declaration list, but after
-- the data type declarations is there is any.
addDecl:: (HasDecls t)
        => t              -- ^The AST to be updated
        -> Maybe GHC.Name -- ^If this is Just, then the declaration
                          -- will be added right after this
                          -- identifier's definition.
        -> ([GHC.LHsDecl GHC.RdrName],  Maybe Anns)
             -- ^ The declaration with optional signatures to be added, together
             -- with optional Annotations.
        -> Bool              -- ^ True means the declaration is a
                             -- toplevel declaration.
        -- ++TODO:AZ get rid of the topLevel Bool, no longer used
        -> RefactGhc t

addDecl parent pn (declSig, mDeclAnns) topLevel = do
  logm $ "addDecl:declSig=" ++ showGhc declSig
  case mDeclAnns of
    Nothing -> return ()
    Just declAnns -> -- addRefactAnns declAnns
      liftT $ modifyAnnsT (mergeAnns declAnns)
  if isJust pn
    then appendDecl parent (gfromJust "addDecl" pn) declSig
    else if topLevel
           -- then addTopLevelDecl parent declSig
           then addLocalDecl    parent declSig
           else addLocalDecl    parent declSig
 where
  setDeclSpacing newDeclSig n c = do
    -- First clear any previous indentation
    mapM_ (\d -> setPrecedingLinesT d 0 0) newDeclSig
    setPrecedingLinesT (head newDeclSig) n c
--    mapM_ (\d -> setPrecedingLinesT d 1 0) (tail newDeclSig)

  appendDecl :: (HasDecls t)
      => t        -- ^Original AST
      -> GHC.Name -- ^Name to add the declaration after
      -> [GHC.LHsDecl GHC.RdrName] -- ^declaration and maybe sig
      -> RefactGhc t -- ^updated AST
  appendDecl parent' pn' newDeclSig
    = do
         logm $ "addDecl.appendDecl:(pn')=" ++ showGhc pn'
         liftT $ setDeclSpacing newDeclSig 2 0
         nameMap <- getRefactNameMap
         decls <- liftT $ hsDecls parent'
         let
            (before,after) = break (definesDeclRdr nameMap pn') decls

         logm $ "addDecl.appendDecl:(before,after)=" ++ showGhc (before,after)
         let (decls1,decls2) = case after of
               [] -> (before,[])
               _  -> (before ++ [ghead "appendDecl14" after],
                      gtail "appendDecl15" after)
         liftT $ replaceDecls parent' (decls1++newDeclSig++decls2)


{-
  -- ^Add a definition to the beginning of the definition declaration
  -- list, but after the data type declarations if there are any.
  addTopLevelDecl :: (HasDecls t)
       => t -> [GHC.LHsDecl GHC.RdrName]
       -> RefactGhc t
  addTopLevelDecl parent' newDeclSig
    = do decls <- liftT (hsDecls parent')
         liftT $ setDeclSpacing newDeclSig 2 0
         liftT $ replaceDecls parent' (newDeclSig++decls)
-}

  addLocalDecl :: (HasDecls t)
               => t -> [GHC.LHsDecl GHC.RdrName]
               -> RefactGhc t
  addLocalDecl parent' newDeclSig
    = do
         decls <- liftT (hsDecls parent')
         case decls of
           -- [] -> liftT $ setDeclSpacing newDeclSig 1 4
           [] -> liftT $ setDeclSpacing newDeclSig 2 0
           ds -> do
             DP (r,c) <- liftT (getEntryDPT (head ds))
             liftT $ setDeclSpacing newDeclSig r c
             -- liftT $ setPrecedingLinesT (head ds) 1 0
             liftT $ setPrecedingLinesT (head ds) 2 0
         r <- liftT $ replaceDecls parent' (newDeclSig++decls)
         return r

-- ---------------------------------------------------------------------

rdrNameFromName :: Bool -> GHC.Name -> RefactGhc GHC.RdrName
rdrNameFromName useQual newName = do
  mname <- case (GHC.nameModule_maybe newName) of
      Just (GHC.Module _ mn) -> return mn
      Nothing -> do
        GHC.Module _ mn <- getRefactModule
        return mn

  if useQual
    then return $ GHC.mkRdrQual mname (GHC.nameOccName newName)
    else return $ GHC.mkRdrUnqual     (GHC.nameOccName newName)

-- ---------------------------------------------------------------------

-- |Take a list of strings and return a list with the longest prefix
-- of spaces removed
stripLeadingSpaces :: [String] -> [String]
stripLeadingSpaces xs = map (drop n) xs
  where
    n = minimum $ map oneLen xs

    oneLen x = length prefix
      where
        (prefix,_) = break (/=' ') x

-- ---------------------------------------------------------------------

-- | add items to the hiding list of an import declaration which
-- imports the specified module.
addHiding::
     GHC.ModuleName       -- ^ The imported module name
  -> GHC.ParsedSource     -- ^ The current module
  -> [GHC.RdrName]        -- ^ The items to be added
  -> RefactGhc GHC.ParsedSource -- ^ The result
addHiding mn p ns = do
  logm $ "addHiding called for (module,names):" ++ showGhc (mn,ns)
  p' <- addItemsToImport' mn p ns Hide
  putRefactParsed p' emptyAnns
  return p'

-- --------------------------------------------------------------------

-- | Creates a new entity list for hiding a name in an ImportDecl.
mkNewEntList :: [GHC.RdrName] -> RefactGhc [GHC.LIE GHC.RdrName]
mkNewEntList idNames = do
  case idNames of
    [] -> return []
    _ -> do
      newEntsInit <- mapM (mkNewEnt True) (init idNames)
      newEntsLast <- mkNewEnt False (last idNames)
      return (newEntsInit ++ [newEntsLast])

-- | Creates a new entity for hiding a name in an ImportDecl.
mkNewEnt :: Bool -> GHC.RdrName -> RefactGhc (GHC.LIE GHC.RdrName)
mkNewEnt addCommaAnn pn = do
  newSpan <- liftT uniqueSrcSpanT
  let lpn = GHC.L newSpan pn
  if addCommaAnn
    then liftT $ addSimpleAnnT lpn (DP (0,0)) [((G GHC.AnnVal),DP (0,0)),((G GHC.AnnComma),DP (0,0))]
    else liftT $ addSimpleAnnT lpn (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
  return (GHC.L newSpan (GHC.IEVar lpn))

-- | Represents the operation type we want to select on addItemsToImport'
data ImportType = Hide     -- ^ Used for addHiding
                | Import   -- ^ Used for addItemsToImport

-- | Add identifiers (given by the third argument) to the explicit entity list in the declaration importing the
--   specified module name. This function does nothing if the import declaration does not have an explicit entity list.
addItemsToImport::
    GHC.ModuleName       -- ^ The imported module name
  ->GHC.ParsedSource     -- ^ The current module
  ->[GHC.RdrName]        -- ^ The items to be added
--  ->Maybe GHC.Name       -- ^ The condition identifier.
  ->RefactGhc GHC.ParsedSource -- ^ The result
addItemsToImport mn r ns = addItemsToImport' mn r ns Import

-- | Add identifiers (given by the third argument) to the explicit entity list in the declaration importing the
--   specified module name. If the ImportType argument is Hide, then the items will be added to the "hiding"
--   list. If it is Import, they will be added to the explicit import entries. This function does nothing if
--   the import declaration does not have an explicit entity list and ImportType is Import.
addItemsToImport'::
     GHC.ModuleName       -- ^ The imported module name
  -> GHC.ParsedSource     -- ^ The current module
  -> [GHC.RdrName]        -- ^ The items to be added
--  ->Maybe GHC.Name       -- ^ The condition identifier.
  -> ImportType           -- ^ Whether to hide the names or import them. Uses special data for clarity.
  -> RefactGhc GHC.ParsedSource -- ^ The result
addItemsToImport' serverModName (GHC.L l p) pns impType = do
    let imps = GHC.hsmodImports p
    imps' <- mapM inImport imps
    return $ GHC.L l p {GHC.hsmodImports = imps'}
  where
    isHide = case impType of
             Hide   -> True
             Import -> False

    inImport :: GHC.LImportDecl GHC.RdrName -> RefactGhc (GHC.LImportDecl GHC.RdrName)
    inImport imp@(GHC.L _ (GHC.ImportDecl _st (GHC.L _ modName) _qualify _source _safe isQualified _isImplicit _as h))
      | serverModName == modName  && not isQualified -- && (if isJust pn then findPN (gfromJust "addItemsToImport" pn) h else True)
       = case h of
           Nothing                          -> insertEnts imp []   True
           Just (_isHide, (GHC.L _le ents)) -> insertEnts imp ents False
    inImport x = return x

    insertEnts ::
      GHC.LImportDecl GHC.RdrName
      -> [GHC.LIE GHC.RdrName]
      -> Bool -- True means there are already existing entities
      -> RefactGhc ( GHC.LImportDecl GHC.RdrName )
    insertEnts imp ents isNew = do
        logm $ "addItemsToImport':insertEnts:(imp,ents,isNew):" ++ showGhc (imp,ents,isNew)
        if isNew && not isHide then return imp
         else do
            logm $ "addItemsToImport':insertEnts:doing stuff"
            newSpan <- liftT uniqueSrcSpanT
            newEnts <- mkNewEntList pns
            let lNewEnts = GHC.L newSpan (ents++newEnts)
            logm $ "addImportDecl.mkImpDecl:adding anns for:" ++ showGhc lNewEnts
            if isHide
              then
                liftT $ addSimpleAnnT lNewEnts (DP (0,1)) [((G GHC.AnnHiding),DP (0,0)),((G GHC.AnnOpenP),DP (0,1)),((G GHC.AnnCloseP),DP (0,0))]
              else
                liftT $ addSimpleAnnT lNewEnts (DP (0,1)) [((G GHC.AnnOpenP),DP (0,0)),((G GHC.AnnCloseP),DP (0,0))]
            when (not (null ents)) $ do liftT (addTrailingCommaT (last ents))
            return (replaceHiding imp  (Just (isHide, lNewEnts)))


    replaceHiding (GHC.L l1 (GHC.ImportDecl st mn q src safe isQ isImp as _h)) h1 =
                  (GHC.L l1 (GHC.ImportDecl st mn q src safe isQ isImp as h1))

-- ---------------------------------------------------------------------

addParamsToDecls::
        [GHC.LHsDecl GHC.RdrName] -- ^ A declaration list where the function is defined and\/or used.
      -> GHC.Name       -- ^ The function name.
      -> [GHC.RdrName]  -- ^ The parameters to be added.
      -> RefactGhc [GHC.LHsDecl GHC.RdrName] -- ^ The result.

addParamsToDecls decls pn paramPNames = do
  -- logm $ "addParamsToDecls (pn,paramPNames,modifyToks)=" ++ (showGhc (pn,paramPNames,modifyToks))
  -- logm $ "addParamsToDecls: decls=" ++ (SYB.showData SYB.Renamer 0 decls)
  nameMap <- getRefactNameMap
  if (paramPNames /= [])
        then mapM (addParamToDecl nameMap) decls
        else return decls
  where
   addParamToDecl :: NameMap -> GHC.LHsDecl GHC.RdrName -> RefactGhc (GHC.LHsDecl GHC.RdrName)
   addParamToDecl nameMap (GHC.L l1 (GHC.ValD (GHC.FunBind lp@(GHC.L l2 pname) i (GHC.MG matches a ptt o) co fvs t)))
    | eqRdrNamePure nameMap lp pn
    = do
         matches' <- mapM addParamtoMatch matches
         return (GHC.L l1 (GHC.ValD (GHC.FunBind (GHC.L l2 pname) i (GHC.MG matches' a ptt o) co fvs t)))
      where
       addParamtoMatch (GHC.L l (GHC.Match fn1 pats mtyp rhs))
        = do
             rhs' <- addActualParamsToRhs pn paramPNames rhs
             pats' <- liftT $ mapM addParam paramPNames
             return (GHC.L l (GHC.Match fn1 (pats'++pats) mtyp rhs'))

   -- TODO: The following will never match, as a PatBind only deals with complex patterns.
   addParamToDecl _nameMap x@(GHC.L _l1 (GHC.ValD (GHC.PatBind _pat@(GHC.L _l2 (GHC.VarPat _p)) _rhs _ty _fvs _t)))
     = return x
   addParamToDecl _ x = return x

   addParam n = do
     newSpan <- uniqueSrcSpanT
     let vn = (GHC.L newSpan (pNtoPat n))
     addSimpleAnnT vn (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
     return vn

-- ---------------------------------------------------------------------

addActualParamsToRhs :: (SYB.Data t) =>
                        GHC.Name -> [GHC.RdrName] -> t -> RefactGhc t
addActualParamsToRhs pn paramPNames rhs = do
    logm $ "addActualParamsToRhs:entered:(pn,paramPNames)=" ++ showGhc (pn,paramPNames)
    nameMap <- getRefactNameMap
    let
       -- |Limit the action to actual RHS elements
       grhs :: (GHC.GRHSs GHC.RdrName (GHC.LHsExpr GHC.RdrName)) -> RefactGhc (GHC.GRHSs GHC.RdrName (GHC.LHsExpr GHC.RdrName))
       grhs (GHC.GRHSs g lb) = do
         g' <- SYB.everywhereMStaged SYB.Renamer (SYB.mkM worker) g
         return (GHC.GRHSs g' lb)

       worker :: (GHC.LHsExpr GHC.RdrName) -> RefactGhc (GHC.LHsExpr GHC.RdrName)
       worker oldExp@(GHC.L l2 (GHC.HsVar pname))
        | eqRdrNamePure nameMap (GHC.L l2 pname) pn
          = do
              logDataWithAnns "addActualParamsToRhs:oldExp=" oldExp
              newExp' <- liftT $ foldlM addParamToExp oldExp paramPNames

              edp <- liftT $ getEntryDPT oldExp
              liftT $ setEntryDPT oldExp (DP (0,0))
              l2' <- liftT $ uniqueSrcSpanT
              let newExp  = (GHC.L l2' (GHC.HsPar newExp'))
              liftT $ addSimpleAnnT newExp (DP (0,0)) [(G GHC.AnnOpenP,DP (0,0)),(G GHC.AnnCloseP,DP (0,0))]
              liftT $ setEntryDPT newExp edp
              return newExp
       worker x = return x

       addParamToExp :: (GHC.LHsExpr GHC.RdrName) -> GHC.RdrName -> Transform (GHC.LHsExpr GHC.RdrName)
       addParamToExp expr param = do
         ss1 <- uniqueSrcSpanT
         ss2 <- uniqueSrcSpanT
         let var   = GHC.L ss2 (GHC.HsVar param)
         let expr' = GHC.L ss1 (GHC.HsApp expr var)
         addSimpleAnnT var (DP (0,0)) [(G GHC.AnnVal,DP (0,1))]
         addSimpleAnnT expr' (DP (0,0)) []
         return expr'

    r <- applyTP (stop_tdTP (failTP `adhocTP` grhs)) rhs
    return r

{-

The code

    sumSqu (x:xs) = (sq bar2) x + sumSquares xs

results in

          (GRHSs
           [
            ({ LiftToToplevel/D1.hs:(13,15)-(15,16) }
             Just (Ann (DP (0,-1)) [] [] [] Nothing Nothing)
             (GRHS
              []
              ({ LiftToToplevel/D1.hs:13:17-43 }
               Just (Ann (DP (0,1)) [] [] [] Nothing Nothing)
               (OpApp
                ({ LiftToToplevel/D1.hs:13:17-27 }
                 Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                 (HsApp
                  ({ LiftToToplevel/D1.hs:13:17-25 }
                   Just (Ann (DP (0,0)) [] [] [((G AnnOpenP),DP (0,0)),((G AnnCloseP),DP (0,0))] Nothing Nothing)
                   (HsPar
                    ({ LiftToToplevel/D1.hs:13:18-24 }
                     Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                     (HsApp
                      ({ LiftToToplevel/D1.hs:13:18-19 }
                       Just (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                       (HsVar
                        (Unqual {OccName: sq})))
                      ({ LiftToToplevel/D1.hs:13:21-24 }
                       Just (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                       (HsVar
                        (Unqual {OccName: bar2})))))))
                  ({ LiftToToplevel/D1.hs:13:27 }
                   Just (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                   (HsVar
                    (Unqual {OccName: x})))))
                ({ LiftToToplevel/D1.hs:13:29 }
                 Just (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                 (HsVar
                  (Unqual {OccName: +})))
                (PlaceHolder)
                ({ LiftToToplevel/D1.hs:13:31-43 }
                 Just (Ann (DP (0,1)) [] [] [] Nothing Nothing)
                 (HsApp
                  ({ LiftToToplevel/D1.hs:13:31-40 }
                   Just (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                   (HsVar
                    (Unqual {OccName: sumSquares})))
                  ({ LiftToToplevel/D1.hs:13:42-43 }
                   Just (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                   (HsVar
                    (Unqual {OccName: xs})))))))))]

-}
{-
Required end result : (sq pow) x + sumSquares xs

                (L {test/testdata/LiftToToplevel/D2.hs:6:21-46}
                 (OpApp

                  (L {test/testdata/LiftToToplevel/D2.hs:6:21-30}
                   (HsApp
                    (L {test/testdata/LiftToToplevel/D2.hs:6:21-28}
                     (HsPar
                      (L {test/testdata/LiftToToplevel/D2.hs:6:22-27}
                       (HsApp
                        (L {test/testdata/LiftToToplevel/D2.hs:6:22-23}
                         (HsVar {Name: LiftToToplevel.D2.sq}))
                        (L {test/testdata/LiftToToplevel/D2.hs:6:25-27}
                         (HsVar {Name: pow}))))))
                    (L {test/testdata/LiftToToplevel/D2.hs:6:30}
                     (HsVar {Name: x}))))

                  (L {test/testdata/LiftToToplevel/D2.hs:6:32}
                   (HsVar {Name: GHC.Num.+})) {Fixity: infixl 6}
                  (L {test/testdata/LiftToToplevel/D2.hs:6:34-46}
                   (HsApp
                    (L {test/testdata/LiftToToplevel/D2.hs:6:34-43}
                     (HsVar {Name: LiftToToplevel.D2.sumSquares}))
                    (L {test/testdata/LiftToToplevel/D2.hs:6:45-46}
                     (HsVar {Name: xs}))))))))]

Alternate, no parens : sq pow x + sumSquares xs

                (L {test/testdata/LiftToToplevel/D2.hs:6:21-44}
                 (OpApp

                  (L {test/testdata/LiftToToplevel/D2.hs:6:21-28}
                   (HsApp
                    (L {test/testdata/LiftToToplevel/D2.hs:6:21-26}
                     (HsApp
                      (L {test/testdata/LiftToToplevel/D2.hs:6:21-22}
                       (HsVar {Name: LiftToToplevel.D2.sq}))
                      (L {test/testdata/LiftToToplevel/D2.hs:6:24-26}
                       (HsVar {Name: pow}))))
                    (L {test/testdata/LiftToToplevel/D2.hs:6:28}
                     (HsVar {Name: x}))))


                  (L {test/testdata/LiftToToplevel/D2.hs:6:30}
                   (HsVar {Name: GHC.Num.+})) {Fixity: infixl 6}
                  (L {test/testdata/LiftToToplevel/D2.hs:6:32-44}
                   (HsApp
                    (L {test/testdata/LiftToToplevel/D2.hs:6:32-41}
                     (HsVar {Name: LiftToToplevel.D2.sumSquares}))
                    (L {test/testdata/LiftToToplevel/D2.hs:6:43-44}
                     (HsVar {Name: xs}))))))))]


Original : sq x + sumSquares xs

                (L {test/testdata/LiftToToplevel/D2.hs:6:21-40}
                 (OpApp

                  (L {test/testdata/LiftToToplevel/D2.hs:6:21-24}
                   (HsApp
                    (L {test/testdata/LiftToToplevel/D2.hs:6:21-22}
                     (HsVar {Name: sq}))
                    (L {test/testdata/LiftToToplevel/D2.hs:6:24}
                     (HsVar {Name: x}))))


                  (L {test/testdata/LiftToToplevel/D2.hs:6:26}
                   (HsVar {Name: GHC.Num.+})) {Fixity: infixl 6}
                  (L {test/testdata/LiftToToplevel/D2.hs:6:28-40}
                   (HsApp
                    (L {test/testdata/LiftToToplevel/D2.hs:6:28-37}
                     (HsVar {Name: LiftToToplevel.D2.sumSquares}))
                    (L {test/testdata/LiftToToplevel/D2.hs:6:39-40}
                     (HsVar {Name: xs}))))))))]

-}

-- ---------------------------------------------------------------------

-- | Duplicate a function\/pattern binding declaration under a new name
-- right after the original one.
duplicateDecl ::
    [GHC.LHsDecl GHC.RdrName] -- ^ decls to be updated, containing the original decl (and sig)
  ->GHC.Name            -- ^ The identifier whose definition is to be duplicated
  ->GHC.Name            -- ^ The new name (possibly qualified)
  ->RefactGhc [GHC.LHsDecl GHC.RdrName]  -- ^ The result
duplicateDecl decls n newFunName
 = do
     logm $ "duplicateDecl entered:(decls,n,newFunName)=" ++ showGhc (decls,n,newFunName)
     -- decls <- liftT $ hsDecls t
     nm <- getRefactNameMap
     let
       declsToDup = definingDeclsRdrNames nm [n] decls True False
       funBinding = filter isFunOrPatBindP declsToDup     --get the fun binding.
     typeSig   <- undefined --mapM wrapSigT $ definingSigsRdrNames nm [n] decls
     funBinding'' <- renamePN' n newFunName False funBinding
     typeSig'' <- renamePN' n newFunName False typeSig
     logm $ "duplicateDecl:funBinding''=" ++ showGhc funBinding''

     funBinding3 <- mapM (\f@(GHC.L _ fb) -> do
                             newSpan <- liftT uniqueSrcSpanT
                             let fb' = GHC.L newSpan fb
                             liftT $ modifyAnnsT (copyAnn f fb')
                             return fb'
                             ) (typeSig'' ++ funBinding'')
     when (not $ null funBinding3) $ do
                             liftT $ setEntryDPT (head funBinding3) (DP (2,0))
                             liftT $ mapM_ (\d -> setEntryDPT d (DP (1,0))) (tail funBinding3)
     let (decls1,decls2) = break (definesDeclRdr nm n) decls
         (declsToDup',declsRest) = break (not . definesDeclRdr nm n) decls2
     return $ decls1 ++ declsToDup' ++ funBinding3 ++ declsRest

-- ---------------------------------------------------------------------
-- |Divide a declaration list into three parts (before, parent, after)
-- according to the PNT, where 'parent' is the first decl containing
-- the PNT, 'before' are those decls before 'parent' and 'after' are
-- those decls after 'parent'.

divideDecls :: SYB.Data t =>
  [t] -> GHC.Located GHC.Name -> RefactGhc ([t], [t], [t])
divideDecls ds (GHC.L _ pnt) = do
  nm <- getRefactNameMap
  let (before,after) = break (\x -> findNameInRdr nm pnt x) ds
  return $ if (not $ emptyList after)
         then (before, [ghead "divideDecls" after], gtail "divideDecls" after)
         else (ds,[],[])

-- ---------------------------------------------------------------------

-- | Remove the declaration (and the type signature is the second
-- parameter is True) that defines the given identifier from the
-- declaration list.
rmDecl:: (SYB.Data t)
    => GHC.Name     -- ^ The identifier whose definition is to be removed.
    -> Bool         -- ^ True means including the type signature.
    -> t            -- ^ The AST fragment containting the declarations,
                    -- originating from the ParsedSource
    -> RefactGhc
        (t,
        GHC.LHsDecl GHC.RdrName,
        Maybe (GHC.LSig GHC.RdrName))  -- ^ The result and the removed declaration
                                       -- and the possibly removed siganture

rmDecl pn incSig t = do
  setStateStorage StorageNone
  t' <- everywhereMStaged' SYB.Parser (SYB.mkM   inModule
                                      `SYB.extM` inLet
                                      `SYB.extM` inMatch
                                      ) t -- top down
         -- applyTP (once_tdTP (failTP `adhocTP` inBinds)) t
  -- t'  <- everywhereMStaged SYB.Renamer (SYB.mkM inBinds) t
  -- logDataWithAnns "rmDecl:(t')" (t')
  storage <- getStateStorage
  let decl' = case storage of
                StorageDeclRdr decl -> decl
                x                   -> error $ "rmDecl: unexpected value in StateStorage:" ++ (show x)
  setStateStorage StorageNone
  (t'',sig') <- if incSig
                  then rmTypeSig pn t'
                  else return (t', Nothing)
  return (t'',decl',sig')
  where
    inModule (p :: GHC.ParsedSource)
      = doRmDeclList p

    inMatch x@(((GHC.L _ (GHC.Match _ _ _ (GHC.GRHSs _ _localDecls)))):: (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName)))
      = doRmDeclList x

    inLet :: GHC.LHsExpr GHC.RdrName -> RefactGhc (GHC.LHsExpr GHC.RdrName)
    inLet x@(GHC.L ss (GHC.HsLet localDecls expr))
      = do
         isDone <- getDone
         if isDone
           then return x
           else do
             nameMap <- getRefactNameMap
             decls <- liftT $ hsDecls x
             let (decls1,decls2) = break (definesDeclRdr nameMap pn) decls
             if not $ emptyList decls2
                then do
                  let decl = ghead "rmDecl" decls2
                  setStateStorage (StorageDeclRdr decl)
                  case length decls of
                    1 -> do -- Removing the last declaration
                     return expr
                    _ -> do
                     -- logm $ "rmDecl.inLet:length decls /= 1"
                     -- let decls2' = gtail "inLet" decls2
                     decls' <- doRmDecl decls1 decls2
                     x' <- liftT $ replaceDecls x decls'
                     -- logDataWithAnns "inLet" (GHC.L ss (GHC.HsLet localDecls' expr))
                     return $ x'
                else do
                  -- liftT $ replaceDecls localDecls decls
                  return x
    inLet x = return x

    -- ---------------------------------

    doRmDeclList parent
      = do
         isDone <- getDone
         if isDone
           then return parent
           else do
             nameMap <- getRefactNameMap
             decls <- liftT $ hsDecls parent
             let (decls1,decls2) = break (definesDeclRdr nameMap pn) decls
             if not $ emptyList decls2
               then do
                 -- logDataWithAnns "doRmDeclList:(parent)" (parent)
                 let decl = ghead "doRmDeclList" decls2
                 setStateStorage (StorageDeclRdr decl)
                 decls'  <- doRmDecl decls1 decls2
                 parent' <- liftT $ replaceDecls parent decls'
                 -- logDataWithAnns "doRmDeclList:(parent')" (parent')
                 return parent'
               else do
                 -- liftT $ replaceDecls parent decls
                 return parent

    -- ---------------------------------

    getDone = do
      s <- getStateStorage
      case s of
        StorageNone -> return False
        _           -> return True

-- ---------------------------------------------------------------------

-- declsSybTransform :: (SYB.Typeable a, SYB.Typeable t)
--              => (GHC.Located a -> Maybe r) -> t -> Maybe r
declsSybTransform :: (SYB.Typeable a)
  => (forall b. HasDecls b => b -> RefactGhc b)
  -> a -> RefactGhc a
declsSybTransform transform = mt
  where
    mt = SYB.mkM inMatch `SYB.extM` inPatDecl `SYB.extM` inModule
                         `SYB.extM` inHsLet

    inModule :: GHC.ParsedSource -> RefactGhc GHC.ParsedSource
    inModule (modu :: GHC.ParsedSource)
       = transform modu

    inMatch :: GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName) -> RefactGhc (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
    inMatch x@(GHC.L _ (GHC.Match _ _ _ (GHC.GRHSs _ _localDecls)))
       = transform x

    inPatDecl ::GHC.LHsDecl GHC.RdrName -> RefactGhc (GHC.LHsDecl GHC.RdrName)
    inPatDecl x@(GHC.L _ (GHC.ValD (GHC.PatBind _ _ _ _ _)))
       = transform x
    inPatDecl x = return x

    inHsLet :: GHC.LHsExpr GHC.RdrName -> RefactGhc (GHC.LHsExpr GHC.RdrName)
    inHsLet x@(GHC.L _ (GHC.HsLet{}))
       = transform x
    inHsLet x = return x

 ---------------------------------------------------------------------

-- |Utility function to remove a decl from the middle of a list, assuming the
-- list has already been split into a (possibly empty) front before the decl,
-- and a back where the head is the decl to be removed.
doRmDecl :: [GHC.LHsDecl GHC.RdrName] -> [GHC.LHsDecl GHC.RdrName] -> RefactGhc [GHC.LHsDecl GHC.RdrName]
doRmDecl decls1 decls2
  = do
      let decls2'      = gtail "doRmDecl 1" decls2
          declToRemove = head decls2

      -- logDataWithAnns "doRmDecl:(decls1,decls2)" (decls1,decls2)
      unless (null decls1)  $ do liftT $ balanceComments (last decls1) declToRemove
      unless (null decls2') $ do liftT $ balanceComments declToRemove  (head decls2')

      -- decl in the middle of a list
      -- when   (not (null decls1) && not (null decls2')) $ do liftT $ transferEntryDPT (last decls1) (head decls2')

      -- Removing first decl
      -- when (null decls1 && not (null decls2')) $ do liftT $ transferEntryDPT declToRemove (head decls2')
      when (not (null decls2') && null decls1) $ do liftT $ transferEntryDPT declToRemove (head decls2')
      when (not (null decls2') && not (null decls1) && not (isTypeSigDecl (last decls1)))
        $ do liftT $ transferEntryDPT declToRemove (head decls2')

      -- logDataWithAnns "doRmDecl:(decls2')" (decls2')
      return $ (decls1 ++ decls2')

-- ---------------------------------------------------------------------

-- | Remove multiple type signatures
rmTypeSigs :: (SYB.Data t) =>
         [GHC.Name]  -- ^ The identifiers whose type signatures are to be removed.
      -> t           -- ^ The declarations
      -> RefactGhc (t,[GHC.LSig GHC.RdrName])
                     -- ^ The result and removed signatures, if there
                     -- were any
rmTypeSigs pns t = do
  (t',demotedSigsMaybe) <- foldM (\(tee,ds) n -> do { (tee',d) <- rmTypeSig n tee; return (tee', ds++[d])}) (t,[]) pns
  return (t',catMaybes demotedSigsMaybe)

-- ---------------------------------------------------------------------

-- | Remove the type signature that defines the given identifier's
-- type from the declaration list.
rmTypeSig :: (SYB.Data t) =>
         GHC.Name    -- ^ The identifier whose type signature is to be removed.
      -> t           -- ^ The declarations
      -> RefactGhc (t,Maybe (GHC.LSig GHC.RdrName))
                     -- ^ The result and removed signature, if there
                     -- was one
                     -- NOTE: It may have originated from a SigD, it is up to the calling function to insert this if required
rmTypeSig pn t
  = do
     setStateStorage StorageNone
     t' <- SYB.everywhereMStaged SYB.Renamer (SYB.mkM inMatch `SYB.extM` inPatDecl `SYB.extM` inModule) t
     storage <- getStateStorage
     let sig' = case storage of
                  StorageSigRdr sig -> Just sig
                  StorageNone       -> Nothing
                  x -> error $ "rmTypeSig: unexpected value in StateStorage:" ++ (show x)
     return (t',sig')
  where
   inModule :: GHC.ParsedSource -> RefactGhc GHC.ParsedSource
   inModule (modu :: GHC.ParsedSource)
      = doRmTypeSig modu

   -- Deals with the distrinct parts of a FunBind
   inMatch :: GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName) -> RefactGhc (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
   inMatch x@(GHC.L _ (GHC.Match _ _ _ (GHC.GRHSs _ _localDecls)))
      = doRmTypeSig x

   inPatDecl ::GHC.LHsDecl GHC.RdrName -> RefactGhc (GHC.LHsDecl GHC.RdrName)
   inPatDecl x@(GHC.L _ (GHC.ValD (GHC.PatBind _ _ _ _ _)))
      = doRmTypeSig x
   inPatDecl x = return x

   -- inPatBind ::GHC.LHsBind GHC.RdrName -> RefactGhc (GHC.LHsBind GHC.RdrName)
   -- inPatBind x@(GHC.L _ (GHC.PatBind _ _ _ _ _))
   --    = doRmTypeSig x
   -- inPatBind x = return x

   -- ----------------------------------

   doRmTypeSig :: (HasDecls t) => t -> RefactGhc t
   doRmTypeSig parent = do
     isDone <- getDone
     if isDone
       then return parent
       else do
         decls <- liftT $ hsDecls parent
         -- logDataWithAnns "doRmTypeSig:decls" decls
         nameMap <- getRefactNameMap
         let (decls1,decls2)= break (definesSigDRdr nameMap pn) decls
         if not $ null decls2
            then do
              -- logDataWithAnns "doRmTypeSig:parent" parent
              let sig@(GHC.L sspan (GHC.SigD (GHC.TypeSig names typ p))) = ghead "rmTypeSig" decls2
              if length names > 1
                  then do
                      let newNames = filter (\rn -> rdrName2NamePure nameMap rn /= pn) names
                          newSig = GHC.L sspan (GHC.SigD (GHC.TypeSig newNames typ p))

                      liftT $ removeTrailingCommaT (glast "doRmTypeSig" newNames)

                      let pnt = ghead "rmTypeSig" (filter (\rn -> rdrName2NamePure nameMap rn == pn) names)
                      liftT $ removeTrailingCommaT pnt

                      -- Construct the old signature, by keeping the
                      -- signature part but discarding the other names
                      newSpan <- liftT uniqueSrcSpanT
                      let oldSig = (GHC.L newSpan (GHC.TypeSig [pnt] typ p))
                      -- ++AZ++ : could just use new SrcSpan and copy top level annotation
                      -- oldSig' <- liftT $ doCloneT oldSig
                      liftT $ modifyAnnsT (copyAnn sig oldSig)
                      setStateStorage (StorageSigRdr oldSig)

                      parent' <- liftT $ replaceDecls parent (decls1++[newSig]++gtail "doRmTypeSig" decls2)
                      return parent'
                  else do
                      [oldSig] <- undefined --decl2SigT sig
                      setStateStorage (StorageSigRdr oldSig)
                      {-
                      unless (null $ tail decls2) $ do
                        liftT $ transferEntryDPT sig (head $ tail decls2)
                        unless (null decls1) $ do liftT $ balanceComments (last decls1) sig
                        liftT $ balanceComments (head decls2) (head $ tail decls2)
                      -}
                      decls' <- doRmDecl decls1 decls2
                      parent' <- liftT $ replaceDecls parent decls'
                      -- parent' <- liftT $ replaceDecls parent (decls1++tail decls2)
                      return parent'
            else do
              -- liftT $ replaceDecls parent decls
              return parent

   getDone = do
     s <- getStateStorage
     case s of
       StorageNone -> return False
       _           -> return True

-- ---------------------------------------------------------------------
{-
-- | Remove the type signature that defines the given identifier's
-- type from the declaration list.
rmTypeSig :: (SYB.Data t) =>
         GHC.Name    -- ^ The identifier whose type signature is to be removed.
      -> t           -- ^ The declarations
      -> RefactGhc (t,Maybe (GHC.LSig GHC.RdrName))
                     -- ^ The result and removed signature, if there
                     -- was one
                     -- NOTE: It may have originated from a SigD, it is up to the calling function to insert this if required
rmTypeSig pn t
  = do
     setStateStorage StorageNone
     t' <- SYB.everywhereMStaged SYB.Renamer (SYB.mkM inSigs `SYB.extM` inDecls) t
     storage <- getStateStorage
     let sig' = case storage of
                  StorageSigRdr sig -> Just sig
                  StorageNone       -> Nothing
                  x -> error $ "rmTypeSig: unexpected value in StateStorage:" ++ (show x)
     return (t',sig')
  where
   {-
   Note: In ParsedSource, GHC.Sig can occur either as a top level declaration in
   which case it is wrapped in a SigD, or on a ValBindsIn construct.
   TODO: unify the common workings
   -}
   inDecls (decls :: [GHC.LHsDecl GHC.RdrName])
     = do
         let getValDSig s@(GHC.L _ (GHC.SigD _)) = [s]
             getValDSig _ = []
             sigds = concatMap getValDSig decls

             sigFromDecl (GHC.L _ (GHC.SigD s)) = s

         nameMap <- getRefactNameMap
         let definesSigD (GHC.L _ (GHC.SigD s)) = definesTypeSigRdr nameMap pn s
             definesSigD _ = False
         if not $ emptyList (snd (break (definesTypeSigRdr nameMap pn . sigFromDecl) sigds))
            then do
              let (decls1,decls2)= break definesSigD decls
              let sig@(GHC.L sspan (GHC.SigD (GHC.TypeSig names typ p))) = ghead "rmTypeSig" decls2
              if length names > 1
                  then do
                      let newSig = (GHC.L sspan (GHC.SigD (GHC.TypeSig (filter (\rn -> rdrName2NamePure nameMap rn /= pn) names) typ p)))

                      let pnt = ghead "rmTypeSig" (filter (\rn -> rdrName2NamePure nameMap rn == pn) names)

                      -- Construct the old signature, by keeping the
                      -- signature part but discarding the other names
                      let oldSig = (GHC.L sspan (GHC.TypeSig [pnt] typ p))
                      setStateStorage (StorageSigRdr oldSig)

                      return (decls1++[newSig]++tail decls2)
                  else do
                      let oldSig = (GHC.L sspan (sigFromDecl sig))
                      setStateStorage (StorageSigRdr oldSig)
                      -- ++ AZ : Need to take the ED from the removed decl and apply it to the tail.
                      unless (null (tail decls2)) $ do
                        anns <- getRefactAnns
                        setRefactAnns (transferEntryDP anns (head decls2) (head $ tail decls2) )
                      return (decls1++tail decls2)
            else return decls

   inSigs (sigs::[GHC.LSig GHC.RdrName])
     = do
         nameMap <- getRefactNameMap
         if not $ emptyList (snd (break (definesTypeSigRdr nameMap pn . GHC.unLoc) sigs))
            then do
              let (decls1,decls2)= break (definesTypeSigRdr nameMap pn . GHC.unLoc) sigs
              let sig@(GHC.L sspan (GHC.TypeSig names typ p)) = ghead "rmTypeSig" decls2
              if length names > 1
                  then do
                      let newSig = (GHC.L sspan (GHC.TypeSig (filter (\rn -> rdrName2NamePure nameMap rn /= pn) names) typ p))

                      let pnt = ghead "rmTypeSig" (filter (\rn -> rdrName2NamePure nameMap rn == pn) names)

                      -- Construct the old signature, by keeping the
                      -- signature part but discarding the other names
                      let oldSig = (GHC.L sspan (GHC.TypeSig [pnt] typ p))
                      setStateStorage (StorageSigRdr oldSig)

                      return (decls1++[newSig]++tail decls2)
                  else do
                      setStateStorage (StorageSigRdr sig)
                      return (decls1++tail decls2)
            else return sigs
   inSigs x = return x
-}
-- ---------------------------------------------------------------------


-- TODO: Is this function needed with GHC?

-- | Remove the qualifier from the given identifiers in the given syntax phrase.
rmQualifier:: (SYB.Data t)
             =>[GHC.Name]       -- ^ The identifiers.
               ->t           -- ^ The syntax phrase.
               ->RefactGhc t -- ^ The result.
rmQualifier pns t = do
  nm <- getRefactNameMap
  SYB.everywhereM (nameSybTransform (rename nm)) t
    where
     rename nm (ln@(GHC.L l pn)::GHC.Located GHC.RdrName)
       | elem (rdrName2NamePure nm ln) pns
       = do do
               -- let (rs,_) = break (=='.') $ reverse $ showGhc pn -- ++TODO: replace this with the appropriate formulation
               --     s = reverse rs
               -- return (GHC.L l (GHC.mkInternalName (GHC.nameUnique pn) (GHC.mkVarOcc s) l))
              case pn of
                GHC.Qual _ n -> return (GHC.L l (GHC.Unqual n))
                _            -> return ln
     rename _ x = return  x

-- ---------------------------------------------------------------------

-- | Replace all occurences of a top level GHC.Name with a qualified version.
qualifyToplevelName :: GHC.Name -> RefactGhc ()
qualifyToplevelName n = do
    -- renamed <- getRefactRenamed
    parsed <- getRefactParsed
    -- logm $ "qualifyToplevelName:renamed=" ++ (SYB.showData SYB.Renamer 0 renamed)
    -- _ <- renamePN n n True renamed
    parsed' <- renamePN' n n True parsed
    putRefactParsed parsed' emptyAnns
    return ()

-- ---------------------------------------------------------------------

data HowToQual = Qualify | NoQualify | PreserveQualify
               deriving (Show,Eq)

-- | Rename each occurrences of the identifier in the given syntax
-- phrase with the new name.

-- TODO: the syntax phrase is required to be GHC.Located, not sure how
-- to specify this without breaking the everywhereMStaged call

renamePN'::(SYB.Data t)
   =>GHC.Name             -- ^ The identifier to be renamed.
   ->GHC.Name             -- ^ The new name, including possible qualifier
   ->Bool                 -- ^ True means use the qualified form for
                          --   the new name.
   ->t                    -- ^ The syntax phrase
   ->RefactGhc t
renamePN' oldPN newName useQual t = do
  -- logm $ "renamePN: (oldPN,newName)=" ++ (showGhc (oldPN,newName))
  -- logm $ "renamePN: t=" ++ (SYB.showData SYB.Parser 0 t)
  nameMap <- getRefactNameMap
  newNameQual   <- rdrNameFromName True  newName
  newNameUnqual <- rdrNameFromName False newName
  newNameRdr    <- rdrNameFromName useQual newName
  -- logm $ "renamePN: (newNameQual,newNameUnqual,newNameRdr)=" ++ showGhc (newNameQual,newNameUnqual,newNameRdr)

  let
    cond :: GHC.Located GHC.RdrName -> Bool
    cond (GHC.L ln _) =
      case Map.lookup ln nameMap of
        Nothing -> False
        Just n -> GHC.nameUnique n == GHC.nameUnique oldPN

    -- Decision process for new names
    newNameCalcBool :: Bool -> GHC.RdrName -> GHC.RdrName
    newNameCalcBool True  n = newNameCalc Qualify   n
    newNameCalcBool False n = newNameCalc NoQualify n

    newNameCalc :: HowToQual -> GHC.RdrName -> GHC.RdrName
    newNameCalc uq old = newNameCalc' uq (GHC.isQual_maybe old)
      where
        newNameCalc' :: HowToQual -> (Maybe (GHC.ModuleName,GHC.OccName)) -> GHC.RdrName
        newNameCalc' Qualify         (Just (mn,_)) = GHC.Qual mn (GHC.occName newName)
        newNameCalc' PreserveQualify (Just (mn,_)) = GHC.Qual mn (GHC.occName newName)
        newNameCalc' NoQualify       (Just (_n,_)) = GHC.Unqual  (GHC.occName newName)
        newNameCalc' uq' _ =  if uq' == Qualify then newNameQual else newNameUnqual

    rename :: Bool -> GHC.Located GHC.RdrName -> Transform (GHC.Located GHC.RdrName)
    rename useQual' old@(GHC.L l n)
     | cond (GHC.L l n)
     = do
          logTr $ "renamePN:rename at :" ++ showGhc l
          let nn = newNameCalcBool useQual' n
          -- A RdrName Can have a number of constructors, which are used to
          -- index the annotations associated with it. Make sure the annotation
          -- lines up.
          let new = (GHC.L l nn)
          modifyAnnsT (replaceAnnKey old new)

          return new
    rename _ x = return x

    renameVar :: Bool -> (GHC.Located (GHC.HsExpr GHC.RdrName)) -> Transform (GHC.Located (GHC.HsExpr GHC.RdrName))
    renameVar useQual' (GHC.L l (GHC.HsVar n))
     | cond (GHC.L l n)
     = do
          logTr $ "renamePN:renameVar at :" ++ (showGhc l)
          -- logTr $ "renamePN:renameVar useQual' :" ++ (show useQual')
          -- logTr $ "renamePN:renameVar ln :" ++ SYB.showData SYB.Parser 0 (GHC.L l (GHC.HsVar n))
          let
            nn = if useQual'
                   then newNameCalcBool useQual'        n
                   else newNameCalc     PreserveQualify n
          return (GHC.L l (GHC.HsVar nn))
    renameVar _ x = return x

    -- HsTyVar {Name: Renaming.D1.Tree}))
    renameTyVar :: Bool -> (GHC.Located (GHC.HsType GHC.RdrName)) -> Transform (GHC.Located (GHC.HsType GHC.RdrName))
    renameTyVar useQual' (GHC.L l (GHC.HsTyVar n))
     | cond (GHC.L l n)
     = do
          logTr $ "renamePN:renameTyVar at :" ++ (showGhc l)
          let nn = newNameCalcBool useQual' n
          return (GHC.L l (GHC.HsTyVar nn))
    renameTyVar _ x = return x


    renameHsTyVarBndr :: Bool -> GHC.LHsTyVarBndr GHC.RdrName -> Transform (GHC.LHsTyVarBndr GHC.RdrName)
    renameHsTyVarBndr useQual' (GHC.L l (GHC.UserTyVar n))
     | cond (GHC.L l n)
     = do
          logTr $ "renamePN:renameHsTyVarBndr at :" ++ (showGhc l)
          let nn = newNameCalcBool useQual' n
          return (GHC.L l (GHC.UserTyVar nn))
    renameHsTyVarBndr _ x = return x

    -- ---------------------------------

    renameLIE :: Bool -> (GHC.LIE GHC.RdrName) -> Transform (GHC.LIE GHC.RdrName)
    renameLIE useQual' (GHC.L l (GHC.IEVar old@(GHC.L ln n)))
     | cond (GHC.L ln n)
     = do
          -- logTr $ "renamePN:renameLIE.IEVar at :" ++ (showGhc l)
          let new = newNameCalcBool useQual' n

          let newn = (GHC.L ln new)
          modifyAnnsT (replaceAnnKey old newn)

          return (GHC.L l (GHC.IEVar (GHC.L ln new)))

    renameLIE useQual' (GHC.L l (GHC.IEThingAbs old@(GHC.L ln n)))
     | cond (GHC.L l n)
     = do
          -- logTr $ "renamePN:renameLIE.IEThingAbs at :" ++ (showGhc l)
          let new = newNameCalcBool useQual' n

          let newn = (GHC.L ln new)
          modifyAnnsT (replaceAnnKey old newn)

          return (GHC.L l (GHC.IEThingAbs (GHC.L ln new)))

    renameLIE useQual' (GHC.L l (GHC.IEThingAll old@(GHC.L ln n)))
     | cond (GHC.L ln n)
     = do
          -- logTr $ "renamePN:renameLIE.IEThingAll at :" ++ (showGhc l)
          let new = newNameCalcBool useQual' n

          let newn = (GHC.L ln new)
          modifyAnnsT (replaceAnnKey old newn)

          return (GHC.L l (GHC.IEThingAll (GHC.L ln new)))

    -- TODO: check inside the ns here too
    renameLIE useQual' (GHC.L l (GHC.IEThingWith old@(GHC.L ln n) ns))
     = do

         old' <- if (cond (GHC.L ln n))
           then do
             logTr $ "renamePN:renameLIE.IEThingWith at :" ++ (showGhc l)
             let new = newNameCalcBool useQual' n

             let newn = (GHC.L ln new)
             modifyAnnsT (replaceAnnKey old newn)

             return (GHC.L ln new)
           else return old


         ns' <- if (any (\(GHC.L lnn nn) -> cond (GHC.L lnn nn)) ns)
           then renameTransform useQual' ns
           else return ns
         return (GHC.L l (GHC.IEThingWith old' ns'))

    renameLIE _ x = do
         -- logTr $ "renamePN:renameLIE miss for :" ++ (showGhc x)
         return x

    -- ---------------------------------

    renameLPat :: Bool -> (GHC.LPat GHC.RdrName) -> Transform (GHC.LPat GHC.RdrName)
    renameLPat useQual' (GHC.L l (GHC.VarPat n))
     | cond (GHC.L l n)
     = do
          logTr $ "renamePNworker:renameLPat at :" ++ (showGhc l)
          let nn = newNameCalcBool useQual' n
          return (GHC.L l (GHC.VarPat nn))
    renameLPat _ x = return x

    renameFunBind :: Bool -> GHC.HsBindLR GHC.RdrName GHC.RdrName -> Transform (GHC.HsBindLR GHC.RdrName GHC.RdrName)
    renameFunBind _useQual (GHC.FunBind (GHC.L ln n) fi (GHC.MG matches a typ o) co fvs tick)
     -- | (GHC.nameUnique n == GHC.nameUnique oldPN) || (GHC.nameUnique n == GHC.nameUnique newName)
     | cond (GHC.L ln n) -- || (GHC.nameUnique n == GHC.nameUnique newName)
     = do -- Need to (a) rename the actual funbind name
          --         NOTE: due to bottom-up traversal, (a) should
          --               already have been done.
          --         (b) rename each of 'tail matches'
          --             (head is renamed in (a) )
          -- logTr $ "renamePN.renameFunBind"
          -- Now do (b)
          logTr $ "renamePN.renameFunBind.renameFunBind:starting matches"
          let w lmatch@(GHC.L lm (GHC.Match mln pats typ' grhss)) = do
                case mln of
                  Just (old@(GHC.L lmn _),f) -> do
                    -- A RdrName Can have a number of constructors, which are used to
                    -- index the annotations associated with it. Make sure the annotation
                    -- lines up.
                    let new = (GHC.L lmn newNameUnqual)
                    modifyAnnsT (replaceAnnKey old new)

                    return (GHC.L lm (GHC.Match (Just (new,f)) pats typ' grhss))
                  Nothing -> return lmatch
          matches' <- mapM w matches
          logTr $ "renamePN.renameFunBind.renameFunBind.renameFunBind:matches done"
          return (GHC.FunBind (GHC.L ln newNameRdr) fi (GHC.MG matches' a typ o) co fvs tick)
    renameFunBind _ x = return x

    renameImportDecl :: Bool -> (GHC.ImportDecl GHC.RdrName) -> Transform (GHC.ImportDecl GHC.RdrName)
    renameImportDecl _useQual (GHC.ImportDecl src mn mq isrc isafe iq ii ma (Just (ij,GHC.L ll ies))) = do
      ies' <- mapM (renameLIE False) ies
      logTr $ "renamePN'.renameImportDecl:(ies,ies')=" ++ showGhc (ies,ies')
      return (GHC.ImportDecl src mn mq isrc isafe iq ii ma (Just (ij,GHC.L ll ies')))
    renameImportDecl _ x = return x

    renameTypeSig :: Bool -> (GHC.Sig GHC.RdrName) -> Transform (GHC.Sig GHC.RdrName)
    renameTypeSig _useQual (GHC.TypeSig ns typ p)
     = do
         logTr $ "renamePN:renameTypeSig"
         -- Has already been renamed, make sure qualifier is removed
         ns'  <- renameTransform False ns
         typ' <- renameTransform False typ
         logTr $ "renamePN:renameTypeSig done"
         return (GHC.TypeSig ns' typ' p)
    renameTypeSig _ x = return x

    renameTransform useQual' t' =
          -- Note: bottom-up traversal (no ' at end)
            (SYB.everywhereM (
            -- (everywhereM' (
                   SYB.mkM   (rename            useQual')
                  `SYB.extM` (renameVar         useQual')
                  `SYB.extM` (renameTyVar       useQual')
                  `SYB.extM` (renameHsTyVarBndr useQual')
                  `SYB.extM` (renameLIE         useQual')
                  `SYB.extM` (renameLPat        useQual')
                  `SYB.extM` (renameTypeSig     useQual')
                  `SYB.extM` (renameImportDecl  useQual')
                  `SYB.extM` (renameFunBind     useQual')
                   ) t')
  -- ans <- getRefactAnns
  -- let (t',(ans',_),logOut) = runTransform ans (renameTransform useQual t)
  -- logm $ "renamePN':transform:\n" ++ unlines logOut
  -- setRefactAnns ans'
  t' <- liftT (renameTransform useQual t)
  return t'

-- ---------------------------------------------------------------------
{-
-- | Rename each occurrences of the identifier in the given syntax
-- phrase with the new name.

-- TODO: the syntax phrase is required to be GHC.Located, not sure how
-- to specify this without breaking the everywhereMStaged call

renamePN::(SYB.Data t)
   =>GHC.Name             -- ^ The identifier to be renamed.
   ->GHC.Name             -- ^ The new name, including possible qualifier
   ->Bool                 -- ^ True means use the qualified form for
                          --   the new name.
   ->t                    -- ^ The syntax phrase
   ->RefactGhc t
renamePN oldPN newName useQual t = do
  -- = error $ "renamePN: sspan=" ++ (showGhc sspan) -- ++AZ++
  -- logm $ "renamePN': (oldPN,newName)=" ++ (showGhc (oldPN,newName))
  -- logm $ "renamePN: t=" ++ (SYB.showData SYB.Renamer 0 t)
  -- Note: bottom-up traversal
  let isRenamed = SYB.something
                   (Nothing `SYB.mkQ` isRenamedSource `SYB.extQ` isRenamedGroup) t


  t' <- if isRenamed == (Just True)
    then
      SYB.everywhereM
                 (SYB.mkM renameRenamedSource
                 `SYB.extM` renameGroup
                 ) t
    else
      renamePNworker oldPN newName useQual t
  -- t'' <- adjustLayoutAfterRename oldPN newName t'
  return t'
  where
    isRenamedSource :: GHC.RenamedSource -> Maybe Bool
    isRenamedSource (_g,_i,_e,_d) = Just True

    isRenamedGroup :: GHC.HsGroup GHC.Name -> Maybe Bool
    isRenamedGroup _g = Just True

    -- ---------------------------------

    renameRenamedSource :: GHC.RenamedSource -> RefactGhc GHC.RenamedSource
    renameRenamedSource (g,i,e,d) = do
      i' <- renamePNworker oldPN newName False i
      e' <- renamePNworker oldPN newName useQual e
      return (g,i',e',d)

    renameGroup :: (GHC.HsGroup GHC.Name) -> RefactGhc (GHC.HsGroup GHC.Name)
    renameGroup  g
     = do
          logm $ "renamePN:renameGroup"
          g' <- renamePNworker oldPN newName useQual g
          return g'
    -- renameGroup x = return x

-- ---------------------------------------------------------------------

-- | Rename each occurrences of the identifier in the given syntax
-- phrase with the new name. If the Bool parameter is True, then
-- modify both the AST and the token stream, otherwise only modify the
-- AST.
-- TODO: the syntax phrase is required to be GHC.Located, not sure how
-- to specify this without breaking the everywhereMStaged call
renamePNworker::(SYB.Data t)
   =>GHC.Name             -- ^ The identifier to be renamed.
   ->GHC.Name             -- ^ The new name, including possible qualifier
   ->Bool                 -- ^ True means use the qualified form for
                          --   the new name.
   ->t                    -- ^ The syntax phrase
   ->RefactGhc t
renamePNworker oldPN newName useQual t = do
  -- logm $ "renamePN: (oldPN,newName)=" ++ (showGhc (oldPN,newName))
  -- Note: bottom-up traversal (no ' at end)
  SYB.everywhereM (SYB.mkM rename
                  `SYB.extM` renameVar
                  `SYB.extM` renameTyVar
                  `SYB.extM` renameHsTyVarBndr
                  `SYB.extM` renameLIE
                  `SYB.extM` renameLPat
                  `SYB.extM` renameTypeSig
                  `SYB.extM` renameFunBind
                  ) t
  where
    rename :: GHC.Located GHC.Name -> RefactGhc (GHC.Located GHC.Name)
    rename (GHC.L l n)
     | GHC.nameUnique n == GHC.nameUnique oldPN
     = do
          logm $ "renamePNworker:rename at :" ++ show l
          -- drawTokenTree "before worker" -- ++AZ++ debug
          worker useQual l Nothing
          return (GHC.L l newName)
    rename x = return x

    renameVar :: (GHC.Located (GHC.HsExpr GHC.Name)) -> RefactGhc (GHC.Located (GHC.HsExpr GHC.Name))
    renameVar v@(GHC.L l (GHC.HsVar n))
     | (GHC.nameUnique n == GHC.nameUnique oldPN)
     = do
          logm $ "renamePNworker:renameVar at :" ++ (showGhc l)

          -- Get the original qualification, if any
          rn <- (getParsedForRenamedLocated v :: RefactGhc (GHC.LHsExpr GHC.RdrName))
          let (GHC.L _ (GHC.HsVar mqn)) = rn
          let mrnq = GHC.isQual_maybe mqn
          logm $ "renamePNworker:renameVar mrn,mrnq :" ++ (showGhc (rn,mrnq))

          worker useQual l mrnq
          return (GHC.L l (GHC.HsVar newName))
    renameVar x = return x

    -- HsTyVar {Name: Renaming.D1.Tree}))
    renameTyVar :: (GHC.Located (GHC.HsType GHC.Name)) -> RefactGhc (GHC.Located (GHC.HsType GHC.Name))
    renameTyVar v@(GHC.L l (GHC.HsTyVar n))
     | GHC.nameUnique n == GHC.nameUnique oldPN
     = do
          logm $ "renamePNworker:renameTyVar at :" ++ (showGhc l)

          -- Get the original qualification, if any
          rn <- (getParsedForRenamedLocated v :: RefactGhc (GHC.LHsType GHC.RdrName))
          let (GHC.L _ (GHC.HsTyVar mqn)) = rn
          let mrnq = GHC.isQual_maybe mqn
          logm $ "renamePNworker:renameVar mrn,mrnq :" ++ (showGhc (rn,mrnq))

          worker useQual l mrnq
          return (GHC.L l (GHC.HsTyVar newName))
    renameTyVar x = return x


    renameHsTyVarBndr :: GHC.LHsTyVarBndr GHC.Name -> RefactGhc (GHC.LHsTyVarBndr GHC.Name)
    renameHsTyVarBndr v@(GHC.L l (GHC.UserTyVar n))
     | GHC.nameUnique n == GHC.nameUnique oldPN
     = do
          logm $ "renamePNworker:renameHsTyVarBndr at :" ++ (showGhc l)

          -- Get the original qualification, if any
          rn <- (getParsedForRenamedLocated v :: RefactGhc (GHC.LHsTyVarBndr GHC.RdrName))
          let (GHC.L _ (GHC.UserTyVar mqn)) = rn
          let mrnq = GHC.isQual_maybe mqn
          logm $ "renamePNworker:renameVar mrn,mrnq :" ++ (showGhc (rn,mrnq))

          worker useQual l mrnq
          return (GHC.L l (GHC.UserTyVar newName))
    renameHsTyVarBndr x = return x

    -- ---------------------------------

    renameLIE :: (GHC.LIE GHC.Name) -> RefactGhc (GHC.LIE GHC.Name)
    renameLIE (GHC.L l (GHC.IEVar (GHC.L ln n)))
     | (GHC.nameUnique n == GHC.nameUnique oldPN)
     = do
          -- logm $ "renamePNworker:renameLIE.IEVar at :" ++ (showGhc l)
          worker useQual l Nothing
          return (GHC.L l (GHC.IEVar (GHC.L ln newName)))

    renameLIE (GHC.L l (GHC.IEThingAbs (GHC.L ln n)))
     | GHC.nameUnique n == GHC.nameUnique oldPN
     = do
          -- logm $ "renamePNworker:renameLIE.IEThingAbs at :" ++ (showGhc l)
          worker useQual l Nothing
          return (GHC.L l (GHC.IEThingAbs (GHC.L ln newName)))

    renameLIE (GHC.L l (GHC.IEThingAll (GHC.L ln n)))
     | GHC.nameUnique n == GHC.nameUnique oldPN
     = do
          -- logm $ "renamePNworker:renameLIE.IEThingAll at :" ++ (showGhc l)
          worker useQual l Nothing
          return (GHC.L l (GHC.IEThingAll (GHC.L ln newName)))

    -- TODO: check inside the ns here too
    renameLIE (GHC.L l (GHC.IEThingWith (GHC.L ln n) ns))
     | GHC.nameUnique n == GHC.nameUnique oldPN
     = do
          logm $ "renamePNworker:renameLIE.IEThingWith at :" ++ (showGhc l)
          worker useQual l Nothing
          return (GHC.L l (GHC.IEThingWith (GHC.L ln newName) ns))
     | any (\(GHC.L _ nn) -> (GHC.nameUnique nn == GHC.nameUnique oldPN)) ns
     = do
          {-
          -- We have to find the right token, no locations to help
          toks <- getToksForSpan l
          -- find the opening parenthesis
          let (_,pt) = break isOpenParen $ filter (not . isWhiteSpaceOrIgnored) toks
          -- logm $ "renamePNworker:renameLIE.IEThingWith ns pt=" ++ (show pt)
          let nstoks = gtail "renamePNworker" pt
          let unQualOld = (GHC.occNameString $ GHC.getOccName oldPN)
          -- logm $ "renamePNworker:renameLIE.IEThingWith unquaOld=" ++ (show unQualOld)
          let _tok@(GHC.L lt _,_) = ghead "renamePNworker" $ filter (\tt -> tokenCon tt == showGhc oldPN || tokenCon tt == unQualOld) nstoks
          -- logm $ "renamePNworker:renameLIE.IEThingWith ns tok=" ++ (show _tok)
          logm $ "renamePNworker:renameLIE.IEThingWith ns at :" ++ (showGhc lt)
          -}
          worker useQual l Nothing
          -- TODO: update ns
          return (GHC.L l (GHC.IEThingWith (GHC.L ln newName) ns))

    renameLIE x = do
         -- logm $ "renamePNworker:renameLIE miss for :" ++ (showGhc x)
         return x

    -- ---------------------------------


    renameLPat :: (GHC.LPat GHC.Name) -> RefactGhc (GHC.LPat GHC.Name)
    renameLPat v@(GHC.L l (GHC.VarPat n))
     | (GHC.nameUnique n == GHC.nameUnique oldPN)
     = do
          logm $ "renamePNworker:renameLPat at :" ++ (showGhc l)

          -- Get the original qualification, if any
          rn <- (getParsedForRenamedLocated v :: RefactGhc (GHC.LPat GHC.RdrName))
          let (GHC.L _ (GHC.VarPat mqn)) = rn
          let mrnq = GHC.isQual_maybe mqn
          logm $ "renamePNworker:renameVar mrn,mrnq :" ++ (showGhc (rn,mrnq))

          worker False l mrnq
          return (GHC.L l (GHC.VarPat newName))
    renameLPat x = return x

    renameFunBind :: GHC.LHsBindLR GHC.Name GHC.Name -> RefactGhc (GHC.LHsBindLR GHC.Name GHC.Name)
    renameFunBind (GHC.L l (GHC.FunBind (GHC.L ln n) fi (GHC.MG matches a typ o) co fvs tick))
     | (GHC.nameUnique n == GHC.nameUnique oldPN) || (GHC.nameUnique n == GHC.nameUnique newName)
     = do -- Need to (a) rename the actual funbind name
          --         NOTE: due to bottom-up traversal, (a) should
          --               already have been done.
          --         (b) rename each of 'tail matches'
          --             (head is renamed in (a) )
          -- logm $ "renamePNWorker.renameFunBind"
          worker False ln Nothing
          -- Now do (b)
          logm $ "renamePNWorker.renameFunBind.renameFunBind:starting matches"
          let w (GHC.L _lm (GHC.Match mln _pats _typ (GHC.GRHSs _grhs _lb))) = do
                case mln of
                  Just (GHC.L lmn _,_) -> worker False lmn Nothing
                  Nothing -> return ()
          mapM_ w $ gtail "renameFunBind" matches
          logm $ "renamePNWorker.renameFunBind.renameFunBind.renameFunBind:matches done"
          return (GHC.L l (GHC.FunBind (GHC.L ln newName) fi (GHC.MG matches a typ o) co fvs tick))
    renameFunBind x = return x

    renameTypeSig :: (GHC.LSig GHC.Name) -> RefactGhc (GHC.LSig GHC.Name)
    renameTypeSig (GHC.L l (GHC.TypeSig ns typ p))
     = do
         -- logm $ "renamePNWorker:renameTypeSig"
         -- _ns' <- renamePN oldPN newName False ns
         -- Has already been renamed, make sure qualifier is removed
         ns' <- renamePN newName newName False ns
         typ' <- renamePN oldPN newName False typ
         -- logm $ "renamePNWorker:renameTypeSig done"
         return (GHC.L l (GHC.TypeSig ns' typ' p))
    renameTypeSig x = return x

    -- The param l is only useful for the start of the token pos
    worker :: Bool -> GHC.SrcSpan -> Maybe (GHC.ModuleName, GHC.OccName) -> RefactGhc ()
    worker useQual' l mmo
     = do    logm $ "renamePNWorker.worker:(useQual',l,mmo)=" ++ showGhc (useQual',l,mmo)
             newTok <- case mmo of
                   Nothing -> rdrNameFromName useQual' newName
                   Just (modu,_) -> do
                     logm $ "renamePNWorker.worker:modu=" ++ showGhcQual modu
                     newName' <- mkNewGhcName (Just $ GHC.mkModule GHC.mainPackageKey modu)
                                              (GHC.occNameString $ GHC.getOccName newName)
                     rdrNameFromName True newName'
             replaceRdrName (GHC.L l newTok)
             return ()
-}
----------------------------------------------------------------------------------------
-- | Check whether the specified identifier is declared in the given syntax phrase t,
-- if so, rename the identifier by creating a new name automatically. If the Bool parameter
-- is True, the token stream will be modified, otherwise only the AST is modified.

autoRenameLocalVar:: (HasDecls t)
                     => GHC.Name    -- ^ The identifier.
                     -> t           -- ^ The syntax phrase.
                     -> RefactGhc t -- ^ The result.
autoRenameLocalVar pn t = do
  logm $ "autoRenameLocalVar: (pn)=" ++ (showGhc (pn))
  -- = everywhereMStaged SYB.Renamer (SYB.mkM renameInMatch)
  nm <- getRefactNameMap
  decls <- liftT $ hsDecls t
  if isDeclaredInRdr nm pn decls
         then do t' <- worker t
                 return t'
         else do return t

      where
         worker :: (SYB.Data t) => t -> RefactGhc t
         worker tt
                   =do (f,d) <- hsFDNamesFromInsideRdr tt
                       ds <- hsVisibleNamesRdr pn tt
                       let newNameStr = mkNewName (nameToString pn) (nub (f `union` d `union` ds)) 1
                       newName <- mkNewGhcName Nothing newNameStr
                       renamePN' pn newName False tt

-- ---------------------------------------------------------------------

-- | Show a list of entities, the parameter f is a function that
-- specifies how to format an entity.
showEntities:: (t->String) -> [t] ->String
showEntities _ [] = ""
showEntities f [pn] = f pn
showEntities f (pn:pns) =f pn ++ "," ++ showEntities f pns


-- ---------------------------------------------------------------------

isMainModule :: GHC.Module -> Bool
isMainModule modu = GHC.modulePackageKey modu == GHC.mainPackageKey

-- ---------------------------------------------------------------------

-- | Return the identifier's defining location.
-- defineLoc::PNT->SrcLoc
defineLoc :: GHC.Located GHC.Name -> GHC.SrcLoc
defineLoc (GHC.L _ name) = GHC.nameSrcLoc name

-- | Return the identifier's source location.
-- useLoc::PNT->SrcLoc
useLoc:: (GHC.Located GHC.Name) -> GHC.SrcLoc
-- useLoc (GHC.L l _) = getGhcLoc l
useLoc (GHC.L l _) = GHC.srcSpanStart l

-- ---------------------------------------------------------------------

-- | Return True if the identifier is used in the RHS if a
-- function\/pattern binding.
isUsedInRhs::(SYB.Data t) => (GHC.Located GHC.Name) -> t -> Bool
isUsedInRhs pnt t = useLoc pnt /= defineLoc pnt  && not (notInLhs)
  where
    notInLhs = fromMaybe False $ SYB.somethingStaged SYB.Parser Nothing
            (Nothing `SYB.mkQ` inMatch `SYB.extQ` inDecl) t
     where
      inMatch ((GHC.FunBind name _ (GHC.MG _matches _ _ _) _ _ _) :: GHC.HsBind GHC.Name)
         | isJust (find (sameOccurrence pnt) [name]) = Just True
      inMatch _ = Nothing

      inDecl ((GHC.TypeSig is _ _) :: GHC.Sig GHC.Name)
        |isJust (find (sameOccurrence pnt) is)   = Just True
      inDecl _ = Nothing

-- ---------------------------------------------------------------------
-- | Find all occurrences with location of the given name
findAllNameOccurences :: (SYB.Data t) => GHC.Name -> t -> [(GHC.Located GHC.Name)]
findAllNameOccurences  name t
  = res
       where
        res = SYB.everythingStaged SYB.Renamer (++) []
            ([] `SYB.mkQ` worker `SYB.extQ` workerBind `SYB.extQ` workerExpr) t

        worker (ln@(GHC.L _l n) :: (GHC.Located GHC.Name))
          | GHC.nameUnique n == GHC.nameUnique name = [ln]
        worker _ = []

        workerBind (GHC.L l (GHC.VarPat n) :: (GHC.Located (GHC.Pat GHC.Name)))
          | GHC.nameUnique n == GHC.nameUnique name  = [(GHC.L l n)]
        workerBind _ = []

        workerExpr (GHC.L l (GHC.HsVar n) :: (GHC.Located (GHC.HsExpr GHC.Name)))
          | GHC.nameUnique n == GHC.nameUnique name  = [(GHC.L l n)]
        workerExpr _ = []

-- ---------------------------------------------------------------------

findNameInRdr :: (SYB.Data t) => NameMap -> GHC.Name -> t -> Bool
findNameInRdr nm pn t =
 isJust $ SYB.something (Nothing `SYB.mkQ` worker) t
   where
      worker (ln :: GHC.Located GHC.RdrName)
         | GHC.nameUnique pn == GHC.nameUnique (rdrName2NamePure nm ln) = Just True
      worker _ = Nothing

-- ---------------------------------------------------------------------

-- | Return True if the identifier occurs in the given syntax phrase.
findPNT::(SYB.Data t) => GHC.Located GHC.Name -> t -> Bool
findPNT (GHC.L _ pn) = findPN pn

-- | Return True if the identifier occurs in the given syntax phrase.
findPN::(SYB.Data t)=> GHC.Name -> t -> Bool
findPN pn
   = isJust . SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` worker)
     where
        worker (n::GHC.Name)
           | GHC.nameUnique pn == GHC.nameUnique n = Just True
        worker _ = Nothing

-- | Return True if any of the specified PNames ocuur in the given syntax phrase.
findPNs::(SYB.Data t)=> [GHC.Name] -> t -> Bool
findPNs pns
   = isJust . SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` worker)
     where
        uns = map GHC.nameUnique pns

        worker (n::GHC.Name)
           | elem (GHC.nameUnique n) uns = Just True
        worker _ = Nothing

-- ---------------------------------------------------------------------

-- | Return True if any of the specified PNames ocuur in the given syntax phrase.
findNamesRdr :: (SYB.Data t) => NameMap -> [GHC.Name] -> t -> Bool
findNamesRdr nm pns t =
  isJust $ SYB.something (inName) t
    where
      -- r = (SYB.everythingStaged SYB.Parser mappend mempty (inName) t)

      checker :: GHC.Located GHC.RdrName -> Maybe Bool
      checker ln
         | elem (GHC.nameUnique (rdrName2NamePure nm ln)) uns = Just True
      checker _ = Nothing

      inName :: (SYB.Typeable a) => a -> Maybe Bool
      inName = nameSybQuery checker

      uns = map GHC.nameUnique pns

      -- worker (ln :: GHC.Located GHC.RdrName)
      --    | elem (GHC.nameUnique (rdrName2NamePure nm ln)) uns = Just True
      -- worker _ = Nothing
{-
findNamesRdr nm pns t =
 isJust $ SYB.something (Nothing `SYB.mkQ` worker) t
   where
      uns = map GHC.nameUnique pns

      worker (ln :: GHC.Located GHC.RdrName)
         | elem (GHC.nameUnique (rdrName2NamePure nm ln)) uns = Just True
      worker _ = Nothing
-}

-- ---------------------------------------------------------------------

-- | Return the type checked `GHC.Id` corresponding to the given
-- `GHC.Name`

-- TODO: there has to be a simpler way, using the appropriate GHC internals
findIdForName :: GHC.Name -> RefactGhc (Maybe GHC.Id)
findIdForName n = do
  tm <- getTypecheckedModule
  let t = GHC.tm_typechecked_source tm
  let r = SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` worker) t
      worker (i::GHC.Id)
         | (GHC.nameUnique n) ==  (GHC.varUnique i) = Just i
      worker _ = Nothing
  return r

-- ---------------------------------------------------------------------

getTypeForName :: GHC.Name -> RefactGhc (Maybe GHC.Type)
getTypeForName n = do
  mId <-  findIdForName n
  case mId of
    Nothing -> return Nothing
    Just i -> return $ Just (GHC.varType i)

-- ---------------------------------------------------------------------

-- | Given the syntax phrase, find the largest-leftmost expression
-- contained in the region specified by the start and end position, if
-- found.
locToExp:: (SYB.Data t,SYB.Typeable n) =>
                   SimpPos    -- ^ The start position.
                -> SimpPos    -- ^ The end position.
                -> t          -- ^ The syntax phrase.
                -> Maybe (GHC.Located (GHC.HsExpr n)) -- ^ The result.
locToExp beginPos endPos t = res
  where
     res = SYB.somethingStaged SYB.Parser Nothing (Nothing `SYB.mkQ` expr) t

     expr :: GHC.Located (GHC.HsExpr n) -> (Maybe (GHC.Located (GHC.HsExpr n)))
     expr e
        |inScope e = Just e
     expr _ = Nothing

     inScope :: GHC.Located e -> Bool
     inScope (GHC.L l _) =
       let
         (startLoc,endLoc) = case l of
           (GHC.RealSrcSpan ss) ->
             ((GHC.srcSpanStartLine ss,GHC.srcSpanStartCol ss),
              (GHC.srcSpanEndLine ss,GHC.srcSpanEndCol ss))
           (GHC.UnhelpfulSpan _) -> ((0,0),(0,0))
       in
        (startLoc>=beginPos) && (startLoc<= endPos) && (endLoc>= beginPos) && (endLoc<=endPos)

--------------------------------------------------------------------------------


ghcToPN :: GHC.RdrName -> PName
ghcToPN rdr = PN rdr

lghcToPN :: GHC.Located GHC.RdrName -> PName
lghcToPN (GHC.L _ rdr) = PN rdr


-- | If an expression consists of only one identifier then return this
-- identifier in the GHC.Name format, otherwise return the default Name
expToName:: GHC.LHsExpr GHC.Name -> GHC.Name -- TODO: Use a Maybe, rather than defaultName
expToName (GHC.L _ (GHC.HsVar pnt)) = pnt
expToName (GHC.L _ (GHC.HsPar e))   = expToName e
expToName _ = defaultName

-- | If an expression consists of only one identifier then return this
-- identifier in the GHC.Name format, otherwise return the default Name
expToNameRdr :: NameMap -> GHC.LHsExpr GHC.RdrName -> Maybe GHC.Name
expToNameRdr nm (GHC.L l (GHC.HsVar pnt)) = Just (rdrName2NamePure nm (GHC.L l pnt))
expToNameRdr nm (GHC.L _ (GHC.HsPar e))   = expToNameRdr nm e
expToNameRdr _ _ = Nothing

nameToString :: GHC.Name -> String
-- nameToString name = showGhc name
nameToString name = showGhcQual name

-- | If a pattern consists of only one identifier then return this
-- identifier, otherwise return Nothing
patToNameRdr :: NameMap -> GHC.LPat GHC.RdrName -> Maybe GHC.Name
patToNameRdr nm (GHC.L l (GHC.VarPat n)) = Just (rdrName2NamePure nm (GHC.L l n))
patToNameRdr _ _ = Nothing

-- | If a pattern consists of only one identifier then return this
-- identifier, otherwise return Nothing
patToPNT :: GHC.LPat GHC.Name -> Maybe GHC.Name
patToPNT (GHC.L _ (GHC.VarPat n)) = Just n
patToPNT _ = Nothing

-- | Compose a pattern from a pName.
pNtoPat :: name -> GHC.Pat name
pNtoPat pname = GHC.VarPat pname

-- ---------------------------------------------------------------------
{-
-- TODO: This should use the TokenUtils API
getToksForDecl :: SYB.Data t =>
  t -> [PosToken] -> [PosToken]
getToksForDecl decl toks
      = let (startPos, endPos) = startEndLocIncComments toks decl
            (toks1, _) =let(ts1,(_t:ts2'))= break (\t -> tokenPos t >= endPos) toks
                        in (ts1, ts2')
        in dropWhile (\t -> tokenPos t < startPos {- was || isNewLn t -}) toks1
-}
-- ---------------------------------------------------------------------
{-
-- TODO: this is currently only used in a test
-- Get the toks for a declaration, and adjust its offset to 0.
getDeclAndToks :: (HsValBinds t GHC.Name)
     => GHC.Name -> Bool -> [PosToken] -> t
     -> ([GHC.LHsBind GHC.Name],[PosToken])
getDeclAndToks pn _incSig _toks t =
  let
    decls     = definingDeclsNames [pn] (hsBinds t) True True
    -- declToks  = getToksForDecl decls toks

  -- in (decls, removeToksOffset declToks)
  in (decls, [])
-}
-- ---------------------------------------------------------------------
{-
-- TODO: this is currently only used in a test
-- | Get the signature and tokens for a declaration
getSigAndToks :: (SYB.Data t) => GHC.Name -> t -> [PosToken]
     -> Maybe (GHC.LSig GHC.Name,[PosToken])
getSigAndToks pn t _toks
  = case (getSig pn t) of
      Nothing -> Nothing
      -- Just sig -> Just (sig, removeToksOffset $ getToksForDecl sig toks)
      Just sig -> Just (sig, [])
-}

-- ---------------------------------------------------------------------

-- | Remove at most `offset` whitespaces from each line in the tokens

-- TODO: move this function to LocUtils.hs
-- TODO: add a test for this
removeOffset :: Int -> [PosToken] -> [PosToken]
-- removeOffset offset toks = error $ "removeOffset:offset=" ++ (show offset) -- ++AZ++
removeOffset offset toks = map (\(t,s) -> (adjust t,s)) toks
  where
    adjust (GHC.L l t) = (GHC.L l' t)
      where
        l' =  case l of
          GHC.RealSrcSpan ss ->
            let
              locs = GHC.mkSrcLoc (GHC.srcSpanFile ss) (GHC.srcSpanStartLine ss) ((GHC.srcSpanStartCol ss) - offset)
              loce = GHC.mkSrcLoc (GHC.srcSpanFile ss) (GHC.srcSpanEndLine ss) ((GHC.srcSpanEndCol ss) - offset)
              -- loc = GHC.mkSrcLoc (GHC.srcSpanFile ss) (1 + GHC.srcSpanEndLine ss) 0
            in
              GHC.mkSrcSpan locs loce
          _ -> l

