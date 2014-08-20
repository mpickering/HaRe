{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ > 706
{-# LANGUAGE PatternSynonyms #-}
#endif

{-

Avoid various landmines. We have

    *FunBind {

        fun_id :: Located idL,

        fun_infix :: Bool,      -- ^ True => infix declaration

        fun_matches :: MatchGroup idR (LHsExpr idR),  -- ^ The payload

        fun_co_fn :: HsWrapper, -- ^ Coercion from the type of the MatchGroup to the type of
                                -- the Id.  Example:
                                -- @
                                --      f :: Int -> forall a. a -> a
                                --      f x y = y
                                -- @
                                -- Then the MatchGroup will have type (Int -> a' -> a')
                                -- (with a free type variable a').  The coercion will take
                                -- a CoreExpr of this type and convert it to a CoreExpr of
                                -- type         Int -> forall a'. a' -> a'
                                -- Notice that the coercion captures the free a'.

        bind_fvs :: PostRn idL NameSet, -- ^ After the renamer, this contains
                                --  the locally-bound
                                -- free variables of this defn.
                                -- See Note [Bind free vars]


        fun_tick :: Maybe (Tickish Id)  -- ^ Tick to put on the rhs, if any

  | *PatBind {
        pat_lhs    :: LPat idL,
        pat_rhs    :: GRHSs idR (LHsExpr idR),
        pat_rhs_ty :: PostTc idR Type,      -- ^ Type of the GRHSs
        bind_fvs   :: PostRn idL NameSet, -- ^ See Note [Bind free vars]
        pat_ticks  :: (Maybe (Tickish Id), [Maybe (Tickish Id)])

data PatSynBind idL idR
  = PSB { psb_id   :: Located idL,             -- ^ Name of the pattern synonym
          psb_fvs  :: PostRn idR NameSet,      -- ^ See Note [Bind free vars]
          psb_args :: HsPatSynDetails (Located idR), -- ^ Formal parameter names
          psb_def  :: LPat idR,                      -- ^ Right-hand side
          psb_dir  :: HsPatSynDir idR                -- ^ Directionality

  | -- | @type@ declaration
    SynDecl { tcdLName  :: Located name            -- ^ Type constructor
            , tcdTyVars :: LHsTyVarBndrs name      -- ^ Type variables; for an associated type
                                                  --   these include outer binders
            , tcdRhs    :: LHsType name            -- ^ RHS of type declaration
            , tcdFVs    :: PostRn name NameSet }


  | -- | @data@ declaration
    DataDecl { tcdLName    :: Located name        -- ^ Type constructor
             , tcdTyVars   :: LHsTyVarBndrs name  -- ^ Type variables; for an assoicated type
                                                  --   these include outer binders
                                                  -- Eg  class T a where
                                                  --       type F a :: *
                                                  --       type F a = a -> a
                                                  -- Here the type decl for 'f' includes 'a' 
                                                  -- in its tcdTyVars
             , tcdDataDefn :: HsDataDefn name
             , tcdFVs      :: PostRn name NameSet }

  | ClassDecl { tcdCtxt    :: LHsContext name,          -- ^ Context...
                tcdLName   :: Located name,             -- ^ Name of the class
                tcdTyVars  :: LHsTyVarBndrs name,       -- ^ Class type variables
                tcdFDs     :: [Located (FunDep name)],  -- ^ Functional deps
                tcdSigs    :: [LSig name],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds name,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl name],       -- ^ Associated types; ie
                tcdATDefs  :: [LTyFamDefltEqn name],    -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl],               -- ^ Haddock docs
                tcdFVs     :: PostRn name NameSet

data TyFamInstDecl name
  = TyFamInstDecl
       { tfid_eqn  :: LTyFamInstEqn name
       , tfid_fvs  :: PostRn name NameSet }

data DataFamInstDecl name
  = DataFamInstDecl
       { dfid_tycon :: Located name
       , dfid_pats  :: HsTyPats name      -- LHS
       , dfid_defn  :: HsDataDefn  name   -- RHS
       , dfid_fvs   :: PostRn name NameSet } -- Rree vars for
                                               -- dependency analysis
data RuleDecl name
  = HsRule                      -- Source rule
        RuleName                -- Rule name
        Activation
        [RuleBndr name]         -- Forall'd vars; after typechecking this includes tyvars
        (Located (HsExpr name)) -- LHS
        (PostRn name NameSet)        -- Free-vars from the LHS
        (Located (HsExpr name)) -- RHS
        (PostRn name NameSet)        -- Free-vars from the RHS

  | HsLamCase (PostTc id Type) (MatchGroup id (LHsExpr id)) -- ^ Lambda-case

  | OpApp       (LHsExpr id)    -- left operand
                (LHsExpr id)    -- operator
                (PostRn id Fixity) -- Renamer adds fixity; bottom until then
                (LHsExpr id)    -- right operand

  -- | Multi-way if
  | HsMultiIf   (PostTc id Type) [LGRHS id (LHsExpr id)]

  | HsDo        (HsStmtContext Name) -- The parameterisation is unimportant
                                     -- because in this context we never use
                                     -- the PatGuard or ParStmt variant
                [ExprLStmt id]       -- "do":one or more stmts
                (PostTc id Type)     -- Type of the whole expression

  | ExplicitList
                (PostTc id Type)        -- Gives type of components of list
                (Maybe (SyntaxExpr id)) -- For OverloadedLists, the fromListN witness
                [LHsExpr id]

  -- | Syntactic parallel array: [:e1, ..., en:]
  | ExplicitPArr
                (PostTc id Type)   -- type of elements of the parallel array
                [LHsExpr id]

  | RecordUpd   (LHsExpr id)
                (HsRecordBinds id)
--              (HsMatchGroup Id)  -- Filled in by the type checker to be
--                                 -- a match that does the job
                [DataCon]          -- Filled in by the type checker to the
                                   -- _non-empty_ list of DataCons that have
                                   -- all the upd'd fields
                [PostTc id Type]   -- Argument types of *input* record type
                [PostTc id Type]   --              and  *output* record type

  | HsArrApp             -- Arrow tail, or arrow application (f -< arg)
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        (PostTc id Type) -- type of the arrow expressions f,
                         -- of the form a t t', where arg :: t
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

data HsTupArg id
  = Present (LHsExpr id)     -- ^ The argument
  | Missing (PostTc id Type) -- ^ The argument is missing, but this is its type

data HsCmd id
  = HsCmdArrApp          -- Arrow tail, or arrow application (f -< arg)
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        (PostTc id Type) -- type of the arrow expressions f,
                         -- of the form a t t', where arg :: t
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  | HsCmdDo     [CmdLStmt id]
                (PostTc id Type)                -- Type of the whole expression


data HsCmdTop id
  = HsCmdTop (LHsCmd id)
             (PostTc id Type)   -- Nested tuple of inputs on the command's stack
             (PostTc id Type)   -- return type of the command
             (CmdSyntaxTable id) -- See Note [CmdSyntaxTable]

data MatchGroup id body
  = MG { mg_alts    :: [LMatch id body]  -- The alternatives
       , mg_arg_tys :: [PostTc id Type]  -- Types of the arguments, t1..tn
       , mg_res_ty  :: PostTc id Type    -- Type of the result, tr
       , mg_origin  :: Origin }

  | BodyStmt body              -- See Note [BodyStmt]
             (SyntaxExpr idR)  -- The (>>) operator
             (SyntaxExpr idR)  -- The `guard` operator; used only in MonadComp
                               -- See notes [Monad Comprehensions]
             (PostTc idR Type) -- Element type of the RHS (used for arrows)

  | RecStmt
     { recS_stmts :: [LStmtLR idL idR body]

        -- The next two fields are only valid after renaming
     , recS_later_ids :: [idR] -- The ids are a subset of the variables bound by the
                               -- stmts that are used in stmts that follow the RecStmt

     , recS_rec_ids :: [idR]   -- Ditto, but these variables are the "recursive" ones,
                               -- that are used before they are bound in the stmts of
                               -- the RecStmt.
        -- An Id can be in both groups
        -- Both sets of Ids are (now) treated monomorphically
        -- See Note [How RecStmt works] for why they are separate

        -- Rebindable syntax
     , recS_bind_fn :: SyntaxExpr idR -- The bind function
     , recS_ret_fn  :: SyntaxExpr idR -- The return function
     , recS_mfix_fn :: SyntaxExpr idR -- The mfix function

        -- These fields are only valid after typechecking
     , recS_later_rets :: [PostTcExpr] -- (only used in the arrow version)
     , recS_rec_rets :: [PostTcExpr] -- These expressions correspond 1-to-1
                                     -- with recS_later_ids and recS_rec_ids,
                                     -- and are the expressions that should be
                                     -- returned by the recursion.
                                     -- They may not quite be the Ids themselves,
                                     -- because the Id may be *polymorphic*, but
                                     -- the returned thing has to be *monomorphic*,
                                     -- so they may be type applications

      , recS_ret_ty :: PostTc idR Type -- The type of
                                       -- do { stmts; return (a,b,c) }
                                   -- With rebindable syntax the type might not
                                   -- be quite as simple as (m (tya, tyb, tyc)).

data HsOverLit id       -- An overloaded literal
  = OverLit {
        ol_val :: OverLitVal,
        ol_rebindable :: Bool,          -- Note [ol_rebindable]
        ol_witness :: SyntaxExpr id,    -- Note [Overloaded literal witnesses]
        ol_type :: PostTc id Type }

data Pat id
    WildPat     (PostTc id Type)        -- Wild card

  | ListPat     [LPat id]                            -- Syntactic list
                (PostTc id Type)                     -- The type of the elements
                (Maybe (PostTc id Type, SyntaxExpr id)) -- For rebindable syntax
                   -- For OverloadedLists a Just (ty,fn) gives
                   -- overall type of the pattern, and the toList
                   -- function to convert the scrutinee to a list value

  | TuplePat    [LPat id]        -- Tuple sub-patterns
                Boxity           -- UnitPat is TuplePat []
                [PostTc id Type] -- [] before typechecker, filled in afterwards

  | PArrPat     [LPat id]               -- Syntactic parallel array
                (PostTc id Type)        -- The type of the elements

  | ViewPat       (LHsExpr id)
                  (LPat id)
                  (PostTc id Type)  -- The overall type of the pattern
                                    -- (= the argument type of the view function)
                                    -- for hsPatType.


--

  | HsSpliceTy          (HsSplice name)
                        (PostTc name Kind)

  | HsExplicitListTy       -- A promoted explicit list
        (PostTc name Kind) -- See Note [Promoted lists and tuples]
        [LHsType name]

  | HsExplicitTupleTy      -- A promoted explicit tuple
        [PostTc name Kind] -- See Note [Promoted lists and tuples]
        [LHsType name]

-}
module Landmines.Mine1 where

-- FunBind
a x = x + 3

-- PatBind
b e = let ![x,y] = e in x + y

#if __GLASGOW_HASKELL__ > 706
-- PatSynBind

#endif
