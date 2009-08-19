{-# LANGUAGE TemplateHaskell, CPP #-}

{- |
This module provides an automatic Template Haskell
routine to scour data type definitions and generate
lense objects for them automatically.

It was copied almost verbatim (2 line change) from the wonderful Data.Accessors.Template module 
made by Luke Palmer, and Henning Thielemann.

You will need to add:
{-# LANGUAGE TemplateHaskell, 
             FlexibleContexts #-}
to the top of any modules that use this one. 
-}
module Data.Lenses.Template (
   nameDeriveLenses, deriveLenses
   ) where

import Language.Haskell.TH.Syntax
  -- (Q, Exp(VarE), Pat(VarP), Dec(ValD), Name(Name), mkOccName, occString, reify, )

import Data.Maybe (catMaybes, )
import Control.Monad.State
import Data.Lenses

-- |@deriveLenses n@ where @n@ is the name of a data type
-- declared with @data@ looks through all the declared fields
-- of the data type, and for each field ending in an underscore
-- generates an accessor of the same name without the underscore.
--
-- It is "nameDeriveLenses" n f where @f@ satisfies
--
-- > f (s ++ "_") = Just s
-- > f x          = Nothing    -- otherwise
--
-- For example, given the data type:
--
-- > data Score = Score { p1Score_ :: Int
-- >                    , p2Score_ :: Int
-- >                    , rounds   :: Int
-- >                    }
--
-- @deriveLenses@ will generate the following objects:
--
-- > p1Score :: (MonadState Score m) => StateT Int m b -> m b
-- > p1Score = fromGetSet p1Score_ (\x s -> s { p1Score_ = x })
-- > p2Score :: (MonadState Score m) => StateT Int m b -> m b
-- > p2Score = fromGetSet p2Score_ (\x s -> s { p2Score_ = x })
--
-- It is used with Template Haskell syntax like:
--
-- > $( deriveLenses ''TypeName )
--
-- And will generate accessors when TypeName was declared
-- using @data@ or @newtype@.
deriveLenses :: Name -> Q [Dec]
deriveLenses n = nameDeriveLenses n stripUnderscore

stripUnderscore :: String -> Maybe String
stripUnderscore s = do
    (stem,'_') <- viewR s
    return stem
  where
    -- add to break dependency with Data.List.HT
    viewR :: [a] -> Maybe ([a], a)
    viewR [] = Nothing
    viewR xs = Just (init xs, last xs)


-- |@nameDeriveLenses n f@ where @n@ is the name of a data type
-- declared with @data@ and @f@ is a function from names of fields
-- in that data type to the name of the corresponding accessor. If
-- @f@ returns @Nothing@, then no accessor is generated for that
-- field.
nameDeriveLenses :: Name -> (String -> Maybe String) -> Q [Dec]
nameDeriveLenses t namer = do
    info <- reify t
    reified <- case info of
                    TyConI dec -> return dec
                    _ -> fail errmsg
    (params, cons) <- case reified of
                 DataD _ _ params cons' _ -> return (params, cons')
                 NewtypeD _ _ params con' _ -> return (params, [con'])
                 _ -> fail errmsg
    decs <- liftM concat $ mapM (makeAccs params) cons
    when (null decs) $ qReport False nodefmsg
    return decs

    where

    errmsg = "Cannot derive accessors for name " ++ show t ++ " because"
          ++ "\n    it is not a type declared with 'data' or 'newtype'"
          ++ "\n    Did you remember to double-tick the type as in"
          ++ "\n      $(deriveLenses ''TheType)?"

    nodefmsg = "Warning: No accessors generated from the name " ++ show t
          ++ "\n    If you are using deriveLenses rather than"
          ++ "\n    nameDeriveLenses, remember accessors are"
          ++ "\n    only generated for fields ending with an underscore"

    makeAccs :: [Name] -> Con -> Q [Dec]
    makeAccs params (RecC _ vars) =
        liftM (concat . catMaybes) $ mapM (\ (name,_,ftype) -> makeAccFromName name params ftype) vars
    makeAccs params (ForallC _ _ c) = makeAccs params c
    makeAccs _ _ = return []

    transformName :: Name -> Maybe Name
    transformName (Name occ f) = do
        n <- namer (occString occ)
        return $ Name (mkOccName n) f

    makeAccFromName :: Name -> [Name] -> Type -> Q (Maybe [Dec])
    makeAccFromName name params ftype =
        case transformName name of
            Nothing -> return Nothing
            Just n -> liftM Just $ makeAcc name params ftype n

    -- haddock doesn't grok TH
#ifndef __HADDOCK__

    makeAcc ::Name -> [Name] -> Type -> Name -> Q [Dec]
    makeAcc name params ftype accName = do
        let appliedT = foldl AppT (ConT t) (map VarT params)
        body <- [|
                 fromGetSet
                    ( $( return $ VarE name ) )
                    ( \x s ->
                        $( return $ RecUpdE (VarE 's) [(name, VarE 'x)] ) )
                |]

        return
          [ SigD accName (ForallT (b:m:params) [AppT (AppT (ConT ''MonadState) appliedT) (VarT m)] (AppT (AppT ArrowT (AppT (AppT (AppT (ConT ''StateT) ftype) (VarT m)) (VarT b))) (AppT (VarT m) (VarT b))))
          , ValD (VarP accName) (NormalB body) []
          ]
      where
        b = mkName "b"
        m = mkName "m"

#endif

