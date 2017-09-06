{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module TH.TypeFamily
  ( deriveNewtypedTypeFamily
  )
  where


import           ClassyPrelude
import           Language.Haskell.TH

{-|
Derive the type family instance of a given type family instance for a newtype instance,
assuming that the newtype wraps a value that implements said type class.

For example, suppose the type family instances
> type Element (Seq a) = a
from ClassyPrelude.

For a newtype like:
> newtype MySeq = MySeq (Set Int)

the following line would be derived:
> type Element MySeq = Int

This function performs some kind of type inference based on the definition
of the wrapped value.
The rule works as follow: if the right hand side of the expression occurs
in the definition of the type (e.g. `a` occurs in `Seq a`) then assume that
the type found in the according posistion should be the right hand side of the
type family instance.

By this rule, we can also infer more complex types, like
> newtype Mseq = MySeq (Set (String, Int, Double))
and similar.

If the right hand side does not occur in the type, then the function
assumes that the right hand side was chosen arbitrarily.
For example, take the following type instance:
> type Index (Seq a) = Int
The type `Int` does not occur in `Seq a`, therefore the derivation would result
in the right hand side being unchanged.
> type Index MySeq = Int
-}
deriveNewtypedTypeFamily :: Name -- ^ Name of the type family instance
                         -> Name -- ^ Name of the newtype which shall derive the type instance
                         -> DecsQ -- ^ Type family instance for use in other template haskell functions
deriveNewtypedTypeFamily typeFamily newtypeName = do
    decl <- getDeclaration
    instances <- getInstances decl
    sequenceA $ getTypeSynInstances decl instances
    where
        getInstances :: Type -> Q [InstanceDec]
        getInstances newtypeName' = do
            inst <- reifyInstances typeFamily [newtypeName']
            return inst

        getDeclaration :: Q Type
        getDeclaration = do
            TyConI mn <- reify newtypeName
            case mn of
                (NewtypeD _ _ _ _ (RecC _ [(_,_, typeD)]) _) -> do
                    return typeD

                (NewtypeD _ _ _ _ (NormalC _ [(_, typeD)]) _) -> do
                    return typeD

                _ ->
                    fail $ show newtypeName ++ "Not a newtype"

        getTypeSynInstances :: Type -> [InstanceDec] -> [DecQ]
        getTypeSynInstances dec inst =
            map
                ( \(TySynInstD _ (TySynEqn c right)) -> do
                    -- TODO: no pattern match
                    [newRight] <- sequenceA $ map (\f -> extractRightSideWithDefault f dec right) c
                    return $ TySynInstD typeFamily (TySynEqn [ConT newtypeName] newRight)
                )
                inst

extractRightSideWithDefault :: Type -> Type -> Type -> Q Type
extractRightSideWithDefault a b r =
    recover
        (return r) -- handler in case of error
        (extractRightSide a b r) -- normal computation

extractRightSide :: Type -> Type -> Type -> Q Type
extractRightSide (AppT a b) (AppT c d) r =
  if r == a then
      return c
  else if r == b then
      return d
  else
      recover
          (extractRightSide a c r) -- handler in case of error
          (extractRightSide b d r) -- normal computation

extractRightSide (ParensT a) (ParensT b) r =
    if r == a then
        return b
    else
        extractRightSide a b r

extractRightSide _ _ _ =
    fail "no matching pair found"
