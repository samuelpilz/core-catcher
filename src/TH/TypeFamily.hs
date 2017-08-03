{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module TH.TypeFamily
  ( deriveNewtypedTypeFamily
  )
  where


import           ClassyPrelude
import           Language.Haskell.TH

deriveNewtypedTypeFamily :: Name -> Name -> DecsQ
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
