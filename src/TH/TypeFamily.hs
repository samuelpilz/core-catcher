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
    isInstance' <- isInstance typeFamily [decl]
    newInstances <- sequenceA $ getTypeSynInstances decl instances
    return newInstances
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
                    [newRight] <- sequenceA $ map (\f -> extractCorrectRightSide f dec right) c
                    return $ TySynInstD typeFamily (TySynEqn [ConT newtypeName] newRight)
                )
                inst

extractCorrectRightSide :: Type -> Type -> Type -> Q Type
extractCorrectRightSide (AppT a b) (AppT c d) right =
  if right == a then
      return c
  else if right == b then
      return d
  else
      recover
          (extractCorrectRightSide a c right)
          (extractCorrectRightSide b d right)

extractCorrectRightSide (ParensT a) (ParensT b) right =
    if right == a then
      return b
    else
      extractCorrectRightSide a b right

extractCorrectRightSide ListT ListT ListT =
    return ListT

extractCorrectRightSide _ _ _ =
    fail "Could not find a fitting type"
