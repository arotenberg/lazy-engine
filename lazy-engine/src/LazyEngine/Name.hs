module LazyEngine.Name where

newtype TyVarID = TyVarID String
    deriving (Show, Eq, Ord)

newtype VarID = VarID String
    deriving (Show, Eq, Ord)
