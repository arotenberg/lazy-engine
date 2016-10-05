module LazyEngine.Name where

newtype TypeName = TypeName String
    deriving (Show, Eq, Ord)

newtype CtorName = CtorName String
    deriving (Show, Eq, Ord)

newtype FieldName = FieldName String
    deriving (Show, Eq, Ord)

-- | The 'Eq' instance is only present for the unit tests.
data CtorDecl = CtorDecl CtorName [FieldName]
    deriving (Show, Eq)

newtype GlobalName = GlobalName String
    deriving (Show, Eq, Ord)

newtype LocalID = LocalID Int
    deriving (Show, Eq, Ord)

data BinaryOp = PlusOp | MinusOp | TimesOp | QuotOp | RemOp
    deriving (Show, Eq, Ord)
