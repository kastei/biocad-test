{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Types where


data Molecule = Molecule 
    { molecule_id          :: Int
    , molecule_smiles      :: String
    , molecule_iupacName   :: String
    } 
    deriving (Show, Eq)

data Reaction = Reaction 
    { reaction_id          :: Int
    , reaction_name        :: String
    } 
    deriving (Show, Eq)

  {-

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text    (Text)

import Database.Bolt (Record, Value (..), RecordValue (..), Node (..), at)

data Molecule = Molecule { id        :: Int
                         , smiles    :: String
                         , iupacName :: String
                         }
  deriving (Show, Eq)

data Reaction = Reaction { id   :: Int
                         , name :: String
                         }
  deriving (Show, Eq)

data Catalyst = Catalyst { id     :: Int
                         , smiles :: String
                         , name   :: Maybe String
                         }
  deriving (Show, Eq)

data PRODUCT_FROM = PRODUCT_FROM { amount :: Float
                                 }
  deriving (Show, Eq, Ord)

data ACCELERATE = ACCELERATE { temperature :: Float
                             , pressure    :: Float
                             }
  deriving (Show, Eq)

{-
data Value = N ()
  | B Bool
  | I Int
  | F Double
  | T Text
  | L [Value]
  | M (Map Text Value)
  | S Structure
  deriving (Show, Eq)
-}

instance ToJSON Value where
  toJSON (N _) = toJSON ()
  toJSON (B b) = toJSON b
  toJSON (I i) = toJSON i
  toJSON (F d) = toJSON d
  toJSON (T t) = toJSON t
  toJSON (L l) = toJSON l
  toJSON _     = undefined  -- we do not need Maps and Structures in this example

instance ToJSON Molecule where
  toJSON (Molecule id smiles iupacName) = object [ "id" .= id, "smiles" .= smiles, "iupacName" .= iupacName ]

instance ToJSON Reaction where
  toJSON (Reaction id name) = object [ "id" .= id, "name" .= name ]

instance ToJSON Catalyst where
  toJSON (Catalyst id smiles name) = object [ "id" .= id, "smiles" .= smiles, "name" .= name ]

instance ToJSON PRODUCT_FROM where
  toJSON (PRODUCT_FROM amount) = object ["amount" .= amount]

instance ToJSON ACCELERATE where
  toJSON (ACCELERATE temperature pressure) = object ["temperature" .= temperature, "pressure" .= pressure]


-- |Converts some BOLT value to 'Molecule'
toMolecule :: Monad m => Value -> m Molecule
toMolecule (L [I id, T smiles, T iupacName]) = return $ Molecule id smiles iupacName
toMolecule _                          = fail "Not a Molecule value"

-- |Converts some BOLT value to 'Reaction'
toReaction :: Monad m => Value -> m Reaction
toReaction (L [I id, T name]) = return $ Reaction id name
toReaction _                          = fail "Not a Reaction value"

-- |Converts some BOLT value to 'Catalyst'
toCatalyst :: Monad m => Value -> m Catalyst
toCatalyst (L [I id, T smiles, T name]) = return $ Catalyst id smiles name
toCatalyst _                          = fail "Not a Catalyst value"
-}