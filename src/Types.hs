{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Types where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text, unpack)

import Database.Bolt (Record, Value (..), RecordValue (..), Node (..), at)


data Molecule = Molecule { m_id        :: Int
                         , m_smiles    :: String
                         , m_iupacName :: String
                         }
    deriving (Show, Eq)
   
data Reaction = Reaction { r_id       :: Int
                         , r_name     :: String
                         , reagents   :: [(Molecule, Double)]
                         , catalysts  :: [Catalyst]
                         , accelerate :: Maybe Accelerate
                         , products   :: [(Molecule, Double)]
                         }
    deriving (Eq)

data Accelerate = Accelerate { temperature  :: Double 
                             , pressure     :: Double
                             }
    deriving (Eq)

data Catalyst = Catalyst { c_id     :: Int
                         , c_smiles :: String
                         , c_name   :: Maybe String
                         }
    deriving (Eq)

instance Show Reaction where
    show (Reaction id name _ _ _ _) = "Reaction { id = " ++ (show id) ++", name = '" ++ name ++"' }"

toNode :: Monad m => Record -> m Node
toNode record = record `at` "n" >>= exact
    
--instance ToJSON Reaction where
--    toJSON (Reaction i n) = object [ "id" .= i, "name" .= n]
    
--toReaction :: Monad m => Value -> m Reaction
--toReaction r = do node :: Node <- exact r
--                  let props = nodeProps node
--                  let identity = nodeIdentity node
--                  name :: Text <- (props `at` "name") >>= exact
--                  return $ Reaction identity (unpack name)
    
instance ToJSON Value where
    toJSON (N _) = toJSON ()
    toJSON (B b) = toJSON b
    toJSON (I i) = toJSON i
    toJSON (F d) = toJSON d
    toJSON (T t) = toJSON t
    toJSON (L l) = toJSON l
    toJSON _     = undefined  -- we do not need Maps and Structures in this example