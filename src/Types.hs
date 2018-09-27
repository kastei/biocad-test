{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Types where

import Data.Text (Text, unpack)

import Database.Bolt (Record, Value (..), RecordValue (..), Node (..), at)

data Reaction = Reaction { r_id       :: Int
                         , r_name     :: String
                         , reagents   :: [(Molecule, Double)]
                         , catalysts  :: [(Maybe Catalyst, Accelerate)]
                         , products   :: [(Molecule, Double)]
                         }
    deriving (Eq)

instance Show Reaction where
    show (Reaction id name reagents catalysts products) 
      = "Reaction {id=" ++ (show id) ++ 
      ", name='" ++ name ++ "'" ++
      ", reagents=" ++ (show reagents) ++ 
      ", catalysts= " ++ (show catalysts) ++ 
      ", products= " ++ (show products) ++ "}"
 
    
data Molecule = Molecule { m_id        :: Int
                         , m_smiles    :: String
                         , m_iupacName :: String
                         }
    deriving (Eq)

instance Show Molecule where
    show (Molecule id smiles name) = "Molecule {id=" ++ (show id) ++ ", name='" ++ name ++ "', SMILES='" ++ smiles ++ "'}"


data Accelerate = Accelerate { temperature  :: Maybe Double 
                             , pressure     :: Maybe Double
                             }
    deriving (Eq)

instance Show Accelerate where
    show (Accelerate t' p') = case (t', p') of
        (Just t,   Just p ) -> "Accelerate {temperature=" ++ (show t) ++ ", pressure=" ++ (show p) ++ "}"
        (Nothing,  Just p ) -> "Accelerate {pressure=" ++ (show p) ++ "}"
        (Just t,   Nothing) -> "Accelerate {temperature=" ++ (show t) ++ "}"
        (Nothing,  Nothing) -> ""


data Catalyst = Catalyst { c_id     :: Int
                         , c_smiles :: String
                         , c_name   :: Maybe String
                         }
    deriving (Eq)

instance Show Catalyst where
    show (Catalyst id smiles (Just name)) = "Catalyst {id=" ++ (show id) ++ ", name='" ++ name ++ "', SMILES='" ++ smiles ++ "'}"
    show (Catalyst id smiles Nothing    ) = "Catalyst {id=" ++ (show id) ++ ", SMILES='" ++ smiles ++ "'}"
 
    

toMoleculeAndAmount :: Value -> (Molecule, Double)
toMoleculeAndAmount (L [I id, T smiles, T name, F amount]) = (Molecule id (unpack smiles) (unpack name), amount)

toCatalystAndAccelerate :: Value -> (Maybe Catalyst, Accelerate)
toCatalystAndAccelerate (L [I id, smiles, name, temperature', pressure']) = (catalyst, accelerate) where
    accelerate = case (temperature', pressure') of
        (F temperature, F pressure) -> Accelerate (Just temperature) (Just pressure)
        (N (),          F pressure) -> Accelerate Nothing (Just pressure)
        (F temperature, N ()      ) -> Accelerate (Just temperature) Nothing
        (_, _      ) -> Accelerate Nothing Nothing
    catalyst = case (smiles, name) of 
        (T "",     _     ) -> Nothing
        (T smiles, T ""  ) -> Just $ Catalyst id (unpack smiles) Nothing
        (T smiles, T name) -> Just $ Catalyst id (unpack smiles) (Just $ unpack name)
        (_,     _     ) -> Nothing


toMolecule :: Monad m => Value -> m Molecule
toMolecule v = do node :: Node <- exact v
                  let props = nodeProps node
                  let identity = nodeIdentity node
                  iupacName :: Text <- (props `at` "iupacName") >>= exact
                  smiles :: Text <- (props `at` "smiles") >>= exact
                  return $ Molecule identity (unpack smiles) (unpack iupacName)

toReaction :: Monad m => Value -> m Reaction
toReaction v = do node :: Node <- exact v
                  let props = nodeProps node
                  let identity = nodeIdentity node
                  name :: Text <- (props `at` "name") >>= exact
                  return $ Reaction identity (unpack name) [] [] [] 
