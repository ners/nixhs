module Main where

import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

type RequiredFeatures = {-NonEmpty-}Set Text

{-
feature :: Text -> RequiredFeatures
feature = Set.singleton

--empty :: RequiredFeatures
--empty = Set.empty

union :: RequiredFeatures -> RequiredFeatures -> RequiredFeatures
union = Set.union

-- x = atom "foo" `and` atom "bar"
-}

-- Version 2
-- data File = File {content :: BS.ByteString, executable :: Maybe RequiredFeatures}
data File = MkFile {content :: BS.ByteString, executable :: Bool}
    deriving stock (Generic, Eq, Ord)

data FileSystemObject
    = Directory (Map Text FileSystemObject)
    | File File
    | Link FilePath
    deriving stock (Generic, Eq, Ord)

hashFileSystemObject :: FileSystemObject -> Text
hashFileSystemObject = const ""

{-
data Deriver
  = InputAddressed Derivation
  | ContentAddressed (Maybe Derivation)
-}

data StoreObject = StoreObject
    { storeObjectReferences :: Set StoreObject
    , storeObjectData :: FileSystemObject
    --, storeObjectDeriver :: Deriver
    }
    deriving stock (Generic, Eq, Ord)

{-
data StorePath = StorePath
    { storePathHash :: Text
    , storePathName :: Text
    }
    deriving stock (Generic, Eq, Ord)

instance Show StorePath where
    show StorePath{..} = Text.unpack $ storePathHash <> "-" <> storePathName

data Store = Store
    { storeObjects :: Map StorePath StoreObject
    --, storeDirectory :: FilePath
    --, storeTrustMapping :: Map Derivation StoreObject
    }
    deriving stock (Generic, Eq)

storeAdd :: FileSystemObject -> Maybe Text -> StoreM s (Ref s)
storeAdd fso maybeName store =
    store
        { storeObjects = Map.insert storePath storeObject store.storeObjects
        }
  where
    storePath =
        StorePath
            { storePathHash = hashFileSystemObject fso
            , storePathName = fromMaybe "source" maybeName
            }
    storeObject =
        StoreObject
            { storeObjectReferences = mempty
            , storeObjectData = fso
            }
-}

data Derivation = Derivation
    { derivationBuilder :: (File, Set StoreObject)
    , derivationArg :: StoreObject
    }

force :: Derivation -> StoreObject
force (Derivation object arg) = undefined --execve (system, executable, args, env)
  where
    possibleRefs = snd object <> storeObjectReferences arg

main :: IO ()
main = putStrLn "Hello, Haskell!"
