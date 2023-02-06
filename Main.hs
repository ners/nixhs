module Main where

import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

data FileSystemObject
    = Directory (Map Text FileSystemObject)
    | File {content :: BS.ByteString, executable :: Bool}
    | Link FilePath
    deriving stock (Generic, Eq, Ord)

hashFileSystemObject :: FileSystemObject -> Text
hashFileSystemObject = const ""

data StoreObject = StoreObject
    { storeObjectPath :: StorePath
    , storeObjectReferences :: Set StoreObject
    , storeObjectData :: FileSystemObject
    }
    deriving stock (Generic, Eq, Ord)

data StorePath = StorePath
    { storePathHash :: Text
    , storePathName :: Text
    }
    deriving stock (Generic, Eq, Ord)

instance Show StorePath where
    show StorePath{..} = Text.unpack $ storePathHash <> "-" <> storePathName

data Store = Store
    { storeObjects :: Map StorePath StoreObject
    , storeDirectory :: FilePath
    }
    deriving stock (Generic, Eq)

storeAdd :: FileSystemObject -> Maybe Text -> Store -> Store
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
            { storeObjectPath = storePath
            , storeObjectReferences = mempty
            , storeObjectData = fso
            }

data Derivation = Derivation
    { derivationSystem :: Text
    , derivationBuilder :: StoreObject
    , derivationArgs :: [Text]
    , derivationEnv :: Map Text Text
    , derivationInputs :: Set StoreObject
    , derivationOutputs :: Map Text StorePath
    }

execve :: Text -> StoreObject -> [Text] -> Map Text Text -> Either Text (Map Text StoreObject)
execve system object args env = undefined (system, executable, args, env)
  where
    executable :: BS.ByteString
    executable = case object.storeObjectData of
        File{executable = True, content} -> content
        _ -> error "File is not executable"

buildDerivation :: Derivation -> Either Text (Map Text StoreObject)
buildDerivation Derivation{..} =
    execve
        derivationSystem
        derivationBuilder
        derivationArgs
        derivationEnv

main :: IO ()
main = putStrLn "Hello, Haskell!"
