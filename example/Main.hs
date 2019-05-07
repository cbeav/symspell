{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

import ClassyPrelude
import Data.Aeson
import Data.Default
import Data.Proxy
import GHC.Compact (getCompact)
import Network.Wai.Handler.Warp (run)
import Prelude (read)
import Servant
import SymSpell
import SymSpell.IO (fromFile)
import System.Environment (lookupEnv)

newtype SymSpellReq
  = SymSpellReq
  { symSpellReqWord :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON SymSpellReq where
  parseJSON (Object o) = SymSpellReq <$> o .: "word"

type SymSpellApi = "top" :> ReqBody '[JSON] SymSpellReq :> Post '[JSON] [SymSpellSuggestion]

symSpellApi :: Proxy SymSpellApi
symSpellApi = Proxy

server :: SymSpell -> Server SymSpellApi
server symSpell = return . suggest symSpell 1 Top . symSpellReqWord

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  symSpell <- fromFile def "resources/frequencies.txt"
  putStrLn $ unwords
    [ "Loaded"
    , tshow . length . getCompact $ symSpellFrequencies symSpell
    , "word frequencies"
    ]
  putStrLn $ "Listening on port " ++ tshow port
  run port . serve symSpellApi $ server symSpell
