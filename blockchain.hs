-- {-# LANGUAGE OverloadedStrings #-}

-- import Data.Time.Clock (UTCTime, getCurrentTime)
-- import Crypto.Hash (Digest, SHA256, hash)
-- import qualified Data.ByteString.Char8 as C
-- import Data.List (unfoldr)
-- import Text.Printf (printf)

-- data Block = Block
-- {
--     index :: Int,
--     timestamp :: UTCTime,
--     previousHash :: String,
--     currentHash :: String,
--     nonce :: Int,
--     blockData :: String
-- } deriving (Show)

-- -- ^ pozitia block-ului, momentul in care a fost creat, hash-ul block-ului precedent, hash-ul block-ului curent, nonce value pentru minat, si datele tinute in block

-- type Blockchain = [Block]

-- calculateHash :: Int -> UTCTime -> String -> Int -> String -> String
-- calculateHash idx time prevHash nonce blkData =
--     let input = C.pack $ concat [show idx, show time, prevHash, show nonce, blkData]
--         hashValue = hash input :: Digest SHA256
--     in show hashValue

-- createGenesisBlock :: IO Block
-- createGenesisBlock = do
--     currentTime <- getCurrentTime
--     let genesisBlock = Block
--         {
--             index = 0,
--             timestamp = currentTime,
--             previousHash = "0",
--             nonce = 0,
--             blockData = "Genesis Block",
--             currentHash =  ""
--         }
--         genesisHash = calculateHashForBlock genesisBlock
--     return genesisBlock { currentHash = genesisHash }

-- calculateHashForBlock :: Block -> String
-- calculateHashForBlock blk = 
--     calculateHash (index blk) (timestamp blk) (previousHash blk) (nonce blk) (blockData blk)

-- main :: IO ()
-- main = do
--     genesisBlock <- createGenesisBlock
--     putStrLn "Genesis Block:"
--     print genesisBlock

{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Clock (UTCTime, getCurrentTime)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as C

data Block = Block
  { index        :: Int
  , timestamp    :: UTCTime
  , previousHash :: String
  , currentHash  :: String
  , nonce        :: Int
  , blockData    :: String
  } deriving (Show)

type Blockchain = [Block]

calculateHash :: Int -> UTCTime -> String -> Int -> String -> String
calculateHash idx time prevHash nonce blkData =
  let input = C.pack $ concat [show idx, show time, prevHash, show nonce, blkData]
      hashValue = hash input :: Digest SHA256
  in show hashValue

createGenesisBlock :: IO Block
createGenesisBlock = do
  currentTime <- getCurrentTime
  let genesisBlock = Block
        { index = 0
        , timestamp = currentTime
        , previousHash = "0"
        , nonce = 0
        , blockData = "Genesis Block"
        , currentHash = ""
        }
      genesisHash = calculateHashForBlock genesisBlock
  return genesisBlock { currentHash = genesisHash }

calculateHashForBlock :: Block -> String
calculateHashForBlock blk =
  calculateHash (index blk) (timestamp blk) (previousHash blk) (nonce blk) (blockData blk)

main :: IO ()
main = do
  genesisBlock <- createGenesisBlock
  putStrLn "Genesis Block:"
  print genesisBlock
