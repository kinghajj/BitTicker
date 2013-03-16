module BitTicker.Fetch where

import Control.Concurrent       ( runInUnboundThread)
import Data.Aeson               ( FromJSON(..), decode)
import Data.ByteString.Internal ( w2c)
import Data.ByteString.Lazy     ( ByteString, hGetContents, unpack)
import System.Process           ( CreateProcess(..), StdStream(..), CmdSpec(..)
                                , createProcess)
import Text.Printf              ( printf)

-- use 'wget' to fetch an HTTP object
fetchHTTP :: String -> IO ByteString
fetchHTTP url = createProcess wget >>= \(_, mstdout, _, _) ->
                case mstdout of
                  Just h  -> hGetContents h
                  Nothing -> error "wget had no stdout handle???"
  where cmd  = "wget --no-check-certificate -q -O - \"%s\""
        wget = CreateProcess (ShellCommand $ printf cmd url)
                             Nothing Nothing CreatePipe CreatePipe CreatePipe
                             False False

-- fetch an HTTP object and attempt to read it
fetchRead :: Read a => String -> IO a
fetchRead = fmap (read . map w2c . unpack) . fetchHTTP

-- fetch an HTTP object and attempt to par
fetchDecode :: FromJSON a => String -> IO (Maybe a)
fetchDecode = runInUnboundThread . fmap decode . fetchHTTP
