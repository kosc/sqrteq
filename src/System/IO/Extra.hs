module System.IO.Extra
    ( flush
    , prompt
    , parsePrompt
    ) where

import           Data.Function (fix)

import           Text.Read     (readEither)

import           System.IO     (hFlush, stdout)


flush :: IO ()
flush = hFlush stdout

prompt :: String -> IO String
prompt text = do
    putStr text
    flush
    getLine

parsePrompt :: Read a => String -> IO a
parsePrompt text = fix $ \repeat -> do
    readEither <$> prompt text >>= \case
        Right value -> pure value
        Left  error -> do
            putStrLn error
            repeat
