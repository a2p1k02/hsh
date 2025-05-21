module Main where

import System.Process (createProcess, proc, waitForProcess)
import System.Exit (ExitCode(..))
import Control.Monad (unless)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering))
import System.Directory (setCurrentDirectory, getHomeDirectory)
import System.Environment (getEnv)
import Data.List (isPrefixOf)
import Control.Exception (catch)
import GHC.IO.Exception (IOError)

handleBuiltin :: String -> IO Bool
handleBuiltin input
    | "cd" `isPrefixOf` input = do
        let dir = drop 3 input
        newDir <- if null dir then getHomeDirectory else return dir
        setCurrentDirectory newDir
        return True
    | otherwise = return False

executeCommand :: String -> IO ()
executeCommand cmd = do
    let (name:args) = words cmd
    (_, _, _, phandle) <- createProcess (proc name args)
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure _ -> putStrLn $ "Command failed: " ++ cmd

shellLoop :: IO ()
shellLoop = do
    putStr "hsh> "
    hFlush stdout
    input <- getLine
    unless (input == "exit") $ do
        isBuiltin <- handleBuiltin input
        unless isBuiltin $ do
            catch (executeCommand input) 
                (\e -> putStrLn $ "Error: " ++ show (e :: IOError))
        shellLoop

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    shellLoop
