{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main where

import Control.Monad.IO.Class
import GHC
import GHC.Data.FastString
import GHC.Exts
import GHC.Paths
import GHC.Runtime.Interpreter
import System.Mem
import Text.Read

main :: IO ()
main = do
    initLoader
    loop defaultF

type LoadedFunc = Int -> Int

loop :: LoadedFunc -> IO ()
loop f = do
    s <- getLine
    case s of
        "l" -> do
            f' <- loadResolveLookup
            loop f'
        "u" -> do
            unload
            loop defaultF
        ('r' : numText) | (Just num) <- readMaybe numText -> do
            reload num
            loop f
        "p" -> do
            print (f 2)
            loop f
        "g" -> do
            performMajorGC
            putStrLn "Performed a major GC"
            loop f
        "q" ->
            pure ()
        _ -> do
            putStrLn $ "Unrecognized: " ++ s
            loop f

initLoader :: IO ()
initLoader = do
    runGhc' $ \env -> do
        liftIO $ initObjLinker env
        setTargets []
        _loadResult <- load LoadAllTargets
        liftIO $ putStrLn "Initialized loader"

loadResolveLookup :: IO LoadedFunc
loadResolveLookup = do
    runGhc' $ \env -> liftIO $ do
        (path, symbolName) <- readStrings
        loadObj env path
        resolveObjs env
        ptrMay <- lookupSymbol env (fsLit symbolName)
        case ptrMay of
            Nothing -> do
                putStrLn "Symbol not found"
                pure defaultF
            Just (Ptr addr) -> do
                case addrToAny# addr of
                    (# val #) -> do
                        putStrLn "Loaded obj"
                        pure val

unload :: IO ()
unload = do
    runGhc' $ \env -> liftIO $ do
        path <- readObjPath
        unloadObj env path
        putStrLn "Unloaded obj"

reload :: Int -> IO ()
reload n 
    | n < 1 = pure ()
    | otherwise = do
        path <- readObjPath
        runGhc' $ \env -> liftIO $ go path env n
        putStrLn $
            "Reloaded " ++ show n ++ " times without resolving or looking up\
            \ any symbols, and performed a major GC after each unload"
  where
    go _ _ 0 = pure ()
    go path env n = do
        loadObj env path

        -- Resolving will allocate more memory
        resolveObjs env

        iservCmd env RtsRevertCAFs
        
        unloadObj env path

        performMajorGC
        go path env (n - 1)

runGhc' :: (HscEnv -> Ghc a) -> IO a
runGhc' action =
    runGhc (Just libdir) $ do
        dynFlags <- getSessionDynFlags
        setSessionDynFlags $
            dynFlags
                { hscTarget = HscInterpreted
                , ghcLink = LinkInMemory
                }
        env <- getSession
        action env

defaultF :: LoadedFunc
defaultF = (+ 1)

readObjPath :: IO String
readObjPath = fst <$> readStrings

readStrings :: IO (String, String)
readStrings = do
    [o, sym] <- lines <$> readFile "strings.txt"
    pure (o, sym)
