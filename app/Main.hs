{-# LANGUAGE MagicHash, UnboxedTuples, ForeignFunctionInterface #-}

module Main (main) where

import Control.Monad.IO.Class
import Foreign
import Foreign.C
import GHC
import GHC.Exts
import GHC.Paths
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
            loop defaultF
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
    c_initLinker 0

    -- This section can be removed to remove the dependencies on 'ghc' or
    -- 'ghc-lib', and 'ghc-paths', but resolving and lookups won't work then,
    -- only load and unload. The leaks happen either way.
    runGhc (Just libdir) $ do
        dynFlags <- getSessionDynFlags
        setSessionDynFlags $
            dynFlags
                { hscTarget = HscInterpreted
                , ghcLink = LinkInMemory
                }
        setTargets []
        loadResult <- load LoadAllTargets
        liftIO $ case loadResult of
            Succeeded -> putStrLn "See README.md for commands"
            Failed    -> putStrLn "Couldn't load base libraries"

loadResolveLookup :: IO LoadedFunc
loadResolveLookup = do
    (path, symbolName) <- readStrings
    loaded <- (/= 0) <$> withCWString path c_loadObj
    if not loaded then do
        putStrLn "Couldn't load the obj"
        pure defaultF
    else do
        resolved <- (/= 0) <$> c_resolveObjs
        if not resolved then do
            putStrLn "Couldn't resolve obj"
            pure defaultF
        else do
            ptr@(Ptr addr) <- withCString symbolName c_lookupSymbol
            if ptr == nullPtr then do
                putStrLn "Symbol not found"
                pure defaultF
            else do
                case addrToAny# addr of
                    (# val #) -> do
                        putStrLn "Loaded obj"
                        pure val

unload :: IO ()
unload = do
    path <- readObjPath
    unloaded <- (/= 0) <$> withCWString path c_unloadObj
    if unloaded
        then putStrLn "Unloaded obj"
        else putStrLn "Couldn't unload the obj"

reload :: Int -> IO ()
reload n 
    | n < 1 = pure ()
    | otherwise = do
        path <- readObjPath
        go path n
        putStrLn $
            "Reloaded " ++ show n ++ " times without looking up any symbols,\
            \ and performed a major GC after each unload"
  where
    go _ 0 = pure ()
    go path n = do
        _loadResult <- withCWString path c_loadObj

        -- Resolving is unnecessary, but will allocate more (unfreed) memory
        _resolveResult <- c_resolveObjs

        _unloadResult <- withCWString path c_unloadObj

        performMajorGC
        go path (n - 1)

defaultF :: LoadedFunc
defaultF = (+ 1)

readObjPath :: IO String
readObjPath = fst <$> readStrings

readStrings :: IO (String, String)
readStrings = do
    [o, sym] <- lines <$> readFile "strings.txt"
    pure (o, sym)

foreign import ccall "initLinker_"  c_initLinker   :: CInt -> IO ()
foreign import ccall "loadObj"      c_loadObj      :: CWString -> IO Int
foreign import ccall "resolveObjs"  c_resolveObjs  :: IO Int
foreign import ccall "lookupSymbol" c_lookupSymbol :: CString -> IO (Ptr a)
foreign import ccall "unloadObj"    c_unloadObj    :: CWString -> IO Int
