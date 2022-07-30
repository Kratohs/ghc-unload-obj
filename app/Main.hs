{-# LANGUAGE MagicHash, UnboxedTuples, ForeignFunctionInterface #-}

module Main (main) where

import GHC.Exts
import Foreign
import Foreign.C
import System.Mem
import System.Posix.Internals
import Text.Read
import GHC

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
initLoader =
    c_initLinker 0

loadResolveLookup :: IO LoadedFunc
loadResolveLookup = do
    (path, symbolName) <- readStrings
    _loadOk <- withFilePath path c_loadObj
    resolved <- (/= 0) <$> c_resolveObjs
    if not resolved then do
        putStrLn "Loaded the obj, but couldn't resolve it"
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
    _unloadResult <- withFilePath path c_unloadObj
    putStrLn "Unloaded obj"

reload :: Int -> IO ()
reload n 
    | n < 1 = pure ()
    | otherwise = do
        path <- readObjPath
        go path n
        putStrLn $
            "Reloaded " ++ show n ++ " times without resolving or looking up\
            \ any symbols, and performed a major GC after each unload"
  where
    go _ 0 = pure ()
    go path n = do
        _loadResult <- withFilePath path c_loadObj

        -- Resolving will allocate more memory
        _resolveResult <- c_resolveObjs

        _unloadResult <- withFilePath path c_unloadObj

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
foreign import ccall "loadObj"      c_loadObj      :: CFilePath -> IO Int
foreign import ccall "resolveObjs"  c_resolveObjs  :: IO Int
foreign import ccall "lookupSymbol" c_lookupSymbol :: CString -> IO (Ptr a)
foreign import ccall "unloadObj"    c_unloadObj    :: CFilePath -> IO Int