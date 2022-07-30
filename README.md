# ghc-unload-obj
Interactive demo of unloadObj failing to free memory with GHC 9.0.2 on Windows.

After building, copy the pre-compiled .o file and strings.txt from the root
folder to the same directory as the executable.

Run the executable and type in the command `r1000` to see roughly 800MB allocated and never freed.

If using a recompiled .o file, copy it to the same directory as the executable
and make sure that the first line of strings.txt is the .o file's file name
including the extension, and that the second line is its symbol name that
corresponds to the `f` function in Lib.hs.

Any .o file should actually work for demonstrating the unloading problem, but it
won't be resolved unless it only depends on base, and it needs a symbol pointing
to a function of type `(Int -> Int)` in order to be used.

Commands:  
  `l` - Load and resolve object code, then lookup the `f :: Int -> Int` function  
  `u` - Unload the object code  
  `r`(*n*) - Load, unload, and `performMajorGC` in a loop *n* times. e.g. `r1000`  
  `p` - Print f(2), where the default `f` is `(\x -> x + 1)` and the `f` in the .o file is `(\x -> x * x)`  
  `g` - Call `performMajorGC`  
  `q` - Quit  