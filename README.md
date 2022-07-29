# ghc-load-obj
Minimal interactive demo of unloadObj failing to free memory on Windows.

After building, copy the pre-compiled .o file and strings.txt from the root
folder to the same directory as the executable.

If using a recompiled .o file, copy it to the same directory as the executable
and make sure that the first line of strings.txt is the .o file's file name
including the extension, and that the second line is its symbol name that
corresponds to the "f" function in Lib.hs.

Any .o file should actually work for demonstrating the unloading problem, but it
won't be resolved unless it only depends on base, and it needs a symbol pointing
to a function of type (Int -> Int) in order to be used.

Commands:
  l    - Load, resolve, and lookup the function from the .o file
  u    - Unload the object code
  r(n) - Load (without resolve or lookup), unload, performMajorGC in a loop n times. e.g. r100
  p    - Print f(2), where the default f is (\x -> x + 1) and the f in the .o file is (\x -> x * x)
  g    - Call performMajorGC
  q    - Quit