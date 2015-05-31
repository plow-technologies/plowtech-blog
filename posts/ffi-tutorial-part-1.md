The Haskell Foreign Function Interface (FFI) is a tool that allows Haskell to call C functions and C to call Haskell functions. Let's start with a simple example of passing an Int from Haskell to C.


### HsFoo.hs (returnInt)
```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsFoo where
import Foreign
import Foreign.C

foreign export ccall "returnIntFromHaskell" returnInt :: Int

returnInt :: Int
returnInt = 5678
```

The quoted string, `"returnIntFromHaskell"`, after `foreign export ccall` is the name of the function that will appear in C. The unquoted string, `returnInt `, after that is the name of the function in Haskell and the type signature.

Here is the C file that calls `returnIntFromHaskell`.

### main.c (returnIntFromHaskell)

```c
#include <stdio.h>
#include <stdlib.h>
#include "HsFoo_stub.h"

int main(int argc, char *argv[]) {  
  hs_init(&argc, &argv);
  
  int x = returnIntFromHaskell();
  printf("C: x value from returnIntFromHaskell: %d\n", x);
  
  hs_exit();
}  
```

`hs_init` starts HsFoo.hs with the command line arguments provided.  
`hs_exit` closes the HsFoo.hs program. You can no longer call any FFI functions in C after this function, unless `hs_init` is run again.  
`HsFoo_stub.h` is a C header file that GHC creates.  

Compiling and running the program is straight forward. 

### command line

```bash
$ ghc -c HsFoo.hs
$ ghc -no-hs-main HsFoo.o main.c -o main
$ ./main
```
`ghc -c` informs the compiler to create a `_stub.h` file created. Remember that you need to declare a `module X where` in the Haskell file so that the compiler does not look for a main function.

Now let's set the value of a C Pointer in Haskell.

### HsFoo.hs (setCInt)
```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module HsFoo where

import Foreign
import Foreign.C

foreign export ccall "setIntInHaskell" setCInt :: Ptr CInt -> IO ()

setCInt :: Ptr CInt -> IO ()
setCInt i = do
  ival <- peek i
  putStrLn $ "Haskell: i value received from C: " ++ show ival
  poke i 1234
  return ()
```

`Ptr` is a data constructor that points to an object which can be marshalled to or from Haskell.  
`CInt` is used to receive an `int` from C.  
`poke` is a function that sets the value referenced by a `Ptr`.   
`peek` is a function that retrieves the referenced value of a `Ptr` as `IO a`.  

### main.c (setIntInHaskell)

```c
#include <stdio.h>
#include <stdlib.h>
#include "HsFoo_stub.h"

int main(int argc, char *argv[]) {  
  hs_init(&argc, &argv);
  
  int *i;
  i = malloc(sizeof(int));
  *i = 500;
  printf("C:       i value is: %d\n", *i);
  setIntInHaskell(i);
  printf("C:       i value after calling setIntInHaskell is: %d\n", *i);
  free(i);
  
  hs_exit();
} 
```

Remember to malloc and free any C pointers that are passed to Haskell. You can run compile and run with the same commands as previously mentioned.