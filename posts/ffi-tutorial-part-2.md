Here is a simple C struct with three common types int, double, and char pointer. We want to send this to Haskell and have Haskell mutate it.
 
###foo.h
```c
typedef struct {
  int     a;
  double  b;
  char   *c;
} foo;
```

###foo.c
```c
#include "foo.h"
```

Now we need the Haskell code. 

###HsFoo.hsc
```haskell
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module HsFoo where

import Foreign
import Foreign.C
import Control.Applicative
import Control.Monad

#include "foo.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Foo = Foo { 
    a :: Int
  , b :: Double
  , c :: CString
} deriving Show

instance Storable Foo where
    sizeOf    _ = #{size foo}
    alignment _ = #{alignment foo}
    
    poke p foo = do
        #{poke foo, a} p $ a foo
        #{poke foo, b} p $ b foo
        #{poke foo, c} p $ c foo

    peek p = return Foo
              `ap` (#{peek foo, a} p)
              `ap` (#{peek foo, b} p)
              `ap` (#{peek foo, c} p)

-- call Haskell free from C
foreign export ccall "freePointerSetInHaskell" free :: Ptr a -> IO ()

foreign export ccall "setFoo" setFoo :: Ptr Foo -> IO ()
setFoo :: Ptr Foo -> IO ()
setFoo f = do
  -- newCString creates a pointer to a CString, IO (Ptr CString)
  -- you are responsible for freeing it before the Haskell program ends
  newC <- newCString "Hello from Haskell!"
  poke f $ Foo 3 3.14159 newC
  return ()
```

The first thing you should notice is that the file ends with `.hsc`, this tells cabal to run `hsc2cs` on it to convert it to a `.hs` file. hsc2cs is a program included with GHC (you can run in from the command line). It helps you create Haskell bindings to C code by looking for `#include`, `#let` and directives contained in `#{}`. 

For any data constructor that will interface to a C struct, you need to typeclass it with `Storable`. For `Storable` you should at least define `sizeOf`, `alignment`, `poke` and `peek`. Keep in mind that the name of the struct in C and the corresponding data constructor in Haskell can have different names.

`#{size foo}` gets the size of the foo struct in C.   
`#{alignment foo}` gets the bit offset based on the C struct, along with the help of the `#let alignment` declaration.  
`#{poke foo ...}` and `#{peek foo ...}` automatically calculate the byte offset, otherwise you would have to do it by hand.

`setFoo` is the function we will be using to mutate the C struct. It receives a pointer to Foo. We create a new CString and then poke the pointer with a new Foo instance.

Here is the main C program that will use both the struct from foo.h and the functions 
from HsFoo.

###main.c
```c
#include <stdio.h>
#include <stdlib.h>
#include "HsFoo_stub.h"
#include "foo.h"

int main(int argc, char *argv[]) {  
  hs_init(&argc, &argv);
  
  foo *f;
  f = malloc(sizeof(foo));
  f->a = 1;
  f->b = 1.5;
  f->c = "Hello from C!"; 
  
  printf("foo has been set in C:\n  a: %d\n  b: %f\n  c: %s\n\n",f->a,f->b,f->c);
  
  setFoo(f);
  
  printf("foo has been set in Haskell:\n  a: %d\n  b: %f\n  c: %s\n\n",f->a,f->b,f->c);
  
  freePointerSetInHaskell(f->c);
  free(f);
  hs_exit();
}
```

###command line (compile and run)
```bash
$ hsc2hs HsFoo.hsc
$ ghc -c HsFoo.hs foo.c
$ ghc -no-hs-main foo.c HsFoo.o main.c -o main
$ ./main
```

`freePointerSetInHaskell` frees the newCArray pointer we created in setFoo.

Note that the character pointer in `f->c` was created in Haskell. Once `hs_exit()` is called, `f->c` will be a dangling pointer if you did not free it in Haskell, or if you did free it will be a free pointer. If you want the value referenced by the char pointer then you can use `strcpy`.

```c
foo *fcopy;
fcopy = malloc(sizeof(foo));
fcopy->a = f->a;
fcopy->b = f->b;
fcopy->c = malloc(strlen(f->c) + 1);
strcpy(fcopy->c, f->c);

freePointerSetInHaskell(f->c);
free(f);
hs_exit();

printf("fcopy works after clean up:\n  a: %d\n  b: %f\n  c: %s\n\n",fcopy->a,fcopy->b,fcopy->c);
free(fcopy->c);
free(fcopy);
```

Another important point is that FFI does not support arbitrary pass by value for Haskell storable types. You cannot define a type in Haskell and pass it directly to C. 

You can only pass the following Haskell types from Haskell to C: 
Int, Word, Char, Float, Double, Addr, ForeignPtr, StablePtr, MutableByteArray, and ByteArray.

