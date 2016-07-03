Ambiguous data muddles the semantics of traditional parsers.

You can use the [contravariant](https://hackage.haskell.org/package/contravariant-1.4) library to build a parser whose use mirrors traditional parsers closesly.

# Why this matters

Suppose you have some protocol with a set of request messages and a request function defined:

``` haskell

data AddressRequest {
  reqDeviceCode   :: Char,
  reqDeviceNumber :: [Request]
  }
   deriving (Show,Eq,Ord)

data Request = RequestInteger | RequestInt
    deriving (Show,Eq,Ord)
    
-- Your function, gets back a Text
requestData :: AddressRequest -> IO Text
requestData requests = ...    

```
The ```Text``` that comes back is serialized by a protocol you have no control over.

example msg: ```"a,2|b,91|c,47" ```

The data type this should be parsed into looks like this:

``` haskell

data AddressResponse {
  resDeviceCode   :: Char,
  resDeviceNumber :: Response
}

data Response = ResponseInt Int | ResponseInteger Integer
    deriving (Show,Eq,Ord)



```

Notice that there is no way of telling from the string given, whether you have ```ResponseInt``` or ```ResponseInteger``` .
You must rely on the ```Request``` to determine it. 

This means that with traditional parsers everything ends up looking like:

``` haskell

dependentParseResponse :: AddressRequest -> Parser [AddressResponse]
dependentParseResponse = ...

```
![Not that there is anything wrong with that](notanythingwrongwiththat.gif)

I think though, it would be nice to have all the pieces embedded.

Specifically, it would be nice to have a form with the following properties:

1. Explicit tracking of needed inputs and expected outputs.
2. Interoperability with existing set of parser-combinators.
3. Ways to transform inputs and outputs.

# The Contravariant Functor
[contravariance](https://en.wikipedia.org/wiki/Covariance_and_contravariance) is a term designed to confuse people
regardless of discipline.  I think of it is tracing a path from output to input.

The contravariant functor typeclass in haskell is defined by the operation ```contramap :: (a -> b) -> f b -> f a  ```
It has a property that is interesting for our purpose.  It can embed output dependent on an input.  


# A Contravariant Parser

The Contravariant Functor for parsing can be defined:

``` haskell
data ContraParser final initial where
   ContraParser :: { getParser :: initial ->  Parser final  } ->  ContraParser final initial

instance Contravariant (ContraParser final) where
  contramap g (ContraParser p) = ContraParser (p <$> g)
```
There are laws:
## Law 1.

``` contramap id == id ```
## Law 2.

``` contramap f . contramap g = contramap (g . f)  ```


There are [proofs](#Proof-Law-1).  However I put the proofs at the bottom because I want to talk more about applications and use.

## Is it useful?
I think the most obvious first test is, does it meet the requirements set out for it.
Lets go through them one by one...

## One, are inputs and outputs tracked by the structure?

Looking at the definition it is clear this fits nicely. 
The ```final``` type tells us what kind of dependent parser will result.
The ```initial``` type tells us what sort of input will produce it.

## Two, interoperability with existing parser combinators.
The bottom level of our ContraParser is a regular attoparsec Parser type.  
This implies we should be able to get at least some interoperability.

We will define functions to embed Parsers into ContraParsers and vice versa soon.

## Three, ways to transform inputs and outputs.  
The composition of parsers is what makes them so darn powerful.  It is the primary 
reason to develop a contraparser in the first place.

The ```contramap``` allows composable ContraParsers, changing the parser input that is being depended on.

Still needed, is something that transforms what the output will be.

# Functions to define Parser interoperability

What might interoperability look like?  Well, it can be helpful to start with a function we
know the look of and work backward. 

In our example the ``` AddressResponse``` contains a part that is parseable and a part that isn't.

``` haskell
responseContraParser :: ContraParser Response 

parseAddressResponse input = do
  c <- alphaNum
  _ <- char ','
  rslts <- (parseResponse)

```


# Functions to define final datatype transformations


## Proof Law 1


``` 
contramap id someParser => 
```

```
contramap id (ContraParser f::(initial -> Parser parserfinal) )  = ContraParser (f <$> id)
```
```
contramap id (ContraParser f::(initial -> Parser parserfinal) )  = ContraParser ((initial -> Parser final ) . (\x -> x)  )
```
```
contramap id (ContraParser f::(initial -> Parser parserfinal) )  = ContraParser (initial ->  Parser final ) = id 
```

## Proof Law 2 :
This one is a bit trickier.

First, lets introduce some terms.
``` haskell
f :: (a -> b)

g :: (b -> c)

compGandF :: a -> c
compGandF = g . f

```

Here is our starting point.
``` haskell 
contramap g . contramap f 
```

Unwrapping the definitions is a good first step. 

Point free style has implicit arguments, let's make them explcit. 
``` haskell
(\cf@(ContraParser pX)  -> contramap f cf)         .(\cg@(ContraParser pX') -> contramap g cg)                         -- make lambda form explicit 

```

We also want to replace contramap by its implementation in both cases.
``` haskell
(\cf@(ContraParser pX)  -> ContraParser (pX <$> f)).(\cg@(ContraParser pX') -> ContraParser (pX' <$> g))               -- expand both contramaps 
```

Now lets transform the composition (.) by its implementation. 
``` haskell
(\cg@(ContraParser pX') -> (\cf@(ContraParser pX) -> ContraParser (pX <$> f) ) ContraParser (pX' <$> g) )              -- transform composition  
```

Applying the outer argument to the inner function and simplifying gives us.
``` haskell
(\cg@(ContraParser pX') -> (ContraParser ( (pX' <$> g) <$> f) ))                                                       -- apply cg to the inner lambda (cf == cg now) 
```

Remember ``` (f.g).h =  f.(g.h) ``` , and ```(<$>) = (.) ``` for the functor ``` (-> r) ``` which is what is present.
``` haskell
(\cg@(ContraParser pX') -> (ContraParser (pX' <$> (g . f)  )))                                                         -- notice that all the '<$>' are function composition 
```
We are almost there, noticing the inner part of our remaining lambda is equivalent to ```contramap``` 
allows us to wrap things back up.
``` haskell
(\cg@(ContraParser pX') -> contramap (g.f) cg)                                                                         -- convert with definition of contramap 
```

Back to point free style wraps things up!
``` haskell
contramap (g.f)                                                                                                        -- point free and done! 
```


# Other Helpful Sources
+ [An FP Complete tutorial](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors#contravariant-functors)
+ [Wiki page on contra and co variance](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)#Formal_definition)
