Ambiguous data muddles the semantics of traditional parsers.

You can use the [contravariant](https://hackage.haskell.org/package/contravariant-1.4) library to build a parser whose use mirrors traditional parsers closely.  
* [Here is the example on the blog...](#example)
* [Here is the code in a repo you can play with ...](https://github.com/plow-technologies/dependent-parser)

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

It might be nice to have all the pieces embedded.

Specifically, can we have a form with the following properties:
## <a name="checklist"></a> Desired Properties
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


There are [proofs](#proof).  

## Yes But.. Is it useful?

The most obvious first test is, does it meet the requirements set out for it.
Lets go through them one by one...

## One: are inputs and outputs tracked by the structure?

Looking at the definition it is clear this fits nicely. 
The ```final``` type tells us what kind of dependent parser will result.
The ```initial``` type tells us what sort of input will produce it.

## Two: interoperability with existing parser combinators.
The bottom level of our ContraParser is a regular attoparsec Parser type.  
This implies we should be able to get at least some interoperability.

We will define functions to embed Parsers into ContraParsers and vice versa soon.

## Three: ways to transform inputs and outputs.  
The composition of parsers is what makes them so darn powerful.  It is the primary 
reason to develop a contraparser in the first place.

The ```contramap``` allows composable ContraParsers, changing the parser input that is being depended on.

Still needed, is something that transforms what the output will be.



--------------------------------------------------

# Functions to define Parser interoperability

What might interoperability look like?  Well, it can be helpful to start with a function we
know the look of and work backward. 

In our example the ``` AddressResponse``` contains a part that is directly parseable and a part that isn't.

``` haskell
responseContraParser :: ContraParser Response Request

parseAddressResponse :: Request -> Parser AddressResponse
parseAddressResponse input = do
  c <- alphaNum
  _ <- char ','
  rslt <- (getParser responseContraParser input)
  return $ AddressResponse c rslt
  

```

Notice, this can be transformed into a ContraParser.

``` haskell
addressResponseContraParser :: ContraParser AddressResponse Request
addressResponseContraParser = ContraParser parseAddressResponse

```
There! A nice shiny new ContraParser, which shows the new final target and what is
needed to get there.

Can we generalize these actions?  Yes!

We can pull everything that is specific for ```parseAddressResponse``` away from the body of the function.

``` haskell
parseAddressResponse input = do
  combinationFunction <- regularParser
  contraResult  <- getParser contraParser input
  return $ combinationFunction contraResult
 where
  regularParser :: Parser (Response -> AddressResponse)
  regularParser = AddressResponse <$> (alphaNum <* char ',')
  
  contraParser  :: ContraParser Response Request
  contraparser  = responseContraparser
  

```

Let's define a new function ``` embed ``` which moves all the specific
parts to arguments which come in from the outside.  It will also wrap 
the function in our ContraParser Contravariant Functor.

``` haskell

embed regularParser contraParser = ContraParser (\input -> do
                                                     combinationFunction <- regularParser
                                                     contraResult        <- contraParser
                                                     return $ combinationFunction contraResult)

```
Ask ghci what the type of this thing is.

``` bash
λ> :i embed
embed :: Parser (t -> b) -> ContraParser t a -> ContraParser b a
```
Inferring from position we can write in our type signature for embed.

``` haskell
-- | Transforms the target of a ContraParser
embed :: Parser (final -> final') -> ContraParser final initial -> ContraParser final initial
embed regularParser contraParser = ContraParser (\input -> do
                                                     combinationFunction <- regularParser
                                                     contraResult        <- contraParser
                                                     return $ combinationFunction contraResult)
```
I love this technique for working out nice abstractions by isolating the specific and 
factoring them to the top level.  

What we see from our signature is ```embed``` provides a transformation on the final target of
the parser.  Considering our [checklist](#checklist), this gives us item 2 by allowing traditional parsers to 
be embedded in our ContraParser.  It gives us item 3 by allowing transformations on the final target.

# A few functions more...

Now to finish up our very small api by defining a few more functions that seem to be useful for
the ContraParser.  I derived them by the same technique I used with embed.

1. Write a function for a specific situation.
2. Pull the specific parts up to the definition of the function (closure converting).  
3. Look for any standard haskell pieces that may fall out.
4. rewrite the type signatures and function name in a more general and descriptive fashon.

``` haskell

-- | convert a ContraParser into a parser
contraParse :: a -> ContraParser b a -> Parser b
contraParse a parseBuild = getParser parseBuild a

-- | convert a parser into a ContraParser
constContraParse :: Parser b -> ContraParser b a
constContraParse p = ContraParser (const p)



-- | run the contra parser with no continuation
-- return the result
contraParseOnly  ::                       initial
                   -> ContraParser  final initial -> Text
                   -> Either String final
                  
contraParseOnly a p = parseOnly (contraParse a p)

```
<a name="example"></a>
# A more complete example 
Why not put everything together and see what use looks like.

``` haskell

-- | This defines a new base parser that takes a list of requests instead of 
-- a single one.
parseListOfAddressResponses :: [Request] -> Parser [AddressResponse]
parseListOfAddressResponses requests = sequence $ List.foldr decodeOneResponse [] requests
   where
     seperator = (char '|' *> pure ()) <|> eof
     decodeOneResponse req lst = let p =  (getParser contraParseAddressResponse req)
                                     in  (p <* seperator):lst


-- | define as a ContraParser
multiAddressContraParser :: ContraParser [AddressResponse] [Request]
multiAddressContraParser = ContraParser parseListOfAddressResponses


-- | Our input part is also found in a different structure.

data AddressRequestFullMessage = AddressRequestFullMessage {
   requestName :: Text,
   requestArray :: [Request]
              }

-- | Use 'contramap' to target the new input
fullMessageResponse  :: ContraParser [AddressResponse] AddressRequestFullMessage
fullMessageResponse = contramap requestArray multiAddressContraParser           


-- | Getting ready for some parsing!
exampleAddressRequestFullMessage :: AddressRequestFullMessage
exampleAddressRequestFullMessage = AddressRequestFullMessage
                                         "all pins"
                                         [RequestInt
                                         ,RequestInteger
                                         ,RequestInteger]


                              
-- | Ready, set parse!
example :: Either String [AddressResponse]
example = contraParseOnly exampleAddressRequestFullMessage fullMessageResponse "a,3|a,2|c,4"
```
Now, to run it!

``` bash
λ> exampleFour
Right [AddressResponse {deviceCode = 'a', deviceNumber = FinalInt 3},AddressResponse {deviceCode = 'a', deviceNumber = FinalInteger 2},AddressResponse {deviceCode = 'c', deviceNumber = FinalInteger 4}]
```

# So should you do this?

The only implementation I have made of this is a reference one.  I think it is best to define the ContraParsers you want in context of a problem you may have.

## Use a ContraParser if ... 
* You have ambiguous representatations in your serializations.
* You have parsers and input data that are also quite complex.
* You have a situation where the inputs and outputs are easy to lose track of.

## Avoid if ...
* Your data is ambiguous but still quite simple.
* The compositional elements of a ContraParser are not very useful for the problem.
* You can embed the resolution of the problem in some other, more convenient way (one function per output might be an example).

# Thinking about Contravariant Functors more broadly.
Something that is tricky about the abstractions concerning contravariant functors,  
they can always be written covariantly by considering the opposite categories.

This ContraParser example illustrates why that doesn't make sense all the time from a
usage point of view.  Thinking of your response parser as a method to take Strings through
Responses to operate on Requests works out mathematically to be the same thing but feels very backwards.

Contravariance is quite common, hopefully this example will help make it easier to see this pattern.

If you have any issues with this post, please leave me a note in the [issues](https://github.com/plow-technologies/dependent-parser/issues) section of the reference code.

Thanks! 


--------------------------------------------------
# Proofs
<a name="proof"></a> 
## Proof Law 1


``` 
contramap id someParser                                                                                                     
```

```
contramap id (ContraParser f::(initial -> Parser parserfinal) )  = ContraParser (f <$> id)                                 -- replace someParser by definition
```
```
contramap id (ContraParser f::(initial -> Parser parserfinal) )  = ContraParser ((initial -> Parser final ) . (\x -> x)  ) -- replace: 'id' & 'f' & '<$>' by definition 
```
```
contramap id (ContraParser f::(initial -> Parser parserfinal) )  = ContraParser (initial ->  Parser final ) = id           -- simply to finish
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
