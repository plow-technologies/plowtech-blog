### Use Either and EitherT to remove fail from recursive functions.

---

Partial functions are mostly terrible but often convenient.
For this reason I end up pulling them out of a lot of code that is in our codebase.

All things possible in a language eventually end up in a code base!

For haskell the worst partial culprits are:
1. `read` (the worst haskell function)
2. `head`
3. `fail`
4. `error`


Here are a couple of techniques for pulling `fail` out of a function.


## A tasty partial example:

``` haskell

getAScoup :: IO (Either Rock IceCream)
getAScoup = iceCreamRetrievalFunctionNotPicturedHere

iAmPartialToIceCream :: Int -> [IceCream] -> IO [IceCream]
iAmPartialToIceCream scoupCount iceCreamScoups = do
     eitherIceCreamOrRock <- getAScoup
     either gotARock gotIceCream eitherIceCreamOrRock
 where
   gotARock _ = fail "Expected Ice Cream... got a rock!"
   gotIceCream scoup = if length (scoup:iceCreamScoups) >= scoupCount
                        then
                          return (scoup:iceCreamScoups)
                        else
                          iAmPartialToIceCream scoupCount (scoup:iceCreamScoups)

```


Obviously, `iAmPartialToIceCream` is a partial function.  That is, it can terminate the program without completing.
The advantage of this is I get to have my ice cream straight away if it is available.
The disadvantage is things break! They don't return the error, they just stop!

Maybe I can use the wonderful `Either` monad to give myself more info by rewriting things

## First try on the rewrite

``` haskell


iLikeIceCreamButAmNotPartial :: Int -> [IceCream] -> IO (Either Rock [IceCream])
iLikeIceCreamButAmNotPartial scoupCount iceCreamScoups = do
          eitherIceCreamOrRock <- getAScoup
          either gotARock gotIceCream eitherIceCreamOrRock
      where
        gotARock rock = return . Left $ rock
        gotIceCream scoup = if length (scoup:iceCreamScoups) >= scoupCount
                       then
                         return . Right $ (scoup:iceCreamScoups)
                       else
                         iLikeIceCreamButAmNotPartial scoupCount (scoup:iceCreamScoups)
``` 


## Hooray that worked!

I have termination if there is a rock, it doesn't throw a fail and doesn't ignore the rock.

Either is a great thing! 

But to make this work I had to change all the outputs of the function.

With EitherT I can remove the partiality with a lot less interfering in the parts of the function that were not partial to begin with.

``` haskell

getAScoup :: IO (Either Rock IceCream)
getAScoup = iceCreamRetrievalFunctionNotPicturedHere

-- 1. Replace: iAmPartialToIceCream :: Int -> [IceCream] -> IO [IceCream]

iAmPartialToIceCream :: Int -> [IceCream] -> IO (Either Rock [IceCream])
iAmPartialToIceCream soupCount' iceCreamScoups' = runEitherT $ iAmPartialToIceCream' soupCount' iceCreamScoups'
 where
-- 3. Move recursion to a loop add runEitherT to the top
  iAmPartialToIceCream' scoupCount iceCreamScoups = do
-- 4. add liftIO to any calls    eitherIceCreamOrRock <- getAScoup
     eitherIceCreamOrRock <- liftIO undefined -- getAScoup
     either gotARock gotIceCream eitherIceCreamOrRock
   where
  -- 2. Replace fail with left
     gotARock rock = left rock
     gotIceCream scoup = if length (scoup:iceCreamScoups) >= scoupCount
                          then
                            return (scoup:iceCreamScoups)
                          else
                            iAmPartialToIceCream' scoupCount (scoup:iceCreamScoups)

                                            -- ^ notice we are recursing over the loop function 
```


Sometimes I find it convenient to rewrite with Either explicitly and sometimes it is convenient to use EitherT.

The key with EitherT is you spend less time thinking about rewriting functions because it is sort of automatic.

## Get fail out with EitherT
1. Replace `IO` a with `IO (Either <what you want for err> a)`
2. Replace `fail` with `left`.
3. Move recursion into a loop and add a `runEitherT` to the top.
4. Add `liftIO` to any `IO` Calls

Small changes that address specific problems are very nice when doing refactors.  So EitherT is a great tool!

## It is good to know about both ways.

## Last notes

1. This function could made into a fold and probably look a lot prettier
2. [OCharles's post on EitherT is good.](https://ocharles.org.uk/blog/posts/2012-07-24-in-praise-of-EitherT.html)
