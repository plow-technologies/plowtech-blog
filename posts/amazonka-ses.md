
Amazonka is a huge intimidating package full of amazing goodness.
------------------------------------------------------------------

[Here is the full overview](http://brendanhay.nz/amazonka-comprehensive-haskell-aws-client/)


What makes amazonka scary is what makes it great...
* The packages are split up along service lines
* The same auth routines control everything!

I think illustrating the power of amazonka can be done with one quick email example.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
module SESDemo where


import           Network.AWS
import           Network.AWS.SES
import           SESDemo.Internal


import           Control.Lens
import           System.IO


testCreateSendMail = sendEmail "<source-email>" dest msg
 where
  dest = destination & dToAddresses .~ ["<target-email>"]
  msg = message content' body'
  content' = content "" & cData .~ "Email as a service"
  body' = body & bHTML .~ (Just (content "<h1>People love their cars </h1>"))

testSendEmail :: IO ()
testSendEmail = do
    env <- newEnv Oregon Discover
    l <- newLogger Debug stdout
    _ <- runResourceT . runAWS (env & envLogger .~ l) $ (send testCreateSendMail)
    return ()

```

``` testSendEmail ``` calls our brand new S3 service!

So there are lots of lenses and things going on in Amazonka that are scary on first sight.

I am going to go through how I pieced it together and hopefully that will be helpful.

First the function with the name I wanted!

``` haskell 
sendEmail
    :: Text -- ^ 'seSource'
    -> Destination -- ^ 'seDestination'
    -> Message -- ^ 'seMessage'
    -> SendEmail
sendEmail pSource_ pDestination_ pMessage_ =
    SendEmail'
    { _seReturnPath = Nothing
    , _seSourceARN = Nothing
    , _seReturnPathARN = Nothing
    , _seReplyToAddresses = Nothing
    , _seSource = pSource_
    , _seDestination = pDestination_
    , _seMessage = pMessage_
    }
```

What is a [SendEmail](http://hackage.haskell.org/package/amazonka-ses-1.3.7/docs/Network-AWS-SES-SendEmail.html#t:SendEmail)?

From the haskell docs...

<hr>

*Represents a request instructing the service to send a single email message.*

*This datatype can be used in application code to compose a message consisting of source, destination, message, reply-to, and return-path parts. This object can then be sent using the SendEmail action.*

*See:* *[sendEmail](http://hackage.haskell.org/package/amazonka-ses-1.3.7/docs/Network-AWS-SES-SendEmail.html#v:sendEmail)* *smart constructor.*

<hr>

So the ```sendEmail``` function is a smart constructor for SendEmail.

There is another for ```Message```, ```Content``` and ```Body```.

There are also lenses to change what I want the default fields (the ones not created by the smart constructor) to be.


from above:

``` haskell
body' = body & bHTML .~ (Just (content "<h1>People love their cars </h1>"))
```

So I selected Html instead of plain text, to allow us to send an email as Html.

## What next?

So we have a ```SendEmail``` but what do we do?

Doing a little exploring in the ```amazonka``` [package I found what to do](http://hackage.haskell.org/package/amazonka-1.3.7/docs/Network-AWS.html).

``` haskell

import Control.Lens
import Network.AWS
import Network.AWS.S3
import System.IO

example :: IO PutObjectResponse
example = do
    e <- newEnv Frankfurt Discover :: IO Env
    l <- newLogger Debug stdout :: IO Logger
    b <- sourceFileIO "local/path/to/object-payload"
    runResourceT . runAWS (e & envLogger .~ l) $
    send (putObject "bucket-name" "object-key" b)
    
```

The function doing the heavy lifting is [```send```](http://hackage.haskell.org/package/amazonka-1.3.7/docs/Network-AWS.html#send).

``` haskell
-- | Send a request, returning the associated response if successful.
send :: (MonadAWS m, AWSRequest a) => a -> m (Rs a)
send = liftAWS . AWST.send
```

Looking back at ```SendEmail```, it is an instance of ```AWSRequest```!

Looks like we have a way to send things now... The last detail is the authorization.

Authorization is handled in the ``` newEnv``` part of that example above.

Follow instructions in amazon's docs to set it up.

...

That is about it, hope this is helpful!





