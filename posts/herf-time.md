
Implementing your own herf instance
--------------------------------------------------

[Herf Time](https://github.com/smurphy8/herf-time) is a small, low dependency package for
time manipulation in haskell.

It is designed to be used to manipulate time stamps in an intuitive way across multiple
time stamp kinds.

I based it loosely on the [Kerf](https://github.com/kevinlawler/kerf) programming language.

The basic principle is:

All the operators one can use on a HerfTime are defined in 3 typeclasses.

You then make your given time-stamp an instance of these classes (Usually by coming up with a route to UTCTime).

Then you can use any of the interval arithmatic that you get for all these classes.


# The Good
The library is very easy to use and I think fills a gap that haskell has.

The haskell time library is really nice but is missing the "great defaults" feel
which kerf has. 

## A few examples

### Add Intervals
``` haskell
>>> date 2016 01 01 `add` hour 3 `add` week 16 `add` month 3 :: UTCTime
UTCHerfTime 2016-07-22 03:00:00 UTC
```

### Subtract Intervals
``` haskell

>>> date 2016 01 01 `add` hour (-3) `add` week (-16) `add` month (-3) :: UTCTime
UTCHerfTime 2015-06-10 21:00:00 UTC
```

### Represent Time in Multiple Ways
``` haskell

>>> dateTime 2016 01 01 01 23 01 `add` hour 3 `add` week 16 `add` month 3 :: UTCTime
UTCHerfTime 2016-07-22 04:23:01 UTC
>>> dateTimePico 2016 01 01 01 23 01 01 `add` hour 3 `add` week 16 `add` month 3 :: UTCTime
UTCHerfTime 2016-07-22 04:23:01.000000000001 UTC
```
### Get Times in any HerfedTime format  (UTC for example)
``` haskell
>>> date 2016 01 01 `add` hour 3 `add` week 16 `add` month 3   :: UTCTime
2016-07-22 03:00:00 UTC
```

### Use HerfTime.ZonedTime to convert easily between times
(reherf $ ( dateTime 2016 01 01 01 01 01 :: HerfZonedTime "CST")) :: HerfZonedTime "PST"
2015-12-31T23:01:01:PST

# The Bad 

* This library is really using typeclasses in a very OO way, I can dress it up and call it "final encoding".
* Nothing is new here, all of this can be done with time by itself!
* A few more Pragmas are required *DataKinds* and *KindSignatures* to use the library in ZonedTime 

# The Ugly

* Making a new instance of ```HerfedTime``` is a bit daunting due to the large number of functions in the typeclass

# How to deal with the Ugly 
###  (Making an instance of the HerfedTime typeclass)



There are several properties it would be nice to have in a time series type class:

### Paths Between Types (and back)
*  ``` a + timeInterval  <->  b + timeInterval ```
Where going back and forth between these two types converges to an invariant
so some of the information is preserved!

### an ability to use 1 operator to discuss multiple time intervals
* So i don't have to have: *addHour* , *addMin*, *addDay*

### an ability to add more intervals at a later date
* A solution this excludes is something based on a sum type
* One way of doing this is [Tagged Final Style](http://okmij.org/ftp/tagless-final/)

## Here is what I came up with

The primary class involved is ```HerfedTime```

``` haskell
class (ToUTCHerfTime a, FromUTCHerfTime a) => HerfedTime a where
  addYear :: a -> HerfYear -> a
  addMonth :: a -> HerfMonth -> a
  addWeek :: a -> HerfWeek -> a
  addDay :: a -> HerfDay -> a
  addHour :: a -> HerfHour -> a
  addMinute :: a -> HerfMin -> a
  addSecond :: a -> HerfSec -> a
  addPicosecond :: a -> HerfPico -> a
  date :: HerfYear -> HerfMonth -> HerfDay -> a
  dateTime :: HerfYear -> HerfMonth -> HerfDay -> HerfHour -> HerfMin -> HerfSec ->  a
  dateTimePico :: HerfYear -> HerfMonth -> HerfDay -> HerfHour -> HerfMin -> HerfSec -> HerfPico ->  a
```

There are 3 support classes:

``` haskell
class  ToUTCHerfTime a where
  herf :: a -> UTCHerfTime


class FromUTCHerfTime a where
  unherf ::  UTCHerfTime -> a

class HerfAdd a where
  add :: (HerfedTime t) => t -> a -> t
```

And a few rules (law is just too big a claim for me!).

1. round trip
If ```ToUTCHerftime``` and ```FromUTCHerftime``` are both defined,
then ```a == (unherf . herf $ a)```  should hold.


2. path independence
``` unherf (herf a  `add` timeInterval ) == (a `add` timeInterval) ```

3. conversion equivalences
Arithmetic relationships between ```HerfAdd``` members must be preserved.
eg.
If 60s = 1m
Then
``` herf (v `add` (HerfSecond 60))  ==  herf (v `add` (HerfMin 1) ```



The round trip property and the path independence property imply our converge requirement...

Because you can remove what ever difference in resolution exist between two types by converging one to the other.

Then you can move ```a ``` by timeIntervals to a point where the ```herf a``` converts to the ```b```


Everything else is just sugar.

``` haskell
year :: Integer -> HerfYear
year = HerfYear

month :: Integer -> HerfMonth
month = HerfMonth

week :: Integer -> HerfWeek
week = HerfWeek

day :: Integer -> HerfDay
day = HerfDay

hour :: Integer -> HerfHour
hour = HerfHour

minute :: Integer -> HerfMin
minute = HerfMin

second :: Integer -> HerfSec
second = HerfSec

pico :: Integer -> HerfPico
pico = HerfPico


herfShow :: FormatTime t => t -> String
herfShow = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S:%Z") )  -- i.e. YYYY-MM-DDTHH:MM:SS
```


