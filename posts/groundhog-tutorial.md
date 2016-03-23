[Groundhog](http://hackage.haskell.org/package/groundhog) is a library for mapping Haskell types to SQL schemas and operating on SQL-stored data from Haskell.
It attaches to many common SQL databases, including MySQL, Sqlite, and PostgreSQL.
Approaching it can be somewhat intimidating, but it is actually an excellent solution for persistence in Haskell.

We're going to walk through building a persistence layer for a Haskell application, using Groundhog with the Sqlite backend.
This tutorial should translate readily to other SQL databases, but we've only tested it against Sqlite.
Sqlite is ideal for local applications or microservices where you don't need to share data via the database with other processes.
It's a widely used persistence backend in its own right, used by Android, Firefox, OS X, and Chrome, among many other projects.

This tutorial focuses on the converters functionality, and is not intended to replace the main [Groundhog tutorial](https://www.schoolofhaskell.com/user/lykahb/groundhog).

# Example Application
Our business at Plow Technologies is automation and monitoring.
But sometimes it's our hobby too.
Your friendly author has a crawlspace under his house with a dirt floor.
Groundwater often wells up through the floor, flooding the crawlspace.
To combat this, the author has dug a pit in the floor and lowered a submersible pump (often called a "sump pump") into the pit.
The sump pump quickly empties the pit of water, thus removing groundwater before it can reach the level of the floor.
However, the pump can be damaged by leaving it to run unsubmerged, as it will overheat.

Instead of keeping one eye always on the pit to see when the pump should be plugged in, your friendly author is rigging an automation system.
The simple but boring way would be to rig a float switch (which closes when water reaches a certain level) to a relay which would switch the pump on and off.
The fun way is to connect a couple of float switches (one for the high water mark at which we start pumping, and one for the low water mark at which the pump is switched off) to a Raspberry Pi.
The automation system on the Pi will, of course, be written in Haskell.
It will provide not just automation for the pump, but remote manual control, monitoring of when the pump engaged and disengaged, and measurement of the power used by the pump and the flow rate through the hose.

We need a way to store these measurements and the logs of the pump being engaged or disengaged.
The automation system polls the state of both float switches every second, and sets the state of the pump accordingly.
Independently, it also polls the flow meter and current meter.

We also need to store some configuration information.
We might attach several float switches to get an idea of the water level beyond high-water and low-water.
We'd like to log the state of all these switches, but select which one engages the pump and which disengages it.
And of course, we need to store authentication information.

# Haskell Types

We'll just show the Types module

``` haskell
module Sump.Types where
```

We'll keep a table of switch positions.

``` haskell

-- | Mapping from switch ID (likely the IO pin it's connected to) to the switch's position
-- * True: Switch closed
-- * False: Switch open
type Switches = IntMap Bool
```

Some entries in the database will have timestamps. We'd like a uniform way of handling them:

``` haskell
-- | Provides a lens to get and set the time of something
class HasTimeStamp a where
  atTime :: (Functor f) => (UTCTime -> f UTCTime) -> a -> f a
```

The pump will be turning on and off throughout the day:

``` haskell
-- | Pump on/off state
data PumpToggle = PumpOn | PumpOff

-- | Pump state change
data PumpEvent
  = PumpEvent
  { pumpTimestamp :: UTCTime
  , pumpState     :: PumpToggle
  }
```

And we'll want to know when that happened:

```
-- We're doing these manually so as not to pull in all of Lens
instance HasTimeStamp PumpEvent where
  atTime f p@(PumpEvent {..}) = fmap (\time -> PumpEvent time pumpState) $ f pumpTimestamp 
```

We have some newtypes for units:

``` haskell
newtype Amps = Amps { amps :: Float } deriving (Eq, Ord, Num, Show, Real, Fractional, RealFrac)

newtype GallonsPerMinute = GallonsPerMinute { gallonsPerMinute :: Float } deriving (Eq, Ord, Num, Show, Real, Fractional, RealFrac)
```

And records for when we poll the switches and instruments:

```haskell
data SumpInstruments
  = SumpInstruments
  { sumpPumpState       :: PumpToggle
  , sumpPumpCurrentDraw :: Amps
  , sumpPumpFlow        :: GallonsPerMinute
  }

data SumpPoll
  = SumpPoll
  { sumpPollTimestamp          :: UTCTime
  , sumpPumpWaterLevelSwitches :: Switches
  , sumpPumpInstruments        :: SumpInstruments
  }

instance HasTimeStamp SumpPoll where
  atTime f p@(SumpPoll {..}) = fmap (\time -> SumpPoll time sumpPumpWaterLevelSwitches sumpPumpState sumpPumpCurrentDraw sumpPumpFlow) $ f sumpPollTimestamp
```

# Groundhog Definitions
Working with all these types in Haskell will be straightforward, but we'd like to store this data persistently.
We have to define how it's stored.
If we're willing to read all the data at once or stream it in until we find the right data, we could write a serializer and write our data to files on disk.
But we'd like to be able to query by date and time ranges, or pull in the last time the pump turned on, or any number of other queries.
And if this runs long enough, polling every second (say), it could accumulate quite a bit of data.
So we're going to store everything in a Sqlite database.
Groundhog lets us define how our Haskell types are stored in the database, and gives us native Haskell functions to query our data.

```haskell
{-# LANGUAGE QuasiQuoters #-}
{-# LANGUAGE TemplateHaskell #-}
module Sump.Groundhog

import Database.Groundhog    -- From the groundhog package
import Database.Groundhog.TH -- From the groundhog-th package
import Sump.Types
```

The `Database.Groundhog.TH` module gives us a quasiquoter and template Haskell function to define the instances Groundhog needs for our types.
We just have to tell Groundhog how to handle these types. We invoke the quasiquoter and template thus:

```haskell
mkPersist defaultCodegenConfig [groundhog|
```

[(We'll close the quasiquoter bracket later.)](https://xkcd.com/859/)
The `mkPersist` function is a Template Haskell function which will generate Groundhog instances from a `CodegenConfig` (we use the default)
and `PersistDefinitions`, which can be written using the `groundhog` quasiquoter.

## Primitives
Let's start by defining the easiest sort of thing that Groundhog can store: a primitive.
Groundhog primitives can be stored by converting them to an already-primitive value.
Since any decent SQL database will store floating-point values natively, Haskell's `Float` type is a groundhog primitive.

We're writing the input to the quasiqouter `groundhog` now.
It expects YAML-formatted input as shown by example in the [Groundhog documentation.](http://hackage.haskell.org/package/groundhog-th/docs/Database-Groundhog-TH.html)
```yaml
- primitive: Amps
  converter: ampsConverter

- primitive: GallonsPerMinute
  converter: gallonsPerMinuteConverter
```

The YAML input is a list of objects. Each object must have one of three fields: `primitive`, `embedded`, or `entity`.
The value of that field is the name of the type whose schema we are specifying.

Primitive types are intended to be stored as a simple value in a single column.
Clearly, this is how we should store Amps and GallonsPerMinute, as they are just newtype-d floating point numbers.
For a `primitive` object, we also give the name of a pair of functions, converting between the new primitive and something already primitive.
We'll show how converters are written a bit later, but for now, if `ampsConverter :: (Amps -> Float, Float -> Amps)`, what is the type of `gallonsPerMinuteConverter`?

## Embedded Types
Embedded types take up several columns, but rather than being stored in their own table, they are normalized into the table of a containing type (either embedded or entity).
For embedded types, we must give Groundhog some information about the fields of our types:

```yaml
- embedded: SumpInstruments
  fields:
    - name: sumpPumpState
      converter: sumpPumpToggleConverter
    - name: sumpPumpCurrentDraw
    - name: sumpPumpFlow
```

The last two fields have already been specified as primitives, so Groundhog needs no further information about how to store them.
For the field `sumpPumpState`, though, we must specify a converter.

## Entities
Entities roughly correspond to tables. (Tables multiply when polymorphism or multiple constructors come into play.)
Entities have keys which are used to reference values in the table.
Groundhog will automatically define an autoincremented key for an entity, but if there is already a unique value, we can use that instead.
In the case of our SumpPoll entity, we use the timestamp:

```yaml
- entity: SumpPoll
  - autoKey: null
  - keys:
    - name: SumpPollConstraint
      type: primary
      default: true
```

Two things to note: First, even if we specify our own key, we must manually disable the `autoKey` functionality.
Second, we haven't said which field(s) to use for our primary key, just that it's a primary key.
The `name` of this key is important, we'll be using it later to pick out fields on a per-constructor basis.
Since entities can have multiple constructors, we must define each one. `SumpPoll` has just one, and we now specify its fields:

```yaml
  - constructor: SumpPoll
    fields:
      - name: sumpPollTimestamp
      - name: sumpPumpWaterLevelSwitches
      - name: sumpPumpInstruments
        embeddedType:
          - name: sumpPumpState
          - name: sumpPumpCurrentDraw
          - name: sumpPumpFlow
```

The `embeddedType` section lets us define how the the columns for `SumpInstruments` type are included in the `SumpPoll` table.

Now we can specify the constraint to be used for the SumpPoll primary key with this constructor:

```yaml
    uniques:
      - name: SumpPollConstraint
        fields: sumpPollTimestamp
```

Note that the `name` for the constraint matches the `name` for the key above.
This specifies that when a `SumpPoll` whose constructor is `SumpPoll` is stored, the `sumpPollTimestamp` field must be unique and is used as the key.

We've defined the database model for our types, so we can close the quasiquoter now:

```haskell
|]
```

![Phew](kirk-relief.gif) 

Now we need to define the converters we mentioned above.
As previously discussed, converters are pairs of functions, one to go from the type we're converting to the type stored in the DB, and the other to go back.

```haskell
ampsConverter :: (Amps -> Float, Float -> Amps)
ampsConverter = (unAmps, Amps)

gallonsPerMinuteConverter :: (GallonsPerMinute -> Float, Float -> GallonsPerMinute)
gallonsPerMinuteConverter = (unGallonsPerMinute, GallonsPerMinute)

pumpToggleConverter :: (PumpToggle -> Bool, Bool -> PumpToggle)
pumpToggleConverter = (pumpBool, boolPump)
  where
    pumpBool PumpOn = True
    pumpBool PumpOff = False
    boolPump True = PumpOn
    boolPump False = PumpOff
```

That's it! We've now got a database model for our Haskell types.
Now we can move on to actually store some things in a database.

# Connections and Queries
