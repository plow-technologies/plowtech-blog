The modicon-prime library is an interface for communicating to a Modbus device. The library is designed to take as much of the complexity out of gathering and writing data as possible while maintaining versatility to be used with as many Modbus devices as possible.

# What is so difficult about talking to different Modbus devices?

## Data Types
Standard Modbus Protocol is based on 16 bit registers, and each register is treated as a single data point. Except in very rare cases, using 16 bits for data means that you are limited to using Integers, and limits values either from -32,768 to 32,767 for signed integers or from 0 to 65535 for unsigned integers. Unfortunately in today's computing world, we often are faced with data that requires floats, doubles, and other values too large for the 16 bit range. In order to continue to use the Modbus Protocol and get the larger data values, companies began using consecutive registers to store the larger numbers. As there was no standardization for this, each company came up with its own way of combining registers, mostly based on what their particular hardware/computations could process the most efficiently. Basically, every company and/or device has its own protocol layered on top of the Modbus protocol. These company/device specific protocols have three main areas where they can differ from each other: Register Structure, Endianness, and Byte Swap. Let's dive into these differences.

### Register Structure
In order to handle data that spans more than 1 register (2 bytes), each company devised a map of these expected values into the existing registers by using one of the following techniques:

1. Making all data points take up 2 registers.
    * Pros:
        1. Sending and receiving of bytes simpler because all data is 2 registers.
        2. Related parameters can be grouped together to reduce number of requests to the device.
        3. Since each data point size is known(2 registers), only response data that is needed, has to be parsed allowing for faster parsing.
        4. Only data points that are needed, have to be defined
    * Cons:
        1. Inflates packet sizes when requesting data points that could fit into 1 register (i.e. Word8,Int16)
        2. Doubles either cause 4 registers to be used for all data (compounding overhead), or have the double divided into 2 data points (negates making everything uniform length), or not have doubles.
2. Taking sections of the register map and designating them a certain type of data. For instance, (registers 0-1000 are 16 bit integer values, registers 2001-4000 are 32 bit float values, etc).
    * Pros:
        1. Sending and receiving bytes is simple as long as requests reside inside a data type block.
        2. Registers can be defined as needed (no full mapping required) since anything in a group is of the same data type.
        3. No extra bytes are sent per data type block as the number of registers requested can directly match the data stored.
        4. Parsing could be defined by data type block and not require information from the request.
    * Cons:
        1. Separate requests must be sent when trying to gather data of differing types(almost always). 
3. Assigning the appropriate number of registers for the data type, but not chunking the data types together.
    * Pros:
        1. Number of data bytes matches needed size so no extra bytes are sent.
        2. Related parameters can be grouped together to reduce number of requests to the device.
    * Cons:
        1. All Registers have to be explicitly defined since data types are intermixed
        2. Parsing of data must be done on a per data point level basis, and every register in a response must be processed

### Endianness
There are two types of Endianness called Little Endian and Big Endian. Little Endian means that the least significant byte comes first in a transmission followed by the next least significant byte and so on. Big Endian is the reverse in that the most significant byte comes first in a transmission followed by the second most signficant byte and so on. For instance, 0xABCDEFGH stored in Little Endian would be 0xGH 0XEF 0xCD 0xAB, or in Big Endian 0xAB 0xCD 0xEF 0xGH. That is the standard idea on applying Endianness, and it is usually applied per piece of data. So for Modbus Protocol, Endianness is applied per register. For example, 0xABCD (2 bytes for a register) stored in Big Endian would be 0xAB 0xCD, and Little Endian would be 0xCD 0xAB. This still follows the pattern. Now when two registers are combined into 0xABCDEFGH, we get Big Endian 0xAB 0xCD 0xEF 0xGH, but for Little Endian we get 0xCD 0xAB 0xGH 0xEF. Due to the structure breaking down at two registers, a feature called byte swap to force the data back to a standardized structure. (* A third Endianness called Mixed/Middle Endian is structurally the same as the Little Endian of 2 registers.) To make things even more confusing, some devices store the data Big Endian for each register, but Little Endian for combined registers. This is why Endianness is independent of byte swap.

### Byte Swap
As mentioned in Endianness, byte swap is rearranging bytes so that they follow the standard Endianness rules. For instance, lets take the Little Endian version of the 2 combined register data, 0xCD 0xAB 0xGH 0xEF. If you were to swap the first register with the second register, it would become 0xGH 0xEF 0xCD 0xAB. Which is the standard Little Endian, and is easily computed using standard deserialization functions. The other case is the when each register is stored Big Endian, but combined registers are stored Little Endian, 0xEF 0xGH 0xAB 0xCD. Again swapping the two registers, it would become 0xAB 0xCD 0xEF 0xGH.

## Modbus TCP vs Modbus RTU/ASCII vs Modbus RTU over TCP
There are three ways that Modbus is generally transmitted to a device. All three ways use the standard Modbus Protocol (Data Types aside), but wrap them into other communication protocols.

### Modbus TCP
This communiation type takes a modbus communication and simply places it into the data section of a TCP transmission. Handshaking,data transfer, and checksums are all handled at the TCP communication level.

### Modbus RTU/ASCII
This RTU communication type is used when talking directly through a serial connection to a device. The wrapping is done in the following steps:
* 3.5 char silence before transmission
* Address of device (Word8)
* Modbus Message
* Checksum (CRC16 for RTU)(LRC for ASCII)
* 3.5 char silence after transmission

### Modbus RTU over TCP
The third communication type is a hybrid of TCP and RTU transmission types. It takes a standard Modbus RTU transmission and wraps it inside a TCP transmission. The reasoning for this is that the communication is expected to travel over large distances through an TCP/IP network, but when it reaches the TCP destination, it is then transferred to a serial line network to the device. For instance, a remote server is talking through the cellular network to a cellular modem, when that modem receives the TCP data transmission(doing all handshakes,checksums,etc.), it can take the data from the TCP message and write it directly to its serial port (i.e. RS232,RS485,USB,etc.) which is connected to a serial port on the remote Modbus device. Many PLCs have "ethernet cards", that do this hand-off, while still others have the feature built direclty into the PLC. Each type needs the RTU layer wrapped in the TCP layer to function.

# What information is needed in order to read/write from a Modbus device?
In order to read/write from a Modbus device, we need to know what type of transport protocol and how it is configured, and how the memory is mapped inside the Modbus device. The combined configuration of these two elements is defined as the Modbus Settings. Generally, the transport settings will be unique to each and every device, where the memory tends to be more static for each type of device. Let's look deeper into each of these settings.

## Modbus Settings

### Modbus Network Type
The Modbus Network Type defines the transmission protocol that will contain the Modbus communications. The three types are defined as:

1. ModbusRTU (communicating through serial only) which requires:
    * DevicePath - a string of the path to the serial port (i.e. "/dev/USB0"
    * ModbusSerialPortSettings(unboxed SerialPortSettings) these are the serial port settings to talk to the Modbus device
        * CommSpeed - baudrate (i.e. CS2400,CS480,CS9600,CS19200, etc.)
        * StopBits - One, Two
        * Parity - Even, Odd, NoParity
        * FlowControl - Software, NoFlowControl
        * Timeout - Int (given in 10ths of a second)
    * CRC16Config - settings for a CRC checker (theoretically should always be modbusConfig, but could technically be different based on device)
2. ModbusTCP (communicating via ethernet no crc needed) which requires:
    * HostName - a string of the public ip to the device
    * ServiceName - a string of the public port number to the device
    * UnitId - the devices Unit Id (sometimes called the slave id)
3. ModbusRTUviaTCP (communicating via ethernet crc needed) which requires:
    * HostName - a string of the public ip to the device
    * ServiceName - a string of the public port number to the device
    * UnitId - the devices Unit Id (sometimes called the slave id)
    * CRC16Config - settings for a CRC checker (theoretically should always be modbusConfig, but could technically be different based on device)
  
### Modbus Configuration
The Modbus Configuration defines the Endianness and ByteSwap of the device's memory as well as how the devices data points are mapped. Endianness and ByteSwap were discussed earlier, let's now tackle how the data points are mapped.

#### ModbusMapping
The Modbus Mapping is where the structure of the data is specifically defined. This the key area where the flexibility is needed. In the ModbusMapping there are three elements: Device Index, Modbus Parameter Type, Interval of Modbus Addresses. Let's take a look at each one.

##### Device Index
The device index is the indexing of parameters based off of the memory map of the device. These need to be unique and have an order so that they can be placed in a map. Due to the fact that this is a parameterized type, it allows the flexibility to shape Modbus data for the device, instead of trying to force the device into the Modbus mold. For instance, the WellPilot RPOC has each parameter as 2 Modbus Holding registers, and they reference the data by parameter number. So WellPilot parameter 20 (Idle Time) is actually Modbus Holding Register 40 and the 2 bytes following it. We can either make the parameterized type something like "WellPilotIndex 20" and make all other points (newtype WellPilotIndex = WellPilotIndex {_wellIndex :: Int}) , or we could create sum type (data WellPilotParameters = WellPilotIdleTime,WellPilotPumpFillage,WellPilotRunTime, etc..). As long as Ord and Eq are derived, the implementation can be as complex or basic as needed.

##### Modbus Parameter Type
The Modbus Parameter Type defines how the data should be transmitted to and from the device. As Modbus only expects 2 byte registers in its messages (requests/responses) and it does not have any method to define what the data represents, the Modbus Parameter is the only way to define the actual data. Parameter Type also verifies against the interval of modbus registers that are being requested. For example, a FloatParameter requires that there be exactly 2 registers being requested as a DoubleParameter would require exactly 4 registers. Any fewer or more registers automatically means that something is wrong. In addition, when trying to chunk requests, a parameter cost (number of bytes) is computed in such a way that it will always be contained in a single request.

##### ModbusAddress Interval
The modbus interval is used to define exactly the modbus registers used for the parameter. For instance, a read/write FloatParameter at register 100, then it would have to be Interval (HoldingRegister 100 ... HoldingRegister 101). Once again when matched with the ModbusParameterType, you can confirm that you are getting the correct number of bytes/registers or error out that there is a mismatch. The matching of the two fields is key when building the ModbusMapping. By definition, the data for each parameter must be contained in contiguous interval of Modus Addresses, and inside the interval the addresses must be of only one constructor from the ModbusAddress sum type. So you can have Invterval (CoilRegister 10 ... CoilRegister 25), but you can NOT have Interval (CoilRegister 100 ... DiscreteRegister 110).

# How do get read/write data from a Modbus Device and what kind of response should I expect?

The communicateModbus function is the gate to all communications with the ModbusDevice. It requires two things, ModbusSettings and ModbusRequests. Let's explore how they are used for reading data and writing data.


  **Note: _communicateModbus function takes a sequence of ModbusRequests and the function excutes each request from left to right and independently of one another, but returns a common list of responses.(i.e Seq ModbusRequest = ReadDataRequest1, WriteDataRequest, ReadDataRequest2, ReadDataRequest3, would do the entire ReadDataRequest1, then the entire WriteDataRequest, then the entire ReadDataRequest2, then the entire ReadDataRequest3 and return the combined results.) The advantage to putting together a sequence of requests instead of just calling communicateModbus over and over is that only one connection (TCP,RTU) is made for the entire process._**

## ModbusSettings
The ModbusSettings are very important for both reading data and writing data. These settings explain how the communication is encapsulated (RTU,TCP,RTUviaTCP), how the communications are serialized (Endianness,ByteSwap), and the memory map of the device (Map idx ModbusMapping). The memory map must contain all the parameters that will be read from or written to.

## ModbusRequests

There are two types of requests: ReadDataRequest and WriteDataRequest.

### ReadDataRequest
Each ReadDataRequest requires a set of user defined indices. These indices must be a key in the memory map (Map idx ModbusMapping) inside the ModbusSettings, but has no other constraints. This allows for a user to request a bunch of differing parameters with out having to do any sorting. The sorting is all done in the underlying code as well as any chunking that is needed. Because the chunking and sorting is done underneath, one ReadDataRequest can and usually does send multiple requests to the Modbus device. This is important to understand as one request does not mean one communication transaction, so it is possible to get partial data back. In order to take advantage of the chunking algorithm, it is beneficial to fill in the memory map (Map idx ModbusMapping) as much as possible. This is because the Standard Modbus Protocol requests data in contiguous address chunks, so a request has to be for all data between two addresses. If the chunking algorithm does not know about any one of the Addresses in between two requested Addresses, then it creates two seperate requests instead of just one. Let's look at an example:

   Lets say parameter1 has (Interval (HoldingRegister 3 ... HoldingRegister 4)) and parameter2 has (Interval (HoldingRegister 7 ... HoldingRegister 8)), and the memory map does NOT have a parameter for (Interval (HoldingRegister 5 ... HoldingRegister 6)), the a ReadDataRequest (Set (parameter1,parameter2)) would end up being two seperate requests sent to the device and parsed seperately. If a parameter3 with (Interval (HoldingRegister 5 .. HoldingRegister 6)) existed, then the chunker knows how to handle the data, and can create a single request.

You might be wondering, "If I can put all parameters in one ReadDataRequest, then why does 'communicateModubus' take a Sequence of ModbusRequests?". The first reason is to be able to send a WriteDataRequest followed by a ReadDataRequest which allows confirmation of a write to a parameter. The second reason is that some Modbus devices have varying rules about how data is read from the device. For instance, Spirit stores time in 3 seperate registers(Hours,Minutes,Seconds) independent of each other, but will only return data for the last two registers if the first register (Hours) is read. Knowing that you can create 2 ReadDataRequest, one with the clumped time registers and one with other data. 

   **Note: _ReadDataRequest always tries to minimize the number of actual requests to the device as possible. So as long as a parameter can fit into the current request(response will be under 256 bytes), it will be added to the request. This is true even if two parameters are the only registers requested._**

### WriteDataRequest
Each WriteDataRequest requires a (Map (user defined index) MobusData). The index must be a key in the memory map (Map idx ModbusMapping) inside the ModbusSettings, and the ModbusData must match the ModbusParameterType of the ModbusMapping of the index key. This allows for a user to make a single request to write to a large number of varied parameters without having to do any sorting. Just like the read request, all the chunking and sorting is done internally. Unlike the read request, filling the memory map beyond the requested write parameters does nothing with the efficiency of write requests being made. The chunking algorithm will assemble all contiguous addresses together and make a single write request for each such grouping (will also break groupings by 256 bytes). The Modbus Protocol has a single register write function and a multiple register write function which is also implemented as part of the chunking algorithm. Any time that a single parameter with a single register is found during the chunking, it is turned into a single register write. Any time two or more contiguous registers are being written, a multiple register write is created. As some devices only support single register writes, a way to guarantee a single register write would be to make a sequence of WriteDataRequest with a singleton map associated with each one. Just like the ReadDataRequest, a single WriteDataRequest does not necessitate a single response. 

## ModbusResponse

There are three types of responses: ReadDataResponse, WriteDataResponse, and ModbusPollErrors.

### ReadDataResponse

The ReadDataResponse contains a (Map index ModbusData) of all successful read responses from the Modbus device. The map will include any indices that were created during the chunking process and their associated data. If a request failed or the response was not parsed, the error will go into the ModbusPollErrors, but will leave any previous successful responses untouched. This means:
1. ReadDataResponse only contains successful response data
2. Unexpected data (Data not requested) can appear in the ReadDataResponse because the actual requests fill in missing indexes if it makes polling more efficient
3. Only 1 ReadDataResponse should be present in the ModbusResponse list
4. User must verify that the data they expected is present and not assume everything worked when a ReadDataResponse is present in the ModbusResponse list

### WriteDataResponse

The WriteDataResponse contains a (Map index ModbusData) of all successful write responses from the Modbus device given in a WriteDataRequest. Depending on how the request algorithm splits a WriteDataRequest, the ModbusData in the return map will either have and actual data value or UnavailableData. This is because a multiregister write does not return data, while a single register write should return the data that was written. This also means, any data that is 2 register (4 bytes) long will always return an UnavailableData for the ModbusData. It is also important to note just like the ReadDataResponse, the user must verify that the index that was expected to be written to is in the WriteDataResponse since it will combine all write responses into a single WriteDataResponse.

# Key Takeaways

* When making ModbusRequests, include as many indices/(Map index ModbusData) as possible and as big a (Map index ModbusMapping) as possible to condense the number of polls.
* Keep in mind each ModbusRequest passes through an algorithm that emphasizes fewest number of requests sent to the device, some devices will only accept certain requests (i.e. Single Writes) so may require individual WriteDataRequest for certain parameters.
* A single ModbusRequest does not mean a single request sent to the device 
* A ModbusResponse is combined culmination of all the ModbusRequests made, so only 1 ModbusResponse will be returned each time the poll routine is called. If a parameter is targeted multiple times, only the last response is returned.
* ModbusResponse only contains valid responses, and can contain more data than requested due to efficiency algorithm. It is up to the user to check for data, and decide what to do with extra/or missing data.
* Choosing ModbusNetworkTypes
    * ModbusRTU should only be used when polling the device locally through a serial cable.
    * ModbusTCP should be used when the connection to the device is completely through TCP (internet/ethernet). If the device does not respond, try using ModbusRTUviaTCP (explained below).
    * ModbusRTUviaTCP has two use cases:
        1. Connection to the device has a TCP (internet/ethernet) and a serial connection. For instance, messages are sent remotely to a cell modem, but the cell modem then relays the message through its serial port to the modbus device.
        2. Some devices expect a TCP encapsulated RTU message even though they have a TCP port directly on the device. Simply put, the ethernet port acts like the cell modem situation above.
* ModbusMapping is the definition of a data point in the device.
    * Device Index is used to uniquely define the data point(i.e. TotalFlowBatteryLevel,Register1,WellPilotIdleTime,...).
    * ParameterType is used to decode/encode the data.
    * ModbusInterval is used to sort parameters and decide which modbus request to make to the device.
