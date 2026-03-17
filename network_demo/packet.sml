structure Packet =
struct
  type bit = bool;
  type polynom = bit list;

  datatype packet = Packet of 
    {
        sequenceNumber: int,
        message: polynom,
        crc: polynom
    }

    fun getSeq (Packet {sequenceNumber, message, crc}) = sequenceNumber
    fun getMessage (Packet {sequenceNumber, message, crc}) = message
    fun getCRC (Packet {sequenceNumber, message, crc}) = crc
    
    fun makePacket seqNum message crc = Packet {sequenceNumber = seqNum, message = message, crc = crc}

    fun bitToString true = "1"
      | bitToString false = "0"

    fun bitsToString bits =
        String.concat (map bitToString bits)

    fun printPacket (Packet {sequenceNumber, message, crc}) =
    (
        print ("Packet " ^ Int.toString sequenceNumber ^ "\n");
        print ("  Payload: " ^ bitsToString message ^ "\n");
        print ("  CRC    : " ^ bitsToString crc ^ "\n\n")
    )

end
