structure Sender =
struct
    open Packet
    open Utils

    fun buildPacket generator seq payload =
        let
            val crc = CRC.createCRC payload generator
        in
            makePacket seq payload crc
        end

    fun makePackets message payloadSize generator =
        let
            val bits = stringToBits message
            val payloads = chunks payloadSize bits
            fun build (payload, seq) = buildPacket generator seq payload
            val indexed = ListPair.zip (payloads, List.tabulate(length payloads, fn i => i))
        in
            map build indexed
        end
end
