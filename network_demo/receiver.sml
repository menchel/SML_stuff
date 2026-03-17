structure Receiver =
struct
  open Packet
  open CRC

  fun validatePacket generator pkt =
      let
          val msg = getMessage pkt
          val crc = getCRC pkt
      in
          checkCRC msg generator crc
      end

  fun analyzePackets generator packets =
      List.map (fn p => (getSeq p, validatePacket generator p)) packets

  fun filterValidPackets generator packets =
      List.filter (validatePacket generator) packets
end
