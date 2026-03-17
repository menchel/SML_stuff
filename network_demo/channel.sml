structure Channel =
struct
  open Packet
  open Utils
  open Random

  fun flipBit p (r:int) b =
      if r < round (p * 1000.0) then not b else b

  fun corruptBits p gen bits =
      let
          fun process [] g = ([], g)
            | process (b::bs) g =
                let
                    (* Adjusted: randRange takes range as one arg, gen as second *)
                    val r = randRange (0, 999) g
                    val flipped = flipBit p r b
                    val (rest, nextGen) = process bs g
                in
                    (flipped :: rest, nextGen)
                end
      in
          process bits gen
      end

  (* Transmit: Threads the generator through the list of packets *)
  fun transmit p packets =
      let
          (* Fixed: Using a standard seed initialization if mkRNG is missing *)
          val gen = rand (0, 42) 
          
          fun transmitAll [] _ = []
            | transmitAll (pkt::pkts) g =
                let
                    val (corruptedMsg, g') = corruptBits p g (getMessage pkt)
                    val newPkt = makePacket (getSeq pkt) corruptedMsg (getCRC pkt)
                in
                    newPkt :: transmitAll pkts g'
                end
      in
          transmitAll packets gen
      end
end
