structure CRC =
struct
  type bit = bool
  type polynom = bit list

  fun xor [] p2 = p2
    | xor p1 [] = p1
    | xor (x::xs) (y::ys) = (x <> y) :: (xor xs ys)

  fun trim [] = []
    | trim (false::bits) = trim bits
    | trim bits = bits

  fun modulo [] _ = []
    | modulo message generator =
        let
            val msg = trim message
            val gen = trim generator
        in
            if length msg < length gen then msg
            else modulo (trim (xor msg gen)) gen
        end

  fun shiftLeft bits 0 = bits
    | shiftLeft bits n = shiftLeft (bits @ [false]) (n - 1)

  fun createCRC message generator =
      let
          val gen = trim generator
          val deg = length gen - 1
          val movedMsg = shiftLeft (trim message) deg
          val remainder = modulo movedMsg gen
          fun pad rem = if length rem < deg then pad (false::rem) else rem
      in
          pad remainder
      end

  fun checkCRC message generator crc = 
      (createCRC message generator) = crc
end
