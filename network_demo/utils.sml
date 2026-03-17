structure Utils =
struct
    type bit = bool

    fun bitToString true = "1"
    | bitToString false = "0"

    fun bitsToString bits =
        String.concat (map bitToString bits)

    fun intToBits n size =
    let
        fun build 0 acc _ = acc
        | build k acc v =
                build (k-1) ((v mod 2 = 1)::acc) (v div 2)
    in
        build size [] n
    end

    fun charToBits c =
        intToBits (Char.ord c) 8

    fun stringToBits s =
        List.concat (map charToBits (String.explode s))

    fun chunks size xs =
    let
        fun take 0 xs acc = (rev acc, xs)
        | take _ [] acc = (rev acc, [])
        | take n (x::xs) acc = take (n-1) xs (x::acc)

        fun loop [] = []
        | loop xs =
            let
                val (chunk, rest) = take size xs []
            in
                chunk :: loop rest
            end
    in
        loop xs
    end

end
