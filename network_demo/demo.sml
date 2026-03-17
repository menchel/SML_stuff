(* Load all dependencies [cite: 39] *)
use "packet.sml";
use "utils.sml";
use "crc.sml";
use "sender.sml";
use "channel.sml";
use "receiver.sml";

structure StatsSuite =
struct
    (* Configuration [cite: 40] *)
    val iters = 50  
    val packetSize = 8

    val generators = [
        ("CRC-4 (Standard)", [true, false, true, true]), 
        ("CRC-8 (Simple)",   [true, false, false, false, false, true, true, true])
    ]

    val messages = [
        "Short", 
        "Hello World!", 
        "This is a significantly longer message to test higher packet counts."
    ]

    val errorRates = [0.0, 0.001, 0.01, 0.05, 0.1]

    (* Runs one full pass through the channel and returns (Total, Valid) [cite: 42] *)
    fun runTrial msg gen prob =
        let
            val original = Sender.makePackets msg packetSize gen
            val noisy = Channel.transmit prob original
            val valid = Receiver.filterValidPackets gen noisy
        in
            (length original, length valid)
        end

    fun run () =
        let
            (* Open the results.txt file for writing *)
            val outStream = TextIO.openOut "results.txt"
            
            (* Helper to write to file and flush immediately *)
            fun say s = (TextIO.output (outStream, s); TextIO.flushOut outStream)

            val _ = say "\n=== STARTING MASSIVE STATISTICAL TEST ===\n"
            val _ = say ("Iterations per case: " ^ Int.toString iters ^ "\n")
            val _ = say "------------------------------------------------------------------\n"

            fun runMsg msg =
                List.app (fn (genName, gen) =>
                    List.app (fn prob =>
                        let
                            fun sum (0, s, v) = (s, v)
                              | sum (n, s, v) =
                                let 
                                    val (ts, tv) = runTrial msg gen prob
                                in 
                                    sum (n - 1, s + ts, v + tv) 
                                end
                            
                            val (totalS, totalV) = sum (iters, 0, 0)
                            val successRate = (Real.fromInt totalV / Real.fromInt totalS) * 100.0
                        in
                            say ("Msg: " ^ String.substring(msg ^ "          ", 0, 10) ^ " | ");
                            say ("Gen: " ^ genName ^ " | ");
                            say ("BER: " ^ Real.toString prob ^ " | ");
                            say ("Success: " ^ Real.fmt (StringCvt.FIX (SOME 2)) successRate ^ "%\n")
                        end
                    ) errorRates
                ) generators
        in
            List.app runMsg messages;
            say "------------------------------------------------------------------\n";
            say "=== TEST COMPLETE ===\n";
            
            (* CRITICAL: Close the file stream *)
            (TextIO.closeOut outStream; print "Results written to results.txt\n")
        end
end

(* Execute the simulation [cite: 52] *)
val _ = StatsSuite.run ();
