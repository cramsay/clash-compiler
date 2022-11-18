module DualBlockRam2 where

import Clash.Prelude
import Clash.Explicit.Testbench
import qualified Clash.Explicit.Prelude as Explicit

type RamData = Unsigned 10
type RamAddrN = 1024

defAt0
  :: (HiddenClockResetEnable dom, Default a, NFDataX a)
  => Signal dom a
  -> Signal dom a
defAt0 a = mux en a def
  where
    en =
      delay False $
      delay False $
      (pure True)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (RamOp RamAddrN RamData)
  -> Signal System (RamOp RamAddrN RamData)
  -> Signal System (RamData, RamData)
topEntity = exposeClockResetEnable go where

  go opA opB = defAt0 $ bundle dout where
    dout =
      trueDualPortBlockRam opA opB
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    (opA, opB) =
      unbundle $ stimuliGenerator
        clk rst
        ( -- Write some values
             (RamWrite 0 10, RamWrite 1 11)
          :> (RamWrite 2 12, RamWrite 3 13)
          :> (RamWrite 4 14, RamWrite 5 15)
          :> (RamWrite 6 16, RamWrite 7 17)

          -- Read written values
          :> (RamRead 0, RamRead 1)
          :> (RamRead 2, RamRead 3)
          :> (RamRead 4, RamRead 5)
          :> (RamRead 6, RamRead 7)

          -- Interleave reads and writes (no collision)
          :> (RamRead  1   , RamWrite 0 20)
          :> (RamWrite 1 21, RamRead  0   )
          :> (RamRead  3   , RamWrite 2 22)
          :> (RamRead  2   , RamWrite 3 23)

          :> Nil

          )

    expectedOutput =
      outputVerifier' clk rst
        (    (0,0)

          -- Writes echoed
          :> (10, 11)
          :> (12, 13)
          :> (14, 15)
          :> (16, 17)

          -- Read back from memory
          :> (10, 11)
          :> (12, 13)
          :> (14, 15)
          :> (16, 17)

          -- Read and writes interleaved
          :> (11, 20)
          :> (21, 20)
          :> (13, 22)
          :> (22, 23)

          :> Nil)

    done           = expectedOutput (topEntity clk rst enableGen opA opB)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
