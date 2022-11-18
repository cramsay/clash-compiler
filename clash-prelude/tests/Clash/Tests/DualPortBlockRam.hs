-- Assert correct behavior:
--
-- Read-back:
--   Most recently written data should be retrievable from either port.
--   Same-port read/write behaviour is WriteFirst.
--   Previous read output is held during any `RamNoOp`s.
--
-- Address collisions:
--   + Read-Read collisions are OK and return valid data
--   + Write-Read collisions result in an undefined read and successful write
--   + Write-Write collisions are undefined

{-# LANGUAGE NoImplicitPrelude #-}

module Clash.Tests.DualPortBlockRam (tests) where

import qualified Data.List as L
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Explicit.Prelude as E
import Clash.Prelude

type RamData = Unsigned 10
type RamAddrN = 1024
type RamCtrl = RamOp RamAddrN RamData

type TDP = (   Signal System RamCtrl
            -> Signal System RamCtrl
            -> Signal System (Maybe RamData, Maybe RamData)
           )

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

ram :: TDP
ram opA opB =
  both maybeIsX <$> bundle (E.trueDualPortBlockRam clockGen clockGen opA opB)

type Samples = [(RamCtrl, RamCtrl, (Maybe RamData, Maybe RamData))]

seqReadBack, interReadBack, conflictRR, conflictWW, conflictWR, conflictRW
  :: Samples

seqReadBack =
  -- opA            opB            expectedOutput
  [ -- Write data
    (RamWrite 0 10, RamWrite 1 11, (Nothing, Nothing))
  , (RamWrite 2 12, RamWrite 3 13, (Just 10, Just 11))
  , (RamWrite 4 14, RamWrite 5 15, (Just 12, Just 13))
  , (RamWrite 6 16, RamWrite 7 17, (Just 14, Just 15))
    -- Read back
  , (RamRead  0   , RamRead  1   , (Just 16, Just 17))
  , (RamRead  2   , RamRead  3   , (Just 10, Just 11))
  , (RamRead  4   , RamRead  5   , (Just 12, Just 13))
  , (RamRead  6   , RamRead  7   , (Just 14, Just 15))
    -- Pad
  , (RamNoOp      , RamNoOp      , (Just 16, Just 17))
  , (RamNoOp      , RamNoOp      , (Just 16, Just 17))
  ]

interReadBack =
  [ -- Swap ops
    (RamWrite 0 10, RamRead  1   , (Nothing, Nothing))
  , (RamRead  0   , RamWrite 1 11, (Just 10, Nothing))
  , (RamWrite 2 12, RamRead  3   , (Just 10, Just 11))
  , (RamRead  2   , RamWrite 3 13, (Just 12, Nothing))
    -- Swap ports
  , (RamWrite 4 14, RamRead  5   , (Just 12, Just 13))
  , (RamWrite 5 15, RamRead  4   , (Just 14, Nothing))
  , (RamWrite 6 16, RamRead  7   , (Just 15, Just 14))
  , (RamWrite 7 17, RamRead  6   , (Just 16, Nothing))
    -- Pad
  , (RamNoOp      , RamNoOp      , (Just 17, Just 16))
  ]

conflictRR =
  [ -- Read same addr on both ports is OK
    (RamWrite 1 11, RamNoOp      , (Nothing, Nothing))
  , (RamRead  1   , RamRead  1   , (Just 11, Nothing))
  , (RamNoOp      , RamNoOp      , (Just 11, Just 11))
  ]

conflictWW =
  [ -- Write same addr on both ports corrupts word
    (RamWrite 1 11, RamWrite 1 12, (Nothing, Nothing))
  , (RamRead  1   , RamNoOp      , (Nothing, Nothing))
  , (RamNoOp      , RamNoOp      , (Nothing, Nothing))
  ]

conflictWR =
  [ -- Writing port A while reading port B gives undefined read
    (RamWrite 1 11, RamRead  1   , (Nothing, Nothing))
  , (RamNoOp      , RamNoOp      , (Just 11, Nothing))
  ]

conflictRW =
  [ -- Writing port B while reading port A gives undefined read
    (RamRead  1   , RamWrite 1 11, (Nothing, Nothing))
  , (RamNoOp      , RamNoOp      , (Nothing, Just 11))
  ]

tdpAssertion
  :: TDP
  -> Samples
  -> Assertion
tdpAssertion ram0 samples = actual @?= expectedOutput
 where
  (opA, opB, expectedOutput) = L.unzip3 samples
  actual = sampleN (L.length samples) $ ram0 (fromList opA)
                                             (fromList opB)
tests :: TestTree
tests = testGroup "DualPortBlockRam"
  [ testCase "Sequential read-back"  $ tdpAssertion ram seqReadBack
  , testCase "Interleaved read-back" $ tdpAssertion ram interReadBack
  , testCase "Conflict Read-Read"    $ tdpAssertion ram conflictRR
  , testCase "Conflict Write-Write"  $ tdpAssertion ram conflictWW
  , testCase "Conflict Write-Read"   $ tdpAssertion ram conflictWR
  , testCase "Conflict Read-Write"   $ tdpAssertion ram conflictRW
  ]
