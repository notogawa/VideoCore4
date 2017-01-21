module VideoCore4.QPU.Instruction.Types where

import Data.Bits
import Data.Int
import Data.Word

class To64 x where
  to64 :: x -> Word64

instance To64 Int32 where
  to64 x = (toEnum (fromEnum (x `testBit` 31)) `shiftL` 31) .|. toEnum (fromEnum (x .&. 0x7FFFFFFF))

instance To64 Word16 where
  to64 = toEnum . fromEnum
