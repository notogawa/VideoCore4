module VideoCore4.QPU.Instruction.OpMul
       (
         OpMul
       , op_mul_NOP
       , op_mul_FMUL
       , op_mul_MUL24
       , op_mul_V8MULD
       , op_mul_V8MIN
       , op_mul_V8MAX
       , op_mul_V8ADDS
       , op_mul_V8SUBS
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype OpMul = OpMul { unOpMul :: Word8 } deriving (Eq, Show, Typeable)

instance To64 OpMul where
  to64 = toEnum . fromEnum . unOpMul

op_mul_NOP :: OpMul
op_mul_NOP = OpMul 0
op_mul_FMUL :: OpMul
op_mul_FMUL = OpMul 1
op_mul_MUL24 :: OpMul
op_mul_MUL24 = OpMul 2
op_mul_V8MULD :: OpMul
op_mul_V8MULD = OpMul 3
op_mul_V8MIN :: OpMul
op_mul_V8MIN = OpMul 4
op_mul_V8MAX :: OpMul
op_mul_V8MAX = OpMul 5
op_mul_V8ADDS :: OpMul
op_mul_V8ADDS = OpMul 6
op_mul_V8SUBS :: OpMul
op_mul_V8SUBS = OpMul 7
