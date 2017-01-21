module VideoCore4.QPU.Instruction.OpAdd
       (
         OpAdd
       , op_add_NOP
       , op_add_FADD
       , op_add_FSUB
       , op_add_FMIN
       , op_add_FMAX
       , op_add_FMINABS
       , op_add_FMAXABS
       , op_add_FTOI
       , op_add_ITOF
       , op_add_ADD
       , op_add_SUB
       , op_add_SHR
       , op_add_ASR
       , op_add_ROR
       , op_add_SHL
       , op_add_MIN
       , op_add_MAX
       , op_add_AND
       , op_add_OR
       , op_add_XOR
       , op_add_NOT
       , op_add_CLZ
       , op_add_V8ADDS
       , op_add_V8SUBS
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype OpAdd = OpAdd { unOpAdd :: Word8 } deriving (Eq, Show, Typeable)

instance To64 OpAdd where
  to64 = toEnum . fromEnum . unOpAdd

op_add_NOP :: OpAdd
op_add_NOP = OpAdd 0
op_add_FADD :: OpAdd
op_add_FADD = OpAdd 1
op_add_FSUB :: OpAdd
op_add_FSUB = OpAdd 2
op_add_FMIN :: OpAdd
op_add_FMIN = OpAdd 3
op_add_FMAX :: OpAdd
op_add_FMAX = OpAdd 4
op_add_FMINABS :: OpAdd
op_add_FMINABS = OpAdd 5
op_add_FMAXABS :: OpAdd
op_add_FMAXABS = OpAdd 6
op_add_FTOI :: OpAdd
op_add_FTOI = OpAdd 7
op_add_ITOF :: OpAdd
op_add_ITOF = OpAdd 8
op_add_ADD :: OpAdd
op_add_ADD = OpAdd 12
op_add_SUB :: OpAdd
op_add_SUB = OpAdd 13
op_add_SHR :: OpAdd
op_add_SHR = OpAdd 14
op_add_ASR :: OpAdd
op_add_ASR = OpAdd 15
op_add_ROR :: OpAdd
op_add_ROR = OpAdd 16
op_add_SHL :: OpAdd
op_add_SHL = OpAdd 17
op_add_MIN :: OpAdd
op_add_MIN = OpAdd 18
op_add_MAX :: OpAdd
op_add_MAX = OpAdd 19
op_add_AND :: OpAdd
op_add_AND = OpAdd 20
op_add_OR :: OpAdd
op_add_OR = OpAdd 21
op_add_XOR :: OpAdd
op_add_XOR = OpAdd 22
op_add_NOT :: OpAdd
op_add_NOT = OpAdd 23
op_add_CLZ :: OpAdd
op_add_CLZ = OpAdd 24
op_add_V8ADDS :: OpAdd
op_add_V8ADDS = OpAdd 30
op_add_V8SUBS :: OpAdd
op_add_V8SUBS = OpAdd 31
