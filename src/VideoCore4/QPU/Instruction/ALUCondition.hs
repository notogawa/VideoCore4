module VideoCore4.QPU.Instruction.ALUCondition
       (
         ALUCondition
       , cond_NV
       , cond_AL
       , cond_ZS
       , cond_ZC
       , cond_NS
       , cond_NC
       , cond_CS
       , cond_CC
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype ALUCondition = ALUCondition { unALUCondition :: Word8 } deriving (Eq, Show, Typeable)

instance To64 ALUCondition where
  to64 = toEnum . fromEnum . unALUCondition

cond_NV :: ALUCondition
cond_NV = ALUCondition 0
cond_AL :: ALUCondition
cond_AL = ALUCondition 1
cond_ZS :: ALUCondition
cond_ZS = ALUCondition 2
cond_ZC :: ALUCondition
cond_ZC = ALUCondition 3
cond_NS :: ALUCondition
cond_NS = ALUCondition 4
cond_NC :: ALUCondition
cond_NC = ALUCondition 5
cond_CS :: ALUCondition
cond_CS = ALUCondition 6
cond_CC :: ALUCondition
cond_CC = ALUCondition 7
