module VideoCore4.QPU.Instruction.BranchCondition
       (
         BranchCondition
       , cond_br_JZS
       , cond_br_JZC
       , cond_br_JZS_ANY
       , cond_br_JZC_ANY
       , cond_br_JNS
       , cond_br_JNC
       , cond_br_JNS_ANY
       , cond_br_JNC_ANY
       , cond_br_JCS
       , cond_br_JCC
       , cond_br_JCS_ANY
       , cond_br_JCC_ANY
       , cond_br_JMP
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype BranchCondition = BranchCondition { unBranchCondition :: Word8 } deriving (Eq, Show, Typeable)

instance To64 BranchCondition where
  to64 = toEnum . fromEnum . unBranchCondition

cond_br_JZS :: BranchCondition
cond_br_JZS = BranchCondition 0
cond_br_JZC :: BranchCondition
cond_br_JZC = BranchCondition 1
cond_br_JZS_ANY :: BranchCondition
cond_br_JZS_ANY = BranchCondition 2
cond_br_JZC_ANY :: BranchCondition
cond_br_JZC_ANY = BranchCondition 3
cond_br_JNS :: BranchCondition
cond_br_JNS = BranchCondition 4
cond_br_JNC :: BranchCondition
cond_br_JNC = BranchCondition 5
cond_br_JNS_ANY :: BranchCondition
cond_br_JNS_ANY = BranchCondition 6
cond_br_JNC_ANY :: BranchCondition
cond_br_JNC_ANY = BranchCondition 7
cond_br_JCS :: BranchCondition
cond_br_JCS = BranchCondition 8
cond_br_JCC :: BranchCondition
cond_br_JCC = BranchCondition 9
cond_br_JCS_ANY :: BranchCondition
cond_br_JCS_ANY = BranchCondition 10
cond_br_JCC_ANY :: BranchCondition
cond_br_JCC_ANY = BranchCondition 11
cond_br_JMP :: BranchCondition
cond_br_JMP = BranchCondition 15
