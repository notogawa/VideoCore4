module VideoCore4.QPU.Instruction.BranchRelative
       (
         BranchRelative
       , rel_Off
       , rel_On
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype BranchRelative = BranchRelative { unBranchRelative :: Word8 } deriving (Eq, Show, Typeable)

instance To64 BranchRelative where
  to64 = toEnum . fromEnum . unBranchRelative

rel_Off :: BranchRelative
rel_Off = BranchRelative 0
rel_On :: BranchRelative
rel_On = BranchRelative 1
