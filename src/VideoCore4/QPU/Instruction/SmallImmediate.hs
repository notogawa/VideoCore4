module VideoCore4.QPU.Instruction.SmallImmediate
       (
         SmallImmediate
       , ToSmallImmediate
       , toSmallImmediate
       , small_immed_0
       , small_immed_1
       , small_immed_2
       , small_immed_3
       , small_immed_4
       , small_immed_5
       , small_immed_6
       , small_immed_7
       , small_immed_8
       , small_immed_9
       , small_immed_10
       , small_immed_11
       , small_immed_12
       , small_immed_13
       , small_immed_14
       , small_immed_15
       , small_immed_M16
       , small_immed_M15
       , small_immed_M14
       , small_immed_M13
       , small_immed_M12
       , small_immed_M11
       , small_immed_M10
       , small_immed_M9
       , small_immed_M8
       , small_immed_M7
       , small_immed_M6
       , small_immed_M5
       , small_immed_M4
       , small_immed_M3
       , small_immed_M2
       , small_immed_M1
       , small_immed_1F
       , small_immed_2F
       , small_immed_4F
       , small_immed_8F
       , small_immed_16F
       , small_immed_32F
       , small_immed_64F
       , small_immed_128F
       , small_immed_1_256F
       , small_immed_1_128F
       , small_immed_1_64F
       , small_immed_1_32F
       , small_immed_1_16F
       , small_immed_1_8F
       , small_immed_1_4F
       , small_immed_1_2F
       , small_immed_VRR5
       , small_immed_VR1
       , small_immed_VR2
       , small_immed_VR3
       , small_immed_VR4
       , small_immed_VR5
       , small_immed_VR6
       , small_immed_VR7
       , small_immed_VR8
       , small_immed_VR9
       , small_immed_VR10
       , small_immed_VR11
       , small_immed_VR12
       , small_immed_VR13
       , small_immed_VR14
       , small_immed_VR15
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

data SmallImmediate = SmallImmediate { unSmallImmediate :: Word8 } deriving (Eq, Show, Typeable)

instance To64 SmallImmediate where
  to64 = toEnum . fromEnum . unSmallImmediate

class ToSmallImmediate a where
  toSmallImmediate :: a -> SmallImmediate

instance ToSmallImmediate SmallImmediate where
  toSmallImmediate = id

instance ToSmallImmediate Int where
  toSmallImmediate 0 = small_immed_0
  toSmallImmediate 1 = small_immed_1
  toSmallImmediate 2 = small_immed_2
  toSmallImmediate 3 = small_immed_3
  toSmallImmediate 4 = small_immed_4
  toSmallImmediate 5 = small_immed_5
  toSmallImmediate 6 = small_immed_6
  toSmallImmediate 7 = small_immed_7
  toSmallImmediate 8 = small_immed_8
  toSmallImmediate 9 = small_immed_9
  toSmallImmediate 10 = small_immed_10
  toSmallImmediate 11 = small_immed_11
  toSmallImmediate 12 = small_immed_12
  toSmallImmediate 13 = small_immed_13
  toSmallImmediate 14 = small_immed_14
  toSmallImmediate 15 = small_immed_15
  toSmallImmediate (-16) = small_immed_M16
  toSmallImmediate (-15) = small_immed_M15
  toSmallImmediate (-14) = small_immed_M14
  toSmallImmediate (-13) = small_immed_M13
  toSmallImmediate (-12) = small_immed_M12
  toSmallImmediate (-11) = small_immed_M11
  toSmallImmediate (-10) = small_immed_M10
  toSmallImmediate (-9) = small_immed_M9
  toSmallImmediate (-8) = small_immed_M8
  toSmallImmediate (-7) = small_immed_M7
  toSmallImmediate (-6) = small_immed_M6
  toSmallImmediate (-5) = small_immed_M5
  toSmallImmediate (-4) = small_immed_M4
  toSmallImmediate (-3) = small_immed_M3
  toSmallImmediate (-2) = small_immed_M2
  toSmallImmediate (-1) = small_immed_M1
  toSmallImmediate _ = error "invalid small_immed"

small_immed_0 :: SmallImmediate
small_immed_0 = SmallImmediate 0
small_immed_1 :: SmallImmediate
small_immed_1 = SmallImmediate 1
small_immed_2 :: SmallImmediate
small_immed_2 = SmallImmediate 2
small_immed_3 :: SmallImmediate
small_immed_3 = SmallImmediate 3
small_immed_4 :: SmallImmediate
small_immed_4 = SmallImmediate 4
small_immed_5 :: SmallImmediate
small_immed_5 = SmallImmediate 5
small_immed_6 :: SmallImmediate
small_immed_6 = SmallImmediate 6
small_immed_7 :: SmallImmediate
small_immed_7 = SmallImmediate 7
small_immed_8 :: SmallImmediate
small_immed_8 = SmallImmediate 8
small_immed_9 :: SmallImmediate
small_immed_9 = SmallImmediate 9
small_immed_10 :: SmallImmediate
small_immed_10 = SmallImmediate 10
small_immed_11 :: SmallImmediate
small_immed_11 = SmallImmediate 11
small_immed_12 :: SmallImmediate
small_immed_12 = SmallImmediate 12
small_immed_13 :: SmallImmediate
small_immed_13 = SmallImmediate 13
small_immed_14 :: SmallImmediate
small_immed_14 = SmallImmediate 14
small_immed_15 :: SmallImmediate
small_immed_15 = SmallImmediate 15
small_immed_M16 :: SmallImmediate
small_immed_M16 = SmallImmediate 16
small_immed_M15 :: SmallImmediate
small_immed_M15 = SmallImmediate 17
small_immed_M14 :: SmallImmediate
small_immed_M14 = SmallImmediate 18
small_immed_M13 :: SmallImmediate
small_immed_M13 = SmallImmediate 19
small_immed_M12 :: SmallImmediate
small_immed_M12 = SmallImmediate 20
small_immed_M11 :: SmallImmediate
small_immed_M11 = SmallImmediate 21
small_immed_M10 :: SmallImmediate
small_immed_M10 = SmallImmediate 22
small_immed_M9 :: SmallImmediate
small_immed_M9 = SmallImmediate 23
small_immed_M8 :: SmallImmediate
small_immed_M8 = SmallImmediate 24
small_immed_M7 :: SmallImmediate
small_immed_M7 = SmallImmediate 25
small_immed_M6 :: SmallImmediate
small_immed_M6 = SmallImmediate 26
small_immed_M5 :: SmallImmediate
small_immed_M5 = SmallImmediate 27
small_immed_M4 :: SmallImmediate
small_immed_M4 = SmallImmediate 28
small_immed_M3 :: SmallImmediate
small_immed_M3 = SmallImmediate 29
small_immed_M2 :: SmallImmediate
small_immed_M2 = SmallImmediate 30
small_immed_M1 :: SmallImmediate
small_immed_M1 = SmallImmediate 31
small_immed_1F :: SmallImmediate
small_immed_1F = SmallImmediate 32
small_immed_2F :: SmallImmediate
small_immed_2F = SmallImmediate 33
small_immed_4F :: SmallImmediate
small_immed_4F = SmallImmediate 34
small_immed_8F :: SmallImmediate
small_immed_8F = SmallImmediate 35
small_immed_16F :: SmallImmediate
small_immed_16F = SmallImmediate 36
small_immed_32F :: SmallImmediate
small_immed_32F = SmallImmediate 37
small_immed_64F :: SmallImmediate
small_immed_64F = SmallImmediate 38
small_immed_128F :: SmallImmediate
small_immed_128F = SmallImmediate 39
small_immed_1_256F :: SmallImmediate
small_immed_1_256F = SmallImmediate 40
small_immed_1_128F :: SmallImmediate
small_immed_1_128F = SmallImmediate 41
small_immed_1_64F :: SmallImmediate
small_immed_1_64F = SmallImmediate 42
small_immed_1_32F :: SmallImmediate
small_immed_1_32F = SmallImmediate 43
small_immed_1_16F :: SmallImmediate
small_immed_1_16F = SmallImmediate 44
small_immed_1_8F :: SmallImmediate
small_immed_1_8F = SmallImmediate 45
small_immed_1_4F :: SmallImmediate
small_immed_1_4F = SmallImmediate 46
small_immed_1_2F :: SmallImmediate
small_immed_1_2F = SmallImmediate 47
small_immed_VRR5 :: SmallImmediate
small_immed_VRR5 = SmallImmediate 48
small_immed_VR1 :: SmallImmediate
small_immed_VR1 = SmallImmediate 49
small_immed_VR2 :: SmallImmediate
small_immed_VR2 = SmallImmediate 50
small_immed_VR3 :: SmallImmediate
small_immed_VR3 = SmallImmediate 51
small_immed_VR4 :: SmallImmediate
small_immed_VR4 = SmallImmediate 52
small_immed_VR5 :: SmallImmediate
small_immed_VR5 = SmallImmediate 53
small_immed_VR6 :: SmallImmediate
small_immed_VR6 = SmallImmediate 54
small_immed_VR7 :: SmallImmediate
small_immed_VR7 = SmallImmediate 55
small_immed_VR8 :: SmallImmediate
small_immed_VR8 = SmallImmediate 56
small_immed_VR9 :: SmallImmediate
small_immed_VR9 = SmallImmediate 57
small_immed_VR10 :: SmallImmediate
small_immed_VR10 = SmallImmediate 58
small_immed_VR11 :: SmallImmediate
small_immed_VR11 = SmallImmediate 59
small_immed_VR12 :: SmallImmediate
small_immed_VR12 = SmallImmediate 60
small_immed_VR13 :: SmallImmediate
small_immed_VR13 = SmallImmediate 61
small_immed_VR14 :: SmallImmediate
small_immed_VR14 = SmallImmediate 62
small_immed_VR15 :: SmallImmediate
small_immed_VR15 = SmallImmediate 63
