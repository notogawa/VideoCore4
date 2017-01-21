{-# LANGUAGE RecordWildCards #-}
module VideoCore4.QPU.Instruction
       (
         Instruction (..)
       , defaultALUInstruction
       , defaultALUSmallImmInstruction
       , defaultBranchInstruction
       , defaultLoadImm32Instruction
       , defaultLoadImmPerElmtSignedInstruction
       , defaultLoadImmPerElmtUnsignedInstruction
       , defaultSemaphoreInstruction
       , encode
       ) where

import Data.Bits
import Data.Int
import Data.List
import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.SignalingBits
import VideoCore4.QPU.Instruction.UnpackMode
import VideoCore4.QPU.Instruction.PM
import VideoCore4.QPU.Instruction.PackMode
import VideoCore4.QPU.Instruction.ALUCondition
import VideoCore4.QPU.Instruction.SetFlag
import VideoCore4.QPU.Instruction.WriteSwap
import VideoCore4.QPU.Instruction.Register
import VideoCore4.QPU.Instruction.OpMul
import VideoCore4.QPU.Instruction.OpAdd
import VideoCore4.QPU.Instruction.InputMux
import VideoCore4.QPU.Instruction.SmallImmediate
import VideoCore4.QPU.Instruction.BranchCondition
import VideoCore4.QPU.Instruction.BranchRelative
import VideoCore4.QPU.Instruction.RegAdd
import VideoCore4.QPU.Instruction.SemaphoreAccess
import VideoCore4.QPU.Instruction.SemaphoreId
import VideoCore4.QPU.Instruction.Types

-- QPU Instruction Encoding
data Instruction = ALUInstruction
                   { sig :: SignalingBits
                   , unpack :: UnpackMode
                   , pm :: PM
                   , pack :: PackMode
                   , cond_add :: ALUCondition
                   , cond_mul :: ALUCondition
                   , sf :: SetFlag
                   , ws :: WriteSwap
                   , waddr_add :: Register
                   , waddr_mul :: Register
                   , op_mul :: OpMul
                   , op_add :: OpAdd
                   , raddr_a :: Register
                   , raddr_b :: Register
                   , add_a :: InputMux
                   , add_b :: InputMux
                   , mul_a :: InputMux
                   , mul_b :: InputMux
                   }
                 | ALUSmallImmInstruction
                   { unpack :: UnpackMode
                   , pm :: PM
                   , pack :: PackMode
                   , cond_add :: ALUCondition
                   , cond_mul :: ALUCondition
                   , sf :: SetFlag
                   , ws :: WriteSwap
                   , waddr_add :: Register
                   , waddr_mul :: Register
                   , op_mul :: OpMul
                   , op_add :: OpAdd
                   , raddr_a :: Register
                   , small_immed :: SmallImmediate
                   , add_a :: InputMux
                   , add_b :: InputMux
                   , mul_a :: InputMux
                   , mul_b :: InputMux
                   }
                 | BranchInstruction
                   { cond_br :: BranchCondition
                   , rel :: BranchRelative
                   , reg :: RegAdd
                   , raddr_a :: Register
                   , ws :: WriteSwap
                   , waddr_add :: Register
                   , waddr_mul :: Register
                   , immediate :: Int32
                   }
                 | LoadImm32Instruction
                   { pm :: PM
                   , pack :: PackMode
                   , cond_add :: ALUCondition
                   , cond_mul :: ALUCondition
                   , sf :: SetFlag
                   , ws :: WriteSwap
                   , waddr_add :: Register
                   , waddr_mul :: Register
                   , immediate :: Int32
                   }
                 | LoadImmPerElmtSignedInstruction
                   { pm :: PM
                   , pack :: PackMode
                   , cond_add :: ALUCondition
                   , cond_mul :: ALUCondition
                   , sf :: SetFlag
                   , ws :: WriteSwap
                   , waddr_add :: Register
                   , waddr_mul :: Register
                   , ms :: Word16
                   , ls :: Word16
                   }
                 | LoadImmPerElmtUnsignedInstruction
                   { pm :: PM
                   , pack :: PackMode
                   , cond_add :: ALUCondition
                   , cond_mul :: ALUCondition
                   , sf :: SetFlag
                   , ws :: WriteSwap
                   , waddr_add :: Register
                   , waddr_mul :: Register
                   , ms :: Word16
                   , ls :: Word16
                   }
                 | SemaphoreInstruction
                   { pm :: PM
                   , pack :: PackMode
                   , cond_add :: ALUCondition
                   , cond_mul :: ALUCondition
                   , sf :: SetFlag
                   , ws :: WriteSwap
                   , waddr_add :: Register
                   , waddr_mul :: Register
                   , sa :: SemaphoreAccess
                   , semaphore :: SemaphoreId
                   }
                 deriving (Eq, Show, Typeable)


defaultALUInstruction :: Instruction
defaultALUInstruction = ALUInstruction { sig = sig_NS
                                       , unpack = unpack_32_32
                                       , pm = pm_Regfile
                                       , pack = pack_32_32
                                       , cond_add = cond_NV
                                       , cond_mul = cond_NV
                                       , sf = sf_Off
                                       , ws = ws_Off
                                       , waddr_add = nop
                                       , waddr_mul = nop
                                       , op_mul = op_mul_NOP
                                       , op_add = op_add_NOP
                                       , raddr_a = nop
                                       , raddr_b = nop
                                       , add_a = mux_R0
                                       , add_b = mux_R0
                                       , mul_a = mux_R0
                                       , mul_b = mux_R0
                                       }

defaultALUSmallImmInstruction :: Instruction
defaultALUSmallImmInstruction = ALUSmallImmInstruction { unpack = unpack_32_32
                                                       , pm = pm_Regfile
                                                       , pack = pack_32_32
                                                       , cond_add = cond_NV
                                                       , cond_mul = cond_NV
                                                       , sf = sf_Off
                                                       , ws = ws_Off
                                                       , waddr_add = nop
                                                       , waddr_mul = nop
                                                       , op_mul = op_mul_NOP
                                                       , op_add = op_add_NOP
                                                       , raddr_a = nop
                                                       , small_immed = small_immed_0
                                                       , add_a = mux_R0
                                                       , add_b = mux_R0
                                                       , mul_a = mux_R0
                                                       , mul_b = mux_R0
                                                       }

defaultBranchInstruction :: Instruction
defaultBranchInstruction = BranchInstruction { cond_br = cond_br_JZS
                                             , rel = rel_Off
                                             , reg = reg_Off
                                             , raddr_a = ra0
                                             , ws = ws_Off
                                             , waddr_add = nop
                                             , waddr_mul = nop
                                             , immediate = 0
                                             }

defaultLoadImm32Instruction :: Instruction
defaultLoadImm32Instruction = LoadImm32Instruction { pm = pm_Regfile
                                                   , pack = pack_32_32
                                                   , cond_add = cond_NV
                                                   , cond_mul = cond_NV
                                                   , sf = sf_Off
                                                   , ws = ws_Off
                                                   , waddr_add = nop
                                                   , waddr_mul = nop
                                                   , immediate = 0
                                                   }

defaultLoadImmPerElmtSignedInstruction :: Instruction
defaultLoadImmPerElmtSignedInstruction = LoadImmPerElmtSignedInstruction { pm = pm_Regfile
                                                                         , pack = pack_32_32
                                                                         , cond_add = cond_NV
                                                                         , cond_mul = cond_NV
                                                                         , sf = sf_Off
                                                                         , ws = ws_Off
                                                                         , waddr_add = nop
                                                                         , waddr_mul = nop
                                                                         , ms = 0
                                                                         , ls = 0
                                                                         }

defaultLoadImmPerElmtUnsignedInstruction :: Instruction
defaultLoadImmPerElmtUnsignedInstruction = LoadImmPerElmtUnsignedInstruction { pm = pm_Regfile
                                                                             , pack = pack_32_32
                                                                             , cond_add = cond_NV
                                                                             , cond_mul = cond_NV
                                                                             , sf = sf_Off
                                                                             , ws = ws_Off
                                                                             , waddr_add = nop
                                                                             , waddr_mul = nop
                                                                             , ms = 0
                                                                             , ls = 0
                                                                             }

defaultSemaphoreInstruction :: Instruction
defaultSemaphoreInstruction = SemaphoreInstruction { pm = pm_Regfile
                                                   , pack = pack_32_32
                                                   , cond_add = cond_NV
                                                   , cond_mul = cond_NV
                                                   , sf = sf_Off
                                                   , ws = ws_Off
                                                   , waddr_add = nop
                                                   , waddr_mul = nop
                                                   , sa = sa_Inc
                                                   , semaphore = semaphore_0
                                                   }

encode :: Instruction -> Word64
encode = foldl' (.|.) 0 . go where
  go ALUInstruction {..} =
    [ to64 sig         `shiftL` 60
    , to64 unpack      `shiftL` 57
    , to64 pm          `shiftL` 56
    , to64 pack        `shiftL` 52
    , to64 cond_add    `shiftL` 49
    , to64 cond_mul    `shiftL` 46
    , to64 sf          `shiftL` 45
    , to64 ws          `shiftL` 44
    , to64 waddr_add   `shiftL` 38
    , to64 waddr_mul   `shiftL` 32
    , to64 op_mul      `shiftL` 29
    , to64 op_add      `shiftL` 24
    , to64 raddr_a     `shiftL` 18
    , to64 raddr_b     `shiftL` 12
    , to64 add_a       `shiftL`  9
    , to64 add_b       `shiftL`  6
    , to64 mul_a       `shiftL`  3
    , to64 mul_b       `shiftL`  0
    ]
  go ALUSmallImmInstruction {..} =
    [ 13               `shiftL` 60
    , to64 unpack      `shiftL` 57
    , to64 pm          `shiftL` 56
    , to64 pack        `shiftL` 52
    , to64 cond_add    `shiftL` 49
    , to64 cond_mul    `shiftL` 46
    , to64 sf          `shiftL` 45
    , to64 ws          `shiftL` 44
    , to64 waddr_add   `shiftL` 38
    , to64 waddr_mul   `shiftL` 32
    , to64 op_mul      `shiftL` 29
    , to64 op_add      `shiftL` 24
    , to64 raddr_a     `shiftL` 18
    , to64 small_immed `shiftL` 12
    , to64 add_a       `shiftL`  9
    , to64 add_b       `shiftL`  6
    , to64 mul_a       `shiftL`  3
    , to64 mul_b       `shiftL`  0
    ]
  go BranchInstruction {..} =
    [ 15               `shiftL` 60
    , to64 cond_br     `shiftL` 52
    , to64 rel         `shiftL` 51
    , to64 reg         `shiftL` 50
    , to64 raddr_a     `shiftL` 45
    , to64 ws          `shiftL` 44
    , to64 waddr_add   `shiftL` 38
    , to64 waddr_mul   `shiftL` 32
    , to64 immediate   `shiftL`  0
    ]
  go LoadImm32Instruction {..} =
    [ 7                `shiftL` 61
    , 0                `shiftL` 57
    , to64 pm          `shiftL` 56
    , to64 pack        `shiftL` 52
    , to64 cond_add    `shiftL` 49
    , to64 cond_mul    `shiftL` 46
    , to64 sf          `shiftL` 45
    , to64 ws          `shiftL` 44
    , to64 waddr_add   `shiftL` 38
    , to64 waddr_mul   `shiftL` 32
    , to64 immediate   `shiftL`  0
    ]
  go LoadImmPerElmtSignedInstruction {..} =
    [ 7                `shiftL` 61
    , 1                `shiftL` 57
    , to64 pm          `shiftL` 56
    , to64 pack        `shiftL` 52
    , to64 cond_add    `shiftL` 49
    , to64 cond_mul    `shiftL` 46
    , to64 sf          `shiftL` 45
    , to64 ws          `shiftL` 44
    , to64 waddr_add   `shiftL` 38
    , to64 waddr_mul   `shiftL` 32
    , to64 ms          `shiftL` 16
    , to64 ls          `shiftL`  0
    ]
  go LoadImmPerElmtUnsignedInstruction {..} =
    [ 7                `shiftL` 61
    , 3                `shiftL` 57
    , to64 pm          `shiftL` 56
    , to64 pack        `shiftL` 52
    , to64 cond_add    `shiftL` 49
    , to64 cond_mul    `shiftL` 46
    , to64 sf          `shiftL` 45
    , to64 ws          `shiftL` 44
    , to64 waddr_add   `shiftL` 38
    , to64 waddr_mul   `shiftL` 32
    , to64 ms          `shiftL` 16
    , to64 ls          `shiftL`  0
    ]
  go SemaphoreInstruction {..} =
    [ 7                `shiftL` 61
    , 4                `shiftL` 57
    , to64 pm          `shiftL` 56
    , to64 pack        `shiftL` 52
    , to64 cond_add    `shiftL` 49
    , to64 cond_mul    `shiftL` 46
    , to64 sf          `shiftL` 45
    , to64 ws          `shiftL` 44
    , to64 waddr_add   `shiftL` 38
    , to64 waddr_mul   `shiftL` 32
    , to64 sa          `shiftL`  4
    , to64 semaphore   `shiftL`  0
    ]
