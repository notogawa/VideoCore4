{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VideoCore4.QPU.Assembler
       (
         Asm
       , asm

       , Inst

       , ALU
       , alu
       , ALUSmallImm
       , alui
       , Branch
       , br
       , LoadImm32
       , li32
       , LoadImmPerElmtSigned
       , lipes
       , LoadImmPerElmtUnsigned
       , lipeu
       , Semaphore
       , sem

       , sig
       , unpack
       , pm
       , pack
       , cond_add
       , cond_mul
       , sf
       , ws
       , waddr_add
       , waddr_mul
       , op_mul
       , op_add
       , raddr_a
       , raddr_b
       , add_a
       , add_b
       , mul_a
       , mul_b
       , small_immed
       , cond_br
       , rel
       , reg
       , immediate
       , ms
       , ls
       , sa
       , semaphore

       , module VideoCore4.QPU.Instruction.SignalingBits
       , module VideoCore4.QPU.Instruction.UnpackMode
       , module VideoCore4.QPU.Instruction.PM
       , module VideoCore4.QPU.Instruction.PackMode
       , module VideoCore4.QPU.Instruction.ALUCondition
       , module VideoCore4.QPU.Instruction.SetFlag
       , module VideoCore4.QPU.Instruction.WriteSwap
       , module VideoCore4.QPU.Instruction.Register
       , module VideoCore4.QPU.Instruction.OpMul
       , module VideoCore4.QPU.Instruction.OpAdd
       , module VideoCore4.QPU.Instruction.InputMux
       , module VideoCore4.QPU.Instruction.SmallImmediate
       , module VideoCore4.QPU.Instruction.BranchCondition
       , module VideoCore4.QPU.Instruction.BranchRelative
       , module VideoCore4.QPU.Instruction.RegAdd
       , module VideoCore4.QPU.Instruction.SemaphoreAccess
       , module VideoCore4.QPU.Instruction.SemaphoreId
       ) where

import Data.Int
import Data.Word
import Control.Applicative
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import VideoCore4.QPU.Instruction hiding (sig, unpack, pm, pack, cond_add, cond_mul, sf, ws, waddr_add, waddr_mul, op_mul, op_add, raddr_a, raddr_b, add_a, add_b, mul_a, mul_b, small_immed, cond_br, rel, reg, immediate, ms, ls, sa, semaphore)
import qualified VideoCore4.QPU.Instruction as I
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

newtype Asm a = Asm { unAsm :: Writer [Instruction] a } deriving (Functor, Applicative, Monad)

toAsm :: Instruction -> Asm ()
toAsm inst = Asm $ tell [inst]

asm :: Asm a -> (a, [Instruction])
asm = runWriter . unAsm

newtype Inst x a = Inst { unInst :: State Instruction a } deriving (Functor, Applicative, Monad)

data ALU

alu :: Inst ALU a -> Asm ()
alu = toAsm . flip execState defaultALUInstruction . unInst

data ALUSmallImm

alui :: Inst ALUSmallImm a -> Asm ()
alui = toAsm . flip execState defaultALUSmallImmInstruction . unInst

data Branch

br :: Inst Branch a -> Asm ()
br = toAsm . flip execState defaultBranchInstruction . unInst

data LoadImm32

li32 :: Inst LoadImm32 a -> Asm ()
li32 = toAsm . flip execState defaultLoadImm32Instruction . unInst

data LoadImmPerElmtSigned

lipes :: Inst LoadImmPerElmtSigned a -> Asm ()
lipes = toAsm . flip execState defaultLoadImmPerElmtSignedInstruction . unInst

data LoadImmPerElmtUnsigned

lipeu :: Inst LoadImmPerElmtUnsigned a -> Asm ()
lipeu = toAsm . flip execState defaultLoadImmPerElmtUnsignedInstruction . unInst

data Semaphore

sem :: Inst Semaphore a -> Asm ()
sem = toAsm . flip execState defaultSemaphoreInstruction . unInst

sig :: SignalingBits -> Inst ALU ()
sig x = Inst $ modify $ \inst -> inst { I.sig = x }

class Has_unpack a where
  unpack :: UnpackMode -> Inst a ()
  unpack x = Inst $ modify $ \inst -> inst { I.unpack = x }
instance Has_unpack ALU
instance Has_unpack ALUSmallImm

class Has_pm a where
  pm :: PM -> Inst a ()
  pm x = Inst $ modify $ \inst -> inst { I.pm = x }
instance Has_pm ALU
instance Has_pm ALUSmallImm
instance Has_pm LoadImm32
instance Has_pm LoadImmPerElmtSigned
instance Has_pm LoadImmPerElmtUnsigned
instance Has_pm Semaphore

class Has_pack a where
  pack :: PackMode -> Inst a ()
  pack x = Inst $ modify $ \inst -> inst { I.pack = x }
instance Has_pack ALU
instance Has_pack ALUSmallImm
instance Has_pack LoadImm32
instance Has_pack LoadImmPerElmtSigned
instance Has_pack LoadImmPerElmtUnsigned
instance Has_pack Semaphore

class Has_cond_add a where
  cond_add :: ALUCondition -> Inst a ()
  cond_add x = Inst $ modify $ \inst -> inst { I.cond_add = x }
instance Has_cond_add ALU
instance Has_cond_add ALUSmallImm
instance Has_cond_add LoadImm32
instance Has_cond_add LoadImmPerElmtSigned
instance Has_cond_add LoadImmPerElmtUnsigned
instance Has_cond_add Semaphore

class Has_cond_mul a where
  cond_mul :: ALUCondition -> Inst a ()
  cond_mul x = Inst $ modify $ \inst -> inst { I.cond_mul = x }
instance Has_cond_mul ALU
instance Has_cond_mul ALUSmallImm
instance Has_cond_mul LoadImm32
instance Has_cond_mul LoadImmPerElmtSigned
instance Has_cond_mul LoadImmPerElmtUnsigned
instance Has_cond_mul Semaphore

class Has_sf a where
  sf :: SetFlag -> Inst a ()
  sf x = Inst $ modify $ \inst -> inst { I.sf = x }
instance Has_sf ALU
instance Has_sf ALUSmallImm
instance Has_sf LoadImm32
instance Has_sf LoadImmPerElmtSigned
instance Has_sf LoadImmPerElmtUnsigned
instance Has_sf Semaphore

ws :: WriteSwap -> Inst a ()
ws x = Inst $ modify $ \inst -> inst { I.ws = x }

waddr_add :: Register -> Inst a ()
waddr_add x = Inst $ modify $ \inst -> inst { I.waddr_add = x }

waddr_mul :: Register -> Inst a ()
waddr_mul x = Inst $ modify $ \inst -> inst { I.waddr_mul = x }

class Has_op_mul a where
  op_mul :: OpMul -> Inst a ()
  op_mul x = Inst $ modify $ \inst -> inst { I.op_mul = x }
instance Has_op_mul ALU
instance Has_op_mul ALUSmallImm

class Has_op_add a where
  op_add :: OpAdd -> Inst a ()
  op_add x = Inst $ modify $ \inst -> inst { I.op_add = x }
instance Has_op_add ALU
instance Has_op_add ALUSmallImm

class Has_raddr_a a where
  raddr_a :: Register -> Inst a ()
  raddr_a x = Inst $ modify $ \inst -> inst { I.raddr_a = x }
instance Has_raddr_a ALU
instance Has_raddr_a ALUSmallImm
instance Has_raddr_a Branch

raddr_b :: Register -> Inst ALU ()
raddr_b x = Inst $ modify $ \inst -> inst { I.raddr_b = x }

class Has_add_a a where
  add_a :: InputMux -> Inst a ()
  add_a x = Inst $ modify $ \inst -> inst { I.add_a = x }
instance Has_add_a ALU
instance Has_add_a ALUSmallImm

class Has_add_b a where
  add_b :: InputMux -> Inst a ()
  add_b x = Inst $ modify $ \inst -> inst { I.add_b = x }
instance Has_add_b ALU
instance Has_add_b ALUSmallImm

class Has_mul_a a where
  mul_a :: InputMux -> Inst a ()
  mul_a x = Inst $ modify $ \inst -> inst { I.mul_a = x }
instance Has_mul_a ALU
instance Has_mul_a ALUSmallImm

class Has_mul_b a where
  mul_b :: InputMux -> Inst a ()
  mul_b x = Inst $ modify $ \inst -> inst { I.mul_b = x }
instance Has_mul_b ALU
instance Has_mul_b ALUSmallImm

small_immed :: SmallImmediate -> Inst ALUSmallImm ()
small_immed x = Inst $ modify $ \inst -> inst { I.small_immed = x }

cond_br :: BranchCondition -> Inst Branch ()
cond_br x = Inst $ modify $ \inst -> inst { I.cond_br = x }

rel :: BranchRelative -> Inst Branch ()
rel x = Inst $ modify $ \inst -> inst { I.rel = x }

reg :: RegAdd -> Inst Branch ()
reg x = Inst $ modify $ \inst -> inst { I.reg = x }

class Has_immediate a where
  immediate :: Int32 -> Inst a ()
  immediate x = Inst $ modify $ \inst -> inst { I.immediate = x }
instance Has_immediate Branch
instance Has_immediate LoadImm32

class Has_ms a where
  ms :: Word16 -> Inst a ()
  ms x = Inst $ modify $ \inst -> inst { I.ms = x }
instance Has_ms LoadImmPerElmtSigned
instance Has_ms LoadImmPerElmtUnsigned

class Has_ls a where
  ls :: Word16 -> Inst a ()
  ls x = Inst $ modify $ \inst -> inst { I.ls = x }
instance Has_ls LoadImmPerElmtSigned
instance Has_ls LoadImmPerElmtUnsigned

sa :: SemaphoreAccess -> Inst Semaphore ()
sa x = Inst $ modify $ \inst -> inst { I.sa = x }

semaphore :: SemaphoreId -> Inst Semaphore ()
semaphore x = Inst $ modify $ \inst -> inst { I.semaphore = x }
