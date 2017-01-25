{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VideoCore4.QPU.Assembler
       (
         Asm
       , asm
       , label
       , toLabel

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

       , setupVCDDMAStore
       , setupVCDDMALoad
       , setupVCDDMALoadStride

       , Mux(..)
       , (#)
       , with

       , fadd
       , fsub
       , fmin
       , fmax
       , fminabs
       , fmaxabs
       , ftoi
       , itof
       , iadd
       , isub
       , shr
       , asr
       , ror
       , shl
       , imin
       , imax
       , band
       , bor
       , bxor
       , bnot
       , clz
       , v8adds
       , v8subs

       , fmul
       , mul24
       , v8muld
       , v8min
       , v8max
       , v8adds'
       , v8subs'

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

import Data.Bits
import Data.Int
import Data.List
import Data.Typeable
import Data.Word
import Control.Applicative
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

data AsmState =
  AsmState
  { asmStateInstruction :: [Instruction]
  , asmStateLabelPositions :: [(String, Int32)]
  , asmStateCurrentPosition :: Int32
  } deriving (Eq, Show, Typeable)

defaultAsmState :: AsmState
defaultAsmState = AsmState { asmStateInstruction = []
                           , asmStateLabelPositions = []
                           , asmStateCurrentPosition = 0
                           }

newtype Asm a = Asm { unAsm :: State AsmState a } deriving (Functor, Applicative, Monad)

toAsm :: Instruction -> Asm ()
toAsm inst = Asm $ modify' updateState where
  updateState s = s { asmStateInstruction = asmStateInstruction s ++ [inst]
                    , asmStateCurrentPosition = 8 + asmStateCurrentPosition s
                    }

asm :: Asm a -> [Instruction]
asm = asmStateInstruction . flip execState defaultAsmState. unAsm

label :: String -> Asm ()
label name = Asm $ modify' updateState where
  updateState s = s { asmStateLabelPositions = (name, asmStateCurrentPosition s) : asmStateLabelPositions s }

toLabel :: String -> Asm (Maybe Int32)
toLabel name = Asm $ do
  s <- get
  case lookup name $ asmStateLabelPositions s of
    Nothing -> return Nothing
    Just p  -> return $ Just (p - asmStateCurrentPosition s - 32)

newtype Inst tag a = Inst { unInst :: State Instruction a } deriving (Functor, Applicative, Monad)

data ALU
data ALUSmallImm
data Branch
data LoadImm32
data LoadImmPerElmtSigned
data LoadImmPerElmtUnsigned
data Semaphore

class IsInst inst where
  alu :: inst ALU a -> Asm ()
  alui :: inst ALUSmallImm a -> Asm ()
  br :: inst Branch a -> Asm ()
  li32 :: inst LoadImm32 a -> Asm ()
  lipes :: inst LoadImmPerElmtSigned a -> Asm ()
  lipeu :: inst LoadImmPerElmtUnsigned a -> Asm ()
  sem :: inst Semaphore a -> Asm ()
  with :: inst tag () -> Inst tag () -> Inst tag ()


instance IsInst Inst where
  alu = toAsm . flip execState defaultALUInstruction . unInst
  alui = toAsm . flip execState defaultALUSmallImmInstruction . unInst
  br = toAsm . flip execState defaultBranchInstruction . unInst
  li32 = toAsm . flip execState defaultLoadImm32Instruction . unInst
  lipes = toAsm . flip execState defaultLoadImmPerElmtSignedInstruction . unInst
  lipeu = toAsm . flip execState defaultLoadImmPerElmtUnsignedInstruction . unInst
  sem = toAsm . flip execState defaultSemaphoreInstruction . unInst
  with = (>>)

instance IsInst (InstStmt a) where
  alu = alu . unInstStmt
  alui = alui . unInstStmt
  br = br . unInstStmt
  li32 = li32 . unInstStmt
  lipes = lipes . unInstStmt
  lipeu = lipeu . unInstStmt
  sem = sem . unInstStmt
  inst `with` additional = unInstStmt inst `with` additional

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


small_immed :: ToSmallImmediate a => a -> Inst ALUSmallImm ()
small_immed x = Inst $ modify $ \inst -> inst { I.small_immed = toSmallImmediate x }

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

setupVCDDMAStore :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Asm ()
setupVCDDMAStore units depth laned horiz baseY baseX modeW = li32 $ do
  cond_mul cond_AL
  waddr_mul vpmvcd_wr_setup
  immediate $ foldl' (.|.) 0 [ 0x80000000
                             , units `shiftL` 23
                             , depth `shiftL` 16
                             , laned `shiftL` 15
                             , horiz `shiftL` 14
                             , baseY `shiftL` 7
                             , baseX `shiftL` 3
                             , modeW
                             ]

setupVCDDMALoad :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Asm ()
setupVCDDMALoad modeW mPitch rowLen nRows vPitch vert addrXY = li32 $ do
  cond_add cond_AL
  waddr_add vpmvcd_rd_setup
  immediate $ foldl' (.|.) 0 [ 0x80000000
                             , modeW `shiftL` 28
                             , mPitch `shiftL` 24
                             , rowLen `shiftL` 20
                             , nRows `shiftL` 16
                             , vPitch `shiftL` 12
                             , vert `shiftL` 11
                             , addrXY
                             ]

class SetupVCDDMALoadStride a where
  setupVCDDMALoadStride :: a -> Asm ()

instance SetupVCDDMALoadStride Int32 where
  setupVCDDMALoadStride mPitchB = li32 $ do
    cond_add cond_AL
    waddr_add vpmvcd_rd_setup
    immediate $ 0x80000000 .|. (1 `shiftL` 28) .|. mPitchB

instance SetupVCDDMALoadStride Register where
  setupVCDDMALoadStride mPitchB = do
    li32 $ do
      cond_add cond_AL
      waddr_add r0
      immediate 0x90000000
    -- TODO: impl
    alu $ bor vpmvcd_rd_setup R0 R1

data Mux = R0 | R1 | R2 | R3 | R4 | R5 | RA | RB deriving (Eq, Show, Typeable)

toInputMux :: Mux -> InputMux
toInputMux R0 = mux_R0
toInputMux R1 = mux_R1
toInputMux R2 = mux_R2
toInputMux R3 = mux_R3
toInputMux R4 = mux_R4
toInputMux R5 = mux_R5
toInputMux RA = mux_RA
toInputMux RB = mux_RB

data InstStmt part tag a =
  InstStmt
  { instStmtInst :: Inst tag a
  , instStmtSkip :: Bool
  , instStmtSwap :: Bool
  }

addInst :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => OpAdd -> Register -> Mux -> Mux -> InstStmt OpAdd tag ()
addInst op r a b = InstStmt { instStmtInst = do
                                 cond_add cond_AL
                                 op_add op
                                 waddr_add r
                                 add_a $ toInputMux a
                                 add_b $ toInputMux b
                            , instStmtSkip = AW `elem` rwab r
                            , instStmtSwap = BW `elem` rwab r
                            }

fadd :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
fadd = addInst op_add_FADD

fsub :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
fsub = addInst op_add_FSUB

fmin :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
fmin = addInst op_add_FMIN

fmax :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
fmax = addInst op_add_FMAX

fminabs :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
fminabs = addInst op_add_FMINABS

fmaxabs :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
fmaxabs = addInst op_add_FMAXABS

ftoi :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
ftoi = addInst op_add_FTOI

itof :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
itof = addInst op_add_ITOF

iadd :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
iadd = addInst op_add_ADD

isub :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
isub = addInst op_add_SUB

shr :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
shr = addInst op_add_SHR

asr :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
asr = addInst op_add_ASR

ror :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
ror = addInst op_add_ROR

shl :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
shl = addInst op_add_SHL

imin :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
imin = addInst op_add_MIN

imax :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
imax = addInst op_add_MAX

band :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
band = addInst op_add_AND

bor :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
bor = addInst op_add_OR

bxor :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
bxor = addInst op_add_XOR

bnot :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
bnot = addInst op_add_NOT

clz :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
clz = addInst op_add_CLZ

v8adds :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
v8adds = addInst op_add_V8ADDS

v8subs :: (Has_cond_add tag, Has_op_add tag, Has_add_a tag, Has_add_b tag) => Register -> Mux -> Mux -> InstStmt OpAdd tag ()
v8subs = addInst op_add_V8SUBS

mulInst :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => OpMul -> Register -> Mux -> Mux -> InstStmt OpMul tag ()
mulInst op r a b = InstStmt { instStmtInst = do
                                 cond_mul cond_AL
                                 op_mul op
                                 waddr_mul r
                                 mul_a $ toInputMux a
                                 mul_b $ toInputMux b
                            , instStmtSkip = BW `elem` rwab r
                            , instStmtSwap = AW `elem` rwab r
                            }

fmul :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => Register -> Mux -> Mux -> InstStmt OpMul tag ()
fmul = mulInst op_mul_FMUL

mul24 :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => Register -> Mux -> Mux -> InstStmt OpMul tag ()
mul24 = mulInst op_mul_MUL24

v8muld :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => Register -> Mux -> Mux -> InstStmt OpMul tag ()
v8muld = mulInst op_mul_V8MULD

v8min :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => Register -> Mux -> Mux -> InstStmt OpMul tag ()
v8min = mulInst op_mul_V8MIN

v8max :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => Register -> Mux -> Mux -> InstStmt OpMul tag ()
v8max = mulInst op_mul_V8MAX

v8adds' :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => Register -> Mux -> Mux -> InstStmt OpMul tag ()
v8adds' = mulInst op_mul_V8ADDS

v8subs' :: (Has_cond_mul tag, Has_op_mul tag, Has_mul_a tag, Has_mul_b tag) => Register -> Mux -> Mux -> InstStmt OpMul tag ()
v8subs' = mulInst op_mul_V8SUBS

(#) :: InstStmt OpAdd tag () -> InstStmt OpMul tag () -> Inst tag ()
opAdd # opMul = do
  instStmtInst opAdd
  instStmtInst opMul
  let skip = instStmtSkip opAdd && instStmtSkip opMul
  let swap = instStmtSwap opAdd && instStmtSwap opMul
  case (skip, swap) of
    (True ,     _) -> return ()
    (False,  True) -> ws ws_On
    (False, False) -> error "invalid register"

unInstStmt :: InstStmt part tag a -> Inst tag ()
unInstStmt op = do
  _ <- instStmtInst op
  let skip = instStmtSkip op
  let swap = instStmtSwap op
  case (skip, swap) of
    (True ,     _) -> return ()
    (False,  True) -> ws ws_On
    (False, False) -> error "invalid register"
