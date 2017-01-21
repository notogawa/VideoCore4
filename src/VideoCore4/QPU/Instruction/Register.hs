module VideoCore4.QPU.Instruction.Register
       (
         Register
       , ra0
       , rb0
       , ra1
       , rb1
       , ra2
       , rb2
       , ra3
       , rb3
       , ra4
       , rb4
       , ra5
       , rb5
       , ra6
       , rb6
       , ra7
       , rb7
       , ra8
       , rb8
       , ra9
       , rb9
       , ra10
       , rb10
       , ra11
       , rb11
       , ra12
       , rb12
       , ra13
       , rb13
       , ra14
       , rb14
       , ra15
       , rb15
       , ra16
       , rb16
       , ra17
       , rb17
       , ra18
       , rb18
       , ra19
       , rb19
       , ra20
       , rb20
       , ra21
       , rb21
       , ra22
       , rb22
       , ra23
       , rb23
       , ra24
       , rb24
       , ra25
       , rb25
       , ra26
       , rb26
       , ra27
       , rb27
       , ra28
       , rb28
       , ra29
       , rb29
       , ra30
       , rb30
       , ra31
       , rb31
       , uniform_read
       , r0
       , r1
       , r2
       , r3
       , tmu_noswap
       , r5
       , element_number
       , qpu_number
       , host_int
       , nop
       , uniforms_address
       , vpm_read
       , vpm_write
       , vpm_ld_busy
       , vpm_st_busy
       , vpmvcd_rd_setup
       , vpmvcd_wr_setup
       , vpm_ld_wait
       , vpm_st_wait
       , vpm_ld_addr
       , vpm_st_addr
       , mutex_acquire
       , mutex_release
       , sfu_recip
       , sfu_recipsqrt
       , sfu_exp
       , sfu_log
       , tmu0s
       , tmu0t
       , tmu0r
       , tmu0b
       , tmu1s
       , tmu1t
       , tmu1r
       , tmu1b
       ) where

import Data.Bits
import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype Register = Register { unRegister :: Word16 } deriving (Eq, Show, Typeable)

instance To64 Register where
  to64 = toEnum . fromEnum . (.&. 0x2F) . unRegister

rwab_R :: Word16
rwab_R = 2 ^ (6 :: Int)
rwab_W :: Word16
rwab_W = 2 ^ (7 :: Int)
rwab_A :: Word16
rwab_A = 2 ^ (8 :: Int)
rwab_B :: Word16
rwab_B = 2 ^ (9 :: Int)
rwab_RW :: Word16
rwab_RW = rwab_R .|. rwab_W
rwab_RA :: Word16
rwab_RA = rwab_R .|. rwab_A
rwab_RB :: Word16
rwab_RB = rwab_R .|. rwab_B
rwab_WA :: Word16
rwab_WA = rwab_W .|. rwab_A
rwab_WB :: Word16
rwab_WB = rwab_W .|. rwab_B
rwab_AB :: Word16
rwab_AB = rwab_A .|. rwab_B
rwab_RWA :: Word16
rwab_RWA = rwab_RW .|. rwab_A
rwab_RWB :: Word16
rwab_RWB = rwab_RW .|. rwab_B
rwab_RAB :: Word16
rwab_RAB = rwab_R .|. rwab_AB
rwab_WAB :: Word16
rwab_WAB = rwab_W .|. rwab_AB
rwab_RWAB :: Word16
rwab_RWAB = rwab_RW .|. rwab_AB

ra0 :: Register
ra0 = Register $ 0 .|. rwab_RWA
rb0 :: Register
rb0 = Register $ 0 .|. rwab_RWB
ra1 :: Register
ra1 = Register $ 1 .|. rwab_RWA
rb1 :: Register
rb1 = Register $ 1 .|. rwab_RWB
ra2 :: Register
ra2 = Register $ 2 .|. rwab_RWA
rb2 :: Register
rb2 = Register $ 2 .|. rwab_RWB
ra3 :: Register
ra3 = Register $ 3 .|. rwab_RWA
rb3 :: Register
rb3 = Register $ 3 .|. rwab_RWB
ra4 :: Register
ra4 = Register $ 4 .|. rwab_RWA
rb4 :: Register
rb4 = Register $ 4 .|. rwab_RWB
ra5 :: Register
ra5 = Register $ 5 .|. rwab_RWA
rb5 :: Register
rb5 = Register $ 5 .|. rwab_RWB
ra6 :: Register
ra6 = Register $ 6 .|. rwab_RWA
rb6 :: Register
rb6 = Register $ 6 .|. rwab_RWB
ra7 :: Register
ra7 = Register $ 7 .|. rwab_RWA
rb7 :: Register
rb7 = Register $ 7 .|. rwab_RWB
ra8 :: Register
ra8 = Register $ 8 .|. rwab_RWA
rb8 :: Register
rb8 = Register $ 8 .|. rwab_RWB
ra9 :: Register
ra9 = Register $ 9 .|. rwab_RWA
rb9 :: Register
rb9 = Register $ 9 .|. rwab_RWB
ra10 :: Register
ra10 = Register $ 10 .|. rwab_RWA
rb10 :: Register
rb10 = Register $ 10 .|. rwab_RWB
ra11 :: Register
ra11 = Register $ 11 .|. rwab_RWA
rb11 :: Register
rb11 = Register $ 11 .|. rwab_RWB
ra12 :: Register
ra12 = Register $ 12 .|. rwab_RWA
rb12 :: Register
rb12 = Register $ 12 .|. rwab_RWB
ra13 :: Register
ra13 = Register $ 13 .|. rwab_RWA
rb13 :: Register
rb13 = Register $ 13 .|. rwab_RWB
ra14 :: Register
ra14 = Register $ 14 .|. rwab_RWA
rb14 :: Register
rb14 = Register $ 14 .|. rwab_RWB
ra15 :: Register
ra15 = Register $ 15 .|. rwab_RWA
rb15 :: Register
rb15 = Register $ 15 .|. rwab_RWB
ra16 :: Register
ra16 = Register $ 16 .|. rwab_RWA
rb16 :: Register
rb16 = Register $ 16 .|. rwab_RWB
ra17 :: Register
ra17 = Register $ 17 .|. rwab_RWA
rb17 :: Register
rb17 = Register $ 17 .|. rwab_RWB
ra18 :: Register
ra18 = Register $ 18 .|. rwab_RWA
rb18 :: Register
rb18 = Register $ 18 .|. rwab_RWB
ra19 :: Register
ra19 = Register $ 19 .|. rwab_RWA
rb19 :: Register
rb19 = Register $ 19 .|. rwab_RWB
ra20 :: Register
ra20 = Register $ 20 .|. rwab_RWA
rb20 :: Register
rb20 = Register $ 20 .|. rwab_RWB
ra21 :: Register
ra21 = Register $ 21 .|. rwab_RWA
rb21 :: Register
rb21 = Register $ 21 .|. rwab_RWB
ra22 :: Register
ra22 = Register $ 22 .|. rwab_RWA
rb22 :: Register
rb22 = Register $ 22 .|. rwab_RWB
ra23 :: Register
ra23 = Register $ 23 .|. rwab_RWA
rb23 :: Register
rb23 = Register $ 23 .|. rwab_RWB
ra24 :: Register
ra24 = Register $ 24 .|. rwab_RWA
rb24 :: Register
rb24 = Register $ 24 .|. rwab_RWB
ra25 :: Register
ra25 = Register $ 25 .|. rwab_RWA
rb25 :: Register
rb25 = Register $ 25 .|. rwab_RWB
ra26 :: Register
ra26 = Register $ 26 .|. rwab_RWA
rb26 :: Register
rb26 = Register $ 26 .|. rwab_RWB
ra27 :: Register
ra27 = Register $ 27 .|. rwab_RWA
rb27 :: Register
rb27 = Register $ 27 .|. rwab_RWB
ra28 :: Register
ra28 = Register $ 28 .|. rwab_RWA
rb28 :: Register
rb28 = Register $ 28 .|. rwab_RWB
ra29 :: Register
ra29 = Register $ 29 .|. rwab_RWA
rb29 :: Register
rb29 = Register $ 29 .|. rwab_RWB
ra30 :: Register
ra30 = Register $ 30 .|. rwab_RWA
rb30 :: Register
rb30 = Register $ 30 .|. rwab_RWB
ra31 :: Register
ra31 = Register $ 31 .|. rwab_RWA
rb31 :: Register
rb31 = Register $ 31 .|. rwab_RWB
uniform_read :: Register
uniform_read = Register $ 32 .|. rwab_RAB
r0 :: Register
r0 = Register $ 32 .|. rwab_WAB
r1 :: Register
r1 = Register $ 33 .|. rwab_WAB
r2 :: Register
r2 = Register $ 34 .|. rwab_WAB
r3 :: Register
r3 = Register $ 35 .|. rwab_WAB
tmu_noswap :: Register
tmu_noswap = Register $ 36 .|. rwab_WAB
r5 :: Register
r5 = Register $ 37 .|. rwab_WAB
element_number :: Register
element_number = Register $ 38 .|. rwab_RA
qpu_number :: Register
qpu_number = Register $ 38 .|. rwab_RB
host_int :: Register
host_int = Register $ 38 .|. rwab_WAB
nop :: Register
nop = Register $ 39 .|. rwab_RWAB
uniforms_address :: Register
uniforms_address = Register $ 40 .|. rwab_WAB
vpm_read :: Register
vpm_read = Register $ 48 .|. rwab_RAB
vpm_write :: Register
vpm_write = Register $ 48 .|. rwab_WAB
vpm_ld_busy :: Register
vpm_ld_busy = Register $ 49 .|. rwab_RA
vpm_st_busy :: Register
vpm_st_busy = Register $ 49 .|. rwab_RB
vpmvcd_rd_setup :: Register
vpmvcd_rd_setup = Register $ 49 .|. rwab_WA
vpmvcd_wr_setup :: Register
vpmvcd_wr_setup = Register $ 49 .|. rwab_WB
vpm_ld_wait :: Register
vpm_ld_wait = Register $ 50 .|. rwab_RA
vpm_st_wait :: Register
vpm_st_wait = Register $ 50 .|. rwab_RB
vpm_ld_addr :: Register
vpm_ld_addr = Register $ 50 .|. rwab_WA
vpm_st_addr :: Register
vpm_st_addr = Register $ 50 .|. rwab_WB
mutex_acquire :: Register
mutex_acquire = Register $ 51 .|. rwab_RAB
mutex_release :: Register
mutex_release = Register $ 51 .|. rwab_WAB
sfu_recip :: Register
sfu_recip = Register $ 52 .|. rwab_WAB
sfu_recipsqrt :: Register
sfu_recipsqrt = Register $ 53 .|. rwab_WAB
sfu_exp :: Register
sfu_exp = Register $ 54 .|. rwab_WAB
sfu_log :: Register
sfu_log = Register $ 55 .|. rwab_WAB
tmu0s :: Register
tmu0s = Register $ 56 .|. rwab_WAB
tmu0t :: Register
tmu0t = Register $ 57 .|. rwab_WAB
tmu0r :: Register
tmu0r = Register $ 58 .|. rwab_WAB
tmu0b :: Register
tmu0b = Register $ 59 .|. rwab_WAB
tmu1s :: Register
tmu1s = Register $ 60 .|. rwab_WAB
tmu1t :: Register
tmu1t = Register $ 61 .|. rwab_WAB
tmu1r :: Register
tmu1r = Register $ 62 .|. rwab_WAB
tmu1b :: Register
tmu1b = Register $ 63 .|. rwab_WAB
