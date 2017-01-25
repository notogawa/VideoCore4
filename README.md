# VideoCore4 Assembler

~~~~haskell
main :: IO ()
main = mapM_ (uncurry (printf "0x%08x, 0x%08x,\n") . convertW64toW32x2 . encode) dummy

convertW64toW32x2 :: Word64 -> (Word32, Word32)
convertW64toW32x2 n = (fromIntegral $ n .&. 0xFFFFFFFF, fromIntegral $ n `shiftR` 32)

mov :: Register -> Register -> Asm ()
mov dst src =
  let (mux, raddr) = if AR `elem` rwab src then (RA, raddr_a) else (RB, raddr_b)
  in alu $ bor dst mux mux `with` do
    raddr src
    when (BW `elem` rwab dst) $
      ws ws_On

dummy :: [Instruction]
dummy = asm $ do
  mov ra0 uniform_read
  mov ra1 uniform_read
  mov ra2 uniform_read
  alui $ shl r0 RA RB `with` do { raddr_a element_number; small_immed (2 :: Int) }
  alu  $ mul24 r0 RA R0 `with` do { raddr_a ra1 }
  alu  $ iadd ra0 RA R0 `with` do { raddr_a ra0 }
  setupVCDDMALoad 0 0 0 0 1 0 0
  li32 $ do { cond_add cond_AL; waddr_add r0; immediate 0x90000000 }
  label "loop"
  -- Add & Mul Operation by (add & mul)
  alu  $ bor ra0 RA RA # v8min rb0 RA RA `with` do { raddr_a ra0 }
  ml <- toLabel "loop"
  let l = maybe (error "unknown label") id ml
  br $ do { cond_br cond_br_JZC_ANY; rel rel_On; immediate l }
  .
  .
  .
~~~~