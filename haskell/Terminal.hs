module Terminal (getTermSize) where

import Foreign
    (Ptr,
      Storable(peek, poke, alignment, sizeOf, peekByteOff, pokeByteOff),
      with)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt(..), CUShort)

data WinSize = WinSize { wsRow, wsCol :: CUShort }

instance Storable WinSize where
  sizeOf :: WinSize -> Int
  sizeOf _ = ((8))
  alignment _ = (2) 
  peek ptr = do
    row <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
    col <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
    return $ WinSize row col
  poke ptr (WinSize row col) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr row
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) ptr col


foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt


getTermSize :: IO (Int, Int)
getTermSize = 
  with (WinSize 0 0) $ \ws -> do
    throwErrnoIfMinus1 "ioctl" $
      ioctl (1) (21523) ws
    WinSize row col <- peek ws
    return (fromIntegral row, fromIntegral col)
