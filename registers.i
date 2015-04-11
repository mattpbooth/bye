; Constants for all registers

; CPU => PPU Memory maps
PPUCTRL                 = $2000
PPUMASK                 = $2001
PPUSTATUS               = $2002
OAMADDR                 = $2003
OAMDATA                 = $2004
PPUSCROLL               = $2005
PPUADDR                 = $2006
PPUDATA                 = $2007

; CPU => Sprite maps
SPRITEDMA               = $4014   ; DMA for sprite memory
; CPU => Controller Memory maps
CNTLLATCH                = $4016  ; Latch/Strobe for both controllers (write)
CNTLREAD1                = $4016  ; Read for controller 1 (read)
CNTLREAD2                = $4017  ; Read for controller 2 (read)

; CPU => APU Memory maps
APUFLAGS                 = $4015  ; Flags for all of APU
APUSQ1_ENV               = $4000  ; Square 1 envelope
APUSQ1_SWEEP             = $4001  ; Square 1 sweep
APUSQ1_LO                = $4002  ; Square 1 period low
APUSQ1_HI                = $4003  ; Square 1 period high
APUSQ2_ENV               = $4004  ; Square 2 envelope
APUSQ2_SWEEP             = $4005  ; Square 2 sweep
APUSQ2_LO                = $4006  ; Square 2 period low
APUSQ2_HI                = $4007  ; Square 2 period high
APUTRI_CTRL              = $4008  ; Triangle control
APUTRI_LO                = $400A  ; Triangle period low
APUTRI_HI                = $400B  ; Triangle period high
APUNOISE_ENV             = $400C  ; Noise envelope
APUNOISE_RAND            = $400E  ; Random generator
APUNOISE_LENGTH_COUNTER  = $400F  ; Length counter load