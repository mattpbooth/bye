;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BYE! BOOTHY!
; synchingfeeling@gmail.com
; (Compile with NESASM)
; Hugely based on the Nerdy Nights tutorials:
; http://www.nintendoage.com/pub/faq/NA/nerdy_nights_out.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; iNES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .inesprg 2   ; 2x 16KB PRG code (1 for sound, 1 for game)
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; VERT mirroring for HORIZ scrolling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CPU Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $000-$100 = zero page, general vars
; $100-$200 = stack
; $200-$300 = OAM
; $300-$400 = audio vars

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Zero page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .rsset $0000 
  
scroll          .rs 1  ; horizontal scroll count
nametable  		  .rs 1  ; which nametable to use, 0 or 1
gamestate       .rs 1  ; current gamestate
buttons1			  .rs 1  ; player 1 buttons
pointerLo       .rs 1  ; for general 16 bit addressing, low byte
pointerHi       .rs 1  ; for general 16 bit addressing, high byte
soundPointer    .rs 2  ; for sound addressing, word
soundPointer2   .rs 2  ; for sound addressing and indirect jumps
controllerDelay .rs 1  ; for controller input frame delays
sleeping        .rs 1  ; main program / NMI sync

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SKY                     = $26    ; Cloud bg addresses
CLOUDTOPLEFT            = $29    
CLOUDTOPRIGHT           = $2A    
CLOUDLEFT               = $28
CLOUDRIGHT              = $2B
CLOUDMIDDLE             = $27
CLOUDBOTTOMLEFT         = $2C
CLOUDBOTTOMMIDDLELEFT   = $2D
CLOUDBOTTOMMIDDLERIGHT  = $2E
CLOUDBOTTOMRIGHT        = $2F
  
  .include "registers.i" ; all register constants

; Screen
CONTROLLERDELAYDEBOUNCE = $10

; Game State
GAMESTATETITLEINIT      = $05
GAMESTATETITLEUPDATE    = $04
GAMESTATEINTROINIT      = $03
GAMESTATEINTROUPDATE    = $02
GAMESTATEPLAYINGINIT    = $01
GAMESTATEPLAYINGUPDATE  = $00

; Controller
CONTROLLERA             = %10000000
CONTROLLERSELECT        = %00100000
CONTROLLERSTART         = %00010000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank 0 (Sound)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .bank 0
  .org $8000  ; first 8k PRG ROM bank
  .include "sound_engine.asm" 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank 1 (???)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .bank 1
  .org $A000  ; second 8k PRG ROM bank
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank 2 (Game)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .bank 2
  .org $C000  ; third 8k PRG ROM bank
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LOAD_PALETTE .macro     ; Load in the palette
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA PPUADDR           ; write the high byte of $3F00 address
  LDA #$00
  STA PPUADDR           ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
.LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                        ; 1st time through loop it will load palette+0
                        ; 2nd time through loop it will load palette+1
                        ; 3rd time through loop it will load palette+2
                        ; etc
  STA PPUDATA           ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE .LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  .endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LOAD_SPRITES .macro     ; Load in the sprites
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $20, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
  .endm
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LOAD_NAMETABLES .macro    ; Fill nametables
  LDA PPUSTATUS           ; read PPU status to reset the high/low latch
  LDA #$20  
  STA PPUADDR               ; write the high byte of $2000 address (nametable 0)
  LDA #$00  
  STA PPUADDR               ; write the low byte of $2000 address
  LDX #$14                ; fill 32 x 32 x 2 bytes = 2KB. 20 or 0x14, being < the 32 for a row gives a good result
  LDY #$10                ; 4 rows for each cloud + sky = 8. Over 2 screens = 10 (As we'll pre-decrement later for BNE)
  LDA #SKY
.FillNametablesLoop:
  STA PPUDATA             ; Row of sky
  DEX
  BNE .FillNametablesLoop
  JSR DrawCloud           ; Draw a cloud over the next 3 rows
  LDX #$14
  LDA #SKY                ; back to sky
  DEY                     ; next 4 rows completed
  BNE .FillNametablesLoop
.FillFinalNametables:
  LDA #SKY
  LDX #$80                ; 128 (0x80) For the remainder
.FillFinalNametablesLoop:  ; the lascalt bit of sky, making up for sky fill being < 32  
  STA PPUDATA
  DEX
  BNE .FillFinalNametablesLoop
  .endm
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LOAD_ATTRIBUTES .macro
  LDA PPUSTATUS         ; read PPU status to reset the high/low latch
  LDA \1                ; substitute high byte to allow macro use across both attributes
  STA PPUADDR           ; write the high byte of $23C0 address (nametable 0 attributes)
  LDA #$C0
  STA PPUADDR           ; write the low byte of $23C0 address
  LDX #$40              ; fill 64 bytes
  LDA #$00
FillAttribLoop\@:       ; Unique label guaranteed by \@ argument
  STA PPUDATA
  DEX
  BNE FillAttribLoop\@
  .endm
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HIDE_SCREEN .macro      ; screen off
  LDA #$0   		      
  STA PPUMASK
  .endm
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SHOW_SCREEN .macro      ; screen on
  LDA #%00011110        ; enable sprites, enable background, no clipping on left side
  STA PPUMASK
  .endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:         ; CPU Bootstrap
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX PPUCTRL  ; disable NMI
  STX PPUMASK  ; disable rendering
  STX $4010    ; disable DMC IRQs

VBlankWait1:       ; First wait for vblank to make sure PPU is ready
  BIT PPUSTATUS
  BPL VBlankWait1

ClrMem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x    ; special case for sprite OAM
  INX
  BNE ClrMem
   
VBlankWait2:      ; Second wait for vblank, PPU is ready after this
  BIT PPUSTATUS
  BPL VBlankWait2

  LOAD_PALETTE          ; Load in the palette
  LOAD_SPRITES          ; Load in the sprites
  LOAD_NAMETABLES       ; Load in the nametables           
  LOAD_ATTRIBUTES #$23  ; Load in the attributes for nametable 0 ($23CO)
  LOAD_ATTRIBUTES #$27  ; Load in the attributes for nametable 1 ($27CO)
              
  JSR SoundInit      ; Initialise sound
  LDA #$00           ; Song #0 please
  JSR SoundLoad      ; Load sound
  
  LDA #%10010000      ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA PPUCTRL

  SHOW_SCREEN         ; Render screen, sprites  

Forever:
  INC sleeping          ; slept another frame
.Loop
  LDA sleeping
  BNE .Loop             ; PPU will awaken when ready
  
  ;Main loop tasks
  JSR ReadController1   ; get the current button data for player 1  
  JMP Forever           ; jump back to Forever, infinite loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NMI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
  ; save registers
  PHA
  TXA
  PHA
  TYA
  PHA
  
  JSR VBlankGameLogicUpdate ; Game Logic Update in VBlank
  
  LDA #$00
  STA OAMADDR       
  LDA #$02
  STA SPRITEDMA      ; sprite DMA from $0200

  LDA #$00
  STA PPUADDR        ; clean up PPU address registers
  STA PPUADDR
  
  LDA scroll
  STA PPUSCROLL   ; write the horizontal scroll count register

  LDA #$00         ; no vertical scrolling
  STA PPUSCROLL
    
  ; This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ORA nametable    ; select correct nametable for bit 0
  STA PPUCTRL
    
.drawFinished           ; NMI non-render block now
  JSR SoundPlayFrame    ; update sound 
  
  LDA #$00              ; awaken main program
  STA sleeping
  
  ; Restore registers
  PLA
  TAY
  PLA
  TAX
  PLA
  
  RTI              ; return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game State Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .include "gamelogicvblank.asm"  ; vblank logic (game specific)
  ;.include "gamelogicpostvblank.asm" ; NMI logic post vblank (game specific)
  ;.include "gamelogicloop.asm" ; Logic loop outside NMI (game specific)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LoadOneScreenAlignedNametable:  ; 1KB read from $xx00 data block
  LDA PPUSTATUS                 ; read PPU status to reset the high/low latch
  LDA #$20
  STA PPUADDR                     ; write the high byte of $2000 address
  LDA #$00
  STA PPUADDR                     ; write the low byte of $2000 address
  LDA #$00
  STA pointerLo                 ; put the low byte of the address of background into pointer
  LDX #$00                      ; start at pointer + 0
  LDY #$00
.LoadOneScreenAlignedNametableOutsideLoop:
  
.LoadOneScreenAlignedNametableInsideLoop:
  LDA [pointerLo], y                              ; copy one background byte from address in pointer plus Y
  STA PPUDATA                                      ; this runs 256 * 4 times (=1K =$400)
  INY                                             ; inside loop counter
  CPY #$00
  BNE .LoadOneScreenAlignedNametableInsideLoop     ; run the inside loop 256 times before continuing down
  INC pointerHi                                   ; low byte went 0 to 256, so high byte needs to be changed now
  INX
  CPX #$04
  BNE .LoadOneScreenAlignedNametableOutsideLoop    ; run the outside loop 256 times before continuing down
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
ReadController1:                ; P1 controller read
  LDA controllerDelay           ; Load the controller delay var
  BEQ .ReadController1Latch      ; If there's a delay
  LDA #$00                      ; Load the accumulator with a null controller value  
  STA buttons1                  ; and store that as current button state
  DEC controllerDelay           ; Decrement by one frame
  JMP .ReadController1End        ; and exit.
.ReadController1Latch:
  LDA #$01                     ; Latch
  STA CNTLLATCH
  LDA #$00
  STA CNTLLATCH
  LDX #$08  
.ReadController1Loop:
  LDA CNTLREAD1
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE .ReadController1Loop
.ReadController1End:
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
DrawCloudRowFiller:
  LDA #SKY
  LDX #$1C                    ; 32 - 4 (cloud tiles) = 28 or 1C
.DrawCloudRowFillerLoop:
  STA PPUDATA
  DEX
  BNE .DrawCloudRowFillerLoop
  RTS
  
DrawCloud:
  LDA #SKY                    ; 1. tile of sky 
  STA PPUDATA        
  LDA #CLOUDTOPLEFT           ; 2. tile of top left cloud
  STA PPUDATA        
  LDA #CLOUDTOPRIGHT          ; 3. tile of top right cloud
  STA PPUDATA
  LDA #SKY                    ; 4. tile of sky 
  STA PPUDATA        
  JSR DrawCloudRowFiller      ; Sky filler for rest of row
  LDA #CLOUDLEFT              ; 1. left cloud
  STA PPUDATA        
  LDA #CLOUDMIDDLE            ; 2-3. cloud fills
  STA PPUDATA
  STA PPUDATA  
  LDA #CLOUDRIGHT             ; 4. right cloud
  STA PPUDATA        
  JSR DrawCloudRowFiller      ; Sky filler for rest of row
  LDA #CLOUDBOTTOMLEFT        ; 1. bottom left cloud
  STA PPUDATA
  LDA #CLOUDBOTTOMMIDDLELEFT  ; 2. bottom middle left cloud
  STA PPUDATA
  LDA #CLOUDBOTTOMMIDDLERIGHT ; 3. bottom middle right cloud
  STA PPUDATA
  LDA #CLOUDBOTTOMRIGHT       ; 4. bottom right cloud
  STA PPUDATA                      
  JSR DrawCloudRowFiller      ; Sky filler for rest of row  
  RTS                         ; done!
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank 4 (Data)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .bank 3
  .org $E000      ; Fourth 8k bank of PRG ROM
  
titleScreenNametable:
  .incbin "title.nam" ; E000 and onwards for name tables 256 byte aligned for lower pointer.
introScreenNametable:
  .incbin "intro.nam" ; E400 (intro screen)

palette:
  .incbin "title.pal" ; background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette
	
sprites:
     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0
  .db $80, $01, $00, $88   ;sprite 1
  .db $88, $02, $00, $80   ;sprite 2
  .db $88, $03, $00, $88   ;sprite 3

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw Reset      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bank 5 (CHR)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .bank 4
  .org $0000  ; CHR Bank
  .incbin "byesprites.chr"   ;includes 8KB graphics file