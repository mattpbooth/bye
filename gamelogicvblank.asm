;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Logic - NMI VBlank
; Must implement VBlankGameLogicUpdate as Subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VBlankGameLogicUpdate:
  ; VBlank Game state update
  LDX gamestate	   		              ; check the gamestate
  BEQ VBlankGameStateTitleInit      ; title init
  DEX
  BEQ VBlankGameStateTitleUpdate    ; title update
  DEX
  BEQ VBlankGameStateIntroInit      ; intro init
  DEX
  BEQ VBlankGameStateIntroUpdate    ; intro update
  DEX
  BEQ VBlankGameStatePlayingInit    ; game init
  DEX
  BEQ VBlankGameStatePlayingUpdate  ; game update
VBlankGameStateDone:
  RTS

VBlankGameStateTitleInit:           ; initialise title during vblank
  HIDE_SCREEN                       ; clear the screen ready
  INC gamestate
  LDA #HIGH(titleScreenNametable)   ; Prepare mem-aligned read of the nametable data for title screen
  STA pointerHi                     ; Pre-set high byte
  JSR LoadOneScreenAlignedNametable ; Load aligned title screen nametable
  JMP VBlankGameStateDone

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
VBlankGameStateTitleUpdate:         ; update title during vblank
  SHOW_SCREEN                       ; enable sprites, enable background, no clipping on left side 
  LDA buttons1
  AND #CONTROLLERSTART
  BEQ .VBlankGameStateTitleUpdateEnd
  INC gamestate
.VBlankGameStateTitleUpdateEnd:
  JMP VBlankGameStateDone
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
VBlankGameStateIntroInit:           ; initialise intro during vblank
  HIDE_SCREEN                       ; clear the screen ready
  INC gamestate
  LDA #HIGH(introScreenNametable)   ; Prepare mem-aligned read of the nametable data for title screen
  STA pointerHi                     ; Pre-set high byte
  JSR LoadOneScreenAlignedNametable ; Load aligned title screen nametable
  LDA #CONTROLLERDELAYDEBOUNCE      ; get a delay value for the intro
  STA controllerDelay               ; and store, ready to stop the controller read from being too twitchy
  JMP VBlankGameStateDone

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
VBlankGameStateIntroUpdate:         ; update intro during vblank
  SHOW_SCREEN                       ; enable sprites, enable background, no clipping on left side 
  LDA buttons1
  AND #CONTROLLERSTART
  BEQ .VBlankGameStateIntroUpdateEnd
  INC gamestate
.VBlankGameStateIntroUpdateEnd:
  JMP VBlankGameStateDone
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
VBlankGameStatePlayingInit:          ; initialise gameplay during vblank
  INC gamestate
  JMP VBlankGameStateDone
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VBlankGameStatePlayingUpdate:        ; update gameplay during vblank
  INC scroll                         ; add one to our scroll variable each frame
  
NTSwapCheck:        ; Do nametable swap
  LDA scroll        ; check if the scroll just wrapped from 255 to 0
  BNE NTSwapCheckDone
  
NTSwap:
  LDA nametable    ; load current nametable number (0 or 1)
  EOR #$01         ; exclusive OR of bit 0 will flip that bit
  STA nametable    ; so if nametable was 0, now 1
                   ;    if nametable was 1, now 0
NTSwapCheckDone:
	 
MovePlayerSprite:   ; Move the player sprite
  LDA $0203		      ; load sprite X position
  CLC			          ; clear carry flag
  ADC #$01		      ; A += 1
  STA $0203		      ; Save new sprite X position
  
  LDA #%00011110    ; enable sprites, enable background, no clipping on left side
  STA PPUMASK
  
  JMP VBlankGameStateDone