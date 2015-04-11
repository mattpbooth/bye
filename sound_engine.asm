;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .rsset $0300 ;sound engine variables will be on the $0300 page of RAM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHANNEL_SQUARE_1  = $00 ;these are channel constants
CHANNEL_SQUARE_2  = $01
CHANNEL_TRIANGLE  = $02
CHANNEL_NOISE     = $03

STREAM_MUSIC_SQ1  = $00 ;these are stream # constants
STREAM_MUSIC_SQ2  = $01 ;stream # is used to index into variables
STREAM_MUSIC_TRI  = $02
STREAM_MUSIC_NOI  = $03
STREAM_SFX_1      = $04
STREAM_SFX_2      = $05
STREAM_COUNT      = $06 ;bounds check limit

REST_BIT_MASK     = %00000010
REST_BIT_INV_MASK = %11111101

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
soundDisableFlag  .rs 1         ;a flag variable that keeps track of whether the sound engine is disabled or not. 
soundTemp1 .rs 1                ;temp, multipurpose 
soundTemp2 .rs 1                ;temp, multipurpose
soundSquare1Old .rs 1           ;last value written to $4003 (for crackle elimination)
soundSquare2Old .rs 1           ;last value written to $4007 (for crackle elimination)
softAPUPorts .rs 16             ;soft APU ports, to ensure we only write once per frame

;reserve 6 bytes, one for each stream
streamCurrentSound .rs 6        ;current song/sfx loaded
streamStatus .rs 6              ;status byte.   bit0: (1: stream enabled; 0: stream disabled)
streamChannel .rs 6             ;what channel is this stream playing on?
streamPointerLo .rs 6           ;low byte of pointer to data stream
streamPointerHi .rs 6           ;high byte of pointer to data stream
streamVolumeEnvelope .rs 6      ;current volume envelope
streamVolumeEnvelopeIndex .rs 6 ;position within current volume envelope
streamVolumeDuty .rs 6          ;stream volume/duty settings
streamNoteLo .rs 6              ;low 8 bits of period for the current note on a stream
streamNoteHi .rs 6              ;high 3 bits of period for the current note on a stream 
streamTempo .rs 6               ;value to add to ticker total each frame
streamTickerTotal .rs 6         ;running ticker total
streamNoteLengthCounter .rs 6   ;ticks into current note length
streamNoteLength .rs 6          ;current note length
streamLoop1 .rs 6               ;loop counter for sound compression
streamNoteOffset .rs 6          ;note offset for sound compression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Logic - init/disable/silence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundInit:
  LDA #$0F
  STA APUFLAGS   ;enable Square 1, Square 2, Triangle and Noise channels
  
  LDA #$00
  STA soundDisableFlag  ;clear disable flag
  ;later, if we have other variables we want to initialize, we will do that here.
  LDA #$FF
  STA soundSquare1Old ; First note will not be skipped now
  STA soundSquare2Old ; Ditto.
SoundSilence:
  LDA #$30
  STA softAPUPorts      ;set Square 1 volume to 0
  STA softAPUPorts + 4  ;set Square 2 volume to 0
  STA softAPUPorts + 12 ;set Noise volume to 0
  LDA #$80
  STA softAPUPorts + 8  ;silence Triangle
  RTS
  
SoundDisable:
  LDA #$00
  STA APUFLAGS   ;disable all channels
  LDA #$01
  STA soundDisableFlag  ;set disable flag
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Load a song or sfx
; @in: A (song or sfx #)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundLoad:
  STA soundTemp1        ;songNumber = A
  ASL a                 ;multiply by 2.  We are indexing into a table of pointers (words)
  TAY
  LDA SongHeaders, y    ;song header pointer
  STA soundPointer
  LDA SongHeaders + 1, y
  STA soundPointer + 1
  
  ;main header:
  ;00       number of streams
  ;01+      stream headers
  LDY #$00
  LDA [soundPointer], y
  STA soundTemp2          ;numberOfStreams = A
  INY
.Loop:
  ;stream headers
  ;00       which stream
  ;01       status byte
  ;02       which channel
  ;03       initial volume (and duty for squares)
  ;04       volume envelope
  ;05-06    pointer to data stream
  ;07       tempo
  ;08       stream ticker total
  ;09       stream note length counter
  LDA [soundPointer], y     ;stream #
  TAX
  INY
  
  LDA [soundPointer], y     ;status (1=enable, 0=disable)
  STA streamStatus, x       ;commit to streamStatus[x]
  BEQ .NextStream           ;stream inactive, next please! 
  INY
  
  LDA [soundPointer], y     ;channel #
  STA streamChannel, x      ;commit to streamChannel[x]
  INY
  
  LDA [soundPointer], y     ;duty and volume settings
  STA streamVolumeDuty, x   ;commit to streamVolumeDuty[x]
  INY
  
  LDA [soundPointer], y       ;stream volume envelope settings
  STA streamVolumeEnvelope, x ;commit to streamVolumeEnvelope[x]
  INY
  
  LDA [soundPointer], y     ;pointer to stream data.  Little endian, so low byte first
  STA streamPointerLo, x
  INY
  
  LDA [soundPointer], y     ;pointer to stream data. Hi.
  STA streamPointerHi, x
  INY
  
  LDA [soundPointer], y     ;tempo
  STA streamTempo, x
  
  LDA #$A0
  STA streamTickerTotal, x
  
  LDA #$01
  STA streamNoteLengthCounter, x ;first note should trigger early
  
.NextStream:
  INY
  
  LDA soundTemp1            ;A = songNumber
  STA streamCurrentSound, x ;commit to streamCurrentSound[x]
  
  DEC soundTemp2            ;--numberOfStreams
  BNE .Loop                 ;while(numberOfStreams > 0)
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Advance sound playback each frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundPlayFrame:
  LDA soundDisableFlag
  BNE .Done   ;if disable flag is set, don't advance a frame 
  
  JSR SoundSilence ;all channels silenced until correct active channels activated later.
  LDX #$00
.Loop:
  LDA streamStatus, x
  AND #$01         ; is this stream active?
  BEQ .EndLoop     ; Guess not...
  
  ;Add current tempo to ticker, if we set the carry flag a $FF=>$00 occured, a 'tick'
  LDA streamTickerTotal, x
  CLC
  ADC streamTempo, x
  STA streamTickerTotal, x
  BCC .SetBuffer      ; Carry clear so no tick...
  
  DEC streamNoteLengthCounter, x ;tick found, so decrement the note length
  BNE .SetBuffer            ; note has not finished playing...
  LDA streamNoteLength, x   ; not HAS finished playing, reload the note length counter
  STA streamNoteLengthCounter, x
  
  JSR SoundFetchByte  ; fetch byte
.SetBuffer:
  JSR SoundSetSoftAPU    ; Write to soft APU ports
.EndLoop:
  INX
  CPX #STREAM_COUNT ; Have we queried all streams?
  BNE .Loop         ; Nope, more please.
  JSR SoundSetAPU   ; Now write to hard API ports
.Done:
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Read a byte from the sound data stream and handle
; @in: X (Stream #)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundFetchByte:
  LDA streamPointerLo, x
  STA soundPointer
  LDA streamPointerHi, x
  STA soundPointer + 1

  LDY #$00
.Fetch:
  ;How to interpret the type of 
  ;Note:       < #$80 (negative)
  ;NoteLength: < #$A0 (positive)
  ;Opcode:     > #$A0
  LDA [soundPointer], y
  BPL .Note
  CMP #$A0
  BCC .NoteLength
  
.OpCode:         ;noteType == OPCODE
  JSR SoundOpcodeLauncher ;Jump table 'trick' for 
  INY                     ;Next position in data stream
  LDA streamStatus, x
  AND #%00000001
  BNE .Fetch              ;Grab the operand for opcode, unless stream is disabled.
  RTS
  
.NoteLength:       ;noteType == NOTELENGTH
  AND #%01111111  ;chop bit 7 to base the note length at $00
  STY soundTemp1  ;Y is now volatile
  TAY
  LDA NoteLengthTable, y  ;get the note length count value
  STA streamNoteLength, x
  STA streamNoteLengthCounter, x 
  LDY soundTemp1  ;Y is back!
  INY             ;Y is next byte in the stream
  JMP .Fetch      ;Make sure we fetch, we'll want a note for example

.Note:            ;noteType == NOTE
  STA soundTemp2        ;save out index into data stream
  LDA streamChannel, x
  CMP #CHANNEL_NOISE
  BNE .NotNoise
  JSR SoundDoNoise
  JMP .ResetVolumeEnvelope
.NotNoise:
  LDA soundTemp2
  STY soundTemp1
  CLC
  ADC streamNoteOffset, x
  ASL a                 ;multiply by 2, because our note table is stored as words
  TAY                   ;we'll use this as an index into the note table

  LDA noteTable, y      ;read the low byte of our period from the table
  STA streamNoteLo, x
  LDA noteTable + 1, y  ;read the high byte of our period from the table
  STA streamNoteHi, x
  LDY soundTemp1        ;restore data stream index
  
  JSR SoundFlagRest    ;flag the rest in the stream status if appropriate
.ResetVolumeEnvelope:
  LDA #$00
  STA streamVolumeEnvelopeIndex, x
.UpdatePointer:
  INY
  TYA
  CLC
  ADC streamPointerLo, x
  STA streamPointerLo, x
  BCC .End
  INC streamPointerHi, x
.End:
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Do noise will run the logic specific to noise channel
; @in: A (noise value)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundDoNoise:
  LDA soundTemp2
  AND #%00010000
  BEQ .Mode0
.Mode1
  LDA soundTemp2
  ORA #%10000000  ;mode should be in bit 7
  STA soundTemp2
.Mode0
  LDA soundTemp2
  STA streamNoteLo, x
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Check Rest will identify a rest and set appropriate flag.
; @in: X (stream number)
;    : Y (data stream index)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundFlagRest:
  LDA [soundPointer], y
  CMP #NOTE_REST  
  BNE .NotRest            ;Guess this wasn't a rest...
  LDA streamStatus, x     ;This is a rest, so get ready
  ORA #REST_BIT_MASK      ;and set the rest bit
  BNE .Store              ;always branch, branch cheaper than JMP
.NotRest:
  LDA streamStatus, x
  AND #REST_BIT_INV_MASK  ;clear the rest bit now
.Store:
  STA streamStatus, x ;store back with rest actioned
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Opcode Launcher will read an address from opcode jumptable
; and make an indirect jump.
; @in: A (opcode)
;    : Y (data stream position)
;    : X (stream number)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundOpcodeLauncher:
    STY soundTemp1        ;save out Y
    SEC                   ;set the carry
    SBC #$A0              ;subtract with borrow, makes our opcodes $00 based
    ASL a                 ;x2 for index into words
    TAY
    LDA SoundOpcodes, y   ;low byte of subroutine
    STA soundPointer2     ;store
    LDA SoundOpcodes+1, y ;high byte
    STA soundPointer2+1   ;store
    LDY soundTemp1        ;restore data stream position
    INY                   ;all our opcodes have 1 arg, so increment
    JMP [soundPointer2]   ;indirect jump to opcode subroutine
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Set Soft APU will store APU state in the soft APU variables
; @in: X (stream number)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundSetSoftAPU:
  LDA streamChannel, x
  ASL a
  ASL a ; x 4
  TAY
  
  JSR SoundSetStreamVolume  ;set the volume
  
  LDA #$08
  STA softAPUPorts + 1, y  ;Sweep
  
  LDA streamNoteLo, x
  STA softAPUPorts + 2, y  ;Period Lo
  
  LDA streamNoteHi, x
  STA softAPUPorts + 3, y  ;Period Hi
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Set Stream Volume deals with volume envelopes
; @in: Y (index into soft APU ports)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundSetStreamVolume:
  STY soundTemp1                    ;save index into softAPUPorts
  LDA streamVolumeEnvelope, x       ;load a with current volume envelope
  ASL a                             ;indexing into words x2
  TAY
  LDA VolumeEnvelopes, y            ;low byte of the address from ptr
  STA soundPointer                  ;store
  LDA VolumeEnvelopes + 1, y        ;high bytes of the address from ptr +1
  STA soundPointer + 1              ;store
.ReadVolumeEnvelope:
  LDY streamVolumeEnvelopeIndex, x  ;current position within current envelope
  LDA [soundPointer], y             ;get the value
  CMP #$FF
  BNE .SetVolume                    ;If we're not at the end, get the volume
  DEC streamVolumeEnvelopeIndex, x  ;Otherwise, seek back to read #$FF again next tick
  JMP .ReadVolumeEnvelope           ;this will repeat the last volume for the remainder of the note
.SetVolume:
  STA soundTemp2                    ;save the new volume
  CPX #CHANNEL_TRIANGLE             ;triangle will be a special case again
  BNE .Squares                      ;not triangle
  LDA soundTemp2                    ;rehydrate new volume into actioned
  BNE .Squares                      ;if volume is non-zero, treat triangle like squares
  LDA #$80
  BMI .StoreVol                     ;Branch on minus (silence) to store volatile
.Squares:
  LDA streamVolumeDuty, x           ;Get volume/duty for current
  AND #$F0                          ;Clear old volume
  ORA soundTemp2                    ;Bleed our new volume in
.StoreVol:
  LDY soundTemp1                    ;rehydrate soft APU port index
  STA softAPUPorts, y               ;store the volume in temporary port
  INC streamVolumeEnvelopeIndex, x  ;increment index into current volume envelope
.RestCheck:
  ;overwrite with silence if a rest is found
  LDA streamStatus, x
  AND #REST_BIT_MASK
  BEQ .Done
  LDA streamChannel,x
  CMP #CHANNEL_TRIANGLE
  BEQ .Triangle
  LDA #$30
  BNE .Store
.Triangle:
  LDA #$80
.Store:
  STA softAPUPorts, y
.Done
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sound Set APU writes the soft APU port data to the (hard) APU ports
; @in: X (stream number)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SoundSetAPU:
; unrolled for special case handling
.Square1:
  LDA softAPUPorts
  STA APUSQ1_ENV
  LDA softAPUPorts+1
  STA APUSQ1_SWEEP
  LDA softAPUPorts+2
  STA APUSQ1_LO
  LDA softAPUPorts+3
  CMP soundSquare1Old       ;compare to last write
  BEQ .Square2            ;don't write this frame if they were equal - danger of crackle
  STA APUSQ1_HI
  STA soundSquare1Old     ;save the value we just wrote to $4003
.Square2:
  LDA softAPUPorts+4
  STA APUSQ2_ENV
  LDA softAPUPorts+5
  STA APUSQ2_SWEEP
  LDA softAPUPorts+6
  STA APUSQ2_LO
  LDA softAPUPorts+7
  CMP soundSquare2Old
  BEQ .Triangle
  STA APUSQ2_HI
  STA soundSquare2Old       ;save the value we just wrote to $4007
.Triangle:
  LDA softAPUPorts+8
  STA APUTRI_CTRL
  LDA softAPUPorts+10   ;there is no $4009, so we skip it
  STA APUTRI_LO
  LDA softAPUPorts+11
  STA APUTRI_HI
.Noise:
  LDA softAPUPorts+12
  STA APUNOISE_ENV
  LDA softAPUPorts+14   ;there is no $400D, so we skip it
  STA APUNOISE_RAND
  LDA softAPUPorts+15
  STA APUNOISE_LENGTH_COUNTER
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SongHeaders:      ;pointer table, each entry is a song header
  .word Song1Header;
  
  .include "sound_opcodes.asm"    ;our opcode subroutines, 
  .include "note_table.i"         ;period lookup table for notes
  .include "note_length_table.i"  ;tempo lookup
  .include "vol_envelopes.i"
  .include "song1.i"              ;holds the data for song 1
