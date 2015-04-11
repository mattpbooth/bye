;these are aliases to use in the sound data.
endsound = $A0
loop = $A1
volume_envelope = $A2
duty = $A3
set_loop1_counter = $A4
loop1 = $A5
set_note_offset = $A6
adjust_note_offset = $A7
transpose = $A8

;-----------------------------------------------------------------------
;this is our JUMP TABLE!
SoundOpcodes:
    .word se_op_endsound            ;$A0
    .word se_op_infinite_loop       ;$A1
    .word se_op_change_ve           ;$A2
    .word se_op_duty                ;$A3
    .word se_op_set_loop1_counter   ;$A4
    .word se_op_loop1               ;$A5
    .word se_op_set_note_offset     ;$A6
    .word se_op_adjust_note_offset  ;$A7
    .word se_op_transpose           ;$A8
    ;etc, 1 entry per subroutine

    
;-----------------------------------------------------------------
; these are the actual opcode subroutines
se_op_endsound:
    lda streamStatus, x    ;end of stream, so disable it and silence
    and #%11111110
    sta streamStatus, x    ;clear enable flag in status byte
    
    lda streamChannel, x
    cmp #CHANNEL_TRIANGLE
    beq .silence_tri        ;triangle is silenced differently from squares and noise
    lda #$30                ;squares and noise silenced with #$30
    bne .silence            ; (this will always branch.  bne is cheaper than a jmp)
.silence_tri:
    lda #$80                ;triangle silenced with #$80
.silence:
    sta streamVolumeDuty, x  ;store silence value in the stream's volume variable.

    rts
    
se_op_infinite_loop:
    lda [soundPointer], y      ;read ptr LO from the data stream
    sta streamPointerLo, x    ;update our data stream position
    iny
    lda [soundPointer], y      ;read ptr HI from the data stream
    sta streamPointerHi, x    ;update our data stream position
    
    sta soundPointer+1         ;update the pointer to reflect the new position.
    lda streamPointerLo, x
    sta soundPointer
    ldy #$FF                ;after opcodes return, we do an iny.  Since we reset  
                            ;the stream buffer position, we will want y to start out at 0 again.
    rts
    
se_op_change_ve:
    lda [soundPointer], y      ;read the argument
    sta streamVolumeEnvelope, x        ;store it in our volume envelope variable
    lda #$00
    sta streamVolumeEnvelopeIndex, x  ;reset volume envelope index to the beginning
    rts
    
se_op_duty:
    lda [soundPointer], y
    sta streamVolumeDuty, x
    rts
    
se_op_set_loop1_counter:
    lda [soundPointer], y      ;read the argument (# times to loop)
    sta streamLoop1, x     ;store it in the loop counter variable
    rts
    
se_op_loop1:
    dec streamLoop1, x     ;decrement the counter
    lda streamLoop1, x
    beq .last_iteration     ;if zero, we are done looping
    jmp se_op_infinite_loop ;if not zero, jump back
.last_iteration:
    iny                     ;skip the first byte of the address argument
                            ; the second byte will be skipped automatically upon return
                            ; (see se_fetch_byte after "jsr se_opcode_launcher")
    rts
    
se_op_set_note_offset:
    lda [soundPointer], y          ;read the argument
    sta streamNoteOffset, x      ;set the note offset.
    rts
    
se_op_adjust_note_offset:
    lda [soundPointer], y          ;read the argument (what value to add)
    clc
    adc streamNoteOffset, x   ;add it to the current offset
    sta streamNoteOffset, x   ;and save.
    rts
    
se_op_transpose:
    lda [soundPointer], y          ;read low byte of the pointer to our lookup table
    sta soundPointer2              ;store it in a new pointer variable
    iny
    lda [soundPointer], y          ;read high byte of pointer to table
    sta soundPointer2+1
    
    sty soundTemp1             ;save y because we are about to destroy it
    lda streamLoop1, x         ;get loop counter, put it in Y
    tay                         ;   this will be our index into the lookup table
    dey                         ;subtract 1 because indexes start from 0.
    
    lda [soundPointer2], y         ;read a value from the table.
    clc
    adc streamNoteOffset, x   ;add it to the note offset
    sta streamNoteOffset, x
    
    ldy soundTemp1             ;restore Y
    rts