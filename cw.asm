;Program READFILE.ASM: Read and display a text file.
;
;use16
FORMAT MZ
ORG 100H
JMP START

;Data segment
FILENAME   DB   "HELLO.TXT",0             ;Filename
HANDLE     RW   1                         ;Handele
FBUFF      RB   1024                      ;file data buffer
OEMSG      DB   'Cannot open HELLO.TX.$'  ;Open error
RFMSG      DB   'Cannot read HELLO.TX.$'  ;Read error
CFMSG      DB   'Cannot close HELLO.TX.$' ;Close error
NAME       RB   18                        ;Name buffer
NAMECNTSTR RB   8                         ;Name counted buffer
SPASECNT   RB   1                         ;Space count
NAMECNT    RB   1                         ;Name count
ENDLINE    DB   13,10,'$'                 ;Endl
;END OF DATA SEGMENT


;Printout macro:
;      Print result string with name contains in NAME and count of it in NAMECNT
;In args:
;      NONE.
MACRO PRINTOUT
{
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX

        MOV  DH,[NAMECNT]

        MOV  SI,NAMECNTSTR
        MOV  BL,10
        DEC  SI
LOOPDIV:                   ; Create from [NAMECNT] string like 01$
        INC  SI
        MOVZX  AX,DH
        DIV  BL
        MOV  DH,AL
        ADD  AH,'0'
        MOV  [SI],AH
        OR   DH,DH
        JNE  LOOPDIV

        INC  SI
        MOV  BYTE[SI],'$'
        DEC  SI
@@:                        ; Swap, cos aftre prew loop ten look like 01$, this loop make it normal 10$
        MOV  AL,[SI-1]
        MOV  AH,[SI]
        CMP  AL,'0'
        JNGE @F
        XCHG AL,AH
        MOV  [SI-1],AL
        MOV  [SI],AH
        DEC  SI
        JMP  @B
@@:                        ; Print out
        MOVZX  AX,[NAME+1]
        MOV  SI,NAME+2
        ADD  SI,AX
        MOV  BYTE[SI],' '
        INC  SI
        MOV  BYTE[SI],'$'
        MOV  AH,09H
        MOV  DX,NAME+2
        INT  21H

        MOV  DX,NAMECNTSTR
        INT  21H

        MOV  DX,ENDLINE
        INT  21H
} ; END OF 'PRINTOUT' MACROS

;Print macro:
;      Print data from FBUFF with CNT len
;In args:
;      CNT - len of FBUFF
MACRO PRINT CNT
{
        PUSH  AX
        PUSH  BX
        PUSH  DX

        MOV   BX,FBUFF
        ADD   BX,CNT
        MOV   BYTE[BX],'$'

        MOV   AH,9
        MOV   DX,FBUFF
        INT   21H

        POP   DX
        POP   BX
        POP   AX
} ; END OF 'PRINT' MACROS

;Parce macro:
;      Count NAME in FBUFF with CNT len
;In args:
;      CNT - len of FBUFF
MACRO PARCE LEN
{
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX

        MOV  DL,[SPASECNT] ;Count of spaces
        MOV  DH,[NAMECNT]  ;Count of name

        MOV  CX,LEN
        MOV  SI,FBUFF
.LOOPP:                    ;Main loop
        MOV  AL,[SI]
        CMP  AL,' '
        JNE  @F
        INC  DL
@@:
        CMP  DL,1
        JNE  .LOOPEND

        INC  SI
        PUSH CX
        PUSH BX
        MOV  DI,NAME+2
        MOVZX  CX,[NAME+1]
.COMPARE:                  ;String compare loop
        MOV  BL,[SI]
        MOV  BH,[DI]
        CMP  BL,BH
        JNE  .TOSPACE
        INC  SI
        INC  DI
        LOOP .COMPARE
        INC  DH
.TOSPACE:                  ;Next space finder loop
        MOV  BL,[SI]
        CMP  BL,' '
        JE   @F
        INC  SI
        JMP  .TOSPACE
@@:
        INC  DL

        POP  BX
        POP  CX
.LOOPEND:
        CMP  DL,2
        JNE  @F
        XOR  DL,DL
@@:
        INC  SI
        LOOP .LOOPP

        MOV  [SPASECNT],DL
        MOV  [NAMECNT],DH

        POP  DX
        POP  CX
        POP  BX
        POP  AX
}

;Open file macro:
;      Just open file with name FILENAME
;In args:
;      NONE.
OPENFILE:
        MOV  AH,3DH
        XOR  AL,AL
        MOV  DX,FILENAME
        XOR  CX,CX
        INT  21H
        JC   .ERR
        MOV  [HANDLE],AX
        RET
.ERR:
        MOV  DX,OEMSG
        MOV  AH,9
        INT  21H
        STC
        RET
; END OF 'OPENFILE' MACROS

;Read file macro:
;      Read data from [HANDLE], that contains in AX,
;      and put data to FBUFF, than for test print it
;In args:
;      NONE.
READFILE:
        MOV   BX,AX
.LOOP:
        MOV   AH,3FH
        MOV   DX,FBUFF
        MOV   CX,1024
        INT   21H
        JC    .ERR
        CMP   AX,0
        JZ    .END
        MOV   DX,FBUFF
        CMP   DX,1AH
        JZ    .END
        PARCE AX
        jmp   .LOOP
.ERR:
        MOV  DX,RFMSG
        MOV  AH,9
        INT  21H
        STC
        RET
.END:
        RET
; END OF 'READFILE' MACROS

;Close file macro:
;      Close file using HANDLE
;In args:
;      NONE.
CLOSEFILE:
        MOV  AH,3EH
        MOV  BX,[HANDLE]
        INT  21H
        JC   .ERR
        RET
.ERR:
        MOV  DX,CFMSG
        MOV  AH,9
        INT  21H
        STC
; END OF 'PRINT' MACROS


START:
        MOV  [SPASECNT],0
        MOV  [NAMECNT],0

        ;MOV [NAME],18
        ;MOV [NAME+1],3
        ;MOV [NAME+2],'L'
        ;MOV [NAME+3],'O'
        ;MOV [NAME+4],'L'

        ;MOV SI,NAME
        ;MOV CX,FBUFF

        MOV  AH,0AH
        MOV  [NAME],18
        MOV  DX,NAME
        INT  21H

        CALL OPENFILE             ;open BUFF.ASM
        JC   EXIT            ;jump if error
        CALL READFILE             ;read BUFF.ASM
        CALL CLOSEFILE            ;close BUFF.ASM

        PRINTOUT

EXIT:
        MOV AH,8
        INT 21H
        MOV AX,4C00H
        INT 21H
