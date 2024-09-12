;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                    D E F L A T E   f o r   S y m b O S                     @
;@                                                                            @
;@                   (c)oded 2015 by Prodatron / SymbiosiS                    @
;@                      based on ideas and code by Grauw                      @
;@                   further optimizations by Wouter et al.                   @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;grauw
;- no 2nd tree jump, new tree-builder ( https//bitbucket.org/grauw/gunzip/commits/29787af9db17af99b48594eafc5e15f82396bc8b )
;- uncompressed blocks
;- presort code length table ( https//bitbucket.org/grauw/gunzip/commits/dfa1b4499afa26a99cd9b71bb12e64b24c36aaab )

;--- READER ROUTINES ----------------------------------------------------------
;...

;--- WRITER ROUTINES ----------------------------------------------------------
;...

;--- ALPHABET ROUTINES --------------------------------------------------------
;...

;--- FIXED ALPHABET ROUTINES --------------------------------------------------
;...

;--- DYNAMIC ALPHABET ROUTINES ------------------------------------------------
;...

;--- INFLATE ROUTINES ---------------------------------------------------------
;...


macro   bc_counter
        ld a,c
        dec bc
        inc b
        ld c,b
        ld b,a
endm


;==============================================================================
;### READER ROUTINES ##########################################################
;==============================================================================

redvarpoi   dw 0    ;pointer to next byte (IX)
redvarbit   dw 0    ;remaining bits of the current byte (C)

;### REDINI -> inits reader
;### Input      HL=reader buffer (256 bytes aligned), A=read buffer size/256
redini  ld (redvarpoi),hl
        add h
        dec a
        ld (redbyt1+1),a
        ld a,h
        ld (redbyt2+2),a
        xor a
        ld (redvarbit),a
        ret

;### REDLOD -> loads reader
;### Output     IX=pointer, C=bits
redlod  ld ix,(redvarpoi)
        ld bc,(redvarbit)
        ret

;### REDSTO -> stores reader
;### Input      IX=pointer, C=bits
redsto  ld (redvarpoi),ix
        ld (redvarbit),bc
        ret

;### REDALN -> aligns reader to byte boundary
redaln  ld c,0
        ret

;### REDBYT -> reads one byte from the input
;### Input      IX=read pointer
;### Output     A=value, IX updated
;### Destroyed  F
redbyt  ld a,(ix+0)
        db #dd:inc l
        ret nz
redbyt0 push af
        db #dd:ld a,h
redbyt1 cp 0
        jr z,redbyt2
        db #dd:inc h
        pop af
        ret
redbyt2 db #dd:ld h,0
        push bc
        push de
        push hl
        push ix
        push iy
        call inplod
        pop iy
        pop ix
        pop hl
        pop de
        pop bc
        pop af
        ret

;### REDBTx -> reads a fixed number of bits from the input
;### Input      IX=read pointer, C=bit field
;### Output     A=value, C,IX updated
;### Destroyed  F
macro   read_bit
        srl c
        call z,redbtna
endm
redbtna ld b,a
        ld a,(ix+0)
        db #dd:inc l
        call z,redbyt0
        scf
        rra
        ld c,a
        ld a,b
        ret
redbt1  xor a:read_bit:                                                                                           rla:ret
redbt2  xor a:read_bit:rra:read_bit:                                                                          rla:rla:ret
redbt3  xor a:read_bit:rra:read_bit:rra:read_bit:                                                         rla:rla:rla:ret
redbt4  xor a:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:                                        rla:rla:rla:rla:ret
redbt5  xor a:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:                           rra:rra:rra:rra:ret
redbt6  xor a:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:                  rra:rra:rra:ret
redbt7  xor a:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:         rra:rra:ret
redbt8        read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:read_bit:rra:ret

;### REDBTN -> reads one bit from the input after end-of-byte
;### Input      IX=read pointer
;### Output     CF=bit, C,IX updated
;### Destroyed  A
redbtn  ld a,(ix+0)
        db #dd:inc l
        call z,redbyt0
        scf
        rra
        ld c,a
        ret


;==============================================================================
;### WRITER ROUTINES ##########################################################
;==============================================================================

wrtvarpoi   dw 0

;### WRTINI -> inits writer
;### Input      HL=writer buffer (256 bytes aligned)
wrtini  ld (wrtvarpoi),hl
        ld a,h
        ld (wrtbyt2+1),a
        ld (wrtcop1+1),a
        ld (wrtcop7+1),a
        ld (wrtcop9+2),a
        add WRT_BUFSIZ/256
        ld (wrtbyt1+1),a
        ld (wrtcop6+1),a
        ld (wrtcopb+2),a
        sub 3
        ld (wrtcop2+1),a
        ret

;### WRTLOD -> loads writer
wrtlod  ld hl,(wrtvarpoi)
        ret

;### WRTSTO -> stores writer
wrtsto  ld (wrtvarpoi),hl
        ret

;### WRTBYT -> writes one byte to the output
;### Input      A=byte, HL=write pointer
;### Output     HL updated
;### JumpsTo    alptre_lit
;### Destroyed  AF,DE,IY
wrtbyt  ld (hl),a
        inc l
        jp nz,alptre_lit
wrtbyt0 inc h
        ld a,h
wrtbyt1 cp 0
        jp nz,alptre_lit
        push hl
        call wrtout             ;save buffer
        pop hl
wrtbyt2 ld h,0
        jp alptre_lit

;### WRTCOP -> copies data from dictionary and writes it to output
;### Input      IY=length (3-258), DE=-distance
;### Output     HL updated
;### JumpsTo    alptre_lit
;### Destroyed  AF,DE,IY
wrtcop  push bc
        ex de,hl                ;de=destination
        add hl,de               ;hl=source
        db #fd:ld c,l           ;bc=length
        db #fd:ld b,h
        ld a,h                  ;check, if source and destination doesn't cross buffer border
        jr nc,wrtcop3
wrtcop1 cp 0
        jr c,wrtcop3
wrtcop2 ld a,0
        cp h
        jp c,wrtcop4
        cp d
        jp c,wrtcop4
wrtcopf ldi                             ;** "fast" copy
        ldi
        ldir
        ex de,hl                ;hl updated
        pop bc
        jp alptre_lit
wrtcop3 add WRT_BUFSIZ/256      ;source starts before buffer border
        ld h,a
        jr wrtcop2

wrtcopc ld a,64             ;2          ;** "mass" copy (faster for >36 bytes)
        sub c               ;1
        and 63              ;2
        add a               ;1
        ld (wrtcopd+1),a    ;4
wrtcopd jr wrtcopd          ;3
wrtcope ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        jp pe,wrtcope       ;3
        db #21:ldi          ;3
        ld (wrtcopf),hl     ;5 + 9 + 3 -> 36
        ex de,hl
        pop bc
        jp alptre_lit

;hl=source, de=destination, bc=length
wrtcop4 push bc                         ;** "slow" copy (take care about borders)
        push hl
        push de
        call wrtcopb
        pop hl
        push hl
        call wrtcopb
        pop de
        pop hl
        ld a,c
        or b
        jr z,wrtcop5
        push bc
        ldir
        pop bc
wrtcop5 ex (sp),hl
        or a
        sbc hl,bc
        ld c,l
        ld b,h
        pop hl
wrtcop6 ld a,0
        cp h
        jr nz,wrtcop8
wrtcop7 ld h,0
wrtcop8 cp d
        jr nz,wrtcopa
        push hl
        ex de,hl
        call wrtout         ;save buffer
        pop hl
wrtcop9 ld de,0
wrtcopa ld a,c
        or b
        jr nz,wrtcop4
        ex de,hl
        pop bc
        jp alptre_lit
;hl=pointer, bc=length -> bc=min(length, bufend+1-pointer)
wrtcopb ld de,0
        add hl,bc           ;hl=pointer+length
        sbc hl,de           ;hl=pointer+length-(bufend+1)
        ret c               ;hl is still <= bufend+1 -> leave bc untouched
        ret z
        ex de,hl            ;de=which is too much
        ld l,c
        ld h,b
        sbc hl,de           ;substract from bc
        ld c,l
        ld b,h              ;bc=bufend+1-pointer (=length-(pointer+length-(bufend+1)))
        ret

;### WRTFLS -> flushes write buffer (only after finishing decompression)
wrtfls  call wrtlod
        jp wrtout

;### WRTOUT -> saves data of the write buffer
;### Input      HL=pointer behind data
;### Destroyed  AF,HL,IY
wrtout  push bc
        push ix
        ld bc,buf_write
        inc b
        or a
        sbc hl,bc
        jr z,wrtout0
        ld c,l
        ld b,h
        push bc
        call outsav
        pop bc
        ld a,(c32flg)
        or a
        jr z,wrtout0
        call c32clc
wrtout0 pop ix
        pop bc
        ret


;==============================================================================
;### ALPHABET ROUTINES ########################################################
;==============================================================================

ALP_MAX_CODLEN equ 15

macro   alp_branch
        srl c
        call z,redbtn
        jp nc,0
        jp 0
endm
ALP_BRANCH_ZERO equ 6
ALP_BRANCH_ONE  equ 9
ALP_BRANCH_SIZE equ 11


;Literal/Length/Header Tree
alptre_hed
alptre_lit  ;288 branches
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
db 0    ;overflow control

;Distance Tree
alptre_dst  ; 32 branches
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch:alp_branch
db 0    ;overflow control


;### ALPGEN -> generates alphabet
;### Input      HL=length table, DE=symbol table, BC=number of entries, IX=branch
;### Output     DE=points behind length table
;### Destroyed  AF,BC,HL,IX,IY
alpgenbuf   ds ALP_MAX_CODLEN*2+2   ;buffer for length counts/next codes

alpgen  push hl
        push de
        push bc
        ld hl,alpgenbuf+2           ;clear length counts
        ld de,alpgenbuf+2+1
        ld (hl),0
        ld bc,ALP_MAX_CODLEN*2-1
        ldir
        pop bc
        pop de
        pop hl
        push hl
        push de
        push bc
        ld de,alpgenbuf
        call alpcnt
        call alpnxt
        pop bc
        pop hl
        pop de
        jr alptre

;### ALPTRE -> builds executable alphabet tree
;### Input      HL=symbol table, DE=length table, BC=number of entries, (alpgenbuf)=next codes, IX=branch
;### Output     DE=points behind length table
;### Destroyed  AF,BC,HL,IX,IY
alptrebrn   dw 0
alptre  ld (alptrebrn),ix
        ld iy,(alptrebrn)               ;IY=last branch
        ld (iy+ALP_BRANCH_ZERO+0),0     ;reset first branch
        ld (iy+ALP_BRANCH_ZERO+1),0
        ld (iy+ALP_BRANCH_ONE+0),0
        ld (iy+ALP_BRANCH_ONE+1),0
        bc_counter
alptre1 ld a,(de)           ;A=code length
        inc de
        or a
        jr z,alptre0
        push bc
        push de
        ld e,(hl)           ;DE=symbol callback
        inc hl
        ld d,(hl)
        inc hl
        push hl
        ld c,a
        ld b,0
        sla c
        ld hl,alpgenbuf-2
        add hl,bc
        ld c,(hl)
        inc hl
        ld b,(hl)           ;BC=next code
        inc bc
        ld (hl),b
        dec hl
        ld (hl),c           ;increase next code
        dec bc
        ld l,c
        ld h,b
        ld b,a
        ld a,16
        sub b
alptre2 add hl,hl
        dec a
        jr nz,alptre2
        ld ix,(alptrebrn)
        call alpbld         ;build treepart
        pop hl
        pop de
        pop bc
        djnz alptre1
        dec c
        jr nz,alptre1
        ret
alptre0 inc hl
        inc hl
        djnz alptre1
        dec c
        jr nz,alptre1
        ret

;### ALPBLD -> build treepart for one code
;### Input      HL=code (left aligned), DE=symbol callback, B=bit length, IX=first branch, IY=last branch
;### Output     IY=new last branch
;### Destroyed  AF,BC,DE,HL,IX
alpbld  push de
alpbld1 add hl,hl                       ;*** next bit
        jr c,alpbld4
        ld e,(ix+ALP_BRANCH_ZERO+0)     ;*** BIT=0
        ld d,(ix+ALP_BRANCH_ZERO+1)
        ld a,d
        or e
        djnz alpbld2
        jp nz,iflerr
        pop de
        ld (ix+ALP_BRANCH_ZERO+0),e
        ld (ix+ALP_BRANCH_ZERO+1),d
        ret
alpbld2 jr nz,alpbld3
        ld de,ALP_BRANCH_SIZE           ;add new branch
        add iy,de
        bit 7,(iy+0)
        jp z,iflerr
        ld (iy+ALP_BRANCH_ZERO+0),0
        ld (iy+ALP_BRANCH_ZERO+1),0
        ld (iy+ALP_BRANCH_ONE+0),0
        ld (iy+ALP_BRANCH_ONE+1),0
        db #fd:ld e,l
        db #fd:ld d,h
        ld (ix+ALP_BRANCH_ZERO+0),e
        ld (ix+ALP_BRANCH_ZERO+1),d
alpbld3 db #dd:ld l,e
        db #dd:ld h,d
        jr alpbld1
alpbld4 ld e,(ix+ALP_BRANCH_ONE+0)      ;*** BIT=1
        ld d,(ix+ALP_BRANCH_ONE+1)
        ld a,d
        or e
        djnz alpbld5
        jp nz,iflerr
        pop de
        ld (ix+ALP_BRANCH_ONE+0),e
        ld (ix+ALP_BRANCH_ONE+1),d
        ret
alpbld5 jr nz,alpbld6
        ld de,ALP_BRANCH_SIZE           ;add new branch
        add iy,de
        bit 7,(iy+0)
        jp z,iflerr
        ld (iy+ALP_BRANCH_ZERO+0),0
        ld (iy+ALP_BRANCH_ZERO+1),0
        ld (iy+ALP_BRANCH_ONE+0),0
        ld (iy+ALP_BRANCH_ONE+1),0
        db #fd:ld e,l
        db #fd:ld d,h
        ld (ix+ALP_BRANCH_ONE+0),e
        ld (ix+ALP_BRANCH_ONE+1),d
alpbld6 db #dd:ld l,e
        db #dd:ld h,d
        jp alpbld1

;### ALPCNT -> calculates length counts for an alphabet
;### Input      HL=length table, DE=count values-2, BC=number of entries
;### Destroyed  AF,BC,HL
alpcnt  bc_counter
alpcnt1 ld a,(hl)
        cp ALP_MAX_CODLEN+1
        jp nc,iflerr
        add a
        jr z,alpcnt3
        push hl
        ld l,a
        ld h,0
        add hl,de
        inc (hl)
        jr z,alpcnt4
alpcnt2 pop hl
alpcnt3 inc hl
        djnz alpcnt1
        dec c
        jr nz,alpcnt1
        ret
alpcnt4 inc hl
        inc (hl)
        jr alpcnt2

;### ALPNXT -> converst length counts into next codes for an alphabet
;### Input      DE=next values (=count values-2)
;### Destroyed  AF,BC,DE,HL
alpnxt  ld a,ALP_MAX_CODLEN-1
        ld hl,0
alpnxt1 ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d
        inc hl
        ld c,(hl)
        inc hl
        ld b,(hl)
        dec hl
        ex de,hl
        add hl,bc
        add hl,hl       ;##!!## ??????????
        dec a
        jr nz,alpnxt1
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d
        ret


;==============================================================================
;### FIXED ALPHABET ROUTINES ##################################################
;==============================================================================

fixlitlen   ds 144,8    ;000-143 -> 8
            ds 112,9    ;144-255 -> 9
            ds  24,7    ;256-279 -> 7
            ds   8,8    ;280-287 -> 8

fixdstlen   ds  32,5    ;000-031 -> 5

;### FIXCON -> generates a fixed alphabet
fixgen  ld hl,fixlitlen
        ld de,ifllitsym
        ld bc,288
        ld ix,alptre_lit
        call alpgen             ;generates literal/length alphabet tree
        ld hl,fixdstlen
        ld de,ifldstsym
        ld bc,32
        ld ix,alptre_dst
        jp alpgen               ;generates distance alphabet tree


;==============================================================================
;### DYNAMIC ALPHABET ROUTINES ################################################
;==============================================================================

DYN_LIT_MAX equ 286
DYN_DST_MAX equ 30
DYN_HED_MAX equ 19

dynhedlen   ds DYN_HED_MAX
dyncodlen   ds DYN_LIT_MAX+DYN_DST_MAX

;### DYNGEN -> generates a dynamic alphabet
dyngen  ld hl,dynhedlen         ;clear code lengths
        ld de,dynhedlen+1
        ld bc,DYN_HED_MAX+DYN_LIT_MAX+DYN_DST_MAX-1
        ld (hl),0
        ldir
        call redlod             ;prepare reader
        call dynlen             ;get lengths
        call dynhed             ;read header code lengths
        call redsto             ;finish reader
        ld hl,dynhedlen
        ld de,dynhedsym
        ld bc,DYN_HED_MAX
        ld ix,alptre_hed
        call alpgen             ;generates header alphabet tree
        ld hl,(dynlenlit)
        ld a,l
        add h
        ld e,a
        ld d,1+1
        ld hl,dyncodlen
        call redlod
        call alptre_hed         ;read literal/length and distance code lengths
        call redsto
        ld hl,dyncodlen
        ld de,ifllitsym
        ld a,(dynlenlit)
        ld c,a
        ld b,1
        ld ix,alptre_lit
        call alpgen             ;generates literal/length alphabet tree
        ex de,hl
        ld de,ifldstsym
        ld a,(dynlendst)
        ld c,a
        ld b,0
        ld ix,alptre_dst
        jp alpgen               ;generates distance alphabet tree

;### DYNLEN -> get code length of literal/length, distance and header
;### Output     A=header code length
dynlenlit   db 0    ;(+256=real length)
dynlendst   db 0

dynlen  call redbt5
        inc a
        cp DYN_LIT_MAX-256+1
        jp nc,iflerr
        ld (dynlenlit),a
        call redbt5
        inc a
        cp DYN_DST_MAX+1
        jp nc,iflerr
        ld (dynlendst),a
        call redbt4
        add 4
        cp DYN_HED_MAX+1
        ret c
        jp iflerr

;### DYNHED -> reads header code lengths
;### Input      A=header code length
dynhedord   db 16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15

dynhed  ld e,a
        ld hl,dynhedord
        ld iy,dynhedlen
dynhed1 ld a,(hl)
	    inc hl
        ld (dynhed2+2),a
        call redbt3
dynhed2 ld (iy+0),a
        dec e
	    jr nz,dynhed1
	    ret

;### DYNWLNxx -> write length
macro   dynwlnmac byte
        ld (hl),byte
        inc hl
        dec e
        jp nz,alptre_hed
        dec d
        jp nz,alptre_hed
        ret
endm
dynwln00 dynwlnmac 00: dynwln01 dynwlnmac 01: dynwln02 dynwlnmac 02: dynwln03 dynwlnmac 03
dynwln04 dynwlnmac 04: dynwln05 dynwlnmac 05: dynwln06 dynwlnmac 06: dynwln07 dynwlnmac 07
dynwln08 dynwlnmac 08: dynwln09 dynwlnmac 09: dynwln10 dynwlnmac 10: dynwln11 dynwlnmac 11
dynwln12 dynwlnmac 12: dynwln13 dynwlnmac 13: dynwln14 dynwlnmac 14: dynwln15 dynwlnmac 15

;### DYNCOP -> copy length
dyncop  call redbt2
        add 3
        ld b,a
        dec hl
        ld a,(hl)
        inc hl
        jr dynfll

;### DYNZ03 -> fill 3-10 zero bytes
dynz03  call redbt3
        add 3
        ld b,a
        xor a
        jr dynfll

;### DYNZ11 -> fill 11-138 zero bytes
dynz11  call redbt7
        add 11
        ld b,a
        xor a
        jr dynfll

;### DYNFLL -> fills bytes
dynfll0 dec b
        jp z,alptre_hed
dynfll  ld (hl),a
        inc hl
        dec e
        jr nz,dynfll0
        dec d
        jr nz,dynfll0
        ret

dynhedsym
dw dynwln00,dynwln01,dynwln02,dynwln03,dynwln04,dynwln05,dynwln06,dynwln07
dw dynwln08,dynwln09,dynwln10,dynwln11,dynwln12,dynwln13,dynwln14,dynwln15
dw dyncop
dw dynz03,dynz11
dw iflerr


;==============================================================================
;### INFLATE ROUTINES #########################################################
;==============================================================================

;register usage during deflating
;   ix=readpointer
;   c =readbits
;   hl=writepointer
;   iy=length
;   de=distance
;   b =temp

;### IFLMAI -> decompresses a deflated data stream
iflmai  call inplod
iflmai1 call redlod
        call redbt3
        call redsto
        push af
        srl a
        jr z,iflunc
        dec a
        jr z,iflfix
        dec a
        jr z,ifldyn
        jp iflerr
iflmai0 pop af
        and 1
        jr z,iflmai1
        jp wrtfls
iflerr  ld a,7
        jp prgendf

;### IFLUNC -> writes one uncompressed block of the deflated data stream
;### JumpsTo    IFLMAI0
iflunc  call redlod
        call redbyt
        ld e,a
        call redbyt
        ld d,a
        call redbyt
        ld l,a
        call redbyt
        ld h,a
        scf
        adc hl,de
        jp nz,iflerr
        ld a,e
        or d
        jr nz,iflmai0
        ld b,e
        dec de
        inc d
        ld c,d
        call wrtlod
iflunc1 call redbyt
        call wrtbyt
        djnz iflunc1
        dec c
        jr nz,iflunc1
        call redaln
        call redsto
        call wrtsto
        jr iflmai0

;### IFLFIX -> writes one deflated block with fixed huffman-alphabet
;### JumpsTo    IFLMAI0
iflfix  call fixgen
        jr ifldyn0

;### IFLDYN -> writes one deflated block with dynamic huffman-alphabet
;### JumpsTo    IFLMAI0
ifldyn  call dyngen
ifldyn0 call redlod
        call wrtlod
        call alptre_lit
        call redsto
        call wrtsto
        jr iflmai0

;### IFLWLTxxx -> write literal
;### A=value -> jp WRTBYT -> jp ALPTRE_LIT
macro   iflwltmac byte
        ld (hl),byte
        inc l
        jp nz,alptre_lit
        jp wrtbyt0
endm
iflwlt000 iflwltmac 000: iflwlt001 iflwltmac 001: iflwlt002 iflwltmac 002: iflwlt003 iflwltmac 003: iflwlt004 iflwltmac 004: iflwlt005 iflwltmac 005: iflwlt006 iflwltmac 006: iflwlt007 iflwltmac 007: iflwlt008 iflwltmac 008: iflwlt009 iflwltmac 009
iflwlt010 iflwltmac 010: iflwlt011 iflwltmac 011: iflwlt012 iflwltmac 012: iflwlt013 iflwltmac 013: iflwlt014 iflwltmac 014: iflwlt015 iflwltmac 015: iflwlt016 iflwltmac 016: iflwlt017 iflwltmac 017: iflwlt018 iflwltmac 018: iflwlt019 iflwltmac 019
iflwlt020 iflwltmac 020: iflwlt021 iflwltmac 021: iflwlt022 iflwltmac 022: iflwlt023 iflwltmac 023: iflwlt024 iflwltmac 024: iflwlt025 iflwltmac 025: iflwlt026 iflwltmac 026: iflwlt027 iflwltmac 027: iflwlt028 iflwltmac 028: iflwlt029 iflwltmac 029
iflwlt030 iflwltmac 030: iflwlt031 iflwltmac 031: iflwlt032 iflwltmac 032: iflwlt033 iflwltmac 033: iflwlt034 iflwltmac 034: iflwlt035 iflwltmac 035: iflwlt036 iflwltmac 036: iflwlt037 iflwltmac 037: iflwlt038 iflwltmac 038: iflwlt039 iflwltmac 039
iflwlt040 iflwltmac 040: iflwlt041 iflwltmac 041: iflwlt042 iflwltmac 042: iflwlt043 iflwltmac 043: iflwlt044 iflwltmac 044: iflwlt045 iflwltmac 045: iflwlt046 iflwltmac 046: iflwlt047 iflwltmac 047: iflwlt048 iflwltmac 048: iflwlt049 iflwltmac 049
iflwlt050 iflwltmac 050: iflwlt051 iflwltmac 051: iflwlt052 iflwltmac 052: iflwlt053 iflwltmac 053: iflwlt054 iflwltmac 054: iflwlt055 iflwltmac 055: iflwlt056 iflwltmac 056: iflwlt057 iflwltmac 057: iflwlt058 iflwltmac 058: iflwlt059 iflwltmac 059
iflwlt060 iflwltmac 060: iflwlt061 iflwltmac 061: iflwlt062 iflwltmac 062: iflwlt063 iflwltmac 063: iflwlt064 iflwltmac 064: iflwlt065 iflwltmac 065: iflwlt066 iflwltmac 066: iflwlt067 iflwltmac 067: iflwlt068 iflwltmac 068: iflwlt069 iflwltmac 069
iflwlt070 iflwltmac 070: iflwlt071 iflwltmac 071: iflwlt072 iflwltmac 072: iflwlt073 iflwltmac 073: iflwlt074 iflwltmac 074: iflwlt075 iflwltmac 075: iflwlt076 iflwltmac 076: iflwlt077 iflwltmac 077: iflwlt078 iflwltmac 078: iflwlt079 iflwltmac 079
iflwlt080 iflwltmac 080: iflwlt081 iflwltmac 081: iflwlt082 iflwltmac 082: iflwlt083 iflwltmac 083: iflwlt084 iflwltmac 084: iflwlt085 iflwltmac 085: iflwlt086 iflwltmac 086: iflwlt087 iflwltmac 087: iflwlt088 iflwltmac 088: iflwlt089 iflwltmac 089
iflwlt090 iflwltmac 090: iflwlt091 iflwltmac 091: iflwlt092 iflwltmac 092: iflwlt093 iflwltmac 093: iflwlt094 iflwltmac 094: iflwlt095 iflwltmac 095: iflwlt096 iflwltmac 096: iflwlt097 iflwltmac 097: iflwlt098 iflwltmac 098: iflwlt099 iflwltmac 099
iflwlt100 iflwltmac 100: iflwlt101 iflwltmac 101: iflwlt102 iflwltmac 102: iflwlt103 iflwltmac 103: iflwlt104 iflwltmac 104: iflwlt105 iflwltmac 105: iflwlt106 iflwltmac 106: iflwlt107 iflwltmac 107: iflwlt108 iflwltmac 108: iflwlt109 iflwltmac 109
iflwlt110 iflwltmac 110: iflwlt111 iflwltmac 111: iflwlt112 iflwltmac 112: iflwlt113 iflwltmac 113: iflwlt114 iflwltmac 114: iflwlt115 iflwltmac 115: iflwlt116 iflwltmac 116: iflwlt117 iflwltmac 117: iflwlt118 iflwltmac 118: iflwlt119 iflwltmac 119
iflwlt120 iflwltmac 120: iflwlt121 iflwltmac 121: iflwlt122 iflwltmac 122: iflwlt123 iflwltmac 123: iflwlt124 iflwltmac 124: iflwlt125 iflwltmac 125: iflwlt126 iflwltmac 126: iflwlt127 iflwltmac 127: iflwlt128 iflwltmac 128: iflwlt129 iflwltmac 129
iflwlt130 iflwltmac 130: iflwlt131 iflwltmac 131: iflwlt132 iflwltmac 132: iflwlt133 iflwltmac 133: iflwlt134 iflwltmac 134: iflwlt135 iflwltmac 135: iflwlt136 iflwltmac 136: iflwlt137 iflwltmac 137: iflwlt138 iflwltmac 138: iflwlt139 iflwltmac 139
iflwlt140 iflwltmac 140: iflwlt141 iflwltmac 141: iflwlt142 iflwltmac 142: iflwlt143 iflwltmac 143: iflwlt144 iflwltmac 144: iflwlt145 iflwltmac 145: iflwlt146 iflwltmac 146: iflwlt147 iflwltmac 147: iflwlt148 iflwltmac 148: iflwlt149 iflwltmac 149
iflwlt150 iflwltmac 150: iflwlt151 iflwltmac 151: iflwlt152 iflwltmac 152: iflwlt153 iflwltmac 153: iflwlt154 iflwltmac 154: iflwlt155 iflwltmac 155: iflwlt156 iflwltmac 156: iflwlt157 iflwltmac 157: iflwlt158 iflwltmac 158: iflwlt159 iflwltmac 159
iflwlt160 iflwltmac 160: iflwlt161 iflwltmac 161: iflwlt162 iflwltmac 162: iflwlt163 iflwltmac 163: iflwlt164 iflwltmac 164: iflwlt165 iflwltmac 165: iflwlt166 iflwltmac 166: iflwlt167 iflwltmac 167: iflwlt168 iflwltmac 168: iflwlt169 iflwltmac 169
iflwlt170 iflwltmac 170: iflwlt171 iflwltmac 171: iflwlt172 iflwltmac 172: iflwlt173 iflwltmac 173: iflwlt174 iflwltmac 174: iflwlt175 iflwltmac 175: iflwlt176 iflwltmac 176: iflwlt177 iflwltmac 177: iflwlt178 iflwltmac 178: iflwlt179 iflwltmac 179
iflwlt180 iflwltmac 180: iflwlt181 iflwltmac 181: iflwlt182 iflwltmac 182: iflwlt183 iflwltmac 183: iflwlt184 iflwltmac 184: iflwlt185 iflwltmac 185: iflwlt186 iflwltmac 186: iflwlt187 iflwltmac 187: iflwlt188 iflwltmac 188: iflwlt189 iflwltmac 189
iflwlt190 iflwltmac 190: iflwlt191 iflwltmac 191: iflwlt192 iflwltmac 192: iflwlt193 iflwltmac 193: iflwlt194 iflwltmac 194: iflwlt195 iflwltmac 195: iflwlt196 iflwltmac 196: iflwlt197 iflwltmac 197: iflwlt198 iflwltmac 198: iflwlt199 iflwltmac 199
iflwlt200 iflwltmac 200: iflwlt201 iflwltmac 201: iflwlt202 iflwltmac 202: iflwlt203 iflwltmac 203: iflwlt204 iflwltmac 204: iflwlt205 iflwltmac 205: iflwlt206 iflwltmac 206: iflwlt207 iflwltmac 207: iflwlt208 iflwltmac 208: iflwlt209 iflwltmac 209
iflwlt210 iflwltmac 210: iflwlt211 iflwltmac 211: iflwlt212 iflwltmac 212: iflwlt213 iflwltmac 213: iflwlt214 iflwltmac 214: iflwlt215 iflwltmac 215: iflwlt216 iflwltmac 216: iflwlt217 iflwltmac 217: iflwlt218 iflwltmac 218: iflwlt219 iflwltmac 219
iflwlt220 iflwltmac 220: iflwlt221 iflwltmac 221: iflwlt222 iflwltmac 222: iflwlt223 iflwltmac 223: iflwlt224 iflwltmac 224: iflwlt225 iflwltmac 225: iflwlt226 iflwltmac 226: iflwlt227 iflwltmac 227: iflwlt228 iflwltmac 228: iflwlt229 iflwltmac 229
iflwlt230 iflwltmac 230: iflwlt231 iflwltmac 231: iflwlt232 iflwltmac 232: iflwlt233 iflwltmac 233: iflwlt234 iflwltmac 234: iflwlt235 iflwltmac 235: iflwlt236 iflwltmac 236: iflwlt237 iflwltmac 237: iflwlt238 iflwltmac 238: iflwlt239 iflwltmac 239
iflwlt240 iflwltmac 240: iflwlt241 iflwltmac 241: iflwlt242 iflwltmac 242: iflwlt243 iflwltmac 243: iflwlt244 iflwltmac 244: iflwlt245 iflwltmac 245: iflwlt246 iflwltmac 246: iflwlt247 iflwltmac 247: iflwlt248 iflwltmac 248: iflwlt249 iflwltmac 249
iflwlt250 iflwltmac 250: iflwlt251 iflwltmac 251: iflwlt252 iflwltmac 252: iflwlt253 iflwltmac 253: iflwlt254 iflwltmac 254: iflwlt255 iflwltmac 255

;### IFLBLKEND -> end of block
;### -> RET
iflblkend   ret

;### IFLCLNxx -> copy length
;### IY=length -> jp ALPTRE_DST
macro   iflclnmc1 length
        ld iy,length
        jp alptre_dst
endm
macro   iflclnmc2 read_bits,length_difference
        call read_bits
        add length_difference
        db #fd:ld l,a
        db #fd:ld h,0
        jp alptre_dst
endm
macro   iflclnmc3 read_bits,length_difference
        call read_bits
        add length_difference
        db #fd:ld l,a
        db #fd:ld h,0
        ld de,wrtcopc-wrtcopf-2*256+#18 ;3
        ld (wrtcopf),de                 ;6
        jp alptre_dst
endm
iflcln00 iflclnmc1        003
iflcln01 iflclnmc1        004
iflcln02 iflclnmc1        005
iflcln03 iflclnmc1        006
iflcln04 iflclnmc1        007
iflcln05 iflclnmc1        008
iflcln06 iflclnmc1        009
iflcln07 iflclnmc1        010
iflcln08 iflclnmc2 redbt1,011
iflcln09 iflclnmc2 redbt1,013
iflcln10 iflclnmc2 redbt1,015
iflcln11 iflclnmc2 redbt1,017
iflcln12 iflclnmc2 redbt2,019
iflcln13 iflclnmc2 redbt2,023
iflcln14 iflclnmc2 redbt2,027
iflcln15 iflclnmc2 redbt2,031
iflcln16 iflclnmc3 redbt3,035
iflcln17 iflclnmc3 redbt3,043
iflcln18 iflclnmc3 redbt3,051
iflcln19 iflclnmc3 redbt3,059
iflcln20 iflclnmc3 redbt4,067
iflcln21 iflclnmc3 redbt4,083
iflcln22 iflclnmc3 redbt4,099
iflcln23 iflclnmc3 redbt4,115
iflcln24 iflclnmc3 redbt5,131
iflcln25 iflclnmc3 redbt5,163
iflcln26 iflclnmc3 redbt5,195
iflcln27  call redbt5:add 227:db #fd:ld l,a:ld a,0:adc a:db #fd:ld h,a:ld de,wrtcopc-wrtcopf-2*256+#18:ld (wrtcopf),de:jp alptre_dst
iflcln28            ld iy,258:                                         ld de,wrtcopc-wrtcopf-2*256+#18:ld (wrtcopf),de:jp alptre_dst

;### IFLCDSxx -> copy distance
;### DE=-distance -> jp WRTCOP -> jp ALPTRE_LIT
macro   iflcdsmc0 distance
        ld de,-distance
        jp wrtcop
endm
macro   iflcdsmc1 read_bits,distance_difference
        call read_bits
        xor -distance_difference
        ld e,a
        ld d,-1
        jp wrtcop
endm
macro   iflcdsmc2 read_bits,distance_difference
        call read_bits
        cpl
        ld e,a
        ld d,-distance_difference/256
        jp wrtcop
endm
macro   iflcdsmc3 read_bits0,read_bits1,distance_difference
        call read_bits0
        cpl
        ld e,a
        call read_bits1
        xor -distance_difference/256
        ld d,a
        jp wrtcop
endm
iflcds00 iflcdsmc0               00001
iflcds01 iflcdsmc0               00002
iflcds02 iflcdsmc0               00003
iflcds03 iflcdsmc0               00004
iflcds04 iflcdsmc1 redbt1,       00005
iflcds05 iflcdsmc1 redbt1,       00007
iflcds06 iflcdsmc1 redbt2,       00009
iflcds07 iflcdsmc1 redbt2,       00013
iflcds08 iflcdsmc1 redbt3,       00017
iflcds09 iflcdsmc1 redbt3,       00025
iflcds10 iflcdsmc1 redbt4,       00033
iflcds11 iflcdsmc1 redbt4,       00049
iflcds12 iflcdsmc1 redbt5,       00065
iflcds13 iflcdsmc1 redbt5,       00097
iflcds14 iflcdsmc1 redbt6,       00129
iflcds15 iflcdsmc1 redbt6,       00193
iflcds16 iflcdsmc2 redbt7,       00257
iflcds17      call redbt7:  xor -00385:ld e,a:ld d,-385/256:jp wrtcop
iflcds18 iflcdsmc2 redbt8,       00513
iflcds19 iflcdsmc2 redbt8,       00769
iflcds20 iflcdsmc3 redbt8,redbt1,01025
iflcds21 iflcdsmc3 redbt8,redbt1,01537
iflcds22 iflcdsmc3 redbt8,redbt2,02049
iflcds23 iflcdsmc3 redbt8,redbt2,03073
iflcds24 iflcdsmc3 redbt8,redbt3,04097
iflcds25 iflcdsmc3 redbt8,redbt3,06145
iflcds26 iflcdsmc3 redbt8,redbt4,08193
iflcds27 iflcdsmc3 redbt8,redbt4,12289
iflcds28 iflcdsmc3 redbt8,redbt5,16385
iflcds29 iflcdsmc3 redbt8,redbt5,24577


;### Symbol tables
ifllitsym   ;literal/length
dw iflwlt000,iflwlt001,iflwlt002,iflwlt003,iflwlt004,iflwlt005,iflwlt006,iflwlt007,iflwlt008,iflwlt009  ;000-255 -> write literal
dw iflwlt010,iflwlt011,iflwlt012,iflwlt013,iflwlt014,iflwlt015,iflwlt016,iflwlt017,iflwlt018,iflwlt019
dw iflwlt020,iflwlt021,iflwlt022,iflwlt023,iflwlt024,iflwlt025,iflwlt026,iflwlt027,iflwlt028,iflwlt029
dw iflwlt030,iflwlt031,iflwlt032,iflwlt033,iflwlt034,iflwlt035,iflwlt036,iflwlt037,iflwlt038,iflwlt039
dw iflwlt040,iflwlt041,iflwlt042,iflwlt043,iflwlt044,iflwlt045,iflwlt046,iflwlt047,iflwlt048,iflwlt049
dw iflwlt050,iflwlt051,iflwlt052,iflwlt053,iflwlt054,iflwlt055,iflwlt056,iflwlt057,iflwlt058,iflwlt059
dw iflwlt060,iflwlt061,iflwlt062,iflwlt063,iflwlt064,iflwlt065,iflwlt066,iflwlt067,iflwlt068,iflwlt069
dw iflwlt070,iflwlt071,iflwlt072,iflwlt073,iflwlt074,iflwlt075,iflwlt076,iflwlt077,iflwlt078,iflwlt079
dw iflwlt080,iflwlt081,iflwlt082,iflwlt083,iflwlt084,iflwlt085,iflwlt086,iflwlt087,iflwlt088,iflwlt089
dw iflwlt090,iflwlt091,iflwlt092,iflwlt093,iflwlt094,iflwlt095,iflwlt096,iflwlt097,iflwlt098,iflwlt099
dw iflwlt100,iflwlt101,iflwlt102,iflwlt103,iflwlt104,iflwlt105,iflwlt106,iflwlt107,iflwlt108,iflwlt109
dw iflwlt110,iflwlt111,iflwlt112,iflwlt113,iflwlt114,iflwlt115,iflwlt116,iflwlt117,iflwlt118,iflwlt119
dw iflwlt120,iflwlt121,iflwlt122,iflwlt123,iflwlt124,iflwlt125,iflwlt126,iflwlt127,iflwlt128,iflwlt129
dw iflwlt130,iflwlt131,iflwlt132,iflwlt133,iflwlt134,iflwlt135,iflwlt136,iflwlt137,iflwlt138,iflwlt139
dw iflwlt140,iflwlt141,iflwlt142,iflwlt143,iflwlt144,iflwlt145,iflwlt146,iflwlt147,iflwlt148,iflwlt149
dw iflwlt150,iflwlt151,iflwlt152,iflwlt153,iflwlt154,iflwlt155,iflwlt156,iflwlt157,iflwlt158,iflwlt159
dw iflwlt160,iflwlt161,iflwlt162,iflwlt163,iflwlt164,iflwlt165,iflwlt166,iflwlt167,iflwlt168,iflwlt169
dw iflwlt170,iflwlt171,iflwlt172,iflwlt173,iflwlt174,iflwlt175,iflwlt176,iflwlt177,iflwlt178,iflwlt179
dw iflwlt180,iflwlt181,iflwlt182,iflwlt183,iflwlt184,iflwlt185,iflwlt186,iflwlt187,iflwlt188,iflwlt189
dw iflwlt190,iflwlt191,iflwlt192,iflwlt193,iflwlt194,iflwlt195,iflwlt196,iflwlt197,iflwlt198,iflwlt199
dw iflwlt200,iflwlt201,iflwlt202,iflwlt203,iflwlt204,iflwlt205,iflwlt206,iflwlt207,iflwlt208,iflwlt209
dw iflwlt210,iflwlt211,iflwlt212,iflwlt213,iflwlt214,iflwlt215,iflwlt216,iflwlt217,iflwlt218,iflwlt219
dw iflwlt220,iflwlt221,iflwlt222,iflwlt223,iflwlt224,iflwlt225,iflwlt226,iflwlt227,iflwlt228,iflwlt229
dw iflwlt230,iflwlt231,iflwlt232,iflwlt233,iflwlt234,iflwlt235,iflwlt236,iflwlt237,iflwlt238,iflwlt239
dw iflwlt240,iflwlt241,iflwlt242,iflwlt243,iflwlt244,iflwlt245,iflwlt246,iflwlt247,iflwlt248,iflwlt249
dw iflwlt250,iflwlt251,iflwlt252,iflwlt253,iflwlt254,iflwlt255
dw iflblkend                                                                                            ;256     -> end of block
dw iflcln00,iflcln01,iflcln02,iflcln03,iflcln04,iflcln05,iflcln06,iflcln07,iflcln08,iflcln09            ;257-285 -> copy length
dw iflcln10,iflcln11,iflcln12,iflcln13,iflcln14,iflcln15,iflcln16,iflcln17,iflcln18,iflcln19
dw iflcln20,iflcln21,iflcln22,iflcln23,iflcln24,iflcln25,iflcln26,iflcln27,iflcln28
dw iflerr,iflerr                                                                                        ;286-287 -> error

ifldstsym   ;distance
dw iflcds00,iflcds01,iflcds02,iflcds03,iflcds04,iflcds05,iflcds06,iflcds07,iflcds08,iflcds09            ;000-029 -> copy distance
dw iflcds10,iflcds11,iflcds12,iflcds13,iflcds14,iflcds15,iflcds16,iflcds17,iflcds18,iflcds19
dw iflcds20,iflcds21,iflcds22,iflcds23,iflcds24,iflcds25,iflcds26,iflcds27,iflcds28,iflcds29
dw iflerr,iflerr                                                                                        ;030-031 -> error


c32flg  db 0
c32val  ds 4

;### C32INI -> inits CRC32 calculation
c32ini  ld hl,-1
        ld (c32val+0),hl
        ld (c32val+2),hl
        ld hl,crc32tab
        ld a,h
        ld (c32clc5+1),a
        ld ix,c32clc0
        ld de,2+19+19
        ld b,16
c32ini1 ld (ix+7+00),h
        ld (ix+7+19),h
        add ix,de
        djnz c32ini1
        ret

;### C32CLC -> updates CRC32 value from current write buffer
;### Input      BC=length
c32clc  ld ix,buf_write
        db #dd:inc h
        ld a,c
        push bc
        ld de,(c32val+0)
        ld bc,(c32val+2)
        and 127
        jr z,c32clc2
        db #fd:ld l,a
c32clc1 ld a,(ix+0)
        inc ix
        xor e
        ld l,a
c32clc5 ld h,000
        ld a,(hl)
        xor d
        ld e,a
        inc h
        ld a,(hl)
        xor c
        ld d,a
        inc h
        ld a,(hl)
        xor b
        ld c,a
        inc h
        ld b,(hl)
        db #fd:dec l
        jr nz,c32clc1
c32clc2 pop hl
        ld a,l
        inc h:dec h
        jr nz,c32clc6
        cp 128
        jp c,c32clc7
c32clc6 rla
        ld a,h
        rla
        db #fd:ld h,a
        push ix:pop hl
        ld (c32clc4+1),sp
c32clc3 db #fd:ld l,4
        di
        ld sp,hl
c32clc0 pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        pop ix:db #dd:ld a,l:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
               db #dd:ld a,h:xor e:ld l,a:ld h,000:ld a,(hl):xor d:ld e,a:inc h:ld a,(hl):xor c:ld d,a:inc h:ld a,(hl):xor b:ld c,a:inc h:ld b,(hl)
        db #fd:dec l
        jp nz,c32clc0
        ld hl,0
        add hl,sp
c32clc4 ld sp,0
        ei
        db #fd:dec h
        jp nz,c32clc3
c32clc7 ld (c32val+0),de
        ld (c32val+2),bc
        ret
