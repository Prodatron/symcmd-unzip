;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                          U N Z I P   for SymShell                          @
;@                                                                            @
;@               (c) 2015 by Prodatron / SymbiosiS (Jörn Mika)                @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo
;- file existiert bereits?


;### PRGPRZ -> Programm-Prozess
prgprz  call SyShell_PARALL     ;get commandline parameters
        push de
        call SyShell_PARSHL     ;fetch shell-specific parameters
        jp c,prgend
        ld hl,txtmsgtit         ;print title text
        call SyShell_STROUT0
        pop de
        inc d:dec d
        ld hl,txterrpar
        jp z,prgende
        ld a,(0*3+SyShell_CmdParas+2)
        dec a
        ld hl,txterrcmd
        jp nz,prgende
        ld hl,(0*3+SyShell_CmdParas+0)
        ld a,(hl)
        call clcucs
        cp "H"
        jr nz,prgprz1
        ld hl,txtmsghlp1        ;help
        call SyShell_STROUT0
        ld hl,txtmsghlp2
        call SyShell_STROUT0
        ld hl,txtmsghlp3
        call SyShell_STROUT0
        jp prgend
prgprz1 ld (prgparcmd),a
        ld a,d
        sub 2
        ld hl,txterrpar
        jp c,prgende
        ld (prgparfil),a

        ld iy,prgparflt         ;validate flags
        call SyShell_PARFLG
        ld (prgparflg),hl
        ex de,hl
        ld hl,txterrswt
        jp c,prgende
        ld a,e
        and 4
        jr z,prgprz3
        ld hl,(2*4+prgparflt+2)     ;priority
        ld a,(hl)
        inc hl
        inc (hl)
        dec (hl)
        ld hl,txterrpri
        jp nz,prgende
        cp "0"
        jp c,prgende
        cp "7"+1
        jp nc,prgende
        sub "0"
        ld (uziprioid),a
prgprz3 ld a,e
        and 2
        ld (c32flg),a               ;crc32
        ld a,e
        and 8
        xor 8
        ld (uzidspflg),a            ;display
        ld a,e
        and 16
        ld (uzitimflg),a            ;timestamp
        ld a,e
        and 1
        ld hl,outfilnul
        jr z,prgprz2
        ld (uzicatflg),a            ;concat
        dec a
        ld (uzitimflg),a
        ld hl,(0*4+prgparflt+2)
prgprz2 ld de,0
        ld bc,outfilpth
        call SyShell_PTHADD     ;create output (file)path
        ld (outfilnam),hl

        ld de,0
        ld hl,(1*3+SyShell_CmdParas+0)
        ld bc,inpfilpth
        call SyShell_PTHADD     ;create input filepath
        ld (inpfilnam),hl
        and 3
        ld hl,txterrarc
        jr nz,prgende

        call uziopn             ;open archive file
        jr c,prgendf

        ld de,(uziprioid)       ;change priority
        ld a,(App_PrcID)
        call SyKernel_MTPRIO

        ld a,(prgparcmd)        ;execute command
        cp "E":jp z,uziexf
        cp "X":jp z,uziexd
        cp "L":jp z,uzilst
        cp "T":jp z,uzitst
        cp "I":jp z,uziinf

        ld hl,txterrcmd
        jr prgende

;### PRGEND -> quit program
prgendf call prgerr
        jr prgend
prgende call SyShell_STROUT0
        ld hl,txterrhlp
prgend0 call SyShell_STROUT0
prgend  call inpclo
        call outclo
        ld e,0
        call SyShell_EXIT       ;tell Shell, that process will quit
        ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend1 rst #30
        jr prgend1
prgerr  add a
        ld l,a
        ld h,0
        ld bc,txterrtab
        add hl,bc
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        jp SyShell_STROUT0


;==============================================================================
;### MAIN ROUTINES ############################################################
;==============================================================================

uzityp  db 0    ;1=gz, 2=zip

;### UZIOPN -> opens an archive file and prepares directory access
;### Output     CF=0 -> ok, (uzityp)=archive type (1=gz, 2=zip)
;###            CF=1 -> error (A=error code)
uziopn  call inpopn             ;open archive file
        ret c
        ld hl,(inpfilnam)       ;check file extension
uziopn1 ld a,(hl)
        inc hl
        or a
        jr z,uziopn2
        cp "."
        jr nz,uziopn1
        ld a,(hl)
        call clcucs
        cp "G"
        jr nz,uziopn2
        inc hl
        ld a,(hl)
        call clcucs
        cp "Z"
        jr nz,uziopn2
        ld a,1
        ld (uzityp),a
        jp gzphed
uziopn2 ld a,2
        ld (uzityp),a
        jp ziphed

;### UZIDIR -> loads next directory entry
uzidir  call cnsctc
        ld a,(uzityp)
        dec a
        jp z,gzpdir
        jp zipdir

;### UZINAM -> checks, if current filename matches with filemask(s)
;### Output     ZF=1 ok
uzinam  ld a,(prgparfil)
        or a
        ret z
        ld ix,2*3+SyShell_CmdParas
uzinam1 push af
        ld l,(ix+0)
        ld h,(ix+1)
        push hl
        ld b,(ix+2)
uzinam2 ld a,(hl)           ;**** check, if filemask contains directory reference
        cp "/"
        jr z,uzinam5
        cp "\"
        jr z,uzinam5
        inc hl
        djnz uzinam2
        ld hl,uzifilnam         ;no  -> skip directory part of filename, if existing
        ld e,l
        ld d,h
uzinam3 ld a,(hl)
        inc hl
        cp "/"
        jr nz,uzinam4
        ld e,l
        ld d,h
uzinam4 or a
        jr nz,uzinam3           ;de=filename
        pop hl                  ;hl=filemask
        jr uzinam8
uzinam5 pop hl                  ;yes -> skip first byte of mask, if root
        ld a,(hl)
        cp "/"
        jr z,uzinam6
        cp "\"
        jr nz,uzinam7
uzinam6 inc hl
uzinam7 ld de,uzifilnam         ;de=filename, hl=filemask (both 0-terminated)
uzinam8 ld a,(hl)           ;**** test filemask with filename
        inc hl
        cp "*"
        jr z,uzinamd
        cp "?"
        jr z,uzinamb
        call clcucs             ;* regular char -> lcase-compare
        ld c,a
        ld a,(de)
        call clcucs
        cp c
        jr nz,uzinamc           ;don't match -> try next filemask
uzinam9 inc de
        or a
        jr nz,uzinam8
        pop bc                  ;0-terminator reached -> MATCHED!
        ret
uzinamb ld a,(de)               ;* "?" -> any char in filename, but no "." or "/"
        or a
        jr z,uzinamc
        cp "."
        jr z,uzinamc
        cp "/"
        jr nz,uzinam9
uzinamc pop af                  ;* current mask doesn't match
        inc ix:inc ix:inc ix
        dec a
        jr nz,uzinam1
        inc a                   ;no matches at all
        ret
uzinamd ld a,(de)               ;* "*" -> 0-x chars until "/", "." or "<ZERO>"
        or a
        jr z,uzinam8
        cp "."
        jr z,uzinam8
        cp "/"
        jr z,uzinam8
        inc de
        jr uzinamd

;### UZISHR -> generates a short filename
;### Input      uzifilnam=original filename, A=index (0=try to use original name, 1-255=append ~ anyway)
;### Output     uzishrnam=short filename, ZF=0 filename has been modified
uzishrnam   ds 8+1+3+1
uzishridx   db 0
uzishrext   db 0    ;bit7=0 extension too long
uzishrflg   db 0    ;0=filename has been modified

uzishr  ld (uzishridx),a
        ld a,-1
        ld (uzishrflg),a
        ld (uzishrext),a
        ld hl,uzifilnam     ;** search for last dot
        ld bc,0
uzishr1 ld a,(hl)
        cp "."
        jr nz,uzishr2
        ld c,l
        ld b,h
uzishr2 inc hl
        or a
        jr z,uzishr4
        cp "/"
        jr nz,uzishr1
uzishr4 ld a,c                  ;if dot found -> check extension length
        or b
        jr z,uzishr5
        or a
        sbc hl,bc
        ld a,l
        sub 6                   ;bit7=0 -> extension too long
        ld (uzishrext),a
uzishr5 db #fd:ld l,8       ;** copy filename first part
        ld hl,uzifilnam
        ld de,uzishrnam
        call uzishr0
        ld hl,uzishridx
        ld a,(hl)
        jr nz,uzishr6           ;source too long
        or a
        jr nz,uzishr6           ;index needed anyway
        inc hl
        bit 7,(hl)              ;extension too long
        jr nz,uzishre
uzishr6 ex de,hl            ;** "~" index needed
        ld e,2:cp 10:jr c,uzishr7
        inc e:cp 100:jr c,uzishr7
        inc e                   ;e=length
uzishr7 ld d,a
        db #fd:ld a,l
        sub e
        jr nc,uzishr9
uzishr8 dec hl                  ;move destination pointer backwards, if needed for adding "~" index
        inc a
        jr nz,uzishr8
uzishr9 ld (hl),"~"             ;write index
        inc hl
        ld a,d
        call clcded
        inc hl
        ex de,hl
        xor a
        ld (uzishrflg),a
uzishre ex de,hl
        ld a,c              ;** continue with extension, if existing
        or b
        ld (hl),a
        jr z,uzishrf
        ld (hl),"."             ;copy extension
        inc hl
        ex de,hl
        db #fd:ld l,3
        ld l,c
        ld h,b
        inc hl
        call uzishr0
        ex de,hl
        ld (hl),0
uzishrf ld a,(uzishrflg)
        inc a
        ret

uzishr0 call uzishr3        ;iyl=max, hl=sourcename, de=destination name -> copies max chars until /, 0 or last . -> zf=1 source not too long, de=next char at destination, max-iyl=copied chars
        ret z
        push bc
        call clcfnc
        pop bc
        ld (de),a
        inc hl
        inc de
        db #fd:dec l
        jr nz,uzishr0
uzishr3 ld a,(hl)
        or a
        ret z
        cp "/"
        ret z
        cp "."
        ret nz
        push hl
        or a
        sbc hl,bc
        pop hl
        ret

;### UZIDIC -> checks, if path contains subdirectory
;### Output     CF=1 subdirectory included, HL=char behind "/"
uzidic  ld hl,uzifilnam
uzidic1 ld a,(hl)
        or a
        ret z
        inc hl
        cp "/"
        jr nz,uzidic1
        scf
        ret

;### UZIDIS -> removes first subdirectory from path, if present
;### Output     CF=1 subdirectory has been removed (ZF=1 -> no file present)
uzidis  call uzidic
        ret nc
        ld de,uzifilnam
        push de
        ld bc,256
        ldir
        pop hl
        inc (hl):dec (hl)
        ret

;### UZIDCR -> uncompresses file of current directory entry
;### Output     CF=1 error, A=error code (0=file error, 1=size error, 2=crc32 error)
uzidcr  ld ix,(uzicprofs+0)
        ld iy,(uzicprofs+2)
        ld c,0
        call inppoi
        ret c
        ld a,(uzityp)
        dec a
        call nz,zippoi
        ret c

        ld hl,buf_read          ;init buffers
        inc h
        ld a,RED_BUFSIZ/256
        call redini
        ld hl,buf_write
        inc h
        call wrtini
        call c32ini             ;init CRC32

        ld a,(uzicprmet)
        dec a
        jr nz,uzidcr3
        call stomai             ;** STORE
        jr uzidcr4
uzidcr3 call iflmai             ;** INFLATE

uzidcr4 ld a,(uzicatflg)
        or a
        call z,outclo           ;close (if not in concat mode)
        ld a,(uzitimflg)
        or a
        call nz,outtim          ;set timestamp, if requested

uzidcr2 ld de,uzilencur         ;size check
        ld hl,uzilenorg
        ld bc,4
uzidcr0 ld a,(de)
        inc de
        cpi
        ld a,1
        scf
        ret nz
        jp pe,uzidcr0
        ld a,(c32flg)           ;CRC32 check
        or a
        ret z
        ld de,c32val
        ld hl,uzicrc32
        ld b,4
uzidcr1 ld a,(de)
        cpl
        cp (hl)
        scf
        ld a,2
        ret nz
        inc de
        inc hl
        djnz uzidcr1
        xor a
        ret

;### UZICNT -> resets all counters
uzicntorg   ds 4    ;original
uzicntcpr   ds 4    ;compressed
uzicntdcr   dw 0    ;extracted
uzicntskp   dw 0    ;skipped
uzicntcor   dw 0    ;corrupt

uzicnt  ld hl,uzicntorg+0
        ld de,uzicntorg+1
        ld (hl),0
        ld bc,4+4+2+2+2-1
        ldir
        ret

;### UZIINC -> increases one 16bit counter
uziinc  inc (hl)
        ret nz
        inc hl
        inc (hl)
        ret

;### UZIPRO -> plots current progress
uziproofs   dw 0
uzipro  ld ix,(uziproofs)
        ld e,(ix-4-1)
        ld d,(ix-4-0)
        ld c,100
        call clcmul
        ld c,l:ld b,h
        ld e,(ix-1)
        ld d,(ix-0)
        call clcdiv
        ld a,l
        ld hl,txtexfln6+2
        call clcded
        inc hl
        ld (hl),"%"
        inc hl
        ld (hl),"/"
        inc hl
        push hl:pop iy
        call uzipro0
        ld (iy+2),"]"
        ld (iy+3),0
        ld hl,txtexfln6
        jp SyShell_STROUT0
uzipro0 ld hl,(uzilencur+1)
        ld a,(uzilencur+3)
        srl a:rr h:rr l
        srl a:rr h:rr l
        push hl:pop ix
        ld e,a
        ld d,0
        call clcn32
        ld (iy+1),"K"
        ret

;### UZIFIL -> generates filename for non-directory mode
;### Output     HL=filename, CF=1 skip entry
uzifil  call uzidis             ;remove subdirectories
        jr nc,uzifil1
        jr nz,uzifil
        ld hl,txtexflnh:call SyShell_STROUT0    ;entry is a subdirectory -> ignore
        scf
        ret
uzifil1 call uzishr             ;shorten filename
        ld hl,(outfilnam)
        ld (outfilpos),hl
        scf:ccf
        ld hl,uzishrnam
        ret

;### UZISUB -> generates filepath for directory mode
;### Output     HL=filepath, CF=1 skip entry
uzisub  ld de,uzifilbuf
uzisub1 push de
        call uzidic
        jr nc,uzisub3
        pop de
        dec hl
        push hl
        push de
        xor a               ;terminate dirname
        ld (hl),a
        call uzishr         ;shrink dirname
        ld hl,uzishrnam
        pop de
uzisub2 ld a,(hl)           ;copy shrinked dirname to destination
        ldi
        or a
        jr nz,uzisub2
        dec de
        ld a,"/"
        ld (de),a           ;append "/"
        inc de
        pop hl
        ld (hl),"/"         ;remove termination in source again
        push de
        call uzidis         ;remove dirname from source
        pop de
        jr nz,uzisub1

        ld a,(uzitstflg)
        or a
        jr nz,uzisub6
        ex de,hl            ;entry is a subdir -> (try to) create it
        dec hl
        ld (hl),0
        ld hl,uzifilbuf
        ld de,(outfilnam)
uzisub5 ld a,(hl)
        ldi
        or a
        jr nz,uzisub5
        ld hl,txtexfln4:        call SyShell_STROUT0
        ld hl,outfilpth:push hl:call SyShell_STROUT0:pop hl
        ld ix,(App_BnkNum-1)
        call SyFile_DIRNEW
        ;...jr c,...
uzisub6 scf
        ret
uzisub3 pop de
        push de
        call uzishr         ;shorten filename
        pop hl
        push af
        ld e,l
        ld d,h
        ld bc,uzifilbuf
        sbc hl,bc
        ld bc,(outfilnam)
        add hl,bc
        ld (outfilpos),hl
        ld hl,uzishrnam
        ld bc,13
        ldir
        pop af
        scf:ccf
        ld hl,uzifilbuf
        ret

;### UZIEXD -> extract files with subdirectories
uziexd  ld a,1
        ld (uzifilmod),a
        jr uziexf

;### UZITST -> test files
uzitst  ld a,1
        ld (uzitstflg),a
        ld (c32flg),a
        ld hl,txtexfln3a
        ld (uziexfa+1),hl
        ld hl,txtexflnbt
        ld de,txtexflnb1
        ld bc,10
        ldir
        jr uziexf
;### UZIEXF -> extract files
uziexfmdf   db 0    ;flag, if plot modified filename

uziexf  call uzicnt
        ld hl,txtexfln1:call SyShell_STROUT0:ld hl,(inpfilnam):call SyShell_STROUT0     ;processing archive
        ld a,(uzicatflg)
        or a
        jr z,uziexfc
        ld hl,txtexflnl:call SyShell_STROUT0:ld hl, outfilpth :call SyShell_STROUT0     ;concatenate
        call outopn
uziexfc ld hl,txtexfln2:call SyShell_STROUT0
uziexf1 call uzidir             ;get next entry
        jp z,uziexf4
        jp c,prgendf
        call uzinam             ;test, if filename/path matches
        jr nz,uziexf1
uziexfa ld hl,txtexfln3:call SyShell_STROUT0:ld hl,uzifilnam:call SyShell_STROUT0       ;extracting ...
        ld hl,(uzicprmet)       ;check, if supported
        xor a
        cp l
        ld a,h
        ld hl,txtexflni
        jr z,uziexf5
        or a
        ld hl,txtexflnj
        jr z,uziexf2
uziexf5 call SyShell_STROUT0
        ld hl,uzicntskp
        call uziinc
        jr uziexf1

uziexf2 ld a,(uzifilmod)
        or a
        jr nz,uziexf3
        call uzifil
        jr uziexf7
uziexf3 call uzisub
uziexf7 jr c,uziexf1
        ld a,0
        jr z,uziexfd
        inc a
uziexfd ld (uziexfmdf),a        ;store "filename modified" flag
        ld a,(uzicatflg)        ;cat mode -> skip destination filename procedure
        or a
        jr nz,uziexfb
        ld a,(uzitstflg)        ;test mode -> skip destination filename procedure
        or a
        jr nz,uziexfb
        ld de,(outfilnam)       ;** hl=path/filename -> append filename to destination path
uziexf6 ld a,(hl)
        ldi
        or a
        jr nz,uziexf6
        push af
uziexfe ld hl,outfilpth         ;test, if file exist
        ld a,(App_BnkNum)
        db #dd:ld h,a
        call SyFile_FILOPN
        pop bc
        jr c,uziexff            ;no -> let's start
        push bc
        call SyFile_FILCLO
        pop af
        inc a                   ;yes -> increase index
        jr z,uziexff
        push af
        call uzishr             ;create a new short/indexed filename
        ld hl,uzishrnam
        ld de,(outfilpos)
        ld bc,13
        ldir                    ;append it
        ld a,1
        ld (uziexfmdf),a
        jr uziexfe              ;and try again
uziexff ld a,(uziexfmdf)
        or a
        jr z,uziexfg
        ld hl,txtexfln4:call SyShell_STROUT0:ld hl,(outfilpos):call SyShell_STROUT0       ;modified -> plot new filename
uziexfg call outopn             ;create destination file
        ld a,6
        jp c,prgendf
uziexfb ld hl,txtexfln5:call SyShell_STROUT0
        call outini
        call uzidcr             ;uncompress data
        jr c,uziexf8
        call uzisum
        ld iy,txtexfln7+2
        call uzipro0
        ld (iy+2),","
        ld (iy+3)," "
        ld (iy+4),"O"
        ld (iy+5),"K"
        ld (iy+6),"]"
        ld (iy+7),18
        ld (iy+8),0
        ld hl,txtexfln7:call SyShell_STROUT0
        ld hl,uzicntdcr
        call uziinc
        jp uziexf1

uziexf8 cp 1
        ld hl,txtexflna
        jr c,uziexf9
        ld hl,txtexflnk
        jr z,uziexf9
        ld hl,txtexfln8
uziexf9 call SyShell_STROUT0
        ld hl,uzicntcor
        call uziinc
        jp uziexf1

uziexf4 ld hl,(uzicntdcr):                      ld iy,txtexflnb0:call clcnum
        ld hl,(uzicntskp):                      ld iy,txtexflne0:call clcnum
        ld hl,(uzicntcor):                      ld iy,txtexflnf0:call clcnum
        ld ix,(uzicntorg+0):ld de,(uzicntorg+2):ld iy,txtexflnc0:call clcn32
        ld ix,(uzicntcpr+0):ld de,(uzicntcpr+2):ld iy,txtexflnd0:call clcn32
        ld (iy+1),")":ld (iy+2),0
        ld hl,txtexflnb:call SyShell_STROUT0
        ld hl,txtexflnc:call SyShell_STROUT0
        ld hl,txtexflnd:call SyShell_STROUT0
        ld hl,txtexflne:call SyShell_STROUT0
        ld hl,txtexflnf:call SyShell_STROUT0
        ld hl,txtexflng:call SyShell_STROUT0
        jp prgend

;### UZILST -> list files
uzilst  call uzilst0
        ld hl,txtlstln5:call SyShell_STROUT0
        ld hl,txtlstln6:call SyShell_STROUT0                                            ;list header
        ld hl,0
uzilst2 push hl
        call uzidir                     ;get next entry
        jr z,uzilst3
        jp c,prgendf
        call uzinam                     ;test, if filename/path matches
        pop hl
        jr nz,uzilst2
        push hl
        ld ix,(uzilenorg+0)             ;include original size
        ld de,(uzilenorg+2)
        ld hl,txtlstln7+txtlstln7o
        call clcnra
        ld ix,(uzilencpr+0)             ;include compressed size
        ld de,(uzilencpr+2)
        ld hl,txtlstln7+txtlstln7c
        call clcnra
        call uzisum
        ld hl,(uzitimstp+0)             ;include date/time
        ld bc,(uzitimstp+2)
        ld de,txtlstln7+txtlstln7t
        call clctim
        ld hl,txtlstln7:call SyShell_STROUT0:ld hl,uzifilnam:call SyShell_STROUT0     ;directory entry
        pop hl
        inc hl
        jr uzilst2
uzilst3 ld ix,(uzicntorg+0)             ;sum of original sizes
        ld de,(uzicntorg+2)
        ld hl,txtlstln71+txtlstln7o
        call clcnra
        ld ix,(uzicntcpr+0)             ;sum of compressed sizes
        ld de,(uzicntcpr+2)
        ld hl,txtlstln71+txtlstln7c
        call clcnra
        pop hl
        ld a,l
        or h
        push hl
        ld hl,txtlstln9 :call z,SyShell_STROUT0
        pop ix
        ld de,0
        ld iy,clcnrab                   ;total number of files
        call clcn32
        ld hl,txtlstln6 :call SyShell_STROUT0
        ld hl,txtlstln71:call SyShell_STROUT0
        ld hl,clcnrab   :call SyShell_STROUT0
        ld hl,txtlstln8 :call SyShell_STROUT0
        jp prgend

uzilst0 call uzicnt
        ld hl,txtlstln1:call SyShell_STROUT0:ld hl,(inpfilnam):call SyShell_STROUT0     ;listing archive
        ld hl,txtlstln2:call SyShell_STROUT0:ld hl,inpfilpth  :call SyShell_STROUT0     ;path
        ld a,(uzityp)
        dec a
        ld hl,txtlstln3a
        jr z,uzilst1
        ld hl,txtlstln3b
uzilst1 call SyShell_STROUT0                                                            ;type
        ld ix,(uzifilsiz+0)
        ld de,(uzifilsiz+2)
        ld iy,clcnrab
        call clcn32
        ld hl,txtlstln4:call SyShell_STROUT0:ld hl,clcnrab    :call SyShell_STROUT0     ;physical size
        ret

uzisum  ld hl,(uzilenorg+0)
        ld de,(uzilenorg+2)
        ld ix,uzicntorg
        call clca32
        ld hl,(uzilencpr+0)
        ld de,(uzilencpr+2)
        ld ix,uzicntcpr
        jp clca32

;### UZIINF -> show detailed file information
uziinf  call uzilst0
        ld hl,txtinfln0:call SyShell_STROUT0
uziinf1 call uzidir                             ;get next entry
        jp z,uziinf0
        jp c,prgendf
        call uzinam                             ;test, if filename/path matches
        jr nz,uziinf1
        ld hl,txtinfln1:call SyShell_STROUT0
        ld hl,uzifilnam:call SyShell_STROUT0
        ld ix,(uzilenorg+0)                     ;size
        ld de,(uzilenorg+2)
        ld iy,txtinfln2+9
        call clcn32
        ld hl,txtinfln2:call SyShell_STROUT0
        ld ix,(uzilencpr+0)                     ;packed size
        ld de,(uzilencpr+2)
        ld iy,txtinfln3+16
        call clcn32
        ld hl,txtinfln3:call SyShell_STROUT0
        ld hl,(uzitimstp+0)                     ;date/time
        ld bc,(uzitimstp+2)
        ld de,txtinfln4+13
        call clctim
        ld hl,txtinfln4:call SyShell_STROUT0
        ld hl,uzicrc32+3                        ;CRC32
        ld de,txtinfln6+10
        ld b,4
uziinf2 ld a,(hl)
        dec hl
        call clchex
        djnz uziinf2
        ld hl,txtinfln6:call SyShell_STROUT0
        ld a,(uzicprmet)                        ;methode
        cp 1
        ld hl,txtinfln7a
        jr c,uziinf3
        ld hl,txtinfln7b
        jr z,uziinf3
        ld hl,txtinfln7c
uziinf3 call SyShell_STROUT0
        ld hl,(uziverned)                       ;version
        ld iy,txtinfln8+12
        call clcnum
        ld hl,txtinfln8:call SyShell_STROUT0
        ld hl,txtinfln9:call SyShell_STROUT0
        jp uziinf1
uziinf0 ld hl,txtinfln9:call SyShell_STROUT0
        jp prgend


;==============================================================================
;### STORE ROUTINES ###########################################################
;==============================================================================

;### STOMAI -> "decompresses" a stored data stream
stomai  ld hl,(uzilenorg+0)
        ld de,(uzilenorg+2)     ;de,hl=total length
stomai1 ld bc,32768
        ld a,e
        or d
        jr nz,stomai2
        ld a,h
        cp 32768/256
        jr nc,stomai2
        ld c,l
        ld b,h
stomai2 push de
        push hl
        ld hl,buf_write
        inc h
        push hl
        push bc
        call inplod0
        pop bc
        pop hl
        add hl,bc
        call wrtout
        pop hl
        pop de
        or a
        sbc hl,bc
        jr z,stomai3
        jr nc,stomai1
        dec de
        jr stomai1
stomai3 ld a,e
        or d
        jr nz,stomai1
        ret


;==============================================================================
;### ZIP MANAGEMENT ROUTINES ##################################################
;==============================================================================

zipdirnum   dw 0    ;number of directory entries
zipdirofs   ds 4    ;offset of current directory entry

;### ZIPHED -> loads header of a ZIP file and prepares directory access
;### Output     CF=0 ok, CF=1 error (A=error code; 0=error while loading, 1=corrupt header, )
ZIPHED_ID           equ 0   ;EOD signature (#50,#4B,#05,#06)
ZIPHED_DISC         equ 4   ;actual disc
ZIPHED_DISC_CD      equ 6   ;disc with CD start
ZIPHED_ENTR_DISC    equ 8   ;CD entries on this disc
ZIPHED_ENTR_TOTAL   equ 10  ;CD entries total
ZIPHED_CD_SIZE      equ 12  ;CD size
ZIPHED_CD_OFFSET    equ 16  ;CD offset
ZIPHED_COMMENT_LEN  equ 20  ;comment length

ziphed  ld ix,0
        ld iy,0
        ld c,2
        call inppoi:ret c               ;skip to file end
        ld (uzifilsiz+0),ix
        ld (uzifilsiz+2),iy             ;store total file size
        ld ix,-512
        ld iy,-1
        ld c,1
        call inppoi:ret c
        ld bc,512
        ld hl,uzifilbuf
        push hl
        call inpred                     ;load last 512 bytes, which should contain the EOD chunk
        pop ix
        ret c
        ld bc,512-22+1                  ;search for EOD signature (#50,#4B,#05,#06)
ziphed1 ld a,(ix+ZIPHED_ID+0)
        cp #50
        jr z,ziphed3
ziphed2 inc ix
        dec bc
        ld a,c
        or b
        jr nz,ziphed1
        ld a,1
        scf
        ret
ziphed3 ld a,#4b:cp (ix+ZIPHED_ID+1):jr nz,ziphed2
        ld a,#05:cp (ix+ZIPHED_ID+2):jr nz,ziphed2
        ld a,#06:cp (ix+ZIPHED_ID+3):jr nz,ziphed2
        xor a
        cp (ix+ZIPHED_DISC+0)           ;check, if multiple disc archive
        jr nz,ziphed4
        cp (ix+ZIPHED_DISC+1)
        jr nz,ziphed4
        ld l,(ix+ZIPHED_ENTR_TOTAL+0)   ;get total number of directory entries
        ld h,(ix+ZIPHED_ENTR_TOTAL+1)
        ld (zipdirnum),hl
        ld l,(ix+ZIPHED_CD_OFFSET+0)    ;get directory offset
        ld h,(ix+ZIPHED_CD_OFFSET+1)
        ld (zipdirofs+0),hl
        ld l,(ix+ZIPHED_CD_OFFSET+2)
        ld h,(ix+ZIPHED_CD_OFFSET+3)
        ld (zipdirofs+2),hl             ;store offset
        xor a
        ret
ziphed4 ld a,4
        scf
        ret

;### ZIPDIR -> loads next directory entry
;### Output     ZF=0 -> CF=0 ok, (uzicprmet), (uzisupflg), (uzicrc32), (uzifilnam), (uzitimstp), (uzicprofs), (uzilencpr), (uzilenorg) updated
;###                    CF=1 error (A=error code; 0=error while loading, 2=corrupt or unsupported data)
;###            ZF=1 -> EOD reached
ZIPDIR_ID           equ 0
ZIPDIR_VERS_MADE    equ 4
ZIPDIR_VERS_NEED    equ 6
ZIPDIR_FLAGS        equ 8
ZIPDIR_CM           equ 10
ZIPDIR_MTIME        equ 12
ZIPDIR_CRC32        equ 16
ZIPDIR_SIZE_CPR     equ 20
ZIPDIR_SIZE_ORG     equ 24
ZIPDIR_LEN_NAME     equ 28
ZIPDIR_LEN_EXTRA    equ 30
ZIPDIR_LEN_COMMENT  equ 32
ZIPDIR_DISC         equ 34
ZIPDIR_ATR_INT      equ 36
ZIPDIR_ATR_EXT      equ 38
ZIPDIR_OFFSET       equ 42
ZIPDIR_NAME         equ 46

zipdir  ld hl,(zipdirnum)
        ld a,l
        or h
        ret z
        dec hl
        ld (zipdirnum),hl
        ld ix,(zipdirofs+0)
        ld iy,(zipdirofs+2)
        ld c,0
        call inppoi:jp c,zipdir0
        ld hl,uzifilbuf
        ld bc,ZIPDIR_NAME+255
        call inpred:jp c,zipdir0
        ld hl,uzifilbuf+ZIPDIR_MTIME
        ld de,uzitimstp             ;store timestamp
        ld bc,4
        ld a,c
        ldir
        ld de,uzicrc32              ;store CRC32
        ld c,a
        ldir
        ld de,uzilencpr             ;store compressed length
        ld c,a
        ldir
        ld de,uzilenorg             ;store original length
        ld c,a
        ldir
        ld hl,uzifilbuf+ZIPDIR_OFFSET
        ld de,uzicprofs             ;store data offset
        ld c,a
        ldir
        ld hl,(uzifilbuf+ZIPDIR_CM) ;store compression method
        xor a
        cp h
        jr nz,zipdir1
        cp l
        ld a,1
        jr z,zipdir1
        ld a,8
        cp l
        ld a,2
        jr z,zipdir1
        xor a
zipdir1 ld (uzicprmet),a
        ld hl,(uzifilbuf+ZIPDIR_FLAGS)
        ld a,l
        and %11111001       ;ignore compression level (fast/max)
        ld l,a
        ld a,h
        and %11110111       ;ignore UTF-8 name encoding
        or l                ;all other additional features are not supported
        ld c,a
        ld hl,(uzifilbuf+ZIPDIR_VERS_NEED)
        ld (uziverned),hl
        inc h:dec h
        ld a,1
        jr nz,zipdir2
        ld a,l
        cp 20+1
        jr nc,zipdir2
        xor a               ;version needed must be <="2.0" (20)
zipdir2 or c
        ld (uzisupflg),a
        ld de,uzifilnam             ;reset and copy filename
        ld hl,(uzifilbuf+ZIPDIR_LEN_NAME)
        inc h:dec h
        ld a,l
        jr z,zipdir3
        ld a,255
zipdir3 or a
        jr z,zipdir5
        ld b,a
        ld hl,uzifilbuf+ZIPDIR_NAME
zipdir4 ld a,(hl)
        call clcasc
        ld (de),a
        inc hl
        inc de
        djnz zipdir4
zipdir5 xor a
        ld (de),a
        ld de,ZIPDIR_NAME           ;calculate and store next directory entry offset
        ld hl,(uzifilbuf+ZIPDIR_LEN_NAME)
        add hl,de
        ld de,(uzifilbuf+ZIPDIR_LEN_EXTRA)
        add hl,de
        ld de,(uzifilbuf+ZIPDIR_LEN_COMMENT)
        add hl,de
        ld de,(zipdirofs+0)
        add hl,de
        ld (zipdirofs+0),hl
        ld de,0
        ld hl,(zipdirofs+2)
        adc hl,de
        ld (zipdirofs+2),hl
        xor a
        inc a
        ret
zipdir0 xor a
        inc a
        ld a,0
        scf
        ret

;### ZIPPOI -> moves filepointer to compressed data
zippoi  ld hl,uzifilbuf
        ld bc,30
        call inpred
        ret c
        ld ix,(uzifilbuf+30-4)
        ld de,(uzifilbuf+30-2)
        add ix,de
        ld iy,0
        ld c,1
        jp inppoi


;==============================================================================
;### GZ MANAGEMENT ROUTINES ###################################################
;==============================================================================

gzpdirnum   db 0

;### GZPHED -> loads header of a GZIP file and prepares directory access
;### Output     CF=0 ok, CF=1 error (A=error code; 0=error while loading, 1=corrupt header)
GZPHED_ID1      equ 0   ;id1 (#1f)
GZPHED_ID2      equ 1   ;id2 (#8b)
GZPHED_CM       equ 2   ;compression method (8=deflate)
GZPHED_FLG      equ 3   ;bit0=ftext (file is probably an ascii text),
                        ;bit1=fhcrc (crc16 for header present; in front of compressed data),
                        ;bit2=fextra (extra field present; starting at byte 10),
                        ;bit3=fname (filename present as 0-terminated ISO 8859-1 [LATIN-1] string; starting behind fextra),
                        ;bit4=fcomment (file comment present as 0-terminated ISO 8859-1 [LATIN-1] string; starting behind fname),
                        ;bit5-7=reserved
GZPHED_MTIME    equ 4   ;timestamp (UNIX time)
GZPHED_XFL      equ 8   ;extra flags
GZPHED_OS       equ 9   ;OS filesystem (0=FAT filesystem [MS-DOS, OS/2, NT/Win32], 1=Amiga, 2=VMS [or OpenVMS], 3=Unix, 4=VM/CMS, 5=Atari TOS, 6=HPFS filesystem [OS/2, NT], 7=Macintosh,
                        ;               8=Z-System, 9=CP/M, 10=TOPS-20, 11=NTFS filesystem [NT], 12=QDOS, 13=Acorn RISCOS, 255=unknown)

gzphed  ld ix,0
        ld iy,0
        ld c,2
        call inppoi:ret c               ;skip to file end
        ld (uzifilsiz+0),ix
        ld (uzifilsiz+2),iy             ;store total file size
        ld ix,-8
        ld iy,-1
        ld c,1
        call inppoi:ret c
        ld bc,8
        ld hl,uzifilbuf
        push hl
        call inpred                     ;load last 8 bytes (not possible with Amsdos ##!!##)
        pop hl
        ret c
        ld de,uzicrc32                  ;store CRC32
        ld bc,4
        ldir
        ld de,uzilenorg                 ;store original size
        ld c,4
        ldir
        ld ix,0
        ld iy,0
        ld c,0
        call inppoi:ret c
        ld bc,512
        ld hl,uzifilbuf
        call inpred:ret c
        ld hl,(uzifilbuf+GZPHED_ID1)    ;check ID
        ld bc,#8b1f
        or a
        sbc hl,bc
        ld a,1
        scf
        ret nz
        xor a
        ld (gzpdirnum),a                ;reset directory pointer
        ld (uzisupflg),a
        ret

;### GZPDIR -> loads next directory entry
;### Output     ZF=0 -> CF=0 ok, (uzicprmet), (uzisupflg), (uzifilnam), (uzitimstp), (uzicprofs), (uzilencpr), (uzilenorg) updated
;###                    CF=1 error (A=error code; 0=error while loading, 2=corrupt or unsupported data)
;###            ZF=1 -> EOD reached
gzpdirnm4   db 4,0,0,0

gzpdir  ld a,1
        ld hl,gzpdirnum
        cp (hl)
        ld (hl),a
        ret z
        ld a,(uzifilbuf+GZPHED_CM)
        cp 8
        ld a,2
        jr z,gzpdir0
        xor a
gzpdir0 ld (uzicprmet),a        ;store compression method
        ld hl,(uzifilbuf+GZPHED_MTIME+0)
        ld de,(uzifilbuf+GZPHED_MTIME+2)
        call timunx
        call SyFile_FILT2F
        ld (uzitimstp+0),bc     ;store timestamp
        ld (uzitimstp+2),de
        ld hl,uzifilbuf+10
        ld de,512-10            ;bc=amount of remaining bytes
        ld a,(uzifilbuf+GZPHED_FLG)
        bit 2,a
        jr z,gzpdir1
        ld c,(hl)               ;skip FEXTRA data
        inc hl
        ld b,(hl)
        inc hl
        add hl,bc
        dec de
        dec de
        ex de,hl
        or a
        sbc hl,bc               ;update remaining data
        ex de,hl
        jr c,gzpdire
gzpdir1 bit 3,a
        ld ix,uzifilnam
        ld (ix+0),0             ;reset filename
        jr z,gzpdir3
        ld b,a
        ld c,0
gzpdir2 ld a,(hl)               ;copy FNAME to filename
        call clcasc
        ld (ix+0),a
        inc hl
        inc ix
        inc c
        jr z,gzpdire
        or a
        jr nz,gzpdir2
        ld a,b
        ld b,0
        ex de,hl
        sbc hl,bc               ;update remaining data
        ex de,hl
        jr c,gzpdire
gzpdir3 bit 4,a
        jr z,gzpdir5
gzpdir4 dec de                  ;skip FCOMMENT data
        inc d
        jr z,gzpdire
        dec d
        inc (hl)
        dec (hl)
        inc hl
        jr nz,gzpdir4
gzpdir5 bit 1,a
        jr z,gzpdir6
        inc hl                  ;skip FHCRC word
        inc hl
gzpdir6 ld bc,uzifilbuf
        or a
        sbc hl,bc
        ld (uzicprofs+0),hl
        ld hl,0
        ld (uzicprofs+2),hl     ;save compressed data position in file
        ld hl,(uzifilsiz+0)
        ld de,(uzifilsiz+2)
        ld ix,uzicprofs         ;calculate compressed size out of header and file size
        call clcs32
        ld ix,gzpdirnm4
        call clcs32
        ld (uzilencpr+0),hl
        ld (uzilencpr+2),de
        xor a
        inc a
        ret
gzpdire ld a,2
        or a
        scf
        ret


;==============================================================================
;### INPUT ROUTINES ###########################################################
;==============================================================================

inphnd  db -1

;### INPOPN -> opens the input file
inpopn  ld a,(App_BnkNum)
        db #dd:ld h,a
        ld hl,inpfilpth
        call SyFile_FILOPN
        jr c,inpopn1
        ld (inphnd),a
inpopn1 ld a,0
        ret

;### INPCLO -> closes the input file
inpclo  ld a,(inphnd)
        cp -1
        ret z
        jp SyFile_FILCLO

;### INPRED -> reads an amount of bytes from the input file to a given destination
;### Input      HL=address, BC=length
;### Output     CF=1 error
inpred  ld a,(inphnd)
        ld de,(App_BnkNum)
        call SyFile_FILINP
        ld a,0
        ret

;### INPPOI -> sets file pointer
;### Input      IY,IX=position, C=reference point (0=begin, 1=current, 2=end)
;### Output     CF=1 error
inppoi  ld a,(inphnd)
        call SyFile_FILPOI
        ld a,0
        ret

;### INPLOD -> load next part into the read buffer
inplod  ld hl,buf_read
        inc h
        ld bc,RED_BUFSIZ
inplod0 ld a,(inphnd)
        ld de,(App_BnkNum)
        call SyFile_FILINP
        ld a,3
        jp c,prgendf
ret
        jp cnsctc


;==============================================================================
;### OUTPUT ROUTINES ##########################################################
;==============================================================================

outhnd  db -1

;### OUTINI -> inits output counter for new file
outini  xor a
        ld l,a
        ld h,a
        ld (uzilencur+0),hl
        ld (uzilencur+2),hl
        ld hl,uzilenorg+4
outini1 dec hl
        cp (hl)
        jr z,outini1
        ld (uziproofs),hl
        ret

;### OUTOPN -> opens a new output file
outopn  ld hl,outfilpth
        ld a,(App_BnkNum)
        db #dd:ld h,a
        xor a
        call SyFile_FILNEW
        ret c
        ld (outhnd),a
        ret

;### OUTCLO -> closes the output file
outclo  ld hl,outhnd
        ld a,(hl)
        ld (hl),-1
        cp -1
        ret z
        jp SyFile_FILCLO

;### OUTTIM -> sets the timestamp of the output file
outtim  ld a,(App_BnkNum)
        db #dd:ld h,a
        ld hl,outfilpth
        ld a,1
        ld bc,(uzitimstp+0)
        ld de,(uzitimstp+2)
        jp SyFile_DIRPRS

;### OUTSAV -> saves data from the write buffer
;### Input      BC=length
;### Output     (uzilencur) updated
outsav  push bc
        ld a,(outhnd)
        ld de,(App_BnkNum)
        ld hl,buf_write
        inc h
        cp -1
        scf
        ccf
        call nz,SyFile_FILOUT
        pop hl
        ld a,5
        jp c,prgendf
        ld de,0
        ld ix,uzilencur
        call clca32
        ld a,(uzidspflg)
        or a
        ret z
        jp uzipro


ds -$ mod 256
buf_write   equ $-256   ;###last label in code area!###


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

;***data area stuff can be moved to code area as soon as the 48K barrier (caused by the transfer area) inside the memory allocator has been removed

App_BegData

;###place first in data area!###
crc32tab
db #00,#96,#2c,#ba,#19,#8f,#35,#a3,#32,#a4,#1e,#88,#2b,#bd,#07,#91,#64,#f2,#48,#de,#7d,#eb,#51,#c7,#56,#c0,#7a,#ec,#4f,#d9,#63,#f5
db #c8,#5e,#e4,#72,#d1,#47,#fd,#6b,#fa,#6c,#d6,#40,#e3,#75,#cf,#59,#ac,#3a,#80,#16,#b5,#23,#99,#0f,#9e,#08,#b2,#24,#87,#11,#ab,#3d
db #90,#06,#bc,#2a,#89,#1f,#a5,#33,#a2,#34,#8e,#18,#bb,#2d,#97,#01,#f4,#62,#d8,#4e,#ed,#7b,#c1,#57,#c6,#50,#ea,#7c,#df,#49,#f3,#65
db #58,#ce,#74,#e2,#41,#d7,#6d,#fb,#6a,#fc,#46,#d0,#73,#e5,#5f,#c9,#3c,#aa,#10,#86,#25,#b3,#09,#9f,#0e,#98,#22,#b4,#17,#81,#3b,#ad
db #20,#b6,#0c,#9a,#39,#af,#15,#83,#12,#84,#3e,#a8,#0b,#9d,#27,#b1,#44,#d2,#68,#fe,#5d,#cb,#71,#e7,#76,#e0,#5a,#cc,#6f,#f9,#43,#d5
db #e8,#7e,#c4,#52,#f1,#67,#dd,#4b,#da,#4c,#f6,#60,#c3,#55,#ef,#79,#8c,#1a,#a0,#36,#95,#03,#b9,#2f,#be,#28,#92,#04,#a7,#31,#8b,#1d
db #b0,#26,#9c,#0a,#a9,#3f,#85,#13,#82,#14,#ae,#38,#9b,#0d,#b7,#21,#d4,#42,#f8,#6e,#cd,#5b,#e1,#77,#e6,#70,#ca,#5c,#ff,#69,#d3,#45
db #78,#ee,#54,#c2,#61,#f7,#4d,#db,#4a,#dc,#66,#f0,#53,#c5,#7f,#e9,#1c,#8a,#30,#a6,#05,#93,#29,#bf,#2e,#b8,#02,#94,#37,#a1,#1b,#8d
db #00,#30,#61,#51,#c4,#f4,#a5,#95,#88,#b8,#e9,#d9,#4c,#7c,#2d,#1d,#10,#20,#71,#41,#d4,#e4,#b5,#85,#98,#a8,#f9,#c9,#5c,#6c,#3d,#0d
db #20,#10,#41,#71,#e4,#d4,#85,#b5,#a8,#98,#c9,#f9,#6c,#5c,#0d,#3d,#30,#00,#51,#61,#f4,#c4,#95,#a5,#b8,#88,#d9,#e9,#7c,#4c,#1d,#2d
db #41,#71,#20,#10,#85,#b5,#e4,#d4,#c9,#f9,#a8,#98,#0d,#3d,#6c,#5c,#51,#61,#30,#00,#95,#a5,#f4,#c4,#d9,#e9,#b8,#88,#1d,#2d,#7c,#4c
db #61,#51,#00,#30,#a5,#95,#c4,#f4,#e9,#d9,#88,#b8,#2d,#1d,#4c,#7c,#71,#41,#10,#20,#b5,#85,#d4,#e4,#f9,#c9,#98,#a8,#3d,#0d,#5c,#6c
db #83,#b3,#e2,#d2,#47,#77,#26,#16,#0b,#3b,#6a,#5a,#cf,#ff,#ae,#9e,#93,#a3,#f2,#c2,#57,#67,#36,#06,#1b,#2b,#7a,#4a,#df,#ef,#be,#8e
db #a3,#93,#c2,#f2,#67,#57,#06,#36,#2b,#1b,#4a,#7a,#ef,#df,#8e,#be,#b3,#83,#d2,#e2,#77,#47,#16,#26,#3b,#0b,#5a,#6a,#ff,#cf,#9e,#ae
db #c2,#f2,#a3,#93,#06,#36,#67,#57,#4a,#7a,#2b,#1b,#8e,#be,#ef,#df,#d2,#e2,#b3,#83,#16,#26,#77,#47,#5a,#6a,#3b,#0b,#9e,#ae,#ff,#cf
db #e2,#d2,#83,#b3,#26,#16,#47,#77,#6a,#5a,#0b,#3b,#ae,#9e,#cf,#ff,#f2,#c2,#93,#a3,#36,#06,#57,#67,#7a,#4a,#1b,#2b,#be,#8e,#df,#ef
db #00,#07,#0e,#09,#6d,#6a,#63,#64,#db,#dc,#d5,#d2,#b6,#b1,#b8,#bf,#b7,#b0,#b9,#be,#da,#dd,#d4,#d3,#6c,#6b,#62,#65,#01,#06,#0f,#08
db #6e,#69,#60,#67,#03,#04,#0d,#0a,#b5,#b2,#bb,#bc,#d8,#df,#d6,#d1,#d9,#de,#d7,#d0,#b4,#b3,#ba,#bd,#02,#05,#0c,#0b,#6f,#68,#61,#66
db #dc,#db,#d2,#d5,#b1,#b6,#bf,#b8,#07,#00,#09,#0e,#6a,#6d,#64,#63,#6b,#6c,#65,#62,#06,#01,#08,#0f,#b0,#b7,#be,#b9,#dd,#da,#d3,#d4
db #b2,#b5,#bc,#bb,#df,#d8,#d1,#d6,#69,#6e,#67,#60,#04,#03,#0a,#0d,#05,#02,#0b,#0c,#68,#6f,#66,#61,#de,#d9,#d0,#d7,#b3,#b4,#bd,#ba
db #b8,#bf,#b6,#b1,#d5,#d2,#db,#dc,#63,#64,#6d,#6a,#0e,#09,#00,#07,#0f,#08,#01,#06,#62,#65,#6c,#6b,#d4,#d3,#da,#dd,#b9,#be,#b7,#b0
db #d6,#d1,#d8,#df,#bb,#bc,#b5,#b2,#0d,#0a,#03,#04,#60,#67,#6e,#69,#61,#66,#6f,#68,#0c,#0b,#02,#05,#ba,#bd,#b4,#b3,#d7,#d0,#d9,#de
db #64,#63,#6a,#6d,#09,#0e,#07,#00,#bf,#b8,#b1,#b6,#d2,#d5,#dc,#db,#d3,#d4,#dd,#da,#be,#b9,#b0,#b7,#08,#0f,#06,#01,#65,#62,#6b,#6c
db #0a,#0d,#04,#03,#67,#60,#69,#6e,#d1,#d6,#df,#d8,#bc,#bb,#b2,#b5,#bd,#ba,#b3,#b4,#d0,#d7,#de,#d9,#66,#61,#68,#6f,#0b,#0c,#05,#02
db #00,#77,#ee,#99,#07,#70,#e9,#9e,#0e,#79,#e0,#97,#09,#7e,#e7,#90,#1d,#6a,#f3,#84,#1a,#6d,#f4,#83,#13,#64,#fd,#8a,#14,#63,#fa,#8d
db #3b,#4c,#d5,#a2,#3c,#4b,#d2,#a5,#35,#42,#db,#ac,#32,#45,#dc,#ab,#26,#51,#c8,#bf,#21,#56,#cf,#b8,#28,#5f,#c6,#b1,#2f,#58,#c1,#b6
db #76,#01,#98,#ef,#71,#06,#9f,#e8,#78,#0f,#96,#e1,#7f,#08,#91,#e6,#6b,#1c,#85,#f2,#6c,#1b,#82,#f5,#65,#12,#8b,#fc,#62,#15,#8c,#fb
db #4d,#3a,#a3,#d4,#4a,#3d,#a4,#d3,#43,#34,#ad,#da,#44,#33,#aa,#dd,#50,#27,#be,#c9,#57,#20,#b9,#ce,#5e,#29,#b0,#c7,#59,#2e,#b7,#c0
db #ed,#9a,#03,#74,#ea,#9d,#04,#73,#e3,#94,#0d,#7a,#e4,#93,#0a,#7d,#f0,#87,#1e,#69,#f7,#80,#19,#6e,#fe,#89,#10,#67,#f9,#8e,#17,#60
db #d6,#a1,#38,#4f,#d1,#a6,#3f,#48,#d8,#af,#36,#41,#df,#a8,#31,#46,#cb,#bc,#25,#52,#cc,#bb,#22,#55,#c5,#b2,#2b,#5c,#c2,#b5,#2c,#5b
db #9b,#ec,#75,#02,#9c,#eb,#72,#05,#95,#e2,#7b,#0c,#92,#e5,#7c,#0b,#86,#f1,#68,#1f,#81,#f6,#6f,#18,#88,#ff,#66,#11,#8f,#f8,#61,#16
db #a0,#d7,#4e,#39,#a7,#d0,#49,#3e,#ae,#d9,#40,#37,#a9,#de,#47,#30,#bd,#ca,#53,#24,#ba,#cd,#54,#23,#b3,#c4,#5d,#2a,#b4,#c3,#5a,#2d


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

;### CNSCTC -> check for ctrl+C
cnsctcf db 0
cnsctc  ld hl,cnsctcf
        dec (hl)
        ld (hl),1
        call nz,cnsctc1
        ld ix,(App_PrcID)       ;** check SymShell messages
        ld a,(SyShell_PrcID)
        db #dd:ld h,a
        ld iy,App_MsgBuf
        rst #18
        db #dd:dec l
        ret nz
        ld a,(App_MsgBuf+0)
        or a
        jp z,prgend             ;SymShell wants us to quit -> bye bye
        cp MSR_SHL_CHRINP
        ret nz
        ld a,(App_MsgBuf+3)
        or a
        jp nz,prgend            ;error -> bye bye
        ld a,(App_MsgBuf+1)
        or a
        ld a,8
        jp nz,prgendf           ;Ctrl+C -> bye bye
cnsctc1 ld hl,MSC_SHL_CHRINP    ;** tell SymShell, that we want a new char from the console
        ld (App_MsgBuf),hl
        ld ix,(App_PrcID)
        ld a,(SyShell_PrcID)
        db #dd:ld h,a
        ld iy,App_MsgBuf
        rst #10
        ret

;### CLCUCS -> converts into LowerCase
;### Input      A=char
;### Output     A=lcase(char)
;### Destroyed  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret

;### CLCFNC -> converts char into allowed filename char
;### Input      A=char
;### Output     A=allowed filename char
;### Destroyed  F,C,IX
clcfncx db 34," |+:;<=>[]."
clcfncy db   "______(-)()_"
clcfnc  call clcucs
        ld c,"_"
        cp 32
        jr c,clcfnc
        cp 127
        jr nc,clcfnc
        ld ix,clcfncx
        ld b,clcfncy-clcfncx
clcfnc1 ld c,(ix+clcfncy-clcfncx)
        cp (ix+0)
        jr z,clcfnc0
        inc ix
        djnz clcfnc1
        ret
clcfnc0 ld a,c
        ret

;### CLCASC -> converts char into readable ASCII (32-127)
;### Input      A=char
;### Output     A=char (32-127)
;### Destroyed  F
clcasc  or a
        ret z
        cp 32
        jr c,clcasc1
        cp 128
        ret c
clcasc1 ld a,"$"
        ret

;### CLCA32 -> add and store 32bit value
;### Input      (IX)=value1, DE,HL=value2
;### Output     (IX)=value1+value2
;### Destroyed  F,BC,DE,HL
clca32  ld c,(ix+0)
        ld b,(ix+1)
        add hl,bc
        ld (ix+0),l
        ld (ix+1),h
        ex de,hl
        ld c,(ix+2)
        ld b,(ix+3)
        adc hl,bc
        ld (ix+2),l
        ld (ix+3),h
        ret

;### CLCS32 -> substract 32bit values
;### Input      DE,HL=variable, (IX)=value
;### Output     DE,HL=DE,HL-(IX), CF=1 overflow
;### Destroyed  F,BC
clcs32  ld c,(ix+0)
        ld b,(ix+1)
        or a
        sbc hl,bc
        ld c,(ix+2)
        ld b,(ix+3)
        ex de,hl
        sbc hl,bc
        ex de,hl
        ret

;### CLCMUL -> Multipliziert zwei Werte (24bit)
;### Input      C=value1, DE=value2
;### Output     A,HL=value1*value2 (24bit)
;### Destroyed  F,BC,DE,IX
clcmul  xor a
        ld l,a
        ld h,a
        ld b,a
clcmul1 inc c:dec c
        ret z
        srl c
        jr nc,clcmul2
        add hl,de
        adc b
clcmul2 sla e
        rl d
        rl b
        jr clcmul1

;### CLCDIV -> Division (24bit)
;### Input      A,BC=value1, DE=value2
;### Output     HL=value1/value2, DE=value1 MOD value2
;### Destroyed  AF,BC,DE,IX,IYL
clcdiv  db #dd:ld l,e
        db #dd:ld h,d   ;IX=Wert2(Nenner)
        ld e,a          ;E,BC=Wert1(Zaehler)
        ld hl,0
        db #dd:ld a,l
        or d
        ret z
        ld d,l          ;D,HL=RechenVar
        db #fd:ld l,24  ;IYL=Counter
clcdiv1 rl c
        rl b
        rl e
        adc hl,hl
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,d
        sbc 0
        ld d,a          ;D,HL=D,HL-IX
        jr nc,clcdiv2
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        adc d
        sub h
        ld d,a
        scf
clcdiv2 db #fd:dec l
        jr nz,clcdiv1
        ex de,hl        ;DE=Wert1 MOD Wert2
        ld a,c
        rla
        cpl
        ld l,a
        ld a,b
        rla
        cpl
        ld h,a          ;HL=Wert1 DIV Wert2
        ret

;### CLCNUM -> Converts 16Bit number into ASCII string (0-terminated)
;### Input      HL=value, IY=address
;### Output     (IY)=last digit
;### Destroyed  AF,BC,DE,HL,IX,IY
clcnumt dw -1,-10,-100,-1000,-10000
clcnum  ld b,5-1        ;1
        ld ix,clcnumt+8 ;4
        xor a           ;1
clcnum1 ld e,(ix+0)     ;5
        ld d,(ix+1)     ;5
        dec ix          ;3
        dec ix          ;3
        ld c,"0"        ;2
clcnum2 add hl,de       ;3
        jr nc,clcnum5   ;3/2
        inc c           ;1
        inc a           ;1
        jr clcnum2      ;3
clcnum5 sbc hl,de       ;4
        or a            ;1
        jr z,clcnum3    ;3/2
        ld (iy+0),c     ;5
        inc iy          ;3
clcnum3 djnz clcnum1    ;4/3
clcnum4 ld a,"0"        ;2
        add l           ;1
        ld (iy+0),a     ;5
        ld (iy+1),0     ;5
        ret

;### CLCN32 -> Converts 32Bit number into ASCII string (0-terminated)
;### Input      DE,IX=value, IY=address
;### Output     IY=address of last char
;### Destroyed  AF,BC,DE,HL,IX
clcn32t dw 1,0,     10,0,     100,0,     1000,0,     10000,0
        dw #86a0,1, #4240,#f, #9680,#98, #e100,#5f5, #ca00,#3b9a
clcn32z ds 4

clcn32  ld (clcn32z),ix
        ld (clcn32z+2),de
        ld ix,clcn32t+36
        ld b,9
        ld c,0
clcn321 ld a,"0"
        or a
clcn322 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  sbc hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):sbc hl,de:ld (clcn32z+2),hl
        jr c,clcn325
        inc c
        inc a
        jr clcn322
clcn325 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  add hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):adc hl,de:ld (clcn32z+2),hl
        ld de,-4
        add ix,de
        inc c
        dec c
        jr z,clcn323
        ld (iy+0),a
        inc iy
clcn323 djnz clcn321
        ld a,(clcn32z)
        add "0"
        ld (iy+0),a
        ld (iy+1),0
        ret

;### CLCNRA -> display 32bit number right-aligned
;### Input      DE,IX=value, HL=destination (10 chars)
clcnrab ds 10+1
clcnra  push hl
        ld iy,clcnrab
        call clcn32
        push iy:pop hl
        ld bc,clcnrab-1
        or a
        sbc hl,bc
        ld b,0
        ld c,l          ;bc=digits
        pop hl
        ld a,10
        sub c
        jr z,clcnra3
clcnra2 ld (hl),32
        inc hl
        dec a
        jr nz,clcnra2
clcnra3 ex de,hl
        ld hl,clcnrab
        ldir
        ret

;### CLCDED -> Converts byte into dynamic decimal digits (-3 digits)
;### Input      A=value, HL=string
;### Output     (hl)=last digit
;### Destroyed  AF,DE
clcded  cp 10
        jr c,clcdedc
        ld e,100
        cp e
        jr c,clcdedb
        call clcdeda
clcdedb ld e,10
        call clcdeda
clcdedc add "0"
        ld (hl),a
        ret
clcdeda ld d,"0"-1          ;a=number, e=100/10 -> add digit at (hl), hl=hl+1, a=-digit
clcdedd inc d
        sub e
        jr nc,clcdedd
        add e
        ld (hl),d
        inc hl
        ret

;### CLCDEZ -> Converts byte into decimal digits
;### Input      A=value
;### Output     L=10.digit, H=1.digit
;### Destroyed  AF
clcdez  ld l,"0"
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ret

;### CLCHEX -> Converts byte into hex digits
;### Input      A=value, DE=stringbuffer
;### Output     DE=DE+2
;### Destroyed  AF,C
clchex  ld c,a
        rra
        rra
        rra
        rra
        call clchex1
        ld a,c
clchex1 or #f0
        daa
        add #a0
        adc #40
        ld (de),a
        inc de
        ret

;### CLCTIM -> generates timestamp string ("YY/MM/DD HH.MM.SS")
;### Input      HL=time, BC=date, DE=string (17 chars)
;### Destroyed  AF,BC,DE,HL
clctim  push hl
        ld a,b          ;** date
        srl a               ;A=year-1980
        add 80
        cp 100
        jr c,clctim1
        sub 100
clctim1 call clctim2
        ld a,"/":ld (de),a:inc de
        ld a,c
        rr b
        rra:rra:rra:rra:rra ;A=month
        and 15
        call clctim2
        ld a,"/":ld (de),a:inc de
        ld a,c
        and 31              ;A=day
        call clctim2
        ld a," ":ld (de),a:inc de
        pop bc          ;** time
        ld a,b
        rra:rra:rra
        and 31              ;A=hour
        call clctim2
        ld a,":":ld (de),a:inc de
        ld a,b
        ld b,c
        rl c:rla
        rl c:rla
        rl c:rla
        and 63              ;A=minute
        call clctim2
        ld a,":":ld (de),a:inc de
        ld a,b
        and 31
        add a               ;A=second
clctim2 call clcdez
        ld a,l
        ld (de),a
        inc de
        ld a,h
        ld (de),a
        inc de
        ret

;### TIMUNX -> reads UNIX timestamp
;### Input      DE,HL=UNIX timestamp
;### Output     A=second, B=minute, C=hour, D=day (starting from 1), E=month (starting from 1), HL=year
;### Destroyed  F,IX,IY
timunx980   dw #a600,#12ce      ;difference 1980-1970
timunxyer   dw #3380,#01e1      ;normal year
timunxyel   dw #8500,#01e2      ;leap year
timunxmo8   dw #ea00,#0024      ;month 28 days
timunxmo9   dw #3b80,#0026      ;month 29 days
timunxmo0   dw #8d00,#0027      ;month 30 days
timunxmo1   dw #de80,#0028      ;month 31 days
timunxday   dw #5180,#0001      ;day
timunxhor   dw #0e10,#0000      ;hour
timunxmin   dw #003c,#0000      ;minute

timunxmon   dw timunxmo1,timunxmo8,timunxmo1,timunxmo0,timunxmo1,timunxmo0,timunxmo1,timunxmo1,timunxmo0,timunxmo1,timunxmo0,timunxmo1

timunx  ld ix,timunx980         ;** skip years until 1980
        call clcs32
        jr nc,timunx1
        ld bc,0
        ld de,32*1+1
        ret
timunx1 xor a                   ;** calculate year starting from 1980
        db #fd:ld l,a
timunx2 ld ix,timunxyer
        jr nz,timunx3
        ld ix,timunxyel         ;take leap years into account (year has 365 or 366 days)
timunx3 push de
        push hl
        call clcs32
        jr c,timunx4
        pop bc
        pop bc
        db #fd:inc l            ;iyl=year-1980
        inc a
        and 3
        jr timunx2
timunx4 or a                    ;** calculate month
        ld hl,timunxmo8
        jr nz,timunx5
        ld hl,timunxmo9         ;take leap year into account (february has 28 or 29 days)
timunx5 ld (timunxmon+2),hl
        pop hl
        pop de
        push iy
        ld iy,timunxmon
        xor a
timunx6 inc a                   ;a=month (1-12)
        ld c,(iy+0)
        ld b,(iy+1)
        push bc:pop ix
        push de
        push hl
        call clcs32
        jr c,timunx7
        pop bc
        pop bc
        inc iy
        inc iy
        jr timunx6
timunx7 pop hl
        pop de
        pop bc
        ld b,a                  ;c=year-1980, b=month (1-12)
        push bc
        db #fd:ld l,3           ;** calculate day, hour, minute
        ld ix,timunxday
timunx8 xor a
timunx9 push de
        push hl
        call clcs32
        jr c,timunxa
        pop bc
        pop bc
        inc a
        jr timunx9
timunxa pop hl
        pop de
        ld bc,4
        add ix,bc
        push af
        db #fd:dec l
        jr nz,timunx8
        ld a,l          ;a=second
        pop de
        ld b,d          ;b=minute
        pop de
        ld c,d          ;c=hour
        pop de
        inc d           ;d=day
        pop hl
        ld e,h          ;e=month
        push de
        ld h,0
        ld de,1980
        add hl,de       ;hl=year
        pop de
        ret


;### STRINGS

txtmsgtit   db 13,10
            db "UNZIP 1.0 for SymbOS (c)oded 2015 by Prodatron / SymbiosiS",13,10
            db "  based on ideas & code by Grauw and Wouter",13,10
            db 13,10,0

txterrcmd   db "Unknown command",13,10,0
txterrswt   db "Unknown switch",13,10,0
txterrpri   db "Wrong task priority (must be 0-7)",13,10,0
txterrprc   db "Too many processes",13,10,0
txterrarc   db "No proper archive file specified",13,10,0
txterrpar   db "Missing parameter(s)",13,10
            db "At least a command and the archive has to be specified.",13,10,0
txterrhlp   db 13,10
            db "Please type UNZIP H for help",13,10,13,10,0

txterrtab   dw txterr000,txterr001,txterr002,txterr003,txterr004,txterr005,txterr006,txterr007,txterr008
txterr000   db "Error while opening archive file",13,10,0
txterr001   db "Corrupt archive header",13,10,0
txterr002   db "Corrupt or unsupported archive data",13,10,0
txterr003   db 13,10,13,10,"Error while reading file",13,10,0
txterr004   db "Multi-disc archives not supported",13,10,0
txterr005   db 13,10,13,10,"Error while writing file",13,10,0
txterr006   db 13,10,"Error while creating file",13,10,0
txterr007   db 13,10,13,10,"Exception during decompression",13,10,0
txterr008   db 13,10,"*BREAK*",13,10,0

txtmsghlp1
db "Usage: UNZIP <command> <archive> [<files>...] [<switches>...]",13,10
db 13,10
db "<Commands>",13,10
db "  e   Extract <files> from <archive> (without using directory names)",13,10
db "  l   List <files> of <archive>",13,10
db "  t   Test integrity of <files> in <archive>",13,10,0
txtmsghlp2
db "  x   eXtract <files> from <archive> with full paths",13,10
db "  i   display detailed <files> Information",13,10
db "  h   show Help",13,10
db "<Switches>",13,10
db "  %c:<file> Concatenate output(s) to <file>",13,10
db "  %r  cRc check",13,10,0
txtmsghlp3
db "  %d  hide progress Display",13,10
db "  %t  adopt Timestamps",13,10
db "  %p:<priority> set task Priority (1[high]-7[low]; 5=default)",13,10
db 13,10
db "An archive can be a Gzip (*.GZ) or a Zip (e.g. *.ZIP) file.",13,10
db 13,10,0

txtlstln1   db       "Listing archive: ",0
txtlstln2   db 13,10,"--"
            db 13,10,"Path = ",0
txtlstln3a  db 13,10,"Type = gzip",0
txtlstln3b  db 13,10,"Type = zip",0
txtlstln4   db 13,10,"Physical Size = ",0
txtlstln5   db 13,10
            db 13,10,"  Date     Time         Size Compressed  Name",0
txtlstln6   db 13,10,"----------------- ---------- ----------  ------------------",0
txtlstln7   db 13,10,"xx-xx-xx xx:xx:xx ?????????? ??????????  ",0
txtlstln71  db 13,10,"                  ?????????? ??????????  ",0

txtlstln7t  equ 2
txtlstln7o  equ 2+18
txtlstln7c  equ 2+18+11

txtlstln8   db " file(s)",13,10,13,10,0
txtlstln9   db 13,10,"(no matching file found)",0

txtinfln0   db 13,10,"----------",13,10,0
txtinfln1   db 13,10,"Path = ",0
txtinfln2   db 13,10,"Size = ??????????",0
txtinfln3   db 13,10,"Packed Size = ??????????",0
txtinfln4   db 13,10,"Modified = xx-xx-xx xx:xx:xx",0
txtinfln6   db 13,10,"CRC32 = XXXXXXXX",0
txtinfln7a  db 13,10,"Methode = [unknown]",0
txtinfln7b  db 13,10,"Methode = Store",0
txtinfln7c  db 13,10,"Methode = Deflate",0
txtinfln8   db 13,10,"Version = ?????",0
txtinfln9   db 13,10,0

txtexfln1   db       "Processing archive: ",0
txtexfln2   db 13,10,0
txtexfln3   db 13,10,"Extracting  ",0
txtexfln3a  db 13,10,"Testing  ",0
txtexfln4   db       " as ",0
txtexfln5   db "  ",4,0
txtexfln6   db 5,"[...%/.......K]",0
txtexfln7   db 5,"[.......K, OK]",18,0
txtexfln8   db 5,"[CRC FAILED]",18,0
txtexflnk   db 5,"[SIZE MISMATCH]",18,0
txtexflna   db 5,"[CORRUPT]",18,0
txtexflnh   db "  [IGNORED]",0
txtexflni   db "  [METHOD N/A]",18,0
txtexflnj   db "  [FEATURE N/A]",18,0

txtexflnbt  db "Tested:   "
txtexflnb   db 13,10,18
            db 13,10
txtexflnb1  db "Extracted: ":txtexflnb0 db "#####",0
txtexflnc   db      "  (Size: ":txtexflnc0 db "##########",0
txtexflnd   db ", Compressed: ":txtexflnd0 db "##########)",0
txtexflne   db 13,10,"Skipped:   ":txtexflne0 db "#####",0
txtexflnf   db 13,10,"Corrupt:   ":txtexflnf0 db "#####",0
txtexflng   db 13,10,13,10,0
txtexflnl   db 13,10,"Concatenate to: ",0

;### COMMAND LINE DATA

prgparcmd   db 0    ;command letter
prgparfil   db 0    ;number of filemasks

prgparflt   dw prgparfl0,0
            dw prgparfl1,0
            dw prgparfl2,0
            dw prgparfl3,0
            dw prgparfl4,0
            dw 0

prgparfl0   db "c:"     ;concatenate to file
prgparfl1   db "r",0    ;no CRC check
prgparfl2   db "p:"     ;set task priority
prgparfl3   db "d",0    ;hide progress display
prgparfl4   db "t",0    ;adopt timestamp

;Flags
prgparflg   dw 0        ;flags

uzidspflg   db 0        ;flag, if display progress
uzitstflg   db 0        ;flag, if test archive
uzitimflg   db 0        ;flag, if adopt timestamp
uzicatflg   db 0        ;flag, if output is concatenate to a single file
uziprioid   db 5        ;priority
uzifilmod   db 0        ;0=non-directory mode, 1=directory mode

;### INPUT/OUTPUT FILEDATA

inpfilpth   ds 256      ;input filepath
inpfilhnd   db 0        ;input file handler
inpfilnam   dw 0        ;input filename pointer

outfilpth   ds 256      ;output directory
outfilhnd   db 0        ;output file handler
outfilnam   dw 0        ;output filename pointer (full additional path)
outfilnul   db 0
outfilpos   dw 0        ;output filename pointer (filename only)

;### ARCHIVE DIRECTORY DATA

uzifilsiz   ds 4        ;total file size
uzifilbuf   ds 512      ;file input buffer

uzicprmet   db 0        ;*\ compression method (0=unknown, 1=stored, 2=deflated)
uzisupflg   db 0        ;*/ >0 -> not supported
uzilencur   ds 4        ;*\ current extracted length
uzilenorg   ds 4        ;*/ length original
uzilencpr   ds 4        ;length compressed
uzitimstp   ds 4        ;DOS timestamp of actual directory entry
uzicrc32    ds 4        ;CRC32
uzicprofs   ds 4        ;compressed data file offset (may include a header [for ZIP archives])
uziverned   dw 0        ;version needed

uzifilnam   ds 256      ;filename of actual directory entry


ds -$ mod 256
buf_read    equ $-256   ;###last label in data area!###


;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

App_BegTrns
            ds 64
prgstk      ds 6*2
            dw prgprz
App_PrcID   db 0
App_MsgBuf  ds 14
