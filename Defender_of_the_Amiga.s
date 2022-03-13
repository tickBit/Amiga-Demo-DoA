
; This can be compiled with at least with PhxAss assembler
; I hope, it can be still found in the Aminet

; It's been long time since by so called active Amiga days.
; This is first bigger project after about 20 years of break...
; ...so the code is quite messy
;
; Forgive...
;



        MACHINE  68040

;        incdir  "ADCD_2.1:NDK/NDK_3.5/Include/include_i"

;        include "exec/types.i"
;        include "libraries/dosextens.i"
;        include "graphics/gfxbase.i"
;        include "exec/libraries.i"
;        include "exec/execbase.i"

; Amiga Developer CD v1.1 can be freely downloaded from the internet
; Use this instead, if you don't have ADCD_2.1

; ..but I guess I have used the DevPack from AmiKit project...

        incdir   "Work:Dev/DevPack/SDK/NDK_3.9/include/include_i"
        include "exec/types.i"
        include "libraries/dosextens.i"
        include "graphics/gfxbase.i"
        include "exec/libraries.i"
        include "exec/execbase.i"
        include "dos/dos.i"                


        include "include/libraries/playsid_lib.i"
        include "include/libraries/playsidbase.i"

;------------------------------------------------------------------------------
; Macros
;------------------------------------------------------------------------------
WaitForBlitter: MACRO
W\@:    btst.w  #14,$dff002
        bne.s   W\@
        ENDM

;------------------------------------------------------------------------------
; Constants
;------------------------------------------------------------------------------
Forbid         equ     -$0084
Permit         equ     -$008a
Disable        equ     -$0078
Enable         equ     -$007e
Write          equ     -$0030
Output         equ     -$003c
OpenLibrary    equ     -$0228
CloseLibrary   equ     -$019e

;---- Playroutine ----

n_note		EQU	0  ; W
n_cmd		EQU	2  ; W
n_cmdlo		EQU	3  ; B
n_start		EQU	4  ; L
n_length	EQU	8  ; W
n_loopstart	EQU	10 ; L
n_replen	EQU	14 ; W
n_period	EQU	16 ; W
n_finetune	EQU	18 ; B
n_volume	EQU	19 ; B
n_dmabit	EQU	20 ; W
n_toneportdirec	EQU	22 ; B
n_toneportspeed	EQU	23 ; B
n_wantedperiod	EQU	24 ; W
n_vibratocmd	EQU	26 ; B
n_vibratopos	EQU	27 ; B
n_tremolocmd	EQU	28 ; B
n_tremolopos	EQU	29 ; B
n_wavecontrol	EQU	30 ; B
n_glissfunk	EQU	31 ; B
n_sampleoffset	EQU	32 ; B
n_pattpos	EQU	33 ; B
n_loopcount	EQU	34 ; B
n_funkoffset	EQU	35 ; B
n_wavestart	EQU	36 ; L
n_reallength	EQU	40 ; W

; Protracker replayroutine related

;---- CIA Interrupt ----

AddICRVector	=   -6
RemICRVector	=  -12
LVOOpenResource	= -498
LVOOpenLibrary 	= -552
LVOCloseLibrary	= -414
LVODelay	= -198

ciatalo = $400
ciatahi = $500
ciatblo = $600
ciatbhi = $700
ciacra  = $E00
ciacrb  = $F00

Execbase       equ      4

NOCHIPREV      equ      0

;------------------------------------------------------------------------------
; Startup code, works with AGA machnies. I found this back in the day somewhere
;------------------------------------------------------------------------------

        SECTION CODE,code

startup:
        movem.l d0/a0,-(sp)             ; 
        move.l  4,a6                    ; SysBase
        move.l  #0,a1
        jsr     -$0126(a6)              ; 
        move.l  d0,a4
        move.l  d0,process
        tst.l   pr_CLI(a4)              ; CLI?
                                        ; 
        bne.s   check_aga               ; check_aga
wb:
        lea     pr_MsgPort(a4),a0       ; 
                                        ; 
        jsr     -$0180(a6)              ; 
        lea     pr_MsgPort(a4),a0
        jsr     -$0174(a6)              ; 
                                        ; GetMsg()
        move.l  d0,wbenchmsg            ; 
                                        ; 
check_aga:                              ; 
        moveq   #0,d0                   ; 
        lea     gfxname,a1
        jsr     -$0228(a6)              ; OpenLibrary()
        move.l  d0,gfxbase
        beq.w   reply_to_wb             ; 
        move.l  d0,a4

        moveq   #0,d0
        lea     intuiname,a1
        jsr     -$0228(a6)
        move.l  d0,intuibase
        beq     Sulje

;        move.l  4,a6
;        jsr     -$0078(a6)              ; Disable()
        cmp.w   #39,LIB_VERSION(a4)     ; 
                                        ; cmp.w #39,$14(a4)
        bne.s   no_chiprev

        move.b  gb_ChipRevBits0(a4),chiprev
                                        ; move.b $ec(a4),chiprev
        bra.s   check_proc
no_chiprev:
        move.b  #NOCHIPREV,chiprev      ; 
check_proc:
        move.w  AttnFlags(a6),processor ; CPU and FPU
                                        ; move.w $128(a6),processor
clear_view:
        move.l  gfxbase,a6
        move.l  gb_ActiView(a6),oldview ; 
                                        ; 
        move.l  #0,a1                   ; 
        jsr     -$00de(a6)              ; 

        jsr     -$010e(a6)
        jsr     -$010e(a6)              ; WaitTOF()

        move.l  4,a6                    ; 
        movem.l (sp)+,d0/a0             ; 
        bsr.s   _start                  ;
        move.l  d0,-(sp)                ; 
old_view:
        move.l  gfxbase,a0
        move.l  $26(a0),$dff080         ; Copperlistan palautus

        move.l  gfxbase,a6
        move.l  oldview,a1              ; old View
        jsr     -$00de(a6)              ; LoadView()
                                                                                                         
;        move.l  4,a6
;        jsr     -$007e(a6)              ; Enable()
                                                                                                        
        move.l  intuibase,a6
        jsr     -$0186(a6)              ; RethinkDisplay()

        move.l  4,a6
        move.l  intuibase,a1
        jsr     -$019e(a6)              ; CloseLibrary()

Sulje   move.l  4,a6
        move.l  gfxbase,a1              ;
        jsr     -$019e(a6)              ; CloseLibrary()
                                                                                                         
reply_to_wb:
        tst.l   wbenchmsg               ; workbench?
        beq.s   exit                    ; 
        ;jsr     -$0084(a6)              ; 
                                        ; Forbid()
        move.l  wbenchmsg,a1
        jsr     -$017a(a6)              ; ReplyMsg()
exit:
        move.l  (sp)+,d0
        rts                             ; 


_start

;------------------------------------------------------------------------------
; The program starts..
;------------------------------------------------------------------------------

        movem.l d0-d7/a0-a6,-(sp)

        move.l  4,a6
        jsr     Forbid(a6)

       

                lea     dosname,a1
                moveq   #0,d0
                move.l  4,a6
                jsr     -$0228(a6)          ; Exec/OpenLibrary
                move.l  d0,dosbase
                beq     CleanUp

                move.l  dosbase,a6
                jsr     -$003c(a6)          ; Dos/Output
                move.l  d0,outfile

                lea     sidnimi,a1
                moveq   #0,d0
                move.l  4,a6                
                jsr     -$0228(a6)          ; Exec/OpenLibrary
                move.l  d0,sidbase
                beq     CleanUp

                move.l  #filename,d1
                move.l  #1005,d2
                move.l  dosbase,a6
                jsr     -$001e(a6)                  ; Dos/Open
                move.l  d0,fh
                beq    CleanUp


                move.l  fh,d1
                move.l  #0,d2
                move.l  #OFFSET_END,d3
                move.l  dosbase,a6
                jsr     -$0042(a6)                  ; Dos/Seek


                move.l  fh,d1
                move.l  #0,d2
                move.l  #OFFSET_BEGINNING,d3
                move.l  dosbase,a6
                jsr     -$0042(a6)                  ; Dos/Seek
                move.w  d0,size


 


                move.l  #32*4,d0
                move.l  #1,d1
                move.l  4,a6
                jsr     -$00c6(a6)          ; Exec/AllocMem
                move.l  d0,header
                beq     CleanUp

              
                lea     filename,a0
                move.l  header,a1
                move.l  sidbase,a6
                jsr     _LVOReadIcon(a6)
                move.l  d0,ri

                move.l  header,a1
                move.l  sidbase,a6
                jsr    _LVOCheckModule(a6)
                move.l d0,cm

                moveq   #0,d0
                move.w  size,d0
                move.l  #1,d1
                move.l  4,a6
                jsr     -$00c6(a6)          ; Exec/AllocMem
                move.l  d0,sidfile
                beq     CleanUp


                move.l  fh,d1
                move.l  sidfile,d2              
                moveq   #0,d3
                move.w  size,d3
                move.l  dosbase,a6
                jsr     -$002a(a6)          ; Dos/Read
                move.l  d0,fileread
                beq     CleanUp

                move.l  fh,d0
                beq     nxt5         
                move.l  d0,d1
                move.l  dosbase,a6
                jsr    -$0024(a6)               ; Dos/Close

        move.l  4,a6
        move.l  #(134*80)*8,d0        ; 8 bitplanes, 640 x 134
        move.l  #65538,d1
        jsr     -$00c6(a6)
        move.l  d0,bitmap01
        beq     CleanUp


        move.l  4,a6
        move.l  #122*40*4,d0        ; 4 bitplanes, 320 x 122
        move.l  #65538,d1
        jsr     -$00c6(a6)
        move.l  d0,bitmap1
        beq     CleanUp

        move.l  4,a6
        move.l  #122*40*4,d0        ; 4 bitplanes, 320 x 122
        move.l  #65538,d1
        jsr     -$00c6(a6)
        move.l  d0,bitmap2
        beq     CleanUp

        move.l  4,a6
        move.l  #1536*4,d0               ; 384 * 32 * 4 chip mem area (scroll)
        move.l  #65538,d1
        jsr     -$00c6(a6)
        move.l  d0,scrollarea
        beq     CleanUp

        move.l  4,a6
        move.l  #10240*4*2,d0              ; 320 * 256 * 4 chip mem area (points)
        move.l  #65538,d1
        jsr     -$00c6(a6)
        move.l  d0,screens
        beq     CleanUp

        move.l  4,a6
        move.l  #10240*2,d0               ; 320 * 256 chip mem area (points)
        move.l  #65538,d1
        jsr     -$00c6(a6)
        move.l  d0,omascreen
        beq     CleanUp


        move.l  4,a6
        move.l  #(159*40)*5,d0        ; 8 bitplanes, 320 x 159
        move.l  #65538,d1
        jsr     -$00c6(a6)
        move.l  d0,modelBitmap
        beq     CleanUp
        

main	

        lea     $dff000,a5

        WaitForBlitter
        move.w  #$ffff,$dff044
        move.w  #$ffff,$dff046
        move.w  #$0000,$dff064   ; BLTAMOD (source modulo)
        move.w  #$0000,$dff066   ; BLTDMOD (dest modulo)
        move.w  #$0000,$dff042   ; BLTCON1
        move.w  #$09f0,$dff040   ; BLTCON0
        
       
        move.w  #$83c0,$dff096         ; DMACON
        move.w  #$0420,$dff096         ; DMACON (sprites off)
                                       ; Sprites must be explicitly set off, if they're
                                       ; not used, otherwise one gets "phantom graphics" flickering
                                       ; on the screen...



        move.l  #StartCopper,$dff080
        tst.w   $dff088                ; Own Copperlist on..
        
        BSR 	SetCIAInt
	    BSR	mt_init
	    ST	mt_Enable
 
        
        move.l  #Together,d0
        move.w  d0,low01
        swap    d0
        move.w  d0,high01
        swap    d0
        add.l   #10240,d0
        move.w  d0,low02
        swap    d0
        move.w  d0,high02
        swap    d0
        add.l   #10240,d0
        move.w  d0,low03
        swap    d0
        move.w  d0,high03
        swap    d0
        add.l   #10240,d0
        move.w  d0,low04
        swap    d0
        move.w  d0,high04
        swap    d0
        add.l   #10240,d0
        move.w  d0,low05
        swap    d0
        move.w  d0,high05        
        swap    d0
        add.l   #10240,d0
        move.w  d0,low06
        swap    d0
        move.w  d0,high06
        swap    d0
        add.l   #10240,d0
        move.w  d0,low07
        swap    d0
        move.w  d0,high07
        swap    d0
        add.l   #10240,d0
        move.w  d0,low08
        swap    d0
        move.w  d0,high08


        lea     $dff000,a5

        WaitForBlitter
        move.w  #$ffff,$dff044
        move.w  #$ffff,$dff046
        move.w  #$0000,$dff064   ; BLTAMOD (source modulo)
        move.w  #$0000,$dff066   ; BLTDMOD (dest modulo)
        move.w  #$0000,$dff042   ; BLTCON1
        move.w  #$09f0,$dff040   ; BLTCON0
        

        moveq   #0,d1
        
        
        
alkuScrollSil
        
        moveq   #0,d6
        lea     starttext,a0
        add.l   at_pointer,a0
        moveq   #0,d7

ksil    cmp.b   #1,(a0)
        beq.s   vaihda
        cmp.b   #0,(a0)
        beq.s   kesk
        addq.l  #1,d7
        addq.l  #1,a0
        addq.l  #1,at_pointer
        bra.s   ksil

kesk    sub.l   d7,a0
        move.l  #40,d5
        sub.l   d7,d5
        lsr.l   #1,d5       ; arvo, jolla teksti keskitetään

lsil    moveq   #0,d0
        cmp.b   #0,(a0)
        beq.s   sil
        move.b  (a0)+,d0
        
        move.l  d0,d1
        sub.l   #32,d1
        cmp.l   #32,d1
        blt.s   eka
        
        move.l  d0,d1
        sub.l   #64,d1
        cmp.l   #32,d1
        blt.s   toka
        add.l   #192+32+32+32+32+32+32+32+32,d0
        bra.s   goon
eka     sub.l   #32,d0      
        bra.s   goon
toka    add.l   #192,d0
        

goon   
        
        bsr     drawLittleLetter
        bra.s   lsil


        moveq   #0,d1
sil     bsr     scrollFirstPart


        addq.l  #1,at_pointer

        

        bra.s   alkuScrollSil


vaihda  
        bsr     ExtraWaitForBeam

        move.l  #Copperlist,$dff080
        tst.w   $dff088                ; Own Copperlist on..



        fmove.x  #0,fp7                ;
        fmove.x  #0.004363*180.0,fp1   ; Approximation in radians of (PI / 4) * screenwidth
                                        
        fdiv.x  #180.0,fp1

        bsr     SwapScreens

        WaitForBlitter

        move.w  #0,$064(a5)  ; BLTAMOD (source modulo)
        move.w  #60-40,$066(a5)  ; BLTDMOD (dest modulo)

        
        move.w  #$0000,$042(A5) ; BLTCON1
      
        move.w  #$09f0,$040(A5)     ; BLTCON0
        
        move.l  #Lady,$050(A5)     ; BLTAPTH source
        
        
        MOVE.L  bitmap01,A0
        add.l   #30,a0
        move.l  a0,$054(A5)     ; BLTDPTH dest
        moveq   #0,d0
        move.w  #134,d0         ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #10,d0         ; width / 16
        move.w  d0,$058(A5)     ; BLTSIZE

        add.l   #5360,a0
        
        WaitForBlitter
        move.l  #Lady+2680,$050(a5)      ; BLTAPTR source
       
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #5360,a0
        
        WaitForBlitter
        move.l  #Lady+2680*2,$050(a5)      ; BLTAPTR source
        
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #5360,a0
       
        WaitForBlitter
        move.l  #Lady+2680*3,$050(a5)      ; BLTAPTR source
        
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #5360,a0

        WaitForBlitter
        move.l  #Lady+2680*4,$050(a5)      ; BLTAPTR source
       
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #5360,a0
        WaitForBlitter
        move.l  #Lady+2680*5,$050(a5)      ; BLTAPTR source
       
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #5360,a0
        WaitForBlitter
        move.l  #Lady+2680*6,$050(a5)      ; BLTAPTR source
       
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #5360,a0
        WaitForBlitter
        move.l  #Lady+2680*7,$050(a5)      ; BLTAPTR source
       
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

MainProgram:
        bsr     SwapScreens
        bsr     Show2
        bsr     ClearScreen
    	bsr     Scroll
        bsr     SinEffect

        bsr     colorBar0
        bsr     colorBar1
        bsr     colorBar2
        bsr     colorBar3

        bsr     WaitForBeam
        move.l  #fontw,a0       ; Font width
        subq.l  #1,(a0)
        cmp.l   #0,(a0)         ; is it time to draw next letter?
        bne.s   mousebutton
        bsr     drawLetter    
        cmp.b   #1,lastPart
        beq.s   vaihaPointPart
        
mousebutton
        bsr     LMB
        cmp.b   #1,buttonreleased      ; Left mousebutton to exit the app
        bne.s   MainProgram
    

vaihaPointPart
        lea     $dff000,a5
        fmove.x  #3.14159265,fp7      ; Pi
        fdiv.x   #180.0,fp7           ; One degree
        fmove.x  fp7,fp6
        fmul.x   #1.5,fp7

        fmove.x  #10.0,fp5

        bsr     WaitForBeam
        bsr     WaitForBeam

        bsr     ExtraWaitForBeam

        move.l  #PointCopper,$dff080
        tst.w   $dff088                ; Own Copperlist on..

        move.l  #0,d6

MainPoint:
        cmp.b   #1,vikaosa
        beq     stretchosa

        ;bsr     SwapScreensP
        bsr     ShowP

        bsr     DrawPoint
  
        bsr     WaitForBeam
        
        bsr     LMB
        cmp.b   #1,buttonreleased      ; Left mousebutton to exit the app
        bne.s   MainPoint

        bra     stretchosa
 

CleanUp
                move.l  sidbase,a6
                jsr     _LVOStopSong(a6)               

                move.l  header,d0
                beq     nxt6
                move.l  d0,a1
                move.l  #32*4,d0
                move.l  4,a6
                jsr     -$00d2(a6)              ; Exec/FreeMem
nxt6
                
nxt5
                move.l dosbase,d0
                beq    nxt
                move.l d0,a1
                move.l 4,a6
                jsr    -$019e(a6)               ; exec/CloseLibrary

nxt             move.l sidfile,d0
                beq    nxt2
                moveq  #0,d0
                move.w  size,d0
                move.l sidfile,a1
                move.l  4,a6
                jsr    -$00d2(a6)               ; exec/FreeMem
                
nxt2                
                move.l emulrc,d0
                cmp.l  #-1,d0
                beq.s  nxt4
                move.l sidbase,a6
                jsr    _LVOFreeEmulResource(a6)

nxt4            

                move.l sidbase,d0
                beq    nxt2
                move.l d0,a1
                move.l 4,a6
                jsr   -$019e(a6)                ; exec/CloseLibrary




nxt3

Freebitplane1
        move.l  4,a6
        move.l  #(134*80*8),d0
        move.l  bitmap01,a1
        jsr     -$00d2(a6)

        move.l  4,a6
        move.l  #122*40*4,d0
        move.l  bitmap1,a1
        jsr     -$00d2(a6)

        move.l  4,a6
        move.l  #122*40*4,d0
        move.l  bitmap2,a1
        jsr     -$00d2(a6)

        move.l  4,a6
        move.l  #1536*4,d0
        move.l  scrollarea,a1
        jsr     -$00d2(a6)

        move.l  4,a6
        move.l  #10240*4*2,d0
        move.l  screens,a1
        jsr     -$00d2(a6)

        move.l  4,a6
        move.l  #10240*2,d0
        move.l  omascreen,a1
        jsr     -$00d2(a6)

        move.l  4,a6
        move.l  #(159*40)*5,d0 
        move.l  modelBitmap,a1
        jsr     -$00d2(a6)

Exit:   

        move.l  4,a6
        jsr     Permit(a6)

        movem.l (sp)+,d0-d7/a0-a6
        moveq   #0,d0
        rts

scrollFirstPart:
       
        move.l   #53-1,d7
scrollLoop:


        WaitForBlitter

        move.l  #Together+40*202,$050(a5)     ; BLTAPTH source
        move.l  #Together+40*201,$054(a5)     ; BLTDPTH dest
        moveq   #0,d0
        move.w  #51,d0        ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #20,d0         ; width / 16
        move.w  d0,$058(a5)     ; BLTSIZE

        WaitForBlitter

        move.l  #Together+40*202+10240,$050(a5)     ; BLTAPTH source
        move.l  #Together+40*201+10240,$054(a5)     ; destination
        moveq   #0,d0
        move.w  #51,d0        ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #20,d0         ; width / 16
        move.w  d0,$058(a5)     ; BLTSIZE

        WaitForBlitter

        move.l  #Together+40*202+10240*2,$050(a5)     ; BLTAPTH source
        move.l  #Together+40*201+10240*2,$054(a5)     ; destination
        moveq   #0,d0
        move.w  #51,d0        ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #20,d0         ; width / 16
        move.w  d0,$058(a5)     ; BLTSIZE

        WaitForBlitter

        move.l  #Together+40*202+10240*3,$050(a5)     ; BLTAPTH source
        move.l  #Together+40*201+10240*3,$054(a5)     ; destination
        moveq   #0,d0
        move.w  #51,d0        ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #21,d0         ; width / 16
        move.w  d0,$058(a5)     ; BLTSIZE

        WaitForBlitter

        move.l  #Together+40*202+10240*4,$050(a5)     ; BLTAPTH source
        move.l  #Together+40*201+10240*4,$054(a5)     ; destination
        moveq   #0,d0
        move.w  #51,d0        ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #20,d0         ; width / 16
        move.w  d0,$058(a5)     ; BLTSIZE

 
        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam
        ;bsr     WaitForBeam
       

        dbf     d7,scrollLoop

        rts

drawLittleLetter
        move.l  #LittleFont,a1
        add.l   d0,a1
        moveq   #8-1,d7
        moveq   #0,d4
dlsil   move.l  #Together,a2
        add.l   d6,a2           ; 8 px oikealle
        add.l   d4,a2           ; seuraava kirjaimen rivi (dest)
        add.l   d5,a2           ; keskitys
        add.l   #40*(255-10),a2  ; tulostus alimmalle riville
        move.b  (a1),(a2)
        
        add.l   #10240,a2
        move.b  (a1),(a2)
        add.l   #10240,a2
        move.b  (a1),(a2)
        add.l   #10240,a2
        move.b  (a1),(a2)
        add.l   #10240,a2
        move.b  (a1),(a2)
        add.l   #32,a1          ; seuraava kirjaimen rivi (source)
        add.l   #40,d4
        dbf     d7,dlsil

        addq.l  #1,d6
        rts


drawLittleLetter2
        move.l  #LittleFont,a1
        add.l   d0,a1
        moveq   #8-1,d7
        moveq   #0,d4
dlsill  move.l  screens,a2
        add.l   d6,a2           ; 8 px oikealle
        add.l   d4,a2           ; seuraava kirjaimen rivi (dest)
        add.l   d5,a2           ; keskitys
        add.l   #40*(255-9),a2  ; tulostus alimmalle riville
        move.b  (a1),(a2)
        
        add.l   #32,a1          ; seuraava kirjaimen rivi (source)
        add.l   #40,d4
        dbf     d7,dlsill

        addq.l  #1,d6
        rts

stretchosa
        move.b  #0,vikaosa

        move.l  screens,a0
        move.l  #2560-1,d7
clrst   move.l  #0,(a0)+
        dbf     d7,clrst

        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     ExtraWaitForBeam


        move.l  #StretchCopper,$dff080
        tst.w   $dff088                ; Own Copperlist on..


; Bitplane pointterit
        move.l   screens,d0
        move.w   d0,low1s
        swap     d0
        move.w   d0,high1s

        fmove.x  #0,fp7                ; 
        fmove.x  #0.017*180.0,fp1
        fdiv.x   #180.0,fp1

stretchMain
        cmp.b   #1,vikaosa
        beq     starsosa

        cmp.b   #1,stretchi
        bne.s   waittar

        lea     $dff000,a5
        bsr     StretchIt
        
        bsr     WaitForBeam
        bra.s   lmb

waittar
        bsr     drawStretchLine
        bsr     WaitForBeam

        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam
               
lmb     ; no LMB here...
        bra.s   stretchMain

        bra     starsosa

StretchIt: 
        move.l   #Stretch,a0    ; The CHIP mem area in the Copperlist
                                ; where the data is to be written

        fmove.x  fp7,fp0
        fsin.x   fp0            ; values [-1,1]
        fmul.x   #32,fp0        ; min value -64, max value 64
        fadd.x   #32,fp0        ; we don't want negative values now
        fmove.x  fp0,fp2
        fdiv.x   #128,fp2       ; divide the max y-coordinate by the
                                ; number of rows, we will wait in the copperlist
        
        

        
        move.l   #64-1,d4  ; 32 * 6 words to be written into the copperlist
        moveq    #0,d2
        move.w   #$009a,d2     ; the starting waitline in the copperlist 
        fmove.x  #0,fp3
loop
        move.l   screens,d3   ; bitplane pointer
        add.l    #40*100,d3
        fadd.x   fp2,fp3        ; fp2 is the step we move in the bitplane pointer
                                ; in y-direction
        
        fmove.l  fp3,d6
        

        muls     #40,d6         ; multiply by width of the screen in bytes
                               
        add.l    d6,d3          ; add the previous value to the bitplane pointer's start

        moveq    #0,d5
        move.w   d2,d5          ; move copy of the copperlist's wait position to d5
        lsl.w    #8,d5          ; shift the wait position to appropriate bits
        or.w     #%0000000000000001,d5  ; "add" the missing 1 for the wait command of Copper
        
        move.w   d5,(a0)+       ; write wait position to coppelist
        move.w   #$ff00,(a0)+   ; write wait command to copperlist

        move.w   #$00e0,(a0)+   ; write BPL1PTH to copperlist
        swap     d3             ; we write first the high word, that's why we must swap d3
        move.w   d3,(a0)+       ; write the high word of the bitplane pointer's address to copperlist
        move.w   #$00e2,(a0)+   ; write BPL1PTL to copperlist
        swap     d3             ; next we'll write the low word of the bitplane pointer, swap again
        move.w   d3,(a0)+       ; write the low word of the bitplane pointer's address to copperlist
        
        addq.l   #1,d2          ; add by 1 the wait position for the Copper
        
        dbf      d4,loop

copperend:

; Finally, we write the looping wait instruction to the copperlist

        move.w   #$ffff,Stretch+1536
        move.w   #$fffe,Stretch+1536+2

        fadd.x   fp1,fp7        ; add the "angle" by the value in fp1
        fcmp.x   #63,fp0
        fblt.x   ertees
        fmove.x  #0,fp7                ; 
        fmove.x  #0.017*180.0,fp1
        fdiv.x   #180.0,fp1
        move.b   #0,stretchi

        move.l  screens,a0
        move.l  #2560-1,d7
clrsts  move.l  #0,(a0)+
        dbf     d7,clrsts

ertees
        rts

drawStretchLine
        moveq   #0,d6
        lea     stretchText,a0
        move.l  st_pointer,d0
        add.l   d0,a0
        moveq   #0,d7

ksills  cmp.b   #2,(a0)
        beq.s   changeSID

        cmp.b   #1,(a0)
        bne.s   ei_viimeinenosa
        move.b  #1,vikaosa
        rts

ei_viimeinenosa
        cmp.b   #0,(a0)
        beq.s   keskls
        addq.l  #1,d7
        addq.l  #1,a0
        addq.l  #1,st_pointer
        bra.s   ksills

keskls  sub.l   d7,a0
        move.l  #40,d5
        sub.l   d7,d5
        lsr.l   #1,d5       ; arvo, jolla teksti keskitetään

lsills  moveq   #0,d0
        cmp.b   #0,(a0)
        beq.s   sills
        move.b  (a0)+,d0
        
        move.l  d0,d1
        sub.l   #32,d1
        cmp.l   #32,d1
        blt.s   ekals
        
        move.l  d0,d1
        sub.l   #64,d1
        cmp.l   #32,d1
        blt.s   tokals
        add.l   #192+32+32+32+32+32+32+32+32,d0
        bra.s   goonls
ekals   sub.l   #32,d0      
        bra.s   goonls
tokals  add.l   #192,d0
        

goonls    
        bsr     drawLittleLetter3
        bra.s   lsills


        moveq   #0,d1
sills   addq.l  #1,st_pointer

        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam

        move.b  #1,stretchi
        rts

changeSID
        BSR	mt_end
	    BSR	ResetCIAInt

        move.l  sidbase,a6
        jsr     _LVOAllocEmulResource(a6)
        move.l  d0,emulrc

        move.l  sidbase,a6
        move.l  header,a0
        move.l  sidfile,a1
        moveq   #0,d0
        move.w  size,d0
        jsr     _LVOSetModule(a6)       

        move.l  sidbase,a6
        moveq   #0,d0
        move.w  #50,d0
        jsr     _LVOSetVertFreq(a6)

        lea     channels,a1
        jsr     _LVOSetChannelEnable(a6)

        
        move.l  sidbase,a6
        moveq   #0,d0       ; tune
        jsr     _LVOStartSong(a6)

skipSID

        addq.l  #1,st_pointer
        move.b  #1,stretchi

        rts


drawLittleLetter3
        move.l  screens,a2
        add.l   #40*100,a2
        move.l  #40*8-1,d7
clt     move.l  #0,(a2)
        dbf     d7,clt
        
        move.l  #LittleFont,a1
        add.l   d0,a1
        moveq   #8-1,d7
        moveq   #0,d4
dlsills move.l  screens,a2
        add.l   d6,a2           ; 8 px oikealle
        add.l   d4,a2           ; seuraava kirjaimen rivi (dest)
        add.l   d5,a2           ; keskitys
        add.l   #40*100,a2      ; tulostus tälle riville..
        move.b  (a1),(a2)
        
        add.l   #32,a1          ; seuraava kirjaimen rivi (source)
        add.l   #40,d4
        dbf     d7,dlsills

        addq.l  #1,d6
        rts

ClearScreen:
        move.l DrawScreen,a0
        move.l #clears,a1
        move.l #(1220)-1,d7

; clear all 4 bitplanes of the scrolling area

clear1
        move.l (a1)+,(a0)+
        sub.l  #4,a1
        dbf    d7,clear1

        move.l DrawScreen,a0
        add.l  #4880,a0
        move.l #(1220)-1,d7
clear2
        move.l (a1)+,(a0)+
        sub.l  #4,a1
        dbf    d7,clear2

        move.l DrawScreen,a0
        add.l  #4880*2,a0
        move.l #(1220)-1,d7
clear3
        move.l (a1)+,(a0)+
        sub.l #4,a1
        dbf    d7,clear3

        move.l DrawScreen,a0
        add.l  #4880*3,a0
        move.l #(1220)-1,d7
clear4
        move.l (a1)+,(a0)+
        sub.l #4,a1
        dbf    d7,clear4

        rts

drawLetter
        move.l  #32,fontw       ; font width
        move.l  #t_pointer,a2
        move.l  (a2),d0
        cmp.b   #1,modelPart
        beq.s   onModelPart
        move.l  #scrolltext,a4
        bra.s   skipOnModelPart
onModelPart
        move.l  #modelScrolltext,a4
skipOnModelPart
        add.l   d0,a4
        moveq   #0,d2
        move.b  (a4),d2
        cmp.b   #0,(a4)         ; end of text?
        bne.s   space_
        move.l  #0,t_pointer
        move.l  #t_pointer,a2
        move.l  #scrolltext,a4
        move.b  (a4),d2
        move.b  #1,lastPart
        rts

space_:
        cmp.b   #32,d2          ; space
        beq.s   space

subtract_A_ascii
      
        sub.l   #65,d2          ; the ASCII code of 'A'
        lsl.l   #7,d2           ; multiply by 128

        addq.l  #1,(a2)         ; next drawLetter

        WaitForBlitter
        
        move.w  #$0000,$042(a5)  ; BLTCON1
        move.w  #$09f0,$040(a5)  ; BLTCON0

        move.l  #Font,a1
        add.l   d2,a1
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  scrollarea,a0
        add.l   #40,a0
        move.l  a0,$054(a5)      ; BLTDPTH dest

        move.w  #0000,$064(a5)   ; BLTAMOD (source modulo)
        move.w  #0044,$066(a5)   ; BLTDMOD (dest modulo)

        moveq   #0,d0
        move.w  #32,d0          ; height
        lsl.w   #6,d0           ; height to appropriate bits
        or.w    #2,d0           ; width / 16
        move.w  d0,$058(a5)      ; BLTSIZE       

; bitplane 2

        add.l   #1536,a0
        add.l   #5504,a1
        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest        
        move.w  d0,$058(a5)

; bitplane 3

        add.l   #1536,a0
        add.l   #5504,a1
        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #1536,a0
        add.l   #5504,a1

; bitplane 4

        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        rts



WaitForBeam:
        cmp.b   #$ff,$dff006
        bne.s   WaitForBeam
WFBeam:
        cmp.b   #$2c,$dff006
        bne.s   WFBeam
        rts

ExtraWaitForBeam
        move.w  $004(a5),d0
        btst.l  #0,d0                   
        beq.s   ExtraWaitForBeam
        cmp.b   #$2d,$006(a5)
        bne.s   ExtraWaitForBeam
        rts

space:
        addq.l  #1,(a2)
        WaitForBlitter
        
        move.w  #$0000,$042(a5)  ; BLTCON1
        move.w  #$09f0,$040(a5)  ; BLTCON0

        move.l  #spaceg,a1
        move.l  a1,$050(a5)      ; BLTAPTH source
        move.l  scrollarea,a0
        add.l   #40,a0
        move.l  a0,$054(a5)      ; BLTDPTH dest

        move.w  #0000,$064(a5)   ; BLTAMOD (source modulo)
        move.w  #0044,$066(a5)   ; BLTDMOD (dest modulo)

        moveq   #0,d0
        move.w  #32,d0          ; height
        lsl.w   #6,d0           ; height to approriate bits
        or.w    #2,d0           ; width / 16
        move.w  d0,$058(a5)      ; BLTSIZE       

        add.l   #1536,a0
        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest        
        move.w  d0,$058(a5)

        add.l   #1536,a0
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest        
        WaitForBlitter
        move.w  d0,$058(a5)

        add.l   #1536,a0
        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest        
        move.w  d0,$058(a5)

        rts

Scroll:        
        WaitForBlitter
        move.w  #$0002,$064(a5)  ; BLTAMOD (source modulo)
        move.w  #$0002,$066(a5)  ; BLTDMOD (dest modulo)

        
        move.w  #0000,$042(a5) ; BLTCON1
        moveq   #0,d0
        moveq   #0,d2
        move.w  #$09f0,d0
        move.w  #15,d2
        mulu    #$1000,d2
        or.w    d2,d0
        move.w  d0,$040(a5)     ; BLTCON0
        move.l  scrollarea,a1
        addq.l  #2,a1
        move.l  a1,$050(a5)     ; BLTAPTH source
        move.l  scrollarea,a0
        add.l   #0,a0
        move.l  a0,$054(a5)     ; BLTDPTH dest
        moveq   #0,d0
        move.w  #32,d0         ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #23,d0         ; width / 16
        move.w  d0,$058(a5)     ; BLTSIZE

        add.l   #1536,a0
        add.l   #1536,a1
        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #1536,a0
        add.l   #1536,a1
        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #1536,a0
        add.l   #1536,a1
        WaitForBlitter
        move.l  a1,$050(a5)      ; BLTAPTR source
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        rts

colorBar0:
        move.l  #13-1,d7
        
        cmp.b   #1,ch0
        beq.s   setCh0


decCh0  

        lea     channel0Cols+6,a0
ch0sil  cmp.w   #0,(a0)
        bgt.s   vah0
        move.w  #0,(a0)
        addq.l  #8,a0
        dbf     d7,ch0sil
        rts
vah0    move.b  #0,ch0
        sub.w   #$0110,(a0)
        addq.l  #8,a0
        dbf     d7,ch0sil
        rts
setCh0: move.b  #1,ch0
        lea     channel0Cols+6,a0
        lea     channel0cols,a1
setCh0Sil
        move.w  (a1)+,(a0)
        addq.l  #8,a0
        dbf     d7,setCh0Sil
        move.b  #0,ch0
        rts

; bar 1

colorBar1:
        move.l  #13-1,d7
       
        cmp.b   #1,ch1
        beq.s   setCh1

decCh1
        
        lea     channel1Cols+6,a0
ch1sil  cmp.w   #0,(a0)
        bgt.s   vah1
        move.w  #0,(a0)
        addq.l  #8,a0
        dbf     d7,ch1sil
        rts
vah1    
        sub.w   #$0101,(a0)
        addq.l  #8,a0
        dbf     d7,ch1sil
        rts
setCh1:
        
        lea     channel1Cols+6,a0
        lea     channel1cols,a1
setCh1Sil
        move.w  (a1)+,(a0)
        addq.l  #8,a0
        dbf     d7,setCh1Sil
        move.b  #0,ch1
        rts

; bar 2

colorBar2:
        move.l  #13-1,d7
        cmp.b   #1,ch2
        beq.s   setCh2

decCh2  

        lea     channel2Cols+6,a0
ch2sil  cmp.w   #0,(a0)
        bgt.s   vah2
        move.w  #0,(a0)
        addq.l  #8,a0
        dbf     d7,ch2sil
        
        rts
vah2    
        sub.w   #$0011,(a0)
        addq.l  #8,a0
        dbf     d7,ch2sil
        
        rts

setCh2: 
        lea     channel2Cols+6,a0
        lea     channel2cols,a1
setCh2Sil
        move.w  (a1)+,(a0)
        addq.l  #8,a0
        dbf     d7,setCh2Sil
        move.b  #0,ch2
        rts

; bar 3

colorBar3:
        move.l  #13-1,d7
     
        cmp.b   #1,ch3
        beq.s   setCh3

decCh3  

        lea     channel3Cols+6,a0
ch3sil  cmp.w   #0,(a0)
        bgt.s   vah3
        move.w  #0,(a0)
        addq.l  #8,a0
        dbf     d7,ch3sil
        
        rts
vah3    
        sub.w   #$0001,(a0)
        addq.l  #8,a0
        dbf     d7,ch3sil
        
        rts

setCh3: 
        lea     channel3Cols+6,a0
        lea     channel3cols,a1
setCh3Sil
        move.w  (a1)+,(a0)
        addq.l  #8,a0
        dbf     d7,setCh3Sil
        move.b  #0,ch3
        rts

SinEffect:
        move.l  #10-1,d7        ; 320 / 32 - 1
        moveq   #0,d6
        move.l  #$80000000,d5       ; bitmask
        moveq   #0,d0
        moveq   #32-1,d1        ; height counter
        moveq   #32-1,d4        ; width counter

        fsub.x  #320.0*0.004363,fp7
        fadd.x  fp1,fp7

        fmove.x  fp7,fp0
        fsin.x   fp0
        fmul.x   #32,fp0
        fmove.l  fp0,d3
        muls     #40,d3   


        ; copy 1 x 32 pixels of font
copybits:
        moveq   #0,d0
        moveq   #0,d2
        
        move.l  scrollarea,a0
        move.l  DrawScreen,a1
        add.l   d6,a0
        add.l   d6,a1
        add.l   #40*32,a1
        add.l   d3,a1

; bitplane 1
cloop1
        move.l  (a0),d0         ; copy 1 x 32 pixels from scrollarea
        and.l   d5,d0
        move.l  (a1),d2
        or.l    d0,d2
        move.l  d2,(a1)
        add.l   #48,a0          ; next line
        add.l   #40,a1          ; next line
        dbf     d1,cloop1

        move.l  scrollarea,a0
        move.l  DrawScreen,a1
        add.l   d6,a0           ; x position
        add.l   d6,a1           ; x position
        add.l   #40*32,a1       ; y centering
        add.l   d3,a1           ; add sinewave value to destination address of the screen
        add.l   #1536,a0
        add.l   #4880,a1
        moveq   #32-1,d1        ; height counter
        moveq   #0,d0
        moveq   #0,d2

; bitplane 2
cloop2
        move.l  (a0),d0         ; copy 1 x 32 pixels from scrollarea
        and.l   d5,d0
        move.l  (a1),d2
        or.l    d0,d2
        move.l  d2,(a1)
        add.l   #48,a0          ; next line
        add.l   #40,a1          ; next line
        dbf     d1,cloop2

        move.l  scrollarea,a0
        move.l  DrawScreen,a1

        add.l   #1536*2,a0
        add.l   #4880*2,a1
        add.l   d6,a0
        add.l   d6,a1
        add.l   #40*32,a1
        add.l   d3,a1
        moveq   #32-1,d1        ; height counter
        moveq   #0,d0
        moveq   #0,d2

; bitplane 3
cloop3
        move.l  (a0),d0         ; copy 1 x 32 pixels from scrollarea
        and.l   d5,d0
        move.l  (a1),d2
        or.l    d0,d2
        move.l  d2,(a1)
        add.l   #48,a0          ; next line
        add.l   #40,a1          ; next line
        dbf     d1,cloop3

        move.l  scrollarea,a0
        move.l  DrawScreen,a1

        add.l   #1536*3,a0
        add.l   #4880*3,a1
        add.l   d6,a0
        add.l   d6,a1
        add.l   #40*32,a1
        add.l   d3,a1

        moveq   #32-1,d1        ; height counter
        moveq   #0,d0
        moveq   #0,d2

; bitplane 4
cloop4
        move.l  (a0),d0         ; copy 1 x 32 pixels from scrollarea
        and.l   d5,d0
        move.l  (a1),d2
        or.l    d0,d2
        move.l  d2,(a1)
        add.l   #48,a0          ; next line
        add.l   #40,a1          ; next line
        dbf     d1,cloop4

        moveq   #32-1,d1
        lsr.l   #1,d5

        fadd.x  fp1,fp7
        
        fmove.x  fp7,fp0
        fsin.x   fp0
        fmul.x   #32,fp0
        fmove.l  fp0,d3

        muls     #40,d3         ; sine wave value multiplied by 40 (width of the screen in bytes)       
    
        dbf     d4,copybits

         
        fadd.x  fp1,fp7

        fmove.x  fp7,fp0
        fsin.x   fp0
        fmul.x   #32,fp0
        fmove.l  fp0,d3

        muls     #40,d3     

        addq.l  #4,d6
        moveq   #32-1,d1
        moveq   #32-1,d4
        move.l  #$80000000,d5
        dbf     d7,copybits
        rts

Show2:
        moveq   #0,d2
        moveq   #0,d3

        fmove.x  fp7,fp0
        fdiv.x   #2,fp0
        fcos.x   fp0
        fmul.x   #80,fp0
        fadd.x   #80,fp0
        fabs.x   fp0
        fmove.l  fp0,d0         ; x-koordinaatti
       
lowert
        move.l  ShowScreen1,d1

        add.l   #30,d1
        move.b  d0,d2
        move.b  d0,d3 
        lsr.l   #4,d0
       
        sub.l   d0,d1
       
        sub.l   d0,d1
        and.l   #$0000000f,d2
        move.w  d2,con1
        lsl.l   #4,d3
        and.l   #$000000f0,d3
        or.w    d3,con1
       
       
 

        move.w  d1,low1
        swap    d1
        move.w  d1,high1
        swap    d1
        add.l   #5360,d1
        move.w  d1,low2
        swap    d1
        move.w  d1,high2
        swap    d1
        add.l   #5360,d1
        move.w  d1,low3
        swap    d1
        move.w  d1,high3
        swap    d1
        add.l   #5360,d1
        move.w  d1,low4
        swap    d1
        move.w  d1,high4        
        swap    d1
        add.l   #5360,d1
        move.w  d1,low5
        swap    d1
        move.w  d1,high5        
        swap    d1
        add.l   #5360,d1
        move.w  d1,low6
        swap    d1
        move.w  d1,high6  
        swap    d1
        add.l   #5360,d1
        move.w  d1,low7
        swap    d1
        move.w  d1,high7  
        swap    d1
        add.l   #5360,d1
        move.w  d1,low8
        swap    d1
        move.w  d1,high8  

        move.l  ShowScreen,d1
        move.w  d1,low11
        swap    d1
        move.w  d1,high11
        swap    d1
        add.l   #4880,d1
        move.w  d1,low22
        swap    d1
        move.w  d1,high22
        swap    d1
        add.l   #4880,d1
        move.w  d1,low33
        swap    d1
        move.w  d1,high33
        swap    d1
        add.l   #4880,d1
        move.w  d1,low44
        swap    d1
        move.w  d1,high44        

   
        rts

SwapScreens:
        cmp.b   #1,which
        beq.s   LetItBeOneThen

        move.l  bitmap2,DrawScreen
        move.l  bitmap1,ShowScreen
        ;move.l  bitmap02,DrawScreen1
        move.l  bitmap01,ShowScreen1
        move.b  #1,which
        rts
LetItBeOneThen:
        move.l  bitmap1,DrawScreen
        move.l  bitmap2,ShowScreen
        ;move.l  bitmap01,DrawScreen1
        move.l  bitmap01,ShowScreen1

        move.b  #2,which
        rts

ShowP   move.l  omascreen,d1
        move.w  d1,low1p
        swap    d1
        move.w  d1,high1p
        move.l  omascreen,d1
        add.l   #10240,d1
        move.w  d1,low2p
        swap    d1
        move.w  d1,high2p
        rts

DrawPoint
       
        fmove.x fp6,fp0

        fmove.x fp0,fp1
        fmove.x fp0,fp2

        fcos.x  fp1
        fsin.x  fp2

        fmul.x  fp5,fp1
        fmul.x  fp5,fp2

        fmove.l fp1,d0
        fmove.l fp2,d1

        add.l   #160,d0
        add.l   #128,d1
    

        mulu    #40,d1
        move.b  d0,d2
        lsr.l   #3,d0
        add.l   d1,d0
        ext.l   d0
        and.l   #$000f,d2
        mulu    #$1000,d2

        move.l  screens,a0
        add.l   d6,a0
        add.l   d0,a0
        move.l  #bob,a2
        move.l  #mask,a3

        WaitForBlitter
        move.w  #0000,$062(a5)  ; BLTBMOD
        move.w  #36,$060(a5)  ; BLTCMOD 
        move.w  #0000,$064(a5)  ; BLTAMOD
        move.w  #36,$066(a5)  ; BLTDMOD

        move.w  d2,$042(a5)     ; BLTCON1
        or.w    #$0fce,d2       ; minterm+use shift valueen
        move.w  d2,$040(a5)     ; BLTCON0
        move.l  a0,$048(a5)     ; BLTCPTR kohde
        move.l  a2,$04c(a5)     ; BLTBPTR bob-data
        move.l  a3,$050(a5)     ; BLTAPTR mask
        move.l  a0,$054(a5)     ; BLTDPTR kohde
        move.w  #1026,$058(a5)     ; BLTSIZE

        add.l   #10240,a0
        add.l   #64,a2

        WaitForBlitter

        move.l  a0,$048(a5)     ; BLTCPTR kohde
        move.l  a2,$04c(a5)     ; BLTBPTR bob-data
        move.l  a3,$050(a5)     ; BLTAPTR mask
        move.l  a0,$054(a5)     ; BLTDPTR kohde
        move.w  #1026,$058(a5)     ; BLTSIZE

        move.w  pointX,d0
        move.w  pointY,d1

        mulu    #40,d1
        move.b  d0,d2
        lsr.l   #3,d0
        add.l   d1,d0
        ext.l   d0
        and.l   #$000f,d2
        mulu    #$1000,d2

        move.l  screens,a0
        add.l   d6,a0
        add.l   d0,a0
        move.l  #bob,a2
        move.l  #mask,a3

        WaitForBlitter
        move.w  #0000,$062(a5)  ; BLTBMOD
        move.w  #36,$060(a5)  ; BLTCMOD 
        move.w  #0000,$064(a5)  ; BLTAMOD
        move.w  #36,$066(a5)  ; BLTDMOD

        move.w  d2,$042(a5)     ; BLTCON1
        or.w    #$0fce,d2       ; minterm+use shift valueen
        move.w  d2,$040(a5)     ; BLTCON0
        move.l  a0,$048(a5)     ; BLTCPTR kohde
        move.l  a2,$04c(a5)     ; BLTBPTR bob-data
        move.l  a3,$050(a5)     ; BLTAPTR mask
        move.l  a0,$054(a5)     ; BLTDPTR kohde
        move.w  #1026,$058(a5)     ; BLTSIZE

        move.w  pointX,d0
        move.w  pointY,d1

        move.w  dx,d2
        move.w  dy,d3

        add.w   d2,d0
        add.w   d3,d1
        move.w  d0,pointX
        move.w  d1,pointY

        cmp.w   #319-4-10,d0
        blt.s   nxtx
        
        neg.w   d2
        move.w  d2,dx

        bra.s   ytst
nxtx    cmp.w   #4,d0
        bgt.s   ytst

        neg.w   d2
        move.w  d2,dx

ytst    cmp.w   #255-4-10,d1
        blt.s   nytst
    
        neg.w   d3
        move.w  d3,dy
nytst   cmp.w   #4,d1
        bgt.s   nxty

        neg.w   d3
        move.w  d3,dy
nxty

        move.l  omascreen,a0

        moveq   #0,d0
        moveq   #0,d1
        moveq   #0,d2
        moveq   #0,d3
        moveq   #0,d4
        move.l  #512-1,d7
clrP_   movem.l d0-d4,(a0)
        add.l   #20,a0
        dbf     d7,clrP_

        move.l  omascreen,a0
        add.l   #10240,a0
        move.l  #512-1,d7
clrP2_  movem.l d0-d4,(a0)
        add.l   #20,a0
        dbf     d7,clrP2_

        WaitForBlitter

cpy     move.l  #2560*2-1,d7
        move.l  screens,a0
        add.l   d6,a0
        move.l  omascreen,a1
s1      move.l  (a0)+,d0
        move.l  d0,(a1)+
        dbf     d7,s1
    

        add.l   #10240*2,d6
        cmp.l   #10240*4*2,d6
        blt.s   jatka
        move.l  #0,d6
jatka
        fadd.x  fp7,fp6
        fadd.x  #0.075,fp5
        fcmp.x  #120,fp5
        fblt     piene
        move.b  #1,vikaosa
piene

        rts

ClearScreenP

        move.l  DrawScreen,a0
        moveq   #0,d0
        moveq   #0,d1
        moveq   #0,d2
        moveq   #0,d3
        moveq   #0,d4
        move.l  #512-1,d7
clrP    movem.l d0-d4,(a0)
        add.l   #20,a0
        dbf     d7,clrP
        rts

viimeinenosa
        lea     $dff000,a5

        WaitForBlitter
        move.w  #$ffff,$dff044
        move.w  #$ffff,$dff046
        move.w  #$0000,$dff064   ; BLTAMOD (source modulo)
        move.w  #$0000,$dff066   ; BLTDMOD (dest modulo)
        move.w  #$0000,$dff042   ; BLTCON1
        move.w  #$09f0,$dff040   ; BLTCON0

        bsr     WaitForBeam
        bsr     WaitForBeam

        bsr     ExtraWaitForBeam

        move.l  #VertCopper,$dff080
        tst.w   $dff088
        
        move.l  screens,d0
        move.w  d0,low1v
        swap    d0
        move.w  d0,high1v

        move.l  screens,a0
        move.l  #2560-1,d7
clrv    move.l  #0,(a0)+
        dbf     d7,clrv

        WaitForBlitter
        move.w  #$ffff,$dff044
        move.w  #$ffff,$dff046
        move.w  #$0000,$dff064   ; BLTAMOD (source modulo)
        move.w  #$0000,$dff066   ; BLTDMOD (dest modulo)
        move.w  #$0000,$dff042   ; BLTCON1
        move.w  #$09f0,$dff040   ; BLTCON0

VertScroll
        moveq   #0,d6
        lea     endtext,a0
        move.l  et_pointer,d0
        add.l   d0,a0
        moveq   #0,d7

ksill   cmp.b   #1,(a0)
        beq.s   lopeta
        cmp.b   #0,(a0)
        beq.s   keskl
        addq.l  #1,d7
        addq.l  #1,a0
        addq.l  #1,et_pointer
        bra.s   ksill

keskl   sub.l   d7,a0
        move.l  #40,d5
        sub.l   d7,d5
        lsr.l   #1,d5       ; arvo, jolla teksti keskitetään

lsill   moveq   #0,d0
        cmp.b   #0,(a0)
        beq.s   sill
        move.b  (a0)+,d0
        
        move.l  d0,d1
        sub.l   #32,d1
        cmp.l   #32,d1
        blt.s   ekal
        
        move.l  d0,d1
        sub.l   #64,d1
        cmp.l   #32,d1
        blt.s   tokal
        add.l   #192+32+32+32+32+32+32+32+32,d0
        bra.s   goonl
ekal    sub.l   #32,d0      
        bra.s   goonl
tokal   add.l   #192,d0
        

goonl   bsr     LMB
        cmp.b   #1,buttonreleased
        beq.s   lopeta
         
        bsr     drawLittleLetter2
        bra.s   lsill

        

        moveq   #0,d1
sill    bsr     lastSil

        

        

        bra.s   VertScroll

lastSil cmp.b  #4,frame
        beq.s  doit
        addq.b #1,frame
        rts
doit
        move.l #8-1,d7
        
scrollLastPart
        
        move.l  screens,a0
        move.l  screens,a1
        add.l   #40,a1
        WaitForBlitter
        move.l  a1,$050(a5)     ; BLTAPTH source
        move.l  a0,$054(a5)     ; BLTDPTH dest
        moveq   #0,d0
        move.w  #255-1,d0        ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #20,d0         ; width / 16
        move.w  d0,$058(a5)     ; BLTSIZE


        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     WaitForBeam

        dbf     d7,scrollLastPart

        addq.l  #1,et_pointer
        rts

lopeta
        move.l  sidbase,a6
        jsr     _LVOStopSong(a6)
        bra     CleanUp

starsosa
        lea     $dff000,a5
        move.l  screens,a0
        move.l  #2560*4-1,d7
asc     move.l  #0,(a0)+
        dbf     d7,asc

        bsr     RandomCoords

        bsr     WaitForBeam
        bsr     WaitForBeam

        bsr     ExtraWaitForBeam

        move.l  #StarsCopper,$dff080
        tst.w   $dff088

starsMain
        bsr     starsSwapScreens
        bsr     clearStarsScreen
        bsr     DrawStars

        bsr     WaitForBeam

        addq.l  #1,starsAika
        cmp.l   #50*30,starsAika
        beq     CopperMovePart

        bsr     LMB
        cmp.b   #1,buttonreleased
        bne.s   starsMain

        bra     CopperMovePart

DrawStars
        move.l  DrawScreen,a1
        move.l  DrawScreen,a4
        add.l   #10240*2,a4

        lea     Coords,a0
        move.l  #512-1,d7
        move.w  #160,d3                 ; x0 for centering
        move.w  #128,d4                 ; y0 for centering
        
TakeC   move.w  (a0)+,d0                ; x
        move.w  (a0)+,d1                ; y
        move.w  (a0),d2                 ; z
        ext.l   d0
        ext.l   d1

        fmove.l  d0,fp0
        fmove.l  d1,fp1
        fmove.l  d2,fp2

        fdiv.x  fp2,fp0
        fmove.l fp0,d0

        add.w   d3,d0
        bge.s   nc
        move.w  #512,(a0)
        bra.s   subZ

nc      cmp.w   #319,d0
        ble.s   nc2
        move.w  #512,(a0)
        bra.s   subZ

nc2     fdiv.x  fp2,fp1
        fmove.l fp1,d1
  
        add.w   d4,d1
        bge.s   nc3
        move.w  #512,(a0)
        bra.s   subZ

nc3     cmp.w   #255,d1
        ble.s   PutPixel
        move.w  #512,(a0)

subZ    subq.w  #2,(a0)+
        dbf     d7,TakeC
        rts

PutPixel                                ; d0=x,d1=y
        moveq   #0,d5
        moveq   #0,d6
 

        move.b  d0,d5
        move.b  #$ff,d6                 
        sub.b   d5,d6                  
        lsr.w   #3,d0                   
    
        mulu    #40,d1
        add.l   d1,d0                   ; "x+y"

        cmp.l   #400,d2
        blt.s   Col2
        bset    d6,(a1,d0.w)            ; Bsetataan BPL1:een
        subq.w  #2,(a0)+
        dbf     d7,TakeC
        rts

Col2    cmp.l   #200,d2
        blt.s   Col3
        bset    d6,(a4,d0.w)            ; Bsetataan BPL2:een
        subq.w  #2,(a0)+
        dbf     d7,TakeC
        rts

Col3    bset    d6,(a1,d0.w)            ; Bsetataan BPL1:een
        bset    d6,(a4,d0.w)            ; & 2:een
        subq.w  #2,(a0)+
        dbf     d7,TakeC

        rts

clearStarsScreen
        move.l  DrawScreen,a0
        move.l  #(2560*2)-1,d7
csss    move.l  #0,(a0)+
        dbf     d7,csss
        rts

starsSwapScreens
        cmp.b   #1,which
        bne.s   vaihdaScreen

        move.l  screens,d0
        move.w  d0,low1st
        swap    d0
        move.w  d0,high1st
        swap    d0
        add.l   #10240,d0
        move.w  d0,low2st
        swap    d0
        move.w  d0,high2st
        
        move.b  #0,which

        swap    d0
        add.l   #10240,d0
        move.l  d0,DrawScreen
        rts

vaihdaScreen
        move.l  screens,d0
        move.l  d0,DrawScreen

        add.l   #10240*2,d0
        move.w  d0,low1st
        swap    d0
        move.w  d0,high1st
        swap    d0
        add.l   #10240,d0
        move.w  d0,low2st
        swap    d0
        move.w  d0,high2st
        move.b  #1,which
        rts

RandomCoords
        moveq   #0,d0
        moveq   #0,d1
        lea     Coords,a0
        move.l  #512-1,d7
        move.w  #$1249,d1
        moveq   #0,d6
RndLoop
        move.b  #0,d6

        bsr.s   NewStar                 ; X-koordinaatti    
        move.w  d0,(a0)+
        
        move.b  #1,d6
        bsr.s   NewStar                 ; Y-koordinaatti      
        move.w  d0,(a0)+
Zeta    bsr.s   NewStar                 ; Z-koordinaatti
        andi.w  #$1ff,d0                ; Z-arvot 0-511
        cmp.w   #0,d0                   ; Nolla ei kelpaa!
        beq.s   Zeta
        add.w   d0,d0                   ; Z parilliseksi
        move.w  d0,(a0)+
        dbf     d7,RndLoop
        rts

NewStar
        move.w  $dff006,d0             ; Otetaan beam position arvo
       
        muls    d1,d0                   ; Kerrotaan edellisellä arvolla
        add.w   #$1249,d0
        move.w  d0,d1
        cmp.b   #0,d6
        beq.s   checkX
        cmp.b   #1,d6
        beq.s   checkY
checkX  lea     Coords,a1
        move.l  #512-1,d5
chx     cmp.w   (a1),d0
        beq.s   NewStar
        addq.l  #6,(a1)
        dbf     d5,chx        
        rts
checkY  lea     Coords,a1
        move.l  #512-1,d5
chy     cmp.w   2(a1),d0
        beq.s   NewStar
        addq.l  #6,a1
        dbf     d5,chy
        rts

CopperMovePart:
        lea     $dff000,a5
        bsr     WaitForBeam
        bsr     WaitForBeam
        bsr     ExtraWaitForBeam

        move.l  #ModelCopper,$dff080
        tst.w   $dff088

        move.b  #0,vikaosa
        move.b  #0,lastPart
        move.b  #1,modelPart

        move.l  #32,fontw
        move.b  #2,which

        fmove.x  #0,fp7                ;
        fmove.x  #3.14159265*180.0,fp1
                                        
        fdiv.x  #180.0*320,fp1
        
        WaitForBlitter
        move.w  #$ffff,$dff044
        move.w  #$ffff,$dff046
        move.w  #$0000,$dff064   ; BLTAMOD (source modulo)
        move.w  #$0000,$dff066   ; BLTDMOD (dest modulo)

        move.w  #0,$064(a5)  ; BLTAMOD (source modulo)
        move.w  #28,$066(a5)  ; BLTDMOD (dest modulo)

        move.w  #$0000,$042(A5) ; BLTCON1
      
        move.w  #$09f0,$040(A5)     ; BLTCON0
        
        move.l  #Model,$050(A5)     ; BLTAPTH source
        
        
        MOVE.L  modelBitmap,A0
        add.l   #0,a0
        move.l  a0,$054(A5)     ; BLTDPTH dest
        moveq   #0,d0
        move.w  #159,d0         ; height
        lsl.w   #6,d0          ; height to appropriate bits
        or.w    #6,d0         ; width / 16
        move.w  d0,$058(A5)     ; BLTSIZE

        add.l   #6360,a0
        
        WaitForBlitter
        move.l  #Model+1908,$050(a5)      ; BLTAPTR source
       
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #6360,a0
        
        WaitForBlitter
        move.l  #Model+1908*2,$050(a5)      ; BLTAPTR source
        
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #6360,a0
       
        WaitForBlitter
        move.l  #Model+1908*3,$050(a5)      ; BLTAPTR source
        
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        add.l   #6360,a0

        WaitForBlitter
        move.l  #Model+1908*4,$050(a5)      ; BLTAPTR source
       
        move.l  a0,$054(a5)      ; BLTDPTH dest
        move.w  d0,$058(a5)

        fmove.x  #3.14159265,fp4   ; Approximation in radians * screenwidth                                
        fdiv.x  #180.0,fp4

        fmove.x #3.14159265,fp5
        fdiv.x  #180.0,fp5

        moveq   #0,d0
        moveq   #0,d1
        moveq   #0,d2
        moveq   #0,d3
        moveq   #0,d4
        moveq   #0,d5
        moveq   #0,d6
        moveq   #0,d7

        bsr     SwapScreens

CopperMoveMain
        

        fmove.x #0,fp7
        fadd.x  fp5,fp7                ;
 

        move.l  #159-1,d7
        move.w  #$002a,d3
        moveq   #0,d2
        move.l  #ModelBPL,a0

CopperMoveLoop
        fmove.x fp7,fp0
        fsin.x  fp0
        fmul.x  #80,fp0
        fadd.x  #80,fp0
        fabs.x  fp0
        fmove.l fp0,d0         ; x-koordinaatti
        move.b  d0,d5
        move.b  d0,d6 
        lsr.l   #4,d0

        and.l   #$0000000f,d5
       
        
        lsl.w   #4,d6
        and.l   #$000000f0,d6
        or.w    d5,d6

        moveq   #0,d4
        
        lsl.l   #1,d0
copsil
        addq.w  #1,d3
        move.w  d3,d4
        lsl.w   #8,d4
        or.w    #%0000000000000001,d4
        move.w  d4,(a0)+
        move.w  #$ff00,(a0)+

        move.w  #$0102,(a0)+
        move.w  d6,(a0)+
; bitplane 1
        move.l  modelBitmap,d1
        add.l   d2,d1
        sub.l   d0,d1

        move.w  #$00e0,(a0)+
        swap    d1
        move.w  d1,(a0)+
        move.w  #$00e2,(a0)+
        swap    d1
        move.w  d1,(a0)+

; bitplane 2
        move.l  modelBitmap,d1
        add.l   d2,d1
        add.l   #6360,d1
        sub.l   d0,d1

        move.w  #$00e4,(a0)+
        swap    d1
        move.w  d1,(a0)+
        move.w  #$00e6,(a0)+
        swap    d1
        move.w  d1,(a0)+

; bitplane 3
        move.l  modelBitmap,d1
        add.l   d2,d1
        add.l   #6360*2,d1
        sub.l   d0,d1
   

        move.w  #$00e8,(a0)+
        swap    d1
        move.w  d1,(a0)+
        move.w  #$00ea,(a0)+
        swap    d1
        move.w  d1,(a0)+

; bitplane 4
        move.l  modelBitmap,d1
        add.l   d2,d1
        add.l   #6360*3,d1
        sub.l   d0,d1

        move.w  #$00ec,(a0)+
        swap    d1
        move.w  d1,(a0)+
        move.w  #$00ee,(a0)+
        swap    d1
        move.w  d1,(a0)+        

; bitplane 5
        move.l  modelBitmap,d1
        add.l   d2,d1
        add.l   #6360*4,d1
        sub.l   d0,d1

        move.w  #$00f0,(a0)+
        swap    d1
        move.w  d1,(a0)+
        move.w  #$00f2,(a0)+
        swap    d1
        move.w  d1,(a0)+        

        fadd.x  fp4,fp7
        add.l   #40,d2

        

        dbf     d7,CopperMoveLoop

        fadd.x  fp4,fp5
        
        bsr     SwapScreens
        bsr     ShowMSin
        bsr     ClearScreen
        bsr     Scroll
        bsr     SinEffect

        bsr     WaitForBeam

        move.l  #fontw,a0           ; Font width
        subq.l  #1,(a0)
        cmp.l   #0,(a0)             ; is it time to draw next letter?
        bne.s   jatkaTata
        bsr     drawLetter    
        cmp.b   #1,lastPart
        bne.s   jatkaTata
        bra     viimeinenosa
jatkaTata
        bsr     LMB
        cmp.b   #1,buttonreleased
        bne.s   CopperMoveMain
        bra     viimeinenosa

ShowMSin
        move.l  ShowScreen,d1
        move.w  d1,mlow11
        swap    d1
        move.w  d1,mhigh11
        swap    d1
        add.l   #4880,d1
        move.w  d1,mlow22
        swap    d1
        move.w  d1,mhigh22
        swap    d1
        add.l   #4880,d1
        move.w  d1,mlow33
        swap    d1
        move.w  d1,mhigh33
        swap    d1
        add.l   #4880,d1
        move.w  d1,mlow44
        swap    d1
        move.w  d1,mhigh44        
        rts

LMB     move.b  #0,buttonreleased
        btst    #6,$bfe001
        beq     pressed
        cmp.b   #1,buttonpressed
        beq.s   released
        rts
released
        move.b  #1,buttonreleased
        move.b  #0,buttonpressed
        rts

pressed 
        move.b  #1,buttonpressed
        move.b  #0,buttonreleased
        rts

SetCIAInt
	MOVEQ	#2,D6
	LEA	$BFD000,A5
	MOVE.B	#'b',CIAAname+3
SetCIALoop
	MOVEQ	#0,D0
	LEA	CIAAname,A1
	MOVE.L	4.W,A6
	JSR	LVOOpenResource(A6)
	MOVE.L	D0,CIAAbase
	BEQ	mt_Return

	LEA	gfxname,A1
	MOVEQ	#0,D0
	JSR	LVOOpenLibrary(A6)
	TST.L	D0
	BEQ	ResetCIAInt
	MOVE.L	D0,A1
	MOVE.W	206(A1),D0	; DisplayFlags
	BTST	#2,D0		; PAL?
	BEQ.S	WasNTSC
	MOVE.L	#1773447,D7 ; PAL
	BRA.S	sciask
WasNTSC	MOVE.L	#1789773,D7 ; NTSC
sciask	MOVE.L	D7,TimerValue
	DIVU	#125,D7 ; Default to normal 50 Hz timer
	JSR	LVOCloseLibrary(A6)

	MOVE.L	CIAAbase,A6
	CMP.W	#2,D6
	BEQ.S	TryTimerA
TryTimerB
	LEA	MusicIntServer,A1
	MOVEQ	#1,D0	; Bit 1: Timer B
	JSR	AddICRVector(A6)
	MOVE.L	#1,TimerFlag
	TST.L	D0
	BNE.S	CIAError
	MOVE.L	A5,CIAAaddr
	MOVE.B	D7,ciatblo(A5)
	LSR.W	#8,D7
	MOVE.B	D7,ciatbhi(A5)
	BSET	#0,ciacrb(A5)
	RTS

TryTimerA
	LEA	MusicIntServer,A1
	MOVEQ	#0,D0	; Bit 0: Timer A
	JSR	AddICRVector(A6)
	CLR.L	TimerFlag
	TST.L	D0
	BNE.S	CIAError
	MOVE.L	A5,CIAAaddr
	MOVE.B	D7,ciatalo(A5)
	LSR.W	#8,D7
	MOVE.B	D7,ciatahi(A5)
	BSET	#0,ciacra(A5)
	RTS

CIAError
	MOVE.B	#'a',CIAAname+3
	LEA	$BFE001,A5
	SUBQ.W	#1,D6
	BNE	SetCIALoop
	CLR.L	CIAAbase
	RTS

ResetCIAInt
	MOVE.L	CIAAbase,D0
	BEQ	mt_Return
	CLR.L	CIAAbase
	MOVE.L	D0,A6
	MOVE.L	CIAAaddr,A5
	TST.L	TimerFlag
	BEQ.S	ResTimerA

	BCLR	#0,ciacrb(A5)
	MOVEQ	#1,D0
	BRA.S	RemInt

ResTimerA
	BCLR	#0,ciacra(A5)
	MOVEQ	#0,D0
RemInt	LEA	MusicIntServer,A1
	MOVEQ	#0,d0
	JSR	RemICRVector(A6)
	RTS

;---- Tempo ----

SetTempo
	MOVE.L	CIAAbase,D2
	BEQ	mt_Return
	CMP.W	#32,D0
	BHS.S	setemsk
	MOVEQ	#32,D0
setemsk	MOVE.W	D0,RealTempo
	MOVE.L	TimerValue,D2
	DIVU	D0,D2
	MOVE.L	CIAAaddr,A4
	MOVE.L	TimerFlag,D0
	BEQ.S	SetTemA
	MOVE.B	D2,ciatblo(A4)
	LSR.W	#8,D2
	MOVE.B	D2,ciatbhi(A4)
	RTS

SetTemA	MOVE.B	D2,ciatalo(A4)
	LSR.W	#8,D2
	MOVE.B	D2,ciatahi(A4)
	RTS


mt_init	
	move.l	#musa,mt_data

	move.l	mt_data,A0
	MOVE.L	A0,mt_SongDataPtr
	MOVE.L	A0,A1
	LEA	952(A1),A1
	MOVEQ	#127,D0
	MOVEQ	#0,D1
	MOVEQ	#0,D2
mtloop	MOVE.B	(A1)+,D1
	CMP.B	D2,D1
	BCS.S	mtloop2
	MOVE.L	D1,D2
mtloop2	DBRA	D0,mtloop
	ADDQ.B	#1,D2

	LEA	mt_SampleStarts,A1
	ASL.L	#8,D2
	ASL.L	#2,D2
	ADD.L	#1084,D2
	ADD.L	A0,D2
	MOVE.L	D2,A2
	MOVEQ	#30,D0
mtloop3	CLR.L	(A2)
	MOVE.L	A2,(A1)+
	MOVEQ	#0,D1
	MOVE.W	42(A0),D1
	ASL.L	#1,D1
	ADD.L	D1,A2
	ADD.L	#30,A0
	DBRA	D0,mtloop3

	OR.B	#2,$BFE001
	MOVE.B	#6,mt_speed
	CLR.B	mt_counter
	CLR.B	mt_SongPos
	CLR.W	mt_PatternPos
mt_end	SF	mt_Enable
	LEA	$DFF000,A0
	CLR.W	$A8(A0)
	CLR.W	$B8(A0)
	CLR.W	$C8(A0)
	CLR.W	$D8(A0)
	MOVE.W	#$F,$DFF096
	RTS

mt_music
	MOVEM.L	D0-D4/A0-A6,-(SP)
	TST.B	mt_Enable
	BEQ	mt_exit
	ADDQ.B	#1,mt_counter
	MOVE.B	mt_counter,D0
	CMP.B	mt_speed,D0
	BLO.S	mt_NoNewNote
	CLR.B	mt_counter
	TST.B	mt_PattDelTime2
	BEQ.S	mt_GetNewNote
	BSR.S	mt_NoNewAllChannels
	BRA	mt_dskip

mt_NoNewNote
	BSR.S	mt_NoNewAllChannels
	BRA	mt_NoNewPosYet

mt_NoNewAllChannels
	LEA	$DFF0A0,A5
	LEA	mt_chan1temp,A6
	BSR	mt_CheckEfx
	LEA	$DFF0B0,A5
	LEA	mt_chan2temp,A6
	BSR	mt_CheckEfx
	LEA	$DFF0C0,A5
	LEA	mt_chan3temp,A6
	BSR	mt_CheckEfx
	LEA	$DFF0D0,A5
	LEA	mt_chan4temp,A6
	BRA	mt_CheckEfx

mt_GetNewNote
	MOVE.L	mt_SongDataPtr,A0
	LEA	12(A0),A3
	LEA	952(A0),A2	;pattpo
	LEA	1084(A0),A0	;patterndata
	MOVEQ	#0,D0
	MOVEQ	#0,D1
	MOVE.B	mt_SongPos,D0
	MOVE.B	(A2,D0.W),D1
	ASL.L	#8,D1
	ASL.L	#2,D1
	ADD.W	mt_PatternPos,D1
	CLR.W	mt_DMACONtemp

	LEA	$DFF0A0,A5
	LEA	mt_chan1temp,A6
	BSR.S	mt_PlayVoice
	LEA	$DFF0B0,A5
	LEA	mt_chan2temp,A6
	BSR.S	mt_PlayVoice
	LEA	$DFF0C0,A5
	LEA	mt_chan3temp,A6
	BSR.S	mt_PlayVoice
	LEA	$DFF0D0,A5
	LEA	mt_chan4temp,A6
	BSR.S	mt_PlayVoice
	BRA	mt_SetDMA

mt_PlayVoice
	TST.L	(A6)
	BNE.S	mt_plvskip
	BSR	mt_PerNop
mt_plvskip
	MOVE.L	(A0,D1.L),(A6)
	ADDQ.L	#4,D1
	MOVEQ	#0,D2
	MOVE.B	n_cmd(A6),D2
	AND.B	#$F0,D2
	LSR.B	#4,D2
	MOVE.B	(A6),D0
	AND.B	#$F0,D0
	OR.B	D0,D2
	TST.B	D2
	BEQ	mt_SetRegs
	MOVEQ	#0,D3
	LEA	mt_SampleStarts,A1
	MOVE	D2,D4
	SUBQ.L	#1,D2
	ASL.L	#2,D2
	MULU	#30,D4
	MOVE.L	(A1,D2.L),n_start(A6)
	MOVE.W	(A3,D4.L),n_length(A6)
	MOVE.W	(A3,D4.L),n_reallength(A6)
	MOVE.B	2(A3,D4.L),n_finetune(A6)
	MOVE.B	3(A3,D4.L),n_volume(A6)
	MOVE.W	4(A3,D4.L),D3 ; Get repeat
	TST.W	D3
	BEQ.S	mt_NoLoop
	MOVE.L	n_start(A6),D2	; Get start
	ASL.W	#1,D3
	ADD.L	D3,D2		; Add repeat
	MOVE.L	D2,n_loopstart(A6)
	MOVE.L	D2,n_wavestart(A6)
	MOVE.W	4(A3,D4.L),D0	; Get repeat
	ADD.W	6(A3,D4.L),D0	; Add replen
	MOVE.W	D0,n_length(A6)
	MOVE.W	6(A3,D4.L),n_replen(A6)	; Save replen
	MOVEQ	#0,D0
	MOVE.B	n_volume(A6),D0
	MOVE.W	D0,8(A5)	; Set volume
	BRA.S	mt_SetRegs

mt_NoLoop
	MOVE.L	n_start(A6),D2
	ADD.L	D3,D2
	MOVE.L	D2,n_loopstart(A6)
	MOVE.L	D2,n_wavestart(A6)
	MOVE.W	6(A3,D4.L),n_replen(A6)	; Save replen
	MOVEQ	#0,D0
	MOVE.B	n_volume(A6),D0
	MOVE.W	D0,8(A5)	; Set volume
mt_SetRegs
	MOVE.W	(A6),D0
	AND.W	#$0FFF,D0
	BEQ	mt_CheckMoreEfx	; If no note
	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0E50,D0
	BEQ.S	mt_DoSetFineTune
	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	CMP.B	#3,D0	; TonePortamento
	BEQ.S	mt_ChkTonePorta
	CMP.B	#5,D0
	BEQ.S	mt_ChkTonePorta
	CMP.B	#9,D0	; Sample Offset
	BNE.S	mt_SetPeriod
	BSR	mt_CheckMoreEfx
	BRA.S	mt_SetPeriod

mt_DoSetFineTune
	BSR	mt_SetFineTune
	BRA.S	mt_SetPeriod

mt_ChkTonePorta
	BSR	mt_SetTonePorta
	BRA	mt_CheckMoreEfx

mt_SetPeriod
	MOVEM.L	D0-D1/A0-A1,-(SP)
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1
	LEA	mt_PeriodTable,A1
	MOVEQ	#0,D0
	MOVEQ	#35,D2
mt_ftuloop
	CMP.W	(A1,D0.W),D1
	BHS.S	mt_ftufound
	ADDQ.L	#2,D0
	DBRA	D2,mt_ftuloop
mt_ftufound
	MOVEQ	#0,D1
	MOVE.B	n_finetune(A6),D1
	MULU	#36*2,D1
	ADD.L	D1,A1
	MOVE.W	(A1,D0.W),n_period(A6)
	MOVEM.L	(SP)+,D0-D1/A0-A1

	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0ED0,D0 ; Notedelay
	BEQ	mt_CheckMoreEfx

;	MOVE.W	n_dmabit(A6),$DFF096
	BTST	#2,n_wavecontrol(A6)
	BNE.S	mt_vibnoc
	CLR.B	n_vibratopos(A6)
mt_vibnoc
	BTST	#6,n_wavecontrol(A6)
	BNE.S	mt_trenoc
	CLR.B	n_tremolopos(A6)
mt_trenoc
	MOVE.L	n_start(A6),(A5)	; Set start
	MOVE.W	n_length(A6),4(A5)	; Set length
;	MOVE.W	n_period(A6),6(A5)	; Set period
	MOVE.W	n_dmabit(A6),D0
	OR.W	D0,mt_DMACONtemp
	BRA	mt_CheckMoreEfx

mt_SetDMA
	MOVE.W	mt_DMACONtemp,D0
	BSR	mt_PlaySamples		; this does the magic
	LEA	$DFF000,A5
	LEA	mt_chan1temp,A6
	MOVE.L	n_loopstart(A6),$A0(A5)
	MOVE.W	n_replen(A6),$A4(A5)
	LEA	mt_chan2temp,A6
	MOVE.L	n_loopstart(A6),$B0(A5)
	MOVE.W	n_replen(A6),$B4(A5)
	LEA	mt_chan3temp,A6
	MOVE.L	n_loopstart(A6),$C0(A5)
	MOVE.W	n_replen(A6),$C4(A5)
	LEA	mt_chan4temp,A6
	MOVE.L	n_loopstart(A6),$D0(A5)
	MOVE.W	n_replen(A6),$D4(A5)

mt_dskip
	ADD.W	#16,mt_PatternPos
	MOVE.B	mt_PattDelTime,D0
	BEQ.S	mt_dskc
	MOVE.B	D0,mt_PattDelTime2
	CLR.B	mt_PattDelTime
mt_dskc	TST.B	mt_PattDelTime2
	BEQ.S	mt_dska
	SUBQ.B	#1,mt_PattDelTime2
	BEQ.S	mt_dska
	SUB.W	#16,mt_PatternPos
mt_dska	TST.B	mt_PBreakFlag
	BEQ.S	mt_nnpysk
	SF	mt_PBreakFlag
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos,D0
	CLR.B	mt_PBreakPos
	LSL.W	#4,D0
	MOVE.W	D0,mt_PatternPos
mt_nnpysk
	CMP.W	#1024,mt_PatternPos
	BLO.S	mt_NoNewPosYet
mt_NextPosition	
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos,D0
	LSL.W	#4,D0
	MOVE.W	D0,mt_PatternPos
	CLR.B	mt_PBreakPos
	CLR.B	mt_PosJumpFlag
	ADDQ.B	#1,mt_SongPos
	AND.B	#$7F,mt_SongPos
	MOVE.B	mt_SongPos,D1
	MOVE.L	mt_SongDataPtr,A0
	CMP.B	950(A0),D1
	BLO.S	mt_NoNewPosYet
	CLR.B	mt_SongPos
mt_NoNewPosYet	
	TST.B	mt_PosJumpFlag
	BNE.S	mt_NextPosition
mt_exit	MOVEM.L	(SP)+,D0-D4/A0-A6
	RTS

;
; busywait-fixed by Delirium Softdesign.
;
; the old routine wasted about 2*10 rasterlines, this routine needs 2-8 lines
; (depending on the module). It was tested on an A500/A3000/A4000 and seems to
; play all modules ok. BTW, using this new technique it should be no problem
; to fix the busywait-loops of almost every other replay !
;
; known bugs:
;
;  sometimes when a module starts playing and the first note is played on
;  a channel, the routine needs ~100 rasterlines (in the mt_Wait2 loop).
;  perhaps this is a dma problem ?
;
; if you know how to fix these bugs please contact me:
; kunath@informatik.tu-muenchen.de (Peter Kunath)
;

mt_PlaySamples
	movem.l	d0-d2/a5-a6,-(sp)
	lea	$DFF000,a5
	move.w	d0,d2

	move.b	$06(a5),d0		; wait until end of line
;	move.w	#$0ff,$180(a5)
mt_Wait1
	cmp.b	$06(a5),d0		; the wait should be enough even on
	beq.s	mt_Wait1		; an A4000 running DBLPAL screenmode
mt_MaxPer0
	btst	#0,d2
	beq.s	mt_MaxPer1
	move.w	#1,$A6(a5)		; max. speed
mt_MaxPer1
	btst	#1,d2
	beq.s	mt_MaxPer2
	move.w	#1,$B6(a5)		; max. speed
mt_MaxPer2
	btst	#2,d2
	beq.s	mt_MaxPer3
	move.w	#1,$C6(a5)		; max. speed
mt_MaxPer3
	btst	#3,d2
	beq.s	mt_MaxPer4
	move.w	#1,$D6(a5)		; max. speed
mt_MaxPer4
	move.w	$02(a5),d0		; get active channels
	and.w	d2,d0
	move.w	d0,d1
	lsl.w	#7,d0
	move.w	d0,$9C(a5)		; stop channels
	move.w	d1,$96(a5)
;	move.w	#$f00,$180(a5)
mt_Wait2
	move.w	$1E(a5),d1		; wait until all channels are stopped
	and.w	d0,d1
	cmp.w	d0,d1
	bne.s	mt_Wait2

	move.w	$02(a5),d0		; get active channels
	not.w	d0
	and.w	d2,d0
	move.w	d0,d1
	lsl.w	#7,d0
	move.w	d0,$9C(a5)
	or.w	#$8000,d1
	move.w	d1,$96(a5)		; start channels
;	move.w	#$0f0,$180(a5)
mt_Wait3
	move.w	$1E(a5),d1		; wait until all channels are running
	and.w	d0,d1
	cmp.w	d0,d1
	bne.s	mt_Wait3
	move.b	$06(a5),d0		; wait until end of line
;	move.w	#$00f,$180(a5)
mt_Wait4
	cmp.b	$06(a5),d0		; the wait should be enough even on
	beq.s	mt_Wait4		; an A4000 running DBLPAL screenmode
mt_SetPer0
	btst	#0,d2
	beq.s	mt_SetPer1

    MOVE.B  #1,ch0

	lea	mt_chan1temp,a6
	move.w	n_period(a6),$A6(a5)
mt_SetPer1
	btst	#1,d2
	beq.s	mt_SetPer2

    MOVE.B  #1,ch1

	lea	mt_chan2temp,a6
	move.w	n_period(a6),$B6(a5)
mt_SetPer2
	btst	#2,d2
	beq.s	mt_SetPer3

    MOVE.B  #1,ch2

	lea	mt_chan3temp,a6
	move.w	n_period(a6),$C6(a5)
mt_SetPer3
	btst	#3,d2
	beq.s	mt_SetPer4

    MOVE.B  #1,ch3

	lea	mt_chan4temp,a6
	move.w	n_period(a6),$D6(a5)
mt_SetPer4
;	move.w	#$000,$180(a5)
	movem.l	(sp)+,d0-d2/a5-a6
	rts

mt_CheckEfx
	BSR	mt_UpdateFunk
	MOVE.W	n_cmd(A6),D0
	AND.W	#$0FFF,D0
	BEQ.S	mt_PerNop
	MOVE.B	n_cmd(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_Arpeggio
	CMP.B	#1,D0
	BEQ	mt_PortaUp
	CMP.B	#2,D0
	BEQ	mt_PortaDown
	CMP.B	#3,D0
	BEQ	mt_TonePortamento
	CMP.B	#4,D0
	BEQ	mt_Vibrato
	CMP.B	#5,D0
	BEQ	mt_TonePlusVolSlide
	CMP.B	#6,D0
	BEQ	mt_VibratoPlusVolSlide
	CMP.B	#$E,D0
	BEQ	mt_E_Commands
SetBack	MOVE.W	n_period(A6),6(A5)
	CMP.B	#7,D0
	BEQ	mt_Tremolo
	CMP.B	#$A,D0
	BEQ	mt_VolumeSlide
mt_Return
	RTS

mt_PerNop
	MOVE.W	n_period(A6),6(A5)
	RTS

mt_Arpeggio
	MOVEQ	#0,D0
	MOVE.B	mt_counter,D0
	DIVS	#3,D0
	SWAP	D0
	CMP.W	#0,D0
	BEQ.S	mt_Arpeggio2
	CMP.W	#2,D0
	BEQ.S	mt_Arpeggio1
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	LSR.B	#4,D0
	BRA.S	mt_Arpeggio3

mt_Arpeggio1
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#15,D0
	BRA.S	mt_Arpeggio3

mt_Arpeggio2
	MOVE.W	n_period(A6),D2
	BRA.S	mt_Arpeggio4

mt_Arpeggio3
	ASL.W	#1,D0
	MOVEQ	#0,D1
	MOVE.B	n_finetune(A6),D1
	MULU	#36*2,D1
	LEA	mt_PeriodTable,A0
	ADD.L	D1,A0
	MOVEQ	#0,D1
	MOVE.W	n_period(A6),D1
	MOVEQ	#35,D3
mt_arploop
	MOVE.W	(A0,D0.W),D2
	CMP.W	(A0),D1
	BHS.S	mt_Arpeggio4
	ADDQ.L	#2,A0
	DBRA	D3,mt_arploop
	RTS

mt_Arpeggio4
	MOVE.W	D2,6(A5)
	RTS

mt_FinePortaUp
	TST.B	mt_counter
	BNE.S	mt_Return
	MOVE.B	#$0F,mt_LowMask
mt_PortaUp
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	mt_LowMask,D0
	MOVE.B	#$FF,mt_LowMask
	SUB.W	D0,n_period(A6)
	MOVE.W	n_period(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#113,D0
	BPL.S	mt_PortaUskip
	AND.W	#$F000,n_period(A6)
	OR.W	#113,n_period(A6)
mt_PortaUskip
	MOVE.W	n_period(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS	
 
mt_FinePortaDown
	TST.B	mt_counter
	BNE	mt_Return
	MOVE.B	#$0F,mt_LowMask
mt_PortaDown
	CLR.W	D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	mt_LowMask,D0
	MOVE.B	#$FF,mt_LowMask
	ADD.W	D0,n_period(A6)
	MOVE.W	n_period(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#856,D0
	BMI.S	mt_PortaDskip
	AND.W	#$F000,n_period(A6)
	OR.W	#856,n_period(A6)
mt_PortaDskip
	MOVE.W	n_period(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS

mt_SetTonePorta
	MOVE.L	A0,-(SP)
	MOVE.W	(A6),D2
	AND.W	#$0FFF,D2
	MOVEQ	#0,D0
	MOVE.B	n_finetune(A6),D0
	MULU	#36*2,D0
	LEA	mt_PeriodTable,A0
	ADD.L	D0,A0
	MOVEQ	#0,D0
mt_StpLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_StpFound
	ADDQ.W	#2,D0
	CMP.W	#36*2,D0
	BLO.S	mt_StpLoop
	MOVEQ	#35*2,D0
mt_StpFound
	MOVE.B	n_finetune(A6),D2
	AND.B	#8,D2
	BEQ.S	mt_StpGoss
	TST.W	D0
	BEQ.S	mt_StpGoss
	SUBQ.W	#2,D0
mt_StpGoss
	MOVE.W	(A0,D0.W),D2
	MOVE.L	(SP)+,A0
	MOVE.W	D2,n_wantedperiod(A6)
	MOVE.W	n_period(A6),D0
	CLR.B	n_toneportdirec(A6)
	CMP.W	D0,D2
	BEQ.S	mt_ClearTonePorta
	BGE	mt_Return
	MOVE.B	#1,n_toneportdirec(A6)
	RTS

mt_ClearTonePorta
	CLR.W	n_wantedperiod(A6)
	RTS

mt_TonePortamento
	MOVE.B	n_cmdlo(A6),D0
	BEQ.S	mt_TonePortNoChange
	MOVE.B	D0,n_toneportspeed(A6)
	CLR.B	n_cmdlo(A6)
mt_TonePortNoChange
	TST.W	n_wantedperiod(A6)
	BEQ	mt_Return
	MOVEQ	#0,D0
	MOVE.B	n_toneportspeed(A6),D0
	TST.B	n_toneportdirec(A6)
	BNE.S	mt_TonePortaUp
mt_TonePortaDown
	ADD.W	D0,n_period(A6)
	MOVE.W	n_wantedperiod(A6),D0
	CMP.W	n_period(A6),D0
	BGT.S	mt_TonePortaSetPer
	MOVE.W	n_wantedperiod(A6),n_period(A6)
	CLR.W	n_wantedperiod(A6)
	BRA.S	mt_TonePortaSetPer

mt_TonePortaUp
	SUB.W	D0,n_period(A6)
	MOVE.W	n_wantedperiod(A6),D0
	CMP.W	n_period(A6),D0
	BLT.S	mt_TonePortaSetPer
	MOVE.W	n_wantedperiod(A6),n_period(A6)
	CLR.W	n_wantedperiod(A6)

mt_TonePortaSetPer
	MOVE.W	n_period(A6),D2
	MOVE.B	n_glissfunk(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_GlissSkip
	MOVEQ	#0,D0
	MOVE.B	n_finetune(A6),D0
	MULU	#36*2,D0
	LEA	mt_PeriodTable,A0
	ADD.L	D0,A0
	MOVEQ	#0,D0
mt_GlissLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_GlissFound
	ADDQ.W	#2,D0
	CMP.W	#36*2,D0
	BLO.S	mt_GlissLoop
	MOVEQ	#35*2,D0
mt_GlissFound
	MOVE.W	(A0,D0.W),D2
mt_GlissSkip
	MOVE.W	D2,6(A5) ; Set period
	RTS

mt_Vibrato
	MOVE.B	n_cmdlo(A6),D0
	BEQ.S	mt_Vibrato2
	MOVE.B	n_vibratocmd(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_vibskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_vibskip
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_vibskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_vibskip2
	MOVE.B	D2,n_vibratocmd(A6)
mt_Vibrato2
	MOVE.B	n_vibratopos(A6),D0
	LEA	mt_VibratoTable,A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVEQ	#0,D2
	MOVE.B	n_wavecontrol(A6),D2
	AND.B	#$03,D2
	BEQ.S	mt_vib_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_vib_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_vib_set
mt_vib_rampdown
	TST.B	n_vibratopos(A6)
	BPL.S	mt_vib_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_sine
	MOVE.B	(A4,D0.W),D2
mt_vib_set
	MOVE.B	n_vibratocmd(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#7,D2
	MOVE.W	n_period(A6),D0
	TST.B	n_vibratopos(A6)
	BMI.S	mt_VibratoNeg
	ADD.W	D2,D0
	BRA.S	mt_Vibrato3
mt_VibratoNeg
	SUB.W	D2,D0
mt_Vibrato3
	MOVE.W	D0,6(A5)
	MOVE.B	n_vibratocmd(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,n_vibratopos(A6)
	RTS

mt_TonePlusVolSlide
	BSR	mt_TonePortNoChange
	BRA	mt_VolumeSlide

mt_VibratoPlusVolSlide
	BSR.S	mt_Vibrato2
	BRA	mt_VolumeSlide

mt_Tremolo
	MOVE.B	n_cmdlo(A6),D0
	BEQ.S	mt_Tremolo2
	MOVE.B	n_tremolocmd(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_treskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_treskip
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_treskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_treskip2
	MOVE.B	D2,n_tremolocmd(A6)
mt_Tremolo2
	MOVE.B	n_tremolopos(A6),D0
	LEA	mt_VibratoTable,A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVEQ	#0,D2
	MOVE.B	n_wavecontrol(A6),D2
	LSR.B	#4,D2
	AND.B	#$03,D2
	BEQ.S	mt_tre_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_tre_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_tre_set
mt_tre_rampdown
	TST.B	n_vibratopos(A6)
	BPL.S	mt_tre_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_sine
	MOVE.B	(A4,D0.W),D2
mt_tre_set
	MOVE.B	n_tremolocmd(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#6,D2
	MOVEQ	#0,D0
	MOVE.B	n_volume(A6),D0
	TST.B	n_tremolopos(A6)
	BMI.S	mt_TremoloNeg
	ADD.W	D2,D0
	BRA.S	mt_Tremolo3
mt_TremoloNeg
	SUB.W	D2,D0
mt_Tremolo3
	BPL.S	mt_TremoloSkip
	CLR.W	D0
mt_TremoloSkip
	CMP.W	#$40,D0
	BLS.S	mt_TremoloOk
	MOVE.W	#$40,D0
mt_TremoloOk
	MOVE.W	D0,8(A5)
	MOVE.B	n_tremolocmd(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,n_tremolopos(A6)
	RTS

mt_SampleOffset
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	BEQ.S	mt_sononew
	MOVE.B	D0,n_sampleoffset(A6)
mt_sononew
	MOVE.B	n_sampleoffset(A6),D0
	LSL.W	#7,D0
	CMP.W	n_length(A6),D0
	BGE.S	mt_sofskip
	SUB.W	D0,n_length(A6)
	LSL.W	#1,D0
	ADD.L	D0,n_start(A6)
	RTS
mt_sofskip
	MOVE.W	#$0001,n_length(A6)
	RTS

mt_VolumeSlide
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	LSR.B	#4,D0
	TST.B	D0
	BEQ.S	mt_VolSlideDown
mt_VolSlideUp
	ADD.B	D0,n_volume(A6)
	CMP.B	#$40,n_volume(A6)
	BMI.S	mt_vsuskip
	MOVE.B	#$40,n_volume(A6)
mt_vsuskip
	MOVE.B	n_volume(A6),D0
	MOVE.W	D0,8(A5)
	RTS

mt_VolSlideDown
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
mt_VolSlideDown2
	SUB.B	D0,n_volume(A6)
	BPL.S	mt_vsdskip
	CLR.B	n_volume(A6)
mt_vsdskip
	MOVE.B	n_volume(A6),D0
	MOVE.W	D0,8(A5)
	RTS

mt_PositionJump
	MOVE.B	n_cmdlo(A6),D0
	SUBQ.B	#1,D0
	MOVE.B	D0,mt_SongPos
mt_pj2	CLR.B	mt_PBreakPos
	ST 	mt_PosJumpFlag
	RTS

mt_VolumeChange
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	CMP.B	#$40,D0
	BLS.S	mt_VolumeOk
	MOVEQ	#$40,D0
mt_VolumeOk
	MOVE.B	D0,n_volume(A6)
	MOVE.W	D0,8(A5)
	RTS

mt_PatternBreak
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	MOVE.L	D0,D2
	LSR.B	#4,D0
	MULU	#10,D0
	AND.B	#$0F,D2
	ADD.B	D2,D0
	CMP.B	#63,D0
	BHI.S	mt_pj2
	MOVE.B	D0,mt_PBreakPos
	ST	mt_PosJumpFlag
	RTS

mt_SetSpeed
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	BEQ	mt_end
	CMP.B	#32,D0
	BHS	SetTempo
	CLR.B	mt_counter
	MOVE.B	D0,mt_speed
	RTS

mt_CheckMoreEfx
	BSR	mt_UpdateFunk
	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	CMP.B	#$9,D0
	BEQ	mt_SampleOffset
	CMP.B	#$B,D0
	BEQ	mt_PositionJump
	CMP.B	#$D,D0
	BEQ.S	mt_PatternBreak
	CMP.B	#$E,D0
	BEQ.S	mt_E_Commands
	CMP.B	#$F,D0
	BEQ.S	mt_SetSpeed
	CMP.B	#$C,D0
	BEQ	mt_VolumeChange
	BRA	mt_PerNop

mt_E_Commands
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$F0,D0
	LSR.B	#4,D0
	BEQ.S	mt_FilterOnOff
	CMP.B	#1,D0
	BEQ	mt_FinePortaUp
	CMP.B	#2,D0
	BEQ	mt_FinePortaDown
	CMP.B	#3,D0
	BEQ.S	mt_SetGlissControl
	CMP.B	#4,D0
	BEQ	mt_SetVibratoControl
	CMP.B	#5,D0
	BEQ	mt_SetFineTune
	CMP.B	#6,D0
	BEQ	mt_JumpLoop
	CMP.B	#7,D0
	BEQ	mt_SetTremoloControl
	CMP.B	#9,D0
	BEQ	mt_RetrigNote
	CMP.B	#$A,D0
	BEQ	mt_VolumeFineUp
	CMP.B	#$B,D0
	BEQ	mt_VolumeFineDown
	CMP.B	#$C,D0
	BEQ	mt_NoteCut
	CMP.B	#$D,D0
	BEQ	mt_NoteDelay
	CMP.B	#$E,D0
	BEQ	mt_PatternDelay
	CMP.B	#$F,D0
	BEQ	mt_FunkIt
	RTS

mt_FilterOnOff
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#1,D0
	ASL.B	#1,D0
	AND.B	#$FD,$BFE001
	OR.B	D0,$BFE001
	RTS	

mt_SetGlissControl
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,n_glissfunk(A6)
	OR.B	D0,n_glissfunk(A6)
	RTS

mt_SetVibratoControl
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,n_wavecontrol(A6)
	OR.B	D0,n_wavecontrol(A6)
	RTS

mt_SetFineTune
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	MOVE.B	D0,n_finetune(A6)
	RTS

mt_JumpLoop
	TST.B	mt_counter
	BNE	mt_Return
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_SetLoop
	TST.B	n_loopcount(A6)
	BEQ.S	mt_jumpcnt
	SUBQ.B	#1,n_loopcount(A6)
	BEQ	mt_Return
mt_jmploop	MOVE.B	n_pattpos(A6),mt_PBreakPos
	ST	mt_PBreakFlag
	RTS

mt_jumpcnt
	MOVE.B	D0,n_loopcount(A6)
	BRA.S	mt_jmploop

mt_SetLoop
	MOVE.W	mt_PatternPos,D0
	LSR.W	#4,D0
	MOVE.B	D0,n_pattpos(A6)
	RTS

mt_SetTremoloControl
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,n_wavecontrol(A6)
	OR.B	D0,n_wavecontrol(A6)
	RTS

mt_RetrigNote
	MOVE.L	D1,-(SP)
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_rtnend
	MOVEQ	#0,D1
	MOVE.B	mt_counter,D1
	BNE.S	mt_rtnskp
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1
	BNE.S	mt_rtnend
	MOVEQ	#0,D1
	MOVE.B	mt_counter,D1
mt_rtnskp
	DIVU	D0,D1
	SWAP	D1
	TST.W	D1
	BNE.S	mt_rtnend
mt_DoRetrig
;	MOVE.W	n_dmabit(A6),$DFF096	; Channel DMA off
	MOVE.L	n_start(A6),(A5)	; Set sampledata pointer
	MOVE.W	n_length(A6),4(A5)	; Set length
	MOVE.W	n_dmabit(A6),D0
	BSR	mt_PlaySamples		; this does the magic
	MOVE.L	n_loopstart(A6),(A5)
	MOVE.L	n_replen(A6),4(A5)
mt_rtnend
	MOVE.L	(SP)+,D1
	RTS

mt_VolumeFineUp
	TST.B	mt_counter
	BNE	mt_Return
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$F,D0
	BRA	mt_VolSlideUp

mt_VolumeFineDown
	TST.B	mt_counter
	BNE	mt_Return
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	BRA	mt_VolSlideDown2

mt_NoteCut
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	CMP.B	mt_counter,D0
	BNE	mt_Return
	CLR.B	n_volume(A6)
	MOVE.W	#0,8(A5)
	RTS

mt_NoteDelay
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	CMP.B	mt_counter,D0
	BNE	mt_Return
	MOVE.W	(A6),D0
	BEQ	mt_Return
	MOVE.L	D1,-(SP)
	BRA	mt_DoRetrig

mt_PatternDelay
	TST.B	mt_counter
	BNE	mt_Return
	MOVEQ	#0,D0
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	TST.B	mt_PattDelTime2
	BNE	mt_Return
	ADDQ.B	#1,D0
	MOVE.B	D0,mt_PattDelTime
	RTS

mt_FunkIt
	TST.B	mt_counter
	BNE	mt_Return
	MOVE.B	n_cmdlo(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,n_glissfunk(A6)
	OR.B	D0,n_glissfunk(A6)
	TST.B	D0
	BEQ	mt_Return
mt_UpdateFunk
	MOVEM.L	A0/D1,-(SP)
	MOVEQ	#0,D0
	MOVE.B	n_glissfunk(A6),D0
	LSR.B	#4,D0
	BEQ.S	mt_funkend
	LEA	mt_FunkTable,A0
	MOVE.B	(A0,D0.W),D0
	ADD.B	D0,n_funkoffset(A6)
	BTST	#7,n_funkoffset(A6)
	BEQ.S	mt_funkend
	CLR.B	n_funkoffset(A6)

	MOVE.L	n_loopstart(A6),D0
	MOVEQ	#0,D1
	MOVE.W	n_replen(A6),D1
	ADD.L	D1,D0
	ADD.L	D1,D0
	MOVE.L	n_wavestart(A6),A0
	ADDQ.L	#1,A0
	CMP.L	D0,A0
	BLO.S	mt_funkok
	MOVE.L	n_loopstart(A6),A0
mt_funkok
	MOVE.L	A0,n_wavestart(A6)
	MOVEQ	#-1,D0
	SUB.B	(A0),D0
	MOVE.B	D0,(A0)
mt_funkend
	MOVEM.L	(SP)+,A0/D1
	RTS

MusicIntServer
	dc.l 0,0
	dc.b 2,5 ; type, priority
	dc.l musintname
	dc.l 0,mt_music


mt_FunkTable dc.b 0,5,6,7,8,10,11,13,16,19,22,26,32,43,64,128

mt_VibratoTable	
	dc.b   0, 24, 49, 74, 97,120,141,161
	dc.b 180,197,212,224,235,244,250,253
	dc.b 255,253,250,244,235,224,212,197
	dc.b 180,161,141,120, 97, 74, 49, 24

mt_PeriodTable
; Tuning 0, Normal
	dc.w	856,808,762,720,678,640,604,570,538,508,480,453
	dc.w	428,404,381,360,339,320,302,285,269,254,240,226
	dc.w	214,202,190,180,170,160,151,143,135,127,120,113
; Tuning 1
	dc.w	850,802,757,715,674,637,601,567,535,505,477,450
	dc.w	425,401,379,357,337,318,300,284,268,253,239,225
	dc.w	213,201,189,179,169,159,150,142,134,126,119,113
; Tuning 2
	dc.w	844,796,752,709,670,632,597,563,532,502,474,447
	dc.w	422,398,376,355,335,316,298,282,266,251,237,224
	dc.w	211,199,188,177,167,158,149,141,133,125,118,112
; Tuning 3
	dc.w	838,791,746,704,665,628,592,559,528,498,470,444
	dc.w	419,395,373,352,332,314,296,280,264,249,235,222
	dc.w	209,198,187,176,166,157,148,140,132,125,118,111
; Tuning 4
	dc.w	832,785,741,699,660,623,588,555,524,495,467,441
	dc.w	416,392,370,350,330,312,294,278,262,247,233,220
	dc.w	208,196,185,175,165,156,147,139,131,124,117,110
; Tuning 5
	dc.w	826,779,736,694,655,619,584,551,520,491,463,437
	dc.w	413,390,368,347,328,309,292,276,260,245,232,219
	dc.w	206,195,184,174,164,155,146,138,130,123,116,109
; Tuning 6
	dc.w	820,774,730,689,651,614,580,547,516,487,460,434
	dc.w	410,387,365,345,325,307,290,274,258,244,230,217
	dc.w	205,193,183,172,163,154,145,137,129,122,115,109
; Tuning 7
	dc.w	814,768,725,684,646,610,575,543,513,484,457,431
	dc.w	407,384,363,342,323,305,288,272,256,242,228,216
	dc.w	204,192,181,171,161,152,144,136,128,121,114,108
; Tuning -8
	dc.w	907,856,808,762,720,678,640,604,570,538,508,480
	dc.w	453,428,404,381,360,339,320,302,285,269,254,240
	dc.w	226,214,202,190,180,170,160,151,143,135,127,120
; Tuning -7
	dc.w	900,850,802,757,715,675,636,601,567,535,505,477
	dc.w	450,425,401,379,357,337,318,300,284,268,253,238
	dc.w	225,212,200,189,179,169,159,150,142,134,126,119
; Tuning -6
	dc.w	894,844,796,752,709,670,632,597,563,532,502,474
	dc.w	447,422,398,376,355,335,316,298,282,266,251,237
	dc.w	223,211,199,188,177,167,158,149,141,133,125,118
; Tuning -5
	dc.w	887,838,791,746,704,665,628,592,559,528,498,470
	dc.w	444,419,395,373,352,332,314,296,280,264,249,235
	dc.w	222,209,198,187,176,166,157,148,140,132,125,118
; Tuning -4
	dc.w	881,832,785,741,699,660,623,588,555,524,494,467
	dc.w	441,416,392,370,350,330,312,294,278,262,247,233
	dc.w	220,208,196,185,175,165,156,147,139,131,123,117
; Tuning -3
	dc.w	875,826,779,736,694,655,619,584,551,520,491,463
	dc.w	437,413,390,368,347,328,309,292,276,260,245,232
	dc.w	219,206,195,184,174,164,155,146,138,130,123,116
; Tuning -2
	dc.w	868,820,774,730,689,651,614,580,547,516,487,460
	dc.w	434,410,387,365,345,325,307,290,274,258,244,230
	dc.w	217,205,193,183,172,163,154,145,137,129,122,115
; Tuning -1
	dc.w	862,814,768,725,684,646,610,575,543,513,484,457
	dc.w	431,407,384,363,342,323,305,288,272,256,242,228
	dc.w	216,203,192,181,171,161,152,144,136,128,121,114

musintname	dc.b "Protracker MusicInt",0
CIAAname	dc.b "ciaa.resource",0

filename        dc.b     "arpo4",0


sidnimi         dc.b    "playsid.library",0
gfxname         dc.b    "graphics.library",0
intuiname       dc.b    "intuition.library",0
dosname		dc.b    "dos.library",0

starttext       dc.b    "Instead of romantic pictures",0
                dc.b    "from Defender of the Crown",0
                dc.b    "I decided to use other pics",0
                dc.b    "Copyright might been an issue",0
                dc.b    "But now.. To this day..",0
                dc.b    "Only Amiga makes this possible!",0
                dc.b    "Today this is still true!",0
                dc.b    "Amazing!!!",0
                dc.b    "Amiga has this little thing called:",0
                dc.b    "Copper",0
                dc.b    "Watch the next screen!",0
                dc.b    "                      ",0
                dc.b    1

                even
scrolltext      dc.b    "SCREEN DIVIDED INTO THREE AREAS TWO OF THEM HAVING DIFFERENT BITPLANE DEPTH AND ALL OF THEM DIFFERENT PALETTE           ",0
        even

modelScrolltext
                dc.b    "THE PIC ABOVE HAS ONLY FIVE BITPLANES BECAUSE I COULD NOT GET AN AGA COPPERLIST WORKING WITH THE EFFECT              ",0

stretchText     dc.b    "The music now playing is",0
                dc.b    "A traditional Protracker mod.",0
                dc.b    "Let's give the 68040 CPU",0
                dc.b    "Quite a bit more work:",0
                dc.b    "Let's emulatate C64's SID!",0
                dc.b    "Let's go!",0
                dc.b    2
                dc.b    "All right! Now you can hear",0
                dc.b    "The sound of the mighty SID!",0
                dc.b    "I'm using the playsid.library.",0
                dc.b    "Now, this is RETRO!!!",0
                dc.b    "The sound of the mighty",0
                dc.b    "Commodore 64",0
                dc.b    1
                       
                even

endtext         dc.b    "What have we seen here?",0
                dc.b    "68040 assembly code",0
                dc.b    "using the built-in FPU, too.",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "Some romantic graphics.",0
                dc.b    "  ",0
                dc.b    "At first those pics were",0
                dc.b    "32 colors only, but I thought,",0
                dc.b    "that it was even too retro.. :-)",0
                dc.b    "Now the pics are 256 color pics.",0
                dc.b    "(except the pic in the previous",0
                dc.b    "part was only 32 colors...)",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "A sinewave scroller.",0
                dc.b    "  ",0
                dc.b    "It's code is highly optimized of",0
                dc.b    "what I've done earlier...",0
                dc.b    "   ",0
                dc.b    "That part (the 1st sine scroller",0
                dc.b    "part) had more colors",0
                dc.b    "than an AGA Amiga could",0
                dc.b    "normally show without HAM",0
                dc.b    "(hold and modify) at once!",0
                dc.b    "  ",0
                dc.b    "(309 colors.)",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "I've never done an AGA",0
                dc.b    "copperlist before!",0
                dc.b    "                  ",0
                dc.b    " ",0
                dc.b    "Unlimited bobs.",0
                dc.b    "               ",0
                dc.b    "  ",0
                dc.b    "Traditional Copper",0
                dc.b    "stretch effect.",0
                dc.b    "   ",0
                dc.b    "   ",0
                dc.b    "3D stars",0
                dc.b    "This was just a poor",0
                dc.b    "experiment to use the FPU",0
                dc.b    "with the stars...",0
                dc.b    "   ",0
                dc.b    "   ",0
                dc.b    "   ",0
                dc.b    "Line by line bitplane",0
                dc.b    "pointer manipulation",0
                dc.b    "with the Copper.....",0
                dc.b    "   ",0
                dc.b    "   ",0
                dc.b    "   ",0
                dc.b    "What about the music?",0
                dc.b    "                      ",0
                dc.b    "The first tune is Protracker",0
                dc.b    "tune from the Aminet",0
                dc.b    "and is called:",0
                dc.b    "   ",0
                dc.b    "Almost Real",0
                dc.b    "   ",0
                dc.b    "   ",0
                dc.b    "The 2nd tune uses..",0
                dc.b    "Amiga's playsid.library!",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "You are still hearing the sound",0
                dc.b    "of mighty SID chip of the",0
                dc.b    "immortal Commodore 64!",0
                dc.b    "               ",0
                dc.b    "  ",0
                dc.b    "The SID tune is to me from an",0
                dc.b    "unknown artist.",0
                dc.b    " ",0
                dc.b    "This tune is called",0
                dc.b    "  ",0
                dc.b    "Arpo4",0
                dc.b    "                  ",0
                dc.b    "                  ",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "..and this little",0
                dc.b    "demo is called:",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "Defender of the Amiga",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "  ",0
                dc.b    "THANKS FOR WATCHING!!!",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    " ",0
                dc.b    1


        section variables,DATA



RealTempo	    dc.w 125
CIAAaddr	    dc.l 0
CIAAbase	    dc.l 0
TimerFlag	    dc.l 0
TimerValue	    dc.l 0

mt_chan1temp	dc.l	0,0,0,0,0,$00010000,0,  0,0,0,0
mt_chan2temp	dc.l	0,0,0,0,0,$00020000,0,  0,0,0,0
mt_chan3temp	dc.l	0,0,0,0,0,$00040000,0,  0,0,0,0
mt_chan4temp	dc.l	0,0,0,0,0,$00080000,0,  0,0,0,0

mt_SampleStarts	dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

mt_SongDataPtr	dc.l 0
mt_speed	dc.b 6
mt_counter	dc.b 0
mt_SongPos	dc.b 0
mt_PBreakPos	dc.b 0
mt_PosJumpFlag	dc.b 0
mt_PBreakFlag	dc.b 0
mt_LowMask	dc.b 0
mt_PattDelTime	dc.b 0
mt_PattDelTime2	dc.b 0
mt_Enable	dc.b 0
mt_PatternPos	dc.w 0
mt_DMACONtemp	dc.w 0

modelPart       dc.b     0
ch0             dc.b     0
ch1             dc.b     0
ch2             dc.b     0
ch3             dc.b     0
                even

channels        dc.w     1
                dc.w     1
                dc.w     1
                dc.w     1

sidbase         dc.l     0

outfile         dc.l    0

sidfile         dc.l    0
emulrc          dc.l    -1
ri              dc.l    0
header          dc.l    0
tune            dc.w    0
size            dc.w    0
cm              dc.l    0
fh              dc.l    0
fileread        dc.l    0

wbenchmsg       dc.l    0
oldview         dc.l    0
process         dc.l    0
processor       dc.w    0
chiprev         dc.b    0
                even

intuibase       dc.l    0
gfxbase         dc.l    0
dosbase		    dc.l    0

y_paikka        dc.l    0

fontw           dc.l    32
t_pointer       dc.l    0
at_pointer      dc.l    0
et_pointer      dc.l    0
DrawScreen1     dc.l    0
ShowScreen1     dc.l    0
bitmap01        dc.l    0
bitmap02        dc.l    0
DrawScreen      dc.l    0
ShowScreen      dc.l    0

st_pointer      dc.l    0
modelBitmap     dc.l    0
piirraScreen    dc.l    0
screens         dc.l    0
bitmap1         dc.l    0
bitmap2         dc.l    0
scrollarea      dc.l    0
omascreen       dc.l    0
scrP            dc.l    0
pointX          dc.w    4
pointY          dc.w    4
dx              dc.w    2
dy              dc.w    2

which           dc.b    1
lastPart        dc.b    0
screeni         dc.b    0
vikaosa         dc.b    0
frame           dc.b    0
stretchi        dc.b    0
buttonreleased  dc.b    0
buttonpressed   dc.b    0
                even

bitmap          dc.l    0
starsAika       dc.l    0

Coords          ds.w    3*512           ; x,y,z

        section ChipData,DATA,CHIP

StartCopper:
        dc.w    $00e0
high01:  dc.w    $0000
        dc.w    $00e2
low01:   dc.w    $0000
        dc.w    $00e4
high02:  dc.w    $0000
        dc.w    $00e6
low02:   dc.w    $0000
        dc.w    $00e8
high03:  dc.w    $0000
        dc.w    $00ea
low03:   dc.w    $0000
        dc.w    $00ec
high04:  dc.w    $0000
        dc.w    $00ee
low04:   dc.w    $0000
        dc.w    $00f0
high05:  dc.w    $0000
        dc.w    $00f2
low05:   dc.w    $0000
        dc.w    $00f4
high06:  dc.w    $0000
        dc.w    $00f6
low06:   dc.w    $0000
        dc.w    $00f8
high07:  dc.w    $0000
        dc.w    $00fa
low07:   dc.w    $0000
        dc.w    $00fc
high08:  dc.w    $0000
        dc.w    $00fe
low08:   dc.w    $0000

        dc.w    $0100,$0010 ; BPLCON0 8 bitplanes...
        dc.w    $0102,$0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        
        dc.w    $010c,$0000 ; BPLCON4
        dc.w    $01fc,$0000 ; FMODE
        dc.w    $0108,0 ; BPL1MOD
        dc.w    $010a,0 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP 

        
; Colors of them
    DC.W $106,$0000 ; BPLCON3
    DC.W $0180,$0100,$0182,$0B85,$0184,$0800,$0186,$0CA9
	DC.W $0188,$0D83,$018A,$0844,$018C,$0400,$018E,$0FB5
	DC.W $0190,$0631,$0192,$0D86,$0194,$0C20,$0196,$0C64
	DC.W $0198,$0841,$019A,$0864,$019C,$0F96,$019E,$0522
	DC.W $01A0,$0A87,$01A2,$0866,$01A4,$0200,$01A6,$0600
	DC.W $01A8,$0ED9,$01AA,$0B86,$01AC,$0D96,$01AE,$0A64
	DC.W $01B0,$0A10,$01B2,$0C97,$01B4,$0EB8,$01B6,$0311
	DC.W $01B8,$0311,$01BA,$0EB9,$01BC,$0300,$01BE,$0ED8

    dc.w $106,$2000
	DC.W $0180,$0D97,$0182,$0420,$0184,$0A41,$0186,$0FB7
	DC.W $0188,$0533,$018A,$0DA7,$018C,$0B98,$018E,$0FB8
	DC.W $0190,$0E83,$0192,$0ECB,$0194,$0EB9,$0196,$0821
	DC.W $0198,$0F85,$019A,$0822,$019C,$0620,$019E,$0C87
	DC.W $01A0,$0100,$01A2,$0411,$01A4,$0975,$01A6,$0FB9
	DC.W $01A8,$0410,$01AA,$0A76,$01AC,$0A63,$01AE,$0A74
	DC.W $01B0,$0754,$01B2,$0A42,$01B4,$0200,$01B6,$0100
	DC.W $01B8,$0200,$01BA,$0621,$01BC,$0965,$01BE,$0C41

    dc.w $106,$4000
	DC.W $0180,$0EA7,$0182,$0EA5,$0184,$0C97,$0186,$0510
	DC.W $0188,$0B97,$018A,$0731,$018C,$0300,$018E,$0FC7
	DC.W $0190,$0EDC,$0192,$0310,$0194,$0400,$0196,$0A20
	DC.W $0198,$0FA6,$019A,$0FC8,$019C,$0A31,$019E,$0300
	DC.W $01A0,$0E50,$01A2,$0643,$01A4,$0ECB,$01A6,$0B84
	DC.W $01A8,$0300,$01AA,$0DA8,$01AC,$0920,$01AE,$0A75
	DC.W $01B0,$0FCA,$01B2,$0A87,$01B4,$0932,$01B6,$0C52
	DC.W $01B8,$0842,$01BA,$0B20,$01BC,$0B75,$01BE,$0FA7

    dc.w $106,$6000
	DC.W $0180,$0DA8,$0182,$0B41,$0184,$0100,$0186,$0754
	DC.W $0188,$0721,$018A,$0876,$018C,$0EEA,$018E,$0532
	DC.W $0190,$0100,$0192,$0732,$0194,$0FA4,$0196,$0865
	DC.W $0198,$0D97,$019A,$0510,$019C,$0853,$019E,$0743
	DC.W $01A0,$0521,$01A2,$0C86,$01A4,$0FA5,$01A6,$0A77
	DC.W $01A8,$0B87,$01AA,$0C96,$01AC,$0977,$01AE,$0FC9
	DC.W $01B0,$0F93,$01B2,$0DA8,$01B4,$0B98,$01B6,$0B76
	DC.W $01B8,$0300,$01BA,$0EA8,$01BC,$0976,$01BE,$0400

    dc.w $106,$8000
	DC.W $0180,$0A31,$0182,$0E64,$0184,$0C30,$0186,$0E52
	DC.W $0188,$0FFD,$018A,$0C76,$018C,$0632,$018E,$0C41
	DC.W $0190,$0A54,$0192,$0E74,$0194,$0D85,$0196,$0DA9
	DC.W $0198,$0E72,$019A,$0C63,$019C,$0EEB,$019E,$0C75
	DC.W $01A0,$0D20,$01A2,$0C61,$01A4,$0321,$01A6,$0755
	DC.W $01A8,$0533,$01AA,$0B52,$01AC,$0951,$01AE,$0D30
	DC.W $01B0,$0B63,$01B2,$0E60,$01B4,$0854,$01B6,$0A86
	DC.W $01B8,$0D95,$01BA,$0B10,$01BC,$0644,$01BE,$0B31

    dc.w $106,$a000
	DC.W $0180,$0742,$0182,$0D31,$0184,$0942,$0186,$0FFE
	DC.W $0188,$0E75,$018A,$0741,$018C,$0F72,$018E,$0D52
	DC.W $0190,$0810,$0192,$0EB6,$0194,$0962,$0196,$0E97
	DC.W $0198,$0FEB,$019A,$0710,$019C,$0D96,$019E,$0C21
	DC.W $01A0,$0D74,$01A2,$0F94,$01A4,$0FCB,$01A6,$0D51
	DC.W $01A8,$0FD9,$01AA,$0D86,$01AC,$0D73,$01AE,$0D64
	DC.W $01B0,$0A65,$01B2,$0FB9,$01B4,$0532,$01B6,$0CA9
	DC.W $01B8,$0765,$01BA,$0942,$01BC,$0FDB,$01BE,$0EC8

    dc.w $106,$c000
	DC.W $0180,$0975,$0182,$0720,$0184,$0521,$0186,$0FA6
	DC.W $0188,$0C41,$018A,$0D76,$018C,$0732,$018E,$0954
	DC.W $0190,$0DBA,$0192,$0EA8,$0194,$0B73,$0196,$0520
	DC.W $0198,$0B41,$019A,$0832,$019C,$0A10,$019E,$0421
	DC.W $01A0,$0B53,$01A2,$0FE9,$01A4,$0831,$01A6,$0C98
	DC.W $01A8,$0F83,$01AA,$0B96,$01AC,$0C98,$01AE,$0E85
	DC.W $01B0,$0FD8,$01B2,$0DA5,$01B4,$0511,$01B6,$0931
	DC.W $01B8,$0E51,$01BA,$0E63,$01BC,$0843,$01BE,$0E83

    dc.w $106,$e000
	DC.W $0180,$0B31,$0182,$0C53,$0184,$0732,$0186,$0E73
	DC.W $0188,$0D61,$018A,$0B42,$018C,$0311,$018E,$0610
	DC.W $0190,$0742,$0192,$0EBA,$0194,$0F73,$0196,$0910
	DC.W $0198,$0FB6,$019A,$0754,$019C,$0953,$019E,$0F74
	DC.W $01A0,$0A73,$01A2,$0EA6,$01A4,$0632,$01A6,$0963
	DC.W $01A8,$0ECB,$01AA,$0643,$01AC,$0D42,$01AE,$0FFD
	DC.W $01B0,$0E61,$01B2,$0D40,$01B4,$0966,$01B6,$0B54
	DC.W $01B8,$0FEC,$01BA,$0D96,$01BC,$0A87,$01BE,$0410





        dc.w    $FFDF,$FFFE ; wait for PAL area
        dc.w    $0106,$0000
        dc.w    $1901,$ff00,$01be,$0ed8
        dc.w    $1a01,$ff00,$01be,$0cb6
        dc.w    $1b01,$ff00,$01be,$0a94
        dc.w    $1c01,$ff00,$01be,$0872
        dc.w    $1d01,$ff00,$01be,$0650
        dc.w    $1e01,$ff00,$01be,$0430
        dc.w    $1f01,$ff00,$01be,$0210
        dc.w    $2001,$ff00,$01be,$0100
        dc.w    $ffff,$fffe

Copperlist:
        dc.w    $00e0
high1:  dc.w    $0000
        dc.w    $00e2
low1:   dc.w    $0000
        dc.w    $00e4
high2:  dc.w    $0000
        dc.w    $00e6
low2:   dc.w    $0000
        dc.w    $00e8
high3:  dc.w    $0000
        dc.w    $00ea
low3:   dc.w    $0000
        dc.w    $00ec
high4:  dc.w    $0000
        dc.w    $00ee
low4:   dc.w    $0000
        dc.w    $00f0
high5:  dc.w    $0000
        dc.w    $00f2
low5:   dc.w    $0000
        dc.w    $00f4
high6:  dc.w    $0000
        dc.w    $00f6
low6:   dc.w    $0000
        dc.w    $00f8
high7:  dc.w    $0000
        dc.w    $00fa
low7:   dc.w    $0000
        dc.w    $00fc
high8:  dc.w    $0000
        dc.w    $00fe
low8:   dc.w    $0000

        dc.w    $0100,$0010 ; BPLCON0 8 bitplanes...
        dc.w    $0102       ; BPLCON1
con1    dc.w    $0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0108,0 ; BPL1MOD
        dc.w    $010a,0 ; BPL2MOD        
        
        dc.w    $010c,$0000 ; BPLCON4
        dc.w    $01fc,$0000 ; FMODE
        
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP 

; Colors of the lady
Colours
    DC.W $106,$0000 ; BPLCON3
	DC.W $0180,$0000,$0182,$0821,$0184,$0400,$0186,$0976
	DC.W $0188,$0112,$018A,$0B98,$018C,$0D84,$018E,$0E96
	DC.W $0190,$0200,$0192,$0322,$0194,$0532,$0196,$0101
	DC.W $0198,$0D86,$019A,$0D98,$019C,$0854,$019E,$0EB9
	DC.W $01A0,$0E96,$01A2,$0222,$01A4,$0122,$01A6,$0E86
	DC.W $01A8,$0211,$01AA,$0B54,$01AC,$0544,$01AE,$0EA7
	DC.W $01B0,$0ECB,$01B2,$0EB8,$01B4,$0100,$01B6,$0FB9
	DC.W $01B8,$0D96,$01BA,$0FB8,$01BC,$0D97,$01BE,$0432

    dc.w $106,$2000
	DC.W $0180,$0D86,$0182,$0311,$0184,$0EB7,$0186,$0632
	DC.W $0188,$0112,$018A,$0D96,$018C,$0621,$018E,$0865
	DC.W $0190,$0322,$0192,$0100,$0194,$0211,$0196,$0211
	DC.W $0198,$0520,$019A,$0422,$019C,$0111,$019E,$0200
	DC.W $01A0,$0FA8,$01A2,$0FC8,$01A4,$0B32,$01A6,$0D97
	DC.W $01A8,$0865,$01AA,$0522,$01AC,$0EA8,$01AE,$0123
	DC.W $01B0,$0941,$01B2,$0E85,$01B4,$0122,$01B6,$0FB8
	DC.W $01B8,$0321,$01BA,$0EB8,$01BC,$0EB9,$01BE,$0EB8


    dc.w $106,$4000
	DC.W $0180,$0D85,$0182,$0E96,$0184,$0FA7,$0186,$0FB8
	DC.W $0188,$0112,$018A,$0212,$018C,$0A64,$018E,$0FB8
	DC.W $0190,$0DA7,$0192,$0FA7,$0194,$0EA7,$0196,$0533
	DC.W $0198,$0C75,$019A,$0632,$019C,$0FB8,$019E,$0EA7
	DC.W $01A0,$0D85,$01A2,$0B86,$01A4,$0112,$01A6,$0211
	DC.W $01A8,$0FB8,$01AA,$0323,$01AC,$0FB8,$01AE,$0322
	DC.W $01B0,$0112,$01B2,$0533,$01B4,$0100,$01B6,$0FB8
	DC.W $01B8,$0422,$01BA,$0211,$01BC,$0FB7,$01BE,$0942

    dc.w $106,$6000
	DC.W $0180,$0FB8,$0182,$0D64,$0184,$0DA7,$0186,$0A75
	DC.W $0188,$0112,$018A,$0B64,$018C,$0110,$018E,$0422
	DC.W $0190,$0EA7,$0192,$0743,$0194,$0B86,$0196,$0122
	DC.W $0198,$0322,$019A,$0122,$019C,$0EA7,$019E,$0322
	DC.W $01A0,$0FA7,$01A2,$0C74,$01A4,$0EA7,$01A6,$0222
	DC.W $01A8,$0FB7,$01AA,$0300,$01AC,$0E96,$01AE,$0FB8
	DC.W $01B0,$0644,$01B2,$0221,$01B4,$0210,$01B6,$0EA8
	DC.W $01B8,$0EA7,$01BA,$0D97,$01BC,$0A65,$01BE,$0E86

    dc.w $106,$8000
	DC.W $0180,$0D96,$0182,$0B53,$0184,$0832,$0186,$0C88
	DC.W $0188,$0754,$018A,$0A31,$018C,$0632,$018E,$0CA8
	DC.W $0190,$0A54,$0192,$0932,$0194,$0A76,$0196,$0953
	DC.W $0198,$0987,$019A,$0E96,$019C,$0755,$019E,$0B43
	DC.W $01A0,$0D53,$01A2,$0B97,$01A4,$0421,$01A6,$0D74
	DC.W $01A8,$0644,$01AA,$0941,$01AC,$0843,$01AE,$0D75
	DC.W $01B0,$0720,$01B2,$0CA9,$01B4,$0854,$01B6,$0EA8
	DC.W $01B8,$0842,$01BA,$0976,$01BC,$0410,$01BE,$0732

    dc.w $106,$a000
	DC.W $0180,$0A53,$0182,$0975,$0184,$0754,$0186,$0732
	DC.W $0188,$0A87,$018A,$0E97,$018C,$0C87,$018E,$0732
	DC.W $0190,$0FCB,$0192,$0A54,$0194,$0A76,$0196,$0B42
	DC.W $0198,$0B63,$019A,$0521,$019C,$0C64,$019E,$0C96
	DC.W $01A0,$0C75,$01A2,$0E64,$01A4,$0421,$01A6,$0765
	DC.W $01A8,$0743,$01AA,$0A43,$01AC,$0533,$01AE,$0B76
	DC.W $01B0,$0853,$01B2,$0721,$01B4,$0A52,$01B6,$0932
	DC.W $01B8,$0754,$01BA,$0EA8,$01BC,$0954,$01BE,$0B43

    dc.w $106,$c000
	DC.W $0180,$0210,$0182,$0843,$0184,$0B98,$0186,$0433
	DC.W $0188,$0965,$018A,$0633,$018C,$0D86,$018E,$0976
	DC.W $0190,$0310,$0192,$0D63,$0194,$0A53,$0196,$0520
	DC.W $0198,$0C98,$019A,$0A63,$019C,$0C86,$019E,$0511
	DC.W $01A0,$0654,$01A2,$0643,$01A4,$0F97,$01A6,$0655
	DC.W $01A8,$0543,$01AA,$0A75,$01AC,$0742,$01AE,$0E86
	DC.W $01B0,$0EB9,$01B2,$0631,$01B4,$0B76,$01B6,$0A87
	DC.W $01B8,$0C53,$01BA,$0B64,$01BC,$0E95,$01BE,$0D75

    dc.w $106,$e000
	DC.W $0180,$0A54,$0182,$0A76,$0184,$0C42,$0186,$0A54
	DC.W $0188,$0B75,$018A,$0842,$018C,$0C85,$018E,$0732
	DC.W $0190,$0954,$0192,$0E96,$0194,$0C97,$0196,$0D76
	DC.W $0198,$0EA6,$019A,$0D97,$019C,$0532,$019E,$0B64
	DC.W $01A0,$0432,$01A2,$0111,$01A4,$0321,$01A6,$0A42
	DC.W $01A8,$0D85,$01AA,$0743,$01AC,$0B43,$01AE,$0C75
	DC.W $01B0,$0876,$01B2,$0753,$01B4,$0C75,$01B6,$0521
	DC.W $01B8,$0422,$01BA,$0B97,$01BC,$0942,$01BE,$0E99


        dc.w    $b201,$ff00
        
        dc.w    $106,$0000

        dc.w    $00e0
high11  dc.w    $0000
        dc.w    $00e2
low11   dc.w    $0000
        dc.w    $00e4
high22  dc.w    $0000
        dc.w    $00e6
low22   dc.w    $0000
        dc.w    $00e8
high33  dc.w    $0000
        dc.w    $00ea
low33   dc.w    $0000
        dc.w    $00ec
high44  dc.w    $0000
        dc.w    $00ee
low44   dc.w    $0000

        dc.w    $0100,$4200 ; BPLCON0 one bitplane...
        dc.w    $0102,$0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0108,$0000 ; BPL1MOD
        dc.w    $010a,$0000 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP 

; Font colors
        DC.W $0180,$0000,$0182,$0FFF,$0184,$0DEC,$0186,$0BDA
	DC.W $0188,$0AC7,$018A,$08A7,$018C,$0797,$018E,$0687
	DC.W $0190,$0577,$0192,$0467,$0194,$0356,$0196,$0255
	DC.W $0198,$0243,$019A,$0223,$019C,$0212,$019E,$0202

channel0Cols
        dc.w    $c001,$ff00,$0180,$0000
        dc.w    $c101,$ff00,$0180,$0000
        dc.w    $c201,$ff00,$0180,$0000
        dc.w    $c301,$ff00,$0180,$0000
        dc.w    $c401,$ff00,$0180,$0000
        dc.w    $c501,$ff00,$0180,$0000
        dc.w    $c601,$ff00,$0180,$0000
        dc.w    $c701,$ff00,$0180,$0000
        dc.w    $c801,$ff00,$0180,$0000
        dc.w    $c901,$ff00,$0180,$0000
        dc.w    $ca01,$ff00,$0180,$0000
        dc.w    $cb01,$ff00,$0180,$0000
        dc.w    $cc01,$ff00,$0180,$0000
        dc.w    $cd01,$ff00,$0180,$0000

        dc.w    $ce01,$ff00,$0180,$0000

channel1Cols
        dc.w    $d001,$ff00,$0180,$0000
        dc.w    $d101,$ff00,$0180,$0000
        dc.w    $d201,$ff00,$0180,$0000
        dc.w    $d301,$ff00,$0180,$0000
        dc.w    $d401,$ff00,$0180,$0000
        dc.w    $d501,$ff00,$0180,$0000
        dc.w    $d601,$ff00,$0180,$0000
        dc.w    $d701,$ff00,$0180,$0000
        dc.w    $d801,$ff00,$0180,$0000
        dc.w    $d901,$ff00,$0180,$0000
        dc.w    $da01,$ff00,$0180,$0000
        dc.w    $db01,$ff00,$0180,$0000
        dc.w    $dc01,$ff00,$0180,$0000
        dc.w    $dd01,$ff00,$0180,$0000
        dc.w    $df01,$ff00,$0180,$0000

        dc.w    $e001,$ff00,$0180,$0000

channel2Cols
        dc.w    $e101,$ff00,$0180,$0000
        dc.w    $e201,$ff00,$0180,$0000
        dc.w    $e301,$ff00,$0180,$0000
        dc.w    $e401,$ff00,$0180,$0000
        dc.w    $e501,$ff00,$0180,$0000
        dc.w    $e601,$ff00,$0180,$0000
        dc.w    $e701,$ff00,$0180,$0000
        dc.w    $e801,$ff00,$0180,$0000
        dc.w    $e901,$ff00,$0180,$0000
        dc.w    $ea01,$ff00,$0180,$0000
        dc.w    $eb01,$ff00,$0180,$0000
        dc.w    $ec01,$ff00,$0180,$0000
        dc.w    $ed01,$ff00,$0180,$0000
        dc.w    $ee01,$ff00,$0180,$0000

        dc.w    $ef01,$ff00,$0180,$0000

channel3Cols
        dc.w    $f101,$ff00,$0180,$0000
        dc.w    $f201,$ff00,$0180,$0000
        dc.w    $f301,$ff00,$0180,$0000
        dc.w    $f401,$ff00,$0180,$0000
        dc.w    $f501,$ff00,$0180,$0000
        dc.w    $f601,$ff00,$0180,$0000
        dc.w    $f701,$ff00,$0180,$0000
        dc.w    $f801,$ff00,$0180,$0000
        dc.w    $f901,$ff00,$0180,$0000
        dc.w    $fa01,$ff00,$0180,$0000
        dc.w    $fb01,$ff00,$0180,$0000
        dc.w    $fc01,$ff00,$0180,$0000
        dc.w    $fd01,$ff00,$0180,$0000
        dc.w    $fe01,$ff00,$0180,$0000

        dc.w    $ff01,$ff00,$0180,$0000

        dc.w    $FFDF,$FFFE ; wait for PAL area
        dc.w    $0c01,$ff00

        dc.w    $0108,-80 ; BPL1MOD
        dc.w    $010a,-80 ; BPL2MOD

        DC.W $0180,$0000,$0182,$0ddd,$0184,$0bca,$0186,$09b8
	DC.W $0188,$08a5,$018A,$08A7,$018C,$0797,$018E,$0465
	DC.W $0190,$0155,$0192,$0245,$0194,$0134,$0196,$0033
	DC.W $0198,$0021,$019A,$0001,$019C,$0000,$019E,$0000

        dc.w    $ffff,$fffe

PointCopper:
        dc.w    $00e0
high1p  dc.w    $0000
        dc.w    $00e2
low1p   dc.w    $0000
        dc.w    $00e4
high2p  dc.w    $0000
        dc.w    $00e6
low2p   dc.w    $0000

        dc.w    $0100,$2200 ; BPLCON0 one bitplane...
        dc.w    $0102,$0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0108,0 ; BPL1MOD
        dc.w    $010a,0 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP

	dc.w	$0182,$0D32,$0184,$0E99,$0186,$0FBB

        dc.w    $ffff,$fffe


StretchCopper:
        dc.w    $00e0
high1s  dc.w    $0000
        dc.w    $00e2
low1s   dc.w    $0000


        dc.w    $0100,$1200 ; BPLCON0 one bitplane...
        dc.w    $0102,$0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0106,0     ; BPLCON3
        dc.w    $0108,0 ; BPL1MOD
        dc.w    $010a,0 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP

        dc.w    $0180,$0000
        dc.w    $0182,$0000

        dc.w    $9901,$ff00,$0182,$08cf

Stretch:
        ; here is the space for our stretch effect
        ds.w    64*6+4
        dc.w    $ffff,$fffe

StarsCopper:
        dc.w    $00e0
high1st dc.w    $0000
        dc.w    $00e2
low1st  dc.w    $0000
        dc.w    $00e4
high2st dc.w    $0000
        dc.w    $00e6
low2st  dc.w    $0000

        dc.w    $0100,$2200 ; BPLCON0 one bitplane...
        dc.w    $0102,$0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0108,0 ; BPL1MOD
        dc.w    $010a,0 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP

        dc.w    $0180,$0000
        dc.w    $0182,$0444
        dc.w    $0184,$0888
        dc.w    $0186,$0fff

        dc.w    $ffff,$fffe

VertCopper:
        dc.w    $00e0
high1v  dc.w    $0000
        dc.w    $00e2
low1v   dc.w    $0000


        dc.w    $0100,$1200 ; BPLCON0 one bitplane...
        dc.w    $0102,$0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0108,0 ; BPL1MOD
        dc.w    $010a,0 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP

        dc.w    $2801,$ff00,$0182,$0000
        dc.w    $2901,$ff00,$0182,$0000
        dc.w    $2a01,$ff00,$0182,$0000
        dc.w    $2b01,$ff00,$0182,$0000
        dc.w    $2c01,$ff00,$0182,$0000
        dc.w    $2d01,$ff00,$0182,$0000
        dc.w    $2e01,$ff00,$0182,$0000
        dc.w    $2f01,$ff00,$0182,$0000

        dc.w    $3001,$ff00,$0182,$0000
        dc.w    $3101,$ff00,$0182,$0111
        dc.w    $3201,$ff00,$0182,$0222
        dc.w    $3301,$ff00,$0182,$0333
        dc.w    $3401,$ff00,$0182,$0444
        dc.w    $3501,$ff00,$0182,$0555
        dc.w    $3601,$ff00,$0182,$0666
        dc.w    $3701,$ff00,$0182,$0777
        dc.w    $3801,$ff00,$0182,$0888
        dc.w    $3901,$ff00,$0182,$0999
        dc.w    $3a01,$ff00,$0182,$0aaa
        dc.w    $3b01,$ff00,$0182,$0bbb
        dc.w    $3c01,$ff00,$0182,$0ccc
        dc.w    $3d01,$ff00,$0182,$0ddd
        dc.w    $3f01,$ff00,$0182,$0eee
        dc.w    $4001,$ff00,$0182,$0fff



	    dc.w	$0180,$0000,$0182,$0fff

        dc.w    $FFDF,$FFFE ; wait for PAL area
        dc.w    $1801,$ff00,$0182,$0fff
        dc.w    $1901,$ff00,$0182,$0eee
        dc.w    $1a01,$ff00,$0182,$0ddd
        dc.w    $1b01,$ff00,$0182,$0ccc
        dc.w    $1c01,$ff00,$0182,$0bbb
        dc.w    $1d01,$ff00,$0182,$0aaa
        dc.w    $1e01,$ff00,$0182,$0999
        dc.w    $1f01,$ff00,$0182,$0888
        dc.w    $2001,$ff00,$0182,$0777
        dc.w    $2101,$ff00,$0182,$0666
        dc.w    $2201,$ff00,$0182,$0555
        dc.w    $2301,$ff00,$0182,$0444
        dc.w    $2401,$ff00,$0182,$0333
        dc.w    $2501,$ff00,$0182,$0222
        dc.w    $2601,$ff00,$0182,$0111
        dc.w    $2701,$ff00,$0182,$0000
        dc.w    $2801,$ff00,$0182,$0000
        dc.w    $2901,$ff00,$0182,$0000
        dc.w    $ffff,$fffe

ModelCopper:

        dc.w    $0100,$5200 ; BPLCON0 8 bitplanes...
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0106,0     ; BPLCON3
        dc.w    $0108,0 ; BPL1MOD
        dc.w    $010a,0 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP 

; Colors of the model

	DC.W $0180,$0000,$0182,$0159,$0184,$0333,$0186,$0124
	DC.W $0188,$0777,$018A,$048B,$018C,$0011,$018E,$0146
	DC.W $0190,$0357,$0192,$0222,$0194,$0345,$0196,$0455
	DC.W $0198,$0567,$019A,$0678,$019C,$0679,$019E,$047A
	DC.W $01A0,$0269,$01A2,$0468,$01A4,$079A,$01A6,$068C
	DC.W $01A8,$027B,$01AA,$028C,$01AC,$039D,$01AE,$05AE
	DC.W $01B0,$09AB,$01B2,$0AAA,$01B4,$07BE,$01B6,$0ABC
	DC.W $01B8,$05BF,$01BA,$07BF,$01BC,$08CF,$01BE,$0CDD


ModelBPL

        ds.w    20*159+2*159+2*159

        dc.w    $ca01,$ff00

        dc.w    $00e0
mhigh11 dc.w    $0000
        dc.w    $00e2
mlow11  dc.w    $0000
        dc.w    $00e4
mhigh22 dc.w    $0000
        dc.w    $00e6
mlow22  dc.w    $0000
        dc.w    $00e8
mhigh33 dc.w    $0000
        dc.w    $00ea
mlow33  dc.w    $0000
        dc.w    $00ec
mhigh44 dc.w    $0000
        dc.w    $00ee
mlow44  dc.w    $0000

        dc.w    $0100,$4200 ; BPLCON0 one bitplane...
        dc.w    $0102,$0000 ; BPLCON1
        dc.w    $0104,$0000 ; BPLCON2
        dc.w    $0108,$0000 ; BPL1MOD
        dc.w    $010a,$0000 ; BPL2MOD
        dc.w    $0092,$0038 ; DDFSTRT
        dc.w    $0094,$00d0 ; DDFSTOP
        dc.w    $008e,$2c81 ; DIWSTRT
        dc.w    $0090,$2cc1 ; DIWSTOP 

; Font colors
        DC.W $0180,$0000,$0182,$0FFF,$0184,$0DEC,$0186,$0BDA
	    DC.W $0188,$0AC7,$018A,$08A7,$018C,$0797,$018E,$0687
	    DC.W $0190,$0577,$0192,$0467,$0194,$0356,$0196,$0255
	    DC.W $0198,$0243,$019A,$0223,$019C,$0212,$019E,$0202

        dc.w $ffff,$fffe


channel0cols    dc.w    $0220
                dc.w    $0440
                dc.w    $0660
                dc.w    $0880
                dc.w    $0aa0
                dc.w    $0cc0
                dc.w    $0ff0
                dc.w    $0cc0
                dc.w    $0aa0
                dc.w    $0880
                dc.w    $0660
                dc.w    $0440
                dc.w    $0220

channel1cols    dc.w    $0202
                dc.w    $0404
                dc.w    $0606
                dc.w    $0808
                dc.w    $0a0a
                dc.w    $0c0c
                dc.w    $0f0f
                dc.w    $0c0c
                dc.w    $0a0a
                dc.w    $0808
                dc.w    $0606
                dc.w    $0404
                dc.w    $0202

channel2cols    dc.w    $0022
                dc.w    $0044
                dc.w    $0066
                dc.w    $0088
                dc.w    $00aa
                dc.w    $00cc
                dc.w    $00ff
                dc.w    $00cc
                dc.w    $00aa
                dc.w    $0088
                dc.w    $0066
                dc.w    $0044
                dc.w    $0022

channel3cols    dc.w    $0002
                dc.w    $0004
                dc.w    $0006
                dc.w    $0008
                dc.w    $000a
                dc.w    $000c
                dc.w    $000f
                dc.w    $000c
                dc.w    $000a
                dc.w    $0008
                dc.w    $0006
                dc.w    $0004
                dc.w    $0002

prevPer0        dc.w    0
prevPer1        dc.w    0
prevPer2        dc.w    0

clears  ds.b    64               ; for clearing the screen with 68040's move16

bob         incbin  "gfx/pallo.bob"
mask        incbin  "gfx/pallo.mask"

Model       incbin  "gfx/malli-amylle-32.raw"
Lady        incbin  "gfx/malli2-2-160x134.raw"
Together    incbin  "gfx/rannalla.raw2"
Font        incbin  "gfx/gradbubble-32x32-wip-4bpls.raw"
LittleFont  incbin  "gfx/littlefont-8x24-256.raw"


spaceg  ds.b    (32/8)*32

mt_data	dc.l	0


musa	incbin "music/almostreal1.mod"

        end
