// ********************************
// *          astro-panic
// *       by charles brannon
// ********************************
// * compute!'s gazette feb. 1984
// ********************************
// * dissembled with merlin-64
// * ported to KickAssmbler
// ********************************
// *   disassembled by chris of
// *    my developer thoughts
// *       youtube channel
// ********************************
 
// zero page usage
// fbfc txt mem adrs of bullet
// fdfe clr mem adrs of bullet
// b4 x pos of bullet
// a8 tmp for hibit of sprites
// a7 tmp for calc white saucer hit
// a9 saucer index (0..6)
// aa saucer lohi index (0,2,4,6..12)

             * = $c000
      
            jmp start

insirq:
            sei
            lda #$7f
            sta $dc0d               // disable cia 1 timer irq
            lda #$01
            sta $d01a               // enable raster compare irq
            lda #233              // ntsc 262 lines / 50-249 visible
            sta $d012               // irq at raster line 233
            lda #$1b
            sta $d011               // bit 8 of raster compare to 0
            lda #<irqh
            sta cinv
            lda #>irqh
            sta cinv+1              // set irq handler to our own at irqh
            cli
            rts
irqh:
            lda $d012
            cmp #233
            bne rasterln0           //  are we raster at 233?
            lda #$00
            sta $d012               //  next rst irq at line 0
            lda #$16                //  1000=char rom.1800 is rvrs char rom
            sta $d018               //  chars at 1800 scn at 0400
            lda #$c8
            sta $d016               // normal text mode
            lda #$0c
            sta $d021
            sta $d020               // brdr n scn to med grey
            lda #$01
            sta $d019               // ack raster irq
            jmp exitirq
rasterln0:
            lda #233
            sta $d012               // next rst irq at line 233
            lda #$1e
            sta $d018               // chars at 3800 scn at 0400
            lda #$d8                // 
            sta $d016               // multicolor txt mode
            lda #$00                // 
            sta $d020               // 
            sta $d021               // brdr n scn black
            lda #$01
            sta $d019               // ack raster irq
            inc $a2                 // low byte of jiffy clock
            jsr scnkey
            lda shflag
            ora datnopause
            beq setplyrx
            jmp exitirq
setplyrx:
            lda datplyrx            // set cannon x pos
            sta $d000
            lda $d010
            and #$fe
            ora datplyrx8
            sta $d010
            lda $dc00               // test bit2=0 (left on joy2)
            and #$04
            bne ifjoyright
            lda datplyrx8
            bne goleft              // move on if bit8=1 (x ppos>255)
            lda datplyrx
            cmp #25
            bcc ifjoyright          // skip ahead if player pos >=25
goleft:
            sec                     // move left 2 pixels
            lda datplyrx
            sbc #$02
            sta datplyrx
            lda datplyrx8
            sbc #$00
            sta datplyrx8
ifjoyright:
            lda $dc00               // test bit3=0 (right on joy2)
            and #$08
            bne isfirebtn           // skip ahead if not right
            lda datplyrx8
            beq goright             // mov on if bit8=0 (x pos<=255)
            lda datplyrx
            cmp #64                 // (320-256=64)
            bcs isfirebtn           // skip ahead if not max right
goright:
            clc                     // move right 2 pixels
            lda datplyrx
            adc #$02
            sta datplyrx
            lda datplyrx8
            adc #$00
            sta datplyrx8
isfirebtn:
            lda $dc00
            and #$10                // bit 4 = 0 = fire pressed
            bne exitirq
            lda datbltmv
            bne exitirq
            sec                     // start a new bullet from cannon
            lda datplyrx            // calc starting x pos of bullet
            sbc #24                 // in txt column (0-39) translated from
            sta $b4                 // sprite x (0-319)
            lda datplyrx8
            sbc #$00
            lsr
            ror $b4
            lsr $b4
            lsr $b4
            inc datbltmv            // flag that a bullet is now in flight
            clc                     // calc txt and color ram address
            lda $b4                 // of bullet and store in
            adc #$21                // fbfc txt   fdfe clrram
            sta $fb
            sta $fd
            lda #$07
            adc #$00
            sta $fc
            adc #$d4
            sta $fe
exitirq:
            pla
            tay
            pla
            tax
            pla
            rti


                                    // start of main code
start:
            jsr insirq
            lda #$04
            sta $fc                 // txt ram at 0400
            ldy #$00
bltllp:
            lda bltdata,y           // load bullet char
            sta $3800,y
            iny
            cpy #$08
            bne bltllp
            ldy #$00
            tya      
zero8:
            sta $3900,y             // load empty char
            iny
            cpy #$08
            bne zero8
            ldy #$00
sprldlp:
            lda sprites,y           // load 4 sprites
            sta $3a00,y
            iny
            bne sprldlp
            lda #$e8
            sta $07f8               // sprt0=cannon
            ldy #$07
            lda #$e9
scsplp:
            sta $07f8,y             // sprt1-7=saucer
            dey
            bne scsplp
            lda #$ff                // for all sprites..
            sta $d01c               // multicolor on
            lda #$00
            sta $d01d               // normal width
            sta $d017               // normal height
            sta $d010               // x bit 8 = 0
            lda #$03
            sta $d025               // multicolor0=3(cyan)
            lda #$08
            sta $d026               // multicolor1=4(purple)
            lda #212
            sta $d001               // cannon y pos
            ldy #$00
sauclrlp:
            lda sauclrs,y           // saucer colors
            sta $d028,y
            iny
            cpy #$07
            bne sauclrlp
            lda #$06
            sta $d027               // cannon color (blue)
            lda #$93
            jsr chrout              // clear screen
            ldy #39
initln1:
            lda #$a0                // fill screen row 23 with...
            sta $0798,y             // rvrs spc
            lda #$05
            sta $db98,y             // green colorram
            dey
            bpl initln1
            ldy #5
            ldx #24
            clc
            jsr plot                // move crsr to x=5 y=24
            ldy #$00
pschs:
            lda shstxt,y
            beq ttlplot
            jsr chrout              // print score/hscore
            iny
            bne pschs
ttlplot:
            ldy #0
            ldx #23
            clc
            jsr plot                // cursor to x=0 y=23
            ldy #$00
ttllp:
            lda ttltxt,y
            beq setmctxtclrs
            jsr chrout              // print astropanic lives and lvl
            iny
            bne ttllp
setmctxtclrs:
            lda #$04
            sta $d023               // text bg color 2 (purple)
            lda #$0e
            sta $d024               // txt ebg color 3 (lt.blue) 192-255
            ldy #$18
            lda #$00
initsid:
            sta $d400,y             // set all sid registers to 0
            dey
            bpl initsid
            lda #$ff
            sta $d40f               // v3 freq = ff00
            lda #$80
            sta $d412               // v3 noise waveform
            lda #$8f
            sta $d418               // v1-3 vol:1 low,band,hi fltrs on v3 off
            lda #$0f
            sta datlevel            // set level to 0 (00001111)
            lda #$03                // # of lives (also in text ram)
            sta datlives
initcannonx:
            lda #$00
            sta datbltmv
            sta datnopause
            tax
            stx datplyrx8           // set cannon bit8 x pos to 0
            lda #184              // init x pos of cannon
            sta datplyrx
initsaucerxy:
            txa                     // set all saucer init x y
            asl
            tay                     // a=x of the sprite (31,63,95...223)
            asl                     // a x=sprite index (0..6)
            asl                     // a y=sprite index*2 (0,2,4...12)
            asl
            asl
            clc
            adc #31
            sta datsaucxlo,y
            lda #$00
            sta datsaucxhi,y
            sta $d003,y             // set saucr y position
            lda #60
            sta datsaucy,x
            jsr rndizscrdir
            inx
            cpx #$07
            bne initsaucerxy
            lda #$ff
            sta $d015               // all sprites visible
            lda $d01e               // read to reset sp to sp collision det
            lda $d01f               // read to reset sp to char collision det
gameloop:
            lda $d01e               // did cannon hit a sprite?
            and #$01
            beq shftisdown          // no
            jmp loselife
shftisdown:
            lda shflag
            bne shftisdown
            jsr getin
            cmp #$88                // is f7 pressed
            bne bltmvchk
            lda #$20
            ldy #$00
            sta ($fb),y             // remove bullet from scn
            jmp newgame             // f7 pressed, reset game
bltmvchk:
            lda datbltmv
            bne mvblt
            jmp delay
mvblt:
            ldy #$00
            lda #$20                // space
            sta ($fb),y             // remove bullet from scn
            sec                     // -40 from txt + clr ram of blt
            lda $fb
            sbc #40
            sta $fb
            sta $fd
            lda $fc
            sbc #$00
            sta $fc
            clc
            adc #$d4                // hi txt + d4 = hi clr
            sta $fe
            lda $d41b               // read rnd num from sid voice 3
            ora #$08                // set bit4 (multicolor on)
            sta ($fd),y
            lda #$00
            sta ($fb),y             // store @ char in txt ram
            lda $d01f
            and #$fe                // did blt hit any sprt other than cannon?
            beq bltnohits           // no
            sta $a7
            sta datsp2chrslt
            ldx #$00
            lsr $a7                 // ignore sprt 0 check (cannon)
saucrhitlp:
            lsr $a7                 // move sprt colision bit into c flag
            bcc scrnothit           // not hit, skip ahead
            lda #$20                // sound of saucer getting hit voice 1
            sta $d405               // 0=decay / 16ms attack
            lda #$f6
            sta $d406               // 204ms release / full volume sustain
            lda #$81
            sta $d404               // start voice 1 with noise waveform
            lda #$ea
            sta $07f9,x             // show explosion sprite
            ldy #$0a
xplofrqlp:
            lda $d41b               // read rnd number from voice 3
            sta $d028,x             // set saucer explosion color
            sty $d401               // voice 1 frequency
            lda $a2
wait1jiffy:
            cmp $a2
            beq wait1jiffy
            dey
            bne xplofrqlp           // hi byte of frq from 10..0
            lda sauclrs,x
            sta $d028,x             // restore original saucer color
            lda #$e9
            sta $07f9,x             // restore to saucer sprite
            lda #$80
            sta $d404               // stop sound. leave vc1 as noise
            txa                     // x = sprt # tested for collision
            pha
            lda datsaucy,x          // calculate score factor
            eor #$ff                // invert # (60 becomes 195)
            lsr
            lsr
            lsr                     // divid by (becomes 24)
            jsr addtoscore
            pla
            tax
scrnothit:
            inx
            cpx #$07
            bne saucrhitlp
            lda datsp2chrslt
            eor #$ff                // toggle hit sprite off
            and $d015
            sta $d015
            jmp clearblt
bltnohits:
            inc datbltmv
            lda datbltmv
            cmp #21                 // is blt at top of scn? (moved 21 up)
            bne delay
clearblt:
            ldy #$00
            sty datbltmv            // clear bullet movin flag
            lda #$20
            sta ($fb),y             // erase bullet from screen
            lda $d01f               // reset sp - sp and sp - ch collision reg
            lda $d01e
            lda $d015
            and #$fe
            bne delay               // are any saucers left?
            lda datlevel            // move to next level
            beq initlevel           // 0 is lowest
            dec datlevel
initlevel:
            ldy #$26
            ldx #$17
            clc
            jsr plot                // mv cursr to level field on scn
            lda datlevel
            eor #$0f                // invert level (14 becomes 1)
            tax
            lda #$00                // hi value always 0
            jsr $bdcd               // print level
            lda #100              // add 100 to score
            jsr addtoscore
            jmp initcannonx
delay:
            ldx datlevel            // lvl 0=15, 1=14...
            ldy #$00                // lower level delays more
delaylp:
            iny
            bne delaylp
            dex
            bne delaylp
putsaucrx:
            txa                     // x=saucer (0..6) y=saucer(0..14)
            asl
            tay
            lda datsaucxlo,y
            sta $d002,y
            lda datsaucy,x
            sta $d003,y
            lda datsaucxhi,y
            sta $a8                 // bit 8 of x pos
            sec
            txa                     // set axy = saucer (0-6)
            tay
            iny
            iny
            lda #$00                // set bit for sprite x (0-6)
setspbitlp:
            rol
            dey
            bne setspbitlp
            sta $a7
            eor #$ff                // flip the bits (0010000 to 1101111)
            and $d010               // mask out other sprites msb x
            ldy $a8
            beq setsaucrmsbx
            ora $a7                 // set msb x for this sprite
setsaucrmsbx:
            sta $d010
            inx
            cpx #$07
            bne putsaucrx
            ldx #$00
movescrs:
            txa
            asl
            tay
            lda datsaucy,x
            clc
            adc cpyscrdir8,x        // add to the y pos.
            cmp #210
            bcs scrbottomscn        // is new y> 210?
            cmp #50
            bcs setnewscry          // is new y> 50?
scrbottomscn:
            jsr rndizscrdir
            jmp mvscrslp
setnewscry:
            sta datsaucy,x
            clc
            lda datsaucxlo,y
            adc cpyscrdir16,y       // add 16 bit value to x pos
            sta $a7                 // store in a7,a8 (lo,hi)
            lda datsaucxhi,y
            adc cpyscrdir16+1,y
            sta $a8
            bne scrxhichk           // is hi not 0?
            lda $a7
            cmp #31
            beq scrleftmost         // if lo = 31
            bcs scrxhichk           // if >= 31
scrleftmost:
            jsr rndizscrdir
            jmp mvscrslp
scrxhichk:
            lda $a8
            beq scrstorex           // is hi=0?
            lda $a7
            cmp #64
            bcc scrstorex           // is lo<64 (x <320? )
            jsr rndizscrdir
            jmp mvscrslp
scrstorex:
            lda $a7
            sta datsaucxlo,y
            lda $a8
            sta datsaucxhi,y
mvscrslp:
            inx
            cpx #$07
            bne movescrs
            jmp gameloop

                                    // x=index of saucer (0-6)
                                    // y=index of saucer lohi (0,2,4,6..12)
                                    // randomize direction of 1 saucer
rndizscrdir:
            stx $a9
            sty $aa
            lda $d41b               // rnd nmbr from vc3
            and #$05                // only 0,1,4 or 5
            tax
            lda datscrdir8,x
            ldx $a9
            sta cpyscrdir8,x
            lda $d41b               // rnd nmbr from vc3
            and #$05                // only 0,1,4 or 5
            asl
            tay
            lda datscrdir16,y
            ldx $aa
            sta cpyscrdir16,x
            lda datscrdir16+1,y
            sta cpyscrdir16+1,x
            ldy $aa
            ldx $a9
            rts
loselife:
            lda #$eb                // select alt cannon sprite
            sta $07f8
            lda #$01
            sta datnopause
            lda #$09
            sta $d405               // atk:0 dly: 9
            lda #$a0
            sta $d406               // rls:0 sus:10
            lda #$21
            sta $d404               // start sound sawtooth
            ldx #$64
lsesndlpx:
            stx $d401               // vc1:freq hi
            ldy #$00
losesndlpy:
            lda $d41b               // oad rnd value from v3
            sta $d027               // prite 0 color
            sta $d400               // c1:freq lo
            dey
            bne losesndlpy
            dex
            bne lsesndlpx
            lda #$ea                // set sprt0 to explosion
            sta $07f8
            lda #$01
            sta $d01d               // sprt0 dblwidth
            sta $d017               // sprt0 dblheight
            lda #$20
            sta $d404               // start rels cycl vc1: sawtooth
            lda #$a8
            sta $d406               // vc1 rls:8 sus:10
            lda #$81
            sta $d404               // start noise wvfrm
            ldx #$64
noiselpx:
            stx $d401               // vc1 frq hi
            ldy #$00
noiselpy:
            sty $d400               // vc1 frq lo
            lda $d41b               // read rnd from vc3
            sta $d027               // sprt0 color
            dey
            bne noiselpy
            dex
            bne noiselpx
            lda #$e8                // spr0: normal cannon
            sta $07f8
            lda #$06
            sta $d027               // spr0: color blue
            lda #$00
            sta $d01d
            sta $d017               // spr0: normal width n height
            lda #$80
            sta $d404               // vc1: release noise
            ldx #$64
            ldy #$00
vc1rlslp:
            dey
            bne vc1rlslp
            dex
            bne vc1rlslp
            lda #$00
            sta datnopause
            tay
scrsprt0lp:
            sta $d002,y             // y=saucer sprite. set x and y to 0
            iny
            cpy #$0e
            bne scrsprt0lp
            sta $d010               // all sprites msb x to 0
            ldy #$00
            lda #$20
            sta ($fb),y             // clear bullet from screen
            lda $d01e               // reset spr to spr collision register
            dec $079e               // -1 to life on the screen
            dec datlives
            lda datlives
            beq gameover
            jmp initcannonx
gameover:
            ldy #$00
chkhisclp:
            lda $07cb,y             // cmp each digit in score to hiscore
            cmp $07de,y
            beq chknxtdigit
            bcs cptohiscore
            jmp waitforf7lp
chknxtdigit:
            iny
            cpy #$06
            bne chkhisclp
            jmp waitforf7lp
cptohiscore:
            ldy #$06
cptohilp:
            lda $07ca,y
            sta $07dd,y
            dey
            bne cptohilp
waitforf7lp:
            jsr scnkey
            jsr getin
            cmp #$88
            bne waitforf7lp
newgame:
            ldy #$06
            lda #$30                //'0' petscii
rstscorelp:
            sta $07ca,y             // write directly to txt ram
            dey
            bne rstscorelp
            jmp ttlplot
                                    // a= points to add to score
addtoscore:
            tax
addsclp1:
            ldy #$06
            sec
addsclp2:
            lda $07ca,y             // read petscii digits off of the scn
            adc #$00
            cmp #$3a                // cmp to ':' ($30=0..$39=9)
            bcc scsetdigit          // carry is set if > 9
            lda #$30                // '0'
scsetdigit:
            sta $07ca,y             // write digit to scn
            dey
            bne addsclp2            // for all 6 digits
            dex
            bne addsclp1            // points times
            rts


bltdata:    .byte $30,$10,$20,$30,$10,$20,$30,$10
// cannon
sprites:    .byte $00,$00,$00,$00,$00,$00,$00,$20
            .byte $00,$00,$ec,$00,$00,$ec,$00,$03,$ff,$00,$0f,$ff,$c0,$0f,$57,$c0
            .byte $0f,$ff,$c0,$3f,$57,$f0,$ff,$ff,$fc,$ff,$57,$fc,$ff,$ff,$fc,$ef
            .byte $57,$ec,$ef,$ff,$ec,$ec,$dc,$ec,$ec,$dc,$ec,$ec,$dc,$ec,$fc,$00
            .byte $fc,$00,$00,$00,$00,$00,$00,$00
// saucer
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$03,$55,$c0,$0d,$55,$70,$3f,$ff,$fc,$ea,$aa,$ab
            .byte $e2,$22,$2b,$3a,$aa,$ac,$0d,$55,$70,$03,$ff,$c0,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
// explosion
            .byte $80,$20,$08,$20,$a0,$20,$00,$a8
            .byte $a0,$22,$a2,$a0,$8a,$a8,$82,$a2,$c5,$2a,$28,$73,$08,$83,$be,$e0
            .byte $2e,$bc,$a2,$0b,$7f,$40,$82,$c9,$58,$0a,$ba,$4a,$23,$be,$e8,$00
            .byte $8d,$20,$0a,$26,$0a,$26,$a6,$a0,$00,$a8,$a8,$82,$8a,$28,$0a,$03
            .byte $80,$08,$20,$00,$00,$02,$20,$00
// cannon2
            .byte $00,$00,$00,$00,$00,$00,$00,$10
            .byte $00,$00,$76,$00,$00,$76,$00,$01,$ff,$80,$07,$ff,$e0,$07,$ab,$e0
            .byte $07,$ff,$e0,$1f,$ab,$f8,$7f,$ff,$fe,$7f,$ab,$fe,$7f,$ff,$fe,$77
            .byte $ab,$f6,$77,$ff,$f6,$76,$6e,$76
            .byte $76,$6e,$76,$76,$6e,$76,$7e,$00
            .byte $7e,$00,$00,$00,$00,$00,$00,$00
//otherdata
            .byte $00,$0d
sauclrs:    .byte $06,$04,$01,$07,$08,$0c,$0d
            .byte $0e
shstxt:     .byte $08,$1f           // disable shft+c=  blue

            .encoding "petscii_mixed"
            .text "Score:"
            .byte $9e               // Yellow
            .text "000000  "
            .byte $95               // Brown
            .text "High Score:"
            .byte $1c               // Red
            .text "000000"
            .byte $00
ttltxt:     .byte $12,$95           // Rvrs on  brown
            .text "Lives:3"
            .byte $1d,$1d,$1d,$1d,$1d,$1d,$1d,$1c  // crsr right x7 red
            .text "Astro-PANIC!"
            .byte $1d,$1d,$1d,$1d,$1d,$1d,$1f      // crsr right x6 blue
            .text "Level:0 "
            .byte $00
datscrdir8: .byte $01,$ff,$02,$fe,$03,$fd
datscrdir16:.byte $01,$00,$ff,$ff,$02,$00,$fe,$ff,$03,$00,$fd,$ff
datbltmv:   .byte $0d
datplyrx:   .byte $0d

.label      data = *                //tmp data (after assembled code)
.label      shflag       = $028d
.label      cinv         = $0314
.label      datplyrx8    = data
.label      datsaucxlo   = data+1
.label      datsaucxhi   = data+2
.label      datsaucy     = data+15
.label      cpyscrdir8   = data+22
.label      cpyscrdir16  = data+30
.label      datlives     = data+46
.label      datnopause   = data+47
.label      datsp2chrslt = data+48
.label      datlevel     = data+49
.label      scnkey       = $ff9f
.label      chrout       = $ffd2
.label      getin        = $ffe4
.label      plot         = $fff0
             
