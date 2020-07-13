   10 ! BASIC Month 5: Crisps Tunes
   20 ! http://reddit.com/r/RetroBattlestations
   30 ! written by FozzTexx
   40 ! ported to Waterloo microBasic 1.0 for the Commodore SuperPET
   50 ! by /u/pateandcognac
   60 !
   70 !
   80 !
   90 !                                setup
  100 call varinit
  110 on eof ignore
  120 poke 59467,16 : poke 59466,15 : poke 59464,0 ! sound on
  130 
  140 
  150 loop !                                     main loop
  160 
  170 
  180   input "(c)onvert (p)playback (q)uit:",choice$
  190   if choice$="c" or choice$="C"
  200     call convert
  210     call playback
  220   elseif choice$="p" or choice$="P"
  230     call playback
  240   elseif choice$="q" or choice$="Q"
  250     call endprg
  260     stop
  270   else
  280     print "enter c, p, or q"
  290   endif
  300 
  310 
  320 endloop !                                  end main loop
  330 
  340 !
  350 !
  360 !
  370 !
  380 !  ------------------------      procedures    -------------------------
  390 !
  400 !
  410 !
  420 !
  430 
  440 proc playback !                            playback
  450   if file$=""
  460     input "enter filename:",file$
  470   else
  480     print "playing ";file$+".XYZ"
  490   endif
  500   open #2, file$+".XYZ", input
  510   loop
  520     input #2,human$
  530     input #2,octval
  540     input #2,freqval
  550     input #2,dur
  560     if io_status<>0 then quit ! leave loop if eof or error
  570     print human$
  580     call playnote
  590   endloop
  600   file$=""
  610   close #2
  620 endproc
  630 
  640 
  650 proc convert !                             convert
  660   on eof ignore
  670   print ".ABC and .XYZ will be appended to filename"
  680   input "Enter filename:",file$
  690   open #2,file$+".ABC",input
  700   open #3,file$+".XYZ",output
  710   get #2, nextnote$
  720   print "generating ";file$+".XYZ";
  730   loop
  740     call parsenote
  750     print #3,human$
  760     print #3,octval
  770     print #3,freqval
  780     print #3,dur
  790     print ".";
  800     if nextnote$="]" then quit
  810   endloop
  820   close #2
  830   close #3
  840 print
  850 endproc ! convert
  860 
  870 
  880 proc endprg !                               endprg
  890   print "bye!"
  900   poke 59467,0 : poke 59466,0 : poke 59464,0
  910 endproc
  920 
  930 
  940 proc varinit !                              varinit
  950   dim m(7)
  960   for i=1 to 7
  970     read m(i)
  980   next i
  990   v=14 ! 14,26,38
 1000   shfl = 0
 1010   rhythm = 0
 1020   omax = 3
 1030   dim a(12*omax)
 1040   dim pf(48)
 1050   dim po(48)
 1060   for i=1 to 48
 1070    read pf(i)
 1080    read po(i)
 1090   next i
 1100 endproc
 1110 
 1120 
 1130 !                                                proc parsenote
 1140 ! reads file$ until next note detected
 1150 ! returns nextnote$ - the next note to be fed into the proc
 1160 ! human$ - human readable note
 1170 ! octval, freqval - for poking, dur - duration
 1180 
 1190 proc parsenote
 1200   call resetnote
 1210   n$=nextnote$
 1220   gotnote=0
 1230   loop ! until gotnote
 1240     if n$=" "
 1250       get #2,n$
 1260       call resetnote
 1270     elseif n$="!"
 1280       for ad=1 to 12*omax
 1290         a(ad)=0
 1300       next ad
 1310       call resetnote
 1320       get #2, n$
 1330     elseif n$="^"
 1340       accd=1
 1350       shfl=shfl+1
 1360       get #2, n$
 1370     elseif n$="-"
 1380       accd=1
 1390       shfl=shfl-1
 1400       get #2, n$
 1410     elseif n$="="
 1420       accd=1
 1430       shfl=0
 1440       get #2, n$
 1450     else
 1460       if n$>="a" and n$<="g"
 1470         n$=chr$(ord(n$)-32)
 1480         octave=octave+1
 1490       endif
 1500       if n$>="A" and n$<="G"
 1510          c=ord(n$)
 1520          c=c-64
 1530          nt=m(c)
 1540          gotnote=1
 1550       endif
 1560     endif
 1570   until gotnote
 1580 call parsemods
 1590 call translatenote
 1600 endproc  !  parsenote
 1610 
 1620 
 1630 
 1640 proc parsemods !                                   parsemods
 1650 nextnote$=""
 1660      loop ! until nextnote$<>""
 1670        get #2, mod$
 1680          if mod$=","
 1690          octave=octave-1
 1700        elseif mod$="'"
 1710          octave=octave+1
 1720        elseif mod$>="0" and mod$<="9"
 1730          c=ord(mod$)
 1740          mult=mult*10+c-48
 1750        elseif mod$="/"
 1760          div=1
 1770        elseif mod$="<"
 1780          rhythm=1
 1790          rmult=.5
 1800        elseif mods$=">"
 1810          rhythm=1
 1820          rmult=1.5
 1830        elseif mod$="]"
 1840          nextnote$="]"
 1850        else
 1860          nextnote$=mod$
 1870        endif
 1880      until nextnote$<>""
 1890 
 1900 endproc ! parsemods
 1910 
 1920 
 1930 ! returns human$, octval, freqval, dur              translatenote
 1940 proc translatenote
 1950   nt = nt + (octave - 4) * 12
 1960   if accd
 1970     a(nt + 13) = shfl
 1980     shfl = 0
 1990   endif
 2000   shfl = a(nt + 13)
 2010   nt = nt + shfl
 2020   if div and mult = 0 then mult = 2
 2030   if div then mult = 1/mult
 2040   if not mult then mult = 1
 2050   if rhythm then mult = mult * rmult
 2060   dur = dur * mult
 2070   f$=""
 2080   if shfl<>0 then f$="#"
 2090   if shfl<0 then f$="$"
 2100   human$=n$+f$+value$(octave)+" "+value$(dur)
 2110   octval=po(nt+v)
 2120   freqval=pf(nt+v)
 2130 endproc ! translatenote
 2140 
 2150 ! expects octval, freqval, dur                  playnote
 2160 proc playnote
 2170   poke 59466,octval:poke 59464,freqval
 2180   for delay=0 to dur*2000
 2190   next delay
 2200   poke 59464,0
 2210 endproc ! playnote
 2220 
 2230 
 2240 proc resetrhythm !                               resetrhythm
 2250   if rhythm then rhythm = rhythm + 1
 2260   rmult = 2 - rmult
 2270   if rhythm = 3 then rhythm = 0
 2280 endproc
 2290 
 2300 
 2310 proc resetnote !                                  resetnote
 2320   octave=4
 2330   dur=.05
 2340   div=0
 2350   mult=0
 2360   accd=0
 2370 endproc
 2380 
 2390 !                                                  data
 2400 !
 2410 ! assign to m()
 2420 data 9,11,0,2,4,5,7
 2430 !
 2440 ! Note to poke data
 2450 !      f,  o    B o0 to A# o0
 2460 data 251, 15
 2470 data 238, 15
 2480 data 224, 15
 2490 data 210, 15
 2500 data 199, 15
 2510 data 188, 15
 2520 data 177, 15
 2530 data 168, 15
 2540 data 158, 15
 2550 data 149, 15
 2560 data 140, 15
 2570 data 133, 15
 2580 !      f,  o    B o1 to A# o1
 2590 data 125, 15
 2600 data 118, 15
 2610 data 110, 15
 2620 data 104, 15
 2630 data  99, 15
 2640 data  93, 15
 2650 data  88, 15
 2660 data  83, 15
 2670 data  78, 15
 2680 data  74, 15
 2690 data  69, 15
 2700 data  65, 15
 2710 !       f,  o    B o1 to A# o1
 2720 !data 251, 51
 2730 !data 238, 51
 2740 !data 224, 51
 2750 !data 210, 51
 2760 !data 199, 51
 2770 !data 188, 51
 2780 !data 177, 51
 2790 !data 168, 51
 2800 !data 158, 51
 2810 !data 149, 51
 2820 !data 140, 51
 2830 !data 133, 51
 2840 !      f,  o    B o2 to A# o2
 2850 data 125, 51
 2860 data 118, 51
 2870 data 110, 51
 2880 data 104, 51
 2890 data  99, 51
 2900 data  93, 51
 2910 data  88, 51
 2920 data  83, 51
 2930 data  78, 51
 2940 data  74, 51
 2950 data  69, 51
 2960 data  65, 51
 2970 !      f,  o    B o2 to A# o2
 2980 !data 251, 85
 2990 !data 238, 85
 3000 !data 224, 85
 3010 !data 210, 85
 3020 !data 199, 85
 3030 !data 188, 85
 3040 !data 177, 85
 3050 !data 168, 85
 3060 !data 158, 85
 3070 !data 149, 85
 3080 !data 140, 85
 3090 !data 133, 85
 3100 !      f,  o    B o3 to A# o3
 3110 data 125, 85
 3120 data 118, 85
 3130 data 110, 85
 3140 data 104, 85
 3150 data  99, 85
 3160 data  93, 85
 3170 data  88, 85
 3180 data  83, 85
 3190 data  78, 85
 3200 data  74, 85
 3210 data  69, 85
 3220 data  65, 85
 3230 
 3240 !                Notes to pokes
 3250 ! use poke 59466,octave
 3260 ! use poke 59464,freq
 3270 ! currently using 85 o3, 51 o2, 15 o1, 15 o0
 3280 !         octave=15     octave=51     octave=85
 3290 !  Note  Oct.0 Oct.1 ! Oct.1 Oct.2 ! Oct.2 Oct.3
 3300 !  Freq  ------------+-------------+--------------
 3310 !   B     251   125  !  251   125  !  251   125
 3320 !   C     238   118  !  238   118  !  238   118
 3330 !   C#    224   110  !  224   110  !  224   110
 3340 !   D     210   104  !  210   104  !  210   104
 3350 !   D#    199    99  !  199    99  !  199    99
 3360 !   E     188    93  !  188    93  !  188    93
 3370 !   F     177    88  !  177    88  !  177    88
 3380 !   F#    168    83  !  168    83  !  168    83
 3390 !   G     158    78  !  158    78  !  158    78
 3400 !   G#    149    74  !  149    74  !  149    74
 3410 !   A     140    69  !  140    69  !  140    69
 3420 !   A#    133    65  !  133    65  !  133    65
 3430 
 3440 end ! :)
