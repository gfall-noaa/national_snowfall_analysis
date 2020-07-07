;***************************************************************************
;*
;*  CALTIME, julian, year, month, day, hour, minute, second
;*
;*  This procedure calls CAL_DAT, and rounds to nearest second
;*  This function is necessary due to the lack of precision in julian days
;*
;*  History:
;*  Anders Nilsson, VV, 04/20/04, Created
;*  Anders Nilsson, VV, 07/22/04, Handles arrays of julian dates
;*
;*************************************************************************

PRO CALTIME, julian, month, day, year, hour, minute, second

;** Call CALDAT **

    i_day = LONG ( julian )

    CALDAT, julian, month, day, year, hour, minute, second    

;** Round second **

    sub_second = second MOD 1
    test1 = where ( sub_second ge 0.5, COMPLEMENT=test0 )
    if ( test1[0] ne -1 ) then $
    begin

       second[test1] = FIX ( second[test1] ) + 1

       test2 = where ( second[test1] ge 60 )
       if ( test2[0] ne -1 ) then $
       begin

          second[test1[test2]] = 0   
          minute[test1[test2]] = minute[test1[test2]] + 1

          test3 = where ( minute[test1[test2]] ge 60 )
          if ( test3[0] ne -1 ) then $
          begin

             minute[test1[test2[test3]]] = 0
             hour[test1[test2[test3]]] = hour[test1[test2[test3]]] + 1

             test4 = where ( hour[test1[test2[test3]]] ge 24 )
             if ( test4[0] ne -1 ) then $
             begin

                hour[test1[test2[test3[test4]]]] = 0
                i_day[test1[test2[test3[test4]]]] = $
                   i_day[test1[test2[test3[test4]]]] + 1

;**             Recalculate day **

                CALDAT, i_day[test1[test2[test3[test4]]]], $
                        month2, day2, year2

                month[test1[test2[test3[test4]]]] = month2
                day[test1[test2[test3[test4]]]] = day2
                year[test1[test2[test3[test4]]]] = year2
 
                month2 = 0
                day2 = 0
                year2 = 0
                test4 = 0

             endif

             test3 = 0

          endif

          test2 = 0

       endif

       test1 = 0

    endif
    if ( test0[0] ne -1 ) then $
    begin
       second[test0] = FIX ( second[test0] )
       test0 = 0
    endif

    i_day = 0     

;** Return to calling routine **
    
END
