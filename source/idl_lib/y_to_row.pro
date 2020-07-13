;*****************************************************************************
;*
;*  y_to_row
;*
;*  This function converts a y-axis coordinate using header geometry into an
;*  row index.
;*
;*  row = y_to_row ( y, bench_y, bench_row, y_res )
;*
;*  Name        Description
;*  =========================================================================
;*  y         > Y-axis coordinate to be converted
;*  bench_y   > Header information: Benchmark y-axis coordinate
;*  bench_row > Header information: Benchmark y-axis row
;*  y_res     > Header information: y-axis pixel resolution
;*
;*  History:
;*  Anders Nilsson, VV, 09/02/03, Copied into separate file
;*  Anders Nilsson, VV, 03/09/04, Modified to allow multiple latitudes
;*  Anders Nilsson, VV, 08/10/04, Optimized handling of arrays
;*
;***************************************************************************

    FUNCTION y_to_row, latitude, bench_y, bench_row, y_res

    number = n_elements ( latitude )
    if number gt 1 then row = LONARR ( number ) $
    else row = 0l

    test1 = where ( latitude gt bench_y, COMPLEMENT=test2 )
    if ( test1[0] ne -1 ) then $
    begin
       scratch = DOUBLE ( latitude[test1] - bench_y )
       quotient = DOUBLE ( scratch / y_res )
       scratch = TEMPORARY ( scratch ) MOD y_res
       test3 = where ( scratch ne ( y_res / 2.0d ) )
       if ( test3[0] ne -1 ) then $
       begin
          quotient[test3] = quotient[test3] + 0.5d
          test3 = 0
       endif

       row[test1] = LONG ( bench_row ) - LONG ( quotient ) ;
 
       test1 = 0
    endif 

    if ( test2[0] ne -1 ) then $
    begin
       row[test2] = LONG ( bench_row ) + $
                    LONG ( DOUBLE ( bench_y - latitude[test2] ) / $
                           y_res + 0.5d )
       test2 = 0
    endif

    RETURN, row

    END

