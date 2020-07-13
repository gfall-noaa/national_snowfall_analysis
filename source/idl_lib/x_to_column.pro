;*****************************************************************************
;*
;*  x_to_column
;*
;*  This function converts an x-axis_coordindate using header geometry into an
;*  column index.
;*
;*  col = x_to_column ( x, bench_x, bench_col, x_res )
;*
;*  Name        Description
;*  =========================================================================
;*  x         > X-axis coordinate to be converted
;*  bench_x   > Header information: Benchmark x-axis coordinate
;*  bench_col > Header information: Benchmark x-axis column
;*  x_res     > Header information: x-axis pixel resolution
;*
;*  History:
;*  Anders Nilsson, VV, 09/02/03, Copied into separate file
;*  Anders Nilsson, VV, 03/09/04, Modified to allow multiple longitudes
;*  Anders Nilsson, VV, 08/10/04, Optimized handling of arrays
;*
;***************************************************************************

    FUNCTION x_to_column, longitude, bench_x, bench_col, x_res

    number = n_elements ( longitude )
    if ( number gt 1 ) then column = LONARR ( number ) $
    else column = 0l

    test1 = where ( longitude lt bench_x, COMPLEMENT=test2 )
    if ( test1[0] ne -1 ) then $
    begin
       scratch = DOUBLE ( bench_x - longitude[test1] )
       quotient = DOUBLE ( scratch / x_res )
       scratch = TEMPORARY ( scratch ) MOD x_res
       test3 = where ( scratch ne ( x_res / 2.0d ) ) 
       if ( test3[0] ne -1 ) then $
       begin
          quotient[test3] = quotient[test3] + 0.5d
          test3 = 0
       endif

       column[test1] = LONG ( bench_col ) - LONG ( quotient )
                              
       test1 = 0
    endif

    if ( test2[0] ne -1 ) then $
    begin
       column[test2] = LONG ( bench_col ) + $
                       LONG ( DOUBLE ( longitude[test2] - bench_x ) / $
                              x_res + 0.5d ) 
       test2 = 0
    endif

    RETURN, column

    END


