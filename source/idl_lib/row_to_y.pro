;*****************************************************************************
;*
;*  row_to_y
;*
;*  This function converts a row number using header geometry into an
;*  y-axis coordinate.
;*
;*  y = row_to_y ( row, bench_y, bench_row, y_res )
;*
;*  Name        Description
;*  =========================================================================
;*  row       > Row in header to be converted
;*  bench_y   > Header information: Benchmark y-axis coordinate
;*  bench_row > Header information: Benchmark y-axis row
;*  y_res     > Header information: y-axis pixel resolution
;*
;*  History:
;*  Anders Nilsson, VV, 09/02/03, Copied into separate file
;*
;***************************************************************************

    FUNCTION row_to_y, row, bench_y, bench_row, y_res

    RETURN, DOUBLE ( bench_y ) - $
            ( DOUBLE ( row ) - DOUBLE ( bench_row ) ) * DOUBLE ( y_res )

    END

