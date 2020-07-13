;*****************************************************************************
;*
;*  column_to_x
;*
;*  This function converts a column number using header geometry into an
;*  x-axis coordinate.
;*
;*  x = column_to_x ( column, bench_x, bench_col, x_res )
;*
;*  Name        Description
;*  =========================================================================
;*  column    > Column in header to be converted
;*  bench_x   > Header information: Benchmark x-axis coordinate
;*  bench_col > Header information: Benchmark x-axis column
;*  x_res     > Header information: x-axis pixel resolution
;*
;*  History:
;*  Anders Nilsson, VV, 09/02/03, Copied into separate file
;*
;***************************************************************************

    FUNCTION column_to_x, column, bench_x, bench_col, x_res

    RETURN, DOUBLE ( bench_x ) + $
            ( DOUBLE ( column ) - DOUBLE ( bench_col ) ) * DOUBLE ( x_res )

    END

