;*****************************************************************************
;*
;*  get_shape_bounds
;*
;*  This subroutine is used to get the bounds of the file or object within
;*  a shapefile. It assumes that the file containing the header (.shp) is 
;*  open
;*
;*  get_shape_bounds, unit, file_loc, type, x_min, y_min, x_max, y_max
;*
;*  Name        Description
;*  =========================================================================
;*  unit      > File descriptor of open .shp file
;*  file_loc  > Numerical location index within the file
;*              Assumes that 32 is top header
;*  type      > 1: Point
;*            > default: Polygon of some sort
;*  x_min     < Minimum x-axis coordinate limit
;*  y_min     < Minimum y_axis coordinate limit
;*  x_max     < Maximum x-axis coordinate limit
;*  y_max     < Maximum y-axis coordinate limit
;*
;*  History:
;*  Anders Nilsson, VV, 09/02/03, Copied into separate file
;*
;***************************************************************************

PRO get_shape_bounds, unit, file_loc, type, x_min, y_min, x_max, y_max


;** Assumes that file containing header (.shp) is open with unit "unit". **

;** Initialize variables **

    type = 0L
    x_min = 180.0D
    y_min = 90.0D
    x_max = -180.0D
    y_max = -90.0D

    POINT_LUN, unit, file_loc

;   Read maximums if not point type, exceptions being top header at 32

    READU, unit, type, x_min, y_min

    if ( type ne 1 or file_loc eq 32L ) then $
    begin
       READU, unit, x_max, y_max
    endif $
    else $
    begin
       x_max = x_min
       y_max = y_min
    endelse

end
            
