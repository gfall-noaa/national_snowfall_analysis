
;    Sample header information

;    benchmark_x[0] = -124.99624999999d
;    benchmark_y[0] = 49.39250068664d
;    benchmark_col[0] = 0
;    benchmark_row[0] = 0
;    x_resolution[0] = 0.008333333333333d
;    y_resolution[0] = 0.008333333333333d
;    x_offset[0] = 0.003750000000000d
;    y_offset[0] = 0.003750000000000d

;***************************************************************
;*
;* line_equation
;*
;* This function computes the equation of line passing through 2 points
;*
;* Boolean line_equation ( option, x1, y1, x2, y2, slope, intercept )
;*
;*  ARGUMENTS:
;*
;*  Name          Description
;*  ================================================
;*
;*  option     >  0 : Compute equation of line
;*                1 : Compute x1 provided y1
;*                2 : Compute y1 provided x1
;*  x1         >  X-axis coordinate of the first point
;*  y1         >  Y-axis coordinate of the first point
;*  x2         >  X-axis coordinate of the second point
;*  y2         >  Y-axis coordinate of the second point
;*  slope      <  The line's slope
;*  intercept  <  The lines y-axis intercept
;*  NOTES:
;*
;*  1) Returns False for option 0 if vertical line
;*  2) Returns False for option 1 if horizontal line
;*
;*  History:
;*  Anders Nilsson, VV, 08/27/03, Adapted from GISRS function
;*
;*****************************************************

FUNCTION line_equation,  option, x1, y1, x2, y2, slope, intercept

    if ( option eq 0 ) then $
    begin
;**    Vertical line? Returns 0 **
       if ( x1 eq x2 ) then return, 0

       slope = DOUBLE ( y1 - y2 ) / DOUBLE ( x1 - x2 )
       intercept = y1 - slope * x1

    endif $
    else if ( option eq 1 ) then $
    begin
;**    Horizontal line? Return 0 **
       if ( slope eq 0.0 ) then return, 0

       x1 = ( y1 - intercept ) / slope ;

    endif $
    else $
    begin

       y1 = slope * x1 + intercept

    endelse

;** Return True **

    return, 1

END

;************************************************************************
;*
;*  polygon_intervals_01
;*
;*  Determine if horizontal segment inside polygon
;*
;************************************************************************

PRO polygon_intervals_01, x_coords, y_coords, number_vertices, x, y, in

;** Initialize variables **

    first       = 0l 
    first_x     = 0l
    loc1        = 0l 
    loc2        = 0l 
    loc3        = 1l 
    loc4        = 1l 
    loc5        = 0l 
    loc6        = 1l 
    loc7        = 0l 
    num_crosses = 0l
    num_segments  = number_vertices - 1l 
    type        = 0l 
    x1          = 0l 
    max_crosses = 256l
   
    crosses = DBLARR ( 2 * max_crosses )
    weights = BYTARR ( max_crosses )

;** Loop through polygon segments **

    for loop1 = 1l, num_segments, 1l do $
    begin

;**    Segment completely intersects x from the west or east **

       if ( ( ( x_coords[loc1] lt x ) and ( x_coords[loc3] gt x ) ) or $
            ( ( x_coords[loc3] lt x ) and ( x_coords[loc1] gt x ) ) ) then $
       begin

          num_crosses = num_crosses + 1

          if ( num_crosses gt max_crosses ) then $
          begin
             max_crosses = max_crosses + 256
             crosses = [crosses, DBLARR ( 2*256 ) ]
             weights = [weights, BYTARR ( 256 ) ]
          endif

          x2 = x_coords[loc1]
          y1 = y_coords[loc2]

          junk = line_equation ( 0, x2, y1, x_coords[loc3], y_coords[loc4], $
                                 slope, intercept )
          junk = line_equation ( 2, x, y1, x_coords[loc3], y_coords[loc4], $
                                 slope, intercept )
          crosses[loc5] = y1
          crosses[loc6] = crosses[loc5] ;
          weights[loc7] = 1l

          loc5 = loc5 + 2l
          loc6 = loc6 + 2l
          loc7 = loc7 + 1l 

       endif $
       else if ( x_coords[loc1] eq x ) then $
       begin

;**    The 1st segment vertex is coincident with x **

          if ( x_coords[loc3] eq x ) then $
          begin
             if ( loop1 eq num_segments ) then $
             begin
                if ( y_coords[loc2] gt y_coords[loc4] ) then $
                begin
                   crosses[0] = crosses[0] > y_coords[loc2]
                   crosses[1] = crosses[1] < y_coords[loc4]
                endif $
                else $
                begin
                   crosses[0] = crosses[0] > y_coords[loc4]
                   crosses[1] = crosses[1] < y_coords[loc2]
                endelse

                if ( x1 lt x ) then $
                begin
                   if ( first_x lt x ) then $
                   begin
                      weights[0] = 2l
                   endif $
                   else $
                   begin
                      weights[0] = 1l
                   endelse
                endif $
                else $
                begin
                   if ( first_x gt x ) then $
                   begin
                      weights[0] = 2l
                   endif $
                   else $
                   begin
                      weights[0] = 1l
                   endelse
                endelse
             endif $
             else $
             begin
                if ( type eq 0 ) then $
                begin
                   if ( y_coords[loc2] gt y_coords[loc4] ) then $
                   begin
                      crosses[loc5] = y_coords[loc2] 
                      crosses[loc6] = y_coords[loc4]
                   endif $
                   else $
                   begin
                      crosses[loc5] = y_coords[loc4] 
                      crosses[loc6] = y_coords[loc2]
                   endelse
                endif $
                else $
                begin
                   if ( y_coords[loc2] gt y_coords[loc4] ) then $
                   begin
                      crosses[loc5] = crosses[loc5] > y_coords[loc2]
                      crosses[loc6] = crosses[loc6] < y_coords[loc4]
                   endif $
                   else $
                   begin
                      crosses[loc5] = crosses[loc5] > y_coords[loc4]
                      crosses[loc6] = crosses[loc6] < y_coords[loc2]
                   endelse
                endelse

                type = 2
             endelse
          endif $
          else $
          begin
             if ( loop1 eq 1 ) then $
             begin
                crosses[loc5] = y_coords[loc2]
                crosses[loc6] = y_coords[loc2]
             endif
             if ( not first ) then $
             begin
                first_x = x_coords[loc3]
                first = 1l
             endif

             num_crosses = num_crosses + 1l

             if ( x_coords[loc3] lt x ) then $
             begin
                if ( x1 lt x ) then $
                begin
                   weights[loc7] = 2l
                endif $
                else $
                begin
                   weights[loc7] = 1l
                endelse
             endif $
             else $
             begin
                if ( x1 gt x ) then $
                begin
                   weights[loc7] = 2l
                endif $
                else $
                begin
                   weights[loc7] = 1l
                endelse 
             endelse

             loc5 = loc5 + 2l
             loc6 = loc6 + 2l
             loc7 = loc7 + 1l
             type = 0l

          endelse
       endif $
       else if ( x_coords[loc3] eq x ) then $
       begin
          if ( loop1 eq num_segments ) then $
          begin
            if ( x_coords[loc1] lt x ) then $
            begin
               if ( first_x lt x ) then $
               begin
                  weights[0] = 2l
               endif $
               else $
               begin
                  weights[0] = 1l
               endelse
            endif $
            else $
            begin
               if ( first_x gt x ) then $
               begin
                  weights[0] = 2l
               endif $
               else $
               begin
                  weights[0] = 1l
               endelse
            endelse
          endif $
          else $
          begin

             if ( num_crosses eq max_crosses ) then $
             begin
                max_crosses = max_crosses + 256l
                crosses = [crosses, DBLARR ( 2*256 ) ]
                weights = [weights, BYTARR ( 256 ) ]
             endif

             crosses[loc5] = y_coords[loc4]
             crosses[loc6] = y_coords[loc4]
             type = 1

             x1 = x_coords[loc1]
 
          endelse
       endif

       loc1 = loc1 + 1l
       loc2 = loc2 + 1l
       loc3 = loc3 + 1l
       loc4 = loc4 + 1l

    endfor

;** Initialize variables **

    loc1 = 0l
    loc2 = 1l
    loc3 = 0l

;** Loop through intersections **

    for loop1 = 1l, num_crosses, 1l do $
    begin
 
       loc4 = loop1 + 1l
       loc5 = loc1
       loc6 = loc2
       loc7 = loc3

       for loop2 = loc4, num_crosses, 1 do $
       begin
 
          loc5 = loc5 + 2l
          loc6 = loc6 + 2l
          loc7 = loc7 + 1l

          if ( crosses[loc5] le crosses[loc1] ) then continue

          temp1 = crosses[loc1]
          temp2 = crosses[loc2]
          temp3 = weights[loc3]
          crosses[loc1] = crosses[loc5]
          crosses[loc2] = crosses[loc6]
          weights[loc3] = weights[loc7]
          crosses[loc5] = temp1
          crosses[loc6] = temp2
          weights[loc7] = temp3

       endfor

       loc1 = loc1 + 2l
       loc2 = loc2 + 2l
       loc3 = loc3 + 1l

    endfor

;** Initialize variables **

    count = 0l
    loc1 = 0l
    loc2 = 0l

;** Loop through intersections **

    for loop1 = 1l, num_crosses, 1l do $
    begin
       count = count + weights[loc1]

       if ( crosses[loc2] eq y ) then break

       loc1 = loc1 + 1l
       loc2 = loc2 + 2l

    endfor

    if ( ( count MOD 2l ) eq 0l ) then $
    begin
       in = 0l
    endif $
    else $
    begin
       in = 1l
    endelse

;** Free memory **

    weights = 0l
    crosses = 0l

;** Return to calling routine

    RETURN

END

;************************************************************************
;*
;*  polygon_intervals
;*
;*  This procedure computes grid cell centers polygon membership by row as
;*  column intervals
;*
;*  polygon_intervals,  option, x_coords, y_coords, row, $
;*                      number_intervals, intervals, $
;*                      benchmark_x, benchmark_y, benchmark_col, $
;*                      benchmark_row, x_resolution, y_resolution, $
;*                      x_offset, y_offset 
;*
;*  ARGUMENTS:
;*
;*  Name    Type                     Description
;*  =========================================================================
;*
;*  option           >  0 : Terminate
;*                      1 : Initialize
;*                      2 : Return polygon membership
;*  x_coords         >  Array of x coordinates of polygon
;*  y_coords         >  Array of y coordinates of polygon
;*  row              >  Polygon membership grid row
;*  number_intervals <  Number of grid row membership intervals
;*  intervals        <  Grid row membership intervals start/stop
;*                      columns array pointer
;*  benchmark_x      >  Header info: benchmark x-axis coordinate
;*  benchmark_y      >  Header info: benchmark y-axis coordinate
;*  benchmark_col    >  Header info: benchmark x-axis column
;*  benchmark_row    >  Header info: benchmark y-axis row
;*  x_resolution     >  Header info: Raster x-axis resolution
;*  y_resolution     >  Header info: Raster y-axis resolution
;*
;*  History:
;*  Anders Nilsson, VV, 08/27/03, Adapted from GISRS function
;*  Anders Nilsson, VV, 11/03/03, Fixed bugs
;*  Anders Nilsson, UCAR 10/06/06 More explicit typing
;*
;********************************************************************

PRO polygon_intervals, option, x_coords, y_coords, row, $
                       number_intervals, intervals, $
                       benchmark_x, benchmark_y, benchmark_col, $
                       benchmark_row, x_resolution, y_resolution, $
                       x_offset, y_offset
                       
    if ( option eq 0 ) then $
    begin

;**    Terminate **

       intervals = 0
       number_intervals = 0

    endif $
    else if ( option eq 1 ) then $
    begin

;**    Shift cell center coincident vertices **

       columns = x_to_column ( x_coords, benchmark_x, benchmark_col, $
                               x_resolution )
       longitudes = column_to_x ( columns, benchmark_x, benchmark_col, $
                                  x_resolution )
       columns = 0 

       rows = y_to_row ( y_coords, benchmark_y, benchmark_row, $
                         y_resolution )
       latitudes = row_to_y ( rows, benchmark_y, benchmark_row, $
                              y_resolution )
       rows = 0

       shifts = where ( longitudes eq x_coords and latitudes eq y_coords )
       if ( shifts[0] ne -1 ) then $
       begin
          x_coords[shifts] = TEMPORARY ( x_coords[shifts] ) + 0.000001
          y_coords[shifts] = TEMPORARY ( y_coords[shifts] ) + 0.000001
       endif

       longitudes = 0
       latitudes = 0
       shifts = 0

    endif $
    else $
    begin

       number_intervals = 0l
       intervals = 0l

;**    Determine membership **

       number_vertices = n_elements ( x_coords ) < n_elements ( y_coords )

;**    Check for rectangle **

       if ( ( number_vertices eq 5 ) and $
            ( ( ( y_coords[0] eq y_coords[1] ) and $
                ( x_coords[1] eq x_coords[2] ) and $
                ( y_coords[2] eq y_coords[3] ) and $
                ( x_coords[3] eq x_coords[0] ) ) or $
              ( ( x_coords[0] eq x_coords[1] ) and $
                ( y_coords[1] eq y_coords[2] ) and $
                ( x_coords[2] eq x_coords[3] ) and $
                ( y_coords[3] eq y_coords[0] ) ) ) ) then $

       begin

;**       Polygon y-axis limits in file coordinates **

          min_x = MIN ( x_coords, MAX=max_x ) 
          min_y = MIN ( y_coords, MAX=max_y )

          min_row = y_to_row ( min_y, benchmark_y, benchmark_row, $
                               y_resolution )
          max_row = y_to_row ( max_y, benchmark_y, benchmark_row, $
                               y_resolution )

;**       Return if row outside of polygon y-axis limits in file **
;**       coordinates                                            **

          if ( row lt min_row ) then RETURN
          if ( row gt max_row ) then RETURN

;**       Compute latitude of center line of row **

          y1 = row_to_y ( row, benchmark_y, benchmark_row, y_resolution ) ;

;**       Return if row center line latitude outside of polygon **

          if ( y1 lt min_y ) then RETURN
          if ( y1 gt max_y ) then RETURN

;**       Rectangle limits **

          rectangle_min = x_to_column ( min_x, benchmark_x, benchmark_col, $
                                        x_resolution )
          rectangle_max = x_to_column ( max_x, benchmark_x, benchmark_col, $
                                        x_resolution )

          x3 = column_to_x ( rectangle_min, benchmark_x, benchmark_col, $
                             x_resolution )
          if ( x3 lt min_x ) then rectangle_min = rectangle_min + 1

          x3 = column_to_x ( rectangle_max, benchmark_x, benchmark_col, $
                             x_resolution )
          if ( x3 ge max_x ) then rectangle_max = rectangle_max - 1

          intervals = [rectangle_min, rectangle_max]
          number_intervals = 1

          RETURN

       endif

;**    Compute latitude of center line of row **

       y1 = row_to_y ( row, benchmark_y, benchmark_row, y_resolution ) ;       

;**    Number of polygon segments **

       number_segments = number_vertices - 1l

;**    Initialize variables **

       max_crosses = 256l
       max_ints = 256l

       crosses = DBLARR ( 2 * max_crosses )
       weights = BYTARR ( max_crosses )
       intervals = LONARR ( 2 * max_crosses )

       first = 0l
       first_y = 0l
       loc1 = 0l 
       loc2 = 0l
       loc3 = 1l
       loc4 = 1l
       loc5 = 0l
       loc6 = 1l
       loc7 = 0l
       num_crosses = 0l
       type = 0l
       y2 = 0l

;**    Loop through polygon segments **

       for loop1 = 1l, number_segments, 1. do $
       begin

          if ( ( ( y_coords[loc2] lt y1 ) and $
                 ( y_coords[loc4] gt y1 ) ) or $
               ( ( y_coords[loc4] lt y1 ) and $
                 ( y_coords[loc2] gt y1 ) ) ) then $
          begin

;**          Segment completely intersects row center line from the south **

             num_crosses = num_crosses + 1.

             if ( num_crosses gt max_crosses ) then $
             begin
                max_crosses = max_crosses + 256l
                crosses = [crosses, DBLARR ( 2*256 ) ]
                weights = [weights, BYTARR ( 256 ) ]
             endif


             x1 = x_coords[loc1]
             y3 = y_coords[loc2]
             if ( line_equation ( 0, x1, y3, x_coords[loc3], y_coords[loc4], $
                                  slope, intercept ) ) then $
             begin

                junk = line_equation ( 1, x1, y1, x_coords[loc3], $
                                       y_coords[loc4], slope, intercept )
                crosses[loc5] = x1

             endif $
             else $
             begin
                crosses[loc5] = x_coords[loc1]
             endelse

             crosses[loc6] = crosses[loc5]
             weights[loc7] = 1l
             loc5 = loc5 + 2l
             loc6 = loc6 + 2l
             loc7 = loc7 + 1l

          endif $
          else if ( y_coords[loc2] eq y1 ) then $
          begin

;**          The 1st segment vertex is coincident with the row center line

             if ( y_coords[loc4] eq y1 ) then $
             begin

;**             The entire segment is coincident with the row center line **

                if ( loop1 eq number_segments ) then $
                begin
                   if ( x_coords[loc1] lt x_coords[loc3] ) then $
                   begin
                      crosses[0] = crosses[0] < x_coords[loc1] 
                      crosses[1] = crosses[1] > x_coords[loc3]
                   endif $
                   else $
                   begin
                      crosses[0] = crosses[0] < x_coords[loc3]
                      crosses[1] = crosses[1] > x_coords[loc1]
                   endelse

                   if ( y2 lt y1 ) then $
                   begin
                      if ( first_y lt y1 ) then $
                      begin
                         weights[0] = 2l
                      endif $
                      else $
                      begin
                         weights[0] = 1l
                      endelse
                   endif $
                   else $
                   begin
                      if ( first_y gt y1 ) then $
                      begin
                         weights[0] = 2l
                      endif $
                      else $
                      begin
                         weights[0] = 1l
                      endelse
                   endelse
                endif $
                else $
                begin
                
                   if ( type eq 0 ) then $
                   begin
                      if ( x_coords[loc1] lt x_coords[loc3] ) then $
                      begin
                         crosses[loc5] = x_coords[loc1]
                         crosses[loc6] = x_coords[loc3]
                      endif $
                      else $
                      begin
                         crosses[loc5] = x_coords[loc3]
                         crosses[loc6] = x_coords[loc1]
                      endelse
                   endif $
                   else $
                   begin
                      if ( x_coords[loc1] lt x_coords[loc3] ) then $
                      begin
                         crosses[loc5] = crosses[loc5] < x_coords[loc1]
                         crosses[loc6] = crosses[loc6] > x_coords[loc3]
                      endif $
                      else $
                      begin
                         crosses[loc5] = crosses[loc5] < x_coords[loc3]
                         crosses[loc6] = crosses[loc6] > x_coords[loc1]
                      endelse
                   endelse

                   type = 2

                endelse

             endif $
             else $
             begin
                if ( loop1 eq 1 ) then $
                begin
                   crosses[loc5] = x_coords[loc1]
                   crosses[loc6] = x_coords[loc1]
                endif
           
                if ( not first ) then $
                begin
                   first_y = y_coords[loc4]
                   first = 1l
                endif

                num_crosses = num_crosses + 1
                if ( y_coords[loc4] lt y1 ) then $
                begin
                   if ( y2 lt y1 ) then $
                   begin
                      weights[loc7] = 2l
                   endif $
                   else $
                   begin
                      weights[loc7] = 1l
                   endelse
                endif $
                else $
                begin $
                   if ( y2 gt y1 ) then $
                   begin
                      weights[loc7] = 2l
                   endif $
                   else $
                   begin
                      weights[loc7] = 1l
                   endelse
                endelse 

                loc5 = loc5 + 2l
                loc6 = loc6 + 2l
                loc7 = loc7 + 1l
                type = 0l
                 
             endelse
          endif $
          else if ( y_coords[loc4] eq y1 ) then $
          begin
             if ( loop1 eq number_segments ) then $
             begin
                if ( y_coords[loc2] lt y1 ) then $
                begin
                   if ( first_y lt y1 ) then $
                   begin
                      weights[0] = 2l
                   endif $
                   else $
                   begin 
                      weights[0] = 1l
                   endelse
                endif $
                else $
                begin
                   if ( first_y gt y1 ) then $
                   begin
                      weights[0] = 2l
                   endif $
                   else $
                   begin 
                      weights[0] = 1l
                   endelse
                endelse
             endif $
             else $
             begin

                if ( num_crosses eq max_crosses ) then $
                begin
                   max_crosses = max_crosses + 256l
                   crosses = [crosses, DBLARR ( 2*256 ) ]
                   weights = [weights, BYTARR ( 256 ) ]
                endif

                crosses[loc5] = x_coords[loc3]
                crosses[loc6] = x_coords[loc3]

                type = 1l
                y2 = y_coords[loc2]
             endelse
          endif

          loc1 = loc1 + 1l
          loc2 = loc2 + 1l
          loc3 = loc3 + 1l
          loc4 = loc4 + 1l

       endfor

;**    Nothing found **

       if ( num_crosses eq 0 ) then $
       begin
          intervals = 0
          number_intervals = 0

          RETURN
       endif

       loc1 = 0l
       loc2 = 1l
       loc3 = 0l

;**   Sort intersections **

       for loop1 = 1l, num_crosses, 1l do $
       begin

          loc4 = loop1 + 1l
          loc5 = loc1
          loc6 = loc2
          loc7 = loc3

          for loop2 = loc4, num_crosses, 1 do $
          begin

             loc5 = loc5 + 2l 
             loc6 = loc6 + 2l 
             loc7 = loc7 + 1l 
 
             if ( crosses[loc5] ge crosses[loc1] ) then continue 
 
             temp1 = crosses[loc1]
             temp2 = crosses[loc2]
             temp3 = weights[loc3]
             crosses[loc1] = crosses[loc5]
             crosses[loc2] = crosses[loc6]
             weights[loc3] = weights[loc7]
             crosses[loc5] = temp1
             crosses[loc6] = temp2
             weights[loc7] = temp3

          endfor

          loc1 = loc1 + 2l ;
          loc2 = loc2 + 2l ;
          loc3 = loc3 + 1l ;

       endfor

;**    Compute candidate columns **

       loc1 = num_crosses * 2l - 1l
       min_col = x_to_column ( crosses[0], benchmark_x, benchmark_col, $
                               x_resolution ) 
       max_col = x_to_column ( crosses[loc1], benchmark_x, benchmark_col, $
                               x_resolution )
       min_col = min_col - 1l

;**    Compute 1st candidate column center coordinate **

       x1 = column_to_x ( min_col, benchmark_x, benchmark_col, $
                          x_resolution )

       count = 0l
       in1 = 0l
       in2 = -1l
       loc1 = 0l
       loc2 = 1l
       x2 = crosses[0]
       x3 = crosses[1]

;**    Loop through candidate columns **

       for loop1 = min_col, max_col, 1l do $
       begin
 
          if ( x1 ge x2 ) then $
          begin
             if ( ( x1 lt x3 ) and ( in2 ne -1 ) ) then $
             begin
                if ( in2 ) then intervals[loc2] = loop1
             endif $
             else $
             begin
                count = 0l
                in2 = -1l
                loc3 = 0l
                loc4 = 1l
                loc5 = 0l

                for loop2 = 1l, num_crosses, 1l do $
                begin
                   x2 = crosses[loc3]
                   x3 = crosses[loc4]

                   if ( crosses[loc3] gt x1 ) then break

                   if ( ( x1 ge crosses[loc3] ) and $
                        ( x1 le crosses[loc4] ) ) then $
                   begin
                      count = -weights[loc5]
                      if ( crosses[loc3] ne crosses[loc4] ) then count = -2l
                      break
                   endif

                   count = count + weights[loc5]
                   loc3 = loc3 + 2l
                   loc4 = loc4 + 2l
                   loc5 = loc5 + 1l

                endfor

;**             Process intersection count **

                if ( count eq -1 ) then $
                begin
                   if ( in1 ) then $
                   begin
                      in1 = 0l
                      loc1 = loc1 + 2l
                      loc2 = loc2 + 2l
                   endif $
                   else $
                   begin
                      number_intervals = number_intervals + 1l

                      if ( number_intervals gt max_ints ) then $
                      begin
                         max_ints = max_ints + 256
                         intervals = [intervals, LONARR ( 2*256 )]
                      endif

                      intervals[loc1] = loop1
                      intervals[loc2] = loop1
                      in1 = 1
                   endelse
                endif $
                else if ( count eq -2 ) then $
                begin

;**                Determine if horizontal segment inside polygon **

                   polygon_intervals_01, x_coords, y_coords, $
                                         number_vertices, x1, y1, in2 

                   if ( in2 ) then $
                   begin
                      if ( in1 ) then $
                      begin
                         intervals[loc2] = loop1
                      endif $
                      else $
                      begin
                         number_intervals = number_intervals + 1l

                         if ( number_intervals gt max_ints ) then $
                         begin
                            max_ints = max_ints + 256
                            intervals = [intervals, LONARR ( 2*256 )]
                         endif

                         intervals[loc1] = loop1
                         intervals[loc2] = loop1
                         in1 = 1
                      endelse
                   endif $
                   else $
                   begin
                      if ( in1 ) then $
                      begin
                         in1 = 0l
                         loc1 = loc1 + 2l
                         loc2 = loc2 + 2l
                      endif
                   endelse
                endif $
                else $
                begin

;**                Cell center does not fall on intersection **

                   if ( ( count MOD 2 ) eq 0 ) then $
                   begin
                      if ( in1 ) then $
                      begin
                         in1 = 0l
                         loc1 = loc1 + 2
                         loc2 = loc2 + 2
                      endif
                   endif $
                   else $
                   begin
                      if ( in1 ) then $
                      begin
                         intervals[loc2] = loop1
                      endif $
                      else $
                      begin
                         number_intervals = number_intervals + 1l

                         if ( number_intervals gt max_ints ) then $
                         begin
                            max_ints = max_ints + 256
                            intervals = [intervals, LONARR ( 2*256 )]
                         endif

                         intervals[loc1] = loop1
                         intervals[loc2] = loop1
                         in1 = 1
                      endelse
                   endelse 
                endelse
             endelse
          endif $
          else $
          begin
;**          Candidate west of start of next intersection **
             if ( in1 ) then $
             begin
                intervals[loc2] = loop1
             endif
          endelse

          x1 = x1 + x_resolution

       endfor

;**    Free memory **

       crosses = 0
       weights = 0

       if ( number_intervals gt 0l ) then $
       begin
          intervals = intervals[0:2*number_intervals-1]
       endif $
       else intervals = 0

    endelse

;** Return to calling routine **

    RETURN

END
