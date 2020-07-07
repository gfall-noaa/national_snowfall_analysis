;**************************************************************************
;*
;* object_intervals
;*
;* This procedure computes grid cell centers polygon membership by row as
;* column intervals for multipart shapes. Otherwise same as polygon intervals
;*
;*    object_intervals, option, shape, row, number_intervals, intervals, $
;*                      benchmark_x, benchmark_y, benchmark_col, $
;*                      benchmark_row, x_resolution, y_resolution
;*
;*  Name                    Description
;*  =========================================================================
;*
;*  option            >  0 : Terminate
;*                       1 : Initialize
;*                       2 : Return polygon membership
;*  shape             <> Read shape structure
;*  row               >  Polygon membership grid row
;*  number_intervals  <  Number of grid row membership
;*                       intervals
;*  intervals         <  Grid row membership intervals
;*                       start/stop columns array 
;*  benchmark_x       >  Header info: benchmark x-axis coordinate
;*  benchmark_y       >  Header info: benchmark y-axis coordinate
;*  benchmark_col     >  Header info: benchmark x-axis column
;*  benchmark_row     >  Header info: benchmark y-axis row
;*  x_resolution      >  Header info: Raster x-axis resolution
;*  y_resolution      >  Header info: Raster y-axis resolution
;*
;*  History:
;*  Anders Nilsson, VV, 08/28/03, Adapted from GISRS function
;*  Anders Nilsson, VV, 11/03/03, Fixed bugs
;*  Anders Nilsson, UCAR, 10/06/06, More explicit for loop typing
;*
;****************************************************************************

PRO object_intervals, option, shape, row, number_intervals, intervals, $
                      benchmark_x, benchmark_y, benchmark_col, $
                      benchmark_row, x_resolution, y_resolution

    if ( option eq 0 ) then $
    begin

;**    Terminate **

       intervals = 0l
       number_intervals = 0l

    endif $
    else if ( option eq 1 ) then $
    begin

;**    Loop through polygons and initialize **/

       number_shapes = shape.n_parts
       if ( number_shapes le 1 ) then $
       begin

          x_coords = (*shape.vertices)[0,*]
          y_coords = (*shape.vertices)[1,*]

          polygon_intervals, 1, x_coords, y_coords, 0, $
                             number_intervals, intervals, $
                             benchmark_x, benchmark_y, benchmark_col, $
                             benchmark_row, x_resolution, y_resolution

          (*shape.vertices)[0,*] = x_coords
          (*shape.vertices)[1,*] = y_coords

       endif $
       else $
       begin

          for loop2 = 0l, ( number_shapes -1l ) do $
          begin

             start = (*shape.parts)[loop2] ;

             if loop2 eq ( shape.n_parts - 1 ) then $
             begin
                stop = shape.n_vertices - 1 
             endif $
             else $
             begin
                stop = (*shape.parts)[ ( loop2 + 1 ) ] - 1 ;
             endelse

             x_coords = (*shape.vertices)[0,start:stop]
             y_coords = (*shape.vertices)[1,start:stop]  

             polygon_intervals, 1, x_coords, y_coords, 0, $
                                number_intervals, intervals, $
                                benchmark_x, benchmark_y, benchmark_col, $
                                benchmark_row, x_resolution, y_resolution

             (*shape.vertices)[0,start:stop] = x_coords
             (*shape.vertices)[1,start:stop] = y_coords

          endfor

       endelse

    endif $
    else $
    begin

;**    Calculate **

       number_shapes = shape.n_parts
       if ( number_shapes le 1 ) then $
       begin

          number_intervals = 0l
          intervals = 0

          x_coords = (*shape.vertices)[0,*]
          y_coords = (*shape.vertices)[1,*]

          polygon_intervals, 2, x_coords, y_coords, row, $
                             number_intervals, intervals, $
                             benchmark_x, benchmark_y, benchmark_col, $
                             benchmark_row, x_resolution, y_resolution

       endif $
       else $
       begin

          number_intervals = 0l
          intervals = 0l
          merged_sets = 0l
       
          for loop2 = 0l, ( number_shapes - 1l ) do $
          begin

             start = (*shape.parts)[loop2] ;

             if loop2 eq ( shape.n_parts - 1 ) then $
             begin
                stop = shape.n_vertices - 1 
             endif $
             else $
             begin
                stop = (*shape.parts)[ ( loop2 + 1 ) ] - 1 ;
             endelse

             x_coords = (*shape.vertices)[0,start:stop]
             y_coords = (*shape.vertices)[1,start:stop]

             polygon_intervals, 2, x_coords, y_coords, row, $
                                number_intervalsi, intervalsi, $
                                benchmark_x, benchmark_y, benchmark_col, $
                                benchmark_row, x_resolution, y_resolution

             if ( number_intervalsi gt 0 ) then $
             begin

                if ( number_intervals eq 0 ) then $
                begin
                   intervals = [intervalsi]
                endif $
                else $ 
                begin
                   intervals = [intervals, intervalsi]
                endelse
             
                number_intervals = number_intervals + number_intervalsi 
                merged_sets = merged_sets + 1

             endif

          endfor

;**       Sort intervals **

          if ( merged_sets gt 1 ) then $
          begin

             sort_index = SORT ( intervals[2*LINDGEN(number_intervals)] )

             intervals[2*LINDGEN(number_intervals)] = $
                                               intervals[2*sort_index]
             intervals[2*LINDGEN(number_intervals)+1] = $
                                               intervals[2*sort_index+1]

             temp_intervals = LONARR ( 2*number_intervals )

;**          Go through and perform subtraction **

             loc_min = 0l
             number_temp = 1l
             last_interval_min = intervals[0]
             last_interval_max = intervals[1]
             temp_intervals[0] = intervals[0]
             temp_intervals[1] = intervals[1]

             for loc1 = 1l, ( number_intervals - 1l ), 1 do $
             begin

                interval_min = intervals[2*loc1]
                interval_max = intervals[2*loc1+1]

;**             Array reduced to no intervals **

                if ( loc_min eq number_temp ) then $
                begin
                   temp_intervals[2*loc_min] = interval_min
                   temp_intervals[2*loc_min+1] = interval_max
                   number_temp = number_temp + 1
                   continue
                endif

;**             Loop from min to max **

                for loc2 = loc_min, ( number_temp - 1l ), 1l do $
                begin

                   last_interval_min = temp_intervals[2*loc2] 
                   last_interval_max = temp_intervals[2*loc2+1]

;**                Separate interval before **

                   if ( interval_max lt ( last_interval_min - 1 ) ) then $
                   begin

;**                   Move values to the right for insertion **

                      for loc3 = number_temp, ( loc2+1l ), -1l do $
                      begin
                         temp_intervals[2*loc3] = temp_intervals[2*loc3-2] 
                         temp_intervals[2*loc3+1] = temp_intervals[2*loc3-1]
                      endfor

                      temp_intervals[2*loc2] = interval_min
                      temp_intervals[2*loc2+1] = interval_max
                      number_temp = number_temp + 1l
                      loc2 = loc2 + 1l

;**                   No need to check further **

                      break 

                   endif $
                   else if ( interval_max eq ( last_interval_min - 1l ) ) then $
                   begin

;**                   Prefix interval **

                      temp_intervals[2*loc2] = interval_min

;**                   No need to check further **/

                      break

                   endif $
                   else if ( ( interval_max lt last_interval_max ) and $
                             ( interval_min lt last_interval_min ) ) then $
                   begin

;**                   Overlapping interval to the left **/

;**                   Move values to the right for insertion **/

                      for loc3 = number_temp, ( loc2+1l ), -1l do $
                      begin
                         temp_intervals[2*loc3] = temp_intervals[2*loc3-2]
                         temp_intervals[2*loc3+1] = temp_intervals[2*loc3-1]
                      endfor

                      temp_intervals[2*loc2] = interval_min
                      temp_intervals[2*loc2+1] = last_interval_min - 1l
                      temp_intervals[2*loc2+2] = interval_max + 1l
                      number_temp = number_temp + 1l
                      loc2 = loc2 + 1l

;**                   No need to check further **/

                      break

                   endif $
                   else if ( ( interval_max lt last_interval_max ) and $
                           ( interval_min eq last_interval_min ) ) then $
                   begin

;**                   Truncate interval to right **/

                      temp_intervals[2*loc2] = interval_max + 1l

;**                   No need to check further **/

                      break

                   endif $
                   else if ( ( interval_max lt last_interval_max ) and $
                             ( interval_min gt last_interval_min ) ) then $
                   begin

;**                   Split interval **/

;**                   Move values to the right for insertion **/

                      for loc3 = number_temp, ( loc2+1 ), -1l do $
                      begin
                         temp_intervals[2*loc3] = temp_intervals[2*loc3-2]
                         temp_intervals[2*loc3+1] = temp_intervals[2*loc3-1]
                      endfor

                      temp_intervals[2*loc2] = last_interval_min
                      temp_intervals[2*loc2+1] = interval_min - 1l
                      temp_intervals[2*loc2+2] = interval_max + 1l
                      number_temp = number_temp + 1l
                      loc2 = loc2 + 1l

;**                   No need to check further **/

                      break

                   endif $
                   else if ( ( interval_max eq last_interval_max ) and $
                             ( interval_min eq last_interval_min ) ) then $
                   begin

;**                   Equal. Remove interval **/

;**                   Move values to the left for deletion **/

                      for loc3 = loc2 + 1l, ( number_temp-1l ), 1l do $
                      begin
                         temp_intervals[2*loc3-2] = temp_intervals[2*loc3]
                         temp_intervals[2*loc3-1] = temp_intervals[2*loc3+1]
                      endfor

                      number_temp = number_temp - 1l
                      loc2 = loc2 - 1l

;**                   No need to check further **/

                      break

                   endif $
                   else if ( ( interval_max eq last_interval_max ) and $
                             ( interval_min gt last_interval_min ) ) then $
                   begin

;**                   Truncate interval to left **/

                      temp_intervals[2*loc2+1] = interval_min - 1l

;**                   No need to check further **/

                      break

                   endif $
                   else if ( ( interval_max eq last_interval_max ) and $
                             ( interval_min < last_interval_min ) ) then $
                   begin

;**                   Prefix and truncate **/

                      temp_intervals[2*loc2] = interval_min
                      temp_intervals[2*loc2+1] = last_interval_min - 1l

;**                   No need to check further **/

                      break

                   endif $
                   else if ( ( interval_min eq last_interval_min ) and $
                             ( interval_max gt last_interval_min ) ) then $
                   begin

;**                   Append and truncate **/

;**                   Move values to the left for deletion **/

                      for loc3 = loc2 + 1l, ( number_temp-1l ), 1l do $
                      begin
                         temp_intervals[2*loc3-2] = temp_intervals[2*loc3]
                         temp_intervals[2*loc3-1] = temp_intervals[2*loc3+1]
                      endfor

                      number_temp = number_temp - 1l
                      loc2 = loc2 - 1l

                      interval_min = last_interval_max + 1l

                   endif $
                   else if ( ( interval_min lt last_interval_min ) and $
                             ( interval_max gt last_interval_max ) ) then $
                   begin

;**                   Completely encloses **/

                      temp_intervals[2*loc2] = interval_min
                      temp_intervals[2*loc2+1] = last_interval_min - 1l
                      interval_min = last_interval_max + 1l

                   endif $
                   else if ( ( interval_min le last_interval_max ) and $
                             ( interval_max gt last_interval_max ) ) then $
                   begin

;**                   Overlapping interval to the right **/

                      temp_intervals[2*loc2+1] = interval_min - 1l
                      interval_min = last_interval_max + 1l

                   endif $
                   else if ( interval_min eq last_interval_max + 1l ) then $
                   begin

;**                   Append **/

;**                   Move values to the left for deletion **/

                      for loc3 = loc2 + 1l, ( number_temp-1l ), 1l do $
                      begin
                         temp_intervals[2*loc3-2] = temp_intervals[2*loc3]
                         temp_intervals[2*loc3-1] = temp_intervals[2*loc3+1]
                      endfor

                      number_temp = number_temp - 1l
                      loc2 = number_temp - 1l

                      interval_min = last_interval_min

                   endif $
                   else if ( interval_min gt last_interval_max + 1l ) then $
                   begin

;**                   Completely to the right **/

;**                   Increment search beginning **/

                      loc_min = loc2

                   endif

;**                Add interval at end **/

                   if ( loc2+1l eq number_temp ) then $
                   begin

                      temp_intervals[2*loc2+2] = interval_min
                      temp_intervals[2*loc2+3] = interval_max
                      number_temp = number_temp + 1l
                      loc2 = loc2 + 1l

                   endif

                endfor

             endfor

;**          Transfer **

             number_intervals = number_temp

             if ( number_temp gt 0 ) then $
             begin
                intervals = temp_intervals[0:2*number_temp - 1l]
             endif $
             else intervals = 0l

             temp_intervals = 0l

          endif

       endelse

;**    Return to calling routine **

       return

    endelse

END
