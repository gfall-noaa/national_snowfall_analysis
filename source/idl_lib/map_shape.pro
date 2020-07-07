;************************************************************************
;*
;*  Draws shapefiles for IDL procedures
;*
;*  
;*  Modifications:
;*
;*  Anders Nilsson, VV,   05/13/02,  Added tweak to handle old Export bug
;*  Anders Nilsson, VV,   09/10/02,  Added web functions
;*                                   Added /CENTERTEXT option
;*                                   Added symsize gt 0 check
;*  Anders Nilsson, VV,   09/12/02,  Makes sure polygon does not go beyond
;*                                   [-180,180,-90,90]
;*  Anders Nilsson, VV,   09/26/02,  Added WEED option to remove_extra_points
;*  Anders Nilsson, VV,   12/31/02,  Handles different htmllink types
;*  Anders Nilsson, VV,   01/06/03,  Uses !P.POSITION to determine placement
;*                                   within canvas for html maps
;*                                   Fixed allocation of out_x_coords2 in 
;*                                   clip_polygon
;*  Anders Nilsson, VV,   01/07/03,  Added outline_arc
;*                                   Clip polygon checks to see if needs to 
;*                                   terminate polygon
;*  Anders Nilsson, VV,   05/07/03,  Fixed lower justification of point
;*                                   annotation
;*  Anders Nilsson, VV,   08/27/03,  Fixed freeing of shape entity
;*  Anders Nilsson, VV,   10/07/03,  Added spatial index (.sbx,.sbn) support
;*  Anders Nilsson, VV,   11/06/03,  Error message prints to standard error
;*  Anders Nilsson, VV,   05/06/04,  Added object graphics support
;*  Anders Nilsson, VV,   10/12/04,  Added transparent filled background for
;*                                   object text. Nudged object labels 
;*                                   vertically closer to point
;*  Anders Nilsson, VV,   11/30/04,  Added support for plotted data values
;*                                   Initializes some common block variables
;*  Anders Nilsson, VV,   01/03/05,  Added disabled text background for direct
;*                                   graphics
;*  Anders Nilsson, VV,   02/23/05,  Calculates bin_map_size from file size
;*                                   remove_extra_points checks reversing
;*  Anders Nilsson, VV,   03/10/05,  Added SYM_BACKGROUND option
;*                                   Added FORMAT_FLOAT option
;*                                   Added FORMAT_PRECISION option
;*  Anders Nilsson, VV,   04/27/05,  Checks bin_number vs number_bins
;*                                   Added ZERO to format_value
;*  Anders Nilsson, UCAR, 09/20/05,  Added GOOD_FIELD and GOOD_SHAPES,
;*                                   which allows the user to filter by name
;*  Anders Nilsson, UCAR, 10/06/06,  More explicit for loop typing
;*
;************************************************************************

; This precedure eliminates duplicate points
; The "WEED" option removes colinear points
;

PRO remove_extra_points, x_coords, y_coords, WEED=weed

;   Check inputs

    length = N_ELEMENTS ( x_coords )
    if ( n_elements ( weed ) eq 0 ) then weed = 0

    length = length - 1
    count = 1L
    last_x = x_coords[0]
    last_y = y_coords[0]
    old_x_diff = 0
    old_y_diff = 0
    removed = 0

    if ( weed eq 0 ) then $
    begin

       for loop = 1L, length do $
       begin

          x_val = x_coords[loop]
          y_val = y_coords[loop]

          if ( x_val ne last_x or y_val ne last_y ) then $
          begin

             count = count + 1
             x_coords[count] = x_val
             y_coords[count] = y_val
             last_x = x_val
             last_y = y_val 

          endif

       endfor

    endif $
    else $
    begin

       for loop = 1L, length do $
       begin

          x_val = x_coords[loop]
          y_val = y_coords[loop]

          x_diff = x_val - last_x
          y_diff = y_val - last_y

          if ( x_diff ne 0 or y_diff ne 0 ) then $
          begin

             if ( ( old_x_diff ne 0 or old_y_diff ne 0 ) and $
                  ( ( ( x_diff * old_y_diff ) ne ( y_diff * old_x_diff ) ) or $
                    ( ( x_diff lt 0 ) xor ( old_x_diff lt 0 ) ) or $
                    ( ( y_diff lt 0 ) xor ( old_y_diff lt 0 ) ) ) ) then $
             count = count + 1

             x_coords[count] = x_val
             y_coords[count] = y_val

             old_x_diff = x_diff
             old_y_diff = y_diff

             last_x = x_val
             last_y = y_val

          endif

       endfor

    endelse

    x_coords = x_coords[0:count]
    y_coords = y_coords[0:count]

END

; This procedure completely outlines an arc with an approximate radius 
;

PRO outline_arc, x_coords, y_coords, radius

;   Check inputs

    length = N_ELEMENTS ( x_coords ) 
    if ( n_elements ( radius ) eq 0 ) then return
    if ( radius eq 0 ) then return
    if ( length lt 1 ) then return
    if ( n_elements ( y_coords ) ne length ) then return

    type = SIZE ( x_coords, /TYPE )
    radiusi = DOUBLE ( radius ) 

    output_length = 5l + 2 * ( ( length - 2 ) > 0 )
    
    out_x_coords = MAKE_ARRAY ( output_length, TYPE=type )
    out_y_coords = MAKE_ARRAY ( output_length, TYPE=type )
    good = MAKE_ARRAY ( output_length, /BYTE, VALUE=0 )

;   Initialize variables

    last_dx = 0.0
    last_dy = 0.0
    limit = length-1
    count = 0

    for loop = 0l, limit do $
    begin

;      Find segment points

       x1 = DOUBLE ( x_coords[loop] )
       y1 = DOUBLE ( y_coords[loop] )

;      Skip to not colocated point

       for loop2 = loop + 1l, limit do $
       begin

          x2 = DOUBLE ( x_coords[loop2] )
          y2 = DOUBLE ( y_coords[loop2] )

          if ( x1 ne x2 or y1 ne y2 ) then break

       endfor

;      Exception for last point
    
       if loop2 gt limit then $
       begin

          if ( last_dx eq 0.0 and last_dy eq 0.0 ) then $
          begin
 
             last_dx = 0.0
             last_dy = radius

          endif

          x2 = x1 + last_dx
          y2 = y1 + last_dy

       endif

;      Calculate slope of segment

       dx = x2 - x1
       dy = y2 - y1

       d_length2 = SQRT ( dx^2 + dy^2 )
       if ( d_length2 ne 0 ) then normal_factor = 1.0 / d_length2 $
       else normal_factor = 1.0

;      Check for colinearity

       if ( ( dx * last_dy - dy * last_dx ) ne 0.0 ) then $
       begin

;         Find point on new segment 1 unit from start point

          x2a = x1 + normal_factor * dx
          y2a = y1 + normal_factor * dy

;         Find average point which bisects angle       

          x3 = ( x2a + last_x2b ) / 2.0
          y3 = ( y2a + last_y2b ) / 2.0

       endif $
       else $
       begin
  
          x3 = x1 - dy
          y3 = y1 + dx

       endelse

;      Determine the side of the segment that the point resides
;      Convert right hand side to left hand side

       dx3 = x3 - x1
       dy3 = y3 - y1

;      Flip point

       if ( ( dx3 * dy - dy3 * dx ) gt 0 ) then $
       begin

          dx3 = -dx3
          dy3 = -dy3

       endif

;      Normalize point to radius

       d_length3 = SQRT ( dx3^2 + dy3^2 )
       normal_factor3 = radius / d_length3

       x3 = x1 + normal_factor3 * dx3
       y3 = y1 + normal_factor3 * dy3

       opp_x3 = x1 - normal_factor3 * dx3
       opp_y3 = y1 - normal_factor3 * dy3

;      Exceptions for end points - extend away from segment

       norm_x = normal_factor * dx
       norm_y = normal_factor * dy
       opp_idx = output_length-loop-2

       if ( loop ne 0 and loop2 le limit ) then $
       begin

;         Non endpoint data

          out_x_coords[loop] = x3
          out_y_coords[loop] = y3
          good[loop] = 1

          out_x_coords[opp_idx] = opp_x3
          out_y_coords[opp_idx] = opp_y3
          good[opp_idx] = 1

       endif $
       else $
       begin

          loop = loop - 1
          opp_idx = opp_idx + 1

          if ( loop lt 0 ) then $
          begin

             loop = loop + 1
             opp_idx = opp_idx - 1

             out_x_coords[loop] = x3 - radius * norm_x
             out_y_coords[loop] = y3 - radius * norm_y
             good[loop] = 1
   
             out_x_coords[opp_idx] = opp_x3 - radius * norm_x
             out_y_coords[opp_idx] = opp_y3 - radius * norm_y
             good[opp_idx] = 1

          endif

          if ( loop2 gt limit ) then $
          begin

             loop = loop + 1
             opp_idx = opp_idx - 1
 
             out_x_coords[loop] = x3 + radius * norm_x
             out_y_coords[loop] = y3 + radius * norm_y
             good[loop] = 1

             out_x_coords[opp_idx] = opp_x3 + radius * norm_x
             out_y_coords[opp_idx] = opp_y3 + radius * norm_y
             good[opp_idx] = 1

          endif

       endelse

;      Find point on new segment 1 unit from stop point

       last_x2b = x2 - norm_x
       last_y2b = y2 - norm_y
  
       last_dx = dx
       last_dy = dy

       loop = loop2 - 1

    endfor

    good_data = where ( good eq 1 )

;   Seal polygon

    x_coords = [ out_x_coords[good_data], out_x_coords[0] ]
    y_coords = [ out_y_coords[good_data], out_y_coords[0] ]

END


; This procedure attempts to clip polygons to within a rectangle, and
; weed them to a certain number of points, if specified.


PRO clip_polygon, x_coords, y_coords, limits, max_number

;   Check inputs

    length = N_ELEMENTS ( x_coords )
    type = SIZE ( x_coords,/TYPE )

    if ( length le 1 ) then return

;   Test to see if polygon actually terminates

    if ( x_coords[0] eq x_coords[length-1] and $
         y_coords[0] eq y_coords[length-1] ) then terminate_flag = 1 $
    else terminate_flag = 0
    


    out_x_coords1 = MAKE_ARRAY ( 2 * length, TYPE=type )
    out_y_coords1 = MAKE_ARRAY ( 2 * length, TYPE=type )

    min_y = limits[0]
    min_x = limits[1]
    max_y = limits[2]
    max_x = limits[3]
    mode = 0B 
    last_wall = 0B

;   Loop through four times, one for each side
;   Left edge

    length = length - 1

    if length ge 0 then $
    begin
       last_x = x_coords[length]
       last_y = y_coords[length]
    endif
    loop2 = 0L

    for loop1 = 0L, length do $
    begin

       x_val = x_coords[loop1]
       y_val = y_coords[loop1]

       val_flag = 0b
       last_flag = 1b

       if ( x_val ge min_x ) then val_flag = 1
       if ( last_x lt min_x ) then last_flag = 0

       if ( val_flag xor last_flag ) then $
       begin

          x_int = min_x
          y_int = last_y + ( DOUBLE ( y_val - last_y ) / $
                             DOUBLE ( x_val - last_x ) ) * $
                           DOUBLE ( x_int - last_x )
          out_x_coords1[loop2] = x_int
          out_y_coords1[loop2] = y_int
          loop2 = loop2 + 1

       endif

       if ( val_flag ) then $
       begin

          out_x_coords1[loop2] = x_val
          out_y_coords1[loop2] = y_val
          loop2 = loop2 + 1  

       endif

       last_x = x_val
       last_y = y_val

    endfor

;   Allocate more memory

    x_coords = 0
    y_coords = 0
    out_x_coords2 = MAKE_ARRAY ( 2 * ( length+1 ), TYPE=type )
    out_y_coords2 = MAKE_ARRAY ( 2 * ( length+1 ), TYPE=type )

;   Right edge

    length = loop2 - 1

    if ( length ge 0 ) then $
    begin
       last_x = out_x_coords1[length]
       last_y = out_y_coords1[length]
    endif
    loop2 = 0L

    for loop1 = 0L, length do $
    begin

       x_val = out_x_coords1[loop1]
       y_val = out_y_coords1[loop1]

       val_flag = 0b
       last_flag = 1b

       if ( x_val le max_x ) then val_flag = 1
       if ( last_x gt max_x ) then last_flag = 0

       if ( val_flag xor last_flag ) then $
       begin

          x_int = max_x
          y_int = last_y + ( DOUBLE ( y_val - last_y ) / $
                             DOUBLE ( x_val - last_x ) ) * $
                           DOUBLE ( x_int - last_x )
          out_x_coords2[loop2] = x_int
          out_y_coords2[loop2] = y_int
          loop2 = loop2 + 1

       endif

       if ( val_flag ) then $
       begin

          out_x_coords2[loop2] = x_val
          out_y_coords2[loop2] = y_val
          loop2 = loop2 + 1

       endif

       last_x = x_val
       last_y = y_val

    endfor

;   Bottom edge

    length = loop2 - 1

    if ( length ge 0 ) then $
    begin
       last_x = out_x_coords2[length]
       last_y = out_y_coords2[length]
    endif
    loop2 = 0L

    for loop1 = 0L, length do $
    begin

       x_val = out_x_coords2[loop1]
       y_val = out_y_coords2[loop1]
       val_flag = 0b
       last_flag = 1b

       if ( y_val ge min_y ) then val_flag = 1
       if ( last_y lt min_y ) then last_flag = 0

       if ( val_flag xor last_flag ) then $
       begin

          y_int = min_y
          x_int = last_x + ( DOUBLE ( x_val - last_x ) / $
                             DOUBLE ( y_val - last_y ) ) * $
                           DOUBLE ( y_int - last_y )

          out_x_coords1[loop2] = x_int
          out_y_coords1[loop2] = y_int
          loop2 = loop2 + 1

       endif

       if ( val_flag ) then $
       begin

          out_x_coords1[loop2] = x_val
          out_y_coords1[loop2] = y_val
          loop2 = loop2 + 1

       endif

       last_x = x_val
       last_y = y_val

    endfor

;   Top edge

    length = loop2 - 1

    if ( length ge 0 ) then $
    begin
       last_x = out_x_coords1[length]
       last_y = out_y_coords1[length]
    endif
    loop2 = 0L

    for loop1 = 0L, length do $
    begin

       x_val = out_x_coords1[loop1]
       y_val = out_y_coords1[loop1]

       val_flag = 0b
       last_flag = 1b

       if ( y_val le max_y ) then val_flag = 1
       if ( last_y gt max_y ) then last_flag = 0


       if ( val_flag xor last_flag ) then $
       begin

          y_int = max_y
          x_int = last_x + ( DOUBLE ( x_val - last_x ) / $
                             DOUBLE ( y_val - last_y ) ) * $
                           DOUBLE ( y_int - last_y )

          out_x_coords2[loop2] = x_int
          out_y_coords2[loop2] = y_int
          loop2 = loop2 + 1

       endif

       if ( val_flag ) then $
       begin

          out_x_coords2[loop2] = x_val
          out_y_coords2[loop2] = y_val
          loop2 = loop2 + 1

       endif

       last_x = x_val
       last_y = y_val

    endfor


;   Free some memory

    out_x_coords1 = 0
    out_y_coords1 = 0


;   Clipped length

    length = loop2 - 1



;   Make sure polygon terminates

    if ( length ge 0 and terminate_flag eq 1 ) then $
    begin

       if ( out_x_coords2[length] ne out_x_coords2[0] or $
            out_y_coords2[length] ne out_y_coords2[0] ) then $
       begin
          length = length + 1
          out_x_coords2[length] = out_x_coords2[0]
          out_y_coords2[length] = out_y_coords2[0]
       endif

    endif

;   Proceed with weeding

    if ( max_number lt ( length + 1 ) ) then $
    begin

;      Count number and length of non-edge lines

       last_x = out_x_coords2[0]
       last_y = out_y_coords2[0]
       seg_start = 0L
       seg_stop = 0L
       in_seg = 0L
       count = 0L
       stay_count = 0L

       for loop1 = 0L, length do $
       begin

          x_val = out_x_coords2[loop1]
          y_val = out_y_coords2[loop1] 

          if ( x_val eq min_x or x_val eq max_x or $
               y_val eq min_y or y_val eq max_y ) then $
          begin

             stay_count = stay_count + 1

             if ( in_seg eq 1 ) then $
             begin

;               End of segment

                seg_stop = [seg_stop,loop1]

             endif

             in_seg = 2

          endif $
          else if ( in_seg eq 0 ) then $
          begin

;            Beginning of segment

             stay_count = stay_count + 1
             seg_start = [seg_start,loop1]
             count = count + 1
             in_seg = 1

          endif $
          else if ( in_seg eq 2 ) then $
          begin 

;            Previous point is beginning of segment

             seg_start = [seg_start,loop1-1]
             count = count + 1
             in_seg = 1

          endif

       endfor

;      Check terminating point

       if ( in_seg eq 1 ) then $
       begin


          stay_count = stay_count + 1
          seg_stop = [seg_stop,length]
          in_seg = 2

       endif

;      Calculate points to reduce

       free_count = ABS ( max_number - stay_count )
       index = 0L
       temp_length = length + 1 - stay_count
       out_x_coords1 = 0
       out_y_coords1 = 0



;      Loop through line segments
;      Will keep beginning and end of each segment

       for loop1 = 1L, count do $
       begin

          start = seg_start[loop1]
          stop = seg_stop[loop1]

          out_x_coords1 = [out_x_coords1, out_x_coords2[index:start]]
          out_y_coords1 = [out_y_coords1, out_y_coords2[index:start]]

          index = stop

          if ( temp_length gt 0 ) then $
          begin

             start = start + 1
             stop = stop - 1
             local_length = stop - start + 1

             new_length = LONG ( DOUBLE ( free_count ) * $
                          ( DOUBLE ( local_length ) / DOUBLE ( temp_length ) ) )
             temp_length = temp_length - local_length 

             if ( new_length gt 0 ) then $
             begin

                out_x_coords1 = [out_x_coords1, $
                                 CONGRID (out_x_coords2[start:stop], $
                                          new_length )]
                out_y_coords1 = [out_y_coords1, $
                                 CONGRID (out_y_coords2[start:stop], $
                                          new_length )] 

                free_count = free_count - new_length

             endif

          endif

       endfor

       x_coords = 0
       y_coords = 0

       out_x_coords1 = [out_x_coords1, out_x_coords2[index:length]]
       out_y_coords1 = [out_y_coords1, out_y_coords2[index:length]]

       x_coords = out_x_coords1[1:*]
       y_coords = out_y_coords1[1:*]

    endif $
    else $
    begin

       if ( length ge 0 ) then $
       begin
          x_coords = out_x_coords2[0:length]
          y_coords = out_y_coords2[0:length]
       endif $
       else $
       begin
          x_coords = 0
          y_coords = 0
       endelse

    endelse


;   Free memory

    out_x_coords1 = 0
    out_y_coords1 = 0
    out_x_coords2 = 0
    out_y_coords2 = 0

END

;*****************************************************************************
;*
;*  MAP_SHAPE_01
;*
;*  This subroutine processes an individual shape
;*
;*  History:
;*  Anders Nilsson, VV, 10/03/03, Created
;*  Anders Nilsson, VV, 10/31/03, HTML imagemap uses title= instead of name=
;*  Anders Nilsson, VV, 01/26/04, Removes trailing spaces, etc from link
;*  Anders Nilsson, VV, 05/06/04, Tessellates object graphics better
;*  Anders Nilsson, VV, 03/16/05, Correctly tesselates in direct graphics
;*
;*****************************************************************************

PRO  MAP_SHAPE_01, shape_file, shape_number, MAP_STRUCTURE = mapStruct

;** Initialize **

    check_bad = 0b
    free_shape = 0b
    clear_attrib = 0b
    label = ''
    windowname= 'test'

;** Common block variables **

    COMMON SHAPE_OPTIONS, min_lat, min_lon, max_lat, max_lon, $
                          alt_idx, link_idx, place_idx, label_idx, color_idx, $
                          good_idx, htmlmap, charsize, symsize, centertext, $
                          sheight, swidth, fheight, fwidth, d_bounds, $
                          d_bounds2, y_size, fill_shape, omov_jscript, $
                          omot_jscript, js_flag, htmllink, radius, $
                          zvalue, html_lun, object, parent, font, symbol, $
                          buffer, tess, image, color, class_offset, $
                          red, green, blue, number_colors, text_background, $
                          symbol_background, label_int, label_slope, $
                          resolution, format_float, format_length, $
                          format_precision, good_shapes, extra

    ;; shape_file->IDLffShape::GetProperty, ATTRIBUTE_NAMES = attName
    ;; ind = WHERE(attName eq 'NAME', count)
    ;; if (count eq 0) then STOP
    ;; nameInd = ind[0]

;** Get individual shapes **

    shape = shape_file -> IDLffShape::GetEntity ( shape_number )
    free_shape = 1b

;** Check extents **

    if shape.bounds[0] gt max_lon or $
       shape.bounds[1] gt max_lat or $
       shape.bounds[4] lt min_lon or $
       shape.bounds[5] lt min_lat then goto, outa_here

;** Check for possible bad shapes **

    if ( shape.bounds[0] le -180.0 ) or $ 
       ( shape.bounds[1] le -90.0 ) or $
       ( shape.bounds[4] ge 180.0 ) or $
       ( shape.bounds[5] ge 90.0 ) then check_bad = 1

    attrib = shape_file -> getAttributes ( shape_number )
    clear_attrib = 1b

    ;; shapeName = attrib.(nameInd)
    ;; print, 'shape_number ', shape_number, ' has name ', shapeName

;** Get attribute fields **

    if ( htmlmap ne 0 or shape.shape_type eq 1 or good_idx ne -1 ) then $
    begin

       attrib = shape_file -> getAttributes ( shape_number )
       clear_attrib = 1b
       if label_idx ne -1 then $
       begin
          label = attrib.(label_idx)
          type = SIZE ( label, /TYPE )
          if ( type ne 7 and ( label_slope ne 1 or label_int ne 0 ) ) then $
          begin
             ON_IOERROR, error_bad_value
             label = label_slope * DOUBLE ( label ) + label_int
             ON_IOERROR, NULL
          endif 

          if ( type ne 7 ) then label = FORMAT_VALUE ( label, type, $
                                                       format_length, $
                                                       format_precision, 0, $
                                                       format_float, /ZERO ) 
          label = STRTRIM ( label, 1 )
       endif $
       else $
       begin
          error_bad_value :
             label = ''
       endelse

       if color_idx ne -1 then $
       begin
          local_color = attrib.(color_idx)

;**       First two colors reserved **

          if ( local_color ge 2 ) then local_color = local_color + class_offset

       endif $
       else local_color = color

       if ( number_colors ne 0 ) then $
       begin
          local_color = ( 0 > local_color ) < ( number_colors - 1 )
          if ( object ) then local_color = [red[local_color], $
                                         green[local_color], $
                                         blue[local_color] ]
       endif 

       if ( good_idx ne -1 ) then $
       begin
          possible_good_name = attrib.(good_idx)
          possible_good_idx = where ( possible_good_name eq good_shapes )
          if ( possible_good_idx[0] eq -1 ) then goto, outa_here
       endif

    endif $
    else local_color = color


;** Remove bad characters **

    if ( htmlmap ne 0 ) then $
    begin

;**    Get more fields **

       if alt_idx ne -1 then alt = attrib.(alt_idx) $
       else alt = ''
       if link_idx ne -1 then link = attrib.(link_idx) $
       else link = ''

;**    Strip alt field of trailing spaces **

       loc = STRPOS ( alt, '  ', /REVERSE_SEARCH )
       if ( loc ne -1 ) then alt = STRTRIM ( STRMID ( alt, 0, loc ), 0 )

       loc = STRPOS ( alt, "'" )
       while ( loc ne -1 ) do $
       begin
          STRPUT, alt, ' ', loc
          loc = STRPOS ( alt, "'", loc )
       endwhile

       loc = STRPOS ( alt, '"' )
       while ( loc ne -1 ) do $
       begin
          STRPUT, alt, ' ', loc
          loc = STRPOS ( alt, '"', loc )
       endwhile

;**    Strip link field of trailing spaces **

       loc = STRPOS ( link, '  ', /REVERSE_SEARCH )
       if ( loc ne -1 ) then link = STRTRIM ( STRMID ( link, 0, loc ), 0 )

       loc = STRPOS ( link, "'" )
       while ( loc ne -1 ) do $
       begin
          STRPUT, link, ' ', loc
          loc = STRPOS ( link, "'", loc )
       endwhile

       loc = STRPOS ( link, '"' )
       while ( loc ne -1 ) do $
       begin
          STRPUT, link, ' ', loc
          loc = STRPOS ( link, '"', loc )
       endwhile

       loc = STRPOS ( link, ' ' )
       while ( loc ne -1 ) do $
       begin
          STRPUT, link, '_', loc
          loc = STRPOS ( link, ' ', loc )
       endwhile

    endif

;** Draw shape **

    if shape.shape_type eq 1 then $
    begin

;**    Point data type **

       if ( htmlmap ne 2 ) then $
       begin

          if ( symsize ge 0 ) then $
          begin

             if ( object ) then $
             begin

                oPoly = OBJ_NEW ( 'IDLgrPolyline', $
                                  [shape.bounds[0], shape.bounds[0]], $
                                  [shape.bounds[1], shape.bounds[1]], $
                                  SYMBOL=symbol, COLOR=local_color, $
                                  _EXTRA=extra )
                parent->Add, oPoly

             endif $
             else $
             begin

                map_coords = CONVERT_COORD ( shape.bounds[0], $
                                             shape.bounds[1], $
                                             /DATA, /TO_DEVICE, /DOUBLE )
                if ( symbol_background ne -1 ) then $
                begin

                   PLOTS, map_coords[0]-resolution, map_coords[1]-resolution, $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                   PLOTS, map_coords[0]-resolution, map_coords[1], $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                   PLOTS, map_coords[0]-resolution, map_coords[1]+resolution, $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                   PLOTS, map_coords[0], map_coords[1]-resolution, $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                   PLOTS, map_coords[0], map_coords[1]+resolution, $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                   PLOTS, map_coords[0]+resolution, map_coords[1]-resolution, $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                   PLOTS, map_coords[0]+resolution, map_coords[1], $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                   PLOTS, map_coords[0]+resolution, map_coords[1]+resolution, $
                          zvalue, NOCLIP=0, /DEVICE, SYMSIZE=symsize, $
                          _EXTRA=extra, COLOR=symbol_background
                endif

                PLOTS, map_coords[0], map_coords[1], zvalue, NOCLIP=0, $
                       /DEVICE, SYMSIZE=symsize, COLOR=local_color, $
                       _EXTRA=extra

             endelse

          endif

;**       Calculate annotation position information **

          if ( charsize ge 0 ) then $
          begin

             if place_idx ne -1 then placement = attrib.(place_idx) $
             else placement = 'UC'

             if STRLEN ( placement ) lt 2 then placement = 'UC'

             placement1 = STRMID ( placement, 0, 1 )
             placement2 = STRMID ( placement, 1, 1 )

             if ( STRCMP ( placement1, 'U', /FOLD_CASE ) ) then $
                top_flag = 2 $
             else if ( STRCMP ( placement1, 'C', /FOLD_CASE ) ) then $
                top_flag = 1 $
             else if ( STRCMP ( placement1, 'L', /FOLD_CASE ) ) then $
                top_flag = 0 $
             else top_flag = 2

             if ( STRCMP ( placement2, 'L', /FOLD_CASE ) ) then $
                left_flag = 2 $
             else if ( STRCMP ( placement2, 'C', /FOLD_CASE ) ) then $
                left_flag = 1 $
             else if ( STRCMP ( placement2, 'R', /FOLD_CASE ) ) then $
                left_flag = 0 $
             else left_flag = 1

             if ( object ) then $
             begin

                x_value = shape.bounds[0]
                y_value = shape.bounds[1]

                oText1 = OBJ_NEW ( 'IDLgrText', label, FONT=font, $
                                  LOCATIONS=[x_value, y_value], $
                                  RECOMPUTE_DIMENSIONS=2, $
                                  COLOR=local_color, _EXTRA=extra )
                parent->Add, oText1
                if ( text_background ne 0 ) then $
                begin
                   oText2 = OBJ_NEW ( 'IDLgrText', label, FONT=font, $
                                     LOCATIONS=[x_value, y_value], $
                                     COLOR=local_color, /FILL_BACKGROUND, $
                                     RECOMPUTE_DIMENSIONS=2, $
                                     ALPHA_CHANNEL=0.3, $
                                     _EXTRA=extra )
                   parent->Add, oText2
                endif
                dims = buffer->GetTextDimensions ( oText1 )
                fwidth = 1.0
                fheight = dims[1] 
                string_length = dims[0]

                sheight = 0.5 * dims[1]

             endif $
             else $
             begin

                text_coord = CONVERT_COORD ( shape.bounds[0], $
                                             shape.bounds[1], $
                                             /DATA, /TO_DEVICE, /DOUBLE )
                x_value = text_coord[0]
                y_value = text_coord[1]
                string_length = STRLEN ( label )
                string_length = string_length * fwidth * 1.5

             endelse

;**          Check vertical bounds **

             valignment = 0.5

             if ( centertext eq 0 ) then $
             begin

                if ( ( y_value + sheight + 1.5 * fheight ) ge $
                     d_bounds[2] ) then $
                begin
                   valignment = 0.0
                endif $
                else if ( ( y_value - sheight - 1.5 * fheight ) le $
                          d_bounds[0] ) then $
                begin
                   valignment = 1.0
                endif $
                else if ( top_flag eq 2 ) then $
                begin
                   valignment = 1.0
                endif $
                else if ( top_flag eq 0 ) then $
                begin
                   valignment = 0.0
                endif

                if ( valignment eq 1.0 ) then $
                   y_value = y_value + sheight $
                else if ( valignment eq 0.0 ) then $
                   y_value = y_value - ( sheight + fheight ) $
                else y_value = y_value - ( 0.5 * fheight )

             endif

             alignment = 0.5

;**          Guess length of string with padding **
;**          Adjust horizontal alignment **

             if ( centertext eq 0 ) then $
             begin

                if ( ( x_value + 0.5 * string_length ) ge $
                     d_bounds[3] ) then $
                begin
                   alignment = 1.0
                endif $
                else if ( ( x_value + string_length ) ge $
                          d_bounds[3] and left_flag eq 0 ) then $
                begin
                   alignment = 0.5
                endif $
                else if ( ( x_value - 0.5 * string_length ) le $
                     d_bounds[1] ) then $
                begin
                   alignment = 0.0
                endif $
                else if ( ( x_value - string_length ) le $
                     d_bounds[1] and left_flag eq 2 ) then $
                begin
                   alignment = 0.5
                endif $
                else if ( left_flag eq 2 ) then alignment = 1.0 $
                else if ( left_flag eq 0 ) then alignment = 0.0

             endif $
             else alignment = 0.0

             if ( object ) then $
             begin

;**             Adjust vertical and horizontal alignment **

                oText1->SetProperty, LOCATIONS=[x_value, y_value]
                oText1->SetProperty, ALIGNMENT=alignment
                if ( text_background ne 0 ) then $
                begin
                   oText2->SetProperty, LOCATIONS=[x_value, y_value]
                   oText2->SetProperty, ALIGNMENT=alignment
                endif

             endif $
             else $
             begin

;**             If this looked ok for text background... **

                if ( text_background ne 0 ) then $
                begin

                   XYOUTS, x_value-resolution, y_value-resolution, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                   XYOUTS, x_value-resolution, y_value, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                   XYOUTS, x_value-resolution, y_value+resolution, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                   XYOUTS, x_value, y_value-resolution, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                   XYOUTS, x_value, y_value+resolution, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                   XYOUTS, x_value+resolution, y_value-resolution, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                   XYOUTS, x_value+resolution, y_value, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                   XYOUTS, x_value+resolution, y_value-resolution, label, $
                           /DEVICE, NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                           CHARSIZE=charsize, COLOR=0
                endif

                XYOUTS, x_value, y_value, label, /DEVICE, $
                        NOCLIP=0, Z=zvalue, ALIGNMENT=alignment, $
                        CHARSIZE=charsize, COLOR=local_color, _EXTRA=extra

             endelse

          endif

       endif

;**    Draw HTML map if asked **

       if ( htmlmap ne 0 ) then $
       begin

          text_coord = CONVERT_COORD ( shape.bounds[0], shape.bounds[1], $
                                       /DATA, /TO_NORMAL, /DOUBLE )
          text_coord = CONVERT_COORD ( text_coord[0], 1.0 - text_coord[1], $
                                       /NORMAL, /TO_DEVICE, /DOUBLE )

;**       Handle possible javascript link **

          if ( js_flag eq 0 ) then $
             map_string = '<area shape="circle" coords="' + $
                 STRTRIM ( STRING ( text_coord[0],FORMAT='(I)'),1) + ',' + $
                 STRTRIM ( STRING ( text_coord[1],FORMAT='(I)'),1) + ',' + $
                 STRTRIM ( STRING ( radius,FORMAT='(I)'),1) + '" target="' + $
                 windowname + '" title="' + alt + $
                 '" href="' + htmllink + link + '&x=' + $
                 STRTRIM ( STRING ( shape.bounds[0],FORMAT='(D)'),1) + $
                 '&y=' + $
                 STRTRIM ( STRING ( shape.bounds[1],FORMAT='(D)'),1) $
          else $
             map_string = '<area shape="circle" coords="' + $
                 STRTRIM ( STRING ( text_coord[0],FORMAT='(I)'),1) + ',' + $
                 STRTRIM ( STRING ( text_coord[1],FORMAT='(I)'),1) + ',' + $
                 STRTRIM ( STRING ( radius,FORMAT='(I)'),1) + '" title="' + $
                 alt + '" href="' + htmllink + '''' + link + ''',' + $
                 STRTRIM ( STRING ( shape.bounds[0],FORMAT='(D)'),1) + $
                 ',' + $
                 STRTRIM ( STRING ( shape.bounds[1],FORMAT='(D)'),1) + ');'



          map_string = map_string + '" alt="' + alt + '"'

          if ( STRLEN ( omov_jscript ) gt 0 ) then $
          begin
             map_string = map_string + ' onMouseOver="' + $
                          omov_jscript + '''' + alt + ''');"'
          endif

          if ( STRLEN ( omot_jscript ) gt 0 ) then $
          begin
             map_string = map_string + ' onMouseOut="' + $
                          omot_jscript + '''' + alt + ''');"'
          endif

          map_string = map_string + '>'

          printf, html_lun, map_string
          map_string = ''

       endif

    endif $
    else $
    begin

;**    Arc or polygon **/

       if shape.n_parts lt 2 then $
       begin

;**       Single part polygon or arc **

          if ( object ) then $
          begin

             dims = SIZE ( (*shape.vertices), /DIMENSIONS )
             length = dims[1]
             status = 0

             if ( fill_shape ne 0 and $
                  shape.shape_type eq 5 and $
                  length ge 3 ) then $
             begin

;**             Tessellate polygon **

                tess->AddPolygon, REFORM ( (*shape.vertices)[0,0:length-2], $
                                           length-1 ), $
                                  REFORM ( (*shape.vertices)[1,0:length-2], $
                                           length-1 )
                status = tess->Tessellate ( tess_verts, tess_poly )

             endif
             if ( status ne 0 ) then $
             begin
                status = 0b
                if ( n_elements ( image ) ne 0 ) then $
                begin
                   if ( OBJ_ISA ( image, 'IDLgrImage' ) ) then $
                   begin
                      status = 1b
                      oPoly = OBJ_NEW ( 'IDLgrPolygon', $
                                        DATA=tess_verts, POLYGONS=tess_poly, $
                                        COLOR=local_color, $
                                        TEXTURE_MAP=image, _EXTRA=extra )
                   endif
                endif
                if ( status eq 0 ) then $
                begin

                   oPoly = OBJ_NEW ( 'IDLgrPolygon', $
                                     DATA=tess_verts, POLYGONS=tess_poly, $
                                     COLOR=local_color, _EXTRA=extra )

                endif

             endif $
             else $
             begin

                oPoly = OBJ_NEW ( 'IDLgrPolyline', $
                                  (*shape.vertices)[0,0:length-1], $
                                  (*shape.vertices)[1,0:length-1], $
                                  COLOR=local_color, _EXTRA=extra )
             endelse

             parent->Add, oPoly

;**          Clear memory **

             if ( fill_shape ne 0 and $
                  shape.shape_type eq 5 and $
                  length ge 3 ) then $
             begin
                tess_verts = 0
                tess_poly = 0
                tess->Reset
             endif

          endif $
          else $
          begin

;**          Bad data check **

             date0=SYSTIME(1)
             if ( check_bad ) then $
             begin

                map_coord = 0

                xData = ((*shape.vertices)[0,*] < $
                         (180.0d - 0.00000000001d)) > $
                        (-180.0d + 0.00000000001d)

                yData = ((*shape.vertices)[1,*] < $
                         (90.d - 0.00000000001d)) > $
                        (-90.0d + 0.00000000001d)

                if (N_TAGS(mapStruct) gt 0) then begin

                   data_coord = $
                      MAP_PROJ_FORWARD(xData, yData, $
                                       MAP_STRUCTURE = mapStruct)

                   xData = data_coord[0,*]
                   yData = data_coord[1,*]

                endif

             endif $
             else $
             begin $

                map_coord = 0

                xData = (*shape.vertices)[0,*]
                yData = (*shape.vertices)[1,*]

                if (N_TAGS(mapStruct) gt 0) then begin

                   data_coord = $
                      MAP_PROJ_FORWARD(xData, yData, $
                                       MAP_STRUCTURE = mapStruct)

                   xData = data_coord[0,*]
                   yData = data_coord[1,*]

                endif

             endelse

             map_coord = LONG(CONVERT_COORD(xData, yData, $
                                            /DATA, /TO_DEVICE, /DOUBLE))

             t_coords_x = LONG ( map_coord[0,*] )
             t_coords_y = LONG ( map_coord[1,*] )

;**          Weed unnecessary points **

             remove_extra_points, t_coords_x, t_coords_y, /WEED

;**          Draw **

             if ( htmlmap ne 2 ) then $
             begin

                if ( fill_shape ne 0 and shape.shape_type eq 5 ) then $
                begin

                   length = N_ELEMENTS ( t_coords_x )
                   if ( length ge 3 ) then $
                   begin

                      POLYFILL, t_coords_x, t_coords_y, $
                                zvalue, NOCLIP=0, /DEVICE, _EXTRA=extra
                   endif

                endif $
                else $
                   PLOTS, t_coords_x, t_coords_y, $
                   zvalue, NOCLIP=0, /DEVICE, _EXTRA=extra

             endif

             if ( htmlmap ne 0 ) then $
             begin

;**             Display HTML **

;**             Reduce number of points **

                clip_polygon, t_coords_x,t_coords_y, LONG(d_bounds2), 100

                t_coords_y = y_size - t_coords_y

                if ( shape.shape_type eq 3 ) then $
                begin

;**                Outline arc types **

                   outline_arc, t_coords_x, t_coords_y, radius

                endif

                length = N_ELEMENTS ( t_coords_x )

                if ( length gt 1 ) then $
                begin

                   map_string = ''

                   for loop3 = 0L, ( length - 1 ) do $
                   begin
                      lon = t_coords_x[loop3]
                      lat = t_coords_y[loop3]

                      if ( loop3 ne 0 ) then map_string = map_string + ','
                      map_string = map_string + $
                                   STRING ( lon,FORMAT='(I)') + $
                                   ',' + STRING ( lat,FORMAT='(I)')

                   endfor

                   map_string = STRCOMPRESS(map_string, /REMOVE_ALL )

                   if ( js_flag eq 0 ) then $
                   begin
                      map_string = '<area shape="poly" coords="' + $
                                   map_string + '" target="' + windowname + $
                                   '" title="' + alt + '" href="' + $
                                   htmllink + link + '" alt="' + alt + '"'
                   endif $
                   else $
                   begin
                      map_string = '<area shape="poly" coords="' + $
                                   map_string + '" title="' + alt + $
                                   '" href="' + htmllink + '''' + $
                                   link + ''');" alt="' + alt + '"'
                   endelse

                   if ( STRLEN ( omov_jscript ) gt 0 ) then $
                   begin
                      map_string = map_string + ' onMouseOver="' + $
                                   omov_jscript + '''' + alt + ''');"'
                   endif

                   if ( STRLEN ( omot_jscript ) gt 0 ) then $
                   begin
                      map_string = map_string + ' onMouseOut="' + $
                                   omot_jscript + '''' + alt + ''');"'
                   endif

                   map_string = map_string + '>'

                   printf, html_lun, map_string
                   map_string = ''

                endif

             endif

          endelse

       endif $
       else $
       begin

;**       Multipart polygon **

;**       Quick loop through for tessellation to see if possible **

          status = 0b

          if ( htmlmap ne 2 and $
               fill_shape and $
               shape.shape_type eq 5 ) then $
          begin

;**          Initialize tesselation **

             tess->Reset 

             for loop2 = 0L, ( shape.n_parts - 1 ) do $
             begin

                start = 0L
                start = (*shape.parts)[loop2] ;

                if loop2 eq ( shape.n_parts - 1 ) then $
                begin
                   stop = shape.n_vertices - 1
                endif $
                else $
                begin
                   stop = (*shape.parts)[ ( loop2 + 1 ) ] - 1 ;
                endelse

                length = stop-start+1

                if ( length ge 3 ) then $
                begin

                   tess->AddPolygon, REFORM ( (*shape.vertices) $
                                              [0,start:stop-1], stop-start ), $
                                     REFORM ( (*shape.vertices) $
                                              [1,start:stop-1], stop-start )
                endif

             endfor

;**          Tessellate **

             status = tess->Tessellate ( tess_verts, tess_poly ) 

             if ( status ne 0 ) then $
             begin
                status = 0b
                if ( object ) then $
                begin
                   if ( n_elements ( image ) ne 0 ) then $
                   begin
                      if ( OBJ_ISA ( image, 'IDLgrImage' ) ) then $
                      begin
                         status = 1b

                         oPoly = OBJ_NEW ( 'IDLgrPolygon', $
                                           DATA=tess_verts, $
                                           POLYGONS=tess_poly, $
                                           COLOR=local_color, $
                                           TEXTURE_MAP=image, _EXTRA=extra )
                      endif
                   endif
                   if ( status eq 0 ) then $
                   begin

                      oPoly = OBJ_NEW ( 'IDLgrPolygon', $
                                        DATA=tess_verts, POLYGONS=tess_poly, $
                                        COLOR=local_color, _EXTRA=extra )
                   endif

                   parent->Add, oPoly

                endif $
                else $
                begin 

;**                Handle filled polygon islands in direct graphics **
                
                   coords_x = ( tess_verts[0,*] > (-180.0d ) ) < 180.0d
                   coords_y = ( tess_verts[1,*] > (-90.0d ) ) < 90.0d

                   poly_size = n_elements ( tess_poly )
                   loc = 0

                   while ( loc lt poly_size ) do $
                   begin

                      num_points = tess_poly[loc]
                      loc = loc + 1
                      if ( num_points lt 0 ) then break
                      if ( num_points gt 2 ) then $
                      begin

                         POLYFILL, coords_x[tess_poly[loc:loc+num_points-1]], $
                                   coords_y[tess_poly[loc:loc+num_points-1]], $
                                   zvalue, NOCLIP=0, /DATA, $ 
                                   _EXTRA=extra, THICK=0
                      endif
                      loc = loc + num_points

                   endwhile

                endelse
                status = 1b
             endif

             tess_verts = 0
             tess_poly = 0
             tess->Reset

          endif

          for loop2 = 0L, ( shape.n_parts - 1 ) do $
          begin

             start = 0L
             start = (*shape.parts)[loop2] ;

             if loop2 eq ( shape.n_parts - 1 ) then $
             begin
                stop = shape.n_vertices - 1
             endif $
             else $
             begin
                stop = (*shape.parts)[ ( loop2 + 1 ) ] - 1 ;
             endelse

;**          Object graphics outline **

             if ( object ) then $
             begin

                if ( status eq 0 ) then $
                begin

                   length = stop-start+1
                   status = 0

;**                Try again, just to make sure **

                   if ( fill_shape ne 0 and $
                        shape.shape_type eq 5 and $
                        length ge 3 ) then $
                   begin

;**                   Tessellate polygon **

                      tess->AddPolygon, REFORM ( (*shape.vertices) $
                                              [0,start:stop-1], stop-start ), $
                                        REFORM ( (*shape.vertices) $
                                              [1,start:stop-1], stop-start )
                      status = tess->Tessellate ( tess_verts, tess_poly )

                   endif
                   if ( status ne 0 ) then $
                   begin
                      status = 0b
                      if ( n_elements ( image ) ne 0 ) then $
                      begin
                         if ( OBJ_ISA ( image, 'IDLgrImage' ) ) then $
                         begin
                            status = 1b
                            oPoly = OBJ_NEW ( 'IDLgrPolygon', $
                                              DATA=tess_verts, $
                                              POLYGONS=tess_poly, $
                                              COLOR=local_color, $
                                              TEXTURE_MAP=image, _EXTRA=extra )

                         endif
                      endif
                      if ( status eq 0 ) then $
                      begin

                         oPoly = OBJ_NEW ( 'IDLgrPolygon', $
                                           DATA=tess_verts, $
                                           POLYGONS=tess_poly, $
                                           COLOR=local_color, _EXTRA=extra )
                      endif

                   endif $
                   else $
                   begin

                      oPoly = OBJ_NEW ( 'IDLgrPolyline', $
                                        (*shape.vertices)[0,start:stop], $
                                        (*shape.vertices)[1,start:stop], $
                                        COLOR=local_color, _EXTRA=extra )
                   endelse

                   parent->Add, oPoly
                   
;**                Clear memory **

                   if ( fill_shape ne 0 and $
                        shape.shape_type eq 5 and $
                        length ge 3 ) then $
                   begin
                      tess_verts = 0
                      tess_poly = 0
                      tess->Reset
                   endif

                endif

             endif $
             else if ( status eq 0 ) then $
             begin

;**             Direct graphics **

                if ( check_bad ) then $
                begin

                    xData = ( (*shape.vertices)[0,start:stop] < $
                              ( 180.0d - 0.00000000001d ) ) > $
                            ( -180.0d + 0.00000000001d )
                    yData = ( (*shape.vertices)[1,start:stop] < $
                              ( 90.d - 0.00000000001d ) ) > $
                            ( -90.0d + 0.00000000001d )

                    if (N_TAGS(mapStruct) gt 0) then begin

                        data_coord = $
                            MAP_PROJ_FORWARD(xData, yData, $
                                             MAP_STRUCTURE = mapStruct)

                        xData = data_coord[0,*]
                        yData = data_coord[1,*]

                    endif

                    map_coord = 0
                    map_coord = $
                        CONVERT_COORD ( xData, yData, $
                                        /DATA, /TO_DEVICE, /DOUBLE )

                endif $
                else $
                begin

                    xData = (*shape.vertices)[0,start:stop]
                    yData = (*shape.vertices)[1,start:stop]

                    if (N_TAGS(mapStruct) gt 0) then begin

                        data_coord = $
                            MAP_PROJ_FORWARD(xData, yData, $
                                             MAP_STRUCTURE = mapStruct)

                        xData = data_coord[0,*]
                        yData = data_coord[1,*]

                    endif

                    map_coord = 0
                    map_coord = $
                        CONVERT_COORD ( xData, yData, $
                                        /DATA, /TO_DEVICE, /DOUBLE )

                endelse


                if (N_TAGS(mapStruct) gt 0) then begin
                    data_coord = $
                        MAP_PROJ_FORWARD(map_coord[0,*], $
                                         map_coord[1,*], $
                                         MAP_STRUCTURE = mapStruct)

                   t_coords_x = data_coord[0,*]
                   t_coords_y = data_coord[1,*]

                endif

                t_coords_x = LONG ( map_coord[0,*] )
                t_coords_y = LONG ( map_coord[1,*] )

                remove_extra_points, t_coords_x, t_coords_y, /WEED

                if ( htmlmap ne 2 ) then $
                begin

;**                Draw **

                   if ( fill_shape ne 0 and shape.shape_type eq 5 ) then $
                   begin

                      length = N_ELEMENTS ( t_coords_x )
                      if ( length ge 3 ) then $
                      begin

                         POLYFILL, t_coords_x, t_coords_y, $
                                   zvalue, NOCLIP=0, /DEVICE, _EXTRA=extra
                      endif

                   endif $

                   else $
                   PLOTS, t_coords_x, t_coords_y, $
                          zvalue, NOCLIP=0, /DEVICE, _EXTRA=extra

                endif

                if ( htmlmap ne 0 ) then $
                begin

;**                Display HTML **

                   clip_polygon, t_coords_x,t_coords_y, LONG(d_bounds2), 100

                   length = N_ELEMENTS ( t_coords_x )
                   if ( length le 1 ) then continue

                   t_coords_y = y_size - t_coords_y

                   if ( shape.shape_type eq 3 ) then $
                   begin

                      outline_arc, t_coords_x, t_coords_y, radius

                   endif

                   map_string = ''

                   for loop3 = 0L, ( length - 1 ) do $
                   begin
                      lon = t_coords_x[loop3]
                      lat = t_coords_y[loop3]

                      if ( loop3 ne 0 ) then map_string = map_string + ','
                      map_string = map_string + $
                                   STRING ( lon,FORMAT='(I)') + $
                                   ',' + STRING ( lat,FORMAT='(I)')

                   endfor

                   map_string=STRCOMPRESS(map_string, /REMOVE_ALL )
                   if ( js_flag eq 0 ) then $
                   map_string = '<area shape="poly" coords="' + map_string + $
                          '" target="' + windowname + '" title="' + alt + $
                          '" href="' + htmllink + link + '" alt="' + $
                          alt + '"' $
                   else $
                   map_string = '<area shape="poly" coords="' + map_string + $
                          '" title="' + alt + '" href="' + htmllink + '''' + $
                          link + ''');" alt="' + alt + '"'

                   if ( STRLEN ( omov_jscript ) gt 0 ) then $
                   begin
                      map_string = map_string + ' onMouseOver="' + $
                                   omov_jscript + '''' + alt + ''');"'
                   endif

                   if ( STRLEN ( omot_jscript ) gt 0 ) then $
                   begin
                      map_string = map_string + ' onMouseOut="' + $
                                   omot_jscript + '''' + alt + ''');"'
                   endif

                   map_string = map_string + '>'

                   printf, html_lun, map_string
                   map_string = ''

                endif

             endif

          endfor

       endelse

    endelse

;** Free shape structures

    if ( free_shape ne 0 ) then $
    begin
       shape_file -> IDLffShape::DestroyEntity, shape
       free_shape = 0b
    endif
    if ( clear_attrib ) then attrib = 0b

;** Return to calling routine **

    RETURN

;** Error exit **

    outa_here :

       if ( free_shape ne 0 ) then $
       begin
          shape_file -> IDLffShape::DestroyEntity, shape
          free_shape = 0b
       endif
       if ( clear_attrib ) then attrib = 0b

       RETURN

END

;*****************************************************************************
;*
;*  MAP_SHAPE_02
;*
;*  This function recursively draws the shapes on screen
;*
;*  History:
;*  Anders Nilsson, VV, 10/06/03, Created
;*  Anders Nilsson, VV, 04/26/05, Checks bin_number vs number_bins
;*
;*****************************************************************************

PRO MAP_SHAPE_02, shape_file, $
                  sub_min_lat, sub_min_lon, sub_max_lat, sub_max_lon, $
                  option, value, $
                  MAP_STRUCTURE = mapStruct

;** Common Data block **

    COMMON SPATIAL_INFO, rmin_lat, rmin_lon, rmax_lat, rmax_lon, $
                         bin_map, bin_map_size, bin_locations, $
                         number_bins, sbn_lun, debug

;** Check if needed **

    if ( value gt bin_map_size ) then RETURN
    if ( value lt 0 ) then RETURN

    bin_number = bin_map[0,value-1]
    number_bin_items = 0l

;** Read values in sbn list **

    if ( bin_number gt 0 and bin_number le number_bins ) then $
    begin
       number_bin_items = bin_locations[1,bin_number-1]/8
       if ( number_bin_items le 0 ) then bin_number = 0
    endif $
    else bin_number = 0l
    if ( bin_number gt 0 ) then $
    begin
       item_list = LONARR ( 2, number_bin_items )

;**    Go to beginning of data record **

       POINT_LUN, sbn_lun, bin_locations[0,bin_number-1] + 8

       READU, sbn_lun, item_list

;**    Display values in list. Loop backwards **

       for loop = number_bin_items-1, 0, -1 do $
       begin

          MAP_SHAPE_01, shape_file, item_list[1,loop]-1, $
                        MAP_STRUCTURE = mapStruct

       endfor

    endif

;** First calculation of new value **

    new_value = 2l*value
    if ( new_value gt bin_map_size ) then RETURN

;** Split **

    if ( option eq 0 ) then $
    begin
       lon = ( sub_min_lon + sub_max_lon ) / 2.0d
;       OPLOT, [lon,lon],[sub_max_lat,sub_min_lat]
       if ( rmax_lon ge lon ) then $
       begin
          MAP_SHAPE_02, shape_file, $
                        sub_min_lat, lon, sub_max_lat, sub_max_lon, $
                        1, new_value, MAP_STRUCTURE = mapStruct
       endif
       if ( ( rmin_lon le lon ) and $
            ( new_value lt bin_map_size ) ) then $
       begin
          MAP_SHAPE_02, shape_file, $
                        sub_min_lat, sub_min_lon, sub_max_lat, lon, $
                        1, new_value+1, MAP_STRUCTURE = mapStruct
       endif
    
    endif $
    else if ( option eq 1 ) then $
    begin
       lat = ( sub_min_lat + sub_max_lat ) / 2.0d
;       OPLOT, [sub_min_lon,sub_max_lon],[lat,lat]
       if ( rmax_lat ge lat ) then $
       begin
          MAP_SHAPE_02, shape_file, $
                        lat, sub_min_lon, sub_max_lat, sub_max_lon, $
                        0, new_value, MAP_STRUCTURE = mapStruct
       endif
       if ( ( rmin_lat le lat ) and $
            ( new_value lt bin_map_size ) ) then $
       begin
          MAP_SHAPE_02, shape_file, $
                        sub_min_lat, sub_min_lon, lat, sub_max_lon, $
                        0, new_value+1, MAP_STRUCTURE = mapStruct
       endif
    endif

;** Return to calling routine **

    RETURN

END

;***********************************************************************
;*
;*  MAP_SHAPE
;*
;**********************************************************************

PRO MAP_SHAPE, $
               path, $
               HTML_MAP = htmlmap_i, $
               HTML_LINK = htmllink_i, $
               HTML_LUN = html_lun_i, $
               HTML_OMOV = omov_jscript_i, $
               HTML_OMOT = omot_jscript_i, $
               HTML_RADIUS = radius_i, $
               CENTERTEXT = centertext_i, $  
               FILL_SHAPE = fill_shape_i, $
               LABEL_INT   = label_int_i, $
               LABEL_SLOPE = label_slope_i, $
               LABEL_FIELD = label_field, $
               ALT_FIELD = alt_field, $
               LINK_FIELD = link_field, $
               PLACE_FIELD = place_field, $
               CLASS_FIELD = color_field, $
               GOOD_FIELD = good_field, $
               LIMITS = limits, $
               MLINESTYLE = mlinestyle_i, $
               MLINETHICK = mlinethick_i, $
               CHARSIZE = charsize_i, $
               SYMSIZE = symsize_i, $
               SPACING = spacing_i, $
               COLOR = color_i, $
               CLASS_OFFSET = class_offset_i, $
               T3D = t3d, $
               ORIENTATION = orientation_i, $
               ZVALUE = zvalue_i, $
               PARENT = parent_i, $
               FONT = font_i, $
               SYMBOL = symbol_i, $
               BUFFER = buffer_i, $
               IMAGE = image_i, $
               RED = red_i, $
               GREEN = green_i, $
               BLUE = blue_i, $
               GOOD_SHAPES = good_shapes_i, $
               FORMAT_FLOAT = format_float_i, $
               FORMAT_PRECISION = format_precision_i, $
               TEXT_BACKGROUND = text_background_i, $
               SYM_BACKGROUND = symbol_background_i, $
               _EXTRA = extra_i, $
               MAP_STRUCTURE = mapStruct
;
;       if fill_shape = 0 you get outline
;       if fill_shape = 1 you get filled solid polygon
;       if fill_shape = 2 you get filled line polygon
;                                                                               
;       if htmlmap = 0 no printed areas for html
;       if htmlmap = 1 printed areas for html
;       if htmlmap = 2 printed areas for html, no drawing

;** Initialize **

    COMMON SPATIAL_INFO, rmin_lat, rmin_lon, rmax_lat, rmax_lon, $
                         bin_map, bin_map_size, bin_locations, $
                         number_bins, sbn_lun, debug

    COMMON SHAPE_OPTIONS, min_lat, min_lon, max_lat, max_lon, $
                          alt_idx, link_idx, place_idx, label_idx, color_idx, $
                          good_idx, htmlmap, charsize, symsize, centertext, $
                          sheight, swidth, fheight, fwidth, d_bounds, $
                          d_bounds2, y_size, fill_shape, omov_jscript, $
                          omot_jscript, js_flag, htmllink, radius, $
                          zvalue, html_lun, object, parent, font, symbol, $
                          buffer, tess, image, color, class_offset, $
                          red, green, blue, number_colors, text_background, $
                          symbol_background, label_int, label_slope, $
                          resolution, format_float, format_length, $
                          format_precision, good_shapes, extra
                           
    alt_idx = -1
    link_idx = -1
    place_idx = -1
    color_idx = -1
    good_idx = -1
    color = 1
    class_offset = 0
    if ( n_elements ( extra ) ) then $
    extra = create_struct ( 'null', '' )
    buffer = 0
    font = 0
    image = 0
    parent = 0
    symbol = 0
    tess = 0
    htmlmap = 0
    charsize = 1
    symsize = 1
    centertext = 0
    fill_shape = 0
    format_float = 1
    format_length = 4
    format_precision = 2
    good_shapes = -1
    label_slope = 1d
    label_int = 0d
    omov_jscript = ''
    omot_jscript = ''
    radius = 5
    resolution = 1 
    symbol_background = -1
    text_background = 0b
    zvalue = 0.
    html_lun = -1
    htmllink = 'index.html'

    sbx_lun = -1
    sbn_lun = -1
    shx_lun = -1
    shp_lun = -1

    destroy_obj = 0b
    destroy_tess = 0b

;** Check inputs

    if ( n_elements ( fill_shape_i ) ) then fill_shape = fill_shape_i
    if ( n_elements ( htmlmap_i ) ne 0 ) then htmlmap = htmlmap_i
    if ( n_elements ( htmllink_i ) ne 0 ) then htmllink = htmllink_i
    if ( n_elements ( html_lun_i ) ne 0 ) then html_lun = html_lun_i
    if ( n_elements ( label_field ) eq 0 ) then label_field = "name"
    if ( n_elements ( alt_field ) eq 0 ) then alt_field = "name"
    if ( n_elements ( link_field ) eq 0 ) then link_field = "name"
    if ( n_elements ( good_field ) eq 0 ) then good_field = "name"
    if ( n_elements ( place_field ) eq 0 ) then place_field = "placement"
    if ( n_elements ( radius_i ) ne 0 ) then radius = radius_i
    if ( n_elements ( centertext_i ) ne 0 ) then centertext = centertext_i
    if ( n_elements ( charsize_i ) ne 0 ) then charsize = charsize_i
    if ( n_elements ( symsize_i ) ne 0 ) then symsize = symsize_i
    if ( n_elements ( zvalue_i ) ne 0 ) then zvalue = zvalue_i
    if ( n_elements ( font_i ) ne 0 ) then font = font_i
    if ( n_elements ( symbol_i ) ne 0 ) then symbol = symbol_i
    if ( n_elements ( buffer_i ) ne 0 ) then buffer = buffer_i
    if ( n_elements ( color_i ) ne 0 ) then color = color_i
    if ( n_elements ( extra_i ) ne 0 ) then extra = extra_i
    if ( n_elements ( image_i ) ne 0 ) then image = image_i
    if ( n_elements ( red_i ) ne 0 ) then red = red_i 
    if ( n_elements ( green_i ) ne 0 ) then green = green_i
    if ( n_elements ( blue_i ) ne 0 ) then blue = blue_i
    if ( n_elements ( format_float_i ) ne 0 ) then $
    format_float = format_float_i
    if ( n_elements ( format_precision_i ) ne 0 ) then $
    format_precision = format_precision_i
    if ( n_elements ( label_int_i ) ne 0 ) then label_int = label_int_i
    if ( n_elements ( label_slope_i ) ne 0 ) then label_slope = label_slope_i
    if ( n_elements ( text_background_i ) ne 0 ) then $
    text_background = text_background_i
    if ( n_elements ( symbol_background_i ) ne 0 ) then $
    symbol_background = symbol_background_i
    if ( n_elements ( class_offset_i ) ne 0 ) then $
    class_offset = class_offset_i

    number_colors = n_elements ( red_i ) < n_elements ( green_i ) < $
                    n_elements ( blue_i ) 

;** Note that the input javascript that is passed in does not contain  **
;** a terminating ');'...so that the name field, if any, can be fed in **

    if ( n_elements ( omov_jscript_i ) ne 0 ) then $
       omov_jscript = omov_jscript_i
    if ( n_elements ( omot_jscript_i ) ne 0 ) then $
       omot_jscript = omot_jscript_i

;** Determine if using object graphics **

    object = 0b
    if ( n_elements ( parent_i ) ne 0 ) then $
    begin
       if ( OBJ_ISA ( parent_i, 'IDL_Container' ) ) then object = 1b
    endif
    if ( object ) then parent = parent_i

    if ( not object and !x.type ne 3 and N_TAGS(mapStruct) eq 0) then $
    MESSAGE, 'Map transform not established.'

;** Process the _EXTRA for graphics keywords. Normally the _EXTRA **
;** values overwrite the input values, but not for maps. **

    if ( not object ) then $
    begin

       if ( n_elements ( color_i ) ) then $
          map_struct_append, extra, 'COLOR', color_i
       if ( n_elements ( mlinestyle_i ) ) then $
          map_struct_append, extra, 'LINESTYLE', mlinestyle_i
       if ( n_elements ( mlinethick_i ) ) then $
          map_struct_append, extra, 'THICK', mlinethick_i
       if ( n_elements ( spacing_i ) and ( fill_shape eq 2 ) ) then $
          map_struct_append, extra, 'SPACING', spacing_i
       if ( n_elements ( orientation_i ) and ( fill_shape eq 2 ) ) then $
          map_struct_append, extra, 'ORIENTATION', orientation_i
       if ( n_elements ( t3d ) ) then $
          map_struct_append, extra,'t3d',t3d
       if ( fill_shape eq 2 ) then $
          map_struct_append, extra, 'LINE_FILL', 1
   
    endif
    
;** Check type of htmllink for presence of javascript **

    js_flag = 0
    length = STRLEN ( STRTRIM ( htmllink ) )
    letter = STRMID ( htmllink, length-1, 1 )
    if ( letter eq ',' or $
         letter eq '(' ) then js_flag = 1 $
    else if ( letter eq '?' or $
              letter eq '&' or $
              letter eq '=') then js_flag = 0 $
    else htmllink = STRTRIM ( htmllink ) + '?'
    

;** Check limits **

    if n_elements ( limits ) eq 4 then $
       limitd = limits $
    else if ( not object and $
              ( !map.ll_box[0] ne !map.ll_box[2] or $
                !map.ll_box[1] ne !map.ll_box[3] ) ) then $
       limitd = !map.ll_box $
    else $
       limitd = [-90., -360., 90., 360.]

    min_lat = limitd[0] 
    min_lon = limitd[1]
    max_lat = limitd[2] 
    max_lon = limitd[3]

    bounds = [min_lon, min_lat, max_lon, max_lat]

    if ( not object ) then $
    begin

;**    Calculate device bounds for annotation adjustment **
;**    And other annotation information **

       llc = CONVERT_COORD ( min_lon > (-180d), min_lat > (-90d), $
                             /DATA, /TO_DEVICE, /DOUBLE )
       urc = CONVERT_COORD ( max_lon < 180d, max_lat < 90d, $
                             /DATA, /TO_DEVICE, /DOUBLE )
       d_bounds = [llc[1], llc[0], urc[1], urc[0]]
       d_bounds2 = [llc[1]-2, llc[0]-2, urc[1]+2, urc[0]+2]
       fheight = !D.Y_CH_SIZE * charsize
       fwidth = !D.X_CH_SIZE * charsize
       sheight = fheight * symsize 
       swidth = fwidth * symsize 

;** Check for size of whole image **

       x_size = d_bounds[3]-d_bounds[1]
       y_size = d_bounds[2]-d_bounds[0]

       if ( ( !P.POSITION[2] - !P.POSITION[0] ) ne 0.0 ) then $
       x_size = x_size / ( !P.POSITION[2] - !P.POSITION[0] )
       if ( ( !P.POSITION[3] - !P.POSITION[1] ) ne 0.0 ) then $
       y_size = y_size / ( !P.POSITION[3] - !P.POSITION[1] )

       x_size = LONG ( x_size + 0.5 )
       y_size = LONG ( y_size + 0.5 )

       if ( !D.FLAGS and 1 ) then resolution = 25.4 $
       else resolution = 1 

    endif $
    else $
    begin
       d_bounds = limits
       d_bounds2 = limits
    endelse

;** Check for file presence **

    count1 = 0
    count2 = 0
    count3 = 0

    if ( not FILE_TEST ( path + '.shp' ) or $
         not FILE_TEST ( path + '.dbf' ) or $
         not FILE_TEST ( path + '.shx' ) ) then $
    begin
       message, 'ERROR: Shape file "' + path + '" not found/complete', $
                /CONTINUE, /INFORMATIONAL
       goto, outa_here

    endif   

;** Check for presence of spatial index files **

    if ( FILE_TEST ( path + '.sbx' ) and $
         FILE_TEST ( path + '.sbn' ) ) then $
    begin
       use_spatial_index = 1b
    endif $
    else $
    begin
       use_spatial_index = 0b
    endelse

;** Initialize Tesselator **

    tess = OBJ_NEW ( 'IDLgrTessellator' )
    destroy_tess = 1b
    tess->Reset

;** Check bounds of shape file **
;** Unfortunately, this has to be done manually, **
;** as it is beyond the capabilities of the shapefile object **

;** Open shapefile data file **

    OPENR, shp_lun, path + '.shp', /GET_LUN, /SWAP_IF_BIG_ENDIAN

;** Position within top header **

    position1 = 32L
    position2 = 32L

    get_shape_bounds, shp_lun, position1, shape_type, $
                      x_min, y_min, x_max, y_max

;** If entire file is not viewable, skip **

    if x_min gt max_lon or $
       y_min gt max_lat or $
       x_max lt min_lon or $
       y_max lt min_lat then $
    begin

;**    Handle old bug in Export which incorrectly sets y_max **
;**    Proceeds to check each polygon if occurs **

       if ( x_max ne y_max ) then goto, outa_here
          
    endif

;** See if worth it to use spatial index **

    if ( use_spatial_index ) then $
    begin
       if ( min_lon le x_min and $
            min_lat le y_min and $
            max_lon ge x_max and $
            max_lat ge y_max ) then use_spatial_index = 0b
    endif

;** Open the shapefile **

    shape_file = OBJ_NEW ('IDLffShape', path )
    destroy_obj = 1b

;** Open .shx file for index reading **

    OPENR, shx_lun, path + '.shx', /GET_LUN, /SWAP_IF_LITTLE_ENDIAN
    position3 = 100L

;** Get number of objects **

    shape_file -> IDLffShape::GetProperty, N_ENTITIES = number_shapes

    record_number = 0L

;** Find which attribute is requested **

    shape_file -> IDLffShape::GetProperty, ATTRIBUTE_NAMES=names
    shape_file -> IDLffShape::GetProperty, N_ATTRIBUTES=num_attrib

    label_idx = -1
    link_idx = -1
    alt_idx = -1
    place_idx = -1
    color_idx = -1
    good_idx = -1 
    for loop = 0l, num_attrib - 1 do $
    begin
      if ( ( label_idx eq -1 ) and $
           STRCMP ( names[loop], label_field, /FOLD_CASE ) ) then $
         label_idx = loop
      if ( ( link_idx eq -1 ) and $
           STRCMP ( names[loop], link_field, /FOLD_CASE ) ) then $
         link_idx = loop
      if ( ( alt_idx eq -1 ) and $
           STRCMP ( names[loop], alt_field, /FOLD_CASE ) ) then $
         alt_idx = loop
      if ( ( place_idx eq -1 ) and $
           STRCMP ( names[loop], place_field, /FOLD_CASE ) ) then $
         place_idx = loop
      if ( ( good_idx eq -1 ) and $
           STRCMP ( names[loop], good_field, /FOLD_CASE ) ) then $
         good_idx = loop
      if ( ( color_idx eq -1 ) and ( n_elements ( color_field ) ne 0 ) ) then $
      begin
         if ( STRCMP ( names[loop], color_field, /FOLD_CASE ) ) then $
         color_idx = loop
      endif
    endfor


;** Try Default values if nothing found **

    for loop = 0l, num_attrib - 1 do $
    begin
      if ( ( label_idx eq -1 ) and $
           STRCMP ( names[loop], 'name', /FOLD_CASE ) ) then $
         label_idx = loop
      if ( ( link_idx eq -1 ) and $
           STRCMP ( names[loop], 'name', /FOLD_CASE ) ) then $
         link_idx = loop
      if ( ( alt_idx eq -1 ) and $
           STRCMP ( names[loop], 'name', /FOLD_CASE ) ) then $
         alt_idx = loop
      if ( ( good_idx eq -1 ) and $
           STRCMP ( names[loop], 'name', /FOLD_CASE ) ) then $
         good_idx = loop
       if ( ( place_idx eq -1 ) and $
           STRCMP ( names[loop], 'placement', /FOLD_CASE ) ) then $
         place_idx = loop
    endfor

    names = 0

;** If good_shapes not specified, clear good_idx **

    if ( n_elements ( good_shapes_i ) ne 0 ) then good_shapes = good_shapes_i $
    else good_idx = -1


;** Loop through Shape file **

    if ( use_spatial_index eq 1 ) then $
    begin

       rmin_lat = min_lat
       rmin_lon = min_lon
       rmax_lat = max_lat
       rmax_lon = max_lon


;**    Open .sbx file **

       OPENR, sbx_lun, path + '.sbx', /GET_LUN, /SWAP_IF_LITTLE_ENDIAN
       info = FSTAT ( sbx_lun )
       file_size = info.size
       number_bins = ( ( file_size - 100 ) / 4 ) / 2
       sbx_position = 100

       if ( number_bins eq 0 ) then goto, outa_here

;**    Read bin locations

       bin_locations = LONARR ( 2, number_bins )
       POINT_LUN, sbx_lun, sbx_position
       READU, sbx_lun, bin_locations
       bin_locations = 2 * bin_locations

;**    Close the .sbx file **

       CLOSE, sbx_lun 
       FREE_LUN, sbx_lun
       sbx_lun = -1

;**    Open the .sbn file **

       OPENR, sbn_lun, path + '.sbn', /GET_LUN, /SWAP_IF_LITTLE_ENDIAN
       info = FSTAT ( sbn_lun )
       file_size = info.size
       sbn_position = bin_locations[0,0] + 8

;**    Get bin map **

;       bin_map_size = bin_locations[1,0] / 8
       bin_map_size = ( file_size - sbn_position ) / 8

       bin_map = LONARR ( 2, bin_map_size )

       POINT_LUN, sbn_lun, sbn_position
       READU, sbn_lun, bin_map

       debug = LONARR ( number_bins )

;**    Start recursive drawing of layer **

;       time_01 = SYSTIME(1)

       MAP_SHAPE_02, shape_file, y_min, x_min, y_max, x_max, 0, 1l, $
                     MAP_STRUCTURE = mapStruct

;       time_02 = SYSTIME(1)
;       print, time_02 - time_01
       
;**    Close files **

       CLOSE, sbn_lun
       FREE_LUN, sbn_lun
       sbn_lun = -1

;**    Free memory **

       bin_map = 0
       bin_locations = 0

    endif 

    if ( use_spatial_index eq 0 ) then $
    begin

       for loop = 0L, ( number_shapes - 1 ) do $
       begin

;**       Read index file - find location to read in .shp file **

          POINT_LUN, shx_lun, position3
          READU, shx_lun, position2
          
          position3 = position3 + 8

;**       Read .shp record header **

          position2 = position2 * 2

;**       Read .shp record contents **
       
          position1 = position2 + 8
          get_shape_bounds, shp_lun, position1, shape_type, $
                         min_x, min_y, max_x, max_y

;**       Test bounds **

          if min_x gt max_lon or $
             min_y gt max_lat or $
             max_x lt min_lon or $
             max_y lt min_lat then continue 

;**       Process individual shapes **

          MAP_SHAPE_01, shape_file, loop, MAP_STRUCTURE = mapStruct

       endfor

    endif


;** Close shapefile **

    OBJ_DESTROY, shape_file    
    destroy_obj = 0b
    OBJ_DESTROY, tess
    destroy_tess = 0b

;** Close files **
        
    CLOSE, shx_lun, shp_lun
    FREE_LUN, shx_lun, shp_lun
    shx_lun = -1
    shp_lun = -1


    RETURN

;** Error handling **

    outa_here :

       if ( destroy_obj ) then OBJ_DESTROY, shape_file
       if ( destroy_tess) then OBJ_DESTROY, tess
       if shx_lun ne -1 then $
       begin
          CLOSE, shx_lun
          FREE_LUN, shx_lun
       endif
       if shp_lun ne -1 then $
       begin
          CLOSE, shp_lun
          FREE_LUN, shp_lun
       endif
       if sbx_lun ne -1 then $
       begin
          CLOSE, sbx_lun
          FREE_LUN, sbx_lun
       endif
       if sbn_lun ne -1 then $
       begin
          CLOSE, sbn_lun
          FREE_LUN, sbn_lun
       endif
 
       RETURN

end
