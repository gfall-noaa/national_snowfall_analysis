;**************************************************************************
;* 
;* FORMAT_VALUE
;*
;* The function displays a maximum number of decimal digits, modified by
;* the number of significant digits.
;*
;* (Value) is the value to be displayed
;* (Type) is the type (for integer or floating point)
;* (Digits) is the number of digits to display
;* (Trace_type) is non-zero for "trace", otherwise "T"
;*
;* Anders Nilsson, VV, 11/18/02, Created.
;* Anders Nilsson, VV, 11/30/04, Revamped significant digit logic
;*                               Added 'trace'
;* Anders Nilsson, VV, 01/20/05, Sets float digits to 1 if float type and
;*                               no digits
;* Anders Nilsson, VV, 03/09/05, Added optional trace_type parameter for
;*                               'trace' instead of 'T'
;*                               Added max_float
;* Anders Nilsson, VV, 04/27/05, Added zero
;*
;************************************************************************

FUNCTION FORMAT_VALUE, value, type, length, digits, trace_type, max_float, $
                       ZERO=zero

    float_digits = 0
    trace = 0b
    if ( n_elements ( trace_type ) eq 0 ) then trace_type = 0b
    if ( n_elements ( max_float ) eq 0 ) then max_float = length > 0
    if ( n_elements ( zero ) eq 0 ) then zero = 0b

;** Determine significant digits **

    a_value = ABS ( value )

    if ( a_value eq 0 ) then $
    begin
       val_length = 1
       if ( zero ) then float_digits = 0 $
       else $
       begin
          float_digits = digits < max_float
          if ( type eq 4 or type eq 5 ) then $
          val_length = val_length + 1 + digits
       endelse
    endif $
    else if ( a_value lt 1.0 ) then $
    begin
       val_length = ( CEIL ( ABS ( ALOG10 ( a_value ) ) ) + 2 ) < length - 2
       float_digits = ( ( val_length - 2 ) > digits ) < max_float
       val_length = float_digits + 2
       if ( type ne 4 and type ne 5 ) then val_length = 1
    endif $
    else if ( a_value ge 1.0 ) then $
    begin
       val_length = FLOOR ( ABS ( ALOG10 ( a_value ) ) ) + 1
       float_digits = ( ( ( length - val_length - 1 ) < $
                          ( digits - val_length ) ) > 0 ) < max_float
       if ( type eq 4 or type eq 5 ) then $
       begin
          if ( digits eq 0 and float_digits eq 0 ) then float_digits = 1 
          if ( float_digits gt 0 ) then $
          val_length = val_length + 1 + float_digits
       endif
    endif

;** Round off **

    new_value = DOUBLE ( ROUND ( value * 10^float_digits ) ) / $
                ( 10.0^float_digits )

;** Detect trace **

    if ( new_value eq 0.0 and value ne 0.0 and $
         ( type eq 4 or type eq 5 ) ) then $
    begin 
       if ( trace_type eq 0 ) then trace_string = 'T' $
       else trace_string = 'trace'

       if ( value lt 0 ) then new_value = '-' + trace_string $
       else new_value = trace_string
       val_length = STRLEN ( new_value ) 
       trace = 1b
    endif

    if ( value lt 0 ) then val_length = val_length + 1
    t_length = val_length > length
    white_space = t_length - val_length

;** Approximately double whitespace to compensate for non-monospace fonts **

    t_length = t_length + ROUND ( 0.50 * white_space ) ;
  
;** Create format **

    if ( trace ) then $ 
    begin
       format = '(%"' + STRING ( FORMAT='(%"%%%ds")', t_length ) + '")' 
    endif $
    else if ( ( type ne 4 and type ne 5 ) or float_digits eq 0 ) then $
    begin
       format = '(%"' + STRING ( FORMAT='(%"%%%dd")', t_length ) + '")'
    endif $
    else $
    begin
       format = '(%"' + STRING ( FORMAT='(%"%%%d.%df")', t_length, $
                                 float_digits ) + '")'
    endelse

;** Convert to String **

    RETURN, STRING ( FORMAT=format, new_value )       

END
