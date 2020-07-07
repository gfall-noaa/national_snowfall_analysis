;******************************************************************************
;*
;*  JULIAN_TO_GISRS_DATE
;*
;*  This function converts from julian date to a 13-character string
;*
;*  History:
;*  Anders Nilsson, VV, 08/03/04, Replaced CALDAT with CALTIME
;*
;******************************************************************************

FUNCTION JULIAN_TO_GISRS_DATE, dateTime_Julian, UNDERSCORE=underscore

    dateTime_Julian = DOUBLE ( dateTime_Julian )

    CALTIME, dateTime_Julian, month, day, year, hour

    if ( n_elements ( underscore ) ne 0 ) then $
    begin

       RETURN, STRING ( FORMAT='(%"%4.4d_%2.2d_%2.2d_%2.2d")', $
                        year, month, day, hour )
    endif $
    else $
    begin

       RETURN, STRING ( FORMAT='(%"%4.4d-%2.2d-%2.2d %2.2d")', $
                        year, month, day, hour )
    endelse

END
