;******************************************************************************
;*
;*  JULIAN_TO_YYYMMDDHH
;*
;*  This function converts from julian date to a 10-character string
;*
;*  History:
;*  Anders Nilsson, VV, 08/03/04, Replaced CALDAT with CALTIME
;*
;*  2007-11-01, GF - Modified to handle arrays of Julian dates.
;*
;******************************************************************************

FUNCTION JULIAN_TO_YYYYMMDDHH, dateTime_Julian

    dateTime_Julian = DOUBLE ( dateTime_Julian )

    CALTIME, dateTime_Julian, month, day, year, hour

    numDates = N_ELEMENTS ( dateTime_Julian )

    dateTime_YYYYMMDDHH = STRARR ( numDates )

    dc = 0L

    while ( dc lt numDates ) do begin

        dateTime_YYYYMMDDHH[dc] = $
          STRING ( FORMAT = '(%"%4.4d%2.2d%2.2d%2.2d")', $
                   year[dc], month[dc], day[dc], hour[dc] )

        dc++

    endwhile

    if ( numDates eq 1 ) then dateTime_YYYYMMDDHH = dateTime_YYYYMMDDHH[0]

    RETURN, dateTime_YYYYMMDDHH

END
