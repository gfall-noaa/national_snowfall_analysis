;******************************************************************************
;*
;*  JULIAN_TO_YYYYMMDDHHMMSS
;*
;*  This function converts from julian date to a 14-character string
;*
;******************************************************************************

FUNCTION JULIAN_TO_YYYYMMDDHHMMSS, dateTime_Julian

    dateTime_Julian = DOUBLE ( dateTime_Julian )

    CALTIME, dateTime_Julian, month, day, year, hour, minute, second

    numDates = N_ELEMENTS ( dateTime_Julian )

    dateTime_YYYYMMDDHHMMSS = STRARR ( numDates )

    dc = 0L

    while ( dc lt numDates ) do begin

        dateTime_YYYYMMDDHHMMSS[dc] = $
          STRING ( FORMAT = '(%"%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d")', $
                   year[dc], month[dc], day[dc], hour[dc], $
                   minute[dc], second[dc] )

        dc++

    endwhile

    if ( numDates eq 1 ) then $
      dateTime_YYYYMMDDHHMMSS = dateTime_YYYYMMDDHHMMSS[0]

    RETURN, dateTime_YYYYMMDDHHMMSS

END
