FUNCTION YYYYMMDDHH_TO_JULIAN, dateTime_YYYYMMDDHH

  dateTime_YYYYMMDDHH = STRCOMPRESS ( dateTime_YYYYMMDDHH, /REMOVE_ALL )

;  bad = WHERE ( STREGEX ( dateTime_YYYYMMDDHH, '[^0-9]+', /BOOLEAN ) or $
;                ( STRLEN ( dateTime_YYYYMMDDHH ) ne 10 ), count )
;
;  if ( count gt 0L ) then begin
;
;      MESSAGE, 'Invalid YYYYMMDDHH date/time of ' + dateTime_YYYYMMDDHH, $
;               /CONTINUE
;      RETURN, -1.0d
;
;  endif

  year = FIX ( STRMID ( dateTime_YYYYMMDDHH, 0, 4 ) )
  month = FIX ( STRMID ( dateTime_YYYYMMDDHH, 4, 2 ) )
  day = FIX ( STRMID ( dateTime_YYYYMMDDHH, 6, 2 ) )
  hour = FIX ( STRMID ( dateTime_YYYYMMDDHH, 8, 2 ) )

  RETURN, JULDAY ( month, day, year, hour, 0, 0 )

end
