FUNCTION FORMAT_FLOAT, valFloat


; Format a floating point value to show the fewest number of trailing zeroes
; possible.  Useful for formatting floats for plot scales where space is
; limited.


; *** OLD VERSION - HAS BUGS ***

;  case 1 of

;      ( ABS ( valFloat ) lt 1.0e-4 ) : $
;        valStr = '0'

;      ( ABS ( valFloat ) lt 1.0e-3 ) : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(e7.0e2)' ), $
;                               /REMOVE_ALL )

;      ( ABS ( valFloat ) lt 1.0e-2 ) : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(e7.0e2)' ), $
;                               /REMOVE_ALL )

;      ( ABS ( valFloat ) lt 1.0e-1 ) : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(f5.2)' ), $
;                               /REMOVE_ALL )

;      ( ABS ( valFloat ) ge 1000 ) : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(f6.0)' ), $
;                               /REMOVE_ALL )

;      ( ABS ( valFloat ) ge 100 ) : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(f5.0)' ), $
;                               /REMOVE_ALL )

;      ( ABS ( valFloat ) ge 10 ) : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(f4.0)' ), $
;                               /REMOVE_ALL )

;      ( ABS ( valFloat ) ge 1 ) : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(f4.1)' ), $
;                               /REMOVE_ALL )

;      else : $
;        valStr = STRCOMPRESS ( STRING ( valFloat, FORMAT = '(f10.2)' ), $
;                             /REMOVE_ALL )

;  endcase

;  TRIM_FLOAT_STR, valStr

  n = N_ELEMENTS ( valFloat )

  valStr = STRARR ( n )

  for i = 0, n - 1 do begin

      valStr[i] = $
        STRCOMPRESS ( STRING ( valFloat[i], FORMAT = '(f10.3)' ), /REMOVE_ALL )

      if ( ABS ( valFloat[i] ) gt 0.0 ) and $
               ( ABS ( valFloat[i] ) lt 1.0e-2 ) then begin

          ;; valStr[i] = $
          ;;   STRCOMPRESS ( STRING ( valFloat[i], FORMAT = '(e8.2)' ), $
          ;;                 /REMOVE_ALL )
          valStr[i] = $
            STRCOMPRESS ( STRING ( valFloat[i], FORMAT = '(e10.3)' ), $
                          /REMOVE_ALL )


;         Remove leading zeroes in exponent.

          while ( STRPOS ( valStr[i], 'e-0' ) ne -1 ) do begin

              pos = STRPOS ( valStr[i], 'e-0' ) + 2

              valStr[i] = STRMID ( valStr[i], 0, pos ) + $
                       STRMID ( valStr[i], pos + 1, $
                                STRLEN ( valStr[i] ) - pos - 1 )

          endwhile

      endif

      if ( ABS ( valFloat[i] ) ge 1 ) then $
        valStr[i] = STRCOMPRESS ( STRING ( valFloat[i], FORMAT = '(f10.2)' ), $
                                  /REMOVE_ALL )

      if ( ABS ( valFloat[i] ) ge 10 ) then $
        valStr[i] = STRCOMPRESS ( STRING ( valFloat[i], FORMAT = '(f10.1)' ), $
                               /REMOVE_ALL )

      if ( ABS ( valFloat[i] ) ge 100 ) then $
        valStr[i] = STRCOMPRESS ( STRING ( valFloat[i], FORMAT = '(f10.0)' ), $
                               /REMOVE_ALL )

      if ( ABS ( valFloat[i] ) ge 1000 ) then begin

          valStr[i] = $
            STRCOMPRESS ( STRLOWCASE ( STRING ( valFloat[i], $
                                                FORMAT = '(g10.2)' ) ), $
                          /REMOVE_ALL )

;         Remove "+" for exponent.

          pos = STRPOS ( valStr[i], 'e+' ) + 1

          if ( pos ne -1 ) then $
            valStr[i] = STRMID ( valStr[i], 0, pos ) + $
                        STRMID ( valStr[i], pos + 1, $
                                 STRLEN ( valStr[i] ) - pos - 1 )


;         Remove leading zeroes in exponent.

          while ( STRPOS ( valStr[i], 'e0' ) ne -1 ) do begin

              pos = STRPOS ( valStr[i], 'e0' ) + 1

              valStr[i] = STRMID ( valStr[i], 0, pos ) + $
                       STRMID ( valStr[i], pos + 1, $
                                STRLEN ( valStr[i] ) - pos - 1 )

          endwhile

      endif


;     Remove zeroes after a decimal point that immediately precede "e.

      while ( STREGEX ( valStr[i], '\..*0e' ) ne -1 ) do begin

          pos = STRPOS ( valStr[i], '0e' )

          valStr[i] = STRMID ( valStr[i], 0, pos ) + $
                      STRMID ( valStr[i], pos + 1, $
                               STRLEN ( valStr[i] - pos - 1 ) )

      endwhile


;     Remove the "." in any ".e" remainning.

      while ( STRPOS ( valStr[i], '.e' ) ne -1 ) do begin

          pos = STRPOS ( valStr[i], '.e' )

          valStr[i] = STRMID ( valStr[i], 0, pos ) + $
                      STRMID ( valStr[i], pos + 1, $
                               STRLEN ( valStr[i] - pos - 1 ) )

      endwhile

      while ( ( ( STRMID ( valStr[i], $
                           STRLEN ( valStr[i] ) - 1, 1 ) eq '0' ) or $
                ( STRMID ( valStr[i], $
                           STRLEN ( valStr[i] ) - 1, 1 ) eq '.' ) ) and $
              ( STRPOS ( valStr[i], '.' ) le $
                ( STRLEN ( valStr[i] ) - 1 ) ) and $
              ( STRPOS ( valStr[i], '.' ) ne -1 ) and $
              ( STRPOS ( valStr[i], 'e' ) eq -1 ) ) do begin

          valStr[i] = STRMID ( valStr[i], 0, STRLEN ( valStr[i] ) - 1 )

      endwhile

  endfor

  if ( n eq 1 ) then valStr = valStr[0]
  RETURN, valStr

end
