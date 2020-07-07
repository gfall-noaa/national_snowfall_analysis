PRO STR_REPLACE, rawStr, oldStr, newStr

  start = 0
  pos = 0

  while ( ( start lt STRLEN ( rawStr ) ) and ( pos ne -1 ) ) do begin

      pos = STRPOS ( rawStr, oldStr, start )

      if ( pos ne -1 ) then begin

          part1 = STRMID ( rawStr, 0, pos )
          part2 = STRMID ( rawStr, pos + STRLEN ( oldStr ), $
                           STRLEN ( rawStr ) - pos - STRLEN ( oldStr ) )
          start = pos + STRLEN ( newStr )

          rawStr = part1 + newStr + part2

      endif

  endwhile

  RETURN

end
