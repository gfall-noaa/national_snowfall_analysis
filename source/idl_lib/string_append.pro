PRO STRING_APPEND, originalString, textToAdd, WITH_CR = withCr

  if ( originalString eq '' ) then begin

      originalString = textToAdd

  endif else begin

      if KEYWORD_SET ( withCr ) then $
        originalString = originalString + STRING ( 10b ) + textToAdd $
      else $
        originalString = originalString + textToAdd

  endelse

  RETURN

end
