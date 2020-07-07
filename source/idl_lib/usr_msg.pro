PRO USR_MSG, text

; Send an informational message to stdout or, if there is no stdout, to a
; message buffer.

  COMMON info, Message
  stdOutStat = FSTAT(-1)
  if stdOutStat.isatty then $
    PRINT, text $
  else $
    STRING_APPEND, Message, text, /WITH_CR
  RETURN
end
