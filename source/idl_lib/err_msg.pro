PRO ERR_MSG, text

; Send an error message to stderr or, if there is no stdout, to a message
; buffer.

  COMMON info, Message
  stdErrStat = FSTAT(-2)
  if stdErrStat.isatty then $
    MESSAGE, text, LEVEL = -1, /CONTINUE $
  else $
    STRING_APPEND, Message, text, /WITH_CR
  RETURN
end
