FUNCTION DATA_TYPE_STR, arg

; Generate a string describing the data type of a variable.

  varType = SIZE(arg, /TYPE)
  case varType of
      0: RETURN, 'UNDEFINED'
      1: RETURN, 'BYTE'
      2: RETURN, 'INT'
      3: RETURN, 'LONG'
      4: RETURN, 'FLOAT'
      5: RETURN, 'DOUBLE'
      6: RETURN, 'COMPLEX'
      7: RETURN, 'STRING'
      8: RETURN, 'STRUCT'
      9: RETURN, 'DCOMPLEX'
     10: RETURN, 'POINTER'
     11: RETURN, 'OBJREF'
     12: RETURN, 'UINT'
     13: RETURN, 'ULONG'
     14: RETURN, 'LONG64'
     15: RETURN, 'ULONG64'
     else: RETURN, 'UNSUPPORTED TYPE'
  endcase

end
