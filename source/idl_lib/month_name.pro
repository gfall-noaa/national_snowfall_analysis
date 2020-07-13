FUNCTION MONTH_NAME, oneToTwelve

  if (NOT(ISA(oneToTwelve, 'BYTE')) and $
      NOT(ISA(oneToTwelve, 'INT')) and $
      NOT(ISA(oneToTwelve, 'UINT')) and $
      NOT(ISA(oneToTwelve, 'LONG')) and $
      NOT(ISA(oneToTwelve, 'ULONG')) and $
      NOT(ISA(oneToTwelve, 'LONG64')) and $
      NOT(ISA(oneToTwelve, 'ULONG64')) and $
      NOT(ISA(oneToTwelve, 'LONG')) and $
      NOT(ISA(oneToTwelve, 'LONG')) and $
      NOT(ISA(oneToTwelve, 'LONG'))) then $
      MESSAGE, 'Argument must be an integer.'

  if ((oneToTwelve lt 1) or (oneToTwelve gt 12)) then $
      MESSAGE, 'Argument must be a number from 1 to 12.'

  case oneToTwelve of
      1: monthName = 'January'
      2: monthName = 'February'
      3: monthName = 'March'
      4: monthName = 'April'
      5: monthName = 'May'
      6: monthName = 'June'
      7: monthName = 'July'
      8: monthName = 'August'
      9: monthName = 'September'
      10: monthName = 'October'
      11: monthName = 'November'
      12: monthName = 'December'
  endcase

  RETURN, monthName

end
