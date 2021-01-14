PRO DRAW_ERROR_BARS, x, y, sigma, $
                     COLOR = color, LINESTYLE = lineStyle

  if (NOT(KEYWORD_SET(color)) and NOT(ARG_PRESENT(color))) then $
      color = 255
  if (NOT(KEYWORD_SET(lineStyle)) and NOT(ARG_PRESENT(lineStyle))) then $
      lineStyle = 0

  numPoints = N_ELEMENTS(x)
  if ((N_ELEMENTS(y) ne numPoints) or $
      (N_ELEMENTS(sigma) ne numPoints)) then begin
      MESSAGE, 'Inconsistent x, y, sigma array sizes.'
  endif

  bracketSize = 0.01 * (!X.CRange[1] - !X.CRange[0])
  for i = 0, numPoints - 1 do begin
      PLOTS, [x[i], x[i]], [y[i] - sigma[i], y[i] + sigma[i]], $
             LINESTYLE = lineStyle, COLOR = color
      PLOTS, [x[i] - 0.5 * bracketSize, x[i] + 0.5 * bracketSize], $
             [y[i] + sigma[i], y[i] + sigma[i]], $
             LINESTYLE = lineStyle, COLOR = color
      PLOTS, [x[i] - 0.5 * bracketSize, x[i] + 0.5 * bracketSize], $
             [y[i] - sigma[i], y[i] - sigma[i]], $
             LINESTYLE = lineStyle, COLOR = color
  endfor

  RETURN

end

