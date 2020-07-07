PRO GF_COLORBAR, posXYXY, $ ; position: x1, y1, x2, y2
                 val, $ ; values to display
                 rgbInd, $ ; indices of rgb table to display
                 outlineInd, $ ; color of outline
                 TITLE = title, $
                 SIZE_FACTOR = sizeFactor, $
                 WEIGHT_FACTOR = weightFactor, $
                 VERTICAL = vertical, $
                 SHOW_LOW = showLow, $
                 SHOW_HIGH = showHigh, $
                 TICK_COLOR = tickColor, $
                 TICK_FLAGS = tickFlag, $
                 TICK_NAMES = tickName, $
                 FONT = font ; -1 = Hershey, 0 = device font, 1 = TrueType

  if NOT ( KEYWORD_SET ( sizeFactor ) ) then sizeFactor = 1.0
  if NOT ( KEYWORD_SET ( weightFactor ) ) then weightFactor = 1.0

  numColors = N_ELEMENTS ( rgbInd )

  if ( N_ELEMENTS ( val ) ne numColors + 1 ) then $
    MESSAGE, 'Data array size must exceed RGB array size by one.'

  if KEYWORD_SET ( vertical ) then begin
      boxHeight = ( posXYXY[3] - posXYXY[1] ) / numColors
      x1 = posXYXY[0]
      x2 = posXYXY[2]
  endif else begin
      boxWidth = ( posXYXY[2] - posXYXY[0] ) / numColors
      y1 = posXYXY[1]
      y2 = posXYXY[3]
  endelse

  for cc = 0, numColors - 1 do begin

      if KEYWORD_SET ( vertical ) then begin
          y1 = posXYXY[1] + cc * boxHeight
          y2 = y1 + boxHeight
      endif else begin
          x1 = posXYXY[0] + cc * boxWidth
          x2 = x1 + boxWidth
      endelse

      POLYFILL, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /NORMAL, $
                COLOR = rgbInd[cc]
;      PLOTS, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /NORMAL, $
;                COLOR = outlineInd

;      if KEYWORD_SET ( vertical ) then begin
;          XYOUTS, x2 + 0.01, y1 - 0.01, $
;                  FORMAT_FLOAT ( val[cc] ), $
;                  ALIGNMENT = 0.0, /NORMAL, COLOR = textInd, $
;                  SIZE = 0.75 * sizeFactor, $
;                  CHARTHICK = sizeFactor
;      endif else begin
;          XYOUTS, x1, y1 - 0.02, $
;                  FORMAT_FLOAT ( val[cc] ), $
;                  ALIGNMENT = 0.5, /NORMAL, COLOR = textInd, $
;                  SIZE = 0.75 * sizeFactor, $
;                  CHARTHICK = sizeFactor
;      endelse

  endfor

;  if KEYWORD_SET ( vertical ) then begin
;      XYOUTS, x2 + 0.01, y2 - 0.01, $
;              FORMAT_FLOAT ( val[cc] ), $
;              ALIGNMENT = 0.0, /NORMAL, COLOR = textInd, $
;              SIZE = 0.75 * sizeFactor, $
;              CHARTHICK = sizeFactor
;  endif else begin
;      XYOUTS, x2, y1 - 0.022, $
;              FORMAT_FLOAT ( val[cc] ), $
;              ALIGNMENT = 0.5, /NORMAL, COLOR = textInd, $
;              SIZE = 0.75 * sizeFactor, $
;              CHARTHICK = sizeFactor
;  endelse

  x1 = posXYXY[0]
  x2 = posXYXY[2]
  y1 = posXYXY[1]
  y2 = posXYXY[3]

; Moved this down to colorbar potting section.
;  PLOTS, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /NORMAL, $
;         COLOR = outlineInd

  if (N_ELEMENTS(title) eq 0) then title = ''
;   if (N_ELEMENTS(title) ne 0) then begin
;       XYOUTS, 0.5 * ( x1 + x2 ), y1 - 0.048, title, $
;               ALIGNMENT = 0.5, /NORMAL, COLOR = textInd, $
;               SIZE = 1.5 * sizeFactor, $
;               CHARTHICK = 1.0 * weightFactor
;   endif

; Here is how tick marks and labels work by default;

;   - Every color is set off by tick marks
;   - Every tick mark gets a label, but you can control those by
;     using the TICK_NAMES keyword, and by setting some of that
;     array of tick names (which must be the # colors + 1 in length)
;     to ''.
;   - The axis that is plotted is actually values of color index;
;     i.e., INDGEN(numColors + 1) - not the data values those colors
;     represent.
;   - You can avoid drawing a tick for every color by including the
;     TICK_FLAGS keyword. That would be an array of ones and zeroes of
;     same size as TICK_NAMES (i.e., numColors + 1).

  if KEYWORD_SET(tickName) then begin
     if (N_ELEMENTS(tickName) ne (numColors + 1)) then $
        MESSAGE, 'Array of tick names must have ' + $
                 STRCOMPRESS(numColors + 1, /REMOVE_ALL) + ' elements.'
  endif else begin
     tickName = FORMAT_FLOAT(val) ; numColors + 1 of these
     if KEYWORD_SET(showLow) then begin
        ;tickName[0] = '<' + tickName[0]
        tickName[0] = '<' + tickName[1]
     endif

     if KEYWORD_SET(showHigh) then begin
        ;tickName[numColors] = tickName[numColors] + '+'
        tickName[numColors] = '>=' + tickName[numColors - 1]
     endif
  endelse

  if KEYWORD_SET(tickFlag) then begin
      if (N_ELEMENTS(tickFlag) ne (numColors + 1)) then $
          MESSAGE, 'TICK_FLAGS array must have ' + $
                   STRCRA(numColors + 1) + ' elements.'
      ind = WHERE((tickFlag ne 0) and (tickFlag ne 1), count)
      if (count gt 0) then $
          MESSAGE, 'TICK_FLAGS must consist only of ones and zeroes.'
  endif else begin
      tickFlag = REPLICATE(1B, numColors + 1)
  endelse

  axisTickV = INDGEN(numColors + 1)
  ind = WHERE(tickFlag eq 1, axisTicks)
  if (axisTicks lt 2) then $
      MESSAGE, 'AXIS_FLAGS must keep at least 2 tick marks.'
  axisTicks = axisTicks - 1
  axisTickV = axisTickV[ind]
  axisTickName = tickName[ind]

  if NOT(KEYWORD_SET(tickColor)) then $
      tickColor = outlineInd

  PLOTS, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /NORMAL, $
         COLOR = tickColor

  if KEYWORD_SET ( vertical ) then begin
      PLOT, REPLICATE ( 0, numColors + 1 ), INDGEN ( numColors + 1 ), $
            POS = posxYXY, $
            /NODATA, /NOERASE, XSTYLE = 5, YSTYLE = 5
;     This call to AXIS only draws the axis line and tick marks.
      AXIS, YAXIS = 1, $
            YTICKLEN = 1, $
            YTICKS = axisTicks, $
            YTICKV = axisTickV, $
            YTICKNAME = REPLICATE(' ', axisTicks + 1), $
            COLOR = tickColor
            ;CHARSIZE = sizeFactor, $
            ;CHARTHICK = ROUND(weightFactor) > 1, $
            ;YTITLE = title, $
            ;FONT = font
;     Generate a base plot that extends one "color" above and below
;     the colorbar. This way we can use the AXIS procedure to annotate
;     the axis without the first and last text values being shifted
;     relative to the others.
      y1Pad = y1 - boxHeight
      y2Pad = y2 + boxHeight
      PLOT, REPLICATE(0, numColors + 3), INDGEN(numColors + 3) - 1, $
            POS = [x1, y1Pad, x2, y2Pad], $
            /NODATA, /NOERASE, XSTYLE = 5, YSTYLE = 5
      AXIS, YAXIS = 1, $
            YTICKS = axisTicks, $
            YTICKV = axisTickV, $
            YTICKNAME = axisTickName, $
            COLOR = tickColor, $
            CHARSIZE = sizeFactor, $
            CHARTHICK = ROUND(weightFactor) > 1, $
            YTITLE = title, $
            FONT = font, $
            YTICKLAYOUT = 1 ; labels only, no axis line or tick marks
  endif else begin
      PLOT, INDGEN ( numColors + 1 ), REPLICATE ( 0, numColors + 1 ), $
            POS = posXYXY, $
            /NODATA, /NOERASE, XSTYLE = 5, YSTYLE = 5
      AXIS, XAXIS = 0, $
            XTICKLEN = 1, $
            XTICKS = axisTicks, $
            XTICKV = axisTickV, $
            XTICKNAME = REPLICATE(' ', axisTicks + 1), $
            COLOR = tickColor
            ;CHARSIZE = sizeFactor, $
            ;CHARTHICK = ROUND(weightFactor) > 1, $
            ;XTITLE = title, $
            ;FONT = font
;     Generate a base plot that extends one "color" left and right of
;     the colorbar. This way we can use the AXIS procedure to annotate
;     the axis without the first and last text values being shifted
;     relative to the others.
      x1Pad = x1 - boxWidth
      x2Pad = x2 + boxWidth
      PLOT, INDGEN(numColors + 3) - 1, REPLICATE(0, numColors + 3), $
            POS = [x1Pad, y1, x2Pad, y2], $
            /NODATA, /NOERASE, XSTYLE = 5, YSTYLE = 5
      AXIS, XAXIS = 0, $
            XTICKS = axisTicks, $
            XTICKV = axisTickV, $
            XTICKNAME = axisTickName, $
            COLOR = tickColor, $
            CHARSIZE = sizeFactor, $
            CHARTHICK = ROUND(weightFactor) > 1, $
            XTITLE = title, $
            FONT = font, $
            XTICKLAYOUT = 1 ; labels only, no axis line or tick marks
  endelse

end
