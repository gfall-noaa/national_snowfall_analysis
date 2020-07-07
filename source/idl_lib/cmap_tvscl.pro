PRO CMAP_TVSCL, $
    grid, $
    dataBinEdges, $
    red, grn, blu, $
    xllPos, yllPos, $
    SHOW_LOW = showLow, $
    SHOW_HIGH = showHigh, $
    NDV = ndv, $
    NO_DATA_RGB = no_data_rgb

  if NOT(KEYWORD_SET(ndv)) then ndv = MIN(grid) - 1

  numColors = N_ELEMENTS(dataBinEdges) - 1
  if ((N_ELEMENTS(red) ne numColors) or $
      (N_ELEMENTS(grn) ne numColors) or $
      (N_ELEMENTS(blu) ne numColors)) then $
          MESSAGE, 'RGB arrays must have one element less than ' + $
                   'bin-edges array.'

  if (!D.Table_Size lt numColors) then $
      MESSAGE, 'Insufficient elements available for color tables.'

  foo = SIZE(grid)
  if (foo[0] ne 2) then MESSAGE, 'Input grid must be 2-D.'
  nc = foo[1]
  nr = foo[2]

  TVLCT, red_, grn_, blu_, /GET

  red_[0] = 0
  grn_[0] = 0
  blu_[0] = 0

  red_[!D.Table_Size - 1] = 255
  grn_[!D.Table_Size - 1] = 255
  blu_[!D.Table_Size - 1] = 255

  red_[1:numColors] = red
  grn_[1:numColors] = grn
  blu_[1:numColors] = blu

  if KEYWORD_SET(no_data_rgb) then begin
      red_[!D.Table_Size - 2] = no_data_rgb[0]
      grn_[!D.Table_Size - 2] = no_data_rgb[1]
      blu_[!D.Table_Size - 2] = no_data_rgb[2]
  endif

  TVLCT, red_, grn_, blu_

  byteGrid = BYTARR(nc, nr)
  byteGrid[*,*] = !D.Table_Size - 2

  for cc = 1, numColors do begin

      ind = WHERE((grid ne ndv) and $
                  (grid ge dataBinEdges[cc-1]) and $
                  (grid lt dataBinEdges[cc]), count)
      if (count eq 0) then CONTINUE

      byteGrid[ind] = cc

  endfor

  if KEYWORD_SET(showLow) then begin
      ind = WHERE((grid ne ndv) and (grid lt dataBinEdges[0]), count)
      if (count gt 0) then byteGrid[ind] = 1
  endif

  if KEYWORD_SET(showHigh) then begin
      ind = WHERE((grid ne ndv) and (grid ge dataBinEdges[numColors]), count)
      if (count gt 0) then byteGrid[ind] = numColors
  endif

  if (ARG_PRESENT(xllPos) and ARG_PRESENT(yllPos)) then $
      TV, byteGrid, xllPos, yllPos $
  else $
      TV, byteGrid

end
