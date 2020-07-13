FUNCTION REGRID_BILIN, GridIn, $ ; [nColsIn, nRowsIn]
                       IGrid, $  ; i (column) and j (row) indices of
                       JGrid, $  ; output data relative to input grid
                       NoDataValue, $    ; no-data value (both input and output)
                       GENEROUS = generous, $
                       NHOOD_MIN = nMinRegridOut, $
                       NHOOD_MAX = nMaxRegridOut, $
                       AUDIT = audit
;_
; Use bilinear interpolation to sample data from GridIn to column and row
; locations provided by IGrid and JGrid.
;-

  COMMON info, message ; used by USR_MSG and ERR_MSG

;+
; Initialize the output grid to !NULL. The caller should use ISA on
; the output grid to judge the success/failure of this function.
;-
  regridOut = !NULL

;+
; Verify that input data are gridded; get dimensions.
;-
  gridSize = SIZE(GridIn)
  if (gridSize[0] ne 2) then begin ; input data must be gridded
      ERR_MSG, 'Input grid must be a 2-dimensional array.'
      RETURN, regridOut
  endif
  nColsIn = gridSize[1]
  nRowsIn = gridSize[2]
  gridInType = gridSize[3]

;+
; Verify that IGrid and JGrid are the correct type and have matching
; dimensions. They do NOT need to be 2-D arrays.
;-
  iGridSize = SIZE(IGrid)
  if (iGridSize[0] lt 1) then begin
      ERR_MSG, 'Input i and j indices must be arrays.'
      RETURN, regridOut
  endif
  jGridSize = SIZE(JGrid)
  if (jGridSize[0] ne iGridSize[0]) then begin
      ERR_MSG, 'Input i and j index arrays do not match.'
      RETURN, regridOut
  endif
  for dc = 1, iGridSize[0] do begin ; dims of IGrid and JGrid
      if (jGridSize[dc] ne iGridSize[dc]) then begin
          ERR_MSG, 'Input i and j index array dimensions do not match.'
          RETURN, regridOut
      endif
  endfor

;+
; Create DOUBLE versions of IGrid and JGrid named iGrid_ and jGrid_.
;-
  if ((iGridSize[iGridSize[0] + 1] ne 5) or $
      (jGridSize[jGridSize[0] + 1] ne 5)) then begin
      ERR_MSG, 'WARNING: input i and j index arrays should be type DOUBLE.'
      iGrid_ = DOUBLE(IGrid)
      jGrid_ = DOUBLE(JGrid)
  endif else begin
      iGrid_ = IGrid
      jGrid_ = JGrid
  endelse

;+
; Make sure the no-data value is a scalar of the same data type as
; dataGrid.
;-
  ndvSize = SIZE(NoDataValue)
  if (ndvSize[0] ne 0) then begin
      ERR_MSG, 'No-data value must be a scalar.'
      RETURN, gridOut
  endif
  ndvType = ndvSize[1]
  if (ndvType ne gridInType) then begin
      ERR_MSG, 'The type of the no-data value (' + TYPENAME(NoDataValue) + $
               ') does not match that of the input grid (' + $
               TYPENAME(GridIn) + ').'
      RETURN, gridOut
  endif

  if KEYWORD_SET(generous) then begin

;+
;     Add a one row/column border to the data before indexing so that
;     edge values are not eliminated. This "frame" will be removed
;     after interpolation.
;-
      gridIn_ = MAKE_ARRAY(nColsIn + 2, nRowsIn + 2, TYPE = gridIntype, $
                           VALUE = NoDataValue)
      gridIn_[1:nColsIn, 1:nRowsIn] = GridIn
      iGrid_ = iGrid_ + 1.0D
      jGrid_ = jGrid_ + 1.0D

  endif else begin

      gridIn_ = GridIn

  endelse

;+
; Regrid data from the input grid/CRS to the output grid/CRS.
;-
  i1Grid = FLOOR(iGrid_)
  i2Grid = i1Grid + 1L
  j1Grid = FLOOR(jGrid_)
  j2Grid = j1Grid + 1L

;+
; Index in-bounds row/column indices.
;-
  if KEYWORD_SET(generous) then begin
      ind = WHERE((i1Grid ge 0) and $
                  (i2Grid lt (nColsIn + 2)) and $
                  (j1Grid ge 0) and $
                  (j2Grid lt (nRowsIn + 2)), $
                  count)
  endif else begin
      ind = WHERE((i1Grid ge 0) and $
                  (i2Grid lt nColsIn) and $
                  (j1Grid ge 0) and $
                  (j2Grid lt nRowsIn), $
                  count)
  endelse

  if (count eq 0) then begin
      ERR_MSG, 'WARNING: all output grid locations are out of bounds.'
      RETURN, regridOut
  endif

;+
; Initialize outputs.
;-
;  regridOut = FLOAT(IGrid) & regridOut[*] = NoDataValue
  regridOut_ = MAKE_ARRAY(iGridSize[1:iGridSize[0]], TYPE = gridInType, $
                          VALUE = NoDataValue)
  if ARG_PRESENT(nMinRegridOut) then nMinRegridOut_ = regridOut_
  if ARG_PRESENT(nMaxRegridOut) then nMaxRegridOut_ = regridOut_

;+
; Sample input grid at adjacent cells for bilinear interpolation.
;-
  gll = gridIn_[i1Grid[ind], j1Grid[ind]]
  glr = gridIn_[i2Grid[ind], j1Grid[ind]]
  gur = gridIn_[i2Grid[ind], j2Grid[ind]]
  gul = gridIn_[i1Grid[ind], j2Grid[ind]]

;+
; Compute 1-D weights for bilinear interpolation.
;-
  diLeft = iGrid_[ind] - i1Grid[ind]
  diRight = i2Grid[ind] - iGrid_[ind]
  djBot = jGrid_[ind] - j1Grid[ind]
  djTop = j2Grid[ind] - jGrid_[ind]

  i1Grid = !NULL & i2Grid = !NULL
  j1Grid = !NULL & j2Grid = !NULL

;+
; Perform interpolation.
;-
  if KEYWORD_SET(generous) then begin

;+
;     For the GENEROUS case, weights are calculated explicitly and set
;     to zero where their corresponding grid values equal
;     NoDataValue. Consequently, the weights cannot be assumed to be
;     normalized, and explicit division by their total is
;     necessary. No explicit setting of NoDataValue is needed, since
;     regridOut_ is initialized to that value.
;-
      weight_ll = diRight * djTop * (gll ne NoDataValue)
      weight_lr = diLeft * djTop * (glr ne NoDataValue)
      weight_ur = diLeft * djBot * (gur ne NoDataValue)
      weight_ul = diRight * djBot * (gul ne NoDataValue)

      weight_total = weight_ll + weight_lr + weight_ur + weight_ul

      ind2 = WHERE(weight_total gt 0.0, count2)

      if (count2 gt 0) then begin

          regridOut_[ind[ind2]] = (weight_ll[ind2] * gll[ind2] + $
                                   weight_lr[ind2] * glr[ind2] + $
                                   weight_ur[ind2] * gur[ind2] + $
                                   weight_ul[ind2] * gul[ind2]) / $
                                  weight_total[ind2]

          weight_ll = !NULL
          weight_lr = !NULL
          weight_ur = !NULL
          weight_ul = !NULL
          weight_total = !NULL

          if (ARG_PRESENT(nMinRegridOut) or ARG_PRESENT(nMaxRegridOut)) $
            then begin

;+
;             Get minimum and maximum values of the input grid, then
;             exceed them (variables aboveMax and belowMin). These
;             numbers do not contribute directly to the results, they
;             are used to manipulate the < and > operators to
;             eliminate no-data values from neighborhood min/max
;             calculations.
;-
              ind3 = WHERE(gridIn_ ne NoDataValue, count3)
              if (count3 eq 0) then STOP ; PROGRAMMING ERROR
              maxValue = MAX(gridIn_[ind3])
              minValue = MIN(gridIn_[ind3])
              aboveMax = maxValue + ABS(maxValue)
              belowMin = minValue - ABS(minValue)
              ind3 = !NULL

          endif

;+
;         Sample min/max neighborhood values if necessary, excluding
;         no-data values by being clever.
;-
          if ARG_PRESENT(nMinRegridOut) then $
              nMinRegridOut_[ind[ind2]] = $
                  (gll[ind2] * (gll[ind2] ne NoDataValue) + $
                   aboveMax * (gll[ind2] eq NoDataValue)) < $
                  (glr[ind2] * (glr[ind2] ne NoDataValue) + $
                   aboveMax * (glr[ind2] eq NoDataValue)) < $
                  (gur[ind2] * (gur[ind2] ne NoDataValue) + $
                   aboveMax * (gur[ind2] eq NoDataValue)) < $
                  (gul[ind2] * (gul[ind2] ne NoDataValue) + $
                   aboveMax * (gul[ind2] eq NoDataValue))

          if ARG_PRESENT(nMaxRegridOut) then $
              nMaxRegridOut_[ind[ind2]] = $ 
                  (gll[ind2] * (gll[ind2] ne NoDataValue) + $
                   belowMin * (gll[ind2] eq NoDataValue)) > $
                  (glr[ind2] * (glr[ind2] ne NoDataValue) + $
                   belowMin * (glr[ind2] eq NoDataValue)) > $
                  (gur[ind2] * (gur[ind2] ne NoDataValue) + $
                   belowMin * (gur[ind2] eq NoDataValue)) > $
                  (gul[ind2] * (gul[ind2] ne NoDataValue) + $
                   belowMin * (gul[ind2] eq NoDataValue))

      endif

  endif else begin

;+
;     For the default case (no GENEROUS setting), interpolation
;     weights are calculated on the fly in a single operation, and
;     consideration of no-data values is managed by a simple index
;     (ind2) after interpolation has been performed.
;-
      regridOut_[ind] = $
          gll * diRight * djTop + $
          glr * diLeft * djTop + $
          gur * diLeft * djBot + $
          gul * diRight * djBot

;+
;     Check for input no-data values. By default, none are tolerated.
;-
      ind2 = WHERE((gll eq NoDataValue) or $
                   (glr eq NoDataValue) or $
                   (gur eq NoDataValue) or $
                   (gul eq NoDataValue), count2)
      if (count2 gt 0) then regridOut_[ind[ind2]] = NoDataValue

;+
;     Sample min/max neighborhood values if necessary.
;-
      if ARG_PRESENT(nMinRegridOut) then begin
          nMinRegridOut_[ind] = gll < glr < gur < gul
          if (count2 gt 0) then nMinRegridOut_[ind[ind2]] = NoDataValue
      endif

      if ARG_PRESENT(nMaxRegridOut) then begin
          nMaxRegridOut_[ind] = gll > glr > gur > gul
          if (count2 gt 0) then nMaxRegridOut_[ind[ind2]] = NoDataValue
      endif

  endelse

  if KEYWORD_SET(audit) then begin

      nOut = N_ELEMENTS(regridOut_)

      for k = 0, nOut - 1 do begin

          if ((k mod 10000) ne 0) then CONTINUE

          PRINT, 'k = ', k, ' / ', nOut - 1
          k2 = WHERE(ind eq k, count)
          if (count eq 0) then begin
              PRINT, 'NDV - out of bounds'
              CONTINUE
          endif

          if (count ne 1) then STOP
          k2 = k2[0]

          if ((gll[k2] eq NoDataValue) or $
              (glr[k2] eq NoDataValue) or $
              (gur[k2] eq NoDataValue) or $
              (gul[k2] eq NoDataValue)) then begin
              PRINT, 'NDV - neighborhood'
              CONTINUE
          endif

          PRINT, '  i ', IGrid[ind[k2]], ', j ', JGrid[ind[k2]]
          PRINT, '  gll ', gll[k2], ', glr ', glr[k2]
          PRINT, '  gul ', gul[k2], ', gur ', gur[k2]
          PRINT, '  left wt ', diLeft[k2]
          PRINT, '  right wt ', diRight[k2]
          PRINT, '  bottom wt ', djBot[k2]
          PRINT, '  top wt ', djTop[k2]
          PRINT, '  result ', regridOut_[ind[k2]]

      endfor

  endif

;+
; Rename regridOut_ to establish that the procedure ran successfully.
;-
  regridOut = TEMPORARY(regridOut_)

  if ARG_PRESENT(nMinRegridOut) then $
      nMinRegridOut = TEMPORARY(nMinRegridOut_)

  if ARG_PRESENT(nMaxRegridOut) then $
      nMaxRegridOuT = TEMPORARY(nMaxRegridOut_)

  RETURN, regridOut

end

