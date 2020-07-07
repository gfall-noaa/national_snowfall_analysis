FUNCTION SFAV2_SMOOTH_NDV_AS_ZERO, zGrid, $
                                   Ndv, $
                                   MinLat, MaxLat, $
                                   LonRes, LatRes, $
                                   SmoothHoodRadM, $
                                   DISTANCE_PRECISION = DistPrecision, $
                                   HASH = hash ;, $ GF 20170309
;GF 20170309 vv
                                   ;TREAT_NDV_AS = treatNdvAs
;GF 20170309 ^^

; Smooth no-data areas in kriging results with a neighborhood boxcar
; average, effectively treating no-data values in the boxcar subgrid
; as zeroes.

  if NOT(KEYWORD_SET(DistPrecision)) then begin
      DistPrecision = 2
  endif else begin
      if ((DistPrecision lt 0) or (DistPrecision gt 3)) then begin
          ERR_MSG, 'DISTANCE_PRECISION must be 0, 1, 2, or 3.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
      endif
  endelse

  foo = SIZE(zGrid)
  if (foo[0] ne 2) then begin
      ERR_MSG, 'Input grid must be a 2-D array.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, !NULL
  endif
  numCols = foo[1]
  numRows = foo[2]

;GF 20170309 vv
;  if NOT(KEYWORD_SET(treatNdvAs)) then treatNdvAs = 0.0
;GF 20170309 ^^

  mPerDegLat = DISTANCE(DistPrecision, $
                        0.0D, MinLat, $
                        0.0D, MinLat - 0.001D) * 1000.0D

  mPerDegLon = DISTANCE(DistPrecision, $
                        0.0D, MinLat, 1.0D, MinLat)

  smoothHoodRadCols = CEIL(SmoothHoodRadM / mPerDegLon / LonRes)
  smoothHoodRadRows = CEIL(SmoothHoodRadM / mPerDegLat / LatRes)

  ndvInd = WHERE(zGrid eq Ndv, ndvCount)

  if (ndvCount gt 0) then begin

      if KEYWORD_SET(hash) then begin
          if (ndvCount lt 50) then begin
              hashes = ndvCount
              lastProgress = -1
          endif else begin
              hashes = 50
              lastProgress = 0
          endelse
          for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
          PRINT, ''
      endif

      newZGrid = zGrid

  endif

  rcPrev = -1

  for nc = 0, ndvCount - 1 do begin

      if KEYWORD_SET(hash) then begin
          progress = FIX(FLOAT(nc) / FLOAT(ndvCount - 1) * $
                         FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      rc = ndvInd[nc] / numCols

      if (rc ne rcPrev) then begin


;         Set up 2-D longitude and latitude arrays for a boxcar in the
;         current row.

          x = DINDGEN(2 * smoothHoodRadCols + 1) * LonRes ; arbitrary longitudes
          y = MinLat + $
              (rc - smoothHoodRadRows + 0.5D + $
               DINDGEN(2 * smoothHoodRadRows + 1)) * LatRes

          x2d = x # REPLICATE(1.0D, 2 * smoothHoodRadRows + 1)
          y2d = TRANSPOSE(y # REPLICATE(1.0D, 2 * smoothHoodRadCols + 1))

          xCtr = x2d[smoothHoodRadCols, smoothHoodRadRows]
          yCtr = y2d[smoothHoodRadCols, smoothHoodRadRows]

          subGridDist = DISTANCE(DistPrecision, $
                                 xCtr, yCtr, $
                                 x2d, y2d)

          d2d = DBLARR(2 * smoothHoodRadCols + 1, $
                       2 * smoothHoodRadRows + 1)
          d2d[*] = subGridDist[*]
          subGridDist = d2d

          subGridFlag = subGridDist lt SmoothHoodRadM ; generic in-range flag

          rcPrev = rc

      endif

      cc = ndvInd[nc] mod numCols
      if (zGrid[cc, rc] ne Ndv) then STOP ; PROGRAMMING ERROR

;      if ((cc lt smoothHoodRadCols) or $
;          (cc gt (numCols - 1 - smoothHoodRadCols))

      si1 = (smoothHoodRadCols - cc) > 0
      si2 = 2 * smoothHoodRadCols - ((cc + smoothHoodRadCols - $
                                      (numCols - 1)) > 0)
      sj1 = (smoothHoodRadRows - rc) > 0
      sj2 = 2 * smoothHoodRadRows - ((rc + smoothHoodRadRows - $
                                      (numRows - 1)) > 0)

      i1 = (cc - smoothHoodRadCols) > 0
      i2 = (cc + smoothHoodRadCols) < (numCols - 1)
      j1 = (rc - smoothHoodRadRows) > 0
      j2 = (rc + smoothHoodRadRows) < (numRows - 1)

      if ((si2 - si1) ne (i2 - i1)) then STOP
      if ((sj2 - sj1) ne (j2 - j1)) then STOP

      subGrid = zGrid[i1:i2, j1:j2]
      sgf = subGridFlag[si1:si2, sj1:sj2]
      subGridSize = TOTAL(sgf)
;GF 20170309 vv
;      subGridSize = TOTAL(sgf, /INT) ; # of subgrid cells in range
;GF 20170309 ^^
      subGridOk = (subGrid ne Ndv) * sgf ; in range AND non-no-data flag
      if (TOTAL(subGridOk, /INT) eq 0) then CONTINUE
;GF 20170309 vv
;      subGridNot = (subGrid eq Ndv) * sgf ; in range AND no-data flag
;      if (TOTAL(subGridNot, /INT) lt 1) then STOP ; PROGRAMMING ERROR
;      default = (TOTAL(subGridNot) - 1.0) * treatNdvAs
;      if ((TOTAL(subGridOk, /INT) + TOTAL(subGridNot, /INT)) ne subGridSize) $
;          then STOP ; PROGRAMMING ERROR
;GF 20170309 ^^


;     Calculate the boxcar average, treating no-data values as zeroes
;     and excluding the (no-data value) point in the center (which is
;     why the denominator is subGridSize - 1 instead of subGridSize)
;     from the average, out of some intuitive sense of right and
;     wrong.

      newZGrid[cc, rc] = $
          TOTAL(subGrid * subGridOk) / FLOAT(subGridSize - 1)
;GF 20170309 vv
;     Calculate the boxcar average, treating no-data values as the
;     treatNdvAs value (which is zero by default) and excluding the
;     (no-data value) point in the center (which is why the
;     denominator is subGridSize - 1 instead of subGridSize) from the
;     average, out of some intuitive sense of right and wrong.

;      newZGrid[cc, rc] = $
;          (TOTAL(subGrid * subGridOk) + default) / FLOAT(subGridSize - 1)
;GF 20170309 ^^

  endfor

  if KEYWORD_SET(hash) then PRINT, ''

  RETURN, newZGrid

end
