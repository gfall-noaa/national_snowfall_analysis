PRO REPLACE_NDV_WITH_NEIGHBORHOOD_AVERAGE, $
    inputGrid, $
    Ndv, $
    MinLon, MaxLon, LonRes, $
    MinLat, MaxLat, LatRes, $
    HoodMinMeters, $
    HoodStepMeters, $
    HoodMaxMeters, $
    MinCellsInHood


; Replace no-data values with neighborhood averages, expanding the
; neighborhood as needed.

  ind = WHERE(inputGrid ne Ndv, count)
  if (count eq 0) then begin
      ERR_MSG, 'Input grid is all no-data values.'
      RETURN
  endif
  inputGridMean = MEAN(inputGrid[ind])

  ndvInd = WHERE(inputGrid eq Ndv, ndvCount)
  if (ndvCount eq 0) then RETURN


; Calculate and verify grid size.

  numLon = ROUND((MaxLon - MinLon) / LonRes)
  numLat = ROUND((MaxLat - MinLat) / LatRes)

  xErr = ABS(numLon * LonRes - (MaxLon - MinLon))
  if (xERR gt 1.0D-8) then begin
      ERR_MSG, 'Inconsistent longitudinal domain/resolution.'
      RETURN
  endif

  yErr = ABS(numLat * LatRes - (MaxLat - MinLat))
  if (yErr gt 1.0D-8) then begin
      ERR_MSG, 'Inconsistent latitudinal domain/resolution.'
      RETURN
  endif


; Set up 2-D grids of longitude and latitude.

  xAxis = MinLon + (0.5D + DINDGEN(numLon)) * LonRes
  yAxis = MinLat + (0.5D + DINDGEN(numLat)) * LatRes

  lonGrid = xAxis # REPLICATE(1.0D, numLat)
  latGrid = TRANSPOSE(yAxis # REPLICATE(1.0D, numLon))
  xAxis = !NULL & yAxis = !NULL


; The length of a degree in latitude is smallest at the equator,
; for the typical ellipsoid, so use the 3.6 arc seconds about the
; equator to estimate meters per degree latitude.

  mPerDegLatRef = $
      DOUBLE(FLOOR(DISTANCE(1, $
                            0.0D, 0.0005D, 0.0D, -0.0005D) * 1000.0D))


; The length of a degree in longitude is smallest at the highest
; latitude, so use that latitude to estimate meters per degree
; longitude.

  MaxLatRef = ABS(MaxLat) > ABS(MinLat)
  mPerDegLonRef = $
      DOUBLE(FLOOR(DISTANCE(1, $
                            0.0D, MaxLatRef, 1.0D, MaxLatRef)))


; Loop over no-data values.

  outputGrid = inputGrid

  iNdv = ndvInd mod numLon
  jNdv = ndvInd / numLon

  jPrev = -1L

  for ijc = 0, ndvCount - 1 do begin

      ic = iNdv[ijc]
      jc = jNdv[ijc]

      if (jc ne jPrev) then begin

          yc = MinLat + (0.5D + jc) * LatRes

          mPerDegLat = $
              DOUBLE(FLOOR(DISTANCE(2, $
                                    0.0D, yc - 0.0005D, $
                                    0.0D, yc + 0.0005D) * 1000.0D))
          mPerDegLon = $
              DOUBLE(FLOOR(DISTANCE(2, $
                                    0.0D, yc, 1.0D, yc)))

          jPrev = jc

      endif else new =0

      xc = MinLon + (0.5D + ic) * LonRes

      hoodRad = hoodMinMeters - HoodStepMeters
      inHoodCount = 0

      outputGrid[ic, jc] = inputGridMean ; default

      while (inHoodCount lt MinCellsInHood) do begin

          hoodRad = hoodRad + HoodStepMeters
          if (hoodRad gt HoodMaxMeters) then BREAK

          dLat = hoodRad / mPerDegLatRef
          dLon = hoodRad / mPerDegLonRef

          dRows = CEIL(dLat / LatRes)
          dCols = CEIL(dLon / LonRes)

          hoodi1 = (ic - dCols) > 0
          hoodi2 = (ic + dCols) < (numLon - 1)

          hoodj1 = (jc - dRows) > 0
          hoodj2 = (jc + dRows) < (numLat - 1)

          hoodCols = hoodi2 - hoodi1 + 1
          hoodRows = hoodj2 - hoodj1 + 1

          iHood = hoodi1 + LINDGEN(hoodCols)
          jHood = hoodj1 + LINDGEN(hoodRows)

          boxVal = inputGrid[hoodi1:hoodi2, hoodj1:hoodj2]
          boxLon = lonGrid[hoodi1:hoodi2, hoodj1:hoodj2]
          boxLat = latGrid[hoodi1:hoodi2, hoodj1:hoodj2]

          boxInHoodInd = WHERE(boxVal ne Ndv, inHoodCount)
          if (inHoodCount lt minCellsInHood) then CONTINUE

          dInBox = DISTANCE(0, xc, yc, boxLon, boxLat)
          boxInHoodInd = WHERE((dInBox lt hoodRad) and $
                               (boxVal ne Ndv), inHoodCount)

;          boxInHoodInd = WHERE(boxVal ne Ndv, inHoodCount)

          if (inHoodCount lt MinCellsInHood) then CONTINUE

          outputGrid[ic, jc] = MEAN(boxVal[boxInHoodInd])

;;           if (jc mod 50 eq 0) then begin
;; ;              print, ic, jc, hoodrad, outputgrid[ic,jc]
;;               loadct, 0
;;               wset_or_window, 5, xsize=numLon, ysize=numLat
;;               real_tvscl, outputGrid, ndv = Ndv
;;           endif

      endwhile

  endfor
              ;; loadct, 0
              ;; wset_or_window, 5, xsize=numLon, ysize=numLat
              ;; real_tvscl, outputGrid, ndv = Ndv

  inputGrid = outputGrid & outputGrid = !NULL

  RETURN

end
