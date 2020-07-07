PRO GET_GEO_NEIGHBORHOOD_STATS, lon_, lat_, val_, $
                                ndv_, $
                                searchRadius_, $ ; meters
                                valMean, $
                                valSD, $
;                            QUANTILE_05 = valQ05, $
;                            QUANTILE_25 = valQ25, $
;                            QUANTILE_50 = valQ50, $
;                            QUANTILE_75 = valQ75, $
;                            QUANTILE_95 = valQ95, $
                                DISTANCE_PRECISION = dPrecision, $
                                HASH = hash, $
                                SHOW_PLOTS = showPlots

; For each element of "val" in a set of data defined by lon, lat, and
; val, generate statistics for the local neighborhood, defined by
; searchRadius.
  
; Statistics always include mean and standard deviation.
; Optional statistics include quantile values for 5, 25, 50, 75, and
; 95%, as well as 

; If val is 2-dimensional, the first dimension provides for multiple
; values at the corresponding lon and lat location.

; For example, if lon and lat are 100-element arrays (i.e., there are
; 100 points), then val might be a 31 x 100 array containing a month of
; daily data values for each location. Alternatively, val might be a
; 100-element array itself.


; Verify scalar arguments.

  if (N_ELEMENTS(ndv_) ne 1) then $
      MESSAGE, 'no-data value must be a single value.'


; Convert geometry arguments to DOUBLE.

  if NOT(ISA(lon_, 'DOUBLE')) then $
      lon = DOUBLE(lon_) $
  else $
      lon = lon_
  if NOT(ISA(lat_, 'DOUBLE')) then $
      lat = DOUBLE(lat_) $
  else $
      lat = lat_

  if NOT(ISA(searchRadius_, 'DOUBLE')) then $
      searchRadius = DOUBLE(searchRadius_) $
  else $
      searchRadius = searchRadius_


; Check types of val and ndv.

  val = val_

  if NOT(ISA(val, 'FLOAT')) then $
      MESSAGE, 'WARNING: input data expected to be of type FLOAT.', /CONTINUE

  if NOT(ISA(ndv_, 'FLOAT')) then begin
      MESSAGE, 'WARNING: input no-data value expected to be of type FLOAT.', $
               /CONTINUE
      ndv = FLOAT(ndv_)
  endif else begin
      ndv = ndv_
  endelse


; Confirm that input arrays of points are consistent.

  n = N_ELEMENTS(lon)
  if (N_ELEMENTS(lat) ne n) then $
      MESSAGE, 'Input latitude and longitude arrays must be the same size.'

  valSize = SIZE(val)
  if (valSize[0] eq 2) then begin
      m = valSize[1]
      if (valSize[2] ne n) then $
          MESSAGE, '2nd dimension of input data must match latitude and ' + $
                   'longitude arrays.'
  endif else if (valSize[0] eq 1) then begin
      m = 1
  endif else begin
      MESSAGE, 'Input data must be 1- or 2-dimensional.'
  endelse


; Get rid of no-data values.

  if (m eq 1) then begin
      numOk = LONG(val ne ndv)
  endif else begin
      numOk = LONARR(n)
      for j = 0, n - 1 do begin
          ind = WHERE(val[*, j] ne ndv, count)
          numOk[j] = count
      endfor
  endelse
  ind = WHERE(numOk gt 0, count)
  if (count eq 0) then $
      MESSAGE, 'All input data are no-data values.'
  lon = lon[ind]
  lat = lat[ind]
  val = val[*,ind]


; Initialize outputs.

  valMean = MAKE_ARRAY(n, /FLOAT, VALUE = ndv)
  valSD = MAKE_ARRAY(n, VALUE = ndv)


; Verify valid setting for precision.

  if NOT(KEYWORD_SET(dPrecision)) then begin
      dPrecision = 2
  endif else begin
      if ((dPrecision lt 0) or (dPrecision gt 3)) then begin
          MESSAGE, 'DISTANCE_PRECISION must be 0, 1, 2, or 3.'
      endif
  endelse


; Use near-equator meters-per-degree ratio (the smallest) at all latitudes for
; determining the latitude band to use for collecting qualifying
; observations. This distance corresponds to about 111 km per degree latitude.

  mPerDegLatRef = $
    DOUBLE(FLOOR(DISTANCE(dPrecision, $
                          0.0D, 0.0005D, 0.0D, -0.0005D) * 1000.0D))


; The length of a degree in longitude is smallest at the highest latitude, so
; use that to estimate meters per degree longitude.

  maxAbsLat = MAX(ABS(lat))
  mPerDegLonRef = $
    DOUBLE(FLOOR(DISTANCE(dPrecision, $
                          0.0D, maxAbsLat, 1.0D, maxAbsLat)))

  deltaLat = searchRadius / mPerDegLatRef
  deltaLon = searchRadius / mPerDegLonRef

  
; Compute distances between all points.

  numDist = n * (n - 1L) / 2L


; Predict the maximum number of distances we will have to
; compute. This takes time, but it may prevent a prohibitive memory
; allocation and avoids the very slow method of reallocating with
; every measurement.

  numDistCalcGuess = 0UL
  maxNumNeighborsGuess = 0UL

  for i = 0, n - 1 do begin

      lon_i = lon[i]
      lat_i = lat[i]

      boxMinLon = lon_i - deltaLon
      boxMaxLon = lon_i + deltaLon
      boxMinLat = lat_i - deltaLat
      boxMaxLat = lat_i + deltaLat

      ind = WHERE((lon gt boxMinLon) and $
                  (lon lt boxMaxLon) and $
                  (lat gt boxMinLat) and $
                  (lat lt boxMaxLat), count)

      numDistCalcGuess += count
      if (count gt maxNumNeighborsGuess) then $
          maxNumNeighborsGuess = count

  endfor

  if (n lt 50) then begin
      hashes = n
      lastProgress = -1
  endif else begin
      hashes = 50
      lastProgress = 0
  endelse
  if KEYWORD_SET(hash) then begin
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif


;----------------------------------------------------------------------------;
;
; Notes on collecting and storing distances between discrete
; locations.
;
; For a collection of n observation locations, the number of
; between-observation distances is
;
;   numDistances = n * (n - 1) / 2
;
; this set represents a triangular matrix where all the values along
; the diagonal (distances between a location and itself) are zero. A
; 1-D index "k" for a 6 x 6 matrix of these distances (for which
; numDistances = 15) would look like this:
;
;    +-------------------------+
;    | 10  11  12  13  14   -  | 5
;    |  6   7   8   9   -   -  | 4
;    |  3   4   5   -   -   -  | 3
;    |  1   2   -   -   -   -  | 2
;    |  0   -   -   -   -   -  | 1
;    |  -   -   -   -   -   -  | 0 = j (row)
;    ---------------------------+
;   i = 0   1   2   3   4   5
;
; Using this convention for row (j) and column (i),
;
;   k = j * (j - 1) / 2 + i
;
; Setting i = 0 and solving the resulting quadratic equation gives a
; solution for i and j given k, though this program probably will not
; use it:
;
;   j = FLOOR(0.5 + 0.5 * SQRT(1.0 + 8.0 * k))
;   i = k - j * (j - 1) / 2
;
; Since this program will only store distances up to some maximum
; separation distance, nowhere near the full numDistances values
; of the matrix will be calculated. Allocating memory for the entire
; matrix is not necessary. There are two possible approaches to
; storing the distances: in a 1-D array, or in a 2-D array.
;
; 1-D option:
;
; Here, the distances calculated are stored in a 1-D array of
; "nearby-neighbor" distances, with a parallel reference array of
; corresponding k values. The distance between any two points i
; and j, as long as they satisfy the "nearby neighbor" criterion, can
; be retrieved by calculating k from i and j (as shown above), finding
; the element of the reference array with that value of k, and getting
; the corresponding element of the reference array.
;
; The drawback of this approach is that the reference array can get
; very big for large data sets, which makes searching it quite slow.
; 
; For example, the array of "nearby neighbor" distances is
; allDist_1D, and the reference array is all_k_1D. For a given i
; and j,
;
;   k = j * (j - 1) / 2  + i
;   ind = WHERE(all_k_1D eq k, count) ; make sure count = 1
;   dist = allDist_1D[ind[0]]
;
; 2-D option:
;
; Here, we set up a 2-D array of distances, where the second (row)
; dimension is the site index (n elements), and the first is just
; large enough to hold the neighbors of each site. The matrix is set
; up to be similarly triangular to what is described above, so that
; for each j = 0, n - 1, no more than i < j neighbor distances will be
; defined, and for each distance, the corresponding site index i will
; be stored as a reference. So whereas the 1-D option used k as the
; reference, the 2-D option can keep it simple and use i as the
; reference. The cost of this option is that every row of the matrix
; must be large enough to hold the maximum number of neighbors
; possible, and so there will be a large number of no-data values and
; a corresponding memory burden. However, searching rows of the
; reference matrix for specific values of i will be much faster than
; searching the 1-D reference matrix for specific values of k.
;
;----------------------------------------------------------------------------;

  i_2d = MAKE_ARRAY(maxNumNeighborsGuess, n, $
                    /LONG, VALUE = -1L)
  dist_2d = MAKE_ARRAY(maxNumNeighborsGuess, n, $
                       /DOUBLE, VALUE = DOUBLE(ndv))

  numDistCalc = 0UL
  maxNumNeighbors = 0UL

  for j = 1L, n - 1L do begin

      progress = FIX(FLOAT(j) / FLOAT(n - 1L) * FLOAT(hashes))
      if (progress gt lastProgress) then begin
          lastProgress = progress
          PRINT, FORMAT='($,"#")'
      endif

      lon_j = lon[j]
      lat_j = lat[j]

      l = 0L

      for i = 0L, j - 1L do begin

          lon_i = lon[i]
          lat_i = lat[i]

;         First decide if the station is "in the box".
          lonDiff = ABS(lon_j - lon_i)
          if (lonDiff gt deltaLon) then CONTINUE
          latDiff = ABS(lat_j - lat_i)
          if (latDiff gt deltaLat) then CONTINUE

;         If the station is in the box we have to use DISTANCE.
          d = DISTANCE(dPrecision, lon_i, lat_i, lon_j, lat_j)
          if (ROUND(d) ge ROUND(searchRadius)) then CONTINUE

          dist_2D[l, j] = d
          i_2D[l, j] = i

          l++
          numDistCalc++

      endfor

      if (l gt maxNumNeighbors) then maxNumNeighbors = l

  endfor

  if KEYWORD_SET(hash) then PRINT, ''

  PRINT, 'Number of distance calculations performed/stored: ', numDistCalc
  PRINT, 'Maximum number of neighbors: ', maxNumNeighbors

  dist_2d = dist_2d[0L:maxNumNeighbors - 1L, *]
  i_2d = i_2d[0L:maxNumNeighbors - 1L, *]


; Set histogram parameters.
  
  hMin = 0.0 ; SLR = 10^0 = 1
  hMax = 2.0 ; SLR = 10^2 = 100
  binSize = 0.05
  numBins = ROUND((hMax - hMin) / binSize)
  pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
  hMin = hMin - pad
  hMax = hMax + pad
  hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize


; Gather statistics for each site.

  for j = 0, n - 1 do begin

      lon_j = lon[j]
      lat_j = lat[j]

      if KEYWORD_SET(showPlots) then begin


;         Get the points in the lon/lat box for this location and
;         search distance.

          boxMinLon = lon_j - deltaLon
          boxMaxLon = lon_j + deltaLon
          boxMinLat = lat_j - deltaLat
          boxMaxLat = lat_j + deltaLat

          boxInd = WHERE((lon gt boxMinLon) and $
                         (lon lt boxMaxLon) and $
                         (lat gt boxMinLat) and $
                         (lat lt boxMaxLat), boxCount)

          if (boxCount eq 0) then CONTINUE

      endif


;     Identify all indices associated with this one in the i_2d and
;     dist_2d arrays.

      i2dRow = i_2d[*, j]
      d2dRow = dist_2d[*, j]
      hoodCount = 0L
      ind = WHERE(i2dRow ne -1L, hoodCount)
      if (hoodCount gt 0) then begin
          iHood = i2dRow[ind]
          dHood = d2dRow[ind]
      endif
      ind = WHERE(i_2d eq j, count)
      if (count gt 0) then begin
          if (hoodCount eq 0) then begin
              iHood = ind / maxNumNeighbors
              dHood = dist_2d[ind]
          endif else begin
              iHood = [iHood, ind / maxNumNeighbors]
              dHood = [dHood, dist_2d[ind]]
          endelse
      endif
      hoodCount += count
      if (hoodCount eq 0) then CONTINUE
      if (MAX(dHood) ge searchRadius) then STOP ; PROGRAMMING CHECK

;     Calculate overall mean and standard deviation.

      hoodVal = val[*, iHood]
      ind = WHERE(hoodVal ne ndv, hoodValCount)
      if (hoodValCount eq 0) then STOP
      hoodVal = hoodVal[ind]
      lmn = MEAN(hoodVal)
      lsd = STDDEV(hoodVal)
      valMean[j] = lmn
      valSD[j] = lsd

      if KEYWORD_SET(showPlots) then begin

          PLOT, lon[boxInd], lat[boxInd], /YNOZERO, $
                XRANGE = [boxMinLon - 1.0D, boxMaxLon + 1.0D], $
                YRANGE = [boxMinLat - 1.0D, boxMaxLat + 1.0D], $
                XSTYLE = 1, YSTYLE = 1, $
                /NODATA
          OPLOT, lon[boxInd], lat[boxInd], $
                 PSYM = 1, COLOR = 100        ; gray "+" for "in the box"
          OPLOT, lon, lat, PSYM = 3           ; dot for all points
          PLOTS, lon_j, lat_j, PSYM = 6       ; square for center point
          PLOTS, lon[iHood], lat[iHood], $
                 PSYM = 4                     ; diamond for "in the circle"

;         Pinwheel to points in the circle.
          for hc = 0, hoodCount - 1 do $
              PLOTS, [lon_j, lon[iHood[hc]]], [lat_j, lat[iHood[hc]]]

;         Generate histogram.

          h = HISTOGRAM(hoodVal, $
                        MIN = hMin, MAX = hMax, BINSIZE = binSize)

          order = SORT(hoodVal)
          hoodVal = hoodVal[order]

          PLOT, hAxis, h[0:numBins - 1], PSYM = 10, $
                XTICK_GET = xt, XSTYLE = 4
          AXIS, XAXIS = 0, XTICKNAME = FORMAT_FLOAT(10.0^xt)
          AXIS, XAXIS = 1, XTICKNAME = REPLICATE(' ', N_ELEMENTS(xt))

;         Plot the empirical CDF.
          eCDF = (1.0 + FINDGEN(hoodValCount)) / FLOAT(hoodValCount)
          OPLOT, hoodVal, $
                 !Y.CRange[0] + eCDF * (!Y.CRange[1] - !Y.CRange[0]), $
                 COLOR = 100

;         Plot the hypothetical CDF based on the mean and standard deviation.
          hCDF = 0.5 * (1.0 + ERF((hoodVal - lmn) / (lsd * SQRT(2.0))))
          OPLOT, hoodVal, $
                 !Y.CRange[0] + hCDF * (!Y.CRange[1] - !Y.CRange[0]), $
                 COLOR = 200

;         Calculate Anderson-Darling statistic for the empirical CDF.
;         This version does not seem to calculate it correctly.
          ;; CDFError = eCDF - hCDF
          ;; ad = CDFError^2.0D / (hCDF * (1.0 - hCDF))
          ;; flag = FINITE(ad)
          ;; ind = WHERE(flag, count)
          ;; if (count eq 0) then STOP
          ;; if (count lt 8) then STOP
          ;; ads = TOTAL(ad[ind]) / count
          ;; daads = ads * (1.0 + 0.75 / count + 2.25 / count^2.0)

;         Calculate Anderson-Darling statistic for the empirical CDF.
;         More commonly cited version, with D''Agostino modification.
          ;; term = ALOG(hCDF) + ALOG(1.0 - REVERSE(hCDF))
          ;; flag = FINITE(term)
          ;; ind = WHERE(flag, count)
          ;; if (count eq 0) then STOP
          ;; if (count lt 8) then STOP
          ;; ads = - FLOAT(count) $
          ;;       - TOTAL((2.0 * FINDGEN(count) + 1.0) $
          ;;               * term[ind]) / count
          ;; daads = ads * (1.0 + 0.75 / count + 2.25 / count^2.0)

          ;; PRINT, 'Anderson-Darling: ' + STRCRA(ads)
          ;; PRINT, 'Anderson-Darling (D''Agostino): ' + STRCRA(daads)

;         Calculate critical value.
          ;; case 1 of
          ;;     daads ge 0.6: begin
          ;;         pVal = EXP(1.2937 - 5.709 * daads + 0.0186 * daads^2.0)
          ;;     end
          ;;     daads ge 0.34 and daads lt 0.6: begin
          ;;         pVal = EXP(0.9177 - 4.279 * daads - 1.38 * daads^2.0)
          ;;     end
          ;;     daads ge 0.2 and daads lt 0.34: begin
          ;;         pVal = 1.0 - EXP(-8.318 + 42.796 * daads - 59.938 * daads^2.0)
          ;;     end
          ;;     else: begin
          ;;         pVal = 1.0 - EXP(-13.436 + 101.14 * daads - 223.73 * daads^2.0)
          ;;     end
          ;; endcase

    ;;       PRINT, 'p Value: ' + STRCRA(pVal)

    ;;       if (daads gt pVal) then $
    ;;           MESSAGE, 'Normality test failed.'
    ;; ;      if (daads gt 0.1) then STOP

          OPLOT, [lmn, lmn], !Y.CRange, COLOR = 100
          OPLOT, REPLICATE(lmn - 2.0 * lsd, 2), !Y.CRange, $
                 LINESTYLE = 1, COLOR = 100
          OPLOT, REPLICATE(lmn + 2.0 * lsd, 2), !Y.CRange, $
                 LINESTYLE = 1, COLOR = 100

          PRINT, 10.0^lmn, 10.0^(lmn - 2.0 * lsd), 10.0^(lmn + 2.0 * lsd)
          WAIT, 1

      endif

  endfor

  RETURN

end
