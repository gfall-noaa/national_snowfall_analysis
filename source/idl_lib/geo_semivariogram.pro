PRO GEO_SEMIVARIOGRAM, lon_, lat_, f_, $
                       maxLagMeters_, $
                       gsStatus, $
                       NDV = ndv, $
                       DISTANCE_PRECISION = dPrecision, $
                       SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = $
                         minRangeSpherical_, $
                       PLOT_TITLE = plotTitle, $
                       MIN_LAG_BIN_COUNT = minLagBinCount, $
                       LAG_TOLERANCE = lagTolerance_, $
                       EXPONENTIAL_SEMIVARIOGRAM_PARAMS = paramsExpo, $
                       EXPONENTIAL_SEMIVARIOGRAM_RMSE = RMSEExpo, $
                       SPHERICAL_SEMIVARIOGRAM_PARAMS = paramsSpherical, $
                       SPHERICAL_SEMIVARIOGRAM_RMSE = RMSESpherical, $
                       SPHERICAL_SEMIVARIOGRAM_IN_RANGE_RMSE = $
                         RMSESphericalInRange, $
                       LAG_OUT = lagMeters, $
                       SEMIVARIOGRAM_OUT = empVario, $
                       HASH = hash, $                 ; 0/1
                       SHOW_PLOT = showPlot, $        ; 0/1
                       SHOW_SD_RANGE = showSDRange, $ ; 0/1
                       VERBOSE = verbose, $           ; 0/1
                       SHORT_LAG_WEIGHT_BIAS = shortLagWeightBias, $ ; 0/1
                       SCALE_INPUT_VARIABLE = scaleInputVar ; 0/1


; Compute the empirical semivariogram of a set of values associated
; with specific longitudes and latitudes, and fit models to results.

  COMMON info, message

  gsStatus = 0


;---------------------------------------------------------;
; 1. Check arguments for correct type and valid contents. ;
;---------------------------------------------------------;

  if NOT(ISA(lon_, 'DOUBLE')) then $
      lon = DOUBLE(lon_) $
  else $
      lon = lon_
  if NOT(ISA(lat_, 'DOUBLE')) then $
      lat = DOUBLE(lat_) $
  else $
      lat = lat_

  f = f_

  n = N_ELEMENTS(f)

  if ((N_ELEMENTS(lon) ne n) or (N_ELEMENTS(lat) ne n)) then begin
      ERR_MSG, 'Input longitude and latitude arrays must be the same size ' + $
               'as the data array.'
      RETURN
  endif

  if (NOT(ISA(f, 'FLOAT')) and NOT(ISA(f, 'DOUBLE'))) then $
      ERR_MSG, 'WARNING: input data for semivariogram should be type ' + $
               'FLOAT or DOUBLE.'

; NOTE: if maxLagMeters is a multiple of 5000 meters, that helps with
; selection of variogram bin sizes.

  if NOT(ISA(maxLagMeters_, 'DOUBLE')) then $
      maxLagMeters = DOUBLE(maxLagMeters_) $
  else $
      maxLagMeters = maxLagMeters_

  if ((maxLagMeters lt 0.0D) or (maxLagMeters gt 3.0D6)) then begin
      ERR_MSG, 'Minimum/maximum separation distance must be between ' + $
               '0 and 3.0e6 meters.'
      RETURN
  endif

  if KEYWORD_SET(ndv) then begin
      if ((ndv eq 0) or (ndv eq 1)) then $
          ERR_MSG, 'WARNING: setting of NDV to ' + STRCRA(ndv) + $
                   ' may have unexpected results.'
  endif else begin
      ndv = -99999.0
  endelse

  if NOT(KEYWORD_SET(dPrecision)) then begin
      dPrecision = 2
  endif else begin
      if ((dPrecision lt 0) or (dPrecision gt 3)) then begin
          ERR_MSG, 'DISTANCE_PRECISION must be 0, 1, 2, or 3'
          RETURN
      endif
  endelse

  if KEYWORD_SET(minRangeSpherical_) then begin

      if NOT(ISA(minRangeSpherical_, 'DOUBLE')) then begin
          ERR_MSG, 'WARNING: SPHERICAL_SEMIVARIOGRAM_MIN_RANGE keyword ' + $
                   'should be provided as type DOUBLE.'
          minRangeSpherical = DOUBLE(minRangeSpherical_)
      endif else begin
          minRangeSpherical = minRangeSpherical_
      endelse

      if (minRangeSpherical lt 0.0D) then begin
          ERR_MSG, 'WARNING: ignoring negative ' + $
                   'SPHERICAL_SEMIVARIOGRAM_MIN_RANGE setting.'
          minRangeSpherical = 0.0D
      endif

  endif else begin

      minRangeSpherical = 0.0D

  endelse

  if KEYWORD_SET(plotTitle) then begin
      if NOT(ISA(plotTitle, 'STRING')) then begin
          ERR_MSG, 'WARNING: PLOT_TITLE keyword should be provided as ' + $
                   'type STRING.'
          RETURN
      endif
  endif

;Kent modified minLagBinCount = 50 

  if KEYWORD_SET(minLagBinCount) then begin
      if (minLagBinCount lt 0) then begin
          ERR_MSG, 'WARNING: ignoring negative ' + $
                   'MIN_LAG_BIN_COUNT setting.'
          minLagBinCount = 40
      endif
  endif else begin
      minLagBinCount = 40
  endelse

  if KEYWORD_SET(lagTolerance_) then begin

      if NOT(ISA(lagTolerance_, 'DOUBLE')) then begin
          ERR_MSG, 'WARNING: LAG_TOLERANCE keyword should be provided as ' + $
                   'type DOUBLE.'
          lagTolerance = DOUBLE(lagTolerance)
      endif else begin
          lagTolerance = lagTolerance_
      endelse

      if (lagTolerance lt 0.0D) then begin
          ERR_MSG, 'LAG_TOLERANCE must be nonnegative.'
          RETURN
      endif

      if (lagTolerance gt maxLagMeters) then begin
          ERR_MSG, 'LAG_TOLERANCE must be less than the maximum lag ' + $
                   'argument, which is ' + STRCRA(maxLagMeters)
          RETURN
      endif


  endif


; Eliminate no-data values.

  ind = WHERE(f ne ndv, count)
  if (count eq 0) then begin
      ERR_MSG, 'Input data are all no-data values.'
      RETURN
  endif
  n = count
  lon = lon[ind]
  lat = lat[ind]
  f = f[ind]


; Scale data by its mean and standard deviation.

  if KEYWORD_SET(scaleInputVar) then f = (f - MEAN(f)) / STDDEV(f)


;----------------------------------------------------------;
; 2. Calculate a lon/lat box associated with maxLagMeters. ;
;----------------------------------------------------------;


; The length of a degree in latitude is smallest at the equator, for the
; typical ellipsoid, so use the 36 arc seconds about the equator to estimate.

  mPerDegLatRef = $
    DOUBLE(FLOOR(DISTANCE(dPrecision, $
                          0.0D, 0.0005D, 0.0D, -0.0005D) * 1000.0D))


; The length of a degree in longitude is smallest at the highest latitude, so
; use that.

  foo = MAX(ABS(lat), maxLatInd)
  mPerDegLonRef = $
    DOUBLE(FLOOR(DISTANCE(dPrecision, $
                          0.0D, lat[maxLatInd], 1.0D, lat[maxLatInd])))

  deltaLat = maxLagMeters / mPerDegLatRef
  deltaLon = maxLagMeters / mPerDegLonRef


;----------------------------------------------------------------------;
; 3. Determine distances between all points within maxLagMeters of one ;
;    another.                                                          ;
;----------------------------------------------------------------------;

; Inner loop is limited so that we don't calculate distances twice.

  numDist = n * (n-1) / 2 ; the total number of distances without limit


; Predict the maximum number of distances we will have to
; compute. This takes time, but it may prevent a prohibitive memory
; allocation and avoids the very slow method of reallocating with
; every measurement.

  kGuess = 0L

  for i = 0, n - 1 do begin

      loni = lon[i]
      lati = lat[i]

      boxMinLon = loni - deltaLon
      boxMaxLon = loni + deltaLon
      boxMinLat = lati - deltaLat
      boxMaxLat = lati + deltaLat

      ind = WHERE((lon gt boxMinLon) and $
                  (lon lt boxMaxLon) and $
                  (lat gt boxMinLat) and $
                  (lat lt boxMaxLat), count)

      kGuess = kGuess + count

  endfor


; Initialize arrays of distance and variance.

  dist = FLTARR(kGuess) & dist[*] = ndv
  var = FLTARR(kGuess) & var[*] = ndv

  k = 0L

  if KEYWORD_SET(hash) then begin
      if (n lt 50) then begin
          hashes = n
          lastProgress = -1
      endif else begin
          hashes = 50
          lastProgress = 0
      endelse
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif


; Calculate distances.

  for i = 0, n - 1 do begin

      if KEYWORD_SET(hash) then begin
          progress = FIX(FLOAT(i) / FLOAT(n - 1) * FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      loni = lon[i]
      lati = lat[i]
      fi = f[i]

      for j = 0, i - 1 do begin

          lonj = lon[j]
          latj = lat[j]

          lonDiff = ABS(lonj - loni)
          latDiff = ABS(latj - lati)
          if (lonDiff gt deltaLon) then CONTINUE
          if (latDiff gt deltaLat) then CONTINUE
          ;if ((lonDiff lt 1.0D-6) and (latDiff lt 1.0D-6)) then CONTINUE

          d = DISTANCE(dPrecision, loni, lati, lonj, latj)
          if (ROUND(d) ge ROUND(maxLagMeters)) then CONTINUE

          fj = f[j]

          dist[k] = d
          var[k] = (fi - fj)^2.0

          k++
          if (k eq kGuess) then begin
              ERR_MSG, 'PROGRAMMING ERROR: distance count estimate failed.'
              RETURN
          endif

      endfor

  endfor

  if KEYWORD_SET(hash) then PRINT, ''

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Number of distance calculations performed: ' + STRCRA(k)

  dist = dist[0:k-1]
  var = var[0:k-1]

  if (MAX(dist) ge maxLagMeters) then begin
      ERR_MSG, 'PROGRAMMING ERROR: distances >= maximum lag argument of ' + $
               STRCRA(maxLagMeters) + ' calculated.'
      RETURN
  endif


; Sort results by distance.

  order = SORT(dist)
  dist = dist[order]
  var = var[order]
  order = !NULL


; Set up the empirical semivariogram.

  if KEYWORD_SET(lagTolerance) then begin

      numLags = ROUND(maxLagMeters / lagTolerance)

  endif else begin

;     Place site separations into a variety of histograms to choose a
;     lag size.

      numLagsToTest = 50
      minLag_ = 1000.0
      maxLag_ = 25000.0
      lagStep = 1000.0
      numLagsToTest = ROUND((maxLag_ - minLag_) / lagStep) + 1
      tryLags = minLag_ + FINDGEN(numLagsToTest) * lagStep
      numLags = ROUND(maxLagMeters / tryLags)
      numLags = numLags[UNIQ(numLags)]
      numLagsToTest = N_ELEMENTS(numLags)
      lagTolerance = maxLagMeters / numLags
      if NOT(KEYWORD_SET(minLag)) then $
          minLag = lagTolerance[0]
      if NOT(KEYWORD_SET(maxLag)) then $
          maxLag = lagTolerance[numLagsToTest - 1]
      numZero = LONARR(numLagsToTest)
      numOne = LONARR(numLagsToTest)
      minCount = LONARR(numLagsToTest)
      maxCount = LONARR(numLagsToTest)
      for lc = 0L, numLagsToTest - 1L do begin
          hist = HISTOGRAM(dist, $
                           MIN = 0.0, $
                           MAX = maxLagMeters, $
                           BINSIZE = lagTolerance[lc])
          if (N_ELEMENTS(hist) eq (numLags[lc] + 1L)) then begin
              if (hist[numLags[lc]] ne 0) then STOP ; weird programming mistake
          endif
          hist = hist[0:numLags[lc] - 1]
          ind = WHERE(hist eq 0, count)
          numZero[lc] = count
          ind = WHERE(hist le 1, count)
          numOne[lc] = count
          minCount[lc] = MIN(hist)
          maxCount[lc] = MAX(hist)
      endfor


;     Select the smallest lag size for which there are at least 50
;     pairs and zero empty bins, by histogramming site separations.

      if (maxLag le minLag) then STOP
      ind = WHERE((numOne eq 0) and (minCount ge minLagBinCount) and $
                  (lagTolerance ge minLag) and (lagTolerance le maxLag), count)
      if (count eq 0) then $
          MESSAGE, 'Insufficient reports to do a variogram.'
      numLags = numLags[ind[0]]
      lagTolerance = lagTolerance[ind[0]]

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'GEO_SEMIVARIOGRAM: lag bin size: ', + $
                   STRCRA(lagTolerance) + $
                   ' meters'

  endelse


; Calculate the empirical semivariogram for lagTolerance and numLags. Note
; that the division by 2 is not included when empVario is
; calculated. This is because "var" is only calculated one time for
; each pair of observations. The division by 2 is implicit in the
; process.

  empVario = FLTARR(numLags) ; textbook empirical semivariogram
  empVarioVariance = FLTARR(numLags)
  empVarioCount = LONARR(numLags)

  for b = 0, numLags - 1 do begin
      x1 = b * lagTolerance
      x2 = (b + 1) * lagTolerance
      ind = WHERE((dist ge x1) and (dist lt x2), count)
      if (count lt minLagBinCount) then begin
          ERR_MSG, 'Insufficient data to generate empirical ' + $
                   'semivariogram.'
          RETURN
      endif
      empVario[b] = TOTAL(var[ind]) / count
      empVarioVariance[b] = TOTAL((var[ind] - empVario[b])^2.0) / (count - 1)
      empVarioCount[b] = count
  endfor

  lagMeters = lagTolerance * (0.5 + FINDGEN(numLags)) ; x axis

  if KEYWORD_SET(showPlot) then begin

      if NOT(KEYWORD_SET(plotTitle)) then plotTitle = ''

      if KEYWORD_SET(showSDRange) then $
          yRange = [MIN(empVario - SQRT(empVarioVariance)), $
                    MAX(empVario + SQRT(empVarioVariance))] $
      else $
          yRange = [0, MAX(empVario)]

      PLOT, lagMeters / 1000, empVario, $
            XTITLE = 'Separation (km)', YTITLE = 'Variogram', $
            YRANGE = yRange, $
            PSYM = 10, $
            TITLE = plotTitle

      if KEYWORD_SET(showSDRange) then begin
          OPLOT, lagMeters / 1000, empVario + SQRT(empVarioVariance), $
                 COLOR = 50, PSYM = 10
          OPLOT, lagMeters / 1000, empVario - SQRT(empVarioVariance), $
                 COLOR = 50, PSYM = 10
      endif

  endif

  if ARG_PRESENT(paramsExpo) then begin


;     Fit an exponential semivariogram model.

      if (N_ELEMENTS(paramsExpo) eq 3) then begin


;         Begin with suggested parameters.

          nugget = paramsExpo[0]
          sill = paramsExpo[1]
          range = paramsExpo[2]

      endif else begin

          nugget = MIN(empVario)
          sill = MAX(empVario)
          range = lagMeters[MIN(WHERE(empVario gt MEAN(empVario)))]

      endelse

      paramsExpo = [nugget, sill, range]


;     Compute weights for semivariogram fitting.

      weightFactor = 1.0D
      if KEYWORD_SET(shortLagWeightBias) then $
          weightFactor = (maxLagMeters - lagMeters) / maxLagMeters
      fitWeight = weightFactor^2.0D * 1.0D / empVarioVariance

      svEFit = CURVEFIT(lagMeters, empVario, $
                        fitWeight, $
                        paramsExpo, $
                        FUNCTION_NAME = 'EXPONENTIAL_SEMIVARIOGRAM', $
                        ITMAX = 1000, $
                        ITER = numIterations, $
                        /DOUBLE, $
;                        YERROR = yErrE, $
                        STATUS = status)

      if (status ne 0) then begin
          case status of
              1: msg = 'chi squared increasing without bounds'
              2: msg = 'failed to converge in 1000 iterations'
              else: msg = 'unknown error'
          endcase
          MESSAGE, 'Exponential semivariogram fit failed with status ' + $
                   STRCOMPRESS(status, /REMOVE_ALL) + $
                   ' (' + msg + ').'
      endif

;      RMSEExpo = SQRT(MEAN(yErrE^2.0D))
      RMSEExpo = SQRT(TOTAL((empVario - svEFit)^2.0) / numLags)

      ind = WHERE(lagMeters lt paramsExpo[2], count)
      if (count gt 0) then begin
          RMSEExponentialInRange = $
              SQRT(TOTAL((empVario[ind] - svEFit[ind])^2.0) / count)
          if KEYWORD_SET(verbose) then $
              USR_MSG, 'In-range RMSE for exponential semivariogram fit: ' + $
                       STRCRA(RMSEExponentialInRange)
      endif

      if KEYWORD_SET(showPlot) then begin
          OPLOT, lagMeters / 1000, svEFit, COLOR = 100
          XYOUTS, lagMeters[numLags - 1] / 1000.0, svEFit[numLags - 1], 'E'
      endif

  endif

  if ARG_PRESENT(paramsSpherical) then begin


;     Fit a spherical semivariogram model.

      if (N_ELEMENTS(paramsSpherical) eq 3) then begin


;         Begin with suggested parameters.

          nugget = paramsSpherical[0]
          sill = paramsSpherical[1]
          range = paramsSpherical[2]

      endif else begin


;         Guess parameters.

          nugget = MIN(empVario)
          sill = MAX(empVario)
          range = lagMeters[MIN(WHERE(empVario gt MEAN(empVario)))]

      endelse

      range = range > minRangeSpherical
      range = range < maxLagMeters

      paramsSpherical = [nugget, sill, range]


;     WARNING: Attempts to use the "tied" field (e.g., to ensure that
;     the nugget is less than the sill) in the PARINFO structure for 
;     MPCURVEFIT have resulted in bad fits, we do not use that field,
;     even though controlling the relationship between the nugget and
;     sill is something we would prefer.

      paramsInfo = REPLICATE({value: 0.0D, $
                              fixed: 0, $
                              limited: [0, 0], $
                              limits: [0.0D, 0.0D], $
                              tied: '', $
                              parname: '', $
                              step: 0.0D, $
                              relstep: 0.0D, $
                              mpside: 0, $
                              mpmaxstep: 0.0, $
                              mpprint: 0}, 3)

      maxEmpVario = MAX(empVario, ind)
      maxSillPermitted = maxEmpVario + 2.0D * SQRT(empVarioVariance[ind])

      paramsInfo[0].value = nugget
      paramsInfo[0].limited = [1,1]
      paramsInfo[0].limits = [0.0D, maxSillPermitted] ; nugget gt 0 mainly
      paramsInfo[0].tied = 'p[0] < p[1]' ; nugget le sill
      paramsInfo[0].tied = 'p[0] < (0.5 * p[1])' ; nugget less than half sill
;paramsInfo[0].tied = 'p[0] < 0.1 < p[1]' ; nugget no larger than 0.1
;                                         ; but also must be less than sill
      paramsInfo[0].parname = 'nugget'

      paramsInfo[1].value = sill
      paramsInfo[1].limited = [1,1]
      paramsInfo[1].limits = [0.0D, maxSillPermitted] ; sill gt 0 mainly
      paramsInfo[1].parname = 'sill'

      paramsInfo[2].value = range
      paramsInfo[2].parname = 'range'
      if (minRangeSpherical eq maxLagMeters) then begin
          if (range ne minRangeSpherical) then STOP ; PROGRAMMING ERROR
          paramsInfo[2].fixed = 1
      endif else begin
          paramsInfo[2].limited = [1,1]
          paramsInfo[2].limits = [minRangeSpherical, maxLagMeters]
      endelse

;+
;     Verify that ranges have nonzero width.
;-
      if ((paramsInfo[0].limits[0] eq paramsInfo[0].limits[1]) or $
          (paramsInfo[1].limits[0] eq paramsInfo[1].limits[1])) then begin
          ERR_MSG, 'WARNING: poor empirical variogram results; ' + $
                   'no variogram fit possible.'
          RETURN
      endif


;     Compute weights for semivariogram fitting.

      weightFactor = 1.0D
      if KEYWORD_SET(shortLagWeightBias) then $
          weightFactor = (maxLagMeters - lagMeters) / maxLagMeters
      fitWeight = weightFactor^2.0D * 1.0D / empVarioVariance

      ;; SPHERICAL_SEMIVARIOGRAM, lagMeters, paramsSpherical, initialVario
      ;; initialChiSq = TOTAL((empVario - initialVario)^2.0D * ABS(fitWeight))

      svSFit = MPCURVEFIT(lagMeters, $
                          empVario, $
                          fitWeight, $
                          paramsSpherical, $
                          FUNCTION_NAME = 'SPHERICAL_SEMIVARIOGRAM', $
                          PARINFO = paramsInfo, $
                          ITMAX = 1000, $
                          ITER = numIterations, $
                          /NODERIVATIVE, $
                          /QUIET, $
                          ERRMSG = mpcfErrMsg, $
                          STATUS = status)

      if ((status eq 0) or (status gt 4)) then begin

          if (minRangeSpherical eq maxLagMeters) then begin
              ERR_MSG, 'WARNING: Spherical semivariogram fit using ' + $
                       'MPCURVEFIT failed with status ' + $
                       STRCOMPRESS(status, /REMOVE_ALL) + $
                       ' (' + mpcfErrMsg + ').'
              RETURN
          endif


;         Attempt fits for a series of fixed ranges in the
;         [minRangeSpherical, maxLagMeters] range.

          range = minRangeSpherical

          allRange = []
          allRMSE = []

          origStatus = status
          origErrMsg = mpcfErrMsg

          while (range le maxLagMeters) do begin

              paramsSpherical = [nugget, sill, range]

              paramsInfo[2].limits = [range, range]
              paramsInfo[2].fixed = 1

              svSFit = MPCURVEFIT(lagMeters, $
                                  empVario, $
                                  fitWeight, $
                                  paramsSpherical, $
                                  FUNCTION_NAME = 'SPHERICAL_SEMIVARIOGRAM', $
                                  PARINFO = paramsInfo, $
                                  ITMAX = 1000, $
                                  ITER = numIterations, $
                                  /NODERIVATIVE, $
                                  /QUIET, $
                                  ERRMSG = mpcfErrMsg, $
                                  STATUS = status)

              if ((status ne 0) and (status le 4)) then begin
                  allRange = [allRange, range]
                  allRMSE = [allRMSE, $
                             SQRT(TOTAL((empVario - svSFit)^2.0) / $
                                  numLags)]
              endif else begin
                  print, 'no fit for range ' + STRCRA(range)
                  print, status, mpcfErrMsg
              endelse

              range = range + 5.0D3

          endwhile

          if (N_ELEMENTS(allRange) lt 2) then begin

              ERR_MSG, 'WARNING: All attempts at spherical ' + $
                       'semivariogram fit using ' + $
                       'MPCURVEFIT failed; status ' + $
                       STRCOMPRESS(origStatus, /REMOVE_ALL) + $
                       ' (' + origErrMsg + ').'
              RETURN

          endif else begin

;+
;             Use the best fit obtained under "plan B".
;-
              minRMSE = MIN(allRMSE, ind)
              range = allRange[ind]
              paramsSpherical = [nugget, sill, range]

              paramsInfo[2].limits = [range, range]
              paramsInfo[2].fixed = 1

              svSFit = MPCURVEFIT(lagMeters, $
                                  empVario, $
                                  fitWeight, $
                                  paramsSpherical, $
                                  FUNCTION_NAME = 'SPHERICAL_SEMIVARIOGRAM', $
                                  PARINFO = paramsInfo, $
                                  ITMAX = 1000, $
                                  ITER = numIterations, $
                                  /NODERIVATIVE, $
                                  /QUIET, $
                                  ERRMSG = mpcfErrMsg, $
                                  STATUS = status)

              if ((status eq 0) or (status gt 4)) then begin
                  ERR_MSG, 'PROGRAMMING ERROR'
                  RETURN
              endif

          endelse

      endif

;      RMSESpherical = SQRT(MEAN(yErrS^2.0D))

      RMSESpherical = SQRT(TOTAL((empVario - svSFit)^2.0) / numLags)

      ind = WHERE(lagMeters lt paramsSpherical[2], count)
      if (count gt 0) then begin
          RMSESphericalInRange = $
              SQRT(TOTAL((empVario[ind] - svSFit[ind])^2.0) / count)
;          PRINT, 'In-range RMSE for spherical semivariogram fit: ' + $
;                 STRCRA(RMSESphericalInRange)
      endif

      ;; PRINT, 'Spherical variogram results:'
      ;; PRINT, 'Nugget: ', paramsSpherical[0]
      ;; PRINT, 'Sill: ', paramsSpherical[1]
      ;; PRINT, 'Range: ' , paramsSpherical[2]
      ;; PRINT, 'RMSE: ', RMSESpherical

      ;; finalChiSq = TOTAL((empVario - svSFit)^2.0D * ABS(fitWeight))

      if KEYWORD_SET(showPlot) then begin
          OPLOT, lagMeters / 1000, svSFit, COLOR = 100
          XYOUTS, lagMeters[numLags - 1] / 1000.0, svSFit[numLags - 1], 'S' 
;          OPLOT, lagMeters / 1000.0, initialVario, LINESTYLE = 1
          ;; WSET_OR_WINDOW, 4, XSIZE = 800, YSIZE = 400
          ;; PLOT, lagMeters / 1000.0D, fitWeight, PSYM = -4
          ;; if (finalChiSq gt initialChiSq) then STOP
          ;; stop
      endif

  endif

  gsStatus = 1

  RETURN

end

