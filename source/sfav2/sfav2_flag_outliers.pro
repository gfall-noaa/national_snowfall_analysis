FUNCTION SFAV2_FLAG_OUTLIERS, $
    SiteID, $           ; site identifier array (STRING)
    Lon, $              ; site longitude array, degrees (-180 to 180; DOUBLE)
    Lat, $              ; site latitude array, degrees (DOUBLE)
    Elev, $             ; site elevation, meters (no-data value -9999; LONG)
    Obs, $              ; site obs associated with Val (if not the same; FLOAT)
    Val, $              ; site values to check as possible outliers (FLOAT)
    Ndv, $              ; no-data value (FLOAT)
    HoodRadiusMeters, $
    MinHoodPoints, $
    MinAbsVal, $        ; minimum abs. value of Val to consider; zero disables.
    SDFloor, $
    MPerDegLonRef, $    ; smallest meters per degree longitude
    MPerDegLatRef, $    ; smallest meters per degree latitude
    KEEP_FLAG = keepFlag, $     ; 0/1 for exemption from outlier test.
    DISTANCE_PRECISION = DistPrecision, $
    VERBOSE = verbose, $
    CHECK_ID = CheckID

; Perform a Grubbs test for outliers, with a few special features for
; snowfall analysis. For each point, we perform a Grubbs test for all
; points within HoodRadiusMeters of that point. If the Val value in
; that neighborhood farthest from the mean is flagged by the test, and
; corresponds to the central point (the point in question), then that
; point is flagged as an outlier.
  
; The SDFloor argument is the smallest neighborhood standard deviation
; that will be applied. If the Val values in the neighborhood of a
; point have a smaller standard deviation than SDFloor, then the
; Grubbs test is still performed, but the neighborhood standard
; deviation in that test is replaced by SDFloor. This feature allows
; us to avoid doing overly aggressive outlier tests on neighborhoods
; where all the Val values are fairly small, relative to the units of
; that variable.

; 2018-11-21, GF - Fixed bug where GCritMin was accidentally called
;                  "GCritical" when it was set to !NULL. This bug
;                  meant that if GCritMin was set to 2.0 *
;                  ABS(hoodElevTStat) for any point, then that same
;                  value of GCritMin would be applied to other points
;                  that followed it in the "for sc" loop, essentially
;                  rendering the outlier test far too gentle to be
;                  effective.
;                  Added more information and modified formatting for
;                  outlier details, and for "checkID" results.

; Check arguments.

  numPoints = N_ELEMENTS(Val)

  if ((N_ELEMENTS(SiteID) ne numPoints) or $
      (N_ELEMENTS(Lon) ne numPoints) or $
      (N_ELEMENTS(Lat) ne numPoints) or $
      (N_ELEMENTS(Elev) ne numPoints) or $
      (N_ELEMENTS(Obs) ne numPoints)) then begin
      ERR_MSG, 'Inconsistent input array sizes.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(ISA(SiteID, 'STRING')) then begin
      ERR_MSG, 'Input site ID variable must be a STRING array.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(ISA(Lon, 'DOUBLE')) then begin
      ERR_MSG, 'Input longitude variable must be a DOUBLE array.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(ISA(Lat, 'DOUBLE')) then begin
      ERR_MSG, 'Input latitude variable must be a DOUBLE array.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(ISA(Elev, 'LONG')) then begin
      ERR_MSG, 'Input elevation variable must be a LONG array.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(ISA(Obs, 'FLOAT')) then begin
      ERR_MSG, 'Input observations must be a FLOAT array.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(ISA(Val, 'FLOAT')) then begin
      ERR_MSG, 'Input values must be a FLOAT array.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(ISA(Ndv, 'FLOAT')) then begin
      ERR_MSG, 'No-data value must be a FLOAT.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if (KEYWORD_SET(keepFlag) and ARG_PRESENT(keepFlag)) then begin

      if (N_ELEMENTS(keepFlag) ne numPoints) then begin
          ERR_MSG, 'List of locations in KEEP_FLAG must match ' + $
                   'other input arrays.'
          RETURN, -1
      endif

  endif else begin


;     Default "keep" flag to "no" (0B).

      keepFlag = BYTARR(numPoints)

  endelse

  if NOT(KEYWORD_SET(DistPrecision)) then begin
      DistPrecision = 2
  endif else begin
      if ((DistPrecision lt 0) or (DistPrecision gt 3)) then begin
          ERR_MSG, 'DISTANCE_PRECISION must be 0, 1, 2, or 3.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
      endif
  endelse

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Performing Grubbs test for outliers.'


; Repeat calculation of max lag degrees estimates.

;  HoodRadiusMeters = vParamsSpher[2]
  hoodRadiusDegLon = HoodRadiusMeters / MPerDegLonRef
  HoodRadiusDegLat = HoodRadiusMeters / MPerDegLatRef


; A minimum neighborhood population of 6 values is recommended for the
; Grubbs test. 

  if (MinHoodPoints lt 6) then $
      ERR_MSG, 'WARNING: minimum neighborhood points setting (' + $
               STRCRA(MinHoodPoints) + ' should be at least 6.'

  confidenceLevel = 0.99D

  outlierFlag = BYTARR(numPoints)


; Adjust !EXCEPT system variable. The Grubbs test throws a lot of
; underflow errors and there is no point in seeing that.

  oldExcept__ = !EXCEPT
  !EXCEPT = 0

  numOutliers = 0L
  foundNoData = 0

  for sc = 0, numPoints - 1 do begin

      check = 0
      if (KEYWORD_SET(CheckID) and ARG_PRESENT(CheckID)) then begin
          ic = WHERE(CheckID eq SiteID[sc], count)
          if (count eq 1) then begin
              check = 1
              ic = ic[0]
          endif else begin
              if (count ne 0) then STOP
          endelse
      endif

      if (Val[sc] eq Ndv) then begin
          if NOT(foundNoData) then $
              ERR_MSG, 'WARNING: no-data value found; these are NOT ' + $
                       'flagged as outliers and should not really be ' + $
                       'here.'
          if check then USR_MSG, SiteID[sc] + ' value is NDV.'
          foundNoData = 1
          CONTINUE
      endif

      if (keepFlag[sc] eq 1) then begin
          if check then USR_MSG, SiteID[sc] + ' has its "keep" flag set.'
          CONTINUE              ; exempt from test
      endif

      if (ABS(Val[sc]) lt ABS(MinAbsVal)) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' value ' + $
                       STRCRA(Val[sc]) + ' is too small for ' + $
                       'the outlier check.'
          CONTINUE
      endif

;-----v
;; ;     Here is how you find neighbors without repeating any distance
;; ;     measurements:

;;       j = assimPointsInd[sc]
;;       allDist = all_dist_2d[0:all_n_1d[j] - 1L, j]
;;       allK = all_k_2d[0:all_n_1d[j] - 1L, j]

;;       inCirc__ = WHERE(allDist lt HoodRadiusMeters, countInCirc__)
;;       ; relative to row of allDist

;;       if (countInCirc__ lt (MinHoodPoints - 1L)) then CONTINUE


;; ;     Convert indices of all_dist_2d row (allDist) to indices of
;; ;     full observation array.

;;       inCirc__ = allK[inCirc__]
;;       ; relative to all obs


;; ;     Intersect inCirc__ with assimPointsInd to remove points not
;; ;     included in this assimilation. Also, get the indices of these
;; ;     points relative to assimPointsInd using the INDICES_A keyword.

;;       inCirc__ = CGSETINTERSECTION(assimPointsInd, inCirc__, $
;;                                    INDICES_A = inCirc2__)

;;       if (inCirc__[0] eq -1) then CONTINUE
;;       countInCirc__ = N_ELEMENTS(inCirc__)
;;       if (countInCirc__ lt (MinHoodPoints - 1L)) then CONTINUE


;; ;     The returned values of INDICES_A above are the indices of
;; ;     neighborhood points relative to assimPointsInd.

;;       inCirc__ = TEMPORARY(inCirc2__)
;;       ; relative to assimPointsInd


;; ;     Add the current point.

;;       inCirc__ = [sc, inCirc__]
;;       countInCirc__++
;;       iii__ = 0L


;; ;     Eliminate correct negatives.

;; ;      abcFlagInCirc = abcFlag[inCirc__]
;;       ind = WHERE(abcFlag[inCirc__] eq 1, countInCirc__)
;;       if (countInCirc__ lt MinHoodPoints) then CONTINUE
;;       inCirc__ = inCirc__[ind]
;;       ;; if (countInCirc__ lt 20) then begin
;;       ;;     print, abcFlag[in
;;       ;; endif


;; ;     Extract elevation and delta values for all neighborhood points.

;;       elevInCirc__ = assimElev[inCirc__]
;;       valInCirc__ = zAssim[inCirc__]
;-----^

;-----v
;     Here is how you find neighbors if willing to repeat some
;     distance calculations that have already been performed.

      inBoxInd = $
          WHERE((Lon gt (Lon[sc] - hoodRadiusDegLon)) and $
                (Lon lt (Lon[sc] + hoodRadiusDegLon)) and $
                (Lat gt (Lat[sc] - HoodRadiusDegLat)) and $
                (Lat lt (Lat[sc] + HoodRadiusDegLat)), inBoxCount)

      if (inBoxCount lt MinHoodPoints) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' has at most ' + $
                       STRCRA(inBoxCount - 1) + ' neighbors.'
          CONTINUE
      endif


;     Spend a DISTANCE call to eliminate points in corners.

      dInBox = DISTANCE(DistPrecision, Lon[sc], Lat[sc], $
                        Lon[inBoxInd], Lat[inBoxInd])

      boxInHoodInd = WHERE((dInBox lt HoodRadiusMeters), inHoodCount)
      if (inHoodCount lt MinHoodPoints) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' has only ' + $
                       STRCRA(inHoodCount - 1) + ' neighbors.'
          CONTINUE
      endif

      inHoodInd = inBoxInd[boxInHoodInd]

      hoodElev = Elev[inHoodInd] ;elevInBox[boxInHoodInd]
      hoodVal = Val[inHoodInd] ;valInBox[boxInHoodInd]

      iii = WHERE(inHoodInd eq sc, count)
      if (count ne 1) then STOP ; PROGRAMMING ERROR
;-----^

      hoodValMean = MEAN(hoodVal)
      hoodValSD = STDDEV(hoodVal)
      hoodValTStat = -999.0
      if (hoodValSD gt 0.0) then $
          hoodValTStat = (Val[sc] - hoodValMean) / hoodValSD
      hoodValMin = MIN(hoodVal)
      hoodValMax = MAX(hoodVal)
      hoodValRange = hoodValMax - hoodValMin
      if (hoodValRange lt 1.0e-8) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' has a very narrow range of data (' + $
                       STRCRA(hoodValRange) + ' in its neighborhood.'
          CONTINUE
      endif
      ind = WHERE((hoodVal ge (hoodValMean - hoodValSD)) and $
                  (hoodVal lt (hoodValMean + hoodValSD)), $
                  count)
      oneSigmaPercent = FLOAT(count) / FLOAT(inHoodCount) * 100.0


;     Elevations need to be handled carefully because many are no-data
;     values (-9999).

      hoodElevMean = -9999
      hoodElevSD = -9999
      hoodElevTStat = -999.0
      hoodElevMin = -9999
      hoodElevMax = -9999
      hoodElevRange = -9999
      ind = WHERE(hoodElev ne -9999, count)
      if (count ge MinHoodPoints) then begin
          hoodElevMean = ROUND(MEAN(FLOAT(hoodElev[ind])))
          hoodElevSD = ROUND(STDDEV(FLOAT(hoodElev[ind])))
          if (hoodElevSD gt 0) then $
              hoodElevTStat = (FLOAT(Elev[sc] - hoodElevMean)) / $
                              FLOAT(hoodElevSD)
          hoodElevMin = MIN(hoodElev[ind])
          hoodElevMax = MAX(hoodElev[ind])
          hoodElevRange = hoodElevMax - hoodElevMin
      endif


;     CONTINUE if the site is at an unusually low or high elevation
;     for its neighborhood.

      if ((hoodElevTStat ne -999.0) and $
          (ABS(hoodElevTStat) gt 2.0)) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' has an elevation t-statistic of ' + $
                       STRCRA(hoodElevTStat) + '. Its criterion for ' + $
                       'receiving outlier status will be loosened. ' + $
                       'It has a t-statistic ' + $
                       'of ' + STRCRA(hoodValTStat) + ' on its data.'
          GCritMin = 2.0 * ABS(hoodElevTStat)
;          CONTINUE
      endif else GCritMin = !NULL


;     Clear math errors.

      xx = CHECK_MATH()

;      if NOT(NORMAL_DIST(hoodVal)) then CONTINUE ; (would be nice)
      flag = GRUBBS_TEST(hoodVal, confidenceLevel, $
                         STD_DEV_FLOOR = SDFloor, $
                         G_CRITICAL_MIN = GCritMin, $
                         G_CRITICAL_GRUBBS = GCritGrubbs, $
                         SAMPLE_SIZE = nGrubbs, $
                         MAX_G = maxGGrubbs)
      if (N_ELEMENTS(flag) ne inHoodCount) then begin
          ERR_MSG, 'Grubbs test failed.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
      endif

      if flag[iii] then begin ; OUTLIER FOUND

          outlierFlag[sc] = 1B

          if (KEYWORD_SET(verbose) and (numOutliers eq 0L)) then begin

              PRINT, '      STATION ID ' + $
                     'LONGITUDE ' + $
                     'LATITUDE ' + $
                     '    OBS ' + $
                     '# HOOD ' + $
                     '   VALUE ' + $
                     'HOOD MEAN ' + $
                     'HOOD SD ' + $
                     '   VAL G '  + $
                     '  CRIT G '  + $
                     '<1 SD % ' + $
                     '  RANGE ' + $
                     ' ELEV ' + $
                     'EL MEAN ' + $
                     'EL SD ' + $
                     '    EL G ' + $
                     'RANGE'

              PRINT, '      ---------- ' + $
                     '--------- ' + $
                     '-------- ' + $
                     '    --- ' + $
                     '------ ' + $
                     '   ----- ' + $
                     '--------- ' + $
                     '------- ' + $
                     '  ------ ' + $
                     '  ------ ' + $
                     '------- ' + $
                     '  ----- ' + $
                     ' ---- ' + $
                     '------- ' + $
                     '----- ' + $
                     '    ---- ' + $
                     '-----'

              idFormat = '(A16)'
              lonFormat = '(F9.4)'
              latFormat = '(F8.4)'
              obsFormat = '(F7.3)'
              nHoodFormat = '(I6)'
              valFormat = '(F8.3)'
              valMeanFormat = '(F9.3)'
              valSDFormat = '(F7.3)'
              valTStatFormat = '(F8.3)'
              oneSigmaFormat = '(F7.3)'
              valRangeFormat = '(F7.3)'
              elevFormat = '(I5)'
              elevMeanFormat = '(I7)'
              elevSDFormat = '(I5)'
              elevTStatFormat = '(F8.3)'
              elevRangeFormat = '(I5)'

          endif

          if KEYWORD_SET(verbose) then $
              PRINT, STRING(SiteID[sc], FORMAT = idFormat) + ' ' + $
                     STRING(Lon[sc], FORMAT = lonFormat) + ' ' + $
                     STRING(Lat[sc], FORMAT = latFormat) + ' ' + $
                     STRING(Obs[sc], FORMAT = obsFormat) + ' ' + $
                     STRING(inHoodCount, FORMAT = nHoodFormat) + ' ' + $
                     STRING(Val[sc], FORMAT = valFormat) + ' ' + $
                     STRING(hoodValMean, FORMAT = valMeanFormat) + ' ' + $
                     STRING(hoodValSD, FORMAT = valSDFormat) + ' ' + $
                     STRING(hoodValTStat, FORMAT = valTStatFormat) + ' ' + $
                     STRING(GCritGrubbs, FORMAT = valTStatFormat) + ' ' + $
                     STRING(oneSigmaPercent, FORMAT = oneSigmaFormat) + ' ' + $
                     STRING(hoodValRange, FORMAT = valRangeFormat) + ' ' + $
                     STRING(Elev[sc], FORMAT = elevFormat) + ' ' + $
                     STRING(hoodElevMean, FORMAT = elevMeanFormat) + ' ' + $
                     STRING(hoodElevSD, FORMAT = elevSDFormat) + ' ' + $
                     STRING(hoodElevTStat, FORMAT = elevTStatFormat) + ' ' + $
                     STRING(hoodElevRange, FORMAT = elevRangeFormat)

          numOutliers++

      endif else begin

          if check then $
              USR_MSG, SiteID[sc] + ', with value ' + $
                       STRCRA(STRING(Val[sc], FORMAT = '(F8.3)')) + $
                       ', has a t-statistic of ' + $
                       STRCRA(STRING(hoodValTStat, FORMAT = '(F8.3)')) + $
                       '. It is not a Grubbs outlier. Grubbs test ' + $
                       'critical G value is ' + $
                       STRCRA(STRING(GCritGrubbs, FORMAT = '(F8.3)')) + $
                       ' (sample size ' + STRCRA(LONG(nGrubbs)) + '), ' + $
                       'maximum G value was ' + STRCRA(maxGGrubbs) + $
                       '.'

      endelse

  endfor

  if KEYWORD_SET(verbose) then begin
      if (numOutliers eq 0) then $
          USR_MSG, 'No Grubbs outliers found.' $
      else $
          USR_MSG, 'Identified ' + STRCRA(numOutliers) + $
                   ' Grubbs outliers.'
  endif


; Restore previous value of !EXCEPT.

  !EXCEPT = oldExcept__


  RETURN, outlierFlag


end
