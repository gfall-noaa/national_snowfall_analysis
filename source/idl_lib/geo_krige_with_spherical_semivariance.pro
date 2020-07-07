PRO GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
    Lon_, $
    Lat_, $
    Val_, $
    Ndv_, $                     ; both for input and output
    ParamsSpher, $
    MinLonOut_, $
    MaxLonOut_, $
    MinLatOut_, $
    MaxLatOut_, $
    XResOut_, $
    YResOut_, $
    gridOut, $
    ERROR_VARIANCE = errVarOut, $ ; gridded error variance estimate
    DISTANCE_PRECISION = DistPrecision, $
;    WATCH_ANIMATION = WatchAnimation, $
    CROSS_VAL_POINTS = valKrige, $
    CROSS_VAL_RMSE = cvRMSE, $
    USE_KRM = useKRM, $ ; use kriging by radial means - much faster than OK
    SKIP_GRUBBS = skipGrubbs, $ ; skip Grubbs outlier test
    STAGGER_KRM_MESH = staggerKRMMesh, $
    NUM_KRM_RADII = numKRMRadii, $
    NUM_KRM_ANGLES = numKRMAngles, $
    MIN_LAG_METERS = minLagMeters, $ ; must be obs within this distance
    MIN_OUTLIER_DIFF = minOutlierDiff, $
    MIN_NEIGHBORHOOD_POINTS = minHoodPoints, $ ; minimum # points for kriging
    NEIGHBORHOOD_TOLERANCE = hoodTolerance, $ ; minimum range for kriging
    VERBOSE = verbose, $
    ;; ALL_DISTANCES_2D = all_dist_2d, $
    ; ;ALL_INDICES_2D = all_k_2d, $
    ;; ALL_COUNT_1D = all_n_1d, $
    KEEP_FLAG = keepFlag, $ ; 0/1 for exemption from outlier test.
    USE_FLAG = useFlag, $ ; 0/1 for not used/used in kriging
    KRIGE_WEIGHT_ADJUST = krigeWeightAdjust, $ ; krige weighting adjustment
    WALL_TIME = wallTime, $
    MEAN_POINTS_PER_SOLVER = meanPointsPerSolver,$
    NUM_SOLVERS = numSolvers


; Use kriging to interpolate data array Val_ with locations given by
; Lon_ and Lat_, using the spherical semivariogram model parameters
; specified in ParamsSpher. Those are:

; nugget: ParamsSpher[0]
; sill: ParamsSpher[1]
; range: ParamsSpher[2] (in units of meters)

; KEEP_FLAG:
;
;   Flag indicating whether points should be exempted from outlier
;   tests.
;
;   If this keyword is sent as input, it should provide an array of
;   bytes equal in dimension to the input Lon_, Lat_, and Val_
;   arrays, and should consist of ones and zeroes. A value of zero
;   indicates that an observation is fair game for outlier tests, but
;   it does NOT indicate that an observation will not be used. A
;   value of one indicates that an observation should be exempt from
;   outlier tests. If the SKIP_GRUBBS keyword is set, this keyword is
;   irrelevant. The contents of the KEEP_FLAG array are not modified
;   by this procedure.
;
; USE_FLAG:
;
;   Flag indicating whether a point will be or is being used in
;   assimilation.
;
;   If this keyword is sent as input, it should provide an array of
;   bytes equal in dimension to the input Lon_, Lat_, and Val_ arrays,
;   and should consist of ones and zeroes. A value of zero indicates
;   that an observation will not be included in assimilation, a value
;   of one indicates that an observation will be included. Whereas the
;   KEEP_FLAG array is not modified by this procedure, the USE_FLAG
;   can be. If an input point has a Val_ of Ndv_, or if its
;   coordinates place it out of bounds, or it is flagged as an outlier
;   (assuming SKIP_GRUBBS is not set), then the USE_FLAG for that
;   point will be set to zero.
;
;   If the USE_FLAG is not sent as input, or is sent uninitialized,
;   then it defaults to an aray of ones, and is adjusted accordingly
;   through no-data, out-of-bounds, and outlier tests as needed.
;
;   If an external outlier test is preferred over the Grubbs test
;   included in this procedure, then its outcome can be established in
;   this procedure by sending the resulting USE_FLAG data as input and
;   and setting the SKIP_GRUBBS keyword.
;
;   If USE_FLAG is provided as input but SKIP_GRUBBS is not set, then
;   any point with a USE_FLAG value of 1 will still be subjected to
;   the outlier test within this procedure.


; Greg Fall, OWP Snow Analysis and Remote Sensing Center, July 2016

  COMMON info, Message


; Set up a structure of wall times for different components of the
; process.

  wallTime = 0.0D
  t1Full = SYSTIME(/UTC, /SECONDS)


; Set distance precision.
;   0 : Very crude Eulidean
;   1 : Crude Euclidean
;   2 : Great circle
;   3 : Ellipsoidal

  if NOT(KEYWORD_SET(DistPrecision)) then begin
      DistPrecision = 2
  endif else begin
      if ((DistPrecision lt 0) or (DistPrecision gt 3)) then begin
          ERR_MSG, 'DISTANCE_PRECISION must be 0, 1, 2, or 3.'
          RETURN
      endif
  endelse

  if NOT(KEYWORD_SET(hoodTolerance)) then hoodTolerance = 0.0D
  if NOT(KEYWORD_SET(minOutlierDiff)) then minOutlierDiff = 0.0D

  if KEYWORD_SET(verbose) then begin
      if (minOutlierDiff eq 0) then $
          USR_MSG, 'Minimum absolute value to consider in outlier test: ' + $
                   STRCRA(minOutlierDiff) + $
                   ' (i.e., all points will be tested).' $
      else $
          USR_MSG, 'Minimum absolute value to consider in outlier test: ' + $
                   STRCRA(minOutlierDiff)
  endif

  if KEYWORD_SET(minLagMeters) then begin
      if ((minLagMeters lt 0.0D) or $
          (minLagMeters gt paramsSpher[2])) then begin
          ERR_MSG, 'Invalid MINIMUM_LAG_METERS setting ' + $
                   STRCRA(minLagMeters)
          STOP
      endif
  endif else minLagMeters = paramsSpher[2]


; Convert geometry arguments to DOUBLE.

  if NOT(ISA(Lon_, 'DOUBLE')) then $
      lon = DOUBLE(Lon_) $
  else $
      lon = Lon_
  if NOT(ISA(Lat_, 'DOUBLE')) then $
      lat = DOUBLE(Lat_) $
  else $
      lat = Lat_
  if NOT(ISA(MinLonOut_, 'DOUBLE')) then $
      minLonOut = DOUBLE(MinLonOut_) $
  else $
      minLonOut = MinLonOut_
  if NOT(ISA(MaxLonOut_, 'DOUBLE')) then $
      maxLonOut = DOUBLE(MaxLonOut_) $
  else $
      maxLonOut = MaxLonOut_
  if NOT(ISA(MinLatOut_, 'DOUBLE')) then $
      minLatOut = DOUBLE(MinLatOut_) $
  else $
      minLatOut = MinLatOut_
  if NOT(ISA(MaxLatOut_, 'DOUBLE')) then $
      maxLatOut = DOUBLE(MaxLatOut_) $
  else $
      maxLatOut = MaxLatOut_
  if NOT(ISA(XResOut_, 'DOUBLE')) then $
      xResOut = DOUBLE(XResOut_) $
  else $
      xResOut = XResOut_
  if NOT(ISA(YResOut_, 'DOUBLE')) then $
      yResOut = DOUBLE(YResOut_) $
  else $
      yResOut = YResOut_


; Check types of val and ndv.

  if NOT(ISA(Val_, 'FLOAT')) then begin
      ERR_MSG, 'WARNING: input data expected to be of type FLOAT. ' + $
               'Received type is ' + DATA_TYPE_STR(Val_) + '.'
      val = FLOAT(Val_)
  endif else begin
      val = Val_
  endelse

  if NOT(ISA(Ndv_, 'FLOAT')) then begin
      ERR_MSG, 'WARNING: input no-data value expected to be of type ' + $
               'FLOAT. Received type is ' + DATA_TYPE_STR(Ndv_) + '.'
      ndv = FLOAT(Ndv_)
  endif else begin
      ndv = Ndv_
  endelse


; Confirm that input arrays of points are consistent.

  numPoints = N_ELEMENTS(lon)

  if (N_ELEMENTS(lat) ne numPoints) then begin
      ERR_MSG, 'Input latitude and longitude arrays must be the same size.'
      RETURN
  endif

  if (N_ELEMENTS(val) ne numPoints) then begin
      ERR_MSG, 'Input data must match latitude and longitude arrays.'
      RETURN
  endif

  if (KEYWORD_SET(keepFlag) and ARG_PRESENT(keepFlag)) then begin
      if (N_ELEMENTS(keepFlag) ne numPoints) then begin
          ERR_MSG, 'List of locations in KEEP_FLAG must match ' + $
                   'latitude and longitude arrays.'
          RETURN
      endif
      if KEYWORD_SET(skipGrubbs) then $
          ERR_MSG, 'WARNING: No Grubbs test will be performed; ' + $
                   'KEEP_FLAG data sent to this procedure will be ignored.'
  endif else begin


;     Default "keep" flag to "no" (0B).

      keepFlag = MAKE_ARRAY(numPoints, /BYTE, VALUE = 0B)
  endelse

  if (KEYWORD_SET(useFlag) and ARG_PRESENT(useFlag)) then begin
      if (N_ELEMENTS(useFlag) ne numPoints) then begin
          ERR_MSG, 'List of locations in USE_FLAG must match ' + $
                   'longitude and latitude arrays.'
          RETURN
      endif
  endif else begin


;     Default "use" flag to "yes" (1B).

      useFlag = MAKE_ARRAY(numPoints, /BYTE, VALUE = 1B)
  endelse

  if (KEYWORD_SET(krigeWeightAdjust) and ARG_PRESENT(krigeWeightAdjust)) $
      then begin

      if (N_ELEMENTS(krigeWeightAdjust) ne numPoints) then begin
          ERR_MSG, 'ERROR: Dimension of KRIGE_WEIGHT_ADJUST ' + $
                   'must match longitude and latitude arrays.'
          RETURN
      endif

      if (NOT(ISA(krigeWeightAdjust, 'FLOAT')) and $
          NOT(ISA(krigeWeightAdjust, 'DOUBLE'))) then begin
          ERR_MSG, 'ERROR: KRIGE_WEIGHT_ADJUST must be a FLOAT or DOUBLE.'
          RETURN
      endif

      if (MIN(krigeWeightAdjust) le 0.0) then begin
          ERR_MSG, 'ERROR: all KRIGE_WEIGHT_ADJUST values must be positive.'
          RETURN
      endif

      if (MAX(krigeWeightAdjust) eq 0.0) then begin
          ERR_MSG, 'ERROR: KRIGE_WEIGHT_ADJUST must include nonzero ' + $
                   'values.'
          RETURN
      endif

  endif else begin

      krigeWeightAdjust = REPLICATE(1.0, numPoints)

  endelse


; First data-weeding: get rid of no-data values.

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Number of points: ' + STRCRA(numPoints)

  ind = WHERE(val eq ndv, count)
  if (count eq numPoints) then begin
      ERR_MSG, 'All input data are no-data values.'
      RETURN
  endif

  if (count gt 0) then begin
      useFlag[ind] = 0B
      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Removal of no-data values reduced # of points to ' + $
                   STRCRA(TOTAL(useFlag, /INT))
  endif

  ;; lon = lon[ind]
  ;; lat = lat[ind]
  ;; val = val[ind]
  ;; if (count lt n) then $
  ;;     PRINT, 'Removal of no-data values reduced # of points to ' + $
  ;;            STRCRA(count)
  ;; n = count


; Verify that output grid geometry is perfect.

  nxOut = ROUND((maxLonOut - minLonOut) / xResOut)
  if (nxOut le 0) then begin
      ERR_MSG, 'Invalid min/max output grid longitudes of ' + $
               STRCRA(minLonOut) + ' and ' + STRCRA(maxLonOut) + '.'
      RETURN
  endif

  nyOut = ROUND((maxLatOut - minLatOut) / xResOut)
  if (nyOut le 0) then begin
      ERR_MSG, 'Invalid min/max output grid latitudes of ' + $
               STRCRA(minLatOut) + ' and ' + STRCRA(maxLatOut) + '.'
      RETURN
  endif

  xErr = ABS(nxOut * xResOut - (maxLonOut - minLonOut))
  if (xERR gt 1.0D-8) then begin
      ERR_MSG, 'Output longitude range and resolution do not specify ' + $
               'an integer number of columns.'
      RETURN
  endif

  yErr = ABS(nyOut * yResOut - (maxLatOut - minLatOut))
  if (yErr gt 1.0D-8) then begin
      ERR_MSG, 'Output latitude range and resolution do not specify ' + $
               'an integer number of rows.'
      RETURN
  endif


; Set the maximum separation distance between a location and a point
; that might influence it. This distance will serve multiple
; purposes. It will be
;
;   1. The distance by which the domain is expanded, if possible,
;   beyond the output domain limits, for collecting input data, so
;   that locations near the edges of the output domain benefit from a
;   complete analysis as well as locations well within the output
;   domain. This will only happen if the input data include those
;   locations, naturally.
;
;   2. The radius of the "neighborhood" associated with a location for
;   purposes of outlier tests.
;
;   3. The radius of the "neighborhood" used for spatial interpolation.
;
; For a spherical semivariogram model, the covariance reaches zero at
; the range, so this is the logical value to use.

  maxLagMeters = ParamsSpher[2]


; This analysis will involve numerous distance calculations at each
; point location. The first step in avoiding too many of those, which
; are expensive, is to calculate a lon/lat box associated with
; maxLagMeters.

; The length of a degree in latitude is smallest at the equator, for the
; typical ellipsoid, so use the 3.6 arc seconds about the equator to
; estimate meters per degree latitude.

  mPerDegLatRef = $
    DOUBLE(FLOOR(DISTANCE(DistPrecision, $
                          0.0D, 0.0005D, 0.0D, -0.0005D) * 1000.0D))
  maxLagDegLat = maxLagMeters / mPerDegLatRef


; The length of a degree in longitude is smallest at the highest latitude, so
; use that latitude to estimate meters per degree longitude.

  maxLatRef = (ABS(maxLatOut) > ABS(minLatOut)) + maxLagDegLat
  mPerDegLonRef = $
      DOUBLE(FLOOR(DISTANCE(DistPrecision, 0.0D, maxLatRef, 1.0D, maxLatRef)))
  maxLagDegLon = maxLagMeters / mPerDegLonRef


; The input data domain for this process should, if possible, exceed
; the output domain, so that near the boundaries the influence of
; points that are nearby, but not strictly within the bounds of the
; output domain, can be realized. The addition of this "analysis halo"
; means that output ("Out") and analysis ("Anl") must be handled very
; carefully throughout the remainder of this procedure.


; Make sure padLon and padLat are an integer number of output grid cells.

  extraRows = CEIL(maxLagDegLat / yResOut)
  padLat = yResOut * extraRows
  extraCols = CEIL(maxLagDegLon / xResOut)
  padLon = xResOut * extraCols


; Expand the output bounding box to define the analysis domain.

  minLonAnl = minLonOut - padLon
  maxLonAnl = maxLonOut + padLon
  minLatAnl = minLatOut - padLat
  maxLatAnl = maxLatOut + padLat


; Second data-weeding: get rid of data outside the analysis domain.

  ;; ind = WHERE((lon gt minLonAnl) and $
  ;;             (lon lt maxLonAnl) and $
  ;;             (lat gt minLatAnl) and $
  ;;             (lat lt maxLatAnl), count)
  ;; if (count eq 0) then $
  ;;     MESSAGE, 'All input data are outside the analysis domain.'

  ind = WHERE((useFlag eq 1B) and $
              ((lon le minLonAnl) or $
               (lon ge maxLonAnl) or $
               (lat le minLatAnl) or $
               (lat ge maxLatAnl)), count)
  if (count eq TOTAL(useFlag, /INT)) then begin
      ERR_MSG, 'All remaining input data are outside the analysis domain.'
      RETURN
  endif

  if (count gt 0) then begin
      useFlag[ind] = 0B
      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Removal of points outside the analysis domain ' + $
                   'reduced # of points to ' + STRCRA(TOTAL(useFlag, /INT))
  endif

  if NOT(KEYWORD_SET(skipGrubbs)) then begin


    ;----------------------------------------------------------------;
    ; Perform a Grubbs test. Loop over points. For each point, do a  ;
    ; Grubbs test on all values within maxLagMeters. The test        ;
    ; iterates on this group, but only flags the central point if it ;
    ; as an outlier. Other points in the circle flagged as outliers  ;
    ; will get their own tests.                                      ;
    ;----------------------------------------------------------------;

;     A minimum neighborhood population of 6 values is recommended for
;     the Grubbs test. 

      if NOT(KEYWORD_SET(minHoodPoints)) then minHoodPoints = 6

      confidenceLevel = 0.99D

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Performing pre-kriging Grubbs test for outliers.'

      outlierFlag = BYTARR(numPoints)


;     Adjust !EXCEPT system variable. The Grubbs test throws a lot of
;     underflow errors and there is no point in seeing that.

      oldExcept = !EXCEPT
      !EXCEPT = 0

      for sc = 0, numPoints - 1 do begin


;         Find all observations within maxLagMeters of this point.

          if (useFlag[sc] eq 0) then CONTINUE ; already not using; skip
          if (keepFlag[sc] eq 1) then CONTINUE ; exempted from test; skip

          loni = lon[sc]
          lati = lat[sc]
          vali = val[sc]

          if (ABS(vali) lt ABS(minOutlierDiff)) then CONTINUE

          inBoxInd = WHERE((lon gt loni - maxLagDegLon) and $
                           (lon lt loni + maxLagDegLon) and $
                           (lat gt lati - maxLagDegLat) and $
                           (lat lt lati + maxLagDegLat), inBoxCount)

          if (inBoxCount lt minHoodPoints) then CONTINUE ; no test possible

          lonInBox = lon[inBoxInd]
          latInBox = lat[inBoxInd]
          valInBox = val[inBoxInd]


;         Spend a DISTANCE call to eliminate points in "corners".

          dInBox = DISTANCE(DistPrecision, loni, lati, lonInBox, latInBox)

          boxInHoodInd = WHERE((dInBox lt maxLagMeters), inHoodCount)
          if (inHoodCount lt minHoodPoints) then CONTINUE ; no test possible

          hoodVal = valInBox[boxInHoodInd]

          iii = WHERE(inBoxInd[boxInHoodInd] eq sc, count)
          if (count ne 1) then STOP

          minHoodVal = MIN(hoodVal)
          maxHoodVal = MAX(hoodVal)

          if (minHoodVal eq maxHoodVal) then CONTINUE ; no test needed


;         Clear math errors.

          xx = CHECK_MATH()

          flag = GRUBBS_TEST(hoodVal, confidenceLevel)

          if flag[iii] then outlierFlag[sc] = 1B

      endfor


;     Restore previous value of !EXCEPT.

      !EXCEPT = oldExcept


;     Third data-weeding: get rid of Grubbs outliers.

      ind = WHERE((useFlag eq 1B) and (outlierFlag eq 1B), count)
      if (count eq TOTAL(useFlag, /INT)) then begin
          ERR_MSG, 'All remaining input data were flagged as outliers.'
          RETURN
      endif

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Grubbs test at ' + $
                   STRCRA(ROUND(confidenceLevel * 100.0)) + $
                   '% confidence flagged ' + $
                   STRCRA(count) + ' outliers.'
      if (count gt 0) then begin
          useFlag[ind] = 0B
          if KEYWORD_SET(verbose) then $
              USR_MSG, 'Removal of outliers reduced # of points to ' + $
                       STRCRA(TOTAL(useFlag, /INT))
      endif

  endif


; Compute distances between all observation pairs up to double the
; maxLagMeters. We need those because pairs of observations in the
; neighborhood of an output grid point may be that far apart, and we
; need covariances for all such pairs, even if they are zero.

  if KEYWORD_SET(verbose) then USR_MSG, 'Computing distance matrix.'

  numDistances = numPoints * (numPoints - 1L) / 2L

;  USR_MSG, 'Number of distances between all points: ' + STRCRA(numDistances)


; Predict the maximum number of distances we will have to
; compute. This takes time, but it may prevent a prohibitive memory
; allocation and avoids the very slow method of reallocating with
; every measurement.

  numDistCalcGuess = 0UL
  maxNumNeighborsGuess = 0UL

  for i = 0UL, numPoints - 1UL do begin

      loni = lon[i]
      lati = lat[i]

      boxMinLon = loni - 2.0D * maxLagDegLon
      boxMaxLon = loni + 2.0D * maxLagDegLon
      boxMinLat = lati - 2.0D * maxLagDegLat
      boxMaxLat = lati + 2.0D * maxLagDegLat

      ind = WHERE((lon gt boxMinLon) and $
                  (lon lt boxMaxLon) and $
                  (lat gt boxMinLat) and $
                  (lat lt boxMaxLat), count)

      numDistCalcGuess += count
      if (count gt maxNumNeighborsGuess) then $
          maxNumNeighborsGuess = count

  endfor

  ;; USR_MSG, 'Upper estimate for number of distances < ' + $
  ;;          STRCRA(2 * maxLagMeters) + $
  ;;          ' meters: ' + STRCRA(numDistCalcGuess)
  ;; USR_MSG, 'Estimated maximum number of neighbors: ' + $
  ;;          STRCRA(maxNumNeighborsGuess)


;----------------------------------------------------------------------------;
;
; Calculate distances (only those we need to know) between pairs of
; points.
;
; For a collection of n observation locations, the number of
; between-observation distances is
;
;   numDistances = n * (n - 1) / 2
;
; this set represents a triangular matrix where all the values along
; the diagonal are also zero. A 1-D index "k" for a 6 x 6 matrix of
; these distances (for which numDistances = 15) would look like this:
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
; 2-D option (plan A):
;
; Here, we set up a 2-D array of distances, where the second (row)
; dimension is the site index (n elements), and the first is just
; large enough to hold the neighbors of each site. The matrix is set
; up to be similarly triangular to what is described above, so that
; for each j = 0, n - 1, no more than i < j neighbor distances will be
; defined, and for each distance, the corresponding site index i will
; be stored as a reference in a second 2-D array the same size as the
; distance array. So whereas the 1-D option used k as the reference,
; the 2-D option can keep it simple and use i as the reference. The
; cost of this option is that every row of the matrix must be large
; enough to hold the maximum number of neighbors possible, and so
; there will be a large number of no-data values and a corresponding
; memory burden. However, searching rows of the reference matrix for
; specific values of i will be much faster than searching the 1-D
; reference matrix for specific values of k.
;
; 2-D option (plan B):
;
; Here, we use a 2-D array of distances similar to that described
; above, but we do not attempt to make it triangular. The second (row)
; dimension is the site index again, but this time the redundant
; values are stored. The memory burden will be slightly larger, but
; the advantage is that when neighborhood covariances are assembled,
; all the neighbors for a given row are IN THAT ROW. For example, in
; the "plan A" version, consider allDist_2D[*,9] (i.e., the 10th row
; of the all-distance matrix). That row will only have distances
; between station 9 and any of stations 0 through 8 that are within
; (2 * maxLagMeters) of station 9. To get the rest of the neighbors
; for station 9, you have to loop through all the rest of the rows of
; allDist_2D, searching for a "9"
;
;----------------------------------------------------------------------------;

  if KEYWORD_SET(verbose) then USR_MSG, 'Generating covariance matrix.'

  allDist_2D = MAKE_ARRAY(maxNumNeighborsGuess, numPoints, $
                          /DOUBLE, VALUE = ndv)
  all_i_2D = MAKE_ARRAY(maxNumNeighborsGuess, numPoints, $
                        /LONG, VALUE = -1L)

  if KEYWORD_SET(verbose) then begin
      if (numPoints lt 50) then begin
          hashes = numPoints
          lastProgress = -1
      endif else begin
          hashes = 50
          lastProgress = 0
      endelse
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif

  numDistCalc = 0UL
  maxNumNeighbors = 0UL

  for j = 1UL, numPoints - 1UL do begin

      if KEYWORD_SET(verbose) then begin
          progress = FIX(FLOAT(j) / FLOAT(numPoints - 1) * FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      if (useFlag[j] eq 0B) then CONTINUE
      lonj = lon[j]
      latj = lat[j]

      l = 0UL

      for i = 0UL, j - 1UL do begin

          if (useFlag[i] eq 0B) then CONTINUE
          loni = lon[i]
          lati = lat[i]


;         Locations up to double the maximum separation for covariance
;         calculation may wind up in the same kriging neighborhood.

          lonDiff = ABS(lonj - loni)
          if (lonDiff gt (2.0D * maxLagDegLon)) then CONTINUE
          latDiff = ABS(latj - lati)
          if (latDiff gt (2.0D * maxLagDegLat)) then CONTINUE

          d = DISTANCE(DistPrecision, loni, lati, lonj, latj)
          if (d ge (2.0D * maxLagMeters)) then CONTINUE

          allDist_2D[l, j] = d
          all_i_2D[l, j] = i

          l = l + 1UL
          numDistCalc = numDistCalc + 1UL

      endfor

      if (l gt maxNumNeighbors) then maxNumNeighbors = l

  endfor

  if KEYWORD_SET(verbose) then PRINT, ''

  ;; USR_MSG, 'Number of distance calculations performed/stored: ' + $
  ;;          STRCRA(numDistCalc)
  ;; USR_MSG, 'Maximum number of neighbors: ' + STRCRA(maxNumNeighbors)


; Truncate distance array columns to the maximum number of neighbors.

  allDist_2D = allDist_2D[0UL:maxNumNeighbors - 1UL, *]
  all_i_2D = all_i_2D[0UL:maxNumNeighbors - 1UL, *]


; Create the covariance matrix.

  allCovar_2D = MAKE_ARRAY(maxNumNeighbors, numPoints, /DOUBLE, VALUE = ndv)

  ind = WHERE(allDist_2D ne ndv, count)
  if (count ne numDistCalc) then STOP
;  SPHERICAL_SEMIVARIOGRAM, allDist_2D[ind], ParamsSpher, semivarianceInd
  semivarianceInd = $
      SPHERICAL_SEMIVARIOGRAM_FUNC(allDist_2D[ind], ParamsSpher)
  covarianceInd = ParamsSpher[1] - semivarianceInd ; sill - semivariance
  allCovar_2D[ind] = covarianceInd

  ind = !NULL ; free memory
  semiVarianceInd = !NULL ; free memory
  covarianceInd = !NULL ; free memory


; --- IF DECOMPOSING DOMAIN, HERE IS WHERE YOU WOULD START. ---

  if KEYWORD_SET(useKRM) then begin
      if KEYWORD_SET(staggerKRMMesh) then $
          staggerKRMMesh = 1 $
      else $
          staggerKRMMesh = 0
      if NOT(KEYWORD_SET(numKRMRadii)) then numKRMRadii = 50
      if NOT(KEYWORD_SET(numKRMAngles)) then numKRMAngles = 4
      ;; targetRingWidthMeters = 20.0D3
      ;; if NOT(KEYWORD_SET(numKRMRadii)) then $
      ;;     numKRMRadii = CEIL(maxLagMeters / targetRingWidthMeters)
      ;; if NOT(KEYWORD_SET(numKRMAngles)) then $
      ;;     numKRMAngles = 8
      maxNoNullCells = 0L

  endif


; Initialize output grids.

  gridOut = MAKE_ARRAY(nxOut, nyOut, /FLOAT, VALUE = ndv)
  errVarOut = gridOut ; GF 20190912

  if KEYWORD_SET(verbose) then begin
      if KEYWORD_SET(useKRM) then $
          USR_MSG, 'Kriging by radial means with ' + $
                   STRCRA(numKRMAngles) + ' angles and ' + $
                   STRCRA(numKRMRadii) + ' radii.' $
      else $
          USR_MSG, 'Performing ordinary kriging for all points.'
  endif

  if KEYWORD_SET(verbose) then begin
      if (nyOut lt 50) then begin
          hashes = nyOut
          lastProgress = -1
      endif else begin
          hashes = 50
          lastProgress = 0
      endelse
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif

  maxHoodCount = 0
  numKrigedGridCells = 0

;  timeDist = 0.0D
;  timeKrige = 0.0D
;  timeKRM = 0.0D

;  t1Krige = SYSTIME(/SECONDS)


;; Identify a lon/lat to sample.

;;   sampleLon = -103.54D
;;   sampleLat = 40.70D

;;   sampleLon = -76.74D ; for 2016-01-24 12Z analysis
;;   sampleLat = 39.26D

  ;; sampleLon = -150.0D ; for 2016-11-18 bug - flat result around CO
  ;; sampleLat = 42.0D

  ;; sampleLon = -107.1169D ; for 2016-12-25 bug
  ;; sampleLat = 41.77885D

  ;; sampleCol = FLOOR((sampleLon - minLonOut) / xResOut)
  ;; sampleRow = FLOOR((sampleLat - minLatOut) / yResOut)

  ;; sampleCol = -1 & sampleRow = -1

  timeLocal = 0.0D
  timeKRM = 0.0D

  t1 = SYSTIME(/UTC, /SECONDS)

;  if keyword_set(usekrm) then begin
;      plot, findgen(11)/10 * paramsspher[2] / 1000, findgen(11)/10, $
;            /nodata, xrange = [0, paramsspher[2] / 1000]
;  endif

  numSolvers = 0L
  numSkips = 0L
  totalSolverPoints = 0L

  for rc = 0L, nyOut - 1L do begin

      ;; showRow = 0B
      if KEYWORD_SET(verbose) then begin
          progress = FIX(FLOAT(rc) / FLOAT(nyOut - 1) * FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
              ;; showRow = 1B
          endif
      endif

      gridLat = minLatOut + (rc + 0.5D) * yResOut

      mPerDegLon = DISTANCE(distPrecision, 0.0D, gridLat, 1.0D, gridLat)
      mPerDegLat = DISTANCE(distPrecision, $
                            0.0D, gridLat + 0.0005D, $
                            0.0D, gridLat - 0.0005D) * 1000.0D

      if (nxOut lt 20) then begin
          xHashes = nxOut
          xLastProgress = -1
      endif else begin
          xHashes = 20
          xLastProgress = 0
      endelse

      for cc = 0, nxOut - 1 do begin

          ;; sampleFlag = 0
          ;; if ((cc eq sampleCol) and (rc eq sampleRow)) then begin
          ;;     sampleFlag = 1
          ;;     USR_MSG, 'Sampling process at ' + $
          ;;              STRCRA(cc) + ', ' + STRCRA(rc)
          ;; endif

          ;; showCol = 0B
          ;; if (showRow) then begin
          ;;     xProgress = FIX(FLOAT(cc) / FLOAT(nxOut - 1) * FLOAT(xHashes))
          ;;     if (xProgress gt xLastProgress) then begin
          ;;         xLastProgress = xProgress
          ;;         showCol = 1B
          ;;     endif
          ;; endif

          gridLon = minLonOut + (cc + 0.5D) * xResOut


;         For the current output location, identify observations
;         within maxLagMeters for covariance
;         calculations.

          inBoxInd = WHERE((useFlag eq 1B) and $
                           (ABS(lon - gridLon) lt maxLagDegLon) and $
                           (ABS(lat - gridLat) lt maxLagDegLat), inBoxCount)

          if (inBoxCount lt minHoodPoints) then CONTINUE

          lonInBox = lon[inBoxInd]
          latInBox = lat[inBoxInd]


;         Identify observations in the "box" that fall within
;         maxLagMeters of the grid point; i.e., within the
;         neighborhood of the output location.

;          t1 = SYSTIME(/SECONDS)
          dInBox = DISTANCE(DistPrecision, $
                            gridLon, gridLat, lonInBox, latInBox)
;          t2 = SYSTIME(/SECONDS)
;          timeDist += (t2 - t1)

          boxInHoodInd = WHERE(dInBox lt maxLagMeters, inHoodCount)
          if (inHoodCount lt minHoodPoints) then CONTINUE

          hoodInd = inBoxInd[boxInHoodInd]
          hoodDist = dInBox[boxInHoodInd]

          numClose = TOTAL(hoodDist lt minLagMeters, /INT)
          if (numClose lt 3) then CONTINUE
;          if (MIN(hoodDist) gt minLagMeters) then CONTINUE

          hoodVal = val[hoodInd]
          hoodKWA = krigeWeightAdjust[hoodInd]

          minHoodVal = MIN(hoodVal)
          maxHoodVal = MAX(hoodVal)

          if ((maxHoodVal - minHoodVal) le hoodTolerance) then begin

              gridOut[cc, rc] = TOTAL(hoodVal * hoodKWA) / TOTAL(hoodKWA)
;             GF 20190913 see yellow notebook entry for 2019-09-13
              errVarOut[cc, rc] = $
                  VARIANCE(inHoodCount * hoodVal * hoodKWA / TOTAL(hoodKWA))
              numSkips++
              CONTINUE

          endif; else print, '++++ ', hoodVal

          if (inHoodCount gt maxHoodCount) then begin
              maxHoodCount = inHoodCount
              ;print, '-->', maxHoodCount
          endif


;         Calculate semivariances for points in the neighborhood
;         relative to the output location.

;          SPHERICAL_SEMIVARIOGRAM, hoodDist, ParamsSpher, hoodSemiVar
          hoodSemiVar = $
              SPHERICAL_SEMIVARIOGRAM_FUNC(hoodDist, ParamsSpher)


;         Convert semivariance to covariance.

          hoodCovar = ParamsSpher[1] - hoodSemiVar ; sill - semivariance

          if KEYWORD_SET(useKRM) then begin

              t2 = SYSTIME(/UTC, /SECONDS)
              timeLocal += (t2 - t1)
              t1 = t2

              hoodLon = lon[hoodInd]
              hoodLat = lat[hoodInd]

              ;; if (sampleFlag) then STOP

;              t1KRM = SYSTIME(/SECONDS)
              ;; if sampleFlag then $
              ;;     gridOut[cc, rc] = $
              ;;     GEO_KRIGE_SPHERICAL_KRM(gridLon, $
              ;;                             gridLat, $
              ;;                             hoodLon, $
              ;;                             hoodLat, $
              ;;                             hoodVal, $
              ;;                             hoodDist, $
              ;;                             hoodCovar, $
              ;;                             inHoodCount, $
              ;;                             1, $ ; use LUSOL (not MINRES)
              ;;                             -1.0, $ ; kmax for MINRES
              ;;                             -1.0, $ ; eta_error for MINRES
              ;;                             numKRMAngles, $ ; # angles
              ;;                             numKRMRadii, $ ; # radii
              ;;                             paramsSpher, $
              ;;                             DistPrecision, $
              ;;                             maxLagMeters, $
              ;;                             mPerDegLon, $
              ;;                             mPerDegLat, $
              ;;                             NUM_RADIAL_MESH_CELLS = $
              ;;                               noNullCells, $
              ;;                             STAGGER_MESH = staggerKRMMesh, $
              ;;                             TIME_ACCOUNT = timeAccount, $
              ;;                             /RADIAL_AVERAGING, $
              ;;                             /SAMPLE) $
              ;; else $
              ;; Youssef originally coded this as GEO_KRIGE_KRM
              ;;     gridOut[cc,rc] = $
              ;;     GEO_KRIGE_KRM(1, 200, 0.01, $
              ;;                   inHoodCount, $
              ;;                   numKRMAngles, $
              ;;                   numKRMRadii, $
              ;;                   paramsSpher, $
              ;;                   DistPrecision, $
              ;;                   maxLagMeters, $
              ;;                   gridLat, $
              ;;                   gridLon, $
              ;;                   hoodLat, $
              ;;                   hoodLon, $
              ;;                   hoodDist, $
              ;;                   hoodCovar, $
              ;;                   hoodVal)
              ;;     timeAccount = 1.0
              ;;     nonullcells = 1
              ;; endelse
                  ;; The current blessed function is GEO_KRIGE_SPHERICAL_KRM
              gridOut[cc, rc] = $
                  GEO_KRIGE_SPHERICAL_KRM(gridLon, $
                                          gridLat, $
                                          hoodLon, $
                                          hoodLat, $
                                          hoodVal, $
                                          hoodDist, $
                                          hoodCovar, $
                                          inHoodCount, $
                                          numKRMAngles, $ ; # angles
                                          numKRMRadii, $ ; # radii
                                          paramsSpher, $
                                          DistPrecision, $
                                          maxLagMeters, $
                                          mPerDegLon, $
                                          mPerDegLat, $
                                          ERROR_VARIANCE = errVarKRM, $
                                          WEIGHT_ADJUST = hoodKWA, $
                                          NUM_RADIAL_MESH_CELLS = $
                                            noNullCells, $
                                          STAGGER_MESH = staggerKRMMesh, $
                                          WALL_TIME = KRMWallTime, $
                                          /RADIAL_AVERAGING)

              errVarOut[cc, rc] = errVarKRM

;              t2KRM = SYSTIME(/SECONDS)
;              timeKRM += (t2KRM - t1KRM)
              if (noNullCells gt maxNoNullCells) then $
                  maxNoNullCells = noNullCells
              if (noNullCells gt 1) then begin
                  numSolvers++
                  totalSolverPoints += noNullCells
              endif

              t2 = SYSTIME(/UTC, /SECONDS)
              timeKRM += (t2 - t1)
              t1 = t2

              GOTO, NEXT_COLUMN ; CONTINUE

          endif
                     ;else begin

          t2 = SYSTIME(/UTC, /SECONDS)
          timeLocal += (t2 - t1)
          t1 = t2


;         Add a covariance entry for the Lagrange factor that ensures
;         Kriging weights will sum to 1.0.

          hoodKCovar = [hoodCovar, 1.0]


;         Generate covariance matrix between observations in the
;         neighborhood. The covariance matrix is initialized to the
;         sill value of the semivariogram. All off-diagonal elements
;         will be replaced with covariances taken from allCovar_2D. I
;         am not 100% sure that the diagonal values are the sill of
;         the semivariogram rather than the sill - nugget difference.
;
;         Since the nugget is theoretically only associated with
;         nonzero separations (i.e., it is a discontinuity in the
;         semivariogram at the origin), and since diagonal covariances
;         are associated with zero separation, then I am pretty sure
;         the sill is the correct value for covariance along the
;         diagonal. However, for separations of 1.0e-1000 (just
;         slightly positive), the covariance will be sill - nugget.

          hoodCovar_2d = MAKE_ARRAY(inHoodCount, inHoodCount, $
                                    /DOUBLE, $
                                    VALUE = ParamsSpher[1]) ; RIGHT???

;         !!! TEMPORARY !!!
          hoodDist_2d = MAKE_ARRAY(inHoodCount, inHoodCount, $
                                   /DOUBLE, $
                                   VALUE = ndv)

          newHoodDist_2d = hoodDist_2d

          for jHood = 1L, inHoodCount - 1L do begin

              j = hoodInd[jHood]

              d2dRow = allDist_2D[*, j]
              i2dRow = all_i_2D[*, j]
              c2dRow = allCovar_2D[*, j]

              for iHood = 0L, jHood - 1L do begin ; iHood < jHood always

                  i = hoodInd[iHood]
                  if (i ge j) then STOP ; programming check
;                  k = j * (j - 1) / 2 + i

                  ind = WHERE(i2dRow eq i, count)
                  ccc3 = c2dRow[ind[0]]
                  ddd3 = d2dRow[ind[0]]

                  hoodCovar_2d[iHood,jHood] = ccc3
                  hoodCovar_2d[jHood,iHood] = ccc3

                  hoodDist_2d[iHood,jHood] = ddd3
                  hoodDist_2d[jHood,iHood] = ddd3

              endfor
 
          endfor


;         Add a row and column for the Lagrange factor that ensures
;         Kriging weights will sum to 1.0. With that zero on the
;         diagonal, the covariance matrix is no longer positive definite.

          hoodKCovar_2d = DBLARR(inHoodCount + 1L, inHoodCount + 1L)
          hoodKCovar_2d[0L:inHoodCount - 1L, $
                        0L:inHoodCount - 1L] = hoodCovar_2d
          hoodKCovar_2d[0L:inHoodCount - 1L, inHoodCount] = 1.0D
          hoodKCovar_2d[inHoodCount, 0L:inHoodCount - 1L] = 1.0D
          hoodKCovar_2d[inHoodCount, inHoodCount] = 0.0D


;         Solve for Kriging weights for observations in the
;         neighborhood.
;
;         The forward equation is
;
;           hoodKCovar_2d # weight = hoodKCovar
;
;         List of canned IDL solvers:
;
;         INVERT
;         CHOLSOL
;         CRAMER
;         GS_ITER
;         LA_CHOLSOL, LA_CHOLMPROVE
;         LA_INVERT
;         LA_LINEAR_EQUATION
;         LA_LUSOL, LA_LUMPROVE
;         LA_TRISOL, LA_TRIMPROVE
;         LUSOL, LUMPROVE
;         SVSOL
;         TRISOL


;         Solve with LUSOL and LUMPROVE.
;         *** FASTEST BUILT-IN IDL METHOD TRIED ***

          hkc_ludc = hoodKCovar_2d
          time1 = SYSTIME(/UTC, /SECONDS)
          LUDC, hkc_ludc, index, /DOUBLE
          weight = LUSOL(hkc_ludc, index, hoodKCovar, /DOUBLE)
          time2 = SYSTIME(/UTC, /SECONDS)
          thisSolverSize = inHoodCount + 1L
          thisSolverTime = time2 - time1
          if NOT(ISA(solverSize)) then begin
              solverSize = [thisSolverSize]
              solverTime = [thisSolverTime]
          endif else begin
              solverSize = [solverSize, thisSolverSize]
              solverTime = [solverTime, thisSolverTime]
          endelse
          ;; weight = LUMPROVE(hoodKCovar_2d, hkc_ludc, index, $
          ;;                   hoodKCovar, $
          ;;                   weight, /DOUBLE)

;GF 20190912
          lagrange_param = weight[inHoodCount]
          weight = weight[0L:inHoodCount - 1L]

          numSolvers++
          totalSolverPoints += inHoodCount


;         Verify that weights sum to 1.0

          if (ABS(TOTAL(weight) - 1.0D) gt 1.0D-6) then STOP; solution check


;         Eliminate negative weights.

          newWeight = REMOVE_NEGATIVE_KRIGING_WEIGHT(weight, hoodCovar, $
                                                     /DEUTSCH)
          weight = newWeight

;if sampleFlag then STOP
;          if doPlots then weight2 = weight


;         Eliminate upswing in weight at large distances.

          newWeight = REMOVE_KRIGING_WEIGHT_UPSWING(weight, hoodCovar)
          weight = newWeight

;if sampleFlag then STOP
;if doPlots then weight3 = weight

;+ GF 20180831
          if (KEYWORD_SET(krigeWeightAdjust) and $
              ARG_PRESENT(krigeWeightAdjust)) $
              then begin

;+
;             Manually adjust kriging weights as instructed by caller.
;-
              newWeight = weight * hoodKWA
              if (TOTAL(newWeight) gt 0.0) then $
                  weight = newWeight / TOTAL(newWeight)

          endif
;- GF 20180831


;         Perform interpolation for the grid point. Note that this is
;         the first time the actual data in the neighborhood is
;         used. The data do not contribute in any way to the
;         generation of Kriging weights.

          gridOut[cc, rc] = TOTAL(weight * hoodVal)
          errVarOut[cc, rc] = $
              ParamsSpher[1] - TOTAL(weight * hoodCovar) - lagrange_param

;GF 20190910 vv
;; if ((cc eq 141) and (rc eq 185)) then begin
;;     PRINT, ' '
;;     PRINT, '---------- LEVU1 ----------'
;;     PRINT, '  weight           value'
;;     order = REVERSE(SORT(weight))
;;     for wwwc = 0, inHoodCount - 1 do $
;;         PRINT, weight[order[wwwc]], hoodVal[order[wwwc]], $
;;                FORMAT = '(F10.6, 7X, F10.6)'
;; ;GF 20190912
;;     PRINT, gridOut[cc,rc], ' +/- ', SQRT(errVarOut[cc, rc])
;;     PRINT, ' '
;; endif
;GF 20190910 ^^

NEXT_COLUMN:

          numKrigedGridCells++

          t2 = SYSTIME(/UTC, /SECONDS)
          timeLocal += (t2 - t1)
          t1 = t2

      endfor

  endfor

  if KEYWORD_SET(verbose) then PRINT, ''

  meanPointsPerSolver = $
      ROUND(FLOAT(totalSolverPoints) / FLOAT(numSolvers))

  if KEYWORD_SET(verbose) then begin
      USR_MSG, 'Number of solvers: ' + STRCRA(numSolvers)
      USR_MSG, 'Mean points per solver: ' + STRCRA(meanPointsPerSolver)
;      USR_MSG, 'TOTAL(solverTime): ' + STRCRA(TOTAL(solverTime))
  endif

  if KEYWORD_SET(verbose) then $
      USR_MSG, '# of solutions skipped due to neighborhood range <= ' + $
               'NEIGHBORHOOD_TOLERANCE = ' + STRCRA(hoodTolerance) + $
               ': ' + STRCRA(numSkips)

  if (KEYWORD_SET(verbose) and KEYWORD_SET(useKRM)) then begin
      USR_MSG, 'Maximum number of radial mesh cells: ' + $
               STRCRA(maxNoNullCells)
      USR_MSG, '(maximum possible is ' + $
               STRCRA(numKRMRadii * numKRMAngles) + ')'
  endif

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Maximum neighborhood count: ' + STRCRA(maxHoodCount)

  if (NOT(ARG_PRESENT(cvRMSE)) and $
      NOT(ARG_PRESENT(valKrige))) then RETURN


; Perform cross-validation analysis to estimate errors.

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Estimating errors via cross validation.'

  if KEYWORD_SET(verbose) then begin
      if (numPoints lt 50) then begin
          hashes = numPoints
          lastProgress = -1
      endif else begin
          hashes = 50
          lastProgress = 0
      endelse
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif

  valKrige = MAKE_ARRAY(numPoints, /FLOAT, VALUE = ndv)


; Loop over input points.

  kInd = LINDGEN(numPoints)

  for k = 0L, numPoints - 1L do begin

      if KEYWORD_SET(verbose) then begin
          progress = FIX(FLOAT(k) / FLOAT(numPoints - 1) * FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      if (useFlag[k] eq 0B) then CONTINUE ; ndv, outside domain, or outlier

      lonk = lon[k]
      latk = lat[k]

      mPerDegLon = DISTANCE(distPrecision, 0.0D, latk, 1.0D, latk)
      mPerDegLat = DISTANCE(distPrecision, $
                            0.0D, latk + 0.0005D, $
                            0.0D, latk - 0.0005D) * 1000.0D


;     Skip points in an analysis "halo" around the output domain.

      if ((lonk lt minLonOut) or $
          (lonk gt maxLonOut) or $
          (latk lt minLatOut) or $
          (latk gt maxLatOut)) then CONTINUE


;     Perform the kriging step at the sample location.

      ;; t1 = SYSTIME(/UTC, /SECONDS)


;     Identify observations within maxLagMeters for covariance
;     calculations.

;     Also, we exclude the sample location at this point.

      inBoxInd = WHERE((useFlag eq 1B) and $
                       (ABS(lon - lonk) lt (maxLagDegLon)) and $
                       (ABS(lat - latk) lt (maxLagDegLat)) and $
                       (kInd ne k), inBoxCount)

      if (inBoxCount lt minHoodPoints) then CONTINUE

      lonInBox = lon[inBoxInd]
      latInBox = lat[inBoxInd]
      

;     Identify observations in the "box" that fall within maxLagMeters
;     of the sample location; i.e., within the radial neighborhood of
;     the point in question.

;      t1 = SYSTIME(/SECONDS)
      dInBox = DISTANCE(DistPrecision, lonk, latk, lonInBox, latInBox)
;      t2 = SYSTIME(/SECONDS)
;      timeDist += (t2 - t1)
      boxInHoodInd = WHERE(dInBox lt maxLagMeters, inHoodCount)
      if (inHoodCount lt minHoodPoints) then CONTINUE

      hoodInd = inBoxInd[boxInHoodInd] ; these are already sorted distances
      hoodDist = dInBox[boxInHoodInd]  ; from points to the sample location

      numClose = TOTAL(hoodDist lt minLagMeters, /INT)
      if (numClose lt 4) then CONTINUE
      ;if (MIN(hoodDist) gt minLagMeters) then CONTINUE

      hoodVal = val[hoodInd]
      hoodKWA = krigeWeightAdjust[hoodInd]

      ind = WHERE(hoodInd eq k, count)
      if (count ne 0) then STOP ; PROGRAMMING ERROR

      minHoodVal = MIN(hoodVal)
      maxHoodVal = MAX(hoodVal)

      if ((maxHoodVal - minHoodVal) le hoodTolerance) then begin

          valKrige[k] = MEAN(hoodVal)

          CONTINUE

      endif

      ;; t2 = SYSTIME(/UTC, /SECONDS)
      ;; print, k, t2 - t1
      ;; t1 = t2


;; ;     Alternate method THAT USES all_i_2d, alldist_2d, and allcovar_2d,
;; ;     avoiding all calls to DISTANCE.

;;       inHoodCount2 = 0
;;       if (k gt 0) then begin
;;           foo = all_i_2d[0:((k - 1) < (maxNumNeighbors - 1)), k]
;;           bar = allDist_2d[0:((k - 1) < (maxNumNeighbors - 1)), k]
;;           ind = WHERE((foo ne -1L) and $
;;                       (bar lt maxLagMeters), inHoodCount2)
;;           if (inHoodCount2 gt 0) then begin
;;               hoodInd2 = all_i_2d[ind, k]
;;               hoodDist2 = allDist_2d[ind, k]
;;           endif
;;       endif
;;       for jj = k + 1, n - 1 do begin
;;           foo = all_i_2d[0:((jj - 1) < (maxNumNeighbors - 1)), jj]
;;           bar = allDist_2d[0:((jj - 1) < (maxNumNeighbors - 1)), jj]
;;           ind = where((foo eq k) and (bar lt maxLagMeters), count)
;;           if (count eq 0) then CONTINUE
;;           if (count ne 1) then STOP
;;           if (inHoodCount2 eq 0) then begin
;;               hoodInd2 = [jj]
;;               hoodDist2 = [bar[ind[0]]]
;;               inHoodCount2 = 1
;;           endif else begin
;;               hoodInd2 = [hoodInd2, jj]
;;               hoodDist2 = [hoodDist2, bar[ind[0]]]
;;               inHoodCount2++
;;           endelse
;;       endfor

      ;; t2 = SYSTIME(/UTC, /SECONDS)
      ;; print, k, t2 - t1

      ;; err = hoodInd2 - hoodInd
      ;; if ((MIN(err) lt 0) or (MAX(err gt 0))) then stop


;     To get covariances we could pull values from the allCovar_2D
;     array, but this method is faster.

;     Calculate semivariances for points in the neighborhood relative
;     to the sample location.

;      SPHERICAL_SEMIVARIOGRAM, hoodDist, ParamsSpher, hoodSemiVar
      hoodSemiVar = SPHERICAL_SEMIVARIOGRAM_FUNC(hoodDist, ParamsSpher)


;     Convert semivariance to covariance.

      hoodCovar = ParamsSpher[1] - hoodSemiVar ; sill - semivariance

      if KEYWORD_SET(useKRM) then begin

          hoodLon = lon[hoodInd]
          hoodLat = lat[hoodInd]

;          t1KRM = SYSTIME(/SECONDS)
          valKrige[k] = $
              GEO_KRIGE_SPHERICAL_KRM(lonk, $
                                      latk, $
                                      hoodLon, $
                                      hoodLat, $
                                      hoodVal, $
                                      hoodDist, $
                                      hoodCovar, $
                                      inHoodCount, $
                                      numKRMAngles, $    ; # angles
                                      numKRMRadii, $     ; # radii
                                      paramsSpher, $
                                      DistPrecision, $
                                      maxLagMeters, $
                                      mPerDegLon, $
                                      mPerDegLat, $
                                      WEIGHT_ADJUST = hoodKWA, $
                                      NUM_RADIAL_MESH_CELLS = $
                                        noNullCells, $
                                      STAGGER_MESH = staggerKRMMesh, $
                                      /RADIAL_AVERAGING)
;          t2KRM = SYSTIME(/SECONDS)
;          timeKRM += (t2KRM - t1KRM)

          CONTINUE

      endif                     ;else begin


;     Add a covariance entry for the Lagrange factor that ensures
;     Kriging weights will sum to 1.0.

      hoodKCovar = [hoodCovar, 1.0]


;     Generate covariance matrix between observations in the
;     neighborhood.

      hoodCovar_2d = MAKE_ARRAY(inHoodCount, inHoodCount, $
                                /DOUBLE, $
                                VALUE = ParamsSpher[1]) ; RIGHT???

      for jHood = 1L, inHoodCount - 1L do begin

          j = hoodInd[jHood]

          i2dRow = all_i_2D[*, j]
          c2dRow = allCovar_2D[*, j]

          for iHood = 0L, jHood - 1L do begin ; iHood < jHood always

              i = hoodInd[iHood]
              if (i ge j) then STOP ; programming check

              ind = WHERE(i2dRow eq i, count)
              ccc3 = c2dRow[ind[0]]

              hoodCovar_2d[iHood,jHood] = ccc3
              hoodCovar_2d[jHood,iHood] = ccc3

          endfor

      endfor


;     Add a row and column for the Lagrange factor that ensures
;     Kriging weights will sum to 1.0. With that zero on the diagonal,
;     the covariance matrix is no longer positive definite.

      hoodKCovar_2d = DBLARR(inHoodCount + 1L, inHoodCount + 1L)
      hoodKCovar_2d[0L:inHoodCount - 1L, $
                    0L:inHoodCount - 1L] = hoodCovar_2d
      hoodKCovar_2d[0L:inHoodCount - 1L, inHoodCount] = 1.0D
      hoodKCovar_2d[inHoodCount, 0L:inHoodCount - 1L] = 1.0D
      hoodKCovar_2d[inHoodCount, inHoodCount] = 0.0D


;     Solve for Kriging weights for observations in the neighborhood.
;
;     The forward equation is
;
;       hoodKCovar_2d # weight = hoodKCovar

;     Solve with LUSOL and LUMPROVE.
;     *** FASTEST BUILT-IN IDL METHOD TRIED ***

      hkc_ludc = hoodKCovar_2d
      LUDC, hkc_ludc, index, /DOUBLE
      weight = LUSOL(hkc_ludc, index, hoodKCovar, /DOUBLE)
      ;; weight = LUMPROVE(hoodKCovar_2d, hkc_ludc, index, $
      ;;                   hoodKCovar, $
      ;;                   weight, /DOUBLE)
      weight = weight[0L:inHoodCount - 1L]


;     Verify that weights sum to 1.0

      if (ABS(TOTAL(weight) - 1.0D) gt 1.0D-6) then STOP ; solution check



;     Eliminate negative weights.

      newWeight = REMOVE_NEGATIVE_KRIGING_WEIGHT(weight, hoodCovar, $
                                                     /DEUTSCH)
      weight = newWeight


;     Eliminate upswing in weight at large distances.

      newWeight = REMOVE_KRIGING_WEIGHT_UPSWING(weight, hoodCovar)
      weight = newWeight

;+ GF 20180831
      if (KEYWORD_SET(krigeWeightAdjust) and $
          ARG_PRESENT(krigeWeightAdjust)) $
          then begin

;+
;         Manually adjust kriging weights as instructed by caller.
;-
          newWeight = weight * hoodKWA
          if (TOTAL(newWeight) gt 0.0) then $
              weight = newWeight / TOTAL(newWeight)

      endif
;- GF 20180831


;     Perform interpolation for the obs point. Note that this is the
;     first time the actual data in the neighborhood are used. The
;     data do not contribute in any way to the generation of Kriging
;     weights.

      valKrige[k] = TOTAL(weight * hoodVal)

  endfor

  if KEYWORD_SET(verbose) then PRINT, ''

  ind = WHERE(valKrige ne ndv, count)
  if (count eq 0) then STOP

  cvRMSE = SQRT(TOTAL((valKrige[ind] - val[ind])^2.0) / count)

  t2Full = SYSTIME(/UTC, /SECONDS)
  wallTime = t2Full - t1Full
  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Kriging ran in ' + STRCRA(wallTime) + ' seconds wall time.'

  RETURN

end
