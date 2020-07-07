FUNCTION SFAV2_FLAG_ODDBALLS, $
    SiteID, $
    Lon, $
    Lat, $
    Elev, $
    Val, $
    Ndv, $
    HoodRadiusMeters, $
    MinHoodPoints, $
    LowThresh, $            ; threshold for small nonzero amounts
    HighThresh, $           ; above this value gets checked as possible oddball
    MPerDegLonRef, $        ; smallest meters per degree longitude
    MPerDegLatRef, $        ; smallest meters per degree latitude
    KEEP_FLAG = keepFlag, $ ; 0/1 for exemption from outlier test.
    DISTANCE_PRECISION = DistPrecision, $
    VERBOSE = verbose, $
    CHECK_ID = CheckID      ; a list of site IDs to perform more thorough
                            ; descriptions on

; Perform an "oddball" outlier test for spatial consistency. Each
; point with Val equal to zero, and each point with Val above the
; HighThresh, is compared with its neighbors and tested for
; corroborating observations if it has enough neighbors. Points for
; which 0.0 < Val <= HighThresh are not tested for "oddball" status.
;
; Typical values for LowThresh and HighThresh are 0.1 inch and 1 inch,
; respectively. Consequently, observations (i.e., Val values) in the
; range 0.0 < Val <= 1.0 are not considered to be potential
; "oddballs", but rather are treated as ordinary observations unlikely
; to disrupt the analysis much, even if they are spurious.
;
; For any Val of 0.0, HighThresh (in addition to defining the Val
; threshold for points being tested as possible "oddballs"), defines
; the highest value for a neighboring report that is considered to
; corroborate that value. For example, given LowThresh = 0.1 and
; HighThresh = 1.0, if a value of 0.0 has a neighbor with a value of
; 1.1 (i.e. Val > HighThresh), that neighbor is considered to NOT be a
; corroborating report, and increases the likelihood that the 0.0
; value will be judged an "oddball". However, a different neighbor
; with a value of 0.5 (i.e. Val <= HighThresh) or a third neighbor
; with a value of 1.0 (again, Val <= HighThresh) is considered a
; corroborating report.
;
; For any Val exceeding HighThresh, LowThresh defines the value for a
; neighboring report that must be exceeded for that report to be
; considered to corroborate the > HighThresh value. For example, given
; LowThresh = 0.1 and HighThresh = 1.0, if a value of 1.1
; (i.e., Val > HighThresh) has a neighbor with a value of 0.11
; (i.e., Val > LowThresh), that neighbor is considered to be a
; corroborating report, and decreases the likelihood that the 1.1
; value will be judged an "oddball". However, a different neighbor
; with a value of 0.001 (i.e. Val <= LowThresh) or a third neighbor
; with a value of 0.1 (again, Val <= LowThresh) is considered a
; non-corroborating report.
;
; Here are the detailed criteria:
;
; For values of 0.0, if ALL the following criteria are met, the point
; is judged to be an outlier:
;
; 1. The very nearest neighbors are all > HighThresh. The size of this
;    group ("very nearest neighbors) is the larger of
;    (MinHoodPoints - 1) and 5% of the neighborhood size (minum 1),
;    whichever is larger.
; 2. At most 5% of the points in the entire neighborhood are also less
;    than or equal to HighThresh; i.e., unlike the value in question,
;    more than 95% of the points in the neighborhood are > HighThresh.
; 3. The point is within 2 standard deviations in elevation of its
;    neighborhood mean elevation; i.e., a point must be at an ordinary
;    elevation for its neighborhood to be considered an outlier under
;    any circumstance. Unusually high or low points are EXEMPT from
;    being flagged as oddballs. Mountaintop sites such as Mount
;    Washington and Mount Mitchell stations, for example, are spared
;    in this way.
;
; For values above the HighThresh (one inch is a typical value), if
; ALL the following criteria are met, the point is judged to be an
; outlier:
;
; 1. The very nearest neighbors are all <= LowThresh. The size of this
;    group is the larger of (MinHoodPoints - 1) and 5% of the
;    neighborhood size (minus 1), whichever is larger.
; 2. At most 5% of the points in the entire neighborhood are also
;    greater than than LowThresh; i.e., unlike the value in question,
;    more than 95% of the points in the neighborhood are <= LowThresh.
; 3. The point is within 2 standard deviations in elevation of its
;    neighborhood mean elevation (see #3, above, for details).
;
; On output, points receiving a flag value of 1 ARE outliers.

;+
; Check arguments.
;-
  numPoints = N_ELEMENTS(Val)

  if ((N_ELEMENTS(SiteID) ne numPoints) or $
      (N_ELEMENTS(Lon) ne numPoints) or $
      (N_ELEMENTS(Lat) ne numPoints) or $
      (N_ELEMENTS(Elev) ne numPoints) or $
      (N_ELEMENTS(Val) ne numPoints)) then begin
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

  if NOT(ISA(Val, 'FLOAT')) then begin
      ERR_MSG, 'Input value variable must be a FLOAT array.'
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

;+
;     Default "keep" flag to "no" (0B).
;-
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
      USR_MSG, 'Performing "oddball" (spatial consistency) test.'

;+
; Calculate the neighborhood radius in degrees longitude and degrees
; latitude.
;-
  hoodRadiusDegLon = HoodRadiusMeters / MPerDegLonRef
  HoodRadiusDegLat = HoodRadiusMeters / MPerDegLatRef

;+
; A minimum neighborhood population of 6 values is recommended.
;-
  if (MinHoodPoints lt 6) then $
      ERR_MSG, 'WARNING: minimum neighborhood points setting (' + $
               STRCRA(MinHoodPoints) + ' should be at least 6.'

  oddballFlag = BYTARR(numPoints)

; Kent added numFalseZeroes below
  numOddballs = 0L
  numHighOddballs = 0L
  numLowOddballs = 0L
; numFalseZeroes = 0L
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

      if ((Val[sc] gt 0.0) and (Val[sc] le HighThresh)) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' value ' + $
                       STRCRA(Val[sc]) + ' is nonzero and too low ' + $
                       'for "oddball" testing.'
          CONTINUE
      endif

;+
;     Find neighbors for the current site.
;-
      inBoxInd = $
          WHERE((Lon gt (Lon[sc] - hoodRadiusDegLon)) and $
                (Lon lt (Lon[sc] + hoodRadiusDegLon)) and $
                (Lat gt (Lat[sc] - HoodRadiusDegLat)) and $
                (Lat lt (Lat[sc] + HoodRadiusDegLat)), inBoxCount)

      if (inBoxCount lt MinHoodPoints) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' has at most ' + $
                       STRCRA(inBoxCount - 1) + ' neighbors. ' + $
                       'No "oddball" test will be performed.'
          CONTINUE 
      endif

;+
;     Spend a DISTANCE call to eliminate points in corners of the box.
;-
      dInBox = DISTANCE(DistPrecision, Lon[sc], Lat[sc], $
                        Lon[inBoxInd], Lat[inBoxInd])

      boxInHoodInd = WHERE((dInBox lt HoodRadiusMeters), inHoodCount)
      if (inHoodCount lt MinHoodPoints) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' has only ' + $
                       STRCRA(inHoodCount - 1) + ' neighbors. ' + $
                       'No "oddball" test will be performed.'
          CONTINUE
      endif

      dInHood = dInBox[boxInHoodInd]
      inHoodInd = inBoxInd[boxInHoodInd]

      hoodElev = Elev[inHoodInd] ;elevInBox[boxInHoodInd]
      hoodVal = Val[inHoodInd] ;valInBox[boxInHoodInd]

;+
;     Sort neighbors by distance.
;-
      order = SORT(dInHood)

      dInHood = dInHood[order]
      inHoodInd = inHoodInd[order]
      hoodElev = hoodElev[order]
      hoodVal = hoodVal[order]

      iii = WHERE(inHoodInd eq sc, count)
      if (count ne 1) then STOP ; PROGRAMMING ERROR
      if (iii[0] ne 0) then STOP ; THE POINT ITSELF MUST BE CLOSEST

;+
;     CONTINUE if any of the closest neighbors corroborate the
;     report. The "+ 1.0e-6" for the check on zeroes is needed to help
;     some values that are 0.1 inch reports but still fail the
;     "le LowThresh" condition, presumably because of floating point
;     noise.
;-
      closeCount = (MinHoodPoints - 1) > ROUND(0.05 * (inHoodCount - 1))

      if (Val[sc] eq 0.0) then begin
          if (MIN(hoodVal[1:closeCount]) le HighThresh) then begin
              if check then $
                  USR_MSG, SiteID[sc] + ' has at least one ' + $
                           'nearby neighbor at or below threshold ' + $
                           '(min. value ' + $
                           STRCRA(MIN(hoodVal[1:closeCount])) + '). ' + $
                           'It passes the "oddball" test.'
              CONTINUE
          endif
      endif else begin
          if (MAX(hoodVal[1:closeCount]) gt LowThresh) then begin
              if check then $
                  USR_MSG, SiteID[sc] + ' has at least one ' + $
                           'nearby neighbor above threshold ' + $
                           '(max. value ' + $
                           STRCRA(MAX(hoodVal[1:closeCount])) + '). ' + $
                           'It passes the "oddball" test.'
              CONTINUE
          endif
      endelse

      if check then begin
          if (Val[sc] eq 0.0) then $
              USR_MSG, 'From the ' + STRCRA(closeCount) + $
                       ' reporting stations closest to ' + SiteID[sc] + $
                       ', the minimum report (' + $
                       STRCRA(MIN(hoodVal[1:closeCount])) + $
                       ') is too high to corroborate its zero report. ' + $
                       'It may fail the "oddball" test.' $
          else $
              USR_MSG, 'From the ' + STRCRA(closeCount) + $
                       ' reporting stations closest to ' + SiteID[sc] + $
                       ', the maximum report (' + $
                       STRCRA(MAX(hoodVal[1:closeCount])) + $
                       ') is too low to corroborate its high report. ' + $
                       'It may fail the "oddball" test.'
      endif

;+
;     CONTINUE if more than some proportion of the neighborhood
;     corroborates the report. See the above comment for the
;     explanation of the "+ 1.0e-6" in the check on zeroes.
;-
      if (Val[sc] eq 0.0) then begin
          ind = WHERE(hoodVal[1:inHoodCount - 1] le HighThresh, $
                      count)
          proportion = FLOAT(count) / FLOAT(inHoodCount - 1)
          if (proportion gt 0.05) then begin
              if check then $
                  USR_MSG, SiteID[sc] + ' has ' + STRCRA(count) + $
                           ' (' + STRCRA(proportion * 100.0) + '%) ' + $
                           'additional at-or-below-threshold values ' + $
                           'in its neighborhood. ' + $
                           'It passes the "oddball" test.'
              CONTINUE
          endif
      endif else begin
          ind = WHERE(hoodVal[1:inHoodCount - 1] gt LowThresh, count)
          proportion = FLOAT(count) / FLOAT(inHoodCount - 1)
          if (proportion gt 0.05) then begin
              if check then $
                  USR_MSG, SiteID[sc] + ' has ' + STRCRA(count) + $
                           ' (' + STRCRA(proportion * 100.0) + '%) ' + $
                           'additional above-threshold values in its ' + $
                           'neighborhood. It passes the "oddball" test.'
              CONTINUE
          endif
      endelse

      if check then begin
          if (Val[sc] eq 0.0) then $
              USR_MSG, 'Only ' + STRCRA(proportion * 100) + '% of the ' + $
                       'reports in the neighborhood of ' + SiteID[sc] + $
                       ' (zero report) were at or below threshold.' $
          else $
              USR_MSG, 'Only ' + STRCRA(proportion * 100) + '% of the ' + $
                       'reports in the neighborhood of ' + SiteID[sc] + $
                       ' (zero report) were above threshold.'
      endif

;+
;     Calculate elevation stats.  Elevations need to be handled
;     carefully because many are no-data values (-9999).
;-
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

;+
;     CONTINUE if the site is at an unusually low or high elevation
;     for its neighborhood.
;-
      if ((hoodElevTStat ne -999.0) and $
          (ABS(hoodElevTStat) gt 2.0)) then begin
          if check then $
              USR_MSG, SiteID[sc] + ' has an elevation t-statistic of ' + $
                       STRCRA(hoodElevTStat) + ' which excuses it from ' + $
                       'receiving "oddball" status.'
          CONTINUE
      endif

;+
;     The current value is an oddball.
;-
      oddballFlag[sc] = 1B

      if (KEYWORD_SET(verbose) and (numOddballs eq 0L)) then begin

          USR_MSG, '      STATION ID  ' + $
                   'LONGITUDE  ' + $
                   'LATITUDE  ' + $
                   '    VAL  ' + $
                   '# HOOD  ' + $
                   ' ELEV  ' + $
                   'EL MEAN  ' + $
                   'EL SD  ' + $
                   '    EL G  ' + $
                   'RANGE'

          USR_MSG, '      ----------  ' + $
                   '---------  ' + $
                   '--------  ' + $
                   '    ---  ' + $
                   '------  ' + $
                   ' ----  ' + $
                   '-------  ' + $
                   '-----  ' + $
                   '    ----  ' + $
                   '-----'

          idFormat = '(A16)'
          lonFormat = '(F9.4)'
          latFormat = '(F8.4)'
          valFormat = '(F7.3)'
          nHoodFormat = '(I6)'
          elevFormat = '(I5)'
          elevMeanFormat = '(I7)'
          elevSDFormat = '(I5)'
          elevTStatFormat = '(F8.3)'
          elevRangeFormat = '(I5)'

      endif

      if KEYWORD_SET(verbose) then $
          USR_MSG, STRING(SiteID[sc], FORMAT = idFormat) + '  ' + $
                   STRING(Lon[sc], FORMAT = lonFormat) + '  ' + $
                   STRING(Lat[sc], FORMAT = latFormat) + '  ' + $
                   STRING(Val[sc], FORMAT = valFormat) + '  ' + $
                   STRING(inHoodCount, FORMAT = nHoodFormat) + '  ' + $
                   STRING(Elev[sc], FORMAT = elevFormat) + '  ' + $
                   STRING(hoodElevMean, FORMAT = elevMeanFormat) + '  ' + $
                   STRING(hoodElevSD, FORMAT = elevSDFormat) + '  ' + $
                   STRING(hoodElevTStat, FORMAT = elevTStatFormat) + '  ' + $
                   STRING(hoodElevRange, FORMAT = elevRangeFormat)

      numOddballs++
      if (Val[sc] eq 0.0) then $
          numLowOddballs++ $
      else $
          numHighOddballs++

  endfor

  if KEYWORD_SET(verbose) then begin
      if (numOddballs eq 0) then $
          USR_MSG, 'No "oddball" outliers found.' $
      else $
          USR_MSG, 'Identified ' + STRCRA(numOddballs) + $
                   ' "oddball" outliers, ' + $
                   STRCRA(numLowOddballs) + ' suspect zeroes and ' + $
                   STRCRA(numHighOddballs) + ' suspect high values.'
  endif


  RETURN, oddballFlag


end
