FUNCTION GEO_KRIGE_SPHERICAL_KRM, $
    LonOut, $        ; IN: output longitude
    LatOut, $        ; IN: output latitude
    HoodLon, $       ; IN: longitude of input values ("obs")
    HoodLat, $       ; IN: latitudes of input values ("obs")
    HoodVal, $       ; IN: input values ("obs")
    HoodDist, $      ; IN: distances from LonOut, LatOut to obs locations
    HoodCovar, $     ; IN: covariances between output point and obs locations
    InHoodCount, $   ; IN: # points in neighborhood (size of previous 5 args)
    NumAngles, $     ; IN: number of angles for radial kriging
    NumRadii, $      ; IN: number of radii for radial kriging
    ParamsSpher, $   ; IN: semivariogram parameters; nugget, sill, range
    DistPrecision, $ ; IN: DISTANCE precision: 2 = spherical, 3 = ellipsoid
    MaxLagMeters, $  ; IN: maximum distance to neighborhood points
    MPerDegLon, $    ; IN: meters per degree longitude at output latitude
    MPerDegLat, $    ; IN: meters per degree latitude at output latitude
    ERROR_VARIANCE = errVar, $ ; error variance estimate for kriging result
    WEIGHT_ADJUST = HoodKWA, $ ; adjust weights for input values by this factor
    NUM_RADIAL_MESH_CELLS = noNullCells, $    ; return # of radial mesh cells
    STAGGER_MESH = staggerMesh, $ ; rotate adjacent rings
    TIME_ACCOUNT = timeAccount, $
    RADIAL_AVERAGING = radialAveraging, $
    WALL_TIME = wallTime


; Perform geospatial kriging with radial means for a single analysis
; point using a spherical semivariogram model.

; This function will perform minimal error checking to maximize its
; speed. Therefore, the caller should be sure of the following:
;
;   1. HoodLon, HoodLat, HoodVal, HoodDist, and HoodCovar must all
;      be the 1-D arrays of the length specified by InHoodCount.
;   2. LonOut, LatOut, HoodLon, and HoodLat must be double
;      precision.
;   3. If present via WEIGHT_ADJUST, HoodKWA must be the same length
;      as HoodLon, HoodLat, HoodVal, HoodDist, and HoodCovar, and all
;      its values must be positive.

; Greg Fall, OWP, Snow Analysis and Remote Sensing Center, Chanhassen, MN
; Youssef Loukili, OWP, NWC-Tuscaloosa
;
; August 2016
;
; Changes:
;
;   2016-09-24, GF: Removed "localKriging" option.
;   2016-11-02, GF: Added /RADIAL_AVERAGING keyword. If this keyword
;                   is set by the caller, then the average longitude
;                   and latitude for each radial mesh cell is derived
;                   from the average polar coordinates (radius and
;                   angle) for the points falling in that cell. This
;                   way, the average location for a given cell is
;                   guaranteed to actually be in that cell. The
;                   default behavior is just to average the longitudes
;                   and latitudes within a radial mesh cell, which
;                   can place the average location outside the cell.
;                   Removed conversions between degrees and
;                   radians. All angles are not in radians.
;   2018-09-04, GF: Removed all MINRES options, which we never used
;                   and never plan to use, and added WEIGHT_ADJUST
;                   keyword. This was added to accommodate some
;                   manipulation of kriging weights we are
;                   experimenting with in order to address false
;                   positives in the NOHRSC snowfall analysis.
;   2019-09-13, GF: Added error variance calculation. Error variance
;                   is now returned via the ERROR_VARIANCE keyword.

; nugget: paramsExpo[0]
; sill: paramsExpo[1]
; range: paramsExpo[2] (in units of meters)

; Speed improvement effort:
  t1 = SYSTIME(/UTC, /SECONDS)

  ;; if ARG_PRESENT(wallTime) then begin
  ;;     if NOT(KEYWORD_SET(wallTime)) then $
  ;;         wallTime = {general: 0.0D, distance: 0.0D, solver: 0.0D}
  ;;     t1 = SYSTIME(/UTC, /SECONDS)
  ;; endif

  nuggetSpher = ParamsSpher[0]
  sillSpher   = ParamsSpher[1]
  rangeSpher  = ParamsSpher[2]

  twoPi = 2.0D * !DPi
  deltaAngle = twoPi / NumAngles

  deltaRadius = MaxLagMeters / NumRadii

  if KEYWORD_SET(radialAveraging) then $
      hoodAngle = MAKE_ARRAY(InHoodCount, VALUE = -99999.0)

  if (NOT(KEYWORD_SET(HoodKWA)) or NOT(ARG_PRESENT(HoodKWA))) then begin
      HoodKWA = MAKE_ARRAY(N_ELEMENTS(HoodVal), VALUE = 1.0)
  endif


;-----------------------------------------------------------------;
; Distribute observation points over the cells of the radial mesh ;
;-----------------------------------------------------------------;

  inCellInd = MAKE_ARRAY(NumRadii, NumAngles, InHoodCount, $
                         /LONG, VALUE = -1L)
  inCellCount = LONARR(NumRadii, NumAngles)

  oldExcept = !EXCEPT
  !EXCEPT = 0

  if (KEYWORD_SET(staggerMesh) and $
      (NumAngles gt 1)) then begin ; random staggering
      angleOriginStag = DBLARR(NumRadii)
      for rc = 0, NumRadii - 1 do begin
          angleOriginStag[rc] = RANDOMU(seed) * 2.0D * !DPi / NumAngles
      endfor
  endif

  for hc = 0L, InHoodCount - 1L do begin

      radius = HoodDist[hc]
      rc = FIX(radius / deltaRadius) ; radial index

      if ((rc lt 0) or $
          (rc ge numRadii)) then CONTINUE ; OUT OF NEIGHBORHOOD

      if (NumAngles gt 1) then begin
          angleOrigin = 0.0D
          if KEYWORD_SET(staggerMesh) then begin
              angleOrigin = angleOriginStag[rc] ; random staggering
          ;; if (rc mod 2) then begin ; "half slice" staggering
          ;;     angleOrigin = !DPi / NumAngles
          ;; endif
          endif

          angle = ATAN((HoodLat[hc] - LatOut) * MPerDegLat, $
                       (HoodLon[hc] - LonOut) * MPerDegLon) ; -pi to pi
          if KEYWORD_SET(radialAveraging) then hoodAngle[hc] = angle
          if (angle lt 0.0) then angle += twoPi ; not changing hoodAngle

;     OLD
      ;; ac = FIX(angle / deltaAngle)
      ;; if ((ac lt 0) or $
      ;;     (ac ge NumAngles)) then STOP ;CONTINUE
;     NEW
          ac = FLOOR((angle - angleOrigin) / deltaAngle) ; angle index
          if (ac lt -1) then MESSAGE, 'PROGRAMMING ERROR #01'
          if (ac ge NumAngles) then MESSAGE, 'PROGRAMMING ERROR #02'
          if (ac eq -1) then ac = NumAngles - 1
      endif else ac = 0

      inCellInd[rc, ac, inCellCount[rc, ac]] = hc
      inCellCount[rc, ac] = inCellCount[rc, ac] + 1L

  endfor

  !EXCEPT = oldExcept


;  if (NOT(LMGR(/RUNTIME)) and KEYWORD_SET(sample)) then STOP


; Count the number of cells in the radial mesh that contain input points.

;  noNullCells = TOTAL(inCellCount < 1, /INTEGER) ; below is same
  noNullCells = TOTAL(inCellCount gt 0, /INTEGER) ; above is same

; Speed improvement effort:
  t2 = SYSTIME(/UTC, /SECONDS)
  time1 = t2 - t1
  t1 = t2

  if (noNullCells eq 1) then begin ; all data are in one cell
      valKrige = TOTAL(HoodVal * HoodKWA) / TOTAL(HoodKWA)
      errVar = VARIANCE(inHoodCount * HoodVal * HoodKWA / TOTAL(HoodKWA))
      RETURN, valKrige
  endif


; Initialize covariance matrices. "radMeshCovar_2d" is the 2-D
; covariance array on the left hand side of the forward equation
; (i.e., gives covariances between points in the kriging
; neighborhood), and "radMeshCovar" is the 1-D covariance array on the
; right hand side (i.e., gives covariances between points in the
; kriging neighborhood and the analysis location). These will both
; have elements added later to ensure kriging weights will sum to
; 1.0.

  radMeshCovar_2d = MAKE_ARRAY(noNullCells, noNullCells, $
                               /DOUBLE, VALUE = sillSpher)


; Initialize arrays of radial mesh data - representative longitude,
; latitude, distance from analysis point, covariance w/r/t analysis
; point, and input value.

  radMeshLon = DBLARR(noNullCells)
  radMeshLat = DBLARR(noNullCells)
  radMeshRad = DBLARR(noNullCells)
  radMeshCovar = DBLARR(noNullCells)
  radMeshMeanCovar = DBLARR(noNullCells)
  radMeshVal  = DBLARR(noNullCells)
  radMeshKWA = DBLARR(noNullCells)

  arrayCount = 0L

  for rc = 0, NumRadii - 1 do begin

      for ac = 0, NumAngles - 1 do begin

          inThisCellCount = inCellCount[rc, ac]
          if (inThisCellCount eq 0) then CONTINUE
          inThisCellInd = REFORM(inCellInd[rc, ac, $
                                           0:inThisCellCount - 1])


        ;--------------------------------------------------------;
        ; Estimate radMeshVal[arrayCount] and                    ;
        ; radMeshCovar[arrayCount] using a simple average of the ;
        ; input values in this radial mesh cell.                 ;
        ;--------------------------------------------------------;

          if (NumAngles eq 1) then begin


;             "Isotropic" case. Use the average distance to all obs in
;             this radial mesh cell (ring).

              ;; if ARG_PRESENT(wallTime) then begin
              ;;     t2 = SYSTIME(/UTC, /SECONDS)
              ;;     wallTime.general += (t2 - t1)
              ;;     t1 = t2
              ;; endif

              rad_dist_xk = DISTANCE(DistPrecision, $
                                     HoodLon[inThisCellInd], $
                                     HoodLat[inThisCellInd], $
                                     LonOut, LatOut)

              ;; if ARG_PRESENT(wallTime) then begin
              ;;     t2 = SYSTIME(/UTC, /SECONDS)
              ;;     wallTime.distance += (t2 - t1)
              ;;     t1 = t2
              ;; endif

              radMeshRad[arrayCount] = MEAN(rad_dist_xk)

          endif else begin


;             Use the average longitude and latitude for obs in this
;             radial mesh cell, and calculate distance and covariance
;             later (outside this nested for loop) if necessary.

;              radMeshLon[arrayCount] = MEAN(HoodLon[inThisCellInd])
;              radMeshLat[arrayCount] = MEAN(HoodLat[inThisCellInd])

              if KEYWORD_SET(radialAveraging) then begin
;                 Need to be careful calculating the average
;                 angle.
                  radMeshAngle = ATAN(MEAN(SIN(hoodAngle[inThisCellInd])), $
                                      MEAN(COS(hoodAngle[inThisCellInd])))
;                  radMeshAngle = MEAN(hoodAngle[inThisCellInd])
                  radMeshRad[arrayCount] = MEAN(hoodDist[inThisCellInd])
                  radMeshLon[arrayCount] = $
                      LonOut + $
                      radMeshRad[arrayCount] / MPerDegLon * COS(radMeshAngle)
                  radMeshLat[arrayCount] = $
                      LatOut + $
                      radMeshRad[arrayCount] / MPerDegLat * SIN(radMeshAngle)
              endif else begin
                  radMeshLon[arrayCount] = MEAN(HoodLon[inThisCellInd])
                  radMeshLat[arrayCount] = MEAN(HoodLat[inThisCellInd])
              endelse

          endelse


;         Calculate average obs value (and covariance if necessary)
;         for this radial mesh cell.

          cellKWA = HoodKWA[inThisCellInd]
          totalCellKWA = TOTAL(cellKWA)
          meanCellKWA = totalCellKWA / inThisCellCount
          radMeshKWA[arrayCount] = meanCellKWA
          radMeshVal[arrayCount] = $
              TOTAL(HoodVal[inThisCellInd] * cellKWA) / totalCellKWA

          arrayCount = arrayCount + 1L

      endfor

  endfor

  ;; if ARG_PRESENT(wallTime) then begin
  ;;     t2 = SYSTIME(/UTC, /SECONDS)
  ;;     wallTime.general += (t2 - t1)
  ;;     t1 = t2
  ;; endif

;  if (NOT(LMGR(/RUNTIME)) and KEYWORD_SET(sample)) then STOP

  if (arrayCount ne noNullCells) then MESSAGE, 'PROGRAMMING ERROR #03'
  
;  if NOT(KEYWORD_SET(useCovarianceMeans)) then begin


; Speed improvement effort:
  t2 = SYSTIME(/UTC, /SECONDS)
  time2 = t2 - t1
  t1 = t2


;     Generate covariances for radial mesh, replacing radial mesh mean
;     covariance. Using radMeshRad as calculated above will lead to
;     major problems in the solver because those distances are not
;     consistent with the use of average longitude and latitude to
;     locate radial mesh cells.

  if (NumAngles gt 1) then begin

          ;; if ARG_PRESENT(wallTime) then begin
          ;;     t2 = SYSTIME(/UTC, /SECONDS)
          ;;     wallTime.general += (t2 - t1)
          ;;     t1 = t2
          ;; endif

      radMeshRad = DISTANCE(DistPrecision, $
                            radMeshLon, radMeshLat, $
                            LonOut, LatOut)

          ;; if ARG_PRESENT(wallTime) then begin
          ;;     t2 = SYSTIME(/UTC, /SECONDS)
          ;;     wallTime.distance += (t2 - t1)
          ;;     t1 = t2
          ;; endif

  endif

  radMeshCovar = sillSpher - $
                 SPHERICAL_SEMIVARIOGRAM_FUNC(radMeshRad, $
                                              ParamsSpher)

;  endif

;  if (NOT(LMGR(/RUNTIME)) and KEYWORD_SET(sample)) then STOP

; VARIABLES USED BELOW:
; radMeshLon
; radMeshLat
; radMeshCovar
; radMeshVal
; radMeshRad (if NumAngles eq 1)

;  order = REVERSE(SORT(radMeshCovar))
;  radMeshLon = radMeshLon[order]
;  radMeshLat = radMeshLat[order]
;  radMeshCovar = radMeshCovar[order]
;  radMeshVal = radMeshVal[order]
;  radMeshRad = radMeshRad[order]

; Speed improvement effort:
  t2 = SYSTIME(/UTC, /SECONDS)
  time3 = t2 - t1
  t1 = t2


; Calculate covariance matrix between representative locations in
; non-empty cells of the radial mesh.

  count = 0L


; The allDist and allCovar variables are for debugging only.

  allDist = DBLARR(noNullCells * (noNullCells - 1) / 2)
  allCovar = allDist

  for j_rm = 0L, noNullCells - 1L do begin

;      radMeshCovar_2d[j_rm, j_rm] = sillSpher ; INITITALIZED TO THIS

      if (NumAngles eq 1) then begin

          rad_dist_j_rm = ABS(radMeshRad[0:j_rm - 1L] - radMeshRad[j_rm])

      endif else begin

          ;; if ARG_PRESENT(wallTime) then begin
          ;;     t2 = SYSTIME(/UTC, /SECONDS)
          ;;     wallTime.general += (t2 - t1)
          ;;     t1 = t2
          ;; endif

          rad_dist_j_rm = DISTANCE(DistPrecision, $
                                   radMeshLon[0:j_rm - 1L], $
                                   radMeshLat[0:j_rm - 1L], $
                                   radMeshLon[j_rm], $
                                   radMeshLat[j_rm])

          ;; if ARG_PRESENT(wallTime) then begin
          ;;     t2 = SYSTIME(/UTC, /SECONDS)
          ;;     wallTime.distance += (t2 - t1)
          ;;     t1 = t2
          ;; endif

      endelse

      covar_j_rm = sillSpher - $
                   SPHERICAL_SEMIVARIOGRAM_FUNC(rad_dist_j_rm, ParamsSpher)

      for i_rm = 0L, j_rm - 1L do begin ; i_rm < j_rm always

          rad_dist_ij = rad_dist_j_rm[i_rm]

          radMeshCovar_2d[i_rm, j_rm] = covar_j_rm[i_rm]
          radMeshCovar_2d[j_rm, i_rm] = covar_j_rm[i_rm]

          allDist[count] = rad_dist_ij
          allCovar[count] = covar_j_rm[i_rm]

          count++

      endfor

  endfor


; Add a row and column for the Lagrange factor that ensures Kriging
; weights will sum to 1.0. 

  radMeshKCovar_2d = DBLARR(noNullCells + 1L, noNullCells + 1L)
  radMeshKCovar_2d[0L: noNullCells - 1L, $
                   0L: noNullCells - 1L] = radMeshCovar_2d
  radMeshKCovar_2d[0L: noNullCells - 1L, noNullCells] = 1.0D
  radMeshKCovar_2d[noNullCells, 0L: noNullCells - 1L] = 1.0D
  radMeshKCovar_2d[noNullCells, noNullCells] = 0.0D

  radMeshKCovar = [radMeshCovar, 1.0D]

  npoints = noNullCells + 1L

  weight = DBLARR(noNullCells + 1L)

; Speed improvement effort:
  t2 = SYSTIME(/UTC, /SECONDS)
  time4 = t2 - t1
  t1 = t2


; Solve with LUSOL and LUMPROVE, which in our experience is the
; fastest built-in IDL solver.

  hkc_ludc = radMeshKCovar_2d
  LUDC, hkc_ludc, index, /DOUBLE
  weight = LUSOL(hkc_ludc, index, radMeshKCovar, /DOUBLE)
;GF 20190913
  lagrange_param = weight[noNullCells]
  weight = weight[0L: noNullCells - 1L]

; Speed improvement effort:
  t2 = SYSTIME(/UTC, /SECONDS)
  time5 = t2 - t1
  t1 = t2

;--weighting function plotting--
;doPlots = 0
;if (ROUND(RANDOMU(seed) * 5000.0) eq 1) then doPlots = 1
;if doPlots then weight1 = weight


; Eliminate negative weights.

  newWeight = $
      REMOVE_NEGATIVE_KRIGING_WEIGHT(weight, radMeshCovar, /DEUTSCH)
  weight = newWeight
;--weighting function plotting--
;if doPlots then weight2 = weight


; Eliminate upswing in weight at large distances.

  newWeight = REMOVE_KRIGING_WEIGHT_UPSWING(weight, radMeshCovar)
  weight = newWeight
;--weighting function plotting--
;if doPlots then weight3 = weight

;--weighting function plotting--
;if doPlots then begin
;    order = SORT(radmeshrad)
;    maxWeight = MAX([weight1, weight2, weight3])
;    PLOT, radMeshRad[order] / 1000.0, weight1[order], $
;          PSYM = -4, SYMSIZE = 1, $
;          XRANGE = [0.0, MaxLagMeters / 1000.0D], XSTYLE = 1, $
;          YRANGE = [-0.1, maxWeight], $
;;        YRANGE = [-0.1, 0.9], YSTYLE = 1, $
;          TITLE = 'KRM Weight and Covariance'
;    PLOTS, !X.CRange, [0.0, 0.0], COLOR = 80
;    OPLOT, radMeshRad[order] / 1000.0, weight1[order], $
;           PSYM = -4, SYMSIZE = 1, COLOR = 125
;    OPLOT, radMeshRad[order] / 1000.0, $
;           radMeshCovar[order] / MAX(radMeshCovar) * !Y.CRange[1], $
;           LINESTYLE = 2
;    OPLOT, radMeshRad[order] / 1000.0, weight2[order], $
;           COLOR = 190, PSYM = -6, SYMSIZE = 1
;    OPLOT, radMeshRad[order] / 1000.0, weight3[order], $
;           COLOR = 255, PSYM = -4, SYMSIZE = 2
;;    move = GET_KBRD(1)
;endif


; Perform interpolation.

  oldWeight = weight
  weight = weight * radMeshKWA
  weight = weight / TOTAL(weight)
  valKrige = TOTAL(weight * radMeshVal)
; GF 20190913
;+
; Calculate error variance based on covariances of radial-mesh
; averages. This does not account for the covariances associated with
; those individual averages; it is based solely on the error variance
; associated with the covariance model defined by the semivariogram.
;-
  errVar = ParamsSpher[1] - TOTAL(weight * radMeshCovar) - lagrange_param


; Speed improvement effort:
  t2 = SYSTIME(/UTC, /SECONDS)
  time6 = t2 - t1


  ;; if (NOT(LMGR(/RUNTIME)) and KEYWORD_SET(sample)) then begin
  ;;     USR_MSG, 'Last stop in GEO_KRIGE_SPHERICAL_KRM'
  ;;     STOP
  ;; endif

  ;; if ARG_PRESENT(wallTime) then begin
  ;;     t2 = SYSTIME(/UTC, /SECONDS)
  ;;     wallTime.general += (t2 - t1)
  ;;     t1 = t2
  ;; endif

  ;; if showplot then begin
  ;;     order = SORT(radMeshRad)
  ;;     PLOT, radMeshRad[order], weight[order], PSYM = -4, $
  ;;           TITLE = 'Radial Mesh Kriging Weight', $
  ;;           XTITLE = 'Distance from Analysis Point'
  ;;     print, 'press a key' & move = get_kbrd(1)
  ;;     PLOT, radMeshRad[order], radMeshCovar[order], PSYM = -4, $
  ;;           TITLE = 'Radial Mesh Covariance w/r/t Analysis Point', $
  ;;           XTITLE = 'Distance from Analysis Point'
  ;;     OPLOT, radMeshRad[order], testCovar[order], PSYM = -6, COLOR = 150
  ;;     print, 'press a key' & move = get_kbrd(1)
  ;;     order = SORT(allDist)
  ;;     PLOT, allDist[order], allCovar[order], PSYM = -4, $
  ;;           TITLE = 'Radial Mesh Point-to-Point Covariance', $
  ;;           XTITLE = 'Separation'
  ;;     print, 'press a key' & move = get_kbrd(1)
  ;; endif


;  print, 'times: ', time1, time2, time3, time4, time5, time6

  timeAccount = [time1, time2, time3, time4, time5, time6]

  RETURN, valKrige



end
