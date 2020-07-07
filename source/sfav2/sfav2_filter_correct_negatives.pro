FUNCTION SFAV2_FILTER_CORRECT_NEGATIVES, $
    MaxSepMeters, $   ; Maximum separation (e.g., maxLagForCN)
    MPerDegLonRef, $  ; Lower limit on meters per degree longitude
    MPerDegLatRef, $  ; Lower limit on meters per degree latitude
    LonGrid_2D, $     ; Full 2-D longitude grid for input data
    LatGrid_2D, $     ; Full 2-D latitude grid for input data
    ValGrid_2D, $     ; Full 2-D input data grid (pre-assimilation)
    ObsLon, $         ; 1-D array of observation point longitudes
    ObsLat, $         ; 1-D array of observation point latitudes
    ObsVal, $         ; 1-D array of observation point values
    ObsSubsetInd, $   ; indices of observation points to consider
    ObsSubsetCount, $ ; number of observation points to consider
    All_dist_2D, $    ; 2-D array of distances between obs points (meters)
    All_k_2D, $       ; 2-D array of indices for obs point neighbors
    All_n_1D, $       ; 1-D array of counts for each row of above
    DISTANCE_PRECISION = DistPrecision, $
    HASH = hash

; Filter observation data to remove unneeded "correct negatives";
; i.e., zero-valued observations that are in the same correlation
; neighborhood as exclusively other zero-valued observations and
; zero-valued grid points.
;
; The value returned is an index, relative to ObsSubsetInd, of points
; that should be retained for analysis.
;
; The MaxSepMeters should represent TWICE the distance over which
; correlation is expected (e.g., twice the range of a
; semivariogram). This way, for example, a zero-valued analysis grid
; point located midway between a zero-valued observation and a
; non-zero-valued observation (or grid point) will be influenced by
; both as long as the distance between the two is less that double the
; correlation distance (semivariogram range).
  
; Each row "j" of All_dist_2D provides the distances between ObsVal[j]
; and all neighboring points in ObsVal within some limit assumed to be
; greater than or equal to MaxSepMeters. The corresponding row of
; All_k_2D provides the indices of those neighbors. The corresponding
; value All_n_1D[j] gives the number of columns of all_dist_2d[*,j]
; and All_k_2D[*,j] that should be considered. Columns of
; all_dist_2d[*,j] beyond All_n_1D[j] - 1 are assumed to be no-data
; values and columns of All_k_2D[*,j] beyond All_n_1D[j] - 1 should
; also be a no-data index value such as -1.


; Check arguments.

  sz1 = SIZE(LonGrid_2D)
  sz2 = SIZE(LatGrid_2D)
  sz3 = SIZE(ValGrid_2D)

  if ((sz1[0] ne 2) or (sz2[0] ne 2) or (sz3[0] ne 2)) then begin
      ERR_MSG, 'Input grid longitudes, latitudes, and values ' + $
               'must be 2-D arrays.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if ((sz2[1] ne sz1[1]) or (sz3[1] ne sz1[1]) or $
      (sz2[2] ne sz1[2]) or (sz3[2] ne sz1[2])) then begin
      ERR_MSG, 'Inconsistent row/column indices for gridded inputs.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  sz1 = !NULL
  sz2 = !NULL
  sz3 = !NULL

  numObs = N_ELEMENTS(ObsVal)
  if ((N_ELEMENTS(ObsLon) ne numObs) or (N_ELEMENTS(ObsLat) ne numObs)) $
      then begin
      ERR_MSG, 'Inconsistent observation data array sizes.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if (N_ELEMENTS(ObsSubsetInd) ne ObsSubsetCount) then begin
      ERR_MSG, 'Inconsistent observation subset index/count.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  szd = SIZE(All_dist_2D)
  szk = SIZE(All_k_2D)
  szn = SIZE(All_n_1D)

  if ((szd[0] ne 2) or (szk[0] ne 2)) then begin
      ERR_MSG, 'Input observation distance and index arrays is not ' + $
               '2-dimensional.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if ((szd[2] ne numObs) or (szk[2] ne numObs)) then begin
      ERR_MSG, 'Input observation distance and index array row ' + $
               'dimensions does not match observation count.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if (szn[0] ne 1) then begin
      ERR_MSG, 'Input neighbor count array is not 1-dimensional.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if (N_ELEMENTS(All_n_1D) ne numObs) then begin
      ERR_MSG, 'Input neighbor count array dimension does not match ' + $
               'observation count.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if (MAX(All_n_1D) gt (numObs - 1)) then begin
      ERR_MSG, 'Input neighbor count array data does not match ' + $
               'observation count.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
  endif

  if NOT(KEYWORD_SET(DistPrecision)) then begin
      DistPrecision = 2
  endif else begin
      if ((DistPrecision lt 0) or (DistPrecision gt 3)) then begin
          ERR_MSG, 'DISTANCE_PRECISION must be 0, 1, 2, or 3.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN, -1
      endif
  endelse


; Estimate latitude and longitude ranges corresponding to MaxSepMeters.

  maxSepDegLat = MaxSepMeters / MPerDegLatRef
  maxSepDegLon = MaxSepMeters / MPerDegLonRef

  aFlag = MAKE_ARRAY(ObsSubsetCount, /BYTE, VALUE = 1B)

  if KEYWORD_SET(hash) then begin
      if (ObsSubsetCount lt 50) then begin
          hashes = ObsSubsetCount
          lastProgress = -1
      endif else begin
          hashes = 50
          lastProgress = 0
      endelse
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif


; Loop over the provided subset of points.

  ndv = -99999.0 ; local no-data value

  for ac = 0, ObsSubsetCount - 1 do begin

      if KEYWORD_SET(hash) then begin
          progress = FIX(FLOAT(ac) / FLOAT(ObsSubsetCount - 1) * $
                         FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      k = ObsSubsetInd[ac]

      if (ObsVal[k] gt 0.0) then CONTINUE ; use this observation

      xk = ObsLon[k]            ; longitude of obs point
      yk = ObsLat[k]            ; latitude of obs point


;     Identify neighboring obs points.

      d2dRow = All_dist_2D[0:All_n_1D[k] - 1L, k]
      k2dRow = All_k_2D[0:All_n_1D[k] - 1L, k]

      pInHoodInd = WHERE(d2dRow lt MaxSepMeters, $
                         pInHoodCount) ; column index in k2dRow
      maxObsInHood = ndv
      if (pInHoodCount gt 0) then begin
          pInHoodInd = k2dRow[pInHoodInd] ; actual obs index
          pInHoodInd = SETINTERSECTION(pInHoodInd, ObsSubsetInd)
          if (pInHoodInd[0] eq -1) then $
              pInHoodCount = 0 $
          else $
              pInHoodCount = N_ELEMENTS(pInHoodInd)
          if (pInHoodCount gt 0) then $
              maxObsInHood = MAX(ObsVal[pInHoodInd])
      endif


;     Test first guess grid values "in the box" surrounding the obs
;     point.

      gInBoxInd = WHERE((ABS(LonGrid_2D - xk) le (1.0D * maxSepDegLon)) and $
                        (ABS(LatGrid_2D - yk) lt (1.0D * maxSepDegLat)) and $
                        (ValGrid_2D gt 0.0), $
                        gInBoxCount)

      if (gInBoxCount eq 0) then begin
;         there are no nonzero grid values in the box
          if (pInHoodCount eq 0) then begin
;             there are no obs points in the neighborhood
              aflag[ac] = 0B
              CONTINUE
          endif
          if (maxObsInHood eq 0.0) then begin
;             there are no nonzero obs points in the neighborhood
              aFlag[ac] = 0B
              CONTINUE
          endif
          CONTINUE ; no grid points in box = none in neighborhood
      endif


;     Re-test grid values and obs points, this time including grid
;     points in the actual neighborhood (i.e., circle, not box)
;     surrounding the obs point in question.

      gDInBox = DISTANCE(DistPrecision, $
                         xk, yk, $
                         LonGrid_2D[gInBoxInd], LatGrid_2D[gInBoxInd])
      gInHoodInd = WHERE(gDInBox lt (1.0D * MaxSepMeters), gInHoodCount)

      if (gInHoodCount eq 0) then begin
;         there are no nonzero grid values in the neighborhood
          if (pInHoodCount eq 0) then begin
;             there are no obs points in the neighborhood
              aflag[ac] = 0B
              CONTINUE
          endif
          if (maxObsInHood eq 0.0) then begin
;             there are no nonzero obs points in the neighborhood
              aFlag[ac] = 0B
              CONTINUE
          endif
      endif

  endfor

  if KEYWORD_SET(hash) then PRINT, ''

  RETURN, WHERE(aFlag eq 1)

end
