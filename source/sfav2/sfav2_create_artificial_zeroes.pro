PRO SFAV2_CREATE_ARTIFICIAL_ZEROES, $
    MinTempGrid, $   ; gridded min. temp. for analysis period
    AveTempGrid, $
    SnowfallGrid, $  ; current state of snowfall analysis grid
    LonGrid_2d, $    ; grid longitudes
    LatGrid_2d, $    ; grid latitudes
    LonAssim, $      ; longitudes of points in planned assimilation
    LatAssim, $      ; latitudes of points in planned assimilation
    MinTempCutoff, $ ; above this min. temperature snow is not allowed
    AveTempCutoff, $ ; above this ave. temperature snow is not allowed
    ArtZeroProb, $  ; chance of a warm cell becoming a fake zero
    MinDistToObs, $  ; minimum distance from fake zero to points
    MaxDistToObs, $  ; maximum distance from fake zero to points
    MPerDegLonRef, $ ; smallest meters per degree longitude
    MPerDegLatRef, $ ; smallest meters per degree latitude
    Ndv, $           ; no-data value for input grids
    warmInd, $   ; OUT - grid points that will be used as artificial zeroes
    warmLon, $   ; OUT - artificial zero longitudes
    warmLat, $   ; OUT - artificial zero latitudes
    warmFakeZ, $ ; OUT - error in SnowfallGrid implied by artificial zero
    warmCount, $
    DISTANCE_PRECISION = DistPrecision, $
    VERBOSE = verbose, $
    CSV_FILE = csvFilePath, $
    MAP_STRUCT = map_struct


; Generate artificial zero reports where it is too warm for snow to
; accumulate.

; Arguments that are capitalized are input arguments that must not be
; modified.

  warmInd = !NULL
  warmLon = !NULL
  warmLat = !NULL
  warmFakeZ = !NULL
  warmCount = 0

  gridSize = SIZE(MinTempGrid)
  if (gridSize[0] ne 2) then begin
      ERR_MSG, 'Input grids must be 2-dimensional arrays.'
      RETURN
  endif
  gridCols = gridSize[1]
  gridRows = gridSize[2]
  gridSize = SIZE(AveTempGrid)
  if (gridSize[0] ne 2) then begin
      ERR_MSG, 'Input grids must be 2-dimensional arrays.'
      RETURN
  endif
  if (gridSize[1] ne gridCols) then begin
      ERR_MSG, 'Input grid column dimensions do not match.'
      RETURN
  endif
  if (gridSize[2] ne gridRows) then begin
      ERR_MSG, 'Input grid row dimensions do not match.'
      RETURN
  endif
  gridSize = SIZE(SnowfallGrid)
  if (gridSize[0] ne 2) then begin
      ERR_MSG, 'Input grids must be 2-dimensional arrays.'
      RETURN
  endif
  if (gridSize[1] ne gridCols) then begin
      ERR_MSG, 'Input grid column dimensions do not match.'
      RETURN
  endif
  if (gridSize[2] ne gridRows) then begin
      ERR_MSG, 'Input grid row dimensions do not match.'
      RETURN
  endif
  gridSize = SIZE(LonGrid_2d)
  if (gridSize[0] ne 2) then begin
      ERR_MSG, 'Input grids must be 2-dimensional arrays.'
      RETURN
  endif
  if (gridSize[1] ne gridCols) then begin
      ERR_MSG, 'Input grid column dimensions do not match.'
      RETURN
  endif
  if (gridSize[2] ne gridRows) then begin
      ERR_MSG, 'Input grid row dimensions do not match.'
      RETURN
  endif
  gridSize = SIZE(LatGrid_2d)
  if (gridSize[0] ne 2) then begin
      ERR_MSG, 'Input grids must be 2-dimensional arrays.'
      RETURN
  endif
  if (gridSize[1] ne gridCols) then begin
      ERR_MSG, 'Input grid column dimensions do not match.'
      RETURN
  endif
  if (gridSize[2] ne gridRows) then begin
      ERR_MSG, 'Input grid row dimensions do not match.'
      RETURN
  endif

  assimPointsCount = N_ELEMENTS(LonAssim)
  if (N_ELEMENTS(LatAssim) ne assimPointsCount) then begin
      ERR_MSG, 'Input lon/lat points array dimensions do not match.'
      RETURN
  endif

  artZeroProb_ = ArtZeroProb ; now we can change it

  if NOT(KEYWORD_SET(DistPrecision)) then begin
      DistPrecision = 2
  endif else begin
      if ((DistPrecision lt 0) or (DistPrecision gt 3)) then begin
          ERR_MSG, 'DISTANCE_PRECISION must be 0, 1, 2, or 3.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
  endelse

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Generating artificial zero reports in warm regions.'


; Identify warm cells, where the minimum daily temperature is above
; freezing by more than minTempCutoffDegC. Here, "or" is used for the
; min/ave temperature criteria to be more aggressive about the spread
; of snowfall into areas where we do not have it. We try to be
; conservative about "creating" snowfall where previous steps in the
; analysis have not produced it, and this choice is consistent with
; that.

  warmInd_ = WHERE((MinTempGrid ne Ndv) and $
                   (SnowfallGrid ne Ndv) and $
                   ((MinTempGrid gt MinTempCutoff) or $ ; OR/AND
                    (AveTempGrid gt AveTempCutoff)), $  ; 20181010
                   warmCount_)

  if (warmCount_ gt 0) then begin

;+
;     Some warm cells separated from real observations (in the
;     assimPointsInd subset) by at least MinDistToObs and at most
;     MaxDistToObs will be given artificial zero reports. The
;     likelihood of an in-range warm cell receiving an artifical zero
;     is determined by the artZeroProb_.
;-
      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Identified ' + STRCRA(warmCount_) + $
                   ' cells too warm for snow accumulation.'

      minDistDegLat = MinDistToObs / MPerDegLatRef
      minDistDegLon = MinDistToObs / MPerDegLonRef

      maxDistDegLat = MaxDistToObs / MPerDegLatRef
      maxDistDegLon = MaxDistToObs / MPerDegLonRef

      if NOT(ISA(LonGrid_2d)) then STOP
      if NOT(ISA(LatGrid_2d)) then STOP

      warmLon_ = LonGrid_2d[warmInd_]
      warmLat_ = LatGrid_2d[warmInd_]

;+
;     Only keep warm data inside the maximum bounding box of the
;     observations in assimPointsInd.
;-
      lowLat = MIN(LatAssim)
      highLat = MAX(LatAssim)
      lowLon = MIN(LonAssim)
      highLon = MAX(LonAssim)

      ind = WHERE((warmLat_ ge (lowLat - maxDistDegLat)) $
                  and $
                  (warmLat_ le (highLat + maxDistDegLat)) $
                  and $
                  (warmLon_ ge (lowLon - maxDistDegLon)) $
                  and $
                  (warmLon_ le (highLon + maxDistDegLon)), $
                  count)

;      ind = WHERE(((highLat + maxDistDegLat) gt warmLat_) $
;                  and $
;                  ((lowLat - maxDistDegLat) lt warmLat_) $
;                  and $
;                  ((highLon + maxDistDegLon) gt warmLon_) $
;                  and $
;                  ((lowLon - maxDistDegLon) lt warmLon_), $
;                  count)

      if (count gt 0) then begin

          warmInd_ = warmInd_[ind]
          warmLon_ = warmLon_[ind]
          warmLat_ = warmLat_[ind]
          warmCount_ = count

          if KEYWORD_SET(verbose) then $
              USR_MSG, '           ' + STRCRA(warmCount_) + $
                       ' warm cells are in the wide neighborhood of all' + $
                       ' assimilation points.'

          flag = BYTARR(warmCount_)
          distToNearestObs = MAKE_ARRAY(warmCount_, VALUE = Ndv)

          if KEYWORD_SET(verbose) then begin
              if (warmCount_ lt 50) then begin
                  hashes = warmCount_
                  lastProgress = -1
              endif else begin
                  hashes = 50
                  lastProgress = 0
              endelse
              for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
              PRINT, ''
          endif

          for wc = 0L, warmCount_ - 1L do begin

              if KEYWORD_SET(verbose) then begin
                  progress = FIX(FLOAT(wc) / FLOAT(warmCount_ - 1L) * $
                                 FLOAT(hashes))
                  if (progress gt lastProgress) then begin
                      lastProgress = progress
                      PRINT, FORMAT='($,"#")'
                  endif
              endif

              warmLonVal = warmLon_[wc]
              warmLatVal = warmLat_[wc]

              inBoxInd = WHERE((ABS(warmLonVal - LonAssim) $
                                lt maxDistDegLon) and $
                               (ABS(warmLatVal - LatAssim) $
                                lt maxDistDegLat), $
                               inBoxCount)
              if (inBoxCount eq 0) then CONTINUE ; warm cell too remote

              lonAssimInBox = LonAssim[inBoxInd]
              latAssimInBox = LatAssim[inBoxInd]

              dInBox = DISTANCE(DistPrecision, $
                                warmLonVal, warmLatVal, $
                                lonAssimInBox, latAssimInBox)

              boxInHoodInd = WHERE(dInBox lt MaxDistToObs, $
                                   inHoodCount)
              if (inHoodCount eq 0) then CONTINUE ; warm cell too remote

              distToNearestObs[wc] = MIN(dInBox[boxInHoodInd])

              tooCloseInd = WHERE(dInBox lt MinDistToObs, $
                                  tooCloseCount)
              if (tooCloseCount gt 0) then CONTINUE

              flag[wc] = 1B

          endfor

          if KEYWORD_SET(verbose) then PRINT, ''

          ind = WHERE(flag, count)

          if (count gt 0) then begin

              warmInd_ = warmInd_[ind]
              warmLon_ = warmLon_[ind]
              warmLat_ = warmLat_[ind]
              warmCount_ = count

              if KEYWORD_SET(verbose) then $
                  USR_MSG, '           ' + STRCRA(warmCount_) + $
                           ' warm cells are between ' + $
                           STRCRA(MinDistToObs) + ' and ' + $
                           STRCRA(MaxDistToObs) + ' meters from an' + $
                           ' assimilation point.'

;+
;             Adjust artZeroProb_ if needed to make sure
;             the number of artificial zeroes this process
;             generates is no more than roughly half the number of
;             assimilation points.
;-
              artZeroProb_ = $
                  artZeroProb_ < $
                  FLOAT(assimPointsCount) * 0.5 / FLOAT(warmCount_)

              if ((artZeroProb_ ne ArtZeroProb) and $
                  KEYWORD_SET(verbose)) then $
                      USR_MSG, 'Probability of artificial zeroes adjusted ' + $
                               'from ' + STRCRA(ArtZeroProb * 100.0) + '% ' + $
                               'to ' + STRCRA(artZeroProb_ * 100.0) + '% ' + $
                               'to preserve dominance of real observations.'

              ind = WHERE(RANDOMU(seed, warmCount_) lt $
                          artZeroProb_, count)

              if (count gt 0) then begin

                  warmInd_ = warmInd_[ind]
                  warmLon_ = warmLon_[ind]
                  warmLat_ = warmLat_[ind]
                  warmCount_ = count

                  if KEYWORD_SET(verbose) then $
                      USR_MSG, 'Generating ' + $
                               STRCRA(warmCount_) + $
                               ' artificial zeroes to accompany ' + $
                               STRCRA(assimPointsCount) + $
                               ' assimilation points.'

                  warmFakeZ = SnowfallGrid[warmInd_]

             endif

          endif

      endif

  endif

  if ((N_ELEMENTS(csvFilePath) eq 1) and ISA(csvFilePath, 'String')) $
      then begin
      OPENW, lun, csvFilePath, /GET_LUN
      PRINTF, lun, 'longitude,latitude,snowfall_error_inches'
      for wc = 0L, warmCount_ -1L do begin
          PRINTF, lun, $
                  STRCRA(STRING(warmLon_[wc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(STRING(warmLat_[wc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(warmFakeZ[wc])
      endfor
      FREE_LUN, lun
  endif

  if (warmCount_ gt 0) then begin
      warmInd = warmInd_
      warmLon = warmLon_
      warmLat = warmLat_
      warmCount = warmCount_
;20-55 (35), -60--130 (70)
      if (KEYWORD_SET(verbose) and KEYWORD_SET(map_struct)) then begin
          uv = MAP_PROJ_FORWARD(warmLon, warmLat, $
                                MAP_STRUCTURE = map_struct)
          u = REFORM(uv[0,*])
          v = REFORM(uv[1,*])
          symX = [FINDGEN(9) / 8.0, $
                  1.0 - FINDGEN(9) / 8.0, $
                  FINDGEN(9) / 8.0]
          symY = [REPLICATE(1.0, 9), $
                  1.0 - FINDGEN(9) / 8.0, $
                  REPLICATE(0.0, 9)]
          USERSYM, symX, symY
          PLOTS, u, v, PSYM = 8, COLOR = 0, SYMSIZE = 1.5
;          OPLOT, warmLon_, warmLat_, COLOR = 150, PSYM = 6
      endif

  endif else begin
      if KEYWORD_SET(verbose) then $
          USR_MSG, 'No artificial zeroes generated.'
  endelse

  RETURN

end
