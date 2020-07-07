PRO SFAV2_REMOVE_WARM_SNOWFALL, snowfallGrid, $
                                MinTempGrid, $
                                AveTempGrid, $
                                LonGrid, $
                                LatGrid, $
                                NoDataValue, $
                                SiteObs, $
                                SiteLon, $
                                SiteLat, $
                                warmSnowMaximum, $
                                warmMinTempThreshold, $
                                warmAveTempThreshold, $
                                MinDistToObs, $
;                                MaxDistToObs, $
                                MetersPerDegLonRef, $
                                MetersPerDegLatRef, $
                                DISTANCE_PRECISION = DistPrecision, $
                                VERBOSE = Verbose
;+
; Remove low snowfall amounts occurring in areas that are too warm,
; based on minimum and average temperature grids covering the snowfall
; accumulation period.
;
; The temperature criteria imposed on low snowfall amounts are similar
; to the logic of SFAV2_CREATE_ARTIFICIAL_ZEROES, but apply to the
; entire snowfallGrid. The purpose of this procedure is not to
; indirectly perform QC on observations that reported snow where none
; occurred; it is meant to prevent an overly simplistic assimilation
; method from smearing snow that DID fall over stations into
; nearby--but not too distant--areas where temperatures are too warm
; to support snow. We still refuse to do this any closer than
; MinDistToObs from nonzero observations.
;
; NOTES:
;
; * May want to restrict this to cases where we get analysis
;   temperatures. That would mean we need
;   GET_MIN_MAX_AVE_2M_HRRR_TEMP, GET_MIN_MAX_AVE_2M_RAP_TEMP, and
;   GET_MIN_MAX_AVE_RUC_2M_TEMP to return a "perfect" flag indicating
;   whether or not they used analysis data to evaluate all hours of
;   temperature data.
;-
  refGridSize = SIZE(snowfallGrid)
  if (refGridSize[0] ne 2) then STOP
  nCols = refGridSize[1]
  nRows = refGridSize[2]
  gridSize = SIZE(MinTempGrid)
  if (gridSize[0] ne 2 ) then STOP
  if (gridSize[1] ne nCols) then STOP
  if (gridSize[2] ne nRows) then STOP
  gridSize = SIZE(AveTempGrid)
  if (gridSize[0] ne 2 ) then STOP
  if (gridSize[1] ne nCols) then STOP
  if (gridSize[2] ne nRows) then STOP
  gridSize = SIZE(LonGrid)
  if (gridSize[0] ne 2 ) then STOP
  if (gridSize[1] ne nCols) then STOP
  if (gridSize[2] ne nRows) then STOP
  gridSize = SIZE(LatGrid)
  if (gridSize[0] ne 2 ) then STOP
  if (gridSize[1] ne nCols) then STOP
  if (gridSize[2] ne nRows) then STOP

  if NOT(KEYWORD_SET(DistPrecision)) then begin
      DistPrecision = 2
  endif else begin
      if ((DistPrecision lt 0) or (DistPrecision gt 3)) then begin
          ERR_MSG, 'DISTANCE_PRECISION must be 0, 1, 2, or 3.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
  endelse

;+
; Select grid cells that were too warm to support snow accumulation
; but have snowfall in them (though not too much). Zero out the
; snowfall for these cells as long as they are at least MinDistToObs
; from a nonzero snowfall observation. If gridded "warm" snowfall
; amounts larger than warmSnowMaximum have survived the process, it is
; likely that there is some inaccuracy in the analysis temperatures,
; and snow is not removed from these grid cells.
;
; Note the AND/OR in the combined criteria for MinTempGrid and
; AveTempGrid. Originally, the idea was to use OR here. That means
; it is too warm for snow if the warmAveTempThreshold OR the
; warmMinTempThreshold is met. Put the other way, snow is
; allowed to accumulate if the average temperature is below the
; warmAveTempThreshold is met but only as long as (i.e., AND) the
; temperature also drops below the warmMinTempThreshold.
;
; If the OR is switched to AND, then it is easier to keep snow, but
; the basic original thinking behind including the minimum temperature
; criterion in the first place is no longer operative. The use of AND
; is consistent with being conservative about removing snowfall that
; previous steps in the analysis have produced.
;-
  warmSnowMaximum = 1.0 ; inches

  warmSnowInd = WHERE((MinTempGrid ne NoDataValue) and $
                      (AveTempGrid ne NoDataValue) and $
                      (snowfallGrid ne NoDataValue) and $
                      ((MinTempGrid ge warmMinTempThreshold) and $  ; AND/OR
                       (AveTempGrid ge warmAveTempThreshold)) and $ ; 20181010
                      (snowfallGrid gt 0.0) and $
                      (snowfallGrid le warmSnowMaximum), warmSnowCount)

  if (warmSnowCount gt 0) then begin

      if KEYWORD_SET(Verbose) then $
          USR_MSG, 'Generated snowfall on ' + STRCRA(warmSnowCount) + $
                   ' "warm snowfall" cells where MinTempGrid >= ' + $
                   STRCRA(warmMinTempThreshold) + $
                   ' and AveTempGrid >= ' + $
                   STRCRA(warmAveTempThreshold)

;+
;     Initialize the "remove snow" flag to 1 (yes), then set it to
;     zero for any observations too close to nonzero snowfall
;     reports.
;-
      removeSnowFlag = REPLICATE(1B, warmSnowCount)

      minDistDegLat = MinDistToObs / MetersPerDegLatRef
      minDistDegLon = MinDistToObs / MetersPerDegLonRef

;+
;     Get locations of warm cells.
;-
      warmLon = lonGrid[warmSnowInd]
      warmLat = latGrid[warmSnowInd]

      obsSnowInd = WHERE(SiteObs gt 0.0, obsSnowCount)

      if (obsSnowCount gt 0) then begin

;+
;         Set removeSnowFlag to zero for grid cells too close to
;         nonzero snowfall reports.
;-
          obsSnowfallLon = SiteLon[obsSnowInd]
          obsSnowfallLat = SiteLat[obsSnowInd]
          obsSnowfallVal = SiteObs[obsSnowInd]

          for wc = 0L, warmSnowCount - 1L do begin

              warmLonVal = warmLon[wc]
              warmLatVal = warmLat[wc]

              inBoxInd = WHERE((ABS(warmLonVal - obsSnowfallLon) $
                                lt minDistDegLon) and $
                               (ABS(warmLatVal - obsSnowfallLat) $
                                lt minDistDegLat), $
                               inBoxCount)
              if (inBoxCount eq 0) then CONTINUE

              obsSnowfallLonInBox = obsSnowfallLon[inBoxInd]
              obsSnowfallLatInBox = obsSnowfallLat[inBoxInd]

              dInBox = DISTANCE(DistPrecision, $
                                warmLonVal, warmLatVal, $
                                obsSnowfallLonInBox, obsSnowfallLatInBox)

              if (MIN(dInBox) lt MinDistToObs) then removeSnowFlag[wc] = 0B

          endfor

      endif

      obsSnowInd = !NULL
      ind = WHERE(removeSnowFlag eq 1, count)

      if (count gt 0) then begin

          if KEYWORD_SET(Verbose) then begin
              msg = 'Removing snowfall from ' + STRCRA(count) + ' of these.'
              if (count ne warmSnowCount) then $
                  msg = msg + ' The remaining ' + $
                        STRCRA(warmSnowCount - count) + $
                        ' are < ' + STRCRA(MinDistToObs) + $
                        ' meters from nonzero snowfall reports ' + $
                        ' and will therefore not be zeroed out.'
              USR_MSG, msg
          endif ; 3323 -> 3309 -> ????

          snowfallGrid[warmSnowInd[ind]] = 0.0

      endif else begin

          if KEYWORD_SET(Verbose) then $
              USR_MSG, 'All "warm snowfall" cells are too close ' + $
                       '(< ' + STRCRA(MinDistToObs) + ' meters) to ' + $
                       'nonzero snowfall reports to be zeroed out.'

      endelse

  endif

end
