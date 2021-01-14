; Generates an inventory of concurrent PRCP and SNOW reports from the
; GHCN Daily dataset.

; Duplicates the read_ghcn_us_ca_mx_by_year.pro functionality of
; old, using more modular code.

; This program is not needed any more to create the IDL save files
; used for generating a climatology of snowfall to liquid equivalent
; ratio (SLR), but it does create those files, which means future
; calls to READ_GHCND_US_CA_MX_BY_YEAR will benefit from them being
; there.

; Greg Fall, NOHRSC
; June 2019

  GHCND_dir = '/nwcdev/archive/GHCN_daily_archive'
  year = 1986 + INDGEN(33)
  year = 2010 + INDGEN(11)
  ndv = -9999
  startDate_MMDD = '0101'
  finishDate_MMDD = '0131'

  for yc = 0, N_ELEMENTS(year) - 1 do begin

      startDate_YYYYMMDD = STRCRA(year[yc]) + startDate_MMDD
      
      READ_GHCND_US_CA_MX_BY_YEAR, GHCND_dir, $
                                   year[yc], $
                                   ndv, $
                                   numDays, $
                                   numStations, $
                                   GHCNStationID, $
                                   GHCNData, $
                                   /VERBOSE

;+
;     Inventory data and report results.
;-
      bothNonNDVInd = WHERE((GHCNData.prcp ne ndv) and $
                            (GHCNData.snow ne ndv), $
                            bothNonNDVCount)
      if (bothNonNDVCount eq 0) then STOP
      bothNonNDVDayInd = bothNonNDVInd mod numDays 
      bothNonNDVStationInd = bothNonNDVInd / numDays
      order = SORT(bothNonNDVStationInd)
      orderedStations = bothNonNDVStationInd[order]
      uniqueStations = orderedStations[UNIQ(orderedStations)]
      order = -1L
      orderedStations = -1L
      numUniqueStations = N_ELEMENTS(uniqueStations)

      PRINT, 'Examining cases where both PRCP and SNOW are reported'
      PRINT, '(but either or both may be zero.'

      PRINT, 'Concurrent PRCP and SNOW reports exist for ' + $
             STRCRA(numUniqueStations) + ' stations out of ' + $
             STRCRA(numStations) + '.'
  
;     Loop over days. For each day record the number of concurrent
;     reports of PRCP and SNOW.

      PRINT, '   date   | both PRCP and SNOW | PRCP only | SNOW only'
      PRINT, '----------|--------------------|-----------|----------'

      startDate_Julian = YYYYMMDDHH_TO_JULIAN(startDate_YYYYMMDD + '00')

      for dc = 0, numDays - 1 do begin

          date_Julian = startDate_Julian + DOUBLE(dc)
          date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
          date_YYYYMMDD = STRMID(date_YYYYMMDDHH, 0, 8)

          dailyData = REFORM(GHCNData[dc, *])

          bothInd = WHERE((dailyData.prcp ne -9999) and $
                          (dailyData.snow ne -9999), $
                          bothCount)
          prcpInd = WHERE((dailyData.prcp ne -9999) and $
                          (dailyData.snow eq -9999), $
                          prcpCount)
          snowInd = WHERE((dailyData.prcp eq -9999) and $
                          (dailyData.snow ne -9999), $
                          snowCount)
          PRINT, ' ' + date_YYYYMMDD + ' | ' + $
                 STRING(bothCount, FORMAT = '(i18)') + ' | ' + $
                 STRING(prcpCount, FORMAT = '(i9)') + ' | ' + $
                 STRING(snowCount, FORMAT = '(i9)')

      endfor

      PRINT, 'Press a key'
      move = GET_KBRD(1)

  endfor

end
