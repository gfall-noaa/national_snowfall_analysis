; Establish probabilistic parameters for predicting that daily QPE
; (i.e., Stage IV) is snow based on 2 m air temperatures.

; The idea here is to establish criteria based on minimum, maximum,
; and average daily temperatures, and to establish these for different
; times of year.

; Currently, the idea is that if the average daily temperature is
; below some threshold AND the minimum daily temperature is below some
; threshold (0 deg C most likely), then any QPE that occurs is
; probably snow.

; Greg Fall, OWP-Chanhassen, and
; Kent Sparrow, NWC

; July 2016


PRO USR_MSG, text

; Send an informational message to stdout or, if there is no stdout, to a
; message buffer.

  COMMON info, message
  stdOutStat = FSTAT(-1)
  if stdOutStat.isatty then $
    PRINT, text $
  else $
    STRING_APPEND, message, text, /WITH_CR
  RETURN
end


PRO ERR_MSG, text

; Send an error message to stderr or, if there is no stdout, to a message
; buffer.

  COMMON info, message
  stdErrStat = FSTAT(-2)
  if stdErrStat.isatty then $
    MESSAGE, text, LEVEL = -1, /CONTINUE $
  else $
    STRING_APPEND, message, text, /WITH_CR
  RETURN
end



PRO SNFL_AIRTEMP_STATS_BATCH

; IDL runtime version of SNFL_AIRTEMP_STATS.

; Generates histograms of snowfall reports as a function of
; min/max/ave temperature, using SNODAS analysis temperature data
; nearest-neighbor sampled at the locations of snowfall reporting
; stations.
  
; The main idea is to establish an average temperature below which one
; can assume daily QPE (e.g., Stage IV precip.) is probably snow.

; MAIN PROGRAM


  COMMON info, message
  dsStatus = 0
  message = ''


; Define date range.

  startYear = GETENV('START_YEAR')
  if (N_ELEMENTS(startYear) ne 1) then $
      MESSAGE, 'no start year'
  if (STRLEN(startYear) ne 4) then $
      MESSAGE, 'bad start year "' + startYear + '"'
  startYear = FIX(startYear)

  finishYear = GETENV('FINISH_YEAR')
  if (N_ELEMENTS(finishYear) ne 1) then $
      MESSAGE, 'no finish year'
  if (STRLEN(finishYear) ne 4) then $
      MESSAGE, 'bad finish year'
  finishYear = FIX(finishYear)

  ;; startYear = 2015
  ;; finishYear = 2015

; 09/21 to 10/11 is 21 days centered on 10/01

  startDate_MMDD = GETENV('START_DATE_MMDD')
  if (N_ELEMENTS(startDate_MMDD) ne 1) then $
      MESSAGE, 'no start mmdd'
  if (STRLEN(startDate_MMDD) ne 4) then $
      MESSAGE, 'bad start mmdd'

  finishDate_MMDD = GETENV('FINISH_DATE_MMDD')
  if (N_ELEMENTS(finishDate_MMDD) ne 1) then $
      MESSAGE, 'no finish mmdd'
  if (STRLEN(finishDate_MMDD) ne 4) then $
      MESSAGE, 'bad finish mmdd'

;  startDate_MMDD = '0921'
;  finishDate_MMDD = '1011'

; 10-22 to 11-11 is 21 days centered on 11-01
  ;; startDate_MMDD = '1022'
  ;; finishDate_MMDD = '1111'

   anlStartDate_YYYYMMDDHH = '2006040112'
   anlFinishDate_YYYYMMDDHH = '2015043012'
 
   ;; anlStartDate_YYYYMMDDHH = '2016040112'
   ;; anlFinishDate_YYYYMMDDHH = '2016043012'

  ;; anlStartDate_YYYYMMDDHH = '2015110112'
  ;; anlFinishDate_YYYYMMDDHH = '2015113012'

  ;; anlStartDate_YYYYMMDDHH = '2015111512'
  ;; anlFinishDate_YYYYMMDDHH = '2015121512'

  ;; anlStartDate_YYYYMMDDHH = '2015120112'
  ;; anlFinishDate_YYYYMMDDHH = '2015123112'

  ;; anlStartDate_YYYYMMDDHH = '2015121512'
  ;; anlFinishDate_YYYYMMDDHH = '2016011512'

   ;; anlStartDate_YYYYMMDDHH = '2016011512'
   ;; anlFinishDate_YYYYMMDDHH = '2016021512'

  ;; anlStartDate_YYYYMMDDHH = '2016021512'
  ;; anlFinishDate_YYYYMMDDHH = '2016031512'

  ;; anlStartDate_YYYYMMDDHH = '2016030112'
  ;; anlFinishDate_YYYYMMDDHH = '2016033112'

  ;; anlStartDate_YYYYMMDDHH = '2016040112'
  ;; anlFinishDate_YYYYMMDDHH = '2016043012'

  ;; anlStartDate_Julian = YYYYMMDDHH_TO_JULIAN(anlStartDate_YYYYMMDDHH)
  ;; anlFinishDate_Julian = YYYYMMDDHH_TO_JULIAN(anlFinishDate_YYYYMMDDHH)


; Determine whether climatology period crosses January 1. Start times
; and finish times are in different calendar years in that case.

  startMonth = FIX(STRMID(startDate_MMDD, 0, 2))
  finishMonth = FIX(STRMID(finishDate_MMDD, 0, 2))

  crossNewYear = 0B
  if (finishMonth lt startMonth) then begin
      crossNewYear = 1B
      finishYear = finishYear + 1
      PRINT, 'This analysis period includes a date range covering '  + $
             'different calendar years.'
  endif

  useObsTemp = 1
  snflThreshold = 0.1 ; inches

  if (useObsTemp) then $
      savFile = 'snfl_tmp_obs_' + $
                startDate_MMDD + '_to_' + $
                finishDate_MMDD + '_' + $
                STRING(startYear, FORMAT = '(i4.4)') + '_to_' + $
                STRING(finishYear, FORMAT = '(i4.4)') + '.sav' $
  else $
      savFile = 'snfl_obs_hrrr_tmp_' + $
                anlStartDate_YYYYMMDDHH + '_to_' + $
                anlFinishDate_YYYYMMDDHH + '.sav'

  fullRun = 1
  if FILE_TEST(savFile) then begin
      fullRun = 0
      GOTO, SKIP
  endif

  snowfallHomeDir = '/nwcdev/nsadev/snowfall_v2_devel/type2_dev'

  windowHoursBack = 3
  windowHoursForward = 3


; Define spatial domain and resolution.

  minLatOut = 21.0D
  maxLatOut = 55.0D
  minLonOut = -126.0D
  maxLonOut = -66.0D

  xResOut = 0.04D
  yResOut = 0.04D

  nxOut = ROUND((maxLonOut - minLonOut) / xResOut)
  nyOut = ROUND((maxLatOut - minLatOut) / yResOut)

  minLatOutStr = STRCOMPRESS(STRING(minLatOut, $
                                    FORMAT='(F25.17)'), /REMOVE_ALL)
  maxLatOutStr = STRCOMPRESS(STRING(maxLatOut, $
                                    FORMAT='(F25.17)'), /REMOVE_ALL)
  minLonOutStr = STRCOMPRESS(STRING(minLonOut, $
                                    FORMAT='(F25.17)'), /REMOVE_ALL)
  maxLonOutStr = STRCOMPRESS(STRING(maxLonOut, $
                                    FORMAT='(F25.17)'), /REMOVE_ALL)
  ndv = -99999.0

  date_Julian = SYSTIME(/JULIAN, /UTC)

;  numDays = ROUND(anlFinishDate_Julian - anlStartDate_Julian) + 1


; Clear out aggregate arrays of snowfall data.

  xAll = !NULL
  yAll = !NULL
  idAll = !NULL
  zAll = !NULL
  fmAll = !NULL
  fAll = !NULL
  ntAll = !NULL
  xtAll = !NULL
  atAll = !NULL
  numAllSnowfall = 0


; Loop over years of analysis period.

  for year = startYear, finishYear do begin

      YYYY = STRING(year, FORMAT = '(I4.4)')


;     Identify days of the analysis period to cover for this year.

      if (crossNewYear) then begin

          Jan01_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + '010112')
          Dec31_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + '123112')

          case year of

              startYear: begin
                  t1_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + startDate_MMDD + '12')
                  t2_Julian = Dec31_Julian
                  date_Julian = t1_Julian + $
                                DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D))
              end

              finishYear: begin
                  t1_Julian = Jan01_Julian
                  t2_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + finishDate_MMDD + '12')
                  date_Julian = t1_Julian + $
                                DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D))
              end

              else: begin
                  t1_Julian = Jan01_Julian
                  t2_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + finishDate_MMDD + '12')
                  t3_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + startDate_MMDD + '12')
                  t4_Julian = Dec31_Julian
                  date_Julian = $
                      [t1_Julian + $
                       DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D)), $
                       t3_Julian + $
                       DINDGEN(ROUND(t4_Julian - t3_Julian + 1.0D))]
              end

          endcase

      endif else begin

          t1_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + startDate_MMDD + '12')
          t2_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + finishDate_MMDD + '12')
          date_Julian = t1_Julian + $
                        DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D))

      endelse

      numDays = N_ELEMENTS(date_Julian)


;     Loop over days of analysis period; collect snowfall and
;     temperature data for each day.

      for dc = 0, numDays - 1 do begin

          finishDate_Julian = date_Julian[dc]
          finishDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(finishDate_Julian)

;          print, '---> ' + finishDate_YYYYMMDDHH
;          continue


;         Establish time window for observations for the current day.

          origFinishDate_YYYYMMDDHH = finishDate_YYYYMMDDHH
          origFinishDate_Julian = $
              YYYYMMDDHH_TO_JULIAN(origFinishDate_YYYYMMDDHH)
          origFinishDate_GISRS = JULIAN_TO_GISRS_DATE(origFinishDate_Julian)

          finishDate_Julian = YYYYMMDDHH_TO_JULIAN(finishDate_YYYYMMDDHH) + $
                              DOUBLE(windowHoursForward) / 24.0D
          finishDate_GISRS = JULIAN_TO_GISRS_DATE(finishDate_Julian)

          startDate_Julian = $
              finishDate_Julian - $
              DOUBLE(windowHoursBack + windowHoursForward) / 24.0D
          startDate_GISRS = JULIAN_TO_GISRS_DATE(startDate_Julian)
          ;; print, 'getting snowfall obs from ' + startdate_gisrs + $
          ;;        ' to ' + finishdate_gisrs


;         Get snowfall_raw observations.
;
;     *** Make sure PGHOSTADDR environment variable is not set. ***

          webPGHost = 'wdb0.dmz.nohrsc.noaa.gov'

          snflThresholdMStr = STRCRA(DOUBLE(snflThreshold) * 0.0254D)

          statement = 'psql -d web_data -h ' + webPGHost + ' -t -A -c ' + $
                      '"' + $
                      'select t1.obj_identifier, ' + $
                      'trim(t1.station_id), ' + $
                      'trim(t1.name), ' + $
                      'trim(t1.station_type), ' + $
                      't1.coordinates[0], ' + $
                      't1.coordinates[1], ' + $
                      't1.elevation, ' + $
                      't2.date, ' + $
                      't2.value ' + $
                      'from point.allstation as t1, ' + $
                      'point.obs_snowfall_raw as t2 ' + $
                      'where ' + $
                      't1.coordinates[0] >= ' + minLonOutStr + ' ' + $
                      'and t1.coordinates[0] <= ' + maxLonOutStr + ' ' + $
                      'and t1.coordinates[1] >= ' + minLatOutStr + ' ' + $
                      'and t1.coordinates[1] <= ' + maxLatOutStr + ' ' + $ 
                      'and t2.date >= ''' + $
                      startDate_GISRS + ':00:00'' ' + $
                      'and t2.date <= ''' + $
                      finishDate_GISRS + ':00:00'' ' + $
                      'and t2.duration = 86400 ' + $
                      'and t2.value is not NULL ' + $
                      'and t2.value >= 0.0 ' + $
                      ;; 'and t2.value >= ' + snflThresholdMStr + ' ' + $
                                ;         'and t2.value >= 0.0 ' + $
                                ;        'and t2.value < 99.5 ' + $
                      'and t1.obj_identifier = t2.obj_identifier ' + $
                      'order by t2.date, t1.station_id;"'

;          USR_MSG, statement
          SPAWN, statement, result, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'psql statement failed: ' + statement
              GOTO, BAIL
          endif
          numSnowfall = N_ELEMENTS(result)
          if (result[0] eq '') then numSnowfall = 0

          ;; USR_MSG, 'snowfall_raw provided ' + $
          ;;          STRCOMPRESS(numSnowfall, /REMOVE_ALL) + $
          ;;          ' reports for ' + $
          ;;          origFinishDate_YYYYMMDDHH

          if (numSnowfall gt 0) then begin


;             Place results in a structure.

              snowfallReport = REPLICATE({object_id: 0L, $
                                          station_id: '', $
                                          station_name: '', $
                                          station_type: '', $
                                          longitude: 0.0D, $
                                          latitude: 0.0D, $
                                          elevation: 0L, $
                                          date_UTC: 0.0D, $
                                          value_meters: 0.0, $
                                          qc: 0L}, $
                                         numSnowfall)

              for sc = 0, numSnowfall - 1 do begin
                  report = STRSPLIT(result[sc], '|', /EXTRACT)
                  if (N_ELEMENTS(report) ne 9) then begin
                      ERR_MSG, 'Unrecognized structure in snowfall_raw reports.'
                      GOTO, BAIL
                  endif
                  snowfallReport[sc].object_id = LONG(report[0])
                  snowfallReport[sc].station_id = report[1]
                  station_name = report[2]
                  snowfallReport[sc].station_name = station_name
                  snowfallReport[sc].station_type = report[3]
                  snowfallReport[sc].longitude = DOUBLE(report[4])
                  snowfallReport[sc].latitude = DOUBLE(report[5])
                  snowfallReport[sc].elevation = LONG(report[6])
                  snowfallReport[sc].date_UTC = $
                      JULDAY(FIX(STRMID(report[7], 5, 2)), $
                             FIX(STRMID(report[7], 8, 2)), $
                             FIX(STRMID(report[7], 0, 4)), $
                             ROUND(DOUBLE(STRMID(report[7], 11, 2)) + $
                                   DOUBLE(STRMID(report[7], 14, 2)) / 60.0D + $
                                   DOUBLE(STRMID(report[7], 17, 2)) / 1440.0D))
                  snowfallReport[sc].value_meters = FLOAT(report[8])
              endfor


;             If any stations delivered multiple reports, choose the
;             one closest to the analysis time.

              useFlag = BYTARR(numSnowfall) & useFlag[*] = 1B

              for sc = 0, numSnowfall - 1 do begin

                  ind = WHERE((snowfallReport.station_id eq $
                               snowfallReport[sc].station_id) and $
                              (useFlag eq 1B), count)
                  if (count eq 0) then begin
                      ERR_MSG, 'Programming error in unique station search.'
                      GOTO, BAIL
                  endif
                  if (count eq 1) then CONTINUE
                  useFlag[ind] = 0B ; set all matches to DO NOT USE
                  dateList = snowfallReport[ind].date_UTC
                  timeDiff = MIN(ABS(origFinishDate_Julian - dateList), minInd)
                  useFlag[ind[minInd]] = 1B
              endfor

              ind = WHERE(useFlag eq 1B, numSnowfall)
              if (numSnowfall gt 0) then $
                  snowfallReport = snowfallReport[ind] $
              else $
                  snowfallReport = -1L

              ;; USR_MSG, 'After removing redundant reports, ' +$
              ;;          STRCOMPRESS(numSnowfall, /REMOVE_ALL) + $
              ;;          ' remain.'

          endif

          if (numSnowfall eq 0) then CONTINUE


;         Put locations and values in 1-D arrays to simplify code.

          objid = snowfallReport.object_id
          id = snowfallReport.station_id
          x = snowfallReport.longitude
          y = snowfallReport.latitude
          z = snowfallReport.elevation
          ft = snowfallReport.date_UTC
          fm = snowfallReport.value_meters
          f = snowfallReport.value_meters * 39.3700787 ; meters to inches

          if NOT(useObsTemp) then begin

;             Get min/max/ave HRRR temperatures.

              fcstHour = 0
              HRRRDir = '/nwcdev/archive/HRRR_archive'
              scratchDir = '/disks/scratch'

              for hc = 0, 23 do begin

                  cycleDate_Julian = origFinishDate_Julian - $
                                     23.0D / 24.0D + $
                                     DOUBLE(hc) / 24.0D - $
                                     DOUBLE(fcstHour) / 24.0D
                  cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
                  cycleDate_YYYY = STRMID(cycleDate_YYYYMMDDHH, 0, 4)
                  cycleDate_MM = STRMID(cycleDate_YYYYMMDDHH, 4, 2)
                  cycleDate_DD = STRMID(cycleDate_YYYYMMDDHH, 6, 2)
                  GRIBDir = HRRRDir + $
                            '/' + cycleDate_YYYY + $
                            '/' + cycleDate_MM + $
                            '/' + cycleDate_DD
                  GRIBFile = 'hrrr.' + $
                             STRMID(cycleDate_YYYYMMDDHH, 0, 8) + $
                             '.t' + STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                             'z.wrfsfcf' + $
                             STRING(fcstHour, FORMAT = '(I2.2)') + $
                             '.grib2'

                  if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile)) then begin

;                     Try farther-out forecasts.

                      altFcstHour = fcstHour + 1
                      while (altFcstHour le 8) do begin
                          cycleDate_Julian = origFinishDate_Julian - $
                                             23.0D / 24.0D + $
                                             DOUBLE(hc) / 24.0D - $
                                             DOUBLE(altFcstHour) / 24.0D
                          cycleDate_YYYYMMDDHH = $
                              JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
                          cycleDate_YYYY = STRMID(cycleDate_YYYYMMDDHH, 0, 4)
                          cycleDate_MM = STRMID(cycleDate_YYYYMMDDHH, 4, 2)
                          cycleDate_DD = STRMID(cycleDate_YYYYMMDDHH, 6, 2)
                          GRIBDir = HRRRDir + $
                                    '/' + cycleDate_YYYY + $
                                    '/' + cycleDate_MM + $
                                    '/' + cycleDate_DD
                          GRIBFile = 'hrrr.' + $
                                     STRMID(cycleDate_YYYYMMDDHH, 0, 8) + $
                                     '.t' + $
                                     STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                                     'z.wrfsfcf' + $
                                     STRING(altFcstHour, FORMAT = '(I2.2)') + $
                                     '.grib2'
                          if FILE_TEST(GRIBDir + '/' + GRIBFile) then BREAK
                          altFcstHour = altFcstHour + 1
                      endwhile
                      if (altFcstHour le 8) then $
                          PRINT, 'WARNING: using ' + $
                                 STRCOMPRESS(altFcstHour, /REMOVE_ALL) + $
                                 '-hour forecast for hour ' + $
                                 STRCOMPRESS(hc + 1, /REMOVE_ALL)
                  endif

                  if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile)) then begin

;                     Try less-far-out forecasts.

                      altFcstHour = fcstHour - 1
                      while (altFcstHour ge 1) do begin
                          cycleDate_Julian = origFinishDate_Julian - $
                                             23.0D / 24.0D + $
                                             DOUBLE(hc) / 24.0D - $
                                             DOUBLE(altFcstHour) / 24.0D
                          cycleDate_YYYYMMDDHH = $
                              JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
                          cycleDate_YYYY = STRMID(cycleDate_YYYYMMDDHH, 0, 4)
                          cycleDate_MM = STRMID(cycleDate_YYYYMMDDHH, 4, 2)
                          cycleDate_DD = STRMID(cycleDate_YYYYMMDDHH, 6, 2)
                          GRIBDir = HRRRDir + $
                                    '/' + cycleDate_YYYY + $
                                    '/' + cycleDate_MM + $
                                    '/' + cycleDate_DD
                          GRIBFile = 'hrrr.' + $
                                     STRMID(cycleDate_YYYYMMDDHH, 0, 8) + $
                                     '.t' + $
                                     STRMID(cycleDate_YYYYMMDDHH, 8, 2) + $
                                     'z.wrfsfcf' + $
                                     STRING(altFcstHour, FORMAT = '(I2.2)') + $
                                     '.grib2'
                          if FILE_TEST(GRIBDir + '/' + GRIBFile) then BREAK
                          altFcstHour = altFcstHour - 1
                      endwhile
                      if (altFcstHour ge 1) then $
                          PRINT, 'WARNING: using ' + $
                                 STRCOMPRESS(altFcstHour, /REMOVE_ALL) + $
                                 '-hour forecast for hour ' + $
                                 STRCOMPRESS(hc + 1, /REMOVE_ALL)
                  endif

                  if NOT(FILE_TEST(GRIBDir + '/' + GRIBFile)) then STOP

                  PRINT, GRIBFile

                  DECODE_GRIB2_FIELD, GRIBFile, GRIBDir, scratchDir, $
                                      ':TMP:', $
                                      record, vt, lev, ftime, $
                                      varAbbrev, varField, varUnits, $
                                      nx_, ny_, $
                                      hourlyTmpProj, $
                                      status, $
                                      NO_DATA_VALUE = ndv
                  if NOT(status) then begin
                      ERR_MSG, 'Failed to decode TMP from ' + $
                               GRIBDir + '/' + GRIBFile
                      GOTO, BAIL
                  endif

                  if (varAbbrev ne 'TMP') then STOP
                  if (varField ne 'Temperature') $
                  then STOP
                  if (varUnits ne 'K') then STOP

                  if (hc eq 0) then begin

                      nx = nx_
                      ny = ny_

                      GET_GRIB2_LAMBERT_CONFORMAL_GRID_INFO, $
                          GRIBFile, GRIBDir, scratchDir, $
                          ':TMP:', $
                          nx_, ny_, $
                          lat1, lon1, lonv, latd, $
                          latin1, latin2, latsp, lonsp, $
                          dx, dy, $
                          STATUS
                      if NOT(status) then STOP

                      if (nx_ ne nx) then STOP
                      if (ny_ ne ny) then STOP

                      minTmpProj = hourlyTmpProj
                      maxTmpProj = hourlyTmpProj
                      aveTmpProj = hourlyTmpProj

                  endif else begin

                      if (nx_ ne nx) then STOP
                      if (ny_ ne ny) then STOP

                      ind = WHERE((aveTmpProj eq ndv) or $
                                  (hourlyTmpProj eq ndv), count)
                      minTmpProj = minTmpProj < hourlyTmpProj
                      maxTmpProj = maxTmpProj > hourlyTmpProj
                      aveTmpProj = aveTmpProj + hourlyTmpProj
                      if (count gt 0) then begin
                          minTmpProj[ind] = ndv
                          maxTmpProj[ind] = ndv
                          aveTmpProj[ind] = ndv
                      endif

                  endelse

              endfor

              hourlyTmpProj = -1L

              ind = WHERE(aveTmpProj eq ndv, count)
              aveTmpProj = aveTmpProj / 24
              if (count gt 0) then aveTmpProj[ind] = ndv

              DEPROJECT_LAMBERT_CONFORMAL, minTmpProj, $
                                           lat1, lon1, lonv, latd, $
                                           latin1, latin2, dx, dy, $
                                           6371229.0D, $
                                           ndv, $
                                           minLonOut, maxLonOut, $
                                           minLatOut, maxLatOut, $
                                           xResOut, yResOut, $
                                           minTmpGrid, $
                                           status, $
                                           /HASH, $
                                           /FLOAT_ROW_COLUMN

              DEPROJECT_LAMBERT_CONFORMAL, maxTmpProj, $
                                           lat1, lon1, lonv, latd, $
                                           latin1, latin2, dx, dy, $
                                           6371229.0D, $
                                           ndv, $
                                           minLonOut, maxLonOut, $
                                           minLatOut, maxLatOut, $
                                           xResOut, yResOut, $
                                           maxTmpGrid, $
                                           status, $
                                           /HASH, $
                                           /FLOAT_ROW_COLUMN

              DEPROJECT_LAMBERT_CONFORMAL, aveTmpProj, $
                                           lat1, lon1, lonv, latd, $
                                           latin1, latin2, dx, dy, $
                                           6371229.0D, $
                                           ndv, $
                                           minLonOut, maxLonOut, $
                                           minLatOut, maxLatOut, $
                                           xResOut, yResOut, $
                                           aveTmpGrid, $
                                           status, $
                                           /HASH, $
                                           /FLOAT_ROW_COLUMN


;             Sample grid data at observation points.

              nt = FLTARR(numSnowfall) ; min temp
              xt = FLTARR(numSnowfall) ; max temp
              at = FLTARR(numSnowfall) ; ave temp

              i = (x - minLonOut) / xResOut - 0.5D
              j = (y - minLatOut) / yResOut - 0.5D

              for k = 0, numSnowfall - 1 do begin
                  ik = i[k]
                  jk = j[k]
                  i1 = FLOOR(ik)
                  i2 = i1 + 1L
                  j1 = FLOOR(jk)
                  j2 = j1 + 1L
                  if ((i1 lt 0L) or $
                      (i2 ge nxOut) or $
                      (j1 lt 0L) or $
                      (j2 ge nyOut)) then STOP ; should have already avoided
                                                ; by psql

                  ntll = minTmpGrid[i1, j1]
                  ntlr = minTmpGrid[i2, j1]
                  ntur = minTmpGrid[i2, j2]
                  ntul = minTmpGrid[i1, j2]
                  if ((ntll eq ndv) or $
                      (ntlr eq ndv) or $
                      (ntur eq ndv) or $
                      (ntul eq ndv)) then $
                          nt[k] = ndv $
                  else $
                      nt[k] = ntll * (i2 - ik) * (j2 - jk) + $
                              ntlr * (ik - i1) * (j2 - jk) + $
                              ntur * (ik - i1) * (jk - j1) + $
                              ntul * (i2 - ik) * (jk - j1)

                  xtll = maxTmpGrid[i1, j1]
                  xtlr = maxTmpGrid[i2, j1]
                  xtur = maxTmpGrid[i2, j2]
                  xtul = maxTmpGrid[i1, j2]
                  if ((xtll eq ndv) or $
                      (xtlr eq ndv) or $
                      (xtur eq ndv) or $
                      (xtul eq ndv)) then $
                          xt[k] = ndv $
                  else $
                      xt[k] = xtll * (i2 - ik) * (j2 - jk) + $
                              xtlr * (ik - i1) * (j2 - jk) + $
                              xtur * (ik - i1) * (jk - j1) + $
                              xtul * (i2 - ik) * (jk - j1)

                  atll = aveTmpGrid[i1, j1]
                  atlr = aveTmpGrid[i2, j1]
                  atur = aveTmpGrid[i2, j2]
                  atul = aveTmpGrid[i1, j2]
                  if ((atll eq ndv) or $
                      (atlr eq ndv) or $
                      (atur eq ndv) or $
                      (atul eq ndv)) then $
                          at[k] = ndv $
                  else $
                      at[k] = atll * (i2 - ik) * (j2 - jk) + $
                              atlr * (ik - i1) * (j2 - jk) + $
                              atur * (ik - i1) * (jk - j1) + $
                              atul * (i2 - ik) * (jk - j1)

              endfor

              ind = WHERE((nt ne ndv) and $
                          (xt ne ndv) and $
                          (at ne ndv), count)
              if (count eq 0) then STOP

              objid = objid[ind]
              id = id[ind]
              x = x[ind]
              y = y[ind]
              z = z[ind]
              ft = ft[ind]
              fm = fm[ind]
              f = f[ind]
              nt = nt[ind]
              xt = xt[ind]
              at = at[ind]

              PRINT, 'After eliminating HRRR temp ndv we have ' + $
                     STRCRA(count) + ' reports'

          endif else begin


;             Collect temperature data at site locations.

              numDailyObsHrsEachDay = windowHoursBack + windowHoursForward + 1
              numHourlyObsHrsEachDay = numDailyObsHrsEachDay + 23

              startDate_GISRS = $
                  JULIAN_TO_GISRS_DATE(startDate_Julian - 23.0D / 24.0D)
              finishDate_GISRS = $
                  JULIAN_TO_GISRS_DATE(finishDate_Julian)
              ;; print, 'getting temperature obs from ' + startdate_gisrs + $
              ;;        ' to ' + finishdate_gisrs

;             Target stations a few at a time.

              numAirTemp = 0L
              groupSize = 100
              result = !NULL

              for sgc = 0, CEIL(FLOAT(numSnowfall) / FLOAT(groupSize)) - 1 $
              do begin
                  sc1 = sgc * groupSize
                  sc2 = (sc1 + groupSize - 1) < (numSnowfall - 1)

                  if (sc1 ge numSnowfall) then STOP

                  stationPart = 't1.obj_identifier in ('

                  for sc = sc1, sc2 do begin
                      stationPart = stationPart + $
                                    STRCRA(snowfallReport[sc].object_id)
                      if (sc lt sc2) then stationPart = stationPart + ', '
                  endfor
                  stationPart = stationPart + ')'

                  ; point.sm_airtemp_surface is a view:
                  ;   create view
                  ;   sm_airtemp_surface as
                  ;   select obj_identifier, date,
                  ;   value_sm_airtemp_surface as value
                  ;   from rasters

                  statement = 'psql -d web_data -h ' + webPGHost + $
                              ' -t -A -c ' + $
                              '"' + $
                              'select t1.obj_identifier, ' + $
                              'trim(t1.station_id), ' + $
                              't1.coordinates[0], ' + $
                              't1.coordinates[1], ' + $
                              't1.elevation, ' + $
                              't2.date, ' + $
                              't2.value ' + $
                              'from point.allstation as t1, ' + $
                              'point.sm_airtemp_surface as t2 ' + $
                              'where ' + $
                              't2.date >= ''' + $
                              startDate_GISRS + ':00:00'' ' + $
                              'and t2.date <= ''' + $
                              finishDate_GISRS + ':00:00'' ' + $
                              'and t2.value >= 183.15 ' + $ ; -90 deg C
                              'and t2.value <= 333.15 ' + $ ; 60 deg C
                              'and t2.value is not NULL ' + $
                              'and ' + stationPart + ' ' +$
                              'and t1.obj_identifier = t2.obj_identifier ' + $
                              'order by t1.station_id, t2.date;"'

;                  USR_MSG, statement

                  SPAWN, statement, groupResult, EXIT_STATUS = status
                  if (status ne 0) then begin
                      ERR_MSG, 'psql statement failed: ' + statement
                      GOTO, BAIL
                  endif
                  if (groupResult[0] eq '') then CONTINUE
                  if NOT(ISA(result)) then $
                      result = groupResult $
                  else $
                      result = [result, groupResult]

              endfor

              if NOT(ISA(result)) then begin
                  numAirTemp = 0
              endif else begin
                  numAirTemp = N_ELEMENTS(result)
              endelse

              if (numAirTemp eq 0) then begin
                  PRINT, 'No air temp data for ' + STRCRA(numSnowfall) + $
                         ' snowfall reports'
                  CONTINUE
              endif else begin
                  PRINT, 'Found ' + STRCRA(numSnowfall) + ' reports.'
              endelse

              mdlAirTempHourly = MAKE_ARRAY(numHourlyObsHrsEachDay, $
                                            numSnowfall, $
                                            /FLOAT, VALUE = ndv)
              prevObjectID = -1L

              minhc = 1000
              maxhc = 0
              numAirTempReportsUsed = 0L

              for rc = 0, numAirTemp - 1 do begin

                  report = STRSPLIT(result[rc], '|', /EXTRACT)
                  if (N_ELEMENTS(report) ne 7) then begin
                      ERR_MSG, 'Unrecognized structure in obs_airtemp reports.'
                      GOTO, BAIL
                  endif

                  objectID = LONG(report[0])

                  if (objectID ne prevObjectID) then begin

;                     Verify station metadata, just one time, for this
;                     object ID.

                      sc = WHERE(snowfallReport.object_id eq objectID, count)
                      if (count eq 0) then CONTINUE
                      if (count ne 1) then STOP ; PROGRAMMING ERROR
                      sc = sc[0]
                      stationID = report[1]
                      if (stationID ne snowfallReport[sc].station_id) then STOP

                      longitude = DOUBLE(report[2])
                      if (longitude ne snowfallReport[sc].longitude) then STOP
                      latitude = DOUBLE(report[3])
                      if (latitude ne snowfallReport[sc].latitude) then STOP
                      elevation = LONG(report[4])
                      if (elevation ne snowfallReport[sc].elevation) then STOP
                      prevObjectID = objectID

                  endif

                  date = report[5]
                  hour = ROUND(DOUBLE(STRMID(date, 11, 2)) + $
                               DOUBLE(STRMID(date, 14, 2)) / 60.0D + $
                               DOUBLE(STRMID(date, 17, 2)) / 1440.0D)
                  dateJul = JULDAY(FIX(STRMID(date, 5, 2)), $
                                   FIX(STRMID(date, 8, 2)), $
                                   FIX(STRMID(date, 0, 4)), $
                                   hour)
                  hc = ROUND(24.0D * (dateJul - $
                                      (startDate_Julian - 23.0D / 24.0D)))

                  if (hc lt 0) then STOP
                  if (hc ge numHourlyObsHrsEachDay) then STOP

                  if (hc lt minhc) then minhc = hc
                  if (hc gt maxhc) then maxhc = hc

                  mdlAirTempHourly[hc, sc] = FLOAT(report[6])
                  numAirTempReportsUsed++

              endfor

              obsAirTempHourly = mdlAirTempHourly
      

;             Get min/max/ave air temperatures where we can.

              nt = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv)
              xt = nt
              at = nt

              numObsBased = 0L
              maxCount = 0

              hhist = LONARR(numhourlyobshrseachday + 1)

              for sc = 0, numSnowfall - 1 do begin

                  stationObsAirTempHourly = obsAirTempHourly[*, sc]
                  h1 = ROUND(24 * (snowfallReport[sc].date_UTC - $
                                   23.0D / 24.0D - $
                                   (startDate_Julian - 23.0D / 24.0D)))
                  h2 = h1 + 23
                  if (h1 lt 0) then STOP
                  if (h2 ge numHourlyObsHrsEachDay) then STOP
                  ;; help, stationobsairtemphourly
                  ;; print, julian_to_yyyymmddhh(snowfallreport[sc].date_utc)
                  ;; print, h1, h2
                  stationObsAirTempHourly = stationObsAirTempHourly[h1:h2]
                  ind = WHERE(stationObsAirTempHourly ne ndv, count)
                  if (count gt 20) then begin
                      ;; print, stationobsairtemphourly
                      nt[sc] = MIN(stationObsAirTempHourly[ind])
                      xt[sc] = MAX(stationObsAirTempHourly[ind])
                      at[sc] = MEAN(stationObsAirTempHourly[ind])
                      ;; print, count, nt[sc], xt[sc], at[sc]
                      ;; move = get_kbrd(1)
                      numObsBased++
                  endif
                  hhist[count] = hhist[count] + 1
                  if (count gt maxCount) then maxCount = count
              endfor

              ;; USR_MSG, 'Were able to calculate observation-based ' + $
              ;;          'min/max/ave temperatures for ' + $
              ;;          STRCRA(numObsBased) + $
              ;;          ' out of ' + STRCRA(numSnowfall) + $
              ;;          ' snowfall reports.'

              ind = WHERE((nt ne ndv) and $
                          (xt ne ndv) and $
                          (at ne ndv), count)

              if (count eq 0) then CONTINUE

              objid = objid[ind]
              id = id[ind]
              x = x[ind]
              y = y[ind]
              z = z[ind]
              ft = ft[ind]
              fm = fm[ind]
              f = f[ind]
              nt = nt[ind] - 273.15
              xt = xt[ind] - 273.15
              at = at[ind] - 273.15

              ;; PRINT, 'After eliminating SNODAS temp ndv we have ' + $
              ;;        STRCRA(count) + ' reports'

          endelse

          if (numAllSnowfall eq 0) then begin

              xAll = x
              yAll = y
              idAll = id
              zAll = z
              fmAll = fm
              fAll = f
              ntAll = nt
              xtAll = xt
              atAll = at
              numAllSnowfall = count

          endif else begin

              xAll = [xAll, x]
              yAll = [yAll, y]
              idAll = [idAll, id]
              zAll = [zAll, z]
              fmAll = [fmAll, fm]
              fAll = [fAll, f]
              ntAll = [ntAll, nt]
              xtAll = [xtAll, xt]
              atAll = [atAll, at]
              numAllSnowfall = numAllSnowfall + count

          endelse

      endfor

  endfor

  x = xAll & xAll = !NULL
  y = yAll & yAll = !NULL
  id = idAll & idAll = !NULL
  z = zAll & zAll = !NULL
  fm = fmAll & fmAll = !NULL
  f = fAll & fAll = !NULL
  nt = ntAll & ntAll = !NULL
  xt = xtAll & xtAll = !NULL
  at = atAll & atAll = !NULL


  SAVE, /ALL, FILE = savFile

SKIP:

  if NOT(fullRun) then RESTORE, savFile



; Look at temperature distributions for observed snow.

  atCutoff = 0.95 ; cumulative histogram cutoff for average temperature
  xtCutoff = 0.95 ; cumulative histogram cutoff for maximum temperature
  ntCutoff = 0.99 ; cumulative histogram cutoff for minimum temperature

  snflEventInd = WHERE(f ge snflThreshold, snflEventCount)
  if (snflEventCount eq 0) then begin
      atMax1 = ndv
      xtMax1 = ndv
      ntMax1 = ndv
      hitRate1 = ndv
      missRate1 = ndv
  endif else begin

      atc = at[snflEventInd]
      xtc = xt[snflEventInd]
      ntc = nt[snflEventInd]

      hMin = -30.0D
      hMax = 30.0D
      binSize = 0.5D
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * (binSize * numBins - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad

      hat = HISTOGRAM(atc, MIN = hMin, MAX = hMax, BINSIZE = binSize)
      hxt = HISTOGRAM(xtc, MIN = hMin, MAX = hMax, BINSIZE = binSize)
      hnt = HISTOGRAM(ntc, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      hx = hMin + (0.5 + FINDGEN(numBins)) * binSize
      hxAcc = hMin + FINDGEN(numBins + 1) * binSize

      hatAcc = FLTARR(numBins + 1)
      hatAcc[0] = 0.0
      for bc = 1, numBins do $
          hatAcc[bc] = hatAcc[bc - 1] + hat[bc - 1] / TOTAL(hat)

      i1 = MAX(WHERE(hatAcc lt atCutoff))
      atMax1 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hatAcc[i1+1] - hatAcc[i1]) * $
               (atCutoff - hatAcc[i1])

      hxtAcc = FLTARR(numBins + 1)
      hxtAcc[0] = 0.0
      for bc = 1, numBins do $
          hxtAcc[bc] = hxtAcc[bc - 1] + hxt[bc - 1] / TOTAL(hxt)

      i1 = MAX(WHERE(hxtAcc lt xtCutoff))
      xtMax1 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hxtAcc[i1+1] - hxtAcc[i1]) * $
               (xtCutoff - hxtAcc[i1])

      hntAcc = FLTARR(numBins + 1)
      hntAcc[0] = 0.0
      for bc = 1, numBins do $
          hntAcc[bc] = hntAcc[bc - 1] + hnt[bc - 1] / TOTAL(hnt)

      i1 = MAX(WHERE(hntAcc lt ntCutoff))
      ntMax1 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hntAcc[i1+1] - hntAcc[i1]) * $
               (ntCutoff - hntAcc[i1])

  ;; WSET_OR_WINDOW, 0, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hat[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Average Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hatAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Average temperature ' + STRCRA(atCutoff * 100) + '%: ', atMax

  ;; WSET_OR_WINDOW, 1, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hxt[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Maximum Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hxtAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Maximum temperature ' + STRCRA(xtCutoff * 100) + '%: ', xtMax

  ;; WSET_OR_WINDOW, 2, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hnt[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Minimum Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hntAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Minimum temperature ' + STRCRA(ntCutoff * 100) + '%: ', ntMax


;     Inventory snowfall reports the following criteria will miss.

      atThreshold = atMax1
      ntThreshold = 0.0

      hitInd = WHERE((atc lt atThreshold) and $
                     (ntc lt ntThreshold), hitCount)

      missInd = WHERE((atc ge atThreshold) or $
                      (ntc ge ntThreshold), missCount)

      if ((hitCount + missCount) ne snflEventCount) then STOP

      missRate1 = FLOAT(missCount) / FLOAT(snflEventCount) * 100.0
      hitRate1 = FLOAT(hitCount) / FLOAT(snflEventCount) * 100.0

  ;; PRINT, 'Thresholds of ' + $
  ;;        STRCRA(ntThreshold) + ' deg for minimum temp. and ' + $
  ;;        STRCRA(atThreshold) + ' deg for average temp. have a ' + $
  ;;        STRCRA(FLOAT(missCount) / FLOAT(snflEventCount) * 100.0) + $
  ;;        '% miss rate (i.e., POD = ' + $
  ;;        STRCRA(FLOAT(hitCount) / FLOAT(snflEventCount) * 100.0) + $
  ;;        ').'

  endelse


; Repeat analysis, but do not consider any results for days where the
; minimum temperature is above freezing.

  atCutoff = 0.95 ; cumulative histogram cutoff for average temperature
  xtCutoff = 0.95 ; cumulative histogram cutoff for maximum temperature
  ntCutoff = 0.99 ; cumulative histogram cutoff for minimum temperature

  snflEventInd = WHERE((f ge snflThreshold) and (nt lt 0.0), $
                       snflEventCount)
  if (snflEventCount eq 0) then begin
      atMax2 = ndv
      xtMax2 = ndv
      ntMax2 = ndv
      hitRate2 = ndv
      missRate2 = ndv
  endif else begin

      atc = at[snflEventInd]
      xtc = xt[snflEventInd]
      ntc = nt[snflEventInd]

      hMin = -30.0D
      hMax = 30.0D
      binSize = 0.5D
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * (binSize * numBins - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad

      hat = HISTOGRAM(atc, MIN = hMin, MAX = hMax, BINSIZE = binSize)
      hxt = HISTOGRAM(xtc, MIN = hMin, MAX = hMax, BINSIZE = binSize)
      hnt = HISTOGRAM(ntc, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      hx = hMin + (0.5 + FINDGEN(numBins)) * binSize
      hxAcc = hMin + FINDGEN(numBins + 1) * binSize

      hatAcc = FLTARR(numBins + 1)
      hatAcc[0] = 0.0
      for bc = 1, numBins do $
          hatAcc[bc] = hatAcc[bc - 1] + hat[bc - 1] / TOTAL(hat)

      i1 = MAX(WHERE(hatAcc lt atCutoff))
      atMax2 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hatAcc[i1+1] - hatAcc[i1]) * $
               (atCutoff - hatAcc[i1])

      hxtAcc = FLTARR(numBins + 1)
      hxtAcc[0] = 0.0
      for bc = 1, numBins do $
          hxtAcc[bc] = hxtAcc[bc - 1] + hxt[bc - 1] / TOTAL(hxt)

      i1 = MAX(WHERE(hxtAcc lt xtCutoff))
      xtMax2 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hxtAcc[i1+1] - hxtAcc[i1]) * $
               (xtCutoff - hxtAcc[i1])

      hntAcc = FLTARR(numBins + 1)
      hntAcc[0] = 0.0
      for bc = 1, numBins do $
          hntAcc[bc] = hntAcc[bc - 1] + hnt[bc - 1] / TOTAL(hnt)

      i1 = MAX(WHERE(hntAcc lt ntCutoff))
      ntMax2 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hntAcc[i1+1] - hntAcc[i1]) * $
               (ntCutoff - hntAcc[i1])

  ;; WSET_OR_WINDOW, 0, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hat[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Average Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hatAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Average temperature ' + STRCRA(atCutoff * 100) + '%: ', atMax

  ;; WSET_OR_WINDOW, 1, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hxt[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Maximum Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hxtAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Maximum temperature ' + STRCRA(xtCutoff * 100) + '%: ', xtMax

  ;; WSET_OR_WINDOW, 2, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hnt[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Minimum Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hntAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Minimum temperature ' + STRCRA(ntCutoff * 100) + '%: ', ntMax


;     Inventory snowfall reports the following criteria will miss.

      atThreshold = atMax2
      ntThreshold = 0.0

      hitInd = WHERE((atc lt atThreshold) and $
                     (ntc lt ntThreshold), hitCount)

      missInd = WHERE((atc ge atThreshold) or $
                      (ntc ge ntThreshold), missCount)

      if ((hitCount + missCount) ne snflEventCount) then STOP

      missRate2 = FLOAT(missCount) / FLOAT(snflEventCount) * 100.0
      hitRate2 = FLOAT(hitCount) / FLOAT(snflEventCount) * 100.0

  ;; PRINT, 'Thresholds of ' + $
  ;;        STRCRA(ntThreshold) + ' deg for minimum temp. and ' + $
  ;;        STRCRA(atThreshold) + ' deg for average temp. have a ' + $
  ;;        STRCRA(FLOAT(missCount) / FLOAT(snflEventCount) * 100.0) + $
  ;;        '% miss rate (i.e., POD = ' + $
  ;;        STRCRA(FLOAT(hitCount) / FLOAT(snflEventCount) * 100.0) + $
  ;;        ').'

  endelse


; Repeat analysis, again do not consider any results for days where the
; minimum temperature is above freezing, but use a 90% average
; temperature cutoff.

  atCutoff = 0.90 ; cumulative histogram cutoff for average temperature
  xtCutoff = 0.95 ; cumulative histogram cutoff for maximum temperature
  ntCutoff = 0.99 ; cumulative histogram cutoff for minimum temperature

  snflEventInd = WHERE((f ge snflThreshold) and (nt lt 0.0), $
                       snflEventCount)
  if (snflEventCount eq 0) then begin

      atMax3 = ndv
      xtMax3 = ndv
      ntMax3 = ndv
      hitRate3 = ndv
      missRate3 = ndv

  endif else begin

      atc = at[snflEventInd]
      xtc = xt[snflEventInd]
      ntc = nt[snflEventInd]

      hMin = -30.0D
      hMax = 30.0D
      binSize = 0.5D
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * (binSize * numBins - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad

      hat = HISTOGRAM(atc, MIN = hMin, MAX = hMax, BINSIZE = binSize)
      hxt = HISTOGRAM(xtc, MIN = hMin, MAX = hMax, BINSIZE = binSize)
      hnt = HISTOGRAM(ntc, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      hx = hMin + (0.5 + FINDGEN(numBins)) * binSize
      hxAcc = hMin + FINDGEN(numBins + 1) * binSize

      hatAcc = FLTARR(numBins + 1)
      hatAcc[0] = 0.0
      for bc = 1, numBins do $
          hatAcc[bc] = hatAcc[bc - 1] + hat[bc - 1] / TOTAL(hat)

      i1 = MAX(WHERE(hatAcc lt atCutoff))
      atMax3 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hatAcc[i1+1] - hatAcc[i1]) * $
               (atCutoff - hatAcc[i1])

      hxtAcc = FLTARR(numBins + 1)
      hxtAcc[0] = 0.0
      for bc = 1, numBins do $
          hxtAcc[bc] = hxtAcc[bc - 1] + hxt[bc - 1] / TOTAL(hxt)

      i1 = MAX(WHERE(hxtAcc lt xtCutoff))
      xtMax3 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hxtAcc[i1+1] - hxtAcc[i1]) * $
               (xtCutoff - hxtAcc[i1])

      hntAcc = FLTARR(numBins + 1)
      hntAcc[0] = 0.0
      for bc = 1, numBins do $
          hntAcc[bc] = hntAcc[bc - 1] + hnt[bc - 1] / TOTAL(hnt)

      i1 = MAX(WHERE(hntAcc lt ntCutoff))
      ntMax3 = hxAcc[i1] + $
               (hxAcc[i1+1] - hxAcc[i1]) / (hntAcc[i1+1] - hntAcc[i1]) * $
               (ntCutoff - hntAcc[i1])

  ;; WSET_OR_WINDOW, 0, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hat[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Average Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hatAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Average temperature ' + STRCRA(atCutoff * 100) + '%: ', atMax

  ;; WSET_OR_WINDOW, 1, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hxt[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Maximum Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hxtAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Maximum temperature ' + STRCRA(xtCutoff * 100) + '%: ', xtMax

  ;; WSET_OR_WINDOW, 2, XSIZE=800, YSIZE=500
  ;; PLOT, hx, hnt[0:numBins - 1], $
  ;;       PSYM = 10, $
  ;;       XTITLE = 'deg C', $
  ;;       TITLE = 'Minimum Temperature with Snowfall >= ' + $
  ;;       FORMAT_FLOAT(snflThreshold) + ' in, ' + $
  ;;       STRMID(startDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(startDate_MMDD, 2, 2) + ' to ' + $
  ;;       STRMID(finishDate_MMDD, 0, 2) + '/' + $
  ;;       STRMID(finishDate_MMDD, 2, 2) + ', ' + $
  ;;       STRING(startYear, FORMAT = '(i4.4)') + ' to ' + $
  ;;       STRING(finishYear, FORMAT = '(i4.4)')
  ;; OPLOT, hxAcc, hntAcc * !Y.CRange[1], PSYM = -4
  ;; PRINT, 'Minimum temperature ' + STRCRA(ntCutoff * 100) + '%: ', ntMax


;     Inventory snowfall reports the following criteria will miss.

      atThreshold = atMax3
      ntThreshold = 0.0

      hitInd = WHERE((atc lt atThreshold) and $
                     (ntc lt ntThreshold), hitCount)

      missInd = WHERE((atc ge atThreshold) or $
                      (ntc ge ntThreshold), missCount)

      if ((hitCount + missCount) ne snflEventCount) then STOP

      missRate3 = FLOAT(missCount) / FLOAT(snflEventCount) * 100.0
      hitRate3 = FLOAT(hitCount) / FLOAT(snflEventCount) * 100.0

  ;; PRINT, 'Thresholds of ' + $
  ;;        STRCRA(ntThreshold) + ' deg for minimum temp. and ' + $
  ;;        STRCRA(atThreshold) + ' deg for average temp. have a ' + $
  ;;        STRCRA(FLOAT(missCount) / FLOAT(snflEventCount) * 100.0) + $
  ;;        '% miss rate (i.e., POD = ' + $
  ;;        STRCRA(FLOAT(hitCount) / FLOAT(snflEventCount) * 100.0) + $
  ;;        ').'

  endelse

  PRINT, startDate_MMDD + ', ' + finishDate_MMDD + ', ' + $
         STRCRA(atMax1) + ', ' + $
         STRCRA(xtMax1) + ', ' + $
         STRCRA(ntMax1) + ', ' + $
         STRCRA(missRate1) + ', ' + $
         STRCRA(atMax2) + ', ' + $
         STRCRA(missRate2) + ', ' + $
         STRCRA(atMax3) + ', ' + $
         STRCRA(missRate3)




BAIL:


end
