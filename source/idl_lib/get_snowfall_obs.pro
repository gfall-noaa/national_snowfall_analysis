PRO GET_SNOWFALL_OBS, targetObsDate_YYYYMMDDHH, $
                      windowStartDate_YYYYMMDDHH, $
                      windowFinishDate_YYYYMMDDHH, $
                      durationHours, $
                      minLon_, maxLon_, minLat_, maxLat_, $
                      PGHost, webPGHost, $
                      snowfallReport

  COMMON info, message

  snowfallReport = !NULL


; Check arguments for correct type and valid contents.

  if NOT(ISA(targetObsDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Target observation date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(targetObsDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid target observation date/time "' + $
               targetObsDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(targetObsDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid target observation date/time "' + $
               targetObsDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  if NOT(ISA(windowStartDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Observation window start date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(windowStartDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid observation window start date/time "' + $
               windowStartDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(windowStartDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid observation window start date/time "' + $
               windowStartDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  if NOT(ISA(windowFinishDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Observation window finish date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(windowFinishDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid observation window finish date/time "' + $
               windowFinishDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(windowFinishDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid observation window finish date/time "' + $
               windowFinishDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  targetObsDate_Julian = YYYYMMDDHH_TO_JULIAN(targetObsDate_YYYYMMDDHH)
  windowStartDate_Julian = YYYYMMDDHH_TO_JULIAN(windowStartDate_YYYYMMDDHH)
  windowFinishDate_Julian = YYYYMMDDHH_TO_JULIAN(windowFinishDate_YYYYMMDDHH)

  if (windowStartDate_Julian gt targetObsDate_Julian) then begin
      ERR_MSG, 'Observation window start date/time may not be later than ' + $
               'target observation date/time.'
      RETURN
  endif

  if (windowFinishDate_Julian lt targetObsDate_Julian) then begin
      ERR_MSG, 'Observation window finish date/time may not be earlier ' + $
               'than target observation date/time.'
      RETURN
  endif

  if (durationHours lt 0) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      RETURN
  endif

  if NOT(ISA(minLon_, 'DOUBLE')) then $
      minLon = DOUBLE(minLon_) $
  else $
      minLon = minLon_

  if NOT(ISA(maxLon_, 'DOUBLE')) then $
      maxLon = DOUBLE(maxLon_) $
  else $
      maxLon = maxLon_

  if NOT(ISA(minLat_, 'DOUBLE')) then $
      minLat = DOUBLE(minLat_) $
  else $
      minLat = minLat_

  if NOT(ISA(maxLat_, 'DOUBLE')) then $
      maxLat = DOUBLE(maxLat_) $
  else $
      maxLat = maxLat_

  if NOT(ISA(PGHost, 'STRING')) then begin
      ERR_MSG, 'PostgreSQL "PGHOST" argument must be a STRING.'
      RETURN
  endif

  if NOT(ISA(webPGHost, 'STRING')) then begin
      ERR_MSG, 'PostgreSQL "WEBPGHOST" argument must be a STRING.'
      RETURN
  endif


; Make sure the PGHOSTADDR environment variable is not set.

  PGHostAddr = GETENV('PGHOSTADDR')
  if (PGHostAddr ne '') then begin
      ERR_MSG, 'Environment variable PGHOSTADDR must not be set.'
      RETURN
  endif

  
; Generate strings for SQL statements defining the analysis domain.

  minLonStr = STRCRA(STRING(minLon, FORMAT='(F25.17)'))
  maxLonStr = STRCRA(STRING(maxLon, FORMAT='(F25.17)'))
  minLatStr = STRCRA(STRING(minLat, FORMAT='(F25.17)'))
  maxLatStr = STRCRA(STRING(maxLat, FORMAT='(F25.17)'))


; Choose a server: operational database or web database.

  date_Julian = SYSTIME(/JULIAN, /UTC)

  if ((date_Julian - windowStartDate_Julian) gt 28.0D) then $
    database = 'web' $
  else $
    database = 'ops'


; Get snowfall_raw observations.

  windowStartDate = STRMID(windowStartDate_YYYYMMDDHH, 0, 4) + '-' + $
                    STRMID(windowStartDate_YYYYMMDDHH, 4, 2) + '-' + $
                    STRMID(windowStartDate_YYYYMMDDHH, 6, 2) + ' ' + $
                    STRMID(windowStartDate_YYYYMMDDHH, 8, 2) + ':' + $
                    '00:00'

  windowFinishDate = STRMID(windowFinishDate_YYYYMMDDHH, 0, 4) + '-' + $
                     STRMID(windowFinishDate_YYYYMMDDHH, 4, 2) + '-' + $
                     STRMID(windowFinishDate_YYYYMMDDHH, 6, 2) + ' ' + $
                     STRMID(windowFinishDate_YYYYMMDDHH, 8, 2) + ':' + $
                     '00:00'

  if (database eq 'web') then begin

      statement = 'psql -d web_data -h ' + webPGHost + ' -t -A -c ' + $
                  '"' + $
                  'select ' + $
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
                  't1.coordinates[0] >= ' + minLonStr + ' ' + $
                  'and t1.coordinates[0] <= ' + maxLonStr + ' ' + $
                  'and t1.coordinates[1] >= ' + minLatStr + ' ' + $
                  'and t1.coordinates[1] <= ' + maxLatStr + ' ' + $ 
                  'and t2.date >= ''' + $
                  windowStartDate + ''' ' + $
                  'and t2.date <= ''' + $
                  windowFinishDate + ''' ' + $
                  'and t2.duration = ' + $
                  STRCRA(durationHours * 3600L) + ' ' + $
                  'and t2.value >= 0.0 ' + $
                  'and t2.value < 99.5 ' + $
                  'and t2.value is not NULL ' + $
                  'and t1.obj_identifier = t2.obj_identifier ' + $
                  'order by t1.coordinates[1], t1.coordinates[0];"'

  endif else begin

      statement = 'psql -d operations -h ' + PGHost + ' -t -A -c ' + $
                  '"' + $
                  'set search_path = nohrsc; ' + $
                  'select ' + $
                  'trim(station.station_id), ' + $
                  'trim(station.name), ' + $
                  'trim(station.station_type), ' + $
                  'station.longitude, ' + $
                  'station.latitude, ' + $
                  'station.elevation, ' + $
                  'snowfall_raw.date, ' + $
                  'snowfall_raw.value ' + $
                  'from station, snowfall_raw ' + $
                  'where station.longitude >= ' + minLonStr + ' ' + $
                  'and station.longitude <= ' + maxLonStr + ' ' + $
                  'and station.latitude >= ' + minLatStr + ' ' + $
                  'and station.latitude <= ' + maxLatStr + ' ' + $
                  'and snowfall_raw.date >= ''' + $
                  windowStartDate + ''' ' + $
                  'and snowfall_raw.date <= ''' + $
                  windowFinishDate + ''' ' + $
                  'and snowfall_raw.duration = ' + $
                  STRCRA(durationHours) + ' ' + $
                  'and snowfall_raw.qc = 1 ' + $
                  'and snowfall_raw.value >= 0.0 ' + $
                  'and snowfall_raw.value < 99.5 ' + $
                  'and station.station_id = snowfall_raw.station_id ' + $
                  'order by station.latitude, station.longitude;"'

  endelse

;  USR_MSG, statement
  SPAWN, statement, result, EXIT_STATUS = status

  if (status ne 0) then begin
      ERR_MSG, 'psql statement failed: ' + statement
      RETURN
  endif

  numSnowfall = N_ELEMENTS(result)

  if (result[0] eq '') then begin
      numSnowfall = 0
      RETURN
  endif


; Place results in a structure.

  snowfallReport = REPLICATE({station_id: '', $
                              station_name: '', $
                              station_type: '', $
                              longitude: 0.0d, $
                              latitude: 0.0d, $
                              elevation: 0L, $
                              date_UTC: '', $
                              value_meters: 0.0, $
                              qc: 0L}, $ ; this is used later, perhaps
                             numSnowfall)

  for sc = 0, numSnowfall - 1 do begin
      report = STRSPLIT(result[sc], '|', /EXTRACT)
      if (N_ELEMENTS(report) ne 8) then begin
          ERR_MSG, 'Unrecognized structure in snowfall_raw reports.'
          numSnowfall = 0
          snowfallReport = !NULL
          RETURN
      endif
      snowfallReport[sc].station_id = report[0]
      snowfallReport[sc].station_name = report[1]
;      snowfallReport[sc].station_name = station_name
      snowfallReport[sc].station_type = report[2]
      snowfallReport[sc].longitude = DOUBLE(report[3])
      snowfallReport[sc].latitude = DOUBLE(report[4])
      snowfallReport[sc].elevation = LONG(report[5])
      snowfallReport[sc].date_UTC = report[6]
      valueMeters = FLOAT(report[7])
;     Convert positive values less than 0.1 inch (0.00254 m) to 0.01 inch
      if ((valueMeters gt 0.0) and (valueMeters lt 0.00254)) then $
          valueMeters = .000254
      snowfallReport[sc].value_meters = valueMeters
  endfor

; If any stations delivered multiple reports, choose the one closest
; to the analysis time.

  useFlag = BYTARR(numSnowfall) & useFlag[*] = 1B

  for sc = 0, numSnowfall - 1 do begin

      ind = WHERE((snowfallReport.station_id eq $
                   snowfallReport[sc].station_id) and $
                  (useFlag eq 1B), count)
      if (count eq 0) then begin
          ERR_MSG, 'Programming error.'
          RETURN
      endif
      if (count eq 1) then CONTINUE
      useFlag[ind] = 0B         ; set all matches to DO NOT USE
      dateList = snowfallReport[ind].date_UTC
      dateList_Julian = DBLARR(count)
      for ic = 0, count - 1 do begin
          dateList_Julian[ic] = $
              JULDAY(FIX(STRMID(dateList[ic], 5, 2)), $    ; month
                     FIX(STRMID(dateList[ic], 8, 2)), $    ; day
                     FIX(STRMID(dateList[ic], 0, 4)), $    ; year
                     FIX(STRMID(dateList[ic], 11, 2)), $   ; hour
                     FIX(STRMID(dateLIst[ic], 14, 2)), $   ; minute
                     FIX(STRMID(dateList[ic], 17, 2)))     ; second
      endfor
      timeDiff = MIN(ABS(targetObsDate_Julian - dateList_Julian), minInd)
      useFlag[ind[minInd]] = 1B
  endfor

  ind = WHERE(useFlag eq 1B, numSnowfall)
  if (numSnowfall gt 0) then begin
      snowfallReport = snowfallReport[ind]
  endif else begin
      ERR_MSG, 'Programming error in unique station search.'
      snowfallReport = !NULL
  endelse

  RETURN

end
