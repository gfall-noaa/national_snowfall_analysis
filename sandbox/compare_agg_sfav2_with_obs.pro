; Compare multi-day (up to seasonal) aggregations of snowfall analysis
; v2 with aggregated observations.

; Greg Fall, NOHRSC
; 2017-04-18
;goto, skip
  minLonAnl = -135.0D
  maxLonAnl = -57.0D
  minLatAnl = 16.0D
  maxLatAnl = 60.0D

; Snowfall analysis output coordinates. Do not mess with these unless
; you also crop or expand the seasonal analysis results stored as a
; GeoTIFF.

  minLonOut = -126.0D
  maxLonOut = -66.0D
  minLatOut = 21.0D
  maxLatOut = 55.0D

  startDate_YYYYMMDDHH = '2019100112' ; leave this one alone too
  finishDate_YYYYMMDDHH = '2020060112' ; this is safe to tamper with

  savFile = 'compare_agg_sfav2_' + startDate_YYYYMMDDHH + '_to_' + $
            finishDate_YYYYMMDDHH + '.sav'
  fullRun = 1
  if FILE_TEST(savFile) then begin
      fullRun = 0
      GOTO, SKIP
  endif

  durationHours = 24 ; do not change
  windowHoursBack = 3
  windowHoursForward = 3

  sfav2Dir = '/operations/misc/snowfall_v2'

  startDate_Julian = YYYYMMDDHH_TO_JULIAN(startDate_YYYYMMDDHH)
  finishDate_Julian = YYYYMMDDHH_TO_JULIAN(finishDate_YYYYMMDDHH)

  numDays = ROUND(finishDate_Julian - startDate_Julian) + 1
  numStations = 10000L ; guess at expected number of stations
  asc = 0L             ; counter of actual stations providing data

  ndv = -99999.0

  snowfallReport = REPLICATE({station_id: '', $
                              station_name: '', $
                              station_type: '', $
                              longitude: 0.0D, $
                              latitude: 0.0D, $
                              elevation: 0L, $
                              use: 1, $
                              outlier_count: 0, $
                              obs_val_meters: REPLICATE(ndv, numDays), $
                              anl_val_inches: REPLICATE(ndv, numDays)}, $
                             numStations)

  date_Julian = startDate_Julian

;;  checkEvery = 15    ; how often (days) to check for good reporters,

;; IDL> help, dailysnowfallreport, /stru 
;; ** Structure <195b958>, 9 tags, length=96, data length=92, refs=1:
;;    STATION_ID      STRING    'FL-MN-16'
;;    STATION_NAME    STRING    'KEY WEST 1.3 ENE, FL'
;;    STATION_TYPE    STRING    'COCORAHS'
;;    LONGITUDE       DOUBLE          -81.755900
;;    LATITUDE        DOUBLE           24.569500
;;    ELEVATION       LONG                 1
;;    DATE_UTC        STRING    '2016-10-01 11:00:00'
;;    OBS_VAL_METERS    FLOAT           0.00000
;;    QC              LONG                 0

  tc = -1L

  while (date_Julian le finishDate_Julian) do begin

      tc++
      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

      windowFinishDate_Julian = $
          date_Julian + DOUBLE(windowHoursForward) / 24.0D
      windowFinishDate_YYYYMMDDHH = $
          JULIAN_TO_YYYYMMDDHH(windowFinishDate_Julian)
      windowStartDate_Julian = $
          windowFinishDate_Julian - $
          DOUBLE(windowHoursBack + windowHoursForward) / 24.0D
      windowStartDate_YYYYMMDDHH = $
          JULIAN_TO_YYYYMMDDHH(windowStartDate_Julian)

      PRINT, date_YYYYMMDDHH, '  ', numStations, asc

      thisSnowfallReport = !NULL

      GET_SNOWFALL_OBS, date_YYYYMMDDHH, $
                        windowStartDate_YYYYMMDDHH, $
                        windowFinishDate_YYYYMMDDHH, $
                        durationHours, $
                        minLonOut, maxLonOut, minLatOut, maxLatOut, $
                        'ddb0.dev.nohrsc.noaa.gov', $
                        'wdb0.dmz.nohrsc.noaa.gov', $
                        thisSnowfallReport

      if NOT(ISA(thisSnowfallReport)) then begin
          date_Julian = date_Julian + 1.0D
          CONTINUE
      endif

      numObs = N_ELEMENTS(thisSnowfallReport)

      for oc = 0, numObs - 1 do begin


;         Look for outlier reporting on this obs.

          isOutlier = 0
          dir = sfav2Dir + '/sfav2_' + STRMID(date_YYYYMMDDHH, 0, 8)
          file = 'sfav2_CONUS_24h_' + date_YYYYMMDDHH + $
                 '_reject_1_diff.csv'
          if FILE_TEST(dir + '/' + file) then begin
              cmd = 'grep -q ",' + $
                    thisSnowfallReport[oc].station_id + $
                    '$" ' + dir + '/' + file
              SPAWN, cmd, EXIT_STATUS = status
              if (status eq 0) then $
                  isOutlier = 1 $
              else begin
                  file = 'sfav2_CONUS_24h_' + date_YYYYMMDDHH + $
                         '_reject_2_diff.csv'
                  if FILE_TEST(dir + '/' + file) then begin
                      cmd = 'grep -q ",' + $
                            thisSnowfallReport[oc].station_id + $
                            '$" ' + dir + '/' + file
                      SPAWN, cmd, EXIT_STATUS = status
                      if (status eq 0) then isOutlier = 1
                  endif
              endelse
          endif


;         Locate the current station in the snowfallReport structure.

          ind = WHERE(thisSnowfallReport[oc].station_id eq $
                      snowfallReport.station_id, $
                      count)

          if (count eq 0) then begin


;             New station. Need to add to snowfallReport.

              if (asc eq numStations) then begin

                  
;                 Expand snowfallReport structure to fit
;                 the new station.

                  snowfallReport = $
                      [snowfallReport, $
                       {station_id: '', $
                        station_name: '', $
                        station_type: '', $
                        longitude: 0.0D, $
                        latitude: 0.0D, $
                        elevation: 0L, $
                        use: 1, $
                        outlier_count: 0, $
                        obs_val_meters: REPLICATE(ndv, numDays), $
                        anl_val_inches: REPLICATE(ndv, numDays)}]

                  numStations++

              endif


;             Add the station and its data for the current day.

              snowfallReport[asc].station_id = $
                  thisSnowfallReport[oc].station_id
              snowfallReport[asc].station_name = $
                  thisSnowfallReport[oc].station_name
              snowfallReport[asc].station_type = $
                  thisSnowfallReport[oc].station_type
              snowfallReport[asc].longitude = $
                  thisSnowfallReport[oc].longitude
              snowfallReport[asc].latitude = $
                  thisSnowfallReport[oc].latitude
              snowfallReport[asc].elevation = $
                  thisSnowfallReport[oc].elevation
              snowfallReport[asc].outlier_count = isOutlier
              snowfallReport[asc].obs_val_meters[tc] = $
                  thisSnowfallReport[oc].value_meters

              asc++
              CONTINUE

          endif                 ; new station

          if (count ne 1) then STOP ; PROGRAMMING ERROR
          ind = ind[0]


;         Check metadata for consistency.

          numMetaChange = 0

          if ((thisSnowfallReport[oc].longitude ne $
               snowfallReport[ind].longitude) or $
              (thisSnowfallReport[oc].latitude ne $
               snowfallReport[ind].latitude)) then begin

              distanceMoved = $
                  DISTANCE(3, $
                           snowfallReport[ind].longitude, $
                           snowfallReport[ind].latitude, $
                           thisSnowfallReport[oc].longitude, $
                           thisSnowfallReport[oc].latitude)
              if (distanceMoved gt 1000.0) then begin
                  snowfallReport[ind].use = 0
                  ERR_MSG, 'WARNING: station "' + $
                           snowfallReport[ind].station_id + '" ' + $
                           ' moved ' + STRCRA(distanceMoved / 1000.0) + $
                           ' km; data will not be used.'
              endif

          endif

          if ((snowfallReport[ind].use) and $
              (thisSnowfallReport[oc].station_name ne $
               snowfallReport[ind].station_name)) then begin
              ERR_MSG, 'WARNING: station "' + $
                       snowfallReport[ind].station_id + '" ' + $
                       'has station_name inconsistency: "' + $
                       snowfallReport[ind].station_name + '" ' + $
                       'vs. "' + $
                       thisSnowfallReport[oc].station_name + '".'
              snowfallReport[ind].station_name = $
                  thisSnowfallReport[oc].station_name
              numMetaChange++
          endif

          if ((snowfallReport[ind].use) and $
              (thisSnowfallReport[oc].station_type ne $
               snowfallReport[ind].station_type)) then begin
              ERR_MSG, 'WARNING: station "' + $
                       snowfallReport[ind].station_id + '" ' + $
                       'has station_type inconsistency: "' + $
                       snowfallReport[ind].station_type + '" ' + $
                       'vs. "' + $
                       thisSnowfallReport[oc].station_type + '".'
              snowfallReport[ind].station_type = $
                  thisSnowfallReport[oc].station_type
              numMetaChange++
          endif

          if ((snowfallReport[ind].use) and $
              (thisSnowfallReport[oc].longitude ne $
               snowfallReport[ind].longitude)) then begin
              ERR_MSG, 'WARNING: station "' + $
                       snowfallReport[ind].station_id + '" ' + $
                       'has longitude inconsistency: ' + $
                       STRCRA(snowfallReport[ind].longitude) + $
                       ' vs. ' + $
                       STRCRA(thisSnowfallReport[oc].longitude)
              snowfallReport[ind].longitude = $
                  thisSnowfallReport[oc].longitude
              numMetaChange++
          endif

          if ((snowfallReport[ind].use) and $
              (thisSnowfallReport[oc].latitude ne $
               snowfallReport[ind].latitude)) then begin
              ERR_MSG, 'WARNING: station "' + $
                       snowfallReport[ind].station_id + '" ' + $
                       'has latitude inconsistency: ' + $
                       STRCRA(snowfallReport[ind].latitude) + $
                       ' vs. ' + $
                       STRCRA(thisSnowfallReport[oc].latitude)
              snowfallReport[ind].latitude = $
                  thisSnowfallReport[oc].latitude
              numMetaChange++
          endif

          if ((snowfallReport[ind].use) and $
              (thisSnowfallReport[oc].elevation ne $
               snowfallReport[ind].elevation)) then begin
              ERR_MSG, 'WARNING: station "' + $
                       snowfallReport[ind].station_id + '" ' + $
                       'has elevation inconsistency: ' + $
                       STRCRA(snowfallReport[ind].elevation) + $
                       ' vs. ' + $
                       STRCRA(thisSnowfallReport[oc].elevation)
              snowfallReport[ind].elevation = $
                  thisSnowfallReport[oc].elevation
              numMetaChange++
          endif

          if (numMetaChange gt 0) then $
              ERR_MSG, 'WARNING: ' + STRCRA(numMetaChange) + $
                       ' metadata fields were modified for station "' + $
                       snowfallReport[ind].station_id + '".'
 

;         Add new data to the obs_val_meters field.

          if (snowfallReport[ind].obs_val_meters[tc] ne ndv) then STOP

          snowfallReport[ind].obs_val_meters[tc] = $
              thisSnowfallReport[oc].value_meters

          snowfallReport[ind].outlier_count = $
              snowfallReport[ind].outlier_count + isOutlier

          if (isOutlier) then begin
              PRINT, snowfallReport[ind].station_id + $
                     ' outlier count: ' + $
                     STRCRA(snowfallReport[ind].outlier_count)
          endif

      endfor


;     Find the daily analysis.

      dir = sfav2Dir + '/sfav2_' + STRMID(date_YYYYMMDDHH, 0, 8)
      file = 'sfav2_CONUS_24h_' + date_YYYYMMDDHH + '.tif'
      if NOT(FILE_TEST(dir + '/' + file)) then STOP


;     Read the daily analysis grid.

      grid = READ_TIFF(dir + '/' + file, GEOTIFF = geoData)
      grid = REVERSE(grid, 2)
      if (MIN(grid) ne ndv) then STOP ; no-data values are guaranteed here


;     Calculate lon/lat grid parameters for the daily analysis.

      minLonOut_ = geoData.modelTiePointTag[3]
      if (minLonOut_ ne minLonOut) then STOP
      maxLatOut_ = geoData.modelTiePointTag[4]
      if (maxLatOut_ ne maxLatOut) then STOP
      lonResOut = geoData.modelPixelScaleTag[0]
      latResOut = geoData.modelPixelScaleTag[1]
      foo = SIZE(grid)
      if (foo[0] ne 2) then STOP
      nColsOut = foo[1]
      nRowsOut = foo[2]
      minLatOut_ = maxLatOut_ - nRowsOut * latResOut
      if (minLatOut_ ne minLatOut) then STOP
      maxLonOut_ = minLonOut_ + nColsOut * lonResOut
      if (maxLonOut_ ne maxLonOut) then STOP


;     Sample the daily analysis for all locations.

      iOut = ((snowfallReport.longitude - minLonOut) / lonResOut - 0.5D)
      jOut = ((snowfallReport.latitude - minLatOut) / latResOut - 0.5D)

      if ((N_ELEMENTS(iOut) ne numStations) or $
          (N_ELEMENTS(jOut) ne numStations)) then STOP

      reportAnlTotal = REGRID_BILIN(grid, iOut, jOut, ndv)

      snowfallReport.anl_val_inches[tc] = $
          REGRID_BILIN(grid, iOut, jOut, ndv)
      
      PRINT, 'Sampled ' + date_YYYYMMDDHH + ' analysis for ' + $
             STRCRA(numStations) + ' station locations.'


;     --- THIS SECTION PLOTS RESULTS AS WE GO AND IS OPTIONAL. ---


;     Identify good reporters and aggregate.

      numTimes = tc + 1
      reportPercent = INTARR(numStations)
      reportObsTotal = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndv)
      reportAnlTotal = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndv)

      for sc = 0, numStations - 1 do begin

          ind = WHERE((snowfallReport[sc].obs_val_meters ne ndv) and $
                      (snowfallReport[sc].anl_val_inches ne ndv), count)
          reportPercent[sc] = ROUND(FLOAT(count) / FLOAT(numTimes) * 100.0)
          if (count gt 0) then begin
              reportObsTotal[sc] = $
                  TOTAL(snowfallReport[sc].obs_val_meters[ind])
              reportAnlTotal[sc] = $
                  TOTAL(snowfallReport[sc].anl_val_inches[ind])
          endif

      endfor


;     Compare seasonal totals for good reporters as we go.

      percentThresh = 50
      ind = WHERE((snowfallReport.use eq 1) and $
                  (reportPercent ge percentThresh) and $
                  (reportObsTotal ne ndv) and $
                  (reportAnlTotal ne ndv), count)

      PRINT, STRCRA(count) + ' of ' + STRCRA(numStations) + $
             ' stations reported at least ' + $
             STRCRA(percentThresh) + '% of the time.'
      if (count eq 0) then GOTO, NEXT

      ind2 = WHERE((reportObsTotal[ind] gt 0.0) and $
                   (reportAnlTotal[ind] gt 0.0), count2)
      if (count2 lt 2) then GOTO, NEXT

      bias = TOTAL(reportAnlTotal[ind[ind2]]) / $
             TOTAL(reportObsTotal[ind[ind2]] * 39.3701)
      corr = CORRELATE(reportObsTotal[ind[ind2]] * 39.3701, $
                       reportAnlTotal[ind[ind2]])
      if NOT(FINITE(corr)) then STOP

      PRINT, 'Bias in total snowfall: ', bias
      PRINT, 'Correlation: ', corr

      WSET_OR_WINDOW, 0, XSIZE = 1000, YSIZE = 1000
      PLOT, reportObsTotal[ind] * 39.3701, $
            reportAnlTotal[ind], $
            TITLE = 'Total SFAv2 vs. Observed Snowfall', $
            XRANGE = [0, 250], $
            XTITLE = 'Total Observed Snowfall (inches)', $
            YRANGE = [0, 250], $
            YTITLE = 'Total Snowfall Analysis (inches)', $
            PSYM = 4


;     Identify stations with the largest discrepancies.

      error = reportAnlTotal[ind[ind2]] - reportObsTotal[ind[ind2]]
      rse = SQRT(error^2.0)
      order = REVERSE(SORT(rse))
      for oc = 0, (9 < (count2 - 1)) do begin
          PRINT, snowfallReport[ind[ind2[order[oc]]]].station_id, ' ', $
                 snowfallReport[ind[ind2[order[oc]]]].longitude, ' ', $
                 snowfallReport[ind[ind2[order[oc]]]].latitude, ' ', $
                 reportObsTotal[ind[ind2[order[oc]]]], ' ', $
                 reportAnlTotal[ind[ind2[order[oc]]]], ' ', $
                 error[order[oc]]
      endfor

NEXT:

;     --- END OF OPTIONAL SECTION FOR AS-YOU-GO RESULTS ---

      date_Julian = date_Julian + 1.0D

  endwhile


  SAVE, /VARIABLES, FILE = savFile
SKIP:
  if NOT(fullRun) then RESTORE, savFile


; Identify good reporters and aggregate.

  numTimes = tc + 1
  reportPercent = INTARR(numStations)
  reportCount = LONARR(numStations)
  reportObsTotal = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndv)
  reportAnlTotal = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndv)

  for sc = 0, numStations - 1 do begin

      ind = WHERE((snowfallReport[sc].obs_val_meters ne ndv) and $
                  (snowfallReport[sc].anl_val_inches ne ndv), count)
      reportCount[sc] = count
      reportPercent[sc] = ROUND(FLOAT(count) / FLOAT(numTimes) * 100.0)
      if (count gt 0) then begin
          reportObsTotal[sc] = $
              TOTAL(snowfallReport[sc].obs_val_meters[ind])
          reportAnlTotal[sc] = $
              TOTAL(snowfallReport[sc].anl_val_inches[ind])
      endif

  endfor


;; ; Find the seasonal accumulated analysis.

;;   dir = sfav2Dir + '/sfav2_' + STRMID(finishDate_YYYYMMDDHH, 0, 8)
;;   ssd_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(startDate_Julian - 1.0D)
;;   file = 'sfav2_CONUS_' + ssd_YYYYMMDDHH + '_to_' + $
;;          finishDate_YYYYMMDDHH + '.tif'
;;   if NOT(FILE_TEST(dir + '/' + file)) then STOP


;; ; Read the seasonal analysis grid.

;;   grid = READ_TIFF(dir + '/' + file, GEOTIFF = geoData)
;;   grid = REVERSE(grid, 2)
;;   if (MIN(grid) ne ndv) then STOP ; no-data values are guaranteed here


;; ; Calculate lon/lat grid parameters for the seasonal analysis.

;;   minLonOut_ = geoData.modelTiePointTag[3]
;;   if (minLonOut_ ne minLonOut) then STOP
;;   maxLatOut_ = geoData.modelTiePointTag[4]
;;   if (maxLatOut_ ne maxLatOut) then STOP
;;   lonResOut = geoData.modelPixelScaleTag[0]
;;   latResOut = geoData.modelPixelScaleTag[1]
;;   foo = SIZE(grid)
;;   if (foo[0] ne 2) then STOP
;;   nColsOut = foo[1]
;;   nRowsOut = foo[2]
;;   minLatOut_ = maxLatOut_ - nRowsOut * latResOut
;;   if (minLatOut_ ne minLatOut) then STOP
;;   maxLonOut_ = minLonOut_ + nColsOut * lonResOut
;;   if (maxLonOut_ ne maxLonOut) then STOP


;; ; Sample the seasonal analysis for all locations.

;;   iOut = ((snowfallReport.longitude - minLonOut) / lonResOut - 0.5D)
;;   jOut = ((snowfallReport.latitude - minLatOut) / latResOut - 0.5D)

;;   reportAnlTotal = REGRID_BILIN(grid, iOut, jOut, ndv)


; Compare seasonal totals for good reporters.

  percentThresh = 90
  ind = WHERE((snowfallReport.use eq 1) and $
              (reportPercent ge percentThresh) and $
              (reportObsTotal ne ndv) and $
              (reportAnlTotal ne ndv), count)

  PRINT, STRCRA(count) + ' of ' + STRCRA(numStations) + $
         ' stations reported at least ' + $
         STRCRA(percentThresh) + '% of the time.'
  if (count eq 0) then STOP

  ind2 = WHERE((reportObsTotal[ind] gt 0.0) and $
               (reportAnlTotal[ind] gt 0.0), count2)
  if (count2 eq 0) then STOP


; Extract points we want to do more stats on.

  x = reportObsTotal[ind[ind2]] * 39.3701
  y = reportAnlTotal[ind[ind2]]
  n = reportCount[ind[ind2]]


; Do temporal correlation.

  tCorr = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndv)
  for sc = 0, count2 - 1 do begin
      obsTimeSeries = snowfallReport[ind[ind2[sc]]].obs_val_meters
      anlTimeSeries = snowfallReport[ind[ind2[sc]]].anl_val_inches
      ind3 = WHERE((obsTimeSeries ne ndv) and $
                   (anlTimeSeries ne ndv), count3)
      if (count3 gt 2) then begin
          tCorr[ind[ind2[sc]]] = $
              CORRELATE(obsTimeSeries[ind3] * 39.3701, $
                        anlTimeSeries[ind3])
      endif
  endfor

  bias = TOTAL(y) / TOTAL(x)
  corr = CORRELATE(x, y)
  rCorr = R_CORRELATE(x, y)
  rmse = SQRT(MEAN(((y - x)^2.0)))
  gmBias = 10.0^MEAN(ALOG10(y/x))
  if NOT(FINITE(corr)) then STOP

  PRINT, 'Comparing ' + STRCRA(count2) + ' results.'
  PRINT, 'Overall bias: ', bias
  PRINT, 'Geometric mean bias: ', gmBias
  PRINT, 'Correlation: ', corr
  PRINT, 'Rank correlation: ', rCorr
  PRINT, 'RMSE: ', rmse
  ind4 = WHERE(tCorr ne ndv, count4)
  if (count4 gt 0) then PRINT, 'Average temporal correlation: ', $
                               MEAN(tCorr[ind4])

  WSET_OR_WINDOW, 0, XSIZE = 1200, YSIZE = 1200
  hBinSize = 0.05
  hMin = -2.0
  hMax = 2.0
  numBins = ROUND((hMax - hMin) / hBinSize)
  hAxis = hMin + 0.5 * hBinSize + FINDGEN(numBins) * hBinSize
  h = HISTOGRAM(ALOG10(y/x), MIN = hMin, MAX = hMax, BINSIZE = hBinSize)
  PLOT, hAxis, h[0:numBins - 1], PSYM = 10, XTICK_GET = xTickV, $
        XTITLE = 'Log Multiplicative Bias'


  oldDevice = !D.Name
  oldFont = !P.Font
  SET_PLOT, 'PS'
  plotFile = 'compare_agg_sfav2_' + startDate_YYYYMMDDHH + '_to_' + $
             finishDate_YYYYMMDDHH + '_season_scatter'
  DEVICE, FILE = plotFile + '.ps'
  ;!P.Font = 0 ; PostScript
  !P.Font = 1 ; TrueType
  DEVICE, /COLOR, BITS = 8
  TVLCT, red, grn, blu, /GET
  blackInd = 0
  redInd = 1
  red[redInd] = 255
  grn[redInd] = 0
  blu[redInd] = 0
  TVLCT, red, grn, blu
  ;DEVICE, SET_FONT = 'Helvetica'
  DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT
  dates_str = STRMID(startDate_YYYYMMDDHH, 0, 4)  + '-' + $
              STRMID(startDate_YYYYMMDDHH, 4, 2)  + '-' + $
              STRMID(startDate_YYYYMMDDHH, 6, 2)  + ' to ' + $
              STRMID(finishDate_YYYYMMDDHH, 0, 4)  + '-' + $
              STRMID(finishDate_YYYYMMDDHH, 4, 2)  + '-' + $
              STRMID(finishDate_YYYYMMDDHH, 6, 2)
  xRange = [0.0, 250.0]
  yRange = [0.0, 250.0]
  ind5 = WHERE((reportObsTotal[ind] * 39.3701 lt xRange[1]) and $
               (reportAnlTotal[ind] lt yRange[1]))
  PLOT, reportObsTotal[ind[ind5]] * 39.3701, $
        reportAnlTotal[ind[ind5]], $
        TITLE = 'Total SFAv2 vs. Observed Snowfall!C' + $
        dates_str + ' ' + $
        '(reporting frequency !Z(2265) ' + STRCRA(percentThresh) + '%)', $
        POS = [0.1, 0.1, 0.95, 0.9], $
        XRANGE = xRange, XSTYLE = 1, $
        XTITLE = 'Total Observed Snowfall (inches)', $
        YRANGE = yRange, YSTYLE = 1, $
        YTITLE = 'Total Snowfall Analysis (inches)', $
        /NODATA


; Identify stations with the largest discrepancies.

  intSlp = LINFIT(x, y, YFIT = f)
  PLOTS, !X.CRange, $
         [intSlp[0] + intSlp[1] * !X.CRange[0], $
          intSlp[0] + intSlp[1] * !X.CRange[1]], $
         COLOR = 200, LINESTYLE = 2
  PLOTS, !X.CRange, $
         !X.CRange, $
         COLOR = 200
  rSquared = 1.0 - $
             TOTAL((y - f)^2.0) / $
             TOTAL((y - MEAN(y))^2.0)
  PRINT, 'Correlation (check): ', SQRT(rSquared) ; should be same as corr

  error = y - x
  rse = SQRT(error^2.0)
  order = REVERSE(SORT(rse))

  csvFile = 'compare_agg_sfav2_' + startDate_YYYYMMDDHH + '_to_' + $
            finishDate_YYYYMMDDHH + '_' + $
            STRING(percentThresh, FORMAT = '(I3.3)') + '_percent_reports.csv'

; Write data to CSV file, and plot and print results for the sites
; having the largest values of rse.

  OPENW, lun, csvFile, /GET_LUN
  ;DEVICE, SET_FONT = 'Helvetica-Oblique'

  numNames = 0
  for oc = 0, count2 - 1 do begin

      ;; PRINT, snowfallReport[ind[ind2[order[oc]]]].station_id, ' ', $
      ;;        ;snowfallReport[ind[ind2[order[oc]]]].station_name, ' ', $
      ;;        snowfallReport[ind[ind2[order[oc]]]].longitude, ' ', $
      ;;        snowfallReport[ind[ind2[order[oc]]]].latitude, ' ', $
      ;;        reportObsTotal[ind[ind2[order[oc]]]] * 39.3701, ' ', $
      ;;        reportAnlTotal[ind[ind2[order[oc]]]], ' ', $
      ;;        error[order[oc]]

      obs = reportObsTotal[ind[ind2[order[oc]]]] * 39.3701
      anl = reportAnlTotal[ind[ind2[order[oc]]]]

;      if (obs gt 180.0) then begin

      ;; if (((oc lt 20) or $
      ;;      (oc lt 200) and (obs gt 180.0)) or $
      ;;     ((oc gt 200) and (obs gt 180.0))) then begin

      ;; if ((oc lt 20) or $
      ;;     ((numNames lt 100) and $
      ;;      (reportObsTotal[ind[ind2[order[oc]]]] * 39.3701 gt 180.0))) $
      ;;   then begin

      worstCount = 20
      if (oc lt worstCount) then $
          textColor = redInd $
      else $
          textColor = blackInd

          ;; if (oc lt 200) then $
          ;;     textColor = redInd $
          ;; else $
          ;;     textColor = blackInd

          ;; if (oc lt 100) then $
          ;;     textColor = redInd $
          ;; else $
          ;;     textColor = blackInd

      xGap = 0.004 * (!X.CRange[1] - !X.CRange[0])
      yGap = 0.004 * (!Y.CRange[1] - !Y.CRange[0])
      if ((reportObsTotal[ind[ind2[order[oc]]]] * 39.3701 lt xRange[1]) and $
          (reportAnlTotal[ind[ind2[order[oc]]]] lt yRange[1])) then begin
          PLOTS, reportObsTotal[ind[ind2[order[oc]]]] * 39.3701, $
                 reportAnlTotal[ind[ind2[order[oc]]]], $
                 COLOR = 0, PSYM = 4, SYMSIZE = 0.5
          if (textColor eq redInd) then $
              XYOUTS, reportObsTotal[ind[ind2[order[oc]]]] * 39.3701 + xGap, $
                      reportAnlTotal[ind[ind2[order[oc]]]] + yGap, $
                      snowfallReport[ind[ind2[order[oc]]]].station_id, $
                      SIZE = 0.5, COLOR = textColor
      endif
          ;; PLOTS, [reportObsTotal[ind[ind2[order[oc]]]] * 39.3701, $
          ;;         reportObsTotal[ind[ind2[order[oc]]]] * 39.3701], $
          ;;        [reportAnlTotal[ind[ind2[order[oc]]]], $
          ;;         reportObsTotal[ind[ind2[order[oc]]]] * 39.3701 * intSlp[1] + $
          ;;         intSlp[0]]
;          numNames++
;      endif else begin
;          PLOTS, reportObsTotal[ind[ind2[order[oc]]]] * 39.3701, $
;                 reportAnlTotal[ind[ind2[order[oc]]]], $
;                 COLOR = 0, PSYM = 4, SYMSIZE = 0.5
;      endelse

      PRINTF, lun, $
              STRCRA(snowfallReport[ind[ind2[order[oc]]]].longitude) + ',' + $
              STRCRA(snowfallReport[ind[ind2[order[oc]]]].latitude) + ',' + $
              STRCRA(reportObsTotal[ind[ind2[order[oc]]]] * 39.3701) + ',' + $
              STRCRA(reportAnlTotal[ind[ind2[order[oc]]]]) + ',' + $
              STRCRA(error[order[oc]]) + ',' + $
              STRCRA(snowfallReport[ind[ind2[order[oc]]]].station_id)

  endfor

  startX = 0.7
  finishX = 0.76
  pad = 0.005
  startY = 0.42
  gapY = 0.025
  ;DEVICE, SET_FONT = 'Helvetica'

  legend_text_size = 0.7

  x1 = startX
  y1 = startY
  XYOUTS, x1, y1, 'Station count: ' + $
          STRCRA(count2), $
          /NORMAL, SIZE = legend_text_size

  y1 = y1 - gapY

  XYOUTS, x1, y1, 'Mean Error: ' + FORMAT_FLOAT(MEAN(error)) + ' inches', $
          /NORMAL, SIZE = legend_text_size

  ;; y1 = y1 - gapY
  ;; XYOUTS, x1, y1, 'Error Std. Dev.: ' + $
  ;;         STRCRA(STDDEV(error)), /NORMAL, SIZE = legend_text_size

  y1 = y1 - gapY
  XYOUTS, x1, y1, 'RMSE: ' + FORMAT_FLOAT(SQRT(MEAN(error^2.0))) + ' inches', $
          /NORMAL, SIZE = legend_text_size

  y1 = y1 - gapY
  XYOUTS, x1, y1, 'MAE: ' + FORMAT_FLOAT(MEAN(ABS(error))) + ' inches', $
          /NORMAL, SIZE = legend_text_size

  y1 = y1 - gapY
  XYOUTS, x1, y1, 'Correlation: ' + FORMAT_FLOAT(corr), /NORMAL, $
          SIZE = legend_text_size

  ;; y1 = y1 - gapY
  ;; XYOUTS, x1, y1, 'Rank Correlation: ' + FORMAT_FLOAT(rCorr[0]), /NORMAL, $
  ;;         SIZE = legend_text_size

  y1 = y1 - gapY
  XYOUTS, x1, y1, 'Bias (Total Snowfall): ' + FORMAT_FLOAT(bias), /NORMAL, $
          SIZE = legend_text_size

  y1 = y1 - gapY
  XYOUTS, x1, y1, 'Bias (Fitted Slope): ' + FORMAT_FLOAT(intSlp[1]), /NORMAL, $
          SIZE = legend_text_size

  y1 = y1 - gapY
  XYOUTS, x1, y1, 'Bias (Geometric Mean): ' + FORMAT_FLOAT(gmBias), /NORMAL, $
          SIZE = legend_text_size

  y1 = y1 - gapY + pad
  x2 = finishX
  PLOTS, [x1, x2], [y1, y1], LINESTYLE = 2, COLOR = 200, /NORMAL

  y1 = y1 - pad
  x1 = x2 + pad
  XYOUTS, x1, y1, 'Fitted Line', /NORMAL, SIZE = legend_text_size

  x1 = startX
  x2 = finishX
  y1 = y1 - gapY + pad
  PLOTS, [x1, x2], [y1, y1], COLOR = 200, /NORMAL

  y1 = y1 - pad
  x1 = x2 + pad
  XYOUTS, x1, y1, 'Y = X Line', /NORMAL, SIZE = legend_text_size
  ;DEVICE, SET_FONT = 'Helvetica-Oblique'

  x1 = startX
  y1 = y1 - gapY
  XYOUTS, x1, y1, 'Site IDs for top ' + STRCRA(worstCount) + ' differences', $
          /NORMAL, SIZE = legend_text_size, COLOR = redInd
  ;; XYOUTS, x1, y1, 'High Snowfall + High Error Site ID', /NORMAL, $
  ;;         SIZE = legend_text_size, COLOR = redInd
  ;; y1 = y1 - gapY
  ;; XYOUTS, x1, y1, 'High Snowfall + Low Error Site ID', /NORMAL, $
  ;;         SIZE = legend_text_size

  FREE_LUN, lun
  DEVICE, /CLOSE
  SET_PLOT, oldDevice
  !P.Font = oldFont
  cmd = 'gs -q -sDEVICE=png16m -dSAFTER -dNOPAUSE -r600 ' + $
        '-sOutputFile=' + plotFile + '.png -I. -- ' + $
        plotFile + '.ps'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -scale "30%x30%" ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -trim -border 4% -bordercolor white ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP

end
