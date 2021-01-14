PRO READ_GHCND_US_CA_MX_BY_YEAR_SNFL_PRCP_TAIR, $
    GHCND_dir, $
    Year, $
    Ndv, $                      ; -9999 is a good idea here
    numDays, $
    numStations, $
    GHCNStationID, $
    GHCNData, $
    VERBOSE = verbose

;+
; Read "by_year" GHCN-Daily data.
;
; The source data for this procedure are "US_CA_MX" files, which are
; created by doing, e.g.,
; % grep "^US\|^CA\|^MX" 2014.csv > GHCN_US_CA_MX_2014.csv
;
; This version differs from read_ghcnd_us_ca_mx_by_year.pro in that
; the latter reads only PRCP and SNFL data, whereas this version reads
; TAVG, TMIN, and TMAX as well.
;
; Organize data into a [numDays, numStations] array of structures:
;
; elements:
;
;   prcp = observed precipitation (tenths of mm)
;   snow = observed snowfall (mm)
;   tmin = minimum temperature (tenths of degrees Celsius)
;   tmax = maximum temperature (tenths of degrees Celsius)
;   tavg = average temperature (tenths of degrees Celsius)
;   [var_name]_mflag - measurement flag for observation (see below)
;   [var_name]_qflag - quality flag for observation (see below)
;   [var_name]_sflag - source flag for observation (see below)
;   [var_name]_time_HHMM - local time of observation
;
; mflag is the measurement flag, having ten possible values. This flag
;       may be present only for the 1st of the month
;
;   blank = no information
;   B = precipitation total formed from two 12-hour totals
;   D = precipitation total formed from four six-hour totals
;   H = represents highest or lowest hourly temperature (TMAX or TMIN)
;       or the average of hourly values (TAVG) 
;   K = converted from knots 
;   L = temperature appears to be lagged with respect to reported hour
;       of observation
;   O = converted from oktas 
;   P = identified as "missing presumed zero" in DSI 3200 and 3206
;   T = trace of precipitation, snowfall, or snow depth
;   W = converted from 16-point WBAN code (for wind direction)
;
; qflag is the quality flag for the first day of the month.  There are 
;       fourteen possible values:
;
;   blank = did not fail any quality assurance check
;   D = failed duplicate check
;   G = failed gap check
;   I = failed internal consistency check
;   K = failed streak/frequent-value check
;   L = failed check on length of multiday period 
;   M = failed megaconsistency check
;   N = failed naught check
;   O = failed climatological outlier check
;   R = failed lagged range check
;   S = failed spatial consistency check
;   T = failed temporal consistency check
;   W = temperature too warm for snow
;   X = failed bounds check
;   Z = flagged as a result of an official Datzilla 
;       investigation
;
; sflag is the source flag for the first day of the month.  There are 
;       twenty nine possible values (including blank, upper and lower
;       case letters):
;
;   blank = no source (i.e., data value missing)
;   0 = U.S. Cooperative Summary of the Day (NCDC DSI-3200)
;   6 = CDMP Cooperative Summary of the Day (NCDC DSI-3206)
;   7 = U.S. Cooperative Summary of the Day -- Transmitted via
;       WxCoder3 (NCDC DSI-3207)
;   A = U.S. Automated Surface Observing System (ASOS) real-time data
;       (since January 1, 2006)
;   a = Australian data from the Australian Bureau of Meteorology
;   B = U.S. ASOS data for October 2000-December 2005 (NCDC DSI-3211)
;   b = Belarus update
;   C = Environment Canada
;   E = European Climate Assessment and Dataset (Klein Tank et al., 2002)
;   F = U.S. Fort data 
;   G = Official Global Climate Observing System (GCOS) or other
;       government-supplied data
;   H = High Plains Regional Climate Center real-time data
;   I = International collection (non U.S. data received through
;       personal contacts)
;   K = U.S. Cooperative Summary of the Day data digitized from paper
;       observer forms (from 2011 to present)
;   M = Monthly METAR Extract (additional ASOS data)
;   N = Community Collaborative Rain, Hail,and Snow (CoCoRaHS)
;   Q = Data from several African countries that had been
;       "quarantined", that is, withheld from public release until
;       permission was granted from the respective meteorological
;       services
;   R = NCEI Reference Network Database (Climate Reference Network and
;       Regional Climate Reference Network)
;   r = All-Russian Research Institute of Hydrometeorological
;       Information-World Data Center
;   S = Global Summary of the Day (NCDC DSI-9618)
;       NOTE: "S" values are derived from hourly synoptic reports
;       exchanged on the Global Telecommunications System (GTS). Daily
;       values derived in this fashion may differ significantly from
;       "true" daily data, particularly for precipitation (i.e., use
;       with caution).
;   s = China Meteorological Administration/National Meteorological
;       Information Center/Climatic Data Center
;       (http://cdc.cma.gov.cn)
;   T = SNOwpack TELemtry (SNOTEL) data obtained from the
;       U.S. Department of Agriculture's Natural Resources
;       Conservation Service
;   U = Remote Automatic Weather Station (RAWS) data obtained from the
;       Western Regional Climate Center
;   u = Ukraine update          
;   W = WBAN/ASOS Summary of the Day from NCDC's Integrated
;       Surface Data (ISD).
;   X = U.S. First-Order Summary of the Day (NCDC DSI-3210)
;   Z = Datzilla official additions or replacements 
;   z = Uzbekistan update
;
;   When data are available for the same time from more than one
;   source, the highest priority source is chosen according to the
;   following priority order (from highest to lowest):
;   Z,R,0,6,C,X,W,K,7,F,B,M,r,E,z,u,b,s,a,G,Q,I,A,N,T,U,H,S
;-
  YYYY = STRING(Year, FORMAT = '(I4.4)')
  if IS_LEAP_YEAR(Year) then numDays_ = 366 else numDays_ = 365

  GHCNFile = 'GHCN_US_CA_MX_' + YYYY + '.csv'
  saveFile = 'GHCN_US_CA_MX_SNFL_PRCP_TAIR_' + YYYY + '.sav'

  if FILE_TEST(GHCND_dir + '/by_year/' + saveFile) then begin

;+
;     Get the data from the IDL save file.
;-
      ndv_ = TEMPORARY(Ndv) 
      RESTORE, GHCND_dir + '/by_year/' + saveFile

;+
;     Verify contents of the IDL save file.
;-
      if NOT(ISA(startDate_YYYYMMDD)) then STOP
      if NOT(ISA(finishDate_YYYYMMDD)) then STOP
      if NOT(ISA(numDays)) then STOP
      if NOT(ISA(numStations)) then STOP
      if NOT(ISA(GHCNStationID)) then STOP
      if NOT(ISA(GHCNData)) then STOP
      if NOT(ISA(Ndv)) then STOP

      if (Ndv ne ndv_) then STOP
      if (startDate_YYYYMMDD ne YYYY + '0101') then STOP
      if (finishDate_YYYYMMDD ne YYYY + '1231') then STOP
      if (numDays ne numDays_) then STOP
      foo = SIZE(GHCNStationID)
      if (foo[0] ne 1) then STOP
      if (foo[1] ne numStations) then STOP
      if (foo[2] ne 7) then STOP ; STRING
      foo = SIZE(GHCNData)
      if (foo[0] ne 2) then STOP
      if (foo[1] ne numDays) then STOP
      if (foo[2] ne numStations) then STOP
      if (foo[3] ne 8) then STOP ; STRUCTURE
      tagNames = TAG_NAMES(GHCNData)
      if (N_ELEMENTS(tagNames) lt 25) then STOP
      if (N_ELEMENTS(tagNames) gt 25) then $
          MESSAGE, 'WARNING: GHCNData structure in ' + $
                   GHCND_dir + '/by_year/' + saveFile + $
                   ' has >25 tags.', /CONTINUE
      ind = WHERE(tagNames eq 'PRCP', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'PRCP_MFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'PRCP_QFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'PRCP_SFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'PRCP_TIME_HHMM', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'SNOW', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'SNOW_MFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'SNOW_QFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'SNOW_SFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'SNOW_TIME_HHMM', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMIN', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMIN_MFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMIN_QFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMIN_SFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMIN_TIME_HHMM', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMAX', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMAX_MFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMAX_QFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMAX_SFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TMAX_TIME_HHMM', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TAVG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TAVG_MFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TAVG_QFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TAVG_SFLAG', count)
      if (count eq 0) then STOP
      ind = WHERE(tagNames eq 'TAVG_TIME_HHMM', count)
      if (count eq 0) then STOP

      if KEYWORD_SET(verbose) then $
          PRINT, 'File ' + GHCND_dir + '/by_year/' + saveFile + $
                 ' has data for ' + $
                 STRCRA(numStations) + ' unique observing stations.'

      ;; RETURN

  endif else begin

;+
;     Read the data from the GHCNFile.
;-
      if NOT(FILE_TEST(GHCND_dir + '/by_year/' + GHCNFile)) then $
          MESSAGE, 'ERROR: GHCND file ' + $
                   GHCND_dir + '/by_year/' + GHCNFile + $
                   ' not found.'

      startDate_YYYYMMDD = YYYY + '0101'
      startDate_Julian = YYYYMMDDHH_TO_JULIAN(startDate_YYYYMMDD + '00')
      finishDate_YYYYMMDD = YYYY + '1231'
      finishDate_Julian = YYYYMMDDHH_TO_JULIAN(finishDate_YYYYMMDD + '00')
      numDays = ROUND(finishDate_Julian - startDate_Julian + 1.0D)
      if (numDays ne numDays_) then STOP
      
;+
;     Count the number of unique stations in the file using shell
;     utilities.
;-
      cmd = 'cat ' + GHCND_dir + '/by_year/' + GHCNFile + $
            ' | awk -F '','' ''{print $1}'' | sort | uniq | wc -l'
      if KEYWORD_SET(verbose) then PRINT, cmd
      SPAWN, cmd, output, EXIT_STATUS = status
      if (status ne 0) then STOP
      numStations = LONG(output[0])
      if KEYWORD_SET(verbose) then $
          PRINT, 'File ' + GHCND_dir + '/by_year/' + GHCNFile + $
                 ' has data for ' + $
                 STRCRA(numStations) + ' unique observing stations.'

;+
;     Set up the GHCN data structure.
;-
      report = {GHCN, $
                prcp: Ndv, $    ; precipitation (tenths of mm)
                prcp_mflag: '', $
                prcp_qflag: '', $
                prcp_sflag: '', $
                prcp_time_HHMM: '', $
                snow: Ndv, $    ; snowfall (mm)
                snow_mflag: '', $
                snow_qflag: '', $
                snow_sflag: '', $
                snow_time_HHMM: '', $
                tmin: Ndv, $    ; minimum daily temperature (tenths of deg C)
                tmin_mflag: '', $
                tmin_qflag: '', $
                tmin_sflag: '', $
                tmin_time_HHMM: '', $
                tmax: Ndv, $    ; maximum daily temperature (deg C)
                tmax_mflag: '', $
                tmax_qflag: '', $
                tmax_sflag: '', $
                tmax_time_HHMM: '', $
                tavg: Ndv, $    ; average daily temperature (deg C)
                tavg_mflag: '', $
                tavg_qflag: '', $
                tavg_sflag: '', $
                tavg_time_HHMM: ''}

      GHCNData = REPLICATE(report, numDays, numStations)
      GHCNStationID = REPLICATE('', numStations)

;+
;     Read the file.
;-
      OPENR, lun, GHCND_dir + '/by_year/' + GHCNFile, /GET_LUN
      lastID = ''
      line = ''
      lastDate_YYYYMMDD = ''
      idCount = -1L             ; index of previous unique station ID
      while NOT(EOF(lun)) do begin
          READF, lun, line
          fields = STRSPLIT(line, ',', /EXTRACT, /PRESERVE_NULL)
          if (N_ELEMENTS(fields) ne 8) then STOP
          id = fields[0]
          date_YYYYMMDD = fields[1]
          if (date_YYYYMMDD ne lastDate_YYYYMMDD) then begin
              if KEYWORD_SET(verbose) then $
                  PRINT, 'Getting data for ' + date_YYYYMMDD
              lastDate_YYYYMMDD = date_YYYYMMDD
          endif
          date_Julian = YYYYMMDDHH_TO_JULIAN(date_YYYYMMDD + '00')
          dayInd = ROUND(date_Julian - startDate_Julian) ; date index
          if (dayInd ge numDays) then STOP
          idInd = WHERE(GHCNStationID eq id, count)
          if (count eq 0) then begin
              idCount++
              GHCNStationID[idCount] = id
              idInd = idCount
          endif else begin
              if (count ne 1) then STOP
              idInd = idInd[0]
          endelse
          element = fields[2]
          if ((element ne 'PRCP') and $
              (element ne 'SNOW') and $
              (element ne 'TMIN') and $
              (element ne 'TMAX') and $
              (element ne 'TAVG')) then CONTINUE
          value = fields[3]
          mFlag = fields[4]     ; measurement flag
          qFlag = fields[5]     ; quality flag
          sFlag = fields[6]     ; source flag
          time_HHMM = fields[7]
          case element of
              'PRCP': begin
                  GHCNData[dayInd, idInd].prcp = LONG(value)
                  GHCNData[dayInd, idInd].prcp_mflag = mFlag
                  GHCNData[dayInd, idInd].prcp_qflag = qFlag
                  GHCNData[dayInd, idInd].prcp_sflag = sFlag
                  GHCNData[dayInd, idInd].prcp_time_HHMM = time_HHMM
              end
              'SNOW': begin
                  GHCNData[dayInd, idInd].snow = LONG(value)
                  GHCNData[dayInd, idInd].snow_mflag = mFlag
                  GHCNData[dayInd, idInd].snow_qflag = qFlag
                  GHCNData[dayInd, idInd].snow_sflag = sFlag
                  GHCNData[dayInd, idInd].snow_time_HHMM = time_HHMM
              end
              'TMIN': begin
                  GHCNData[dayInd, idInd].tmin = LONG(value)
                  GHCNData[dayInd, idInd].tmin_mflag = mFlag
                  GHCNData[dayInd, idInd].tmin_qflag = qFlag
                  GHCNData[dayInd, idInd].tmin_sflag = sFlag
                  GHCNData[dayInd, idInd].tmin_time_HHMM = time_HHMM
              end
              'TMAX': begin
                  GHCNData[dayInd, idInd].tmax = LONG(value)
                  GHCNData[dayInd, idInd].tmax_mflag = mFlag
                  GHCNData[dayInd, idInd].tmax_qflag = qFlag
                  GHCNData[dayInd, idInd].tmax_sflag = sFlag
                  GHCNData[dayInd, idInd].tmax_time_HHMM = time_HHMM
              end
              'TAVG': begin
                  GHCNData[dayInd, idInd].tavg = LONG(value)
                  GHCNData[dayInd, idInd].tavg_mflag = mFlag
                  GHCNData[dayInd, idInd].tavg_qflag = qFlag
                  GHCNData[dayInd, idInd].tavg_sflag = sFlag
                  GHCNData[dayInd, idInd].tavg_time_HHMM = time_HHMM
              end
              else: STOP        ; programming error
          endcase
      endwhile

      FREE_LUN, lun

;+
;     Store the results.
;-
      SAVE, startDate_YYYYMMDD, $
            finishDate_YYYYMMDD, $
            numDays, $
            numStations, $
            GHCNStationID, $
            GHCNData, $
            Ndv, $
            FILE = GHCND_dir + '/by_year/' + saveFile

  endelse
      
  if KEYWORD_SET(verbose) then begin

;+
;     Inventory data and report results.
;-
      allNonNDVInd = WHERE((GHCNData.prcp ne ndv) and $
                           (GHCNData.snow ne ndv) and $
                           (GHCNData.tmin ne ndv) and $
                           (GHCNData.tmax ne ndv) and $
                           (GHCNData.tavg ne ndv), $
                            allNonNDVCount)
      if (allNonNDVCount eq 0) then STOP
      allNonNDVDayInd = allNonNDVInd mod numDays 
      allNonNDVStationInd = allNonNDVInd / numDays
      order = SORT(allNonNDVStationInd)
      orderedStations = allNonNDVStationInd[order]
      uniqueStations = orderedStations[UNIQ(orderedStations)]
      order = -1L
      orderedStations = -1L
      numUniqueStations = N_ELEMENTS(uniqueStations)
      PRINT, 'Examining cases where all of ' + $
             'PRCP, SNOW, TMIN, TMAX, and TAVG ' + $
             'are reported.'

      PRINT, 'Concurrent reports exist for ' + $
             STRCRA(numUniqueStations) + ' out of ' + $
             STRCRA(numStations) + ' stations.'

;+  
;     Loop over days. For each day record the number of concurrent
;     reports.
;-
      PRINT, '   date   | all quantities | PRCP and SNOW | ' + $
             'PRCP, SNOW, TMIN | PRCP, SNOW, TAVG | ' + $
             'PRCP, SNOW, TMIN, TAVG'
      PRINT, '----------|----------------|---------------|-' + $
             '-----------------|------------------|-' + $
             '----------------------'

      startDate_Julian = YYYYMMDDHH_TO_JULIAN(startDate_YYYYMMDD + '00')

      for dc = 0, numDays - 1 do begin

          date_Julian = startDate_Julian + DOUBLE(dc)
          date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
          date_YYYYMMDD = STRMID(date_YYYYMMDDHH, 0, 8)

          dailyData = REFORM(GHCNData[dc, *])

          allInd = WHERE((dailyData.prcp ne Ndv) and $
                         (dailyData.snow ne Ndv) and $
                         (dailyData.tmin ne Ndv) and $
                         (dailyData.tmax ne Ndv) and $
                         (dailyData.tavg ne Ndv), $
                         allCount)
          psInd = WHERE((dailyData.prcp ne Ndv) and $
                        (dailyData.snow ne Ndv), $
                         psCount)
          pstnInd = WHERE((dailyData.prcp ne Ndv) and $
                          (dailyData.snow ne Ndv) and $
                          (dailyData.tmin ne Ndv), $
                          pstnCount)
          pstvInd = WHERE((dailyData.prcp ne Ndv) and $
                          (dailyData.snow ne Ndv) and $
                          (dailyData.tavg ne Ndv), $
                          pstvCount)
          psttInd = WHERE((dailyData.prcp ne Ndv) and $
                          (dailyData.snow ne Ndv) and $
                          (dailyData.tmin ne Ndv) and $
                          (dailyData.tavg ne Ndv), $
                          psttCount)
          PRINT, ' ' + date_YYYYMMDD + ' | ' + $
                 STRING(allCount, FORMAT = '(I14)') + ' | ' + $
                 STRING(psCount, FORMAT = '(I13)') + ' | ' + $
                 STRING(pstnCount, FORMAT = '(I16)') + ' | ' + $
                 STRING(pstvCount, FORMAT = '(I16)') + ' | ' + $
                 STRING(psttCount, FORMAT = '(I22)')

      endfor

  endif

  RETURN

end
