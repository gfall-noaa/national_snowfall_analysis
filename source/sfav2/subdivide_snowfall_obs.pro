PRO SUBDIVIDE_SNOWFALL_OBS, StartDate_YYYYMMDDHH, $
                            FinishDate_YYYYMMDDHH, $
                            MinLon, $
                            MaxLon, $
                            MinLat, $
                            MaxLat, $
                            TargetFcstHour, $
                            MinSubFcstHour, $
                            MaxSubFcstHour, $
                            ;; PrecipSource, $ ; gf2019 may not need
                            ;; PrecipDir, $ ; gf2019 may not need
                            NSAPrefix, $
                            ArchiveDir, $
                            ScratchDir, $
                            NoDataValue, $
                            PGHost, $
                            WebPGHost, $
                            subSnowfallReport, $
                            RAW_SNOWFALL_DATA = snowfallReport, $
                            AVOID_OVERLAP = avoidOverlap, $
                            VERBOSE = verbose
;+
; Subdivide multi-hour snowfall observations into hourly amounts,
; using short-term snow accumulation from NWP models to determine the
; normalizing "curve."

; For each supported duration, move through the time window,
; subdividing reports using HRRR, RAP, or RUC precipitation.

; Observations for later dates will overwrite observations for earlier
; ones, when there is overlap.

; Supported durations in hours for snowfall observations are given in
; a hard-coded array below. Observations for durations appearing later
; in this array will overwrite those for durations appearing
; earlier. The order of the values in the duration array will thereby
; prioritize some durations (listed later) over others (listed
; earlier). Therefore, durations associated with routine reliable
; reports, such as 6, 12, and 24 hours, are listed last in the
; duration array.

; HRRR, RAP, or RUC water equivalent of accumulated snow depth
; (WEASD), sampled at station locations, is used to subdivide
; reports.

; Two categories of conflict may arise:
;
;   1. If the observed snowfall is zero and HRRR has nonzero WEASD
;      over the duration of the observation, then the observed
;      snowfall is subdivided into zeroes, ignoring the conflict and
;      deferring to the observation.
;
;   2. If the observed snowfall is nonzero and HRRR has zero WEASD
;      throughout the duration of the observation, three alternatives
;      are available to provide for subdivision:
;
;      a. First, a reduced-resolution resampling of HRRR WEASD from 3
;         km to 15 km is considered. If this does not provide nonzero
;         WEASD...
;
;      b. The HRRR total precipitation (APCP) record is subdivided
;         instead. If HRRR has zero APCP throughout the duration of
;         the observation, then...
;
;      c. A reduced resolution resampling of HRRR APCP from 3 km to 15
;         km is considered.
;
;      If none of these alternatives provides nonzero model
;      precipitation for subdividing the observation, then the
;      observation is ignored. In this manner, it is possible for a
;      nonzero snowfall report to be effectively "vetoed" by HRRR,
;      RAP, or RUC.
;
; :Author:
;
;     Greg Fall, Office of Water Prediction, Chanhassen, MN USA
;
;      United States Department of Commerce;
;      National Oceanic and Atmospheric Administration (NOAA);
;      National Weather Service (NWS);
;      Office of Water Prediction (OWP)
;
; :History:
;
;     Version 1.0, March 2017
;
;     2018-07-25, Added support for subdivision using RAP and RUC QPF,
;                 and added a section to verify that QPF is present
;                 before reading the observations.
;     2019-08-02, Modified "dry run" section to give this program the
;                 ability to find its own QPF source and location.
;                 Taking PrecipSource and PrecipDir from the caller
;                 does not always work.
;
; :Params:
;
;     StartDate_YYYYMMDDHH : in, required, type=STRING
;         The start of the first hour for which snowfall accumulation
;         data is sought, in UTC, in the form YYYYMMDDHH.
;
;     FinishDate_YYYYMMDDHH : in, required, type=STRING
;         The end of the last hour for which snowfall accumulation
;         data is sought, in UTC, in the form YYYYMMDDHH.
;     MinLon : in, required, type=DOUBLE
;         The minimum station longitude to include, in degrees.
;     MaxLon : in, required, type=DOUBLE
;         The maximum station longitude to include, in degrees.
;     MinLat : in, required, type=DOUBLE
;         The minimum station latitude to include, in degrees.
;     MaxLat : in, required, type=DOUBLE
;         The maximum station latitude to include, in degrees.
;     TargetFcstHour : in, required, type=INT
;         The forecast hour that is sought for all QPF data.
;     MinSubFcstHour : in, required, type=INT
;         The minimum forecast hour to substitute in place of
;         TargetFcstHour, if QPF for TargetFcstHour is unavailable.
;     MaxSubFcstHour : in, required, type=INT
;         The maximum forecast hour to substitute in place of
;         TargetFcstHour, if QPF for TargetFcstHour is unavailable.
;     NSAPrefix : in, required, type=STRING
;         The location of the operations cache for SNODAS forcing
;         data, which includes short term NWP QPF, specifically (as of
;         summer 2019) Rapid Refresh (RAP) and High Resolution Rapid
;         Refresh (HRRR).
;         Traditionally, this is '/operations'
;     ArchiveDir : in, required, type=STRING
;         The location of archived Rapid Update Cycle (RUC), Rapid
;         Refresh (RAP), and High Resolution Rapid Refresh (HRRR)
;         data. Traditionally, this is '/nwcdev/archive'
;     ScratchDir : in, required, type=STRING
;         The directory where temporary files generated by this
;         procedure and the procedures it calls are stored.
;     NoDataValue : in, required, type=FLOAT
;         The value to use for missing/no-data.
;     PGHost : in, required, type=STRING
;         The location of the "operations" database; either ddb0
;         (NOHRSC development) or odb0 (NOHRSC operations). This is
;         where near-real-time (< 30 days) observation data are
;         stored.
;     WebPGHost : in, required, type=STRING
;         The location of the "operations" database; either ddb0
;         (NOHRSC development) or odb0 (NOHRSC operations). This is
;         where observation data are permanently stored.
;     subSnowfallReport : out, type=STRUCT
;         A named variable that will capture the subdivided snowfall
;         data.
;         Structure tags:
;
;          station_id (type=STRING):
;          A unique string identifying each station.
;
;          station_name (type=STRING):
;          The long name of each station.
;
;          station_type (type=STRING):
;          The station type of each station.
;
;          longitude (type=DOUBLE):
;          The longitude of each station location, degrees.
;
;          latitude (type=DOUBLE):
;          The latitude of each station location, degrees.
;
;          elevation (type=LONG):
;          The elevation of each station location, meters. Watch out
;          for ambiguous no-data values, usually -9999.
;
;          value_meters (type=FLTARR(# hours))
;          Subdivided snowfall observations, in meters, for each
;          station, for each hour.
;
;          revision_history (type=STRING)
;          A string describing the process of subdivision as all
;          durations and hours of observations are examined.
;          "N" = NoDataValue occurs in HRRR WEASD for station.
;          "Z" = A zero snowfall observation was subdivided into
;                zeroes.
;          "W" = A nonzero snowfall observation was subdivided using
;                WEASD.
;          "w" = A nonzero snowfall observation was subdivided using
;                reduced resolution WEASD.
;          "A" = A nonzero snowfall observation was subdivided using
;                APCP.
;          "a" = A nonzero snowfall observation was subdivided using
;                reduced resolution APCP.
;          "F" = A nonzero snowfall observation could not be
;                subdivided.
;          "P" = For hours preceding the period covering a nonzero
;                snowfall observation, no-data values were replaced
;                with zeroes because zero WEASD occurred for those
;                hours.
;          "L" = After all observations for a station have been
;                subdivided, missing values were replaced with zeroes
;                because zero WEASD occurred for those hours.
;
; :Keywords:
;
;     RAW_SNOWFALL_DATA : type=STRUCT Array(# stations)
;         A named variable that will capture the raw snowfall data.
;         Structure tags:
;         
;          station_id (type=STRING):
;          A unique string identifying each station.
;
;          station_name (type=STRING):
;          The long name of each station.
;
;          station_type (type=STRING):
;          The station type of each station.
;
;          longitude (type=DOUBLE):
;          The longitude of each station location, degrees.
;
;          latitude (type=DOUBLE):
;          The latitude of each station location, degrees.
;
;          elevation (type=LONG):
;          The elevation of each station location, meters. Watch out
;          for ambiguous no-data values, usually -9999.
;
;          value_meters (type=FLTARR(# durations, # hours))
;          Row snowfall observations, in meters, for each
;          station, for each duration, for each hour.
;
;     AVOID_OVERLAP : in, type=BOOLEAN
;          When, for a given station, observations from different
;          times overlap in the time dimension, if this keyword is
;          set, only a small amount of that overlap is tolerated for
;          most stations, and no overlap at all is tolerated for NWS
;          spotter sites--these are notorious for providing
;          overlapping reports that conflict with each other in ways
;          that are impossible to resolve.
;
;     VERBOSE : in, type=BOOLEAN
;         Activates the "verbose" option, causing this procedure to
;         produce some extra output text.
;
;-
  COMMON info, Message ; used by USR_MSG and ERR_MSG

  subSnowfallReport = !NULL
  snowfallReport = !NULL


;===================;
; 1. Initial setup. ;
;===================;

;+
; Check arguments for valid type and contents.
;-
  if NOT(ISA(StartDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Start date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(StartDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid start date/time "' + $
               StartDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(StartDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid start date/time "' + $
               StartDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  if NOT(ISA(FinishDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Finish date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(FinishDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid finish date/time "' + $
               FinishDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(FinishDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid finish date/time "' + $
               FinishDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  startDate_Julian = YYYYMMDDHH_TO_JULIAN(StartDate_YYYYMMDDHH)
  finishDate_Julian = YYYYMMDDHH_TO_JULIAN(FinishDate_YYYYMMDDHH)

  numHours = ROUND((finishDate_Julian - startDate_Julian) * 24.0D)
  if (numHours lt 6) then begin
      ERR_MSG, 'Time window must be at least 6 hours long.'
      RETURN
  endif

  if NOT(ISA(MinLon, 'DOUBLE')) then $
      minLon_ = DOUBLE(MinLon) $
  else $
      minLon_ = MinLon

  if NOT(ISA(MaxLon, 'DOUBLE')) then $
      maxLon_ = DOUBLE(MaxLon) $
  else $
      maxLon_ = MaxLon

  if NOT(ISA(MinLat, 'DOUBLE')) then $
      minLat_ = DOUBLE(MinLat) $
  else $
      minLat_ = MinLat

  if NOT(ISA(MaxLat, 'DOUBLE')) then $
      maxLat_ = DOUBLE(MaxLat) $
  else $
      maxLat_ = MaxLat

  if ((TargetFcstHour lt 1) or $
      (MinSubFcstHour lt 1) or $
      (MaxSubFcstHour lt 1)) then begin
      ERR_MSG, 'All forecast hour options must be greater than zero.'
      RETURN
  endif

  if (MinSubFcstHour gt MaxSubFcstHour) then begin
      ERR_MSG, 'Minimum substitute forecast hour may not be larger than ' + $
               'maximum substitute forecast hour.'
      RETURN
  endif

  ;; if NOT(ISA(PrecipSource, 'STRING')) then begin
  ;;     ERR_MSG, 'Precipitation source must be a STRING.'
  ;;     RETURN
  ;; endif

  ;; if ((PrecipSource ne 'HRRR') and $
  ;;     (PrecipSource ne 'RAP') and $
  ;;     (PrecipSource ne 'RUC')) then begin
  ;;     ERR_MSG, 'Precipitation source must be "HRRR", "RAP", or "RUC".'
  ;;     RETURN
  ;; endif

  ;; if NOT(ISA(PrecipDir, 'STRING')) then begin
  ;;     ERR_MSG, 'Location of ' + PrecipSource + ' archive must be a STRING.'
  ;;     RETURN
  ;; endif

  ;; if NOT(FILE_TEST(PrecipDir, /DIRECTORY)) then begin
  ;;     ERR_MSG, PrecipSource + ' archive directory "' + $
  ;;              PrecipDir + '" not found.'
  ;;     RETURN
  ;; endif

  ;; if NOT(FILE_TEST(PrecipDir, /READ)) then begin
  ;;     ERR_MSG, PrecipSource + ' archive directory "' + $
  ;;              PrecipDir + '" not readable.'
  ;;     RETURN
  ;; endif

  if NOT(ISA(NSAPrefix, 'STRING')) then begin
      ERR_MSG, 'NSA prefix must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(NSAPrefix, /DIRECTORY)) then begin
      ERR_MSG, 'Directory "' + NSAPrefix + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(NSAPrefix, /READ)) then begin
      ERR_MSG, 'Directory "' + NSAPrefix + '" not readable.'
      RETURN
  endif

  if NOT(ISA(ArchiveDir, 'STRING')) then begin
      ERR_MSG, 'Archive directory location must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(ArchiveDir, /DIRECTORY, /READ)) then $
      archive_dir_visible = 0 $
  else $
      archive_dir_visible = 1

  if NOT(ISA(ScratchDir, 'STRING')) then begin
      ERR_MSG, 'Location of scratch directory must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(ScratchDir, /DIRECTORY)) then begin
      ERR_MSG, 'Scratch directory "' + ScratchDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(ScratchDir, /READ)) then begin
      ERR_MSG, 'Scratch directory "' + ScratchDir + '" not readable.'
      RETURN
  endif

  if NOT(FILE_TEST(ScratchDir, /WRITE)) then begin
      ERR_MSG, 'Scratch directory "' + ScratchDir + '" not writeable.'
      RETURN
  endif

  if NOT(ISA(NoDataValue, 'FLOAT')) then $
      ERR_MSG, 'WARNING: no-data value should be a floating point value.'

  if NOT(ISA(PGHost, 'STRING')) then begin
      ERR_MSG, 'PostgreSQL "PGHOST" argument must be a STRING.'
      RETURN
  endif

  if NOT(ISA(WebPGHost, 'STRING')) then begin
      ERR_MSG, 'PostgreSQL "WEBPGHOST" argument must be a STRING.'
      RETURN
  endif

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Subdividing snowfall observations from ' + $
               StartDate_YYYYMMDDHH + ' to ' + $
               FinishDate_YYYYMMDDHH + ' into hourly amounts.'

;+
; Define durations to support. These are arranged in order of their
; priority; i.e., data with later durations will be able to overwrite
; data with earlier durations, if both are provided for the same
; station.
;-
  duration = [2, 4, 5, 7, 8, 10, 11, 13, 14, 15, 16, 17, 19, 20, $
              21, 22, 23, $
              25 + INDGEN(23), $
              49 + INDGEN(23), $
              73 + INDGEN(23), $
              97 + INDGEN(24), $
              96, 72, 48, 18, 9, 3, 6, 12, 24]

  duration = [2, 4, 5, 7, 8, 10, 11, 13, 14, 15, 16, 17, 19, 20, $
              21, 22, 23, $
              25 + INDGEN(23), $
              48, 18, 9, 3, 6, 12, 24]

;+
; Compare time range with durations and remark on any durations that
; exceed the time range.
;-
  ind = WHERE(duration gt numHours, count)
  if ((count gt 0) and KEYWORD_SET(verbose)) then begin
      USR_MSG, 'WARNING: the following durations exceed the analysis ' + $
               'period and will not be considered:'
      USR_MSG, STRCRA(duration[ind])
  endif

  ind = WHERE(duration le numHours, count)
  if (count eq 0) then begin ; PROGRAMMING ERROR
      ERR_MSG, 'PROGRAMMING ERROR: no supported durations are shorter ' + $
               'than the ' + STRCRA(numHours) + '-hour analysis period.'
      RETURN
  endif
  duration = duration[ind]

;+
; Set up a structure for the data for all durations and times.
;-
  numDurations = N_ELEMENTS(duration)
  numTimes = numHours - MIN(duration) + 1L ; number of dates we need
                                           ; to fetch data for.

;+
; Guess at the number of stations the program will find. If this guess
; is low, the program will not use so much memory, but may be slowed
; by a lot of memory reallocation. If this guess is high, the program
; may chew up extra memory, but will run faster because of fewer
; reallocations. Any unneeded memory will be freed after all
; observations have been collected.
;-
  numStations = 10000L    ; Guess at expected number of stations.
  asc = 0L                ; Counter of actual stations providing data.

  snowfallReport_ = REPLICATE({station_id: '', $
                              station_name: '', $
                              station_type: '', $
                              longitude: 0.0D, $
                              latitude: 0.0D, $
                              elevation: 0L, $
                              value_meters: REPLICATE(NoDataValue, $
                                                      numDurations, $
                                                      numTimes)}, $
                             numStations)


;+
; Find forecast precipitation data. This procedure makes its own
; decisions regarding source and location of QPF.
;-
  ;; NSAPrefix = '/operations'
  ;; ArchiveDir = '/nwcdev/archive'
  precip_source = 'HRRR'
  precip_dir = NSAPrefix + '/misc/gisrs_incoming/HRRR'
  for hc = 1, numHours do begin
      date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
      QPF_found = 0
      GET_HOURLY_HRRR_LC_APCP_WEASD, date_YYYYMMDDHH, $
                                     targetFcstHour, $
                                     minSubFcstHour, $
                                     maxSubFcstHour, $
                                     precip_dir, $
                                     ScratchDir, $
                                     NoDataValue, $
                                     WEASDGrid, $
                                     APCPGrid, $
                                     perfect, $
                                     DRY_RUN_STATUS = status, $
                                     /NO_SAVE_FILE
      if NOT(status) then break
      QPF_found = 1
  endfor
  if (NOT(QPF_found) and archive_dir_visible) then begin
      precip_dir = ArchiveDir + '/HRRR_archive'
      for hc = 1, numHours do begin
          date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
          date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
          QPF_found = 0
          GET_HOURLY_HRRR_LC_APCP_WEASD, date_YYYYMMDDHH, $
                                         targetFcstHour, $
                                         minSubFcstHour, $
                                         maxSubFcstHour, $
                                         precip_dir, $
                                         ScratchDir, $
                                         NoDataValue, $
                                         WEASDGrid, $
                                         APCPGrid, $
                                         perfect, $
                                         DRY_RUN_STATUS = status, $
                                         /NO_SAVE_FILE
          if NOT(status) then break
          QPF_found = 1
      endfor
  endif
  if NOT(QPF_found) then begin
      precip_source = 'RAP'
      precip_dir = NSAPrefix + '/misc/gisrs_incoming/HRRR'
      for hc = 1, numHours do begin
          date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
          date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
          QPF_found = 0
          GET_HOURLY_RAP130_APCP_WEASD, date_YYYYMMDDHH, $
                                        targetFcstHour, $
                                        minSubFcstHour, $
                                        maxSubFcstHour, $
                                        precip_dir, $
                                        ScratchDir, $
                                        NoDataValue, $
                                        APCPGrid, $
                                        WEASDGrid, $
                                        perfect, $
                                        DRY_RUN_STATUS = status, $
                                        /NO_SAVE_FILE
          if NOT(status) then break
          QPF_found = 1
      endfor
  endif
  if (NOT(QPF_found) and archive_dir_visible) then begin
      precip_source = 'RAP'
      precip_dir = ArchiveDir + '/RAP_archive'
      for hc = 1, numHours do begin
          date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
          date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
          QPF_found = 0
          GET_HOURLY_RAP130_APCP_WEASD, date_YYYYMMDDHH, $
                                        targetFcstHour, $
                                        minSubFcstHour, $
                                        maxSubFcstHour, $
                                        precip_dir, $
                                        ScratchDir, $
                                        NoDataValue, $
                                        APCPGrid, $
                                        WEASDGrid, $
                                        perfect, $
                                        DRY_RUN_STATUS = status, $
                                        /NO_SAVE_FILE
          if NOT(status) then break
          QPF_found = 1
      endfor
  endif
  if (NOT(QPF_found) and archive_dir_visible) then begin
      precip_source = 'RUC'
      precip_dir = ArchiveDir + '/RUC_archive'
      for hc = 1, numHours do begin
          date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
          date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
          QPF_found = 0
          GET_HOURLY_RUC130_ACPCP_NCPCP_WEASD, $
              date_YYYYMMDDHH, $
              targetFcstHour, $
              minSubFcstHour, $
              maxSubFcstHour, $
              precip_dir, $
              ScratchDir, $
              NoDataValue, $
              ACPCPGrid, $
              NCPCPGrid, $
              WEASDGrid, $
              perfect, $
              DRY_RUN_STATUS = status, $
              /NO_SAVE_FILE
          if NOT(status) then break
          QPF_found = 1
      endfor
  endif
  if NOT(QPF_found) then begin
      ERR_MSG, 'No hourly QPF was found to subdivide snowfall observations.'
      RETURN
  endif

;; ;+
;; ; Perform a dry run search for precipitation data.
;; ;-
;;   for hc = 1, numHours do begin

;;       date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
;;       date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

;; print, 'sso01 ', precipsource, precipdir
;;       case PrecipSource of
;;           'HRRR' : begin
;;               GET_HOURLY_HRRR_LC_APCP_WEASD, date_YYYYMMDDHH, $
;;                                              targetFcstHour, $
;;                                              minSubFcstHour, $
;;                                              maxSubFcstHour, $
;;                                              PrecipDir, $
;;                                              ScratchDir, $
;;                                              NoDataValue, $
;;                                              WEASDGrid, $
;;                                              APCPGrid, $
;;                                              perfect, $
;;                                              DRY_RUN_STATUS = status
;;           end

;;           'RAP' : begin
;;               GET_HOURLY_RAP130_APCP_WEASD, date_YYYYMMDDHH, $
;;                                             targetFcstHour, $
;;                                             minSubFcstHour, $
;;                                             maxSubFcstHour, $
;;                                             PrecipDir, $
;;                                             ScratchDir, $
;;                                             NoDataValue, $
;;                                             APCPGrid, $
;;                                             WEASDGrid, $
;;                                             perfect, $
;;                                             DRY_RUN_STATUS = status, $
;;                                             /VERBOSE
;;           end
;;           'RUC' : begin
;;               GET_HOURLY_RUC130_ACPCP_NCPCP_WEASD, $
;;                   date_YYYYMMDDHH, $
;;                   targetFcstHour, $
;;                   minSubFcstHour, $
;;                   maxSubFcstHour, $
;;                   PrecipDir, $
;;                   ScratchDir, $
;;                   NoDataValue, $
;;                   ACPCPGrid, $
;;                   NCPCPGrid, $
;;                   WEASDGrid, $
;;                   perfect, $
;;                   DRY_RUN_STATUS = status
;;           end
;;       endcase
;;       if NOT(status) then begin
;;           ERR_MSG, 'Missing ' + PrecipSource + ' data for hour ending ' + $
;;                    date_YYYYMMDDHH
;;           RETURN
;;       end
;;   endfor


;---------------------------------------------------------------------;
; 2. Get observations for each duration and time, gathering them into ;
;    the snowfallReport_ structure.                                    ;
;---------------------------------------------------------------------;

  for dc = 0, numDurations - 1 do begin

      for hc = duration[dc], numHours do begin

          tc = hc - MIN(duration) ; time ind for snowfallReport_.value_meters

          date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
          date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

          GET_SNOWFALL_OBS, date_YYYYMMDDHH, $
                            date_YYYYMMDDHH, $
                            date_YYYYMMDDHH, $
                            duration[dc], $
                            minLon_, maxLon_, minLat_, maxLat_, $
                            PGHost, WebPGHost, $
                            thisSnowfallReport

          if NOT(ISA(thisSnowfallReport)) then CONTINUE

          numObs = N_ELEMENTS(thisSnowfallReport)

          for oc = 0, numObs - 1 do begin

              ind = WHERE(thisSnowfallReport[oc].station_id eq $
                          snowfallReport_.station_id, $
                          count)

              if (count eq 0) then begin


;                 This station has not been entered yet. First make
;                 sure there is room in the snowfallReport_ structure
;                 for a new entry.

                  if (asc eq numStations) then begin


;                     Add a structure element for the new station
;                     (reallocate).

                      snowfallReport_ = $
                          [snowfallReport_, $
                           {station_id: '', $
                            station_name: '', $
                            station_type: '', $
                            longitude: 0.0D, $
                            latitude: 0.0D, $
                            elevation: 0L, $
                            value_meters: REPLICATE(NoDataValue, $
                                                    numDurations, $
                                                    numTimes)}]

                      numStations++

                  endif


;                 Place the station information and the snowfall data
;                 into the structure.

                  snowfallReport_[asc].station_id = $
                      thisSnowfallReport[oc].station_id
                  snowfallReport_[asc].station_name = $
                      thisSnowfallReport[oc].station_name
                  snowfallReport_[asc].station_type = $
                      thisSnowfallReport[oc].station_type
                  snowfallReport_[asc].longitude = $
                      thisSnowfallReport[oc].longitude
                  snowfallReport_[asc].latitude = $
                      thisSnowfallReport[oc].latitude
                  snowfallReport_[asc].elevation = $
                      thisSnowfallReport[oc].elevation
                  snowfallReport_[asc].value_meters[dc, tc] = $
                      thisSnowfallReport[oc].value_meters

                  asc++
                  CONTINUE

              endif ; new station

              if (count ne 1) then STOP ; PROGRAMMING ERROR

              ind = ind[0]


;             Check remaining metadata elements for consistency. If
;             any of them change a warning is issued, and the new
;             information replaces the old in snowfallReport_.

              numMetaChange = 0

              if (thisSnowfallReport[oc].station_name ne $
                  snowfallReport_[ind].station_name) then begin
                  ERR_MSG, 'WARNING: station "' + $
                           snowfallReport_[ind].station_id + '" ' + $
                           'has station_name inconsistency: "' + $
                           snowfallReport_[ind].station_name + '" ' + $
                           'vs. "' + $
                           thisSnowfallReport[oc].station_name + '".'
                  snowfallReport_[ind].station_name = $
                      thisSnowfallReport[oc].station_name
                  numMetaChange++
              endif

              if (thisSnowfallReport[oc].station_type ne $
                  snowfallReport_[ind].station_type) then begin
                  ERR_MSG, 'WARNING: station "' + $
                           snowfallReport_[ind].station_id + '" ' + $
                           'has station_type inconsistency: "' + $
                           snowfallReport_[ind].station_type + '" ' + $
                           'vs. "' + $
                           thisSnowfallReport[oc].station_type + '".'
                  snowfallReport_[ind].station_type = $
                      thisSnowfallReport[oc].station_type
                  numMetaChange++
              endif

              if (thisSnowfallReport[oc].longitude ne $
                  snowfallReport_[ind].longitude) then begin
                  ERR_MSG, 'WARNING: station "' + $
                           snowfallReport_[ind].station_id + '" ' + $
                           'has longitude inconsistency: ' + $
                           STRCRA(snowfallReport_[ind].longitude) + $
                           ' vs. ' + $
                           STRCRA(thisSnowfallReport[oc].longitude)
                  snowfallReport_[ind].longitude = $
                      thisSnowfallReport[oc].longitude
                  numMetaChange++
              endif

              if (thisSnowfallReport[oc].latitude ne $
                  snowfallReport_[ind].latitude) then begin
                  ERR_MSG, 'WARNING: station "' + $
                           snowfallReport_[ind].station_id + '" ' + $
                           'has latitude inconsistency: ' + $
                           STRCRA(snowfallReport_[ind].latitude) + $
                           ' vs. ' + $
                           STRCRA(thisSnowfallReport[oc].latitude)
                  snowfallReport_[ind].latitude = $
                      thisSnowfallReport[oc].latitude
                  numMetaChange++
              endif

              if (thisSnowfallReport[oc].elevation ne $
                  snowfallReport_[ind].elevation) then begin
                  ERR_MSG, 'WARNING: station "' + $
                           snowfallReport_[ind].station_id + '" ' + $
                           'has elevation inconsistency: ' + $
                           STRCRA(snowfallReport_[ind].elevation) + $
                           ' vs. ' + $
                           STRCRA(thisSnowfallReport[oc].elevation)
                  snowfallReport_[ind].elevation = $
                      thisSnowfallReport[oc].elevation
                  numMetaChange++
              endif

              if (numMetaChange gt 0) then $
                  ERR_MSG, 'WARNING: ' + STRCRA(numMetaChange) + $
                           ' metadata fields were modified for station "' + $
                           snowfallReport_[ind].station_id + '".'


;             Add new data to the value_meters field.

              if (snowfallReport_[ind].value_meters[dc, tc] ne NoDataValue) $
                  then STOP         ; PROGRAMMING ERROR

              snowfallReport_[ind].value_meters[dc, tc] = $
                  thisSnowfallReport[oc].value_meters

          endfor

      endfor

;      if (snowfallReport_[ind].station_id eq checkID) then STOP

  endfor

  if (asc eq 0) then begin
      ERR_MSG, 'No snowfall observations were found.'
      RETURN
  endif

  if (asc lt numStations) then begin
;      USR_MSG, 'Truncating "snowfallReport_" structure from ' + $
;               STRCRA(numStations) + ' stations (expected) to ' + $
;               STRCRA(asc) + ' (actual).'
      snowfallReport_ = snowfallReport_[0:asc - 1]
      numStations = asc
  endif

  if (numStations ne asc) then STOP ; PROGRAMMING ERROR

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Received data for ' + STRCRA(numStations) + ' stations.'

;  PRINT, '--- found data for ' + STRCRA(numStations) + ' stations ---'


;--------------------------------------------------------------------;
; 3. Read QPF data, collecting APCP and WEASD for each hour of the   ;
;    analysis period, sampling the data at station locations at full ;
;    and reduced resolution.                                         ;
;--------------------------------------------------------------------;

  stationWEASD = $
      MAKE_ARRAY(numHours, numStations, /FLOAT, VALUE = NoDataValue)
  stationWEASDLow = $
      MAKE_ARRAY(numHours, numStations, /FLOAT, VALUE = NoDataValue)
  stationAPCP = $
      MAKE_ARRAY(numHours, numStations, /FLOAT, VALUE = NoDataValue)
  stationAPCPLow = $
      MAKE_ARRAY(numHours, numStations, /FLOAT, VALUE = NoDataValue)

  case precip_source of
      'HRRR' : resFactor = 5 ; Resample HRRR at 15 km for low-res option.
      'RAP' : resFactor = 1  ; Disables low-res option for RAP.
      'RUC' : resFactor = 1  ; Disables low-res option for RUC.
  endcase

  for hc = 1, numHours do begin

      date_Julian = startDate_Julian + DOUBLE(hc) / 24.0D
      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)

      case precip_source of
          'HRRR' : begin
              GET_HOURLY_HRRR_LC_APCP_WEASD, $
                  date_YYYYMMDDHH, $
                  targetFcstHour, $
                  minSubFcstHour, $
                  maxSubFcstHour, $
                  precip_dir, $
                  ScratchDir, $
                  NoDataValue, $
                  WEASDGrid, $
                  APCPGrid, $
                  perfect, $
                  HRRR_GRID_PROJ_INFO = QPFGridInfo, $
                  /NO_SAVE_FILE
          end

          'RAP' : begin
              GET_HOURLY_RAP130_APCP_WEASD, $
                  date_YYYYMMDDHH, $
                  targetFcstHour, $
                  minSubFcstHour, $
                  maxSubFcstHour, $
                  precip_dir, $
                  ScratchDir, $
                  NoDataValue, $
                  APCPGrid, $
                  WEASDGrid, $
                  perfect, $
                  RAP_GRID_PROJ_INFO = QPFGridInfo, $
                  /NO_SAVE_FILE
          end
          'RUC' : begin
              GET_HOURLY_RUC130_ACPCP_NCPCP_WEASD, $
                  date_YYYYMMDDHH, $
                  targetFcstHour, $
                  minSubFcstHour, $
                  maxSubFcstHour, $
                  precip_dir, $
                  ScratchDir, $
                  NoDataValue, $
                  ACPCPGrid, $
                  NCPCPGrid, $
                  WEASDGrid, $
                  perfect, $
                  RUC_GRID_PROJ_INFO = QPFGridInfo, $
                  /NO_SAVE_FILE
              if (ISA(ACPCPGrid) and $
                  ISA(NCPCPGrid) and $
                  ISA(WEASDGrid)) then begin
                  ind = WHERE((ACPCPGrid eq NoDataValue) or $
                              (NCPCPGrid eq NoDataValue), count)
                  APCPGrid = ACPCPGrid + NCPCPGrid
                  if (count gt 0) then APCPGrid[ind] = NoDataValue
                  ind = !NULL
                  ACPCPGrid = !NULL
                  NCPCPGrid = !NULL
              endif
          end
      endcase

      if (NOT(ISA(WEASDGrid)) or NOT(ISA(APCPGrid))) then begin
          ERR_MSG, 'Failed to get ' + precip_source + ' QPF for ' + $
                   date_YYYYMMDDHH + '.'
          RETURN
      endif

      if (hc eq 1) then begin

;+
;         Determine QPF grid locations of stations. All candidates
;         (HRRR, RAP, and RUC) are on grids in Lambert conformal
;         coordinates.
;-
          LCC_GRIB2_TO_SNYDER, QPFGridInfo.latSec1, $
                               QPFGridInfo.latSec2, $
                               QPFGridInfo.latD, $
                               QPFGridInfo.lonV, $
                               QPFGridInfo.lat00, $
                               QPFGridInfo.lon00, $
                               QPFGridInfo.eRadM, $
                               lonV_rad, $
                               nSny, $
                               FSny, $
                               rho0, $
                               x00, $
                               y00

          degToRad = !DPi / 180.0D

          latRad = snowfallReport_.latitude * degToRad
          rho = QPFGridInfo.eRadM * FSny / $
                (TAN(!DPi / 4.0D + latRad / 2.0D))^nSny
          theta = nSny * (snowfallReport_.longitude - QPFGridInfo.lonV)
          thetaRad = theta * degToRad

          x = rho * SIN(thetaRad)
          y = rho0 - rho * COS(thetaRad)

          iGrid = (x - x00) / QPFGridInfo.dx
          jGrid = (y - y00) / QPFGridInfo.dy

          iGridLow = (x - (x00 + $
                           (0.5D * DOUBLE(resFactor) - 0.5D) * $
                           QPFGridInfo.dx)) / $
                     (QPFGridInfo.dx * DOUBLE(resFactor))
          jGridLow = (y - (y00 + $
                           (0.5D * DOUBLE(resFactor) - 0.5D) * $
                           QPFGridInfo.dy)) / $
                     (QPFGridInfo.dy * DOUBLE(resFactor))


          lonV_rad = !NULL
          nSny = !NULL
          FSny = !NULL
          rho0 = !NULL
          x00 = !NULL
          y00 = !NULL

          latRad = !NULL
          rho = !NULL
          theta = !NULL
          thetaRad = !NULL
          x = !NULL
          y = !NULL

      endif

;+
;     Sample QPF data at stations.
;-
      hourlySiteWEASD = REGRID_BILIN(WEASDGrid, iGrid, jGrid, NoDataValue)
      hourlySiteAPCP = REGRID_BILIN(APCPGrid, iGrid, jGrid, NoDataValue)

      if (resFactor eq 1) then begin

;+
;         To simplify things, replicate WEASDGrid and APCPGrid
;-
          APCPGridLow = APCPGrid
          WEASDGridLow = WEASDGrid

      endif else begin

;+
;         Generate reduced resolution versions of QPF grids. Make sure
;         the full-resolution grids are an integer multiple of the
;         resFactor. If they are not, pad them with copies of the last
;         row/column. This will give the last row and column extra
;         weight when REBIN generates neighborhood averages, but we do
;         not expect that to have any major consequences for what we
;         are doing with them here.
;-
          nColsNeeded = resFactor * $
                        CEIL( DOUBLE(QPFGridInfo.nCols) / DOUBLE(resFactor) )
          nRowsNeeded = resFactor * $
                        CEIL(DOUBLE(QPFGridInfo.nRows) / DOUBLE(resFactor))

          nColsLow = nColsNeeded / resFactor
          nRowsLow = nRowsNeeded / resFactor

          if ((nColsNeeded gt QPFGridInfo.nCols) or $
              (nRowsNeeded gt QPFGridInfo.nRows)) then begin

              killme = MAKE_ARRAY(nColsNeeded, nRowsNeeded, $
                                  VALUE = NoDataValue)

              killme[0:QPFGridInfo.nCols - 1, $
                     0:QPFGridInfo.nRows - 1] = WEASDGrid

              for cc = QPFGridInfo.nCols, nColsNeeded - 1 do $
                  ;killme[cc, *] = WEASDGrid[QPFGridInfo.nCols - 1, *]
                  killme[cc, 0:QPFGridInfo.nRows - 1] = $
                  WEASDGrid[QPFGridInfo.nCols - 1, *]

              for rc = QPFGridInfo.nRows, nRowsNeeded - 1 do $
                  killme[*, rc] = killme[*, QPFGridInfo.nRows - 1]
                  ;killme[0:QPFGridInfo.nCols - 1, rc] = $
                  ;WEASDGrid[*, QPFGridInfo.nRows - 1]

              flag = REBIN(FLOAT(killme ne NoDataValue), nColsLow, nRowsLow)
              WEASDGridLow = REBIN(killme, nColsLow, nRowsLow)
              ind = WHERE(flag ne 1.0, count)
              if (count gt 0) then WEASDGridLow[ind] = NoDataValue

              killme[0:QPFGridInfo.nCols - 1, $
                     0:QPFGridInfo.nRows - 1] = APCPGrid

              for cc = QPFGridInfo.nCols, nColsNeeded - 1 do $
                  ;killme[cc, *] = APCPGrid[QPFGridInfo.nCols - 1, *]
                  killme[cc, 0:QPFGridInfo.nRows - 1] = $
                  APCPGrid[QPFGridInfo.nCols - 1, *]

              for rc = QPFGridInfo.nRows, nRowsNeeded - 1 do $
                  killme[*,rc] = killme[*, QPFGridInfo.nRows - 1]
                  ;killme[*, rc] = APCPGrid[*, QPFGridInfo.nRows - 1]

              flag = REBIN(FLOAT(killme ne NoDataValue), nColsLow, nRowsLow)
              APCPGridLow = REBIN(killme, nColsLow, nRowsLow)
              ind = WHERE(flag ne 1.0, count)
              if (count gt 0) then APCPGridLow[ind] = NoDataValue

              killme = !NULL

          endif else begin

              flag = $
                  REBIN(FLOAT(WEASDGrid ne NoDataValue), nColsLow, nRowsLow)
              WEASDGridLow = REBIN(WEASDGridLow, nColsLow, nRowsLow)
              ind = WHERE(flag ne 1.0, count)
              if (count gt 0) then WEASDGridLow[ind] = NoDataValue

              flag = $
                  REBIN(FLOAT(APCPGrid ne NoDataValue), nColsLow, nRowsLow)
              APCPGridLow = REBIN(APCPGridLow, nColsLow, nRowsLow)
              ind = WHERE(flag ne 1.0, count)
              if (count gt 0) then APCPGridLow[ind] = NoDataValue

          endelse

          flag = !NULL
          ind = !NULL

      endelse

      hourlySiteWEASDLow = $
          REGRID_BILIN(WEASDGridLow, iGridLow, jGridLow, NoDataValue)

      hourlySiteAPCPLow = $
          REGRID_BILIN(APCPGridLow, iGridLow, jGridLow, NoDataValue)

      stationWEASD[hc - 1, *] = hourlySiteWEASD
      stationWEASDLow[hc - 1, *] = hourlySiteWEASDLow
      stationAPCP[hc - 1, *] = hourlySiteAPCP
      stationAPCPLow[hc - 1, *] = hourlySiteAPCPLow

      WEASDGrid = !NULL
      APCPGrid = !NULL

      WEASDGridLow = !NULL
      APCPGridLow = !NULL

  endfor

;  PRINT, '--- finished getting HRRR data ---'


;------------------------------------------------------------------;
; 4. Cycle through snowfall observations, subdividing data at each ;
;    duration.                                                     ;
;------------------------------------------------------------------;

  stationSnowfall = MAKE_ARRAY(numHours, numStations, VALUE = NoDataValue)
  stationRevHist = STRARR(numStations)
  stationHasData = BYTARR(numStations)
  zeroToZeroCount = 0L
  nonDivisibleCount = 0L

  if KEYWORD_SET(avoidOverlap) then begin
      numNonSpotterOverlaps = 0L
      numSpotters = 0L
      numSpotterOverlaps = 0L
  endif

  for sc = 0, numStations - 1 do begin

      if KEYWORD_SET(avoidOverlap) then $
          thisStationCount = INTARR(numHours) ; counter, # obs each hour

;      if ((sc mod 100) eq 0) then PRINT, sc, numStations - 1

      id = snowfallReport_[sc].station_id


;     Identify spotter sites; important if AVOID_OVERLAP is set.

      if KEYWORD_SET(avoidOverlap) then begin
          isSpotter = 0
          if (STREGEX(id, '^[0-9]{2}\.[0-9]{4}_[0-9]{3}\.[0-9]{4}$', $
                      /BOOLEAN)) then begin
              isSpotter = 1
              numSpotters++
          endif
      endif

      ;; check = 0
      ;; if (KEYWORD_SET(CheckID) and ARG_PRESENT(CheckID)) then begin
      ;;     ic = WHERE(CheckID eq id, count)
      ;;     if (count eq 1) then begin
      ;;         check = 1
      ;;         ic = ic[0]
      ;;         if KEYWORD_SET(verbose) then $
      ;;             USR_MSG, 'Subdividing reports for "' + CheckID[ic] + '":'
      ;;         plottedYet = 0
      ;;     endif else begin
      ;;         if (count ne 0) then STOP ; PROGRAMMING CHECK
      ;;     endelse
      ;; endif

      for dc = 0, numDurations - 1 do begin

          for hc = duration[dc], numHours do begin

              tc = hc - MIN(duration)

              snowfallObs = snowfallReport_[sc].value_meters[dc, tc]
              if (snowfallObs eq NoDataValue) then CONTINUE

              WEASD = stationWEASD[hc - duration[dc]:hc - 1, sc]
              ind = WHERE(WEASD eq NoDataValue, count)
              if (count gt 0) then begin
                  if ((stationRevHist[sc] ne '') and $
                      (stationRevHist[sc] ne 'N')) then STOP
                  stationRevHist[sc] = 'N'
;                  if (snowfallReport_[sc].station_id eq checkID) then STOP
                  CONTINUE      ; NODATAVALUE put us out of business
              endif

;                  PRINT, '  - HRRR has ' + STRCRA(count) + $
;                         ' NODATAVALUE for ' + id + ', duration ' + $
;                         STRCRA(duration[dc]) + ' -'
;                  CONTINUE
;
;              endif

              totWEASD = TOTAL(WEASD)

              success = 0

              if (snowfallObs eq 0.0) then begin


;                 A zero subdivides into zeroes no matter what the
;                 model says. At some point we may find it useful to
;                 flag zero observations that are accompanied by
;                 nonzero model data, but for now we trust the
;                 observation and do not even look at the model.

                  stationSnowfall[hc - duration[dc]:hc - 1, sc] = 0.0
                  success = 1
                  stationRevHist[sc] = stationRevHist[sc] + 'Z'

              endif else begin

                  if (totWEASD gt 0.0) then begin


;                     *** Subdivide ***

                      stationSnowfall[hc - duration[dc]:hc - 1, sc] = $
                          snowfallObs * WEASD / totWEASD
                      success = 1
                      stationRevHist[sc] = stationRevHist[sc] + 'W'

                  endif else begin


;                     Full resolution WEASD is zero for all hours.
;                     Try low-res WEASD.

                      revStr = 'X' ; if this survives there is an error
                      WEASDLow = stationWEASDLow[hc - duration[dc]:hc - 1, sc]
                      totWEASDLow = TOTAL(WEASDLow)
                      ind = WHERE(WEASDLow eq NoDataValue, count)

                      if ((count eq 0) and (totWEASDLow gt 0.0)) then begin

                          WEASD = WEASDlow
                          totWEASD = totWEASDLow
                          revStr = 'w'

                      endif else begin


;                         Low resolution WEASD is zero for all hours.
;                         Try full-res APCP.

                          APCP = stationAPCP[hc - duration[dc]:hc - 1, sc]
                          totAPCP = TOTAL(APCP)
                          ind = WHERE(APCP eq NoDataValue, count)
                          if ((count eq 0) and (totAPCP gt 0.0)) $
                              then begin

                              WEASD = APCP
                              totWEASD = totAPCP
                              revStr = 'A'

                          endif else begin


;                             Full resolution APCP is zero for all hours.
;                             Try low-res APCP.

                              APCPLow = $
                                  stationAPCPLow[hc - duration[dc]:hc - 1, $
                                                 sc]
                              totAPCPLow = TOTAL(APCPLow)
                              ind = WHERE(APCPLow eq NoDataValue, count)
                              if ((count eq 0) and (totAPCPLow gt 0.0)) $
                                then begin

                                  WEASD = APCPLow
                                  totWEASD = totAPCPLow
                                  revStr = 'a'

                              endif else begin

                                  revStr = 'F'

                              endelse ; APCPLow did not work

                          endelse ; APCP did not work

                      endelse ; WEASDLow did not work

                      ind = WHERE(WEASD eq NoDataValue, count)
                      if ((count eq 0) and (totWEASD gt 0.0)) then begin

                          stationSnowfall[hc - duration[dc]:hc - 1, sc] = $
                              snowfallObs * WEASD / totWEASD
                          success = 1
;                          stationRevHist[sc] = stationRevHist[sc] + revStr

                      endif else begin

                          if (revStr ne 'F') then STOP ; PROGRAMMING ERROR
                          nonDivisibleCount++

                      endelse

                      if (revStr eq 'X') then STOP ; PROGRAMMING ERROR
                      stationRevHist[sc] = stationRevHist[sc] + revStr

                  endelse ; hi-res WEASD did not work

              endelse           ; nonzero obs


              if (success and KEYWORD_SET(avoidOverlap)) then begin
                  

;                 Increment counter of how many observations cover
;                 each hour.

                  thisStationCount[hc - duration[dc]:hc - 1] = $
                      thisStationCount[hc - duration[dc]:hc - 1] + 1

              endif

              if (success and ((hc - duration[dc]) gt 0)) then begin


;                 If subdivided snowfall values for any hours previous
;                 to the observation are no-data values, and the WEASD
;                 for those hours is zero, we will trust it.

                  earlierSnowfall = stationSnowfall[0:hc - duration[dc] - 1]
                  earlierWEASD = stationWEASD[0:hc - duration[dc] - 1, sc]

                  ind = WHERE((earlierSnowfall eq NoDataValue) and $
                              (earlierWEASD eq 0.0), count)

                  if (count gt 0) then begin

                      stationSnowfall[ind, sc] = 0.0
                      stationRevHist[sc] = stationRevHist[sc] + 'P'

                  endif

              endif

          endfor

      endfor


;     Any remaining no-data values are replaced with zeroes as long as
;     HRRR gives all zeroes.

      ind = WHERE(stationSnowfall[*, sc] eq NoDataValue, count)
      if (count gt 0) then begin
          model = stationWEASD[ind, sc]
          ind2 = WHERE(model eq 0.0, count2)
          if (count2 eq count) then begin
              stationSnowfall[ind, sc] = 0.0
              stationRevHist[sc] = stationRevHist[sc] + 'L'
          endif
      endif

      ind = WHERE(stationSnowfall[*, sc] ne NoDataValue, count)
      if (count gt 0) then stationHasData[sc] = 1B

      if KEYWORD_SET(avoidOverlap) then begin


;         Check for overlapping reports. For non-spotters, a limited
;         amount of overlap is tolerated, but no overlap from spotters
;         is allowed.

          ind1 = WHERE((thisStationCount gt 0), count1) ; hours with some obs

          if (count1 gt 0) then begin

              minObsCount = MIN(thisStationCount[ind1])
              maxObsCount = MAX(thisStationCount[ind1])

              obsCountLimit = 3

              if ((minObsCount ne maxObsCount) and $
                  (maxObsCount gt obsCountLimit)) then begin


;                 Significant overlap - discard data.

                  if (KEYWORD_SET(verbose) and check) then $
                      USR_MSG, 'Site "' + id + $
                               '" has > ' + STRCRA(obsCountLimit) + ' ' + $
                               '(' + STRCRA(maxObsCount) + ') ' + $
                               'overlapping reports.'

                  stationSnowfall[*, sc] = NoDataValue
                  stationHasData[sc] = 0B
                  if isSpotter then $
                      numSpotterOverlaps++ $
                  else $
                      numNonSpotterOverlaps++

              endif else begin

                  if ((maxObsCount gt 1) and isSpotter) then begin


;                     Overlapping reports for spotter sites are not
;                     allowed, period.

                      if (KEYWORD_SET(verbose) and check) then $
                          USR_MSG, 'Spotter site "' + id + $
                                   '" has ' + $
                                   STRCRA(maxObsCount) + $
                                   ' overlapping reports.'

                      stationSnowfall[*, sc] = NoDataValue
                      stationHasData[sc] = 0B
                      numSpotterOverlaps++

                  endif

              endelse

          endif

      endif

  endfor


; Remove stations where no reports were subdivided.

  if KEYWORD_SET(avoidOverlap) then begin
      if ((numNonSpotterOverlaps gt 0) and KEYWORD_SET(verbose)) then $
          USR_MSG, 'Eliminated ' + STRCRA(numNonSpotterOverlaps) + $
                   ' sites due to excessive temporal overlap' + $
                   ' between observations.'
      if ((numSpotterOverlaps gt 0) and KEYWORD_SET(verbose)) then $
          USR_MSG, 'Eliminated ' + STRCRA(numSpotterOverlaps) + $
                   ' of ' + STRCRA(numSpotters) + $
                   ' spotter sites due to temporal overlap' + $
                   ' between observations.'
  endif

  ind = WHERE(stationHasData eq 0, count)
  if ((count gt 0) and (KEYWORD_SET(verbose))) then $
      USR_MSG, 'Removing a total of ' + STRCRA(count) + $
               ' stations where observations could not be subdivided.'
  ind = WHERE(stationHasData ne 0, count)
  if (count eq 0) then begin
      ERR_MSG, 'No observations were subdivided.'
      RETURN
  endif
  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Subdivided reports for ' + STRCRA(count) + ' stations.'
  snowfallReport = snowfallReport_ ; for posterity
  snowfallReport_ = snowfallReport_[ind]
  stationSnowfall = stationSnowfall[*, ind]
  numStations = count


; Create output data structure.

  subSnowfallReport = REPLICATE({station_id: '', $
                                 station_name: '', $
                                 station_type: '', $
                                 longitude: 0.0D, $
                                 latitude: 0.0D, $
                                 elevation: 0L, $
                                 value_meters: REPLICATE(NoDataValue, numHours), $
                                 revision_history: ''}, $
                                numStations)

  for sc = 0, numStations - 1 do begin
      subSnowfallReport[sc].station_id = snowfallReport_[sc].station_id
      subSnowfallReport[sc].station_name = snowfallReport_[sc].station_name
      subSnowfallReport[sc].station_type = snowfallReport_[sc].station_type
      subSnowfallReport[sc].longitude = snowfallReport_[sc].longitude
      subSnowfallReport[sc].latitude = snowfallReport_[sc].latitude
      subSnowfallReport[sc].elevation = snowfallReport_[sc].elevation
      subSnowfallReport[sc].value_meters = stationSnowfall[*, sc]
      subSnowfallReport[sc].revision_history = stationRevHist[sc]
  endfor

  RETURN

end
