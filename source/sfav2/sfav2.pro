
; Experimental Snowfall Analysis
; Version 2

; Greg Fall, Nathan Patrick, Kent Sparrow
; Office of Weather Prediction
; National Weather Service

; August 2016

;   target finish date to nearest hour (DATE_YYYYMMDDHH)
;   duration [hours] (DURATION_HOURS)
;   window hours back (WINDOW_HOURS_BACK)
;   window hours forward (WINDOW_HOURS_FORWARD)
   
; TODO:
;
;   Replace FILE_MKDIR with MKDIR_2775 where appropriate.
;   Copy observation/diagnostic files to "issDir".
;   Replace STOPs on exceptions with proper CATCH handling.
;   Make sure operational script/s are careful about IDL Runtime licenses.

; Messaging philosophy:
;
;   Regular diagnostic information should be provided with USR_MSG
;   (not PRINT) but only under the "if (verbose)" condition.
;   Equivalent of syslog INFO (level 6).
;
;   Messages indicating something non-ideal has occurred (e.g.,
;   missing environment variable; missing Stage IV data; poor
;   variogram fit) should be provided with USR_MSG (not PRINT)
;   regardless of the value of "verbose". These should begin with
;   "NOTICE:".
;   Equivalent of syslog NOTICE (level 5).
;
;   Messages that are more serious than the above, such as missing
;   directories, or more serious non-fatal problems, should be
;   provided with ERR_MSG (not PRINT) regardless of the value of
;   "verbose". These should begin with "WARNING:".
;   Equivalent of syslog WARNING (level 4).
;
;   Messages connected with fatal errors should be provided with
;   ERR_MSG (not PRINT), should begin with "ERROR:", and should be
;   followed by "if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL"
;   Equivalent of syslog ERROR (level 3).


            ;============================================;
            ; MAIN SNOWFALL ANALYSIS VERSION 2 PROCEDURE ;
            ;============================================;


PRO SFAV2, anlEndDate_YYYYMMDDHH

  COMMON info, message

  !QUIET = 1
  !EXCEPT = 1 ; 2 slows the program but will help us spot math errors

  COMPILE_OPT STRICTARRSUBS     ; No out-of-bounds array indexing
  COMPILE_OPT STRICTARR         ; No parentheses for array references

  debug_tag = 100

  t1Full = SYSTIME(/UTC, /SECONDS)

  sfaStatus = 0 ; success/failure (1/0) status of this program
  message = ''
  verbose = 0
  produceImages = 1
  produceTIFFs = 1
  stdOutStat = FSTAT(-1)
  stdErrStat = FSTAT(-2)
  if (stdOutStat.isatty and stdErrStat.isatty) then verbose = 1

  weHaveBailed = 0

  CATCH, errorStatus
  if (errorStatus ne 0) then begin
      MESSAGE, !Error_State.Msg, /CONTINUE
      if ISA(debug_tag) then $
          MESSAGE, 'debug_tag = ' + STRCRA(debug_tag), /CONTINUE
      if (weHaveBailed) then EXIT, STATUS = 1
      GOTO, BAIL
  endif

  if LMGR(/VM) then begin
      ERR_MSG, 'ERROR: This program cannot run in the IDL Virtual Machine.'
      GOTO, BAIL
  endif


;===================;
; 1. Initial setup. ;
;===================;

  debug_tag = 200

;----------------------------------------------------------------------;
; 1a. Get arguments and system information from environment variables. ;
;----------------------------------------------------------------------;

;+
; These are best set with the calling script "sfav2.sh"
;-

;+
; Get system information.
;-

;+
; Establish whether or not a "quiet" run is requested. If so, the
; "verbose" flag is set to zero regardless of its previous automatic
; setting.
;-
  quietRun = GETENV('QUIET_RUN')
  if (quietRun eq '') then begin
      quietRun = 0
  endif else begin
      if ((quietRun ne '0') and (quietRun ne '1')) then begin
          ERR_MSG, 'ERROR: Invalid QUIET_RUN value "' + quietRun + '".'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      quietRun = FIX(quietRun)
  endelse

  if quietRun then begin
      verbose = 0
      ;; produceImages = 0
      ;; produceTIFFs = 0
  endif

  if (LMGR(/RUNTIME) and verbose) then begin

;+
;     Establish colors, etc., for plots.
;-
      WINDOW, /PIXMAP & WDELETE
      DEVICE, DECOMPOSED=0
      DEVICE, RETAIN=2

  endif

;+
; Establish whether the "extended" color map, which distinguishes
; snowfall accumulations from 4 feet to 10 feet.
;-
  extendedColors = GETENV('EXTENDED_COLORS')
  if (extendedColors eq '') then begin
      extendedColors = 0
  endif else begin
      if ((extendedColors ne '0') and (extendedColors ne '1')) then begin
          ERR_MSG, 'ERROR: Invalid EXTENDED_COLORS value "' + $
                   extendedColors + '".'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      extendedColors = FIX(extendedColors)
  endelse

  if verbose then begin
      USR_MSG, '----------------------- NATIONAL SNOWFALL ANALYSIS V2 ' + $
               '-----------------------'
      USR_MSG, '--- 1. INITIAL SETUP ---'
  endif

  environment = GETENV('ENVIRONMENT')
  if (environment eq '') then begin
      ERR_MSG, 'ERROR: Failed to identify ENVIRONMENT.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if ((environment ne 'development') and (environment ne 'operations')) $
    then begin
      ERR_MSG, 'ERROR: Unsupported ENVIRONMENT of "' + environment + '".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif  

  PGHost = GETENV('PGHOST')
  if (PGHost eq '') then begin
      ERR_MSG, 'ERROR: Failed to identify PGHOST.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  PGHost_split = STRSPLIT(PGHost, '.', /EXTRACT)
  if ((environment eq 'development') and $
      (PGHost_split[0] ne 'ddb0')) then begin
      ERR_MSG, 'ERROR: Invalid PGHOST="' + PGHost + '" for environment "' + $
               environment + '".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if ((environment eq 'operations') and $
      (PGHost_split[0] ne 'odb0')) then begin
      ERR_MSG, 'ERROR: Invalid PGHOST="' + PGHost + '" for environment "' + $
               environment + '".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  webPGHost = GETENV('WEB_PGHOST')
  if (webPGHost eq '') then begin
      ERR_MSG, 'ERROR: Failed to identify WEB_PGHOST.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Kriging by radial means (KRM) parameters.

  debug_tag = 300

  targetKRMRingWidthMeters = GETENV('TARGET_KRM_RING_WIDTH')
  if (targetKRMRingWidthMeters eq '') then begin
      targetKRMRingWidthMeters = 4.0D3
  endif else begin
      targetKRMRingWidthMeters = DOUBLE(targetKRMRingWidthMeters)
  endelse
  if ((targetKRMRingWidthMeters le 0.0D) or $
      (targetKRMRingWidthMeters gt 100.0D3)) then begin
      ERR_MSG, 'ERROR: TARGET_KRM_RING_WIDTH value of ' + $
               STRCRA(targetKRMRingWidthMeters) + ' must be ' + $
               'nonnegative and no more than 100000 m (100 km).'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  numKRMAngles = GETENV('NUM_KRM_ANGLES')
  if (numKRMAngles eq '') then begin
      numKRMAngles = 4
  endif else begin
      numKRMAngles = FIX(numKRMAngles)
  endelse
  if ((numKRMAngles lt 1) or $
      (numKRMAngles gt 360)) then begin
      ERR_MSG, 'ERROR: NUM_KRM_ANGLES value of ' + $
               STRCRA(numKRMAngles) + ' must be ' + $
               'nonnegative and no more than 360.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Establish whether or not parameter_testing is being done. If it is,
; then the output directory is identified by the KRM parameters.

  ;; paramTesting = GETENV('PARAMETER_TESTING')
  ;; if (paramTesting eq '') then begin
  ;;     paramTesting = 0
  ;; endif else begin
  ;;     if ((paramTesting ne '0') and (paramTesting ne '1')) then begin
  ;;         ERR_MSG, 'ERROR: Invalid PARAMETER_TESTING value "' + $
  ;;                  paramTesting + '".'
  ;;         if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  ;;     endif
  ;;     paramTesting = FIX(paramTesting)
  ;; endelse


; Check for a "dry run" where a first guess and first-pass variogram
; are produced, but no analysis is performed.

  dryRun = GETENV('DRY_RUN')
  if (dryRun eq '') then begin
      dryRun = 0
  endif else begin
      if ((dryRun ne '0') and (dryRun ne '1')) then begin
          ERR_MSG, 'ERROR: Invalid DRY_RUN value "' + dryRun + '".'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      dryRun = FIX(dryRun)
  endelse


; Determine whether or not copies of TIFF and CSV files should be
; created/updated to accommodate e.g. a GIS project.

  loginInfo = GET_LOGIN_INFO()

  updateGISProject = GETENV('UPDATE_GIS_PROJECT')
  if (updateGISProject eq '') then begin
      updateGISProject = 0
  endif else begin
      if ((updateGISProject ne '0') and $
          (updateGISProject ne '1')) then begin
          ERR_MSG, 'ERROR: Invalid UPDATE_GIS_PROJECT value "' + $
                   updateGISProject + '".'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      updateGISProject = FIX(updateGISProject)
  endelse

  ;; if updateGISProject then begin
  ;;     produceTIFFs = 1
  ;;     GISProjectDir = 'sfav2_' + loginInfo.user_name
  ;;     if NOT(FILE_TEST(GISProjectDir, /DIRECTORY)) then begin
  ;;         FILE_MKDIR, GISProjectDir
  ;;     endif
  ;; endif

  if (dryRun ne 0) then begin
      produceImages = 0
      produceTIFFs = 0
      updateGISProject = 0
  endif


; Check for the option to bypass QPE analysis.

  skipQPEAnalysis = GETENV('SKIP_QPE_ANALYSIS')
  if (skipQPEAnalysis eq '') then begin
      skipQPEAnalysis = 0
  endif else begin
      if ((skipQPEAnalysis ne '0') and (skipQPEAnalysis ne '1')) then begin
          ERR_MSG, 'ERROR: Invalid SKIP_QPE_ANALYSIS value "' + $
                   skipQPEAnalysis + '".'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      skipQPEAnalysis = FIX(skipQPEAnalysis)
  endelse


; Finish date/time of analysis period. Default is 12Z of the current
; day in UTC.

  debug_tag = 400

  if NOT(ISA(anlEndDate_YYYYMMDDHH)) then begin
      anlEndDate_YYYYMMDDHH = GETENV('DATE_YYYYMMDDHH')
      if (anlEndDate_YYYYMMDDHH eq '') then begin
          sysTime_Julian = SYSTIME(/JULIAN, /UTC)
          sysTime_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(sysTime_Julian)
          anlEndDate_YYYYMMDDHH = STRMID(sysTime_YYYYMMDDHH, 0, 8) + '12'
          USR_MSG, 'NOTICE: Environment variable DATE_YYYYMMDDHH ' + $
                   'not provided. Using default of ' + $
                   anlEndDate_YYYYMMDDHH + '.'
      endif
  endif else begin
      if NOT(STREGEX(anlEndDate_YYYYMMDDHH, '^[0-9]{10}$', /BOOLEAN)) $
      then begin
          ERR_MSG, 'ERROR: Analysis date value of "' + $
                   anlEndDate_YYYYMMDDHH + '" is invalid.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endelse

;+
; Identify the NSA_PREFIX, which is the bottom level directory for
; everything in operations.
;-
  NSAPrefix = GETENV('NSA_PREFIX')
  if (NSAPrefix eq '') then begin
      NSAPrefix = '/operations'
      USR_MSG, 'NOTICE: Environment variable NSA_PREFIX not provided. ' + $
               'Using default of ' + NSAPrefix + '.'
  endif

;+
; Define the directory for utility programs.
;-
  utilsDir = GETENV('SFAV2_UTILS_DIR')
  if (utilsDir eq '') then begin
      if LMGR(/RUNTIME) then $
          utilsDir = NSAPrefix + '/gisrs/idl/snowfall_v2/utils' $
      else $
          utilsDir = '/nwcdev/nsadev/snowfall_v2_devel' + $
                     '/national_snowfall_analysis/utils'
      USR_MSG, 'NOTICE: Environment variable SFAV2_UTILS_DIR not ' + $
               'provided. Using default of ' + utilsDir + '.'
  endif
  if NOT(FILE_TEST(utilsDir, /DIR, /READ)) then begin
      if FILE_TEST(utilsDir, /DIR) then $
          ERR_MSG, 'ERROR: Utility directory ' + utilsDir + $
                   ' is not readable by this user.' $
      else $
          ERR_MSG, 'ERROR: Utility directory ' + utilsDir + ' not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

 
; Define the directory for resources (i.e., static/parameter data sets
; used at different points in the analysis).

  resourcesDir = GETENV('SFAV2_RESOURCES_DIR')
  if (resourcesDir eq '') then begin
      if LMGR(/RUNTIME) then $
          resourcesDir = NSAPrefix + '/gisrs/idl/snowfall_v2/resources' $
      else $
          resourcesDir = '/nwcdev/nsadev/snowfall_v2_devel' + $
                         '/national_snowfall_analysis/resources'
      USR_MSG, 'NOTICE: Environment variable SFAV2_RESOURCES_DIR not ' + $
               'provided. Using default of ' + resourcesDir + '.'
  endif
  if NOT(FILE_TEST(resourcesDir, /DIR, /WRITE)) then begin
      if FILE_TEST(resourcesDir, /DIR) then $
          ERR_MSG, 'ERROR: Resources directory ' + resourcesDir + $
                   ' is not writeable by this user.' $
      else $
          ERR_MSG, 'ERROR: Resources directory ' + resourcesDir + ' not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Define the parent directory for archives of QPF (HRRR, RAP, RUC) and
; QPE (Stage IV) inputs to the analysis.

  archiveDir = GETENV('SFAV2_ARCHIVE_DIR')
  if (archiveDir eq '') then begin
      archiveDir = '/nwcdev/archive'
      USR_MSG, 'NOTICE: Environment variable SFAV2_ARCHIVE_DIR not ' + $
               'provided. Using default of ' + archiveDir + '.'
  endif
  if (NOT(FILE_TEST(archiveDir, /DIR, /READ)) and $
      (environment eq 'development')) then begin
      if FILE_TEST(archiveDir, /DIR) then $
          ERR_MSG, 'ERROR: Archive directory ' + archiveDir + $
                   ' is not readable by this user.' $
      else $
          ERR_MSG, 'ERROR: Archive directory ' + archiveDir + ' not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Define the output directory for images and CSV files, as well as
; "bad_station" and "good_station" files.

  outputDir = GETENV('SFAV2_OUTPUT_DIR')
  if (outputDir eq '') then begin
      if LMGR(/RUNTIME) then $
          outputDir = NSAPrefix + '/misc/snowfall_v2' $
      else $
          outputDir = '/nwcdev/nsadev/snowfall_v2_output'
      USR_MSG, 'NOTICE: Environment variable SFAV2_OUTPUT_DIR not ' + $
               'provided. Using default of ' + outputDir + '.'
  endif
  if NOT(FILE_TEST(outputDir, /DIR, /WRITE)) then begin
      if FILE_TEST(outputDir, /DIR) then $
          ERR_MSG, 'ERROR: Output directory ' + outputDir + $
                   ' is not writable by this user.' $
      else $
          ERR_MSG, 'ERROR: Output directory ' + outputDir + ' does not exist.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  outputParentDir = outputDir
  outputDir = outputDir + '/' + $
              'sfav2_' + STRMID(anlEndDate_YYYYMMDDHH, 0, 8)

  ;; if paramTesting then begin
  ;;     outputDir = outputDir + '_' + $
  ;;                 'KRM_ring_width_' + $
  ;;                 STRCRA(ROUND(targetKRMRingWidthMeters)) + '_meters_' + $
  ;;                 'KRM_num_angles_' + $
  ;;                 STRCRA(numKRMAngles)
  ;; endif

  if NOT(FILE_TEST(outputDir, /DIR)) then begin
      FILE_MKDIR, outputDir
      if NOT(FILE_TEST(outputDir, /DIR)) then begin
          ERR_MSG, 'ERROR: Failed to create output directory ' + outputDir
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endif


  if updateGISProject then begin


;     Define the output directory for generically-named copies of
;     files (i.e., without dates indicated).

      produceTIFFs = 1
      GISProjectDir = outputParentDir + '/' + 'sfav2_' + loginInfo.user_name
      if NOT(FILE_TEST(GISProjectDir, /DIRECTORY)) then begin
          FILE_MKDIR, GISProjectDir
      endif
  endif


; Define the output directory for web-accessible results.

  if (environment eq 'operations') then $
      webDir = NSAPrefix + '/web/snowfall_v2/data' $
  else $
      webDir = '/net/http0/htdocs/snowfall_v2/data'
  if FILE_TEST(webDir, /DIRECTORY, /WRITE) then begin
      webOutputDir = webDir + '/' + STRMID(anlEndDate_YYYYMMDDHH, 0, 6)
      if NOT(FILE_TEST(webOutputDir)) then $
          FILE_MKDIR, webOutputDir
      if NOT(FILE_TEST(webOutputDir, /DIRECTORY, /WRITE)) then begin
          ERR_MSG, 'WARNING: failed to create output web directory ' + $
                   webOutputDir + '.'
          webOutputDir = !NULL
      endif
  endif else begin
      if FILE_TEST(webDir, /DIRECTORY) then $
          ERR_MSG, 'WARNING: output web directory ' + webDir + $
                   ' not writable by this user.' $
      else $
          ERR_MSG, 'WARNING: output web directory ' + webDir + $
                   ' not visible on this system.'
  endelse


; Identify locations of shapefiles needed in analysis.

  debug_tag = 500

  vectorDir = resourcesDir + '/shapefiles' ; for maps

  if NOT(FILE_TEST(vectorDir, /DIRECTORY, /READ)) then begin
      if FILE_TEST(vectorDir, /DIRECTORY) then $
          ERR_MSG, 'ERROR: Vector data directory ' + vectorDir + $
                   ' is not readable by this user.' $
      else $
          ERR_MSG, 'ERROR: Vector data directory ' + vectorDir + ' not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  end

  shapePathList = [vectorDir + '/' + $
                   'National_Boundary_Canada', $
                   vectorDir + '/' + $
                   'Provincial_Boundaries', $
                   vectorDir + '/' + $
                   'National_Boundary_Mexico_2004', $
                   vectorDir + '/' + $
                   'National_Boundary_Coterminous_US', $
                   vectorDir + '/' + $
                   'State_Boundaries_Coterminous_US']

  for sc = 0, N_ELEMENTS(shapePathList) - 1 do begin
      if NOT(FILE_TEST(shapePathList[sc] + '.shp')) then $
          ERR_MSG, 'WARNING: missing shapefile ' + $
                   shapePathList[sc] + '.shp'
  endfor


; Set up the scratch disk. This will hold the following:
;
;   1. Temporary files used by GRIB decoders:
;      tmpGRIBRaster.[0-9]{14}.*
;      tmpGRIBHeader.[0-9]{14}.*
;   2. Daily min/max temperatures from SNODAS, HRRR, RAP, and RUC:
;      SNODAS_min_max_ave_2m_temp_anl_24h_ending_[0-9]{10}.sav
;      HRRR_min_max_ave_2m_temp_f00_24h_ending_[0-9]{10}.sav
;      RAP_min_max_ave_2m_temp_f00_24h_ending_[0-9]{10}.sav
;      RUC_min_max_ave_2m_temp_f00_24h_ending_[0-9]{10}.av
;   3. HRRR, RAP, and RUC precipitation accumulations:
;      HRRR_APCP_WEASD_f[0-1][0-9]_24h_ending_[0-9]{10}.sav
;      RAP_ACPCP_NCPCP_WEASD_f[0-2][0-9]_24h_ending_[0-9]{10}.sav
;      RUC_ACPCP_NCPCP_WEASD_f[0-2][0-9]_24h_ending_[0-9]{10}.sav
;
; These should be preserved for a few days once they are generated so
; repeat runs of this program can use them as the observations
; (station snowfall observations as well as QPE) evolve and improve.

  scratchDir = GETENV('SFAV2_SCRATCH_DIR')
  if (scratchDir eq '') then begin
      if LMGR(/RUNTIME) then $
          scratchDir = NSAPrefix + '/misc/snowfall_v2/scratch' $
      else $
          scratchDir = '/net/scratch/' + loginInfo.user_name
      USR_MSG, 'NOTICE: Environment variable SFAV2_SCRATCH_DIR not ' + $
               'provided. Using default of ' + scratchDir + '.'
  endif
  if NOT(FILE_TEST(scratchDir, /DIR, /WRITE)) then begin
      if FILE_TEST(scratchDir, /DIR) then $
          ERR_MSG, 'ERROR: Scratch directory ' + scratchDir + $
                   ' is not writable by this user.' $
      else $
          ERR_MSG, 'ERROR: Scratch directory ' + scratchDir + $
                   ' does not exist.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Make sure the analysis date is in the past.

  anlEndDate_Julian = YYYYMMDDHH_TO_JULIAN(anlEndDate_YYYYMMDDHH)
  sysTime_Julian = SYSTIME(/JULIAN, /UTC)

  if (sysTime_Julian lt anlEndDate_Julian) then begin
      ERR_MSG, 'ERROR: Analysis time ' + anlEndDate_YYYYMMDDHH + $
               ' is in the future. ' + $
               'No analysis is possible.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Set a general-purpose no-data value for floating point variables.

  ndv = -99999.0


; Duration of analyis period in hours. Default is 24.

  durationStr = GETENV('DURATION_HOURS')
  if (durationStr eq '') then begin
      durationStr = '24'
      duration = 24
      USR_MSG, 'NOTICE: Environment variable DURATION_HOURS not ' + $
               'provided. Using default of ' + durationStr + '.'
  endif else begin
      if NOT(STREGEX(durationStr, '^[0-9]+$', /BOOLEAN)) then begin
          ERR_MSG, 'ERROR: DURATION_HOURS value of "' + duration + $
                   '" is invalid.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      duration = FIX(durationStr)
      if (duration le 0) then begin
          ERR_MSG, 'ERROR: DURATION_HOURS (value of "' + $
                   durationStr + '") ' + $
                   'must be a positive SHORT integer.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endelse


; Observation time window may include hours before and after analysis
; time. For example, if the analysis hour is 12Z, and windowHoursBack
; is 3, and windowHoursForward is 5, then snowfall observations dated
; 09Z to 17Z will be included in the analysis. Default is three hours
; for both.

  windowHoursBack = GETENV('WINDOW_HOURS_BACK')
  if (windowHoursBack eq '') then begin
      USR_MSG, 'NOTICE: Environment variable WINDOW_HOURS_BACK not ' + $
               'provided. Using default of 3.'
      windowHoursBack = 3
  endif else begin
      if NOT(STREGEX(windowHoursBack, '^[0-9]+$', /BOOLEAN)) then begin
          ERR_MSG, 'ERROR: WINDOW_HOURS_BACK value of "' + $
                   windowHoursBack + '" is invalid.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      windowHoursBackStr = windowHoursBack
      windowHoursBack = FIX(windowHoursBack)
      if (windowHoursBack lt 0) then begin
          ERR_MSG, 'ERROR: WINDOW_HOURS_BACK (value of "' + $
                   windowHoursBackStr + $
                   '") must be a nonnegative SHORT integer.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endelse

  windowHoursForward = GETENV('WINDOW_HOURS_FORWARD')
  if (windowHoursForward eq '') then begin
      USR_MSG, 'NOTICE: Environment variable WINDOW_HOURS_FORWARD not ' + $
               'provided. Using default of 3.'
      windowHoursForward = 3
  endif else begin
      if NOT(STREGEX(windowHoursForward, '^[0-9]+$', /BOOLEAN)) then begin
          ERR_MSG, 'ERROR: WINDOW_HOURS_FORWARD value of "' + $
                   windowHoursForward + '" is invalid.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      windowHoursForwardStr = windowHoursForward
      windowHoursForward = FIX(windowHoursForward)
      if (windowHoursForward lt 0) then begin
          ERR_MSG, 'ERROR: WINDOW_HOURS_FORWARD (value of "' + $
                   windowHoursForwardStr + $
                   '") must be a nonnegative SHORT integer.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endelse

;+
; Define color ramps for plotting.
;-

  debug_tag = 600

;+
; Temperature, Celsius.
;-
  edges_TdegC = [-40.0, -30.0, -20.0, -10.0, -5.0, 0.0, $
                 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 40.0]
  tickNames_TdegC = FORMAT_FLOAT(edges_TdegC)
  tickNames_TdegC[0] = '<' + tickNames_TdegC[1]
  tickNames_TdegC[12] = '>' + tickNames_TdegC[11]
  red_TdegC = [204, 153, 112, 052, 000, 000, 144, 255, 247, 255, 200, 145]
  grn_TdegC = [000, 000, 048, 050, 153, 175, 209, 193, 150, 000, 000, 000]
  blu_TdegC = [153, 153, 162, 255, 255, 080, 079, 000, 073, 000, 000, 000]

;+
; Snowfall difference, red-to-blue
;-
  edges_sfDiff = [-100.0, -2.0, -1.0, -0.5, -0.25, $
                 -0.1, 0.1, $
                 0.25, 0.5, 1.0, 2.0, 100.0]
  tickNames_sfDiff = FORMAT_FLOAT(edges_sfDiff)
  tickNames_sfDiff[0] = '<' + tickNames_sfDiff[1]
  tickNames_sfDiff[N_ELEMENTS(edges_sfDiff) - 1] = $
      '>' + tickNames_sfDiff[N_ELEMENTS(edges_sfDiff) - 2]
  red_sfDiff = [215, 230, 245, 253, 254, 220, 221, 187, 145, 094, 044]
  grn_sfDiff = [025, 084, 144, 190, 222, 220, 239, 224, 198, 160, 123]
  blu_sfDiff = [028, 055, 083, 115, 153, 220, 207, 224, 222, 202, 182]

;+
; Snowfall ratio.
;-
  edges_sfRatio = [-25.0, -2.0, -1.0, -0.5, -0.25, -0.1, $
                   0.1, 0.25, 0.5, 1.0, 2.0, 25.0]
  tickNames_sfRatio = FORMAT_FLOAT(edges_sfRatio)
  tickNames_sfRatio[0] = '<' + tickNames_sfRatio[1]
  tickNames_sfRatio[11] = '>' + tickNames_sfRatio[10]
  red_sfRatio = red_sfDiff
  grn_sfRatio = grn_sfDiff
  blu_sfRatio = blu_sfDiff

;+
; Precipitation.
;-
  edges_QPF = [0.0, 0.1, 2.0, 5.0, 10.0, 15.0, 20.0, 25.0, 35.0, 50.0, $
               75.0, 100.0, 125.0, 150.0, 175.0, 1000.0]
  tickNames_QPF = FORMAT_FLOAT(edges_QPF)
  tickNames_QPF[15] = '>' + tickNames_QPF[14]
  red_QPF = [255, 000, 000, 127, 238, 255, 255, 255, 238, 205, 139, 145, $
             137, 016, 030]
  grn_QPF = [228, 139, 205, 255, 238, 215, 165, 127, 064, 000, 000, 044, $
             104, 078, 144]
  blu_QPF = [220, 000, 000, 000, 000, 000, 079, 000, 000, 000, 000, 238, $
             205, 139, 255]

;+
; Snowfall-to-liquid ratio.
;-
  edges_SLR = [6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, $
               16.0, 17.0, 18.0, 19.0, 20.0, 21.0]
  red_SLR = [200, 000, 000, 000, 000, 000, 127, 240, 204, 255, 255, 255, $
             237, 238, 204]
  grn_SLR = [255, 255, 240, 180, 140, 204, 255, 240, 127, 125, 175, 105, $
             064, 045, 000]
  blu_SLR = [255, 255, 240, 240, 000, 000, 000, 000, 000, 000, 185, 105, $
             000, 045, 000]

  if (extendedColors eq 1) then begin

;+
;     Snowfall: Color curve working group colors for "Snow Amount
;     (Extended)" but with an additional color for "trace" amounts
;     between 0.0 and 0.1. This change adds several shades of
;     violet/purple for amounts above 48 inches.
;-
      edges_snowfall = [0.0, 1.0e-8, $
                        0.1, 1.0, 2.0, 3.0, $
                        4.0, 6.0, $
                        8.0, 12.0, 18.0, 24.0, 30.0, 36.0, 48.0, 60.0, $
                        72.0, 96.0, 120.0, 500.0]
      tickNames_snowfall = [' ', '0', '0.1', '1', '2', '3', '4', '6', '8', $
                            '12', '18', '24', '30', '36', '48', '60', $
                            '72', '96', '120', '>120']
      red_snowfall = [255, $
                      228, 189, 107, 049, $
                      008, 008, $
                      255, 255, 255, 219, 158, 105, 054, 204, $
                      159, 124, 086, 046]
      grn_snowfall = [255, $
                      238, 215, 174, 130, $
                      081, 038, $
                      255, 196, 135, 020, 000, 000, 000, 204, $
                      140, 082, 028, 000]
      blu_snowfall = [255, $
                      245, 231, 214, 189, $
                      156, 148, $
                      150, 000, 000, 000, 000, 000, 000, 255, $
                      216, 165, 114, 051]

  endif else begin


;+
;     Snowfall: Color curve working group colors, with an additional
;     color for "trace" amounts between 0.0 and 0.1, and an additional
;     bin at the top.
;-
      edges_snowfall = [0.0, 1.0e-8, $
                        0.1, 1.0, 2.0, 3.0, $
                        4.0, 6.0, $
                        8.0, 12.0, 18.0, 24.0, 30.0, 36.0, 48.0, 500.0]
      tickNames_snowfall = [' ', '0', '0.1', '1', '2', '3', '4', '6', '8', $
                            '12', '18', '24', '30', '36', '48', '>48']
      red_snowfall = [255, $
                      228, 189, 107, 049, $
                      008, 008, $
                      255, 255, 255, 219, 158, 105, 043, 076]
      grn_snowfall = [255, $
                      238, 215, 174, 130, $
                      081, 038, $
                      255, 196, 135, 020, 000, 000, 000, 000]
      blu_snowfall = [255, $
                      245, 231, 214, 189, $
                      156, 148, $
                      150, 000, 000, 000, 000, 000, 046, 115]

  endelse

  units_snowfall = 'inches'


;------------------------------;
; 1b. Set analysis parameters. ;
;------------------------------;

  debug_tag = 700

;---------------------------------------;
; 1b-1. Set time and domain parameters. ;
;---------------------------------------;


; Establish time window.

  anlEndDate_GISRS = JULIAN_TO_GISRS_DATE(anlEndDate_Julian)

  windowFinishDate_Julian = anlEndDate_Julian + $
                            DOUBLE(windowHoursForward) / 24.0D
  windowFinishDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(windowFinishDate_Julian)

  windowStartDate_Julian = windowFinishDate_Julian - $
                           DOUBLE(windowHoursBack + windowHoursForward) / 24.0D
  windowStartDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(windowStartDate_Julian)


; Define spatial domain and resolution.

  domainLabel = 'CONUS'

  minLatOut = 21.0D
  maxLatOut = 55.0D
  minLonOut = -126.0D
  maxLonOut = -66.0D

  ;; domainLabel = 'Colorado'

  ;; minLatOut = 37.0D
  ;; maxLatOut = 41.0D
  ;; minLonOut = -109.0D
  ;; maxLonOut = -102.0D

  ;; domainLabel = 'West'

  ;; minLatOut = 37.0D
  ;; maxLatOut = 43.0D
  ;; minLonOut = -110.0D
  ;; maxLonOut = -98.0D

  ;; domainLabel = 'Northeast'
  ;; minLatOut = 39.0D
  ;; maxLatOut = 48.0D
  ;; minLonOut = -87.0D
  ;; maxLonOut = -66.0D


; Output resolution should be an integer fraction of a whole
; degree. Following this suggestion will make life easier for all of us.

  lonResOut = 0.04D
  latResOut = 0.04D


; Confirm resolutions are integer fractions of a degree.

  if (DOUBLE(ROUND(1.0D / lonResOut)) ne (1.0D / lonResOut)) then begin
      ERR_MSG, 'ERROR: Longitudinal resolution must be an integer ' + $
               'fraction of a whole degree.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if (DOUBLE(ROUND(1.0D / latResOut)) ne (1.0D / latResOut)) then begin
      ERR_MSG, 'ERROR: Latitudinal resolution must be an integer ' + $
               'fraction of a whole degree.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  numColsOut = ROUND((maxLonOut - minLonOut) / lonResOut)
  numRowsOut = ROUND((maxLatOut - minLatOut) / latResOut)

  xszt = 1000 ; target window size for plots


; Verify grid geometry is perfect.

  lonErr = ABS(numColsOut * lonResOut - (maxLonOut - minLonOut))
  if (lonErr gt 1.0D-8) then begin
      ERR_MSG, 'ERROR: Inconsistent longitudinal domain/resolution.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  latErr = ABS(numRowsOut * latResOut - (maxLatOut - minLatOut))
  if (latErr gt 1.0D-8) then begin
      ERR_MSG, 'ERROR: Inconsistent latitudinal domain/resolution.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Accommodate kriging at decreased resolution. The krigeResFactor CAN
; be a (double precision) floating point value, and must be set so
; that the resulting resolution is still an integer fraction of a
; degree.

  krigeResFactor = 25.0D / 12.0D
  krigeResFactor = 25.0D / 8.0D   ; 0.04 (1/25th deg) to 1/8 deg
  krigeResFactor = 25.0D / 4.0D   ; 0.04 (1/25th deg) to 1/4 deg
  krigeResFactor = 25.0D / 10.0D  ; 0.04 (1/25th deg) to 1/10 deg
  krigeResFactor = 1.0D
  krigeResFactor = 2.5D

  numColsOutKrige = ROUND((maxLonOut - minLonOut) / $
                          (lonResOut * krigeResFactor))
  numRowsOutKrige = ROUND((maxLatOut - minLatOut) / $
                          (latResOut * krigeResFactor))

  lonResOutKrige = lonResOut * krigeResFactor
  latResOutKrige = latResOut * krigeResFactor


; Verify grid geometry is perfect relative to the kriging resolution.

  lonErr = ABS(numColsOutKrige * lonResOutKrige - (maxLonOut - minLonOut))
  if (lonErr gt 1.0D-8) then begin
      ERR_MSG, 'ERROR: Inconsistent longitudinal domain/resolution for ' + $
               'kriging resolution.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  latErr = ABS(numRowsOutKrige * latResOutKrige - (maxLatOut - minLatOut))
  if (latErr gt 1.0D-8) then begin
      ERR_MSG, 'ERROR: Inconsistent latitudinal domain/resolution for ' + $
               'kriging resolution.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Confirm kriging resolution is an integer fraction of a degree.

  if (DOUBLE(ROUND(1.0D / lonResOutKrige)) ne $
      (1.0D / lonResOutKrige)) then begin
      ERR_MSG, 'ERROR: Longitudinal kriging resolution must be an integer ' + $
               'fraction of a whole degree.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if (DOUBLE(ROUND(1.0D / latResOutKrige)) ne $
      (1.0D / latResOutKrige)) then begin
      ERR_MSG, 'ERROR: Latitudinal kriging resolution must be an integer ' + $
               'fraction of a whole degree.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


;--------------------------------------------------------------------;
; 1b-2. Determine grid points in a U.S. National Boundaries polygon. ;
;--------------------------------------------------------------------;

  debug_tag = 800

;  shapeFileDir = NSAPrefix + '/gisrs/data/common/shapefiles'
  shapeFileDir = vectorDir
  shapeFileName = 'National_Boundary_Coterminous_US'
  shapeFilePath = shapeFileDir + '/' + shapeFileName
  shapeFile = OBJ_NEW('IDLffShape', shapeFilePath + '.shp')
  shapeFile->IDLffShape::GetProperty, N_ATTRIBUTES = numAtts
  shapeFile->IDLffShape::GetProperty, ATTRIBUTE_NAMES = attNames
  ind = WHERE(attNames eq 'NAME', count)
  if (count ne 1) then begin
      ERR_MSG, 'ERROR: Problem reading shapefile "' + shapeFileName + '".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  ind = ind[0]
  shapeFile->IDLffShape::GetProperty, N_ENTITIES = numPolygons
  if (numPolygons ne 1) then begin
      ERR_MSG, 'ERROR: Unexpected structure in shapefile + "' + $
               shapeFileName + '".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  attrib = shapeFile->getAttributes(0)
  name = attrib.(ind)
  shapeIndSavFile = outputParentDir + '/' + $
                    shapeFileName + '_for_sfav2_' + domainLabel + '.sav'
  polygon = shapeFile->IDLffShape::GetEntity(0)
  ind = GRID_POINTS_IN_POLYGON(numColsOut, numRowsOut, $
                               minLonOut, maxLatOut, $
                               lonResOut, latResOut, $
                               polygon, $
                               shapeIndSavFile, $
                               COUNT = numCellsInPolygon)
  OBJ_DESTROY, shapeFile
  flag = BYTARR(numColsOut, numRowsOut)
  flag[ind] = 1B
  flag = ROTATE(flag, 7)
  outsideDomainInd = WHERE(flag eq 0, outsideDomainCount)
  if (outsideDomainCount eq 0) then begin
      ERR_MSG, 'ERROR: (PROGRAMMING) Indexing of outside-domain cells failed.'
      GOTO, BAIL
  endif


;-----------------------------------------;
; 1b-2. Set data assimilation parameters. ;
;-----------------------------------------;

; General variogram parameters.

;  maxLagForCN = 100.0D3         ; filtering distance for correct negatives


; General kriging parameters (taken from environment variables)

;  targetKRMRingWidthMeters = 5.0D3
;  numKRMAngles = 4
  smoothHoodRadM = 50.0D3 ; boxcar smoother "radius" in meters.


; Variogram/Kriging parameters that apply to both passes.

  minAssimPoints = 200      ; 100 failed for 2016100612
;+ GF 20180806
;  minRangeMeters = 100.0D3  ; minimum range for fitted semivariograms
;- GF 20180806
  lagTolMeters = 10.0D3     ; lag bin width for empirical semivariograms
  minLagMeters = 50.0D3     ; kriging requires data within this distance
  minHoodPoints = 8         ; kriging neighborhood must be at least this sizr
  hoodTolerance = 0.0D      ; If the kriging neighborhood range does not
                            ; exceed this value, a simple average is done

;+ GF 20180806
;  if (minLagMeters gt minRangeMeters) then begin
;      ERR_MSG, 'Minimum lag may not exceed the minimum ' + $
;               'allowed fitted variogram range.'
;      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;  endif
;- GF 20180806


; Variogram parameters for 1st pass.

  minRangeMeters_P1 = 100.0D3
  maxLagMeters_P1 = 500.0D3   ; maximum lag / maximum range allowed
  minOutlierDiff_P1 = 0.25  ; minimum absolute difference to check as outlier
                            ; (only used when kriging difference, not ratio)
;+ GF 20180806
  if (minLagMeters gt minRangeMeters_P1) then begin
      ERR_MSG, 'ERROR: Minimum lag may not exceed the minimum ' + $
               'allowed fitted pass 1 variogram range.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
;- GF 20180806


; Variogram parameters for 2nd pass.

  minRangeMeters_P2 = 50.0D3
  maxLagMeters_P2 = 250.0D3 ; maximum lag / maximum range allowed
  minOutlierDiff_P2 = 0.1   ; minimum absolute difference to check as outlier

;+ GF 20180806
  if (minLagMeters gt minRangeMeters_P2) then begin
      ERR_MSG, 'ERROR: Minimum lag may not exceed the minimum ' + $
               'allowed fitted pass 2 variogram range.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
;- GF 20180806


; Kriging with radial means (KRM) parameters.

  staggerKRMMesh = 1
  obsLimitForOK = 600 ; need to have fewer assim points than this to do OK
  obsLimitForOK = 2000 ; need to have fewer assim points than this to do OK
  meanPointsPerSolverLimit = 100 ; need to have fewer mean points per solver than this to do OK
  numSolversLimit = 25000 ; need to have fewer solvers than this to do OK


; By default, analysis is done in two assimilation passes, with
; background:observation ratio (bias) done in the first. If useRatio
; is set to zero, then the first assimilation pass uses differences
; rather than ratios, and no second pass is done.

  useRatio = 1 ; if set, then analysis is done in two assimilation passes


;-------------------------------------------------------------;
; 1b-3. Refine the spatial domain to accommodate assimilation ;
;       parameters.                                           ;
;-------------------------------------------------------------;


; Set precision for DISTANCE calculations.

  dPrecision = 2


; Calculate a lon/lat box associated with maxLagMeters_P1.

; The length of a degree in latitude is smallest at the equator, for the
; typical ellipsoid, so use the 3.6 arc seconds about the equator to
; estimate meters per degree latitude.

  mPerDegLatRef = $
      DOUBLE(FLOOR(DISTANCE(dPrecision, $
                            0.0D, 0.0005D, 0.0D, -0.0005D) * 1000.0D))
  maxLagDegLat = (maxLagMeters_P1 > maxLagMeters_P2) / mPerDegLatRef


; The length of a degree in longitude is smallest at the highest latitude, so
; use that latitude to estimate meters per degree longitude.

  maxLatRef = (ABS(maxLatOut) > ABS(minLatOut)) + maxLagDegLat
  mPerDegLonRef = $
      DOUBLE(FLOOR(DISTANCE(dPrecision, 0.0D, maxLatRef, 1.0D, maxLatRef)))
  maxLagDegLon = (maxLagMeters_P1 > maxLagMeters_P2) / mPerDegLonRef


; The input data domain for this process should, if possible, exceed
; the output domain, so that near the boundaries the influence of
; points that are nearby, but not strictly within the bounds of the
; output domain, can be realized.


; Make sure padLon and padLat are an integer number of output grid cells.

  extraRows = CEIL(maxLagDegLat / (latResOut * krigeResFactor)) * krigeResFactor
  padLat = CEIL(latResOut * extraRows)
  extraRows = ROUND(padLat / latResOut)
  extraCols = CEIL(maxLagDegLon / (lonResOut * krigeResFactor)) * krigeResFactor
  padLon = CEIL(lonResOut * extraCols)
  extraCols = ROUND(padLon / lonResOut)

  extraRows = CEIL(maxLagDegLat / (latResOut * krigeResFactor))
  padLat = CEIL(latResOut * krigeResFactor * extraRows)
  extraRows = ROUND(padLat / latResOut)
  extraCols = CEIL(maxLagDegLon / (lonResOut * krigeResFactor)) * krigeResFactor
  padLon = CEIL(lonResOut * extraCols)
  extraCols = ROUND(padLon / lonResOut)


; Verify as above for low resolution.

  if (DOUBLE(extraCols / krigeResFactor) ne $
      (DOUBLE(extraCols) / DOUBLE(krigeResFactor))) then begin
;;       ERR_MSG, 'WARNING: additional longitudinal domain padding needed ' + $
;;                'to accommodate kriging resolution factor.'
;; stop
     ERR_MSG, 'ERROR: Longitudinal analysis domain expansion ' + $
              'inconsistent with reduced resolution for kriging.'
     if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  while (DOUBLE(extraCols / krigeResFactor) ne $
         (DOUBLE(extraCols) / DOUBLE(krigeResFactor))) do begin
      extraCols = extraCols + 1
      padLon = lonResOut * extraCols
  endwhile

  if (DOUBLE(extraRows / krigeResFactor) ne $
      (DOUBLE(extraRows) / DOUBLE(krigeResFactor))) then begin
;;       ERR_MSG, 'WARNING: additional latitudinal domain padding needed ' + $
;;                'to accommodate kriging resolution factor.'
;; stop
      ERR_MSG, 'ERROR: Latitudinal analysis domain expansion ' + $
               'inconsistent with reduced resolution for kriging.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  while (DOUBLE(extraRows / krigeResFactor) ne $
         (DOUBLE(extraRows) / DOUBLE(krigeResFactor))) do begin
      extraRows = extraRows + 1
      padLat = latResOut * extraRows
  endwhile


; We call the expansion of the output domain the "analysis" domain.

  minLonAnl = minLonOut - padLon
  maxLonAnl = maxLonOut + padLon
  minLatAnl = minLatOut - padLat
  maxLatAnl = maxLatOut + padLat

  numColsAnl = ROUND((maxLonAnl - minLonAnl) / lonResOut)
  numRowsAnl = ROUND((maxLatAnl - minLatAnl) / latResOut)

  if ((numColsAnl - numColsOut) ne (2 * extraCols)) $
      then begin                    ; PROGRAMMING ERROR
      ERR_MSG, 'ERROR: Fatal programming error. Analysis domain expansion ' + $
               'failure in the longitudinal dimension.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if ((numRowsAnl - numRowsOut) ne (2 * extraRows)) $
      then begin                    ; PROGRAMMING ERROR
      ERR_MSG, 'ERROR: Fatal programming error. Analysis domain expansion ' + $
               'failure in the latitudinal dimension.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  numColsAnlKrige = ROUND((maxLonAnl - minLonAnl) / $
                          (lonResOut * krigeResFactor))
  numRowsAnlKrige = ROUND((maxLatAnl - minLatAnl) / $
                          (latResOut * krigeResFactor))


; Verify grid geometry is perfect.

  lonErr = ABS(numColsAnl * lonResOut - (maxLonAnl - minLonAnl))
  if (lonErr gt 1.0D-8) then begin
      ERR_MSG, 'ERROR: Analysis domain has inconsistent longitudinal ' + $
               'domain/resolution.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  latErr = ABS(numRowsAnl * latResOut - (maxLatAnl - minLatAnl))
  if (latErr gt 1.0D-8) then begin
      ERR_MSG, 'ERROR: Analysis domain has inconsistent latitudinal ' + $
               'domain/resolution.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if verbose then $
      USR_MSG, 'Output domain plus analysis halo: ' + $
               'longitude ' + STRCRA(minLonAnl) + ' to ' + $
               STRCRA(maxLonAnl) + ', ' + $
               'latitude ' + STRCRA(minLatAnl) + ' to ' + $
               STRCRA(maxLatAnl) + ', ' + $
               'lon/lat resolution ' + $
               STRCRA(lonResOut) + ' / ' + STRCRA(latResOut)


;=============================================;
; 2. Perform background (1st guess) analysis. ;
;=============================================;

  debug_tag = 1000

  ;; PRINT, QPE___GRID ; INTENTIONAL ERROR GF 20200417

  if verbose then USR_MSG, '--- 2. BACKGROUND ANALYSIS ---'

;-------------------------------------------------------------;
; 2a. Get initial APCP and WEASD background precip/snow data. ;
;-------------------------------------------------------------;

  WEASDGrid = !NULL
  APCPGrid = !NULL

  if verbose then USR_MSG, 'Getting background APCP and WEASD from HRRR.'

  targetFcstHour_QPF = 3  ; ideal forecast hour (3 = hours 2-3 of cycle)
  minSubFcstHour_QPF = 1  ; minimum substitute forecast hour
  maxSubFcstHour_QPF = 13 ; maximum substitute forecast hour

;+
; The QPF_source will be used in plot titles, file names, and
; diagnostic results that summarize this model run. It should be
; defined so it can accommodate all of those (e.g., avoid spaces and
; special characters).
;-
  QPF_source_base = 'HRRR'
  QPF_source = 'HRRR'
  QPF_source_noSpace = 'HRRR'

  HRRRDir = NSAPrefix + '/misc/gisrs_incoming/HRRR'

  GET_ACCUM_HRRR_APCP_WEASD, anlEndDate_YYYYMMDDHH, $
                             duration, $
                             targetFcstHour_QPF, $
                             minSubFcstHour_QPF, $
                             maxSubFcstHour_QPF, $
                             HRRRDir, $
                             scratchDir, $
                             minLonAnl, $
                             maxLonAnl, $
                             minLatAnl, $
                             maxLatAnl, $
                             lonResOut, $
                             latResOut, $
                             ndv, $
                             WEASDGrid, $
                             APCPGrid, $
                             perfect, $
                             HRRR_GRID_PROJ_INFO = HRRRGridProjInfo, $
                             MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
                             MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
                             VERBOSE = verbose

  if ((NOT(ISA(WEASDGrid)) or $
       NOT(ISA(APCPGrid))) and $
      FILE_TEST(archiveDir + '/HRRR_archive', /DIRECTORY, /READ)) $
      then begin

      if verbose then USR_MSG, 'HRRR QPF not found in ' + HRRRDir

;+
;     Try HRRR in development archives.
;-
      HRRRDir = archiveDir + '/HRRR_archive'

      if verbose then USR_MSG, 'Looking for HRRR QPF in ' + HRRRDir

      GET_ACCUM_HRRR_APCP_WEASD, anlEndDate_YYYYMMDDHH, $
                                 duration, $
                                 targetFcstHour_QPF, $
                                 minSubFcstHour_QPF, $
                                 maxSubFcstHour_QPF, $
                                 HRRRDir, $
                                 scratchDir, $
                                 minLonAnl, $
                                 maxLonAnl, $
                                 minLatAnl, $
                                 maxLatAnl, $
                                 lonResOut, $
                                 latResOut, $
                                 ndv, $
                                 WEASDGrid, $
                                 APCPGrid, $
                                 perfect, $
                                 HRRR_GRID_PROJ_INFO = HRRRGridProjInfo, $
                                 MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
                                 MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
                                 VERBOSE = verbose

      if (NOT(ISA(WEASDGrid)) or $
          NOT(ISA(APCPGrid)) and $
          verbose) then $
              USR_MSG, 'HRRR QPF not found in ' + HRRRDir

  endif

  if (NOT(ISA(WEASDGrid)) or $
      NOT(ISA(APCPGrid))) then begin
      
;+
;     Try RAP.
;-
      if verbose then $
          USR_MSG, 'HRRR QPF not available. Trying RAP.'

      QPF_source_base = 'RAP'
      QPF_source = 'RAP'
      QPF_source_noSpace = 'RAP'

      RAPDir = NSAPrefix + '/misc/gisrs_incoming/model'

      GET_ACCUM_RAP_APCP_WEASD, $
          anlEndDate_YYYYMMDDHH, $
          duration, $
          targetFcstHour_QPF, $
          minSubFcstHour_QPF, $
          maxSubFcstHour_QPF, $
          RAPDir, $
          scratchDir, $
          minLonAnl, $
          maxLonAnl, $
          minLatAnl, $
          maxLatAnl, $
          lonResOut, $
          latResOut, $
          ndv, $
          APCPGrid, $
          WEASDGrid, $
          perfect, $
          RAP_GRID_PROJ_INFO = RAPGridProjInfo, $
          MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
          MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
          VERBOSE = verbose

      if ((NOT(ISA(APCPGrid)) or $
           NOT(ISA(WEASDGrid))) and $
          FILE_TEST(archiveDir + '/RAP_archive', /DIRECTORY, /READ)) $
          then begin

          if verbose then USR_MSG, 'RAP QPF not found in ' + RAPDir

;+
;         Try RAP in development archives.
;-
          RAPDir = archiveDir + '/RAP_archive'

          if verbose then USR_MSG, 'Looking for RAP QPF in ' + RAPDir

          GET_ACCUM_RAP_APCP_WEASD, $
              anlEndDate_YYYYMMDDHH, $
              duration, $
              targetFcstHour_QPF, $
              minSubFcstHour_QPF, $
              maxSubFcstHour_QPF, $
              RAPDir, $
              scratchDir, $
              minLonAnl, $
              maxLonAnl, $
              minLatAnl, $
              maxLatAnl, $
              lonResOut, $
              latResOut, $
              ndv, $
              APCPGrid, $
              WEASDGrid, $
              perfect, $
              RAP_GRID_PROJ_INFO = RAPGridProjInfo, $
              MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
              MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
              VERBOSE = verbose

          if (NOT(ISA(APCPGrid)) or $
              NOT(ISA(WEASDGrid)) and $
              verbose) then $
                  USR_MSG, 'RAP QPF not found in ' + RAPDir

      endif

;     GF 20190802 - should this be outside the if-block for RAP so it
;                   can be applied if the source is also HRRR?
      if (ISA(APCPGrid) and ISA(WEASDGrid)) then begin

          QPF_source = QPF_source + ' f' + $
                       STRING(minFcstHourFound, FORMAT = '(I2.2)')
          QPF_source_noSpace = QPF_source_noSpace + '_f' + $
                               STRING(minFcstHourFound, FORMAT = '(I2.2)')

          if (minFcstHourFound ne maxFcstHourFound) then begin
              QPF_source = QPF_source + ' to f' + $
                           STRING(maxFcstHourFound, FORMAT = '(I2.2)')
              QPF_source_noSpace = QPF_source_noSpace + '_to_f' + $
                                   STRING(maxFcstHourFound, FORMAT = '(I2.2)')
          endif

      endif

  endif else begin

;+
;    GF 20190805
;    Identify forecast choice for HRRR. This code is repeated three
;    times and we should probably find a way to put it all in one
;    location, but this is the smallest code change to make it work
;    correctly today.
;-
      QPF_source = QPF_source + ' f' + $
                   STRING(minFcstHourFound, FORMAT = '(I2.2)')
      QPF_source_noSpace = QPF_source_noSpace + '_f' + $
                           STRING(minFcstHourFound, FORMAT = '(I2.2)')

      if (minFcstHourFound ne maxFcstHourFound) then begin
          QPF_source = QPF_source + ' to f' + $
                       STRING(maxFcstHourFound, FORMAT = '(I2.2)')
          QPF_source_noSpace = QPF_source_noSpace + '_to_f' + $
                               STRING(maxFcstHourFound, FORMAT = '(I2.2)')
      endif

  endelse

  debug_tag = 1100

;+ GF 20180705
  if ((NOT(ISA(WEASDGrid)) or $
       NOT(ISA(APCPGrid))) and $
      FILE_TEST(archiveDir + '/RUC_archive', /DIRECTORY, /READ)) $
    then begin

;+
;     Try RUC in development archives. Unlike HRRR and RAP, which were
;     operational as of 2018-07-05 when this code was written, RUC has
;     been out of operations since mid-2012, so an initial look for
;     operational cached files in /operations/misc does not make
;     sense.
;-
      if verbose then USR_MSG, 'RAP QPF not available. Trying RUC.'

      WEASDGrid = !NULL
      APCPGrid = !NULL

      QPF_source_base = 'RUC'
      QPF_source = 'RUC'
      QPF_source_noSpace = 'RUC'

      RUCDir = archiveDir + '/RUC_archive'

      if verbose then USR_MSG, 'Looking for RUC QPF in ' + RUCDir

      GET_ACCUM_RUC_ACPCP_NCPCP_WEASD, $
          anlEndDate_YYYYMMDDHH, $
          duration, $
          targetFcstHour_QPF, $
          minSubFcstHour_QPF, $
          maxSubFcstHour_QPF, $
          RUCDir, $
          scratchDir, $
          minLonAnl, $
          maxLonAnl, $
          minLatAnl, $
          maxLatAnl, $
          lonResOut, $
          latResOut, $
          ndv, $
          ACPCPGrid, $
          NCPCPGrid, $
          WEASDGrid, $
          perfect, $
          RUC_GRID_PROJ_INFO = RUCGridProjInfo, $
          MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
          MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
          VERBOSE = verbose

      if (NOT(ISA(WEASDGrid)) or $
          NOT(ISA(ACPCPGrid)) or $
          NOT(ISA(NCPCPGrid))) then begin

          if verbose then USR_MSG, 'RUC QPF not found in ' + RUCDir

      endif else begin

          QPF_source = QPF_source + ' f' + $
                       STRING(minFcstHourFound, FORMAT = '(I2.2)')
          QPF_source_noSpace = QPF_source_noSpace + '_f' + $
                               STRING(minFcstHourFound, FORMAT = '(I2.2)')

          if (minFcstHourFound ne maxFcstHourFound) then begin
              QPF_source = QPF_source + ' to f' + $
                           STRING(maxFcstHourFound, FORMAT = '(I2.2)')
              QPF_source_noSpace = QPF_source_noSpace + '_to_f' + $
                                   STRING(maxFcstHourFound, FORMAT = '(I2.2)')
          endif

      endelse

      if (ISA(WEASDGrid) and $
          ISA(ACPCPGrid) and $
          ISA(NCPCPGrid)) then begin


;         Combine large scale (ACPCP) and convective (NCPCP)
;         precipitation into total (APCP).

          ind = WHERE((ACPCPGrid eq ndv) or (NCPCPGrid eq ndv), count)
          APCPGrid = ACPCPGrid + NCPCPGrid
          if (count gt 0) then APCPGrid[ind] = ndv

          ACPCPGrid = !NULL
          NCPCPGrid = !NULL

      endif

  endif
;- GF 20180705

  debug_tag = 1200

  if (NOT(ISA(WEASDGrid)) and NOT(ISA(APCPGrid))) then begin

      ;; if skipQPEAnalysis then begin

      ;;     ERR_MSG, 'Failed to generate QPF grids. Since this run is ' + $
      ;;              'configured to skip the QPE analysis, no snowfall ' + $
      ;;              'background can be generated. Analysis not possible.'

      ;;     if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL

      ;; endif else begin

      USR_MSG, 'NOTICE: Unable to generate QPF grids. Initializing to zero.'

      ;; endelse

      QPF_source_base = 'Missing'
      QPF_source = 'Missing'
      APCPGrid = MAKE_ARRAY(numColsAnl, numRowsAnl, VALUE = 0.0)
      WEASDGrid = MAKE_ARRAY(numColsAnl, numRowsAnl, VALUE = 0.0)

  endif ; else begin

;; ;+
;; ;     Define QPFDir, which will be needed for the call to
;; ;     SUBDIVIDE_SNOWFALL_OBS later. HRRRDir, RAPDir, and RUCDir may
;; ;     all be given new values in the temperature section that precedes
;; ;     the call to SUBDIVIDE_SNOWFALL_OBS.
;; ;-
;;       case QPF_source_base of
;;           'HRRR' : QPFDir = HRRRDir
;;           'RAP' : QPFDir = RAPDir
;;           'RUC' : QPFDir = RUCDir
;;       endcase

;;   endelse

  if NOT(ISA(WEASDGrid)) then begin
      ERR_MSG, 'ERROR: Failed to generate WEASD grid even though ' + $
               'APCP grid exists.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if NOT(ISA(APCPGrid)) then begin
      ERR_MSG, 'ERROR: Failed to generate APCP grid even though ' + $
               'WEASD grid exists.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Initialize the "ok data" mask.

  okGrid = (WEASDGrid ne ndv) * (APCPGrid ne ndv)
  ndvInd = WHERE(okGrid eq 0, count)
  if (count gt 0) then begin
      WEASDGrid[ndvInd] = ndv
      APCPGrid[ndvInd] = ndv
  endif


; Verify that WEASD never exceeds APCP by a significant amount.

  ind = WHERE((WEASDGrid ne ndv) and $
              (APCPGrid ne ndv) and $
              (WEASDGrid gt APCPGrid), count)
  if (count gt 0) then begin
      maxWEASDExceedance = MAX(WEASDGrid[ind] - APCPGrid[ind])
      if (maxWEASDExceedance gt 0.1) then begin
          ERR_MSG, 'WARNING: WEASD exceeds APCP in ' + STRCRA(count) + $
                   ' cells. Capping WEASD at APCP value.'
;          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
;          ERR_MSG, 'WARNING: minor WEASD > APCP errors adjusted in ' + $
;                   STRCRA(count) + ' cells.'
      WEASDGrid[ind] = APCPGrid[ind]
  endif

  debug_tag = 1300

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  subTitle = STRCRA(duration) + '-hr acc. ending ' + $
             anlEndDate_YYYYMMDDHH + ' UTC'
  longSubTitle = STRCRA(duration) + '-hour accumulation ending ' + $
                 anlEndDate_GISRS + ' UTC'

  if verbose then begin

      SET_PLOT, 'X'


;     Display accumulated APCP and WEASD.

      X_MAP_GEO_GRID, WEASDGrid, $
                      minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                      edges_QPF, red_QPF, grn_QPF, blu_QPF, $
                      3, $
                      status, $
                      NDV = ndv, $
                      /SHOW_HIGH, $
                      TITLE = QPF_source + ' Snowfall Water Equivalent ' + $
                      '(WEASD), ' + subTitle, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'inches', $
                      SHAPE_PATH_LIST = shapePathList


      X_MAP_GEO_GRID, APCPGrid, $
                      minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                      edges_QPF, red_QPF, grn_QPF, blu_QPF, $
                      2, $
                      status, $
                      NDV = ndv, $
                      /SHOW_HIGH, $
                      TITLE = QPF_source + ' Total Precipitation ' + $
                      '(APCP), ' + subTitle, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'mm', $
                      SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write APCP and WEASD to images.

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_01a_' + QPF_source_noSpace + '_WEASD.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          WEASDGrid[extraCols:numColsAnl - extraCols - 1, $
                    extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_QPF, red_QPF, grn_QPF, blu_QPF, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          QPF_source + ' Snowfall Water Equivalent ' + $
          '(WEASD)!C' + longSubTitle, $
          'mm', $
          outputDir + '/' + PNGFile, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_QPF, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_01b_' + QPF_source_noSpace + '_QPF.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          APCPGrid[extraCols:numColsAnl - extraCols - 1, $
                   extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_QPF, red_QPF, grn_QPF, blu_QPF, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          QPF_source + ' Total Precipitation ' + $
          '(APCP)!C' + longSubTitle, $
          'mm', $
          outputDir + '/' + PNGFile, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_QPF, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

  endif

  if produceTIFFs then begin


;     Write APCP and WEASD to GeoTIFF.

      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_' + QPF_source_noSpace + '_WEASD.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(WEASDGrid, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if (updateGISProject) then begin
          ;; cmd = 'ln -fs ' + $
          ;;       '../' + outputDir + '/' + TIFFFile + ' ' + $
          ;;       GISProjectDir + '/' + $
          ;;       'sfav2_' + domainLabel + '_HRRR_WEASD.tif'
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $ 
                     'sfav2_' + domainLabel + '_' + QPF_source_noSpace + $
                     '_WEASD.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_' + QPF_source_noSpace + '_QPF.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(APCPGrid, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if (updateGISProject) then begin
          ;; cmd = 'ln -fs ' + $
          ;;       '../' + outputDir + '/' + TIFFFile + ' ' + $
          ;;       GISProjectDir + '/' + $
          ;;       'sfav2_' + domainLabel + '_HRRR_QPF.tif'
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_' + QPF_source_noSpace + $
                     '_QPF.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s after reading QPF:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


;------------------------------------------------------;
; 2b. Get reference QPE for bias and error evaluation. ;
;------------------------------------------------------;

  debug_tag = 1400

  QPEGrid = !NULL

  if NOT(skipQPEAnalysis) then begin

      if verbose then USR_MSG, 'Getting Stage IV QPE.'

      StageIVDir = NSAPrefix + '/misc/gisrs_incoming/RADAR'

      GET_ACCUM_STAGE4_LONLAT_QPE, anlEndDate_YYYYMMDDHH, $
                                   duration, $
                                   StageIVDir, $
                                   scratchDir, $
                                   minLonAnl, $
                                   maxLonAnl, $
                                   minLatAnl, $
                                   maxLatAnl, $
                                   lonResOut, $
                                   latResOut, $
                                   ndv, $
                                   QPEGrid, $
                                   HRAP_GRID_PROJ_INFO = HRAPGridProjInfo, $
                                   VERBOSE = verbose

      if (NOT(ISA(QPEGrid)) and $
          FILE_TEST(archiveDir + '/StageIV_archive', /DIRECTORY, /READ)) $
          then begin

          if verbose then $
              USR_MSG, 'Stage IV QPE not found in ' + StageIVDir

;+
;         Try Stage IV in development archives.
;-
          StageIVDir = archiveDir + '/StageIV_archive'

          if verbose then USR_MSG, 'Looking for Stage IV QPE in ' + StageIVDir

          GET_ACCUM_STAGE4_LONLAT_QPE, anlEndDate_YYYYMMDDHH, $
                                       duration, $
                                       StageIVDir, $
                                       scratchDir, $
                                       minLonAnl, $
                                       maxLonAnl, $
                                       minLatAnl, $
                                       maxLatAnl, $
                                       lonResOut, $
                                       latResOut, $
                                       ndv, $
                                       QPEGrid, $
                                       HRAP_GRID_PROJ_INFO = HRAPGridProjInfo

          if NOT(ISA(QPEGrid)) then begin
              USR_MSG, 'NOTICE: Unable to read StageIV QPE.'
              if (QPF_source eq 'Missing') then begin
                  ERR_MSG, 'WARNING: no QPE and no QPF - the snowfall ' + $
                           'background will be all zeroes.'
;              ERR_MSG, 'With no QPE and no QPF, no snowfall background ' + $
;                       'can be generated.'
;              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
              endif
          endif else begin
              if verbose then USR_MSG, 'Found Stage IV QPE in ' + StageIVDir
          endelse

      endif

      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          errMsg = ['Integer divide by zero', $
                    'Integer overflow', $
                    'Unspecified error 2^2', $
                    'Unspecified error 2^4', $
                    'Floating divide by zero', $
                    'Floating underflow', $
                    'Floating overflow', $
                    'Floating Illegal operand']
          ERR_MSG, 'Unhandled math error/s after reading QPE:'
          for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
              ERR_MSG, errMsg[i]
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

  endif

  debug_tag = 1500

  if ISA(QPEGrid) then begin

;+
;     Verify the existence of the RFC boundaries shapefile. It is used
;     to check data RFC-by-RFC later (see "check for good data in all
;     RFCs"), but checking for the shapefile here allows us to
;     use those boundaries in graphics.
;-
      shapeFileName = 'RFC_Boundaries_Coterminous_US'
      shapeFilePath = vectorDir + '/' + shapeFileName
      if NOT(FILE_TEST(shapeFilePath + '.shp')) then begin
          ERR_MSG, 'ERROR: RFC bounds shapefile ' + $
                   shapeFilePath + '.shp not found.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

      if verbose then begin


;         Display the QPE grid.

          X_MAP_GEO_GRID, QPEGrid, $
                          minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                          edges_QPF, red_QPF, grn_QPF, blu_QPF, $
                          1, $
                          status, $
                          NDV = ndv, $
                          /SHOW_HIGH, $
                          TITLE = 'Stage IV QPE, ' + subTitle, $
                          /COLORBAR, $
                          XSIZE_TARGET = xszt, $
                          UNITS = 'mm', $
                          SHAPE_PATH_LIST = [shapePathList, $
                                             shapeFilePath]

      endif

      if produceImages then begin


;         Write QPE to an image.

          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_02_StageIV_QPE.png'

          MAKE_LON_LAT_MAP_PNG_SFAV2, $
              QPEGrid[extraCols:numColsAnl - extraCols - 1, $
                      extraRows:numRowsAnl - extraRows - 1], $
              ndv, $
              edges_QPF, red_QPF, grn_QPF, blu_QPF, $
              lonResOut, minLonOut, maxLonOut, $
              latResOut, minLatOut, maxLatOut, $
              'Stage IV QPE!C' + longSubTitle, $
              'mm', $
              outputDir + '/' + PNGFile, $
              /SHOW_HIGH, $
              TICK_NAMES = tickNames_QPF, $
              /NO_GRID, /NO_CONTINENTS, /NO_USA, $
              /BLACK_ON_WHITE, $
              MAP_SHAPE_PATH = [shapePathList, $
                                shapeFilePath]

      endif

      if produceTIFFs then begin


;         Write QPE to GeoTIFF.

          TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                     anlEndDate_YYYYMMDDHH + $
                     '_StageIV_QPE.tif'

          MAKE_GEOTIFF_FROM_GRID, ROTATE(QPEGrid, 7), $
                                  minLonAnl, $
                                  maxLatAnl, $
                                  lonResOut, $
                                  latResOut, $
                                  outputDir + '/' + TIFFFile, $
                                  NO_DATA_VALUE = ndv, $
                                  COMPRESS = 1

          if (updateGISProject) then begin
              ;; cmd = 'ln -fs ' + $
              ;;       '../' + outputDir + '/' + TIFFFile + ' ' + $
              ;;       GISProjectDir + '/' + $
              ;;       'sfav2_' + domainLabel + '_StageIV_QPE.tif'
              origFile = outputDir + '/' + TIFFFile
              copyFile = GISProjectDir + '/' + $
                         'sfav2_' + domainLabel + '_StageIV_QPE.tif'
              if FILE_TEST(copyFile) then begin
                  cmd = 'rm -f ' + copyFile
                  SPAWN, cmd, EXIT_STATUS = status
                  if (status ne 0) then $
                      ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
              cmd = 'cp -f ' + origFile + ' ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

      endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

;+ GF 20180823 - Moved QPE good-data check below section for reading
;                temperatures, so we can set QPE to zero in missing
;                areas that are too warm for snowfall anyway.
;     Check for good data in all RFCs.

;;       if verbose then $
;;           USR_MSG, 'Checking Stage IV QPE against RFC boundaries.'

;;       shapeFileName = 'RFC_Boundaries_Coterminous_US'
;;       shapeFilePath = vectorDir + '/' + shapeFileName
;;       if NOT(FILE_TEST(shapeFilePath + '.shp')) then begin
;;           ERR_MSG, 'RFC bounds shapefile ' + $
;;                    shapeFilePath + '.shp not found.'
;;           if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;;       endif

;;       shapeFile = OBJ_NEW('IDLffShape', shapeFilePath + '.shp')
;;       shapeFile->IDLffShape::GetProperty, N_ATTRIBUTES = numAtts
;;       shapeFile->IDLffShape::GetProperty, ATTRIBUTE_NAMES = attNames
;;       attIndNAME = WHERE(attNames eq 'NAME', count)
;;       if (count ne 1) then begin
;;           ERR_MSG, 'Problem reading shapefile "' + shapeFileName + '".'
;;           OBJ_DESTROY, shapeFile
;;           if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;;       endif
;;       attIndNAME = attIndNAME[0]
;;       shapeFile->IDLffShape::GetProperty, N_ENTITIES = numPolygons
;;       if (numPolygons ne 12) then begin
;;           ERR_MSG, 'Expected 12 polygons in shapefile "' + shapeFileName + $
;;                    '", found ' + STRCRA(numPolygons) + '.'
;;           OBJ_DESTROY, shapeFile
;;           if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;;       endif
;;       for rfc = 0, numPolygons - 1 do begin
;;           attrib = shapeFile->getAttributes(rfc)
;;           RFCName = attrib.(attIndNAME)
;; ;          if verbose then USR_MSG, 'Checking ' + RFCName
;;           ;; shapeIndSavFile = $
;;           ;;     outputParentDir + '/' + RFCName + '_' + $
;;           ;;     shapeFileName + '_for_sfav2_' + domainLabel + '.sav'
;;           shapeIndSavFile = resourcesDir + '/' + $
;;                             shapeFileName + $
;;                             '_for_sfav2_' + domainLabel + '_' + $
;;                             RFCName + '.sav'
;;           polygon = shapeFile->IDLffShape::GetEntity(rfc)
;;           ind = GRID_POINTS_IN_POLYGON(numColsAnl, numRowsAnl, $
;;                                        minLonAnl, maxLatAnl, $
;;                                        lonResOut, latResOut, $
;;                                        polygon, $
;;                                        shapeIndSavFile, $
;;                                        COUNT = numCellsInPolygon) ; north-down
;;           if (numCellsInPolygon eq 0) then begin
;;               ERR_MSG, 'Found no analysis domain cells in "' + $
;;                        RFCName + '".'
;;               OBJ_DESTROY, shapeFile
;;               if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;;           endif

;;           killme = ROTATE(QPEGrid, 7) ; QPE made north-down
;;           QPEInRFC = killme[ind]      ; RFC data sampled from QPE
;;           killme = !NULL

;;           okInd = WHERE(QPEInRFC ne ndv, okCount)

;;           numCellsMissing = numCellsInPolygon - okCount
;;           proportionMissing = $
;;               FLOAT(numCellsMissing) / FLOAT(numCellsInPolygon)

;;           if (proportionMissing gt 0.025) then begin

;;               ERR_MSG, 'QPE data for ' + RFCName + ' is missing data in ' + $
;;                        STRCRA(numCellsMissing) + $
;;                        ' (' + STRCRA(proportionMissing * 100) + $
;;                        '%) of its cells. Data are rejected.'
;;               QPEGrid = !NULL
;;               BREAK

;;           endif

;;       endfor

;;       OBJ_DESTROY, shapeFile
;- GF 20180823

  endif


;--------------------------------------------------------------------;
; 2c. Get minimum, maximum, and average 2 meter air temperatures for ;
;     the analysis period.                                           ;
;--------------------------------------------------------------------;

  debug_tag = 1600

  if verbose then USR_MSG, 'Getting min./max./ave. temperatures.'

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s before reading temperatures:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

;+
; Try getting SNODAS temperatures.
;-
  temperature_source = 'SNODAS'

  GET_MIN_MAX_AVE_2M_SNODAS_TEMP, anlEndDate_YYYYMMDDHH, $
                                  duration, $
                                  scratchDir, $
                                  PGHost, $
                                  minLonAnl, maxLonAnl, $
                                  minLatAnl, maxLatAnl, $
                                  lonResOut, latResOut, $
                                  ndv, $
                                  minTempGrid, $
                                  maxTempGrid, $
                                  aveTempGrid, $
                                  VERBOSE = verbose

  if (NOT(ISA(minTempGrid)) or $
      NOT(ISA(maxTempGrid)) or $
      NOT(ISA(aveTempGrid))) then $
          maxMissingTempHours = ROUND(0.2 * duration)

  debug_tag = 1700

  if ((NOT(ISA(minTempGrid)) or $
       NOT(ISA(maxTempGrid)) or $
       NOT(ISA(aveTempGrid))) and $
      FILE_TEST(archiveDir + '/RTMA_archive', /DIRECTORY, /READ)) $
    then begin

;+
;     Try getting 2.5 km RTMA temperatures from development archives.
;-
      temperature_source = 'RTMA (2.5 km)'
      RTMADir = archiveDir + '/RTMA_archive'

      RTMA2p5EraDate_YYYYMMDDHH = '2010092900'
      RTMA2p5EraDate_Julian = YYYYMMDDHH_TO_JULIAN(RTMA2p5EraDate_YYYYMMDDHH)

      if (anlEndDate_Julian ge RTMA2p5EraDate_Julian) then begin

          if verbose then $
              USR_MSG, 'SNODAS temperatures not available. ' + $
                       'Trying for 2.5 km RTMA in ' + RTMADir

          GET_MIN_MAX_AVE_RTMA2P5_2M_TEMP, $
              anlEndDate_YYYYMMDDHH, $
              duration, $
              maxMissingTempHours, $
              RTMADir, $
              scratchDir, $
              minLonAnl, maxLonAnl, $
              minLatAnl, maxLatAnl, $
              lonResOut, latResOut, $
              ndv, $
              minTempGrid, $
              maxTempGrid, $
              aveTempGrid, $
              perfect, $
              NUM_MISSING_HOURS = numMissingHours, $
              VERBOSE = verbose

          if (verbose and $
              ISA(minTempGrid) and $
              ISA(maxTempGrid) and $
              ISA(aveTempGrid) and $
              (numMissingHours gt 0)) then $
              USR_MSG, 'NOTICE: Missing ' + STRCRA(numMissingHours) + $
                       ' hour/s in ' + temperature_source + $
                       ' min/max/ave temperature calculation.'

      endif

  endif ; end of 2.5 km RTMA temperatures (from development archives) section

  debug_tag = 1800

  if ((NOT(ISA(minTempGrid)) or $
       NOT(ISA(maxTempGrid)) or $
       NOT(ISA(aveTempGrid))) and $
      FILE_TEST(archiveDir + '/RTMA_archive', /DIRECTORY, /READ)) $
    then begin

;+
;     Try getting 5 km RTMA temperatures from development archives.
;-
      temperature_source = 'RTMA (5 km)'
      RTMADir = archiveDir + '/RTMA_archive'

      if verbose then begin
          if (anlEndDate_Julian ge RTMA2p5EraDate_Julian) then $
              USR_MSG, 'RTMA 2.5 km temperatures not available. ' + $
                       'Trying for 5 km RTMA in ' + RTMADir $
          else $
              USR_MSG, 'SNODAS temperatures not available. ' + $
                       'Trying for 5 km RTMA in ' + RTMADir
      endif

      GET_MIN_MAX_AVE_RTMA5_2M_TEMP, anlEndDate_YYYYMMDDHH, $
                                     duration, $
                                     maxMissingTempHours, $
                                     RTMADir, $
                                     scratchDir, $
                                     minLonAnl, maxLonAnl, $
                                     minLatAnl, maxLatAnl, $
                                     lonResOut, latResOut, $
                                     ndv, $
                                     minTempGrid, $
                                     maxTempGrid, $
                                     aveTempGrid, $
                                     perfect, $
                                     NUM_MISSING_HOURS = numMissingHours, $
                                     VERBOSE = verbose

      if (verbose and $
          ISA(minTempGrid) and $
          ISA(maxTempGrid) and $
          ISA(aveTempGrid) and $
          (numMissingHours gt 0)) then $
          USR_MSG, 'NOTICE: Missing ' + STRCRA(numMissingHours) + $
                   ' hour/s in ' + temperature_source + $
                   ' min/max/ave temperature calculation.'

  endif ; end of 5 km RTMA temperatures (from development archives) section

  debug_tag = 1900

  if (NOT(ISA(minTempGrid)) or $
      NOT(ISA(maxTempGrid)) or $
      NOT(ISA(aveTempGrid))) then begin

;+
;     No additionally assimilated analysis temperatures were
;     found. Use NWP analyses and, if necessary, forecasts.
;-
      targetFcstHour_temp = 0   ; ideal forecast hour (3 = hours 2-3 of cycle)
      minSubFcstHour_temp = 1   ; minimum substitute forecast hour
      maxSubFcstHour_temp = 12  ; maximum substitute forecast hour

;+
;     Try getting HRRR temperatures from operations cache.
;-
      if verbose then $
          USR_MSG, 'RTMA temperatures not available. Trying HRRR.'

      temperature_source = 'HRRR'

      HRRRDir = NSAPrefix + '/misc/gisrs_incoming/HRRR'
      
      GET_MIN_MAX_AVE_HRRR_2M_TEMP, $
          anlEndDate_YYYYMMDDHH, $
          duration, $
          targetFcstHour_temp, $
          minSubFcstHour_temp, $
          maxSubFcstHour_temp, $
          HRRRDir, $
          scratchDir, $
          minLonAnl, $
          maxLonAnl, $
          minLatAnl, $
          maxLatAnl, $
          lonResOut, $
          latResOut, $
          ndv, $
          minTempGrid, $
          maxTempGrid, $
          aveTempGrid, $`
          perfect, $
          HRRR_GRID_PROJ_INFO = HRRRGridProjInfo, $
          MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
          MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
          MAX_MISSING_HOURS = maxMissingTempHours, $
          NUM_MISSING_HOURS = numMissingHours, $
          VERBOSE = verbose

      if ((NOT(ISA(minTempGrid)) or $
           NOT(ISA(maxTempGrid)) or $
           NOT(ISA(aveTempGrid))) and $
          FILE_TEST(archiveDir + '/HRRR_archive', /DIRECTORY, /READ)) $
          then begin

          if verbose then $
              USR_MSG, 'HRRR temperatures not found in ' + HRRRDir

;+
;         Try getting HRRR temperatures from development archives.
;-
          HRRRDir = archiveDir + '/HRRR_archive'

          if verbose then $
              USR_MSG, 'Looking for HRRR temperatures in ' + HRRRDir

          GET_MIN_MAX_AVE_HRRR_2M_TEMP, $
              anlEndDate_YYYYMMDDHH, $
              duration, $
              targetFcstHour_temp, $
              minSubFcstHour_temp, $
              maxSubFcstHour_temp, $
              HRRRDir, $
              scratchDir, $
              minLonAnl, $
              maxLonAnl, $
              minLatAnl, $
              maxLatAnl, $
              lonResOut, $
              latResOut, $
              ndv, $
              minTempGrid, $
              maxTempGrid, $
              aveTempGrid, $
              perfect, $
              HRRR_GRID_PROJ_INFO = HRRRGridProjInfo, $
              MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
              MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
              MAX_MISSING_HOURS = maxMissingTempHours, $
              NUM_MISSING_HOURS = numMissingHours, $
              VERBOSE = verbose

          if (NOT(ISA(minTempGrid)) or $
              NOT(ISA(maxTempGrid)) or $
              NOT(ISA(aveTempGrid)) and $
              verbose) then $
                  USR_MSG, 'HRRR temperatures not found in ' + HRRRDir

          if (verbose and $
              ISA(minTempGrid) and $
              ISA(maxTempGrid) and $
              ISA(aveTempGrid) and $
              (numMissingHours gt 0)) then $
              USR_MSG, 'NOTICE: Missing ' + STRCRA(numMissingHours) + $
                       ' hour/s in ' + temperature_source + $
                       ' min/max/ave temperature calculation.'

;GFKS 20170526 vv commented out for ops install
;          endif
;GFKS 20170526 ^^

      endif ; end of HRRR temperatures (from development archives) section

      if (NOT(ISA(minTempGrid)) or $
          NOT(ISA(maxTempGrid)) or $
          NOT(ISA(aveTempGrid))) then begin

;+
;         Try getting RAP from operations cache.
;-
          if verbose then $
              USR_MSG, 'HRRR temperatures not available. Trying RAP.'

          temperature_source = 'RAP'

          RAPDir = NSAPrefix + '/misc/gisrs_incoming/model'

          GET_MIN_MAX_AVE_RAP_2M_TEMP, $
              anlEndDate_YYYYMMDDHH, $
              duration, $
              targetFcstHour_temp, $
              minSubFcstHour_temp, $
              maxSubFcstHour_temp, $
              RAPDir, $
              scratchDir, $
              minLonAnl, $
              maxLonAnl, $
              minLatAnl, $
              maxLatAnl, $
              lonResOut, $
              latResOut, $
              ndv, $
              minTempGrid, $
              maxTempGrid, $
              aveTempGrid, $
              perfect, $
              RAP_GRID_PROJ_INFO = RAPGridProjInfo, $
              MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
              MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
              MAX_MISSING_HOURS = maxMissingTempHours, $
              NUM_MISSING_HOURS = numMissingHours, $
              VERBOSE = verbose

          if (verbose and $
              ISA(minTempGrid) and $
              ISA(maxTempGrid) and $
              ISA(aveTempGrid) and $
              (numMissingHours gt 0)) then $
              USR_MSG, 'NOTICE: Missing ' + STRCRA(numMissingHours) + $
                       ' hour/s in ' + temperature_source + $
                       ' min/max/ave temperature calculation.'

      endif ; end of RAP temperatures (from operations cache) section

      if ((NOT(ISA(minTempGrid)) or $
           NOT(ISA(maxTempGrid)) or $
           NOT(ISA(aveTempGrid))) and $
          FILE_TEST(archiveDir + '/RAP_archive', /DIRECTORY, /READ))$
          then begin

          if verbose then USR_MSG, 'RAP temperatures not found in ' + RAPDir

;+
;         Try getting RAP temperatures from development archives.
;-
          RAPDir = archiveDir + '/RAP_archive'

          if verbose then $
              USR_MSG, 'Looking for RAP temperatures in ' + RAPDir

          GET_MIN_MAX_AVE_RAP_2M_TEMP, $
              anlEndDate_YYYYMMDDHH, $
              duration, $
              targetFcstHour_temp, $
              minSubFcstHour_temp, $
              maxSubFcstHour_temp, $
              RAPDir, $
              scratchDir, $
              minLonAnl, maxLonAnl, $
              minLatAnl, maxLatAnl, $
              lonResOut, latResOut, $
              ndv, $
              minTempGrid, $
              maxTempGrid, $
              aveTempGrid, $
              perfect, $
              RAP_GRID_PROJ_INFO = RAPGridProjInfo, $
              MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
              MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
              MAX_MISSING_HOURS = maxMissingTempHours, $
              NUM_MISSING_HOURS = numMissingHours, $
              VERBOSE = verbose

          if (NOT(ISA(minTempGrid)) or $
              NOT(ISA(maxTempGrid)) or $
              NOT(ISA(aveTempGrid)) and $
              verbose) then $
                  USR_MSG, 'RAP temperatures not found in ' + RAPDir

          if (verbose and $
              ISA(minTempGrid) and $
              ISA(maxTempGrid) and $
              ISA(aveTempGrid) and $
              (numMissingHours gt 0)) then $
              USR_MSG, 'NOTICE: Missing ' + STRCRA(numMissingHours) + $
                       ' hour/s in ' + temperature_source + $
                       ' min/max/ave temperature calculation.'

      endif ; end of RAP temperatures (from development archives) section

      if ((NOT(ISA(minTempGrid)) or $
           NOT(ISA(maxTempGrid)) or $
           NOT(ISA(aveTempGrid))) and $
          FILE_TEST(archiveDir + '/RUC_archive', /DIRECTORY, /READ)) $
          then begin

;+
;         Try getting RUC temperatures from development
;         archives. Unlike HRRR and RAP, which were operational as of
;         July 2018 when this code was written, RUC has been out of
;         operations since mid-2012, so an initial look for
;         operational cached files in /operations/misc does not make
;         sense.
;-
          if verbose then $
              USR_MSG, 'RAP temperatures not available. Trying RUC.'

          temperature_source = 'RUC'

          RUCDir = archiveDir + '/RUC_archive'

          if verbose then $
              USR_MSG, 'Looking for RUC temperatures in ' + RUCDir

          GET_MIN_MAX_AVE_RUC_2M_TEMP, $
              anlEndDate_YYYYMMDDHH, $
              duration, $
              targetFcstHour_temp, $
              minSubFcstHour_temp, $
              maxSubFcstHour_temp, $
              RUCDir, $
              scratchDir, $
              minLonAnl, maxLonAnl, $
              minLatAnl, maxLatAnl, $
              lonResOut, latResOut, $
              ndv, $
              minTempGrid, $
              maxTempGrid, $
              aveTempGrid, $
              perfect, $
              RUC_GRID_PROJ_INFO = RUCGridProjInfo, $
              MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
              MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
              MAX_MISSING_HOURS = maxMissingTempHours, $
              NUM_MISSING_HOURS = numMissingHours, $
              VERBOSE = verbose

          if (NOT(ISA(minTempGrid)) or $
              NOT(ISA(maxTempGrid)) or $
              NOT(ISA(aveTempGrid)) and $
              verbose) then $
                  USR_MSG, 'RUC temperatures not found in ' + RUCDir

          if (verbose and $
              ISA(minTempGrid) and $
              ISA(maxTempGrid) and $
              ISA(aveTempGrid) and $
              (numMissingHours gt 0)) then $
              USR_MSG, 'NOTICE: Missing ' + STRCRA(numMissingHours) + $
                       ' hour/s in ' + temperature_source + $
                       ' min/max/ave temperature calculation.'

      endif ; end of RUC temperatures (from development archives) section

      if (ISA(minTempGrid) and $
          ISA(maxTempGrid) and $
          ISA(aveTempGrid)) then begin

          temperature_source = temperature_source + ' f' + $
                               STRING(minFcstHourFound, FORMAT = '(I2.2)')
          if (minFcstHourFound ne maxFcstHourFound) then $
              temperature_source = temperature_source + ' to f' + $
                                   STRING(maxFcstHourFound, FORMAT = '(I2.2)')

      endif

  endif ; end of NWP temperature section

  debug_tag = 2000

  if (NOT(ISA(minTempGrid)) or $
      NOT(ISA(maxTempGrid)) or $
      NOT(ISA(aveTempGrid))) then begin
      ERR_MSG, 'ERROR: Unable to generate temperature grids.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s after reading temperatures:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Update the "ok data" mask.

  okGrid = okGrid * $
           (minTempGrid ne ndv) * $
           (maxTempGrid ne ndv) * $
           (aveTempGrid ne ndv)

  ndvInd = WHERE(okGrid eq 0, count)
  if (count gt 0) then begin
      WEASDGrid[ndvInd] = ndv
      APCPGrid[ndvInd] = ndv
      minTempGrid[ndvInd] = ndv
      maxTempGrid[ndvInd] = ndv
      aveTempGrid[ndvInd] = ndv
  endif


; Convert gridded temperatures to degrees Celsius.

  ind = WHERE(minTempGrid eq ndv, count)
  minTempGrid = minTempGrid - 273.15
  if (count gt 0) then minTempGrid[ind] = ndv
  ind = WHERE(maxTempGrid eq ndv, count)
  maxTempGrid = maxTempGrid - 273.15
  if (count gt 0) then maxTempGrid[ind] = ndv
  ind = WHERE(aveTempGrid eq ndv, count)
  aveTempGrid = aveTempGrid - 273.15
  if (count gt 0) then aveTempGrid[ind] = ndv


; Set cutoff values for minimum and average temperature.

  minTempCutoff = 0.0
  aveTempCutoff = $
      GET_MAX_AVE_DAILY_TEMP_FOR_SNOW(anlEndDate_YYYYMMDDHH)

  if verbose then $
      USR_MSG, 'Average temperature cutoff for ' + $
               anlEndDate_YYYYMMDDHH + ' is ' + $
               STRCRA(aveTempCutoff) + ' degrees Celsius'

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if verbose then begin


;     Display the average temperature grid.

      X_MAP_GEO_GRID, aveTempGrid, $
                      minLonAnl, maxLonAnl, $
                      minLatAnl, maxLatAnl, $
                      edges_TdegC, red_TdegC, grn_TdegC, blu_TdegC, $
                      0, $
                      status, $
                      NDV = ndv, $
                      /SHOW_LOW, $
                      /SHOW_HIGH, $
                      TITLE = 'Avg. ' + temperature_source + $
                      ' Air Temp. ' + $
                      '(95% cutoff = ' + $
                      FORMAT_FLOAT(aveTempCutoff) + '), ' + $
                      STRCRA(duration) + ' hours ending ' + $
                      anlEndDate_YYYYMMDDHH + ' UTC', $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'degrees Celsius', $
                      SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write the average and minimum temperature grids to images.

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_03a_ave_temp.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          aveTempGrid[extraCols:numColsAnl - extraCols - 1, $
                      extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_TdegC, red_TdegC, grn_TdegC, blu_TdegC, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          'Average ' + temperature_source + $
          ' Air Temperature ' + $
          '(95% cutoff = ' + FORMAT_FLOAT(aveTempCutoff) + ')!C' + $
          STRCRA(duration) + ' hours ending ' + $
          anlEndDate_GISRS + ' UTC', $
          'degrees Celsius', $
          outputDir + '/' + PNGFile, $
          /SHOW_LOW, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_TdegC, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_03b_min_temp.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          minTempGrid[extraCols:numColsAnl - extraCols - 1, $
                      extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_TdegC, red_TdegC, grn_TdegC, blu_TdegC, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          'Minimum ' + temperature_source + ' Air Temperature!C' + $
          STRCRA(duration) + ' hours ending ' + $
          anlEndDate_GISRS + ' UTC', $
          'degrees Celsius', $
          outputDir + '/' + PNGFile, $
          /SHOW_LOW, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_TdegC, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

  endif

  if produceTIFFs then begin


;     Write average and minimum temperatures to GeoTIFF.

      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_ave_temperature.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(aveTempGrid, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if (updateGISProject) then begin
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_ave_temp.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = STATUS
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_min_temperature.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(minTempGrid, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if (updateGISProject) then begin
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_min_temp.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = STATUS
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


;-------------------------------------------------;
; 2d. Correct APCP and WEASD for errors and bias. ;
;-------------------------------------------------;

  debug_tag = 2100

  QPEThreshold = 0.0

;GF 20170309 vv
;; Store the original WEASDGrid.

;  origWEASDGrid = WEASDGrid
;GF 20170309 ^^

;---------------------------------------------------------------;
; 2d-1. Identify and correct type 1 errors (false positives) in ;
;       WEASD based on average and minimum analysis period      ;
;       temperatures.                                           ;
;---------------------------------------------------------------;

;+
; Here, "and" is used for the min/ave temperature criteria to be
; conservative about removing snowfall that a previous step in the
; analysis has produced.
;-
  tempSlack = 2.0

  type1WInd = $
      WHERE((WEASDGrid ne ndv) and $
            (minTempGrid ne ndv) and $
            (aveTempGrid ne ndv) and $
            (WEASDGrid gt QPEThreshold) and $
            ((minTempGrid ge (minTempCutoff + tempSlack)) and $ ; AND/OR
             (aveTempGrid ge (aveTempCutoff + tempSlack))), $   ; 20181010
            type1WCount)

  if (type1WCount gt 0) then begin

      if verbose then $
          USR_MSG, 'Removing WEASD in ' + STRCRA(type1WCount) + $
                   ' cells due to too-high ' + temperature_source + $
                   ' analysis temperatures.'

      WEASDGrid[type1WInd] = 0.0

  endif

  debug_tag = 2200

;+ Gf 20180823
  if ISA(QPEGrid) then begin

;+
;     Before checking for good QPE data in all RFCs, replace no-data
;     values in the QPE with zeroes for areas where the average and
;     minimum daily temperatures are too high for snowfall
;     accumulation to be realistic. This can help us take advantage of
;     QPE in cases where there are coverage gaps or missing data from
;     particular RFC regions, as long as those gaps or missing regions
;     correspond to places where it was too warm to have snowed.
;
;     Here, "and" is used for the min/ave temperature criteria to be
;     conservative about declaring what are often likely to be entire
;     RFC regions to be snow-free.
;-
;+ GF commented this out 2018-10-23
      ;; ind = WHERE((QPEGrid eq ndv) and $
      ;;             (minTempGrid ne ndv) and $
      ;;             (aveTempGrid ne ndv) and $
      ;;             ((minTempGrid ge (minTempCutoff + tempSlack)) and $ ; AND/OR
      ;;              (aveTempGrid ge (aveTempCutoff + tempSlack))), $   ; 20181010
      ;;             count)

      ;; if (count gt 0) then QPEGrid[ind] = 0.0
 ;-

;+
;     Check for good data in all RFCs.
;-
      if verbose then $
          USR_MSG, 'Checking Stage IV QPE against RFC boundaries.'

      shapeFile = OBJ_NEW('IDLffShape', shapeFilePath + '.shp')
      shapeFile->IDLffShape::GetProperty, N_ATTRIBUTES = numAtts
      shapeFile->IDLffShape::GetProperty, ATTRIBUTE_NAMES = attNames
      attIndNAME = WHERE(attNames eq 'NAME', count)
      if (count ne 1) then begin
          ERR_MSG, 'ERROR: Problem reading shapefile "' + shapeFileName + '".'
          OBJ_DESTROY, shapeFile
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      attIndNAME = attIndNAME[0]
      shapeFile->IDLffShape::GetProperty, N_ENTITIES = numPolygons
      if (numPolygons ne 12) then begin
          ERR_MSG, 'ERROR: Expected 12 polygons in shapefile "' + $
                   shapeFileName + $
                   '", found ' + STRCRA(numPolygons) + '.'
          OBJ_DESTROY, shapeFile
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      if (QPF_source ne 'Missing') then fullReplaceCount = 0L ; GF 20181017
      for rfc = 0, numPolygons - 1 do begin
          attrib = shapeFile->getAttributes(rfc)
          RFCName = attrib.(attIndNAME)
          shapeIndSavFile = resourcesDir + '/' + $
                            shapeFileName + $
                            '_for_sfav2_' + domainLabel + '_' + $
                            RFCName + '.sav'
          polygon = shapeFile->IDLffShape::GetEntity(rfc)
          ind = GRID_POINTS_IN_POLYGON(numColsAnl, numRowsAnl, $
                                       minLonAnl, maxLatAnl, $
                                       lonResOut, latResOut, $
                                       polygon, $
                                       shapeIndSavFile, $
                                       COUNT = numCellsInPolygon) ; north-down
          if (numCellsInPolygon eq 0) then begin
              ERR_MSG, 'ERROR: Found no analysis domain cells in "' + $
                       RFCName + '".'
              OBJ_DESTROY, shapeFile
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif

          QPEGrid_NorthDown = ROTATE(QPEGrid, 7) ; QPE made north-down
          if (QPF_source ne 'Missing') then $
              QPFGrid_NorthDown = ROTATE(APCPGrid, 7)
         
          QPEInRFC = QPEGrid_NorthDown[ind]      ; RFC data sampled from QPE
          ;QPEGrid_NorthDown = !NULL

          okInd = WHERE(QPEInRFC ne ndv, okCount)

          numCellsMissing = numCellsInPolygon - okCount
          proportionMissing = $
              FLOAT(numCellsMissing) / FLOAT(numCellsInPolygon)

          oldQPEEval = 0        ; old vs. new QPE eval
          HRRREraDate_YYYYMMDDHH = '2014100200'
          HRRREraDate_Julian = YYYYMMDDHH_TO_JULIAN(HRRREraDate_YYYYMMDDHH)

          if (proportionMissing gt 0.025) then begin

              if (oldQPEEval or $
                  (anlEndDate_Julian lt HRRREraDate_Julian)) then begin

;+
;                 Prior to the availability of operational HRRR QPF as
;                 a possible substitute, reject the entire QPE grid if
;                 the number of no-data values in any RFC region is
;                 above-threshold.
;-
                  USR_MSG, 'NOTICE: QPE data for ' + RFCName + $
                           ' is missing data in ' + $
                           STRCRA(numCellsMissing) + $
                           ' (' + STRCRA(proportionMissing * 100) + $
                           '%) of its cells. Data are rejected.'
                  QPEGrid = !NULL
                  BREAK

              endif else begin

                  if (QPF_source ne 'Missing') then begin

;+
;                     Replace the QPE across the RFC region with
;                     APCPGrid values. The idea is that so much QPE
;                     data are missing from this RFC region that the
;                     entire analysis from that RFC is no longer
;                     credible.
;-
                      USR_MSG, 'NOTICE: QPE data for ' + RFCName + $
                               ' is missing data in ' + $
                               STRCRA(numCellsMissing) + $
                               ' (' + STRCRA(proportionMissing * 100) + $
                               '%) of its cells. QPE for this RFC region ' + $ 
                               'will be replaced with QPF.'
                      QPEGrid_NorthDown[ind] = QPFGrid_NorthDown[ind]
                      QPEGrid = ROTATE(QPEGrid_NorthDown, 7)
                      fullReplaceCount = fullReplaceCount + numCellsInPolygon

                  endif else begin

;+
;                     Use the same solution as prior to the HRRR era
;                     in the absence of QPF. This will probably cause
;                     the entire analysis to be abandoned, since both
;                     QPE and QPF data are missing, making a
;                     background analysis not possible.
;-
                      USR_MSG, 'NOTICE: QPE data for ' + RFCName + $
                               ' is missing data in ' + $
                               STRCRA(numCellsMissing) + $
                               ' (' + STRCRA(proportionMissing * 100) + $
                               '%) of its cells. No QPF is available, so ' + $
                               'the entire QPE grid is rejected.'
                      QPEGrid = !NULL
                      BREAK

                  endelse

              endelse

          endif else begin

              if ((proportionMissing gt 0.0) and $
                  (QPF_source ne 'Missing')) then begin

;+
;                 Replace missing QPE cells within the current RFC
;                 region with corresponding APCPGrid values to
;                 prevent missing QPE cells from imposing no-data
;                 values on the final analysis.
;-
                  QPFInRFC = QPFGrid_NorthDown[ind]
                  replaceInd = WHERE((QPEInRFC eq ndv) and $
                                     (QPFInRFC ne ndv), replaceCount)
                  if (replaceCount gt 0) then begin
                      QPEInRFC[replaceInd] = QPFInRFC[replaceInd]
                      QPEGrid_NorthDown[ind] = QPEInRFC
                      QPEGrid = ROTATE(QPEGrid_NorthDown, 7)
                      fullReplaceCount = fullReplaceCount + replaceCount
                  endif

              endif

          endelse

      endfor

      if ((QPF_source ne 'Missing') and verbose) then begin
          if (fullReplaceCount gt 0) then $
              USR_MSG, 'NOTICE: no-data values in the QPE grid have been ' + $
                       'replaced by QPF values in ' + $
                       STRCRA(fullReplaceCount) + ' cells.'
      endif

      OBJ_DESTROY, shapeFile

  endif
;- GF 20180823

;+
; Create a grid that describes the origin of WEASD.
;     0 = no WEASD present in background
;     1 = WEASD present in QPF data and verified by QPE.
;     2 = WEASD not present in QPF data but added because it is
;         in the QPE and temperatures are cold enough for snow (type 2
;         error in QPF)
;-

  debug_tag = 2300

  WEASDSourceGrid = WEASDGrid
  ind = WHERE((WEASDGrid ne ndv) and (WEASDGRid le QPEThreshold), count)
  if (count gt 0) then WEASDSourceGrid[ind] = 0.0
  ind = WHERE((WEASDGrid ne ndv) and (WEASDGrid gt QPEThreshold), count)
  if (count gt 0) then WEASDSourceGrid[ind] = 1.0

  if ISA(QPEGrid) then begin

;+
;     Update the "ok data" mask.
;-
      okGrid = okGrid * (QPEGrid ne ndv)
      ndvInd = WHERE(okGrid eq 0, count)
      if (count gt 0) then begin
          WEASDGrid[ndvInd] = ndv
          APCPGrid[ndvInd] = ndv
          QPEGrid[ndvInd] = ndv
          minTempGrid[ndvInd] = ndv
          maxTempGrid[ndvInd] = ndv
          aveTempGrid[ndvInd] = ndv
      endif

;GF 20170309 vv
;GOTO, SKIP_WEASD_FALSE_POSITIVES_BASED_ON_QPE
;GF 20170309 ^^


    ;---------------------------------------------------------------;
    ; 2d-2. Identify and correct type 1 errors (false positives) in ;
    ;       APCP and WEASD based on the QPE reference.              ;
    ;---------------------------------------------------------------;

      type1Ind = WHERE((APCPGrid ne ndv) and $
                       (QPEGrid ne ndv) and $
                       (APCPGrid gt QPEThreshold) and $
                       (QPEGrid le QPEThreshold), type1Count)

      if (type1Count gt 0) then begin


;         Identify cells where type 1 errors also apply to WEASD.

          t1wInd = WHERE((WEASDGrid[type1Ind] ne ndv) and $
                         (WEASDGrid[type1Ind] gt QPEThreshold), type1WCount)

          if verbose then $
              USR_MSG, 'Removing false positives in ' + $
                       STRCRA(type1Count) + ' APCP cells and ' + $
                       STRCRA(type1WCount) + ' WEASD cells.'


;         Remove APCP false positives.

          APCPGrid[type1Ind] = 0.0


;         Remove WEASD false positives.

          if (type1WCount gt 0) then begin
              type1WInd = type1Ind[t1wInd]
              t1wInd = !NULL
              WEASDGrid[type1WInd] = 0.0
              WEASDSourceGrid[type1WInd] = 0.0
          endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

          if verbose then begin


;             Display WEASD with type 1 errors corrected.

              X_MAP_GEO_GRID, $
                  WEASDGrid, $
                  minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                  edges_QPF, red_QPF, grn_QPF, blu_QPF, $
                  2, $
                  status, $
                  NDV = ndv, $
                  /SHOW_HIGH, $
                  TITLE = QPF_source + ' WEASD (no false positives), ' + $
                  subTitle, $
                  /COLORBAR, $
                  XSIZE_TARGET = xszt, $
                  UNITS = 'mm', $
                  SHAPE_PATH_LIST = shapePathList

          endif

          if produceImages then begin


;             Write (type 1) error-corrected WEASD to an image.

              PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                        anlEndDate_YYYYMMDDHH + $
                        '_03c_' + QPF_source_noSpace + $
                        '_WEASD_corrected_1_err.png'

              MAKE_LON_LAT_MAP_PNG_SFAV2, $
                  WEASDGrid[extraCols:numColsAnl - extraCols - 1, $
                            extraRows:numRowsAnl - extraRows - 1], $
                  ndv, $
                  edges_QPF, red_QPF, grn_QPF, blu_QPF, $
                  lonResOut, minLonOut, maxLonOut, $
                  latResOut, minLatOut, maxLatOut, $
                  QPF_source + ' Snowfall Water Equivalent ' + $
                  '(WEASD); Type 1 Errors Corrected!C' + longSubTitle, $
                  'mm', $
                  outputDir + '/' + PNGFile, $
                  /SHOW_HIGH, $
                  TICK_NAMES = tickNames_QPF, $
                  /NO_GRID, /NO_CONTINENTS, /NO_USA, $
                  /BLACK_ON_WHITE, $
                  MAP_SHAPE_PATH = shapePathList

          endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

      endif else begin

          ERR_MSG, 'WARNING: no type 1 errors (false positives) ' + $
                   'identified in accumulated ' + QPF_source + $
                   ' APCP data. This is VERY unusual.'

      endelse

;GF 20170309 vv
;SKIP_WEASD_FALSE_POSITIVES_BASED_ON_QPE:
;GF 20170309 vv


    ;---------------------------------------------------------------;
    ; 2d-3. Identify and correct type 2 errors (misses) in APCP and ;
    ;       WEASD based on the QPE reference.                       ;
    ;---------------------------------------------------------------;

;     We correct misses in the APCP data not because it is essential
;     (we use WEASD for the first guess), but because it simplifies
;     the logic for calculating bias.

      type2Ind = WHERE((APCPGrid ne ndv) and $
                       (QPEGrid ne ndv) and $
                       (APCPGrid le QPEThreshold) and $
                       (QPEGrid gt QPEThreshold), type2Count)

      if (type2Count gt 0) then begin


;         Replace APCP misses with QPE.

          APCPGrid[type2Ind] = QPEGrid[type2Ind]


;         Verify that no above-threshold WEASD exists where APCP does
;         not. In such cases, WEASD exceeds APCP, which should never
;         happen because it is handled earlier in the program, and
;         may also be handled when the "GET_ACCUM_..." procedures are
;         called.

          ind = WHERE((WEASDGrid[type2Ind] ne ndv) and $
                      (WEASDGrid[type2Ind] gt QPEThreshold), count)
          if (count gt 0) then begin
              ERR_MSG, 'ERROR: (PROGRAMMING) Above-threshold WEASD values ' + $
                       'found in ' + STRCRA(count) + ' cells for which ' + $
                       'APCP is below-threshold.'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif

;+
;         Replace WEASD misses with QPE, as long as the average and
;         minimum temperatures for the analysis period are cold enough
;         to assume snow has occurred. Here, "and" is used for the
;         min/ave temperature criteria to be conservative about
;         creating snowfall where we have no direct evidence (just
;         cool temperatures and nonzero precipitation) that it
;         occurred (in contradiction with the original WEASDGrid).
;-
          ind = WHERE((WEASDGrid[type2Ind] ne ndv) and $
                      (minTempGrid[type2Ind] ne ndv) and $
                      (aveTempGrid[type2Ind] ne ndv) and $
                      ((minTempGrid[type2Ind] lt minTempCutoff) and $ ; AND/OR
                       (aveTempGrid[type2Ind] lt aveTempCutoff)), $   ; 20181010
                      count)

          if (count gt 0) then begin
              if verbose then $
                  USR_MSG, 'Replacing below-threshold WEASD with QPE in ' + $
                           STRCRA(count) + ' cells (type II errors) ' + $
                           'where the minimum temperature < ' + $
                           STRCRA(minTempCutoff) + ' ' + $
                           'and the average tempature < ' + $
                           STRCRA(aveTempCutoff) +'.'
              WEASDGrid[type2Ind[ind]] = QPEGrid[type2Ind[ind]]
              WEASDSourceGrid[type2Ind[ind]] = 2.0
          endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

          if verbose then begin


;             Display WEASD with type 1 and type 2 errors corrected.

              X_MAP_GEO_GRID, $
                  WEASDGrid, $
                  minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                  edges_QPF, red_QPF, grn_QPF, blu_QPF, $
                  2, $
                  status, $
                  NDV = ndv, $
                  /SHOW_HIGH, $
                  TITLE = QPF_source + ' WEASD (error corrected), ' + $
                  subTitle, $
                  /COLORBAR, $
                  XSIZE_TARGET = xszt, $
                  UNITS = 'mm', $
                  SHAPE_PATH_LIST = shapePathList

          endif

          if produceImages then begin


;             Write (type 2) error-corrected WEASD to an image.

              PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                        anlEndDate_YYYYMMDDHH + $
                        '_03d_WEASD_corrected_2_err.png'

              MAKE_LON_LAT_MAP_PNG_SFAV2, $
                  WEASDGrid[extraCols:numColsAnl - extraCols - 1, $
                            extraRows:numRowsAnl - extraRows - 1], $
                  ndv, $
                  edges_QPF, red_QPF, grn_QPF, blu_QPF, $
                  lonResOut, minLonOut, maxLonOut, $
                  latResOut, minLatOut, maxLatOut, $
                  QPF_source + ' Snowfall Water Equivalent ' + $
                  '(WEASD) - Type 1/2 Errors Corrected!C' + longSubTitle, $
;                  STRCRA(duration) + ' hours ending ' + $
;                  anlEndDate_YYYYMMDDHH, $
                  'mm', $
                  outputDir + '/' + PNGFile, $
                  /SHOW_HIGH, $
                  TICK_NAMES = tickNames_QPF, $
                  /NO_GRID, /NO_CONTINENTS, /NO_USA, $
                  /BLACK_ON_WHITE, $
                  MAP_SHAPE_PATH = shapePathList

          endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

      endif else begin

          ERR_MSG, 'WARNING: no type 2 errors (misses) ' + $
                   'identified in aggregate ' + QPF_source + $
                   ' APCP data. This ' + $
                   'is VERY unusual.'

      endelse


    ;------------------------------------;
    ; 2d-4. Calculate bias in APCP data. ;
    ;------------------------------------;

      QPFBiasGrid = MAKE_ARRAY(numColsAnl, numRowsAnl, /FLOAT, VALUE = 1.0)

      QPEInd = WHERE((APCPGrid ne ndv) and $
                     (QPEGrid ne ndv) and $
                     (QPEGrid gt QPEThreshold), QPECount)

      if (QPECount eq 0) then begin
          ERR_MSG, 'ERROR: above-threshold QPE not found.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          errMsg = ['Integer divide by zero', $
                    'Integer overflow', $
                    'Unspecified error 2^2', $
                    'Unspecified error 2^4', $
                    'Floating divide by zero', $
                    'Floating underflow', $
                    'Floating overflow', $
                    'Floating Illegal operand']
          ERR_MSG, 'Unhandled math error/s after QPF error check:'
          for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
              ERR_MSG, errMsg[i]
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      QPFBiasGrid[QPEInd] = APCPGrid[QPEInd] / QPEGrid[QPEInd]

      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          errMsg = ['Integer divide by zero', $
                    'Integer overflow', $
                    'Unspecified error 2^2', $
                    'Unspecified error 2^4', $
                    'Floating divide by zero', $
                    'Floating underflow', $
                    'Floating overflow', $
                    'Floating Illegal operand']
          ERR_MSG, 'Unhandled math error/s after QPF bias calculation:'
          for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
              ERR_MSG, errMsg[i]
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif


;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

      if verbose then begin


;         Display calculated bias in QPF.

;+
;         Bin edges are close to the antilog of edges_sfRatio.
;-
          edges = [1.0D-6, 0.01, 0.1, 0.25, 0.5, $
                   0.9, 1.1, $
                   2.0, 4.0, 10.0, 100.0, 1.0D6] 

          X_MAP_GEO_GRID, QPFBiasGrid, $
                          minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                          edges, red_sfRatio, grn_sfRatio, blu_sfRatio, $
                          0, $
                          status, $
                          NDV = ndv, $
                          /SHOW_LOW, $
                          /SHOW_HIGH, $
                          TITLE = 'Multiplicative Bias in ' + $
                          QPF_source + ' QPF vs. Stage IV, ' + subTitle, $
                          /COLORBAR, $
                          XSIZE_TARGET = xszt, $
                          UNITS = 'dimensionless', $
                          SHAPE_PATH_LIST = shapePathList

      endif

      if produceImages then begin


;         Write calculated bias in QPF to an image.

;+
;         Bin edges are close to the antilog of edges_sfRatio.
;-
          edges = [1.0D-6, 0.01, 0.1, 0.25, 0.5, $
                   0.9, 1.1, $
                   2.0, 4.0, 10.0, 100.0, 1.0D6] 

          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_03e_' + QPF_source_noSpace + '_QPF_Bias_Calculated.png'

          MAKE_LON_LAT_MAP_PNG_SFAV2, $
              QPFBiasGrid[extraCols:numColsAnl - extraCols - 1, $
                          extraRows:numRowsAnl - extraRows - 1], $
              ndv, $
              edges, red_sfRatio, grn_sfRatio, blu_sfRatio, $
              lonResOut, minLonOut, maxLonOut, $
              latResOut, minLatOut, maxLatOut, $
              'Multiplicative Bias in ' + $
              QPF_source + ' QPF vs. Stage IV!C' + longSubTitle, $
              'dimensionless', $
              outputDir + '/' + PNGFile, $
              /SHOW_LOW, $
              /SHOW_HIGH, $
              /NO_GRID, /NO_CONTINENTS, /NO_USA, $
              /BLACK_ON_WHITE, $
              MAP_SHAPE_PATH = shapePathList

      endif


;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


  endif else begin ; ISA(QPEGrid)

;+ GFKS 20181026
;+
;     We need at least one of QPE/QPF to produce the analysis.
;-
;      if (QPF_source eq 'Missing') then begin
;          ERR_MSG, 'ERROR: No QPE or QPF found; cannot generate ' + $
;                   'background analysis.'
;          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;      endif
;- GFKS 20181026     

      QPFBiasGrid = MAKE_ARRAY(numColsAnl, numRowsAnl, /FLOAT, VALUE = 1.0)

  endelse


;----------------------------------------------------------;
; 2d-6. Adjust WEASD grid to correct for the bias in APCP. ;
;----------------------------------------------------------;

  debug_tag = 2400

  WEASDGrid = WEASDGrid / QPFBiasGrid

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s after calculating background WEASD:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

;+
; GF 20200106 vv
;-
  if produceTIFFs then begin

      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + '_' + $
                 QPF_source_noSpace + $
                 '_corr_WEASD.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(WEASDGrid, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

  endif
;+
; GF 20200106 ^^
;-

  if verbose then begin


;     Display WEASD with errors corrected (if applicable) and bias removed.

      if ISA(QPEGrid) then $
          title = QPF_source + ' WEASD - Error- and Bias-Corrected' $
      else $
          title = QPF_source + ' WEASD - Estimated Bias Removed'

      X_MAP_GEO_GRID, $
          WEASDGrid, $
          minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
          edges_QPF, red_QPF, grn_QPF, blu_QPF, $
          2, $
          status, $
          NDV = ndv, $
          /SHOW_HIGH, $
          TITLE = title + ', ' + subTitle, $
          /COLORBAR, $
          XSIZE_TARGET = xszt, $
          UNITS = 'mm', $
          SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write WEASD with errors corrected (if applicable) and bias
;     removed to an image.

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_04_WEASD_corrected_err_and_bias.png'

      if ISA(QPEGrid) then $
          title = 'Accumulated ' + QPF_source + $
                  ' WEASD; Error- and Bias-Corrected' $
      else $
          title = 'Accumulated ' + QPF_source + $
                  ' WEASD; Estimated Bias Removed'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          WEASDGrid[extraCols:numColsAnl - extraCols - 1, $
                    extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_QPF, red_QPF, grn_QPF, blu_QPF, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          title + '!C' + longSubTitle, $
          'mm', $
          outputDir + '/' + PNGFile, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_QPF, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

      if ISA(QPEGrid) then begin

;+
;         GF 20200106 vv
;         Calculate and display percent of precipitation falling as snow.
;-
          percSnowGrid = MAKE_ARRAY(numColsAnl, numRowsAnl, /FLOAT, $
                                    VALUE = ndv)
          QPEInd = WHERE((WEASDGrid ne ndv) and $
                         (QPEGrid ne ndv) and $
                         (QPEGrid gt QPEThreshold), QPECount)
          if (QPECount eq 0) then begin
              ERR_MSG, 'ERROR: above-threshold QPE not found.'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif

          percSnowGrid[QPEInd] = WEASDGrid[QPEInd] / QPEGrid[QPEInd] * 100.0
          ;; PRINT, MIN(percSnowGrid[QPEInd]), MAX(percSnowGrid[QPEInd])

          if produceTIFFs then begin

              TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                         anlEndDate_YYYYMMDDHH + $
                         '_QPE_snow_percent.tif'

              MAKE_GEOTIFF_FROM_GRID, ROTATE(percSnowGrid, 7), $
                                      minLonAnl, $
                                      maxLatAnl, $
                                      lonResOut, $
                                      latResOut, $
                                      outputDir + '/' + TIFFFile, $
                                      NO_DATA_VALUE = ndv, $
                                      COMPRESS = 1

          endif

          edges = [0.0, 10.0, 20.0, 30.0, 40.0, 50.0, $
                   60.0, 70.0, 80.0, 90.0, 100.0]
          red_percSnow = [001, 001, 002, 054, 103, 166, 208, 236, 255, 255]
          grn_percSnow = [070, 108, 129, 144, 169, 189, 209, 226, 247, 255]
          blu_percSnow = [054, 089, 138, 192, 207, 219, 230, 240, 251, 255]
          edges = [0.0, 20.0, 40.0, 60.0, 80.0, 100.0]
          red_percSnow = [001, 028, 103, 189, 246]
          grn_percSnow = [108, 144, 169, 201, 239]
          blu_percSnow = [089, 153, 207, 225, 247]

          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_04a_QPE_snow_percent.png'

          MAKE_LON_LAT_MAP_PNG_SFAV2, $
              percSnowGrid[extraCols:numColsAnl - extraCols - 1, $
                           extraRows:numRowsAnl - extraRows - 1], $
              ndv, $
              edges, red_percSnow, grn_percSnow, blu_percSnow, $
              lonResOut, minLonOut, maxLonOut, $
              latResOut, minLatOut, maxLatOut, $
              'Estimated Percent of QPE Falling as Snow!C' + longSubTitle, $
              'percent', $
              outputDir + '/' + PNGFile, $
              ;; /SHOW_LOW, $
              ;; /SHOW_HIGH, $
              /NO_GRID, /NO_CONTINENTS, /NO_USA, $
              /BLACK_ON_WHITE, $
              MAP_SHAPE_PATH = shapePathList
;+
;         GF 20200106 ^^
;-
      endif

  endif



;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


;------------------------------------------------------------;
; The WEASDGrid now represents the error- and bias-corrected ;
; snowfall background water equivalent.                      ;
;------------------------------------------------------------;

  debug_tag = 2500

;-----------------------------------------------------------------;
; 2e. Estimate snowfall-to-liquid (water equivalent) ratio (SLR). ;
;-----------------------------------------------------------------;

  SLRDir = resourcesDir + '/SLR_climatology'

  if NOT(FILE_TEST(SLRDir, /DIRECTORY, /READ)) then begin
      if FILE_TEST(SLRDir, /DIRECTORY) then $
          ERR_MSG, 'ERROR: SLR directory ' + SLRDir + $
                   ' is not readable by this user.' $
      else $
          ERR_MSG, 'ERROR: SLR directory ' + SLRDir + ' not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  end


; Note that we read output bounds instead of analysis bounds here,
; because we know those are the bounds of the SLR climatology. We will
; have to expand the SLR result manually.

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s before reading climatological SLR:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  GET_SLR_CLIMATOLOGY, STRMID(anlEndDate_YYYYMMDDHH, 0, 8), $ ; YYYYMMDD
                       SLRDir, $
                       minLonAnl, maxLonAnl, $
                       minLatAnl, maxLatAnl, $
                       lonResOut, latResOut, $
                       ndv, $
                       SLRGrid, $
                       /PAD, $ ; adds ndv outside SLR domain
                       /REMOVE_NDV, $ ; replaces ndv w/neighborhood average
                       YEAR_RANGE = SLRYearRange

  STR_REPLACE, SLRYearRange, '_to_', '-'

  if NOT(ISA(SLRGrid)) then begin
      ERR_MSG, 'ERROR: Failed to get SLR climatology for ' + $
               anlEndDate_YYYYMMDDHH + '.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s after reading climatological SLR:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if verbose then begin


;     Display SLR values.

      X_MAP_GEO_GRID, SLRGrid, $
                      minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                      edges_SLR, red_SLR, grn_SLR, blu_SLR, $
                      0, $
                      status, $
                      NDV = ndv, $
                      /SHOW_LOW, $
                      /SHOW_HIGH, $
                      TITLE = 'SLR Climatology (' + SLRYearRange + ') for ' + $
                      anlEndDate_YYYYMMDDHH, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'dimensionless', $
                      SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write SLR result to an image.

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_05_SLR.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          SLRGrid[extraCols:numColsAnl - extraCols - 1, $
                  extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_SLR, red_SLR, grn_SLR, blu_SLR, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          'Snowfall-to-Liquid Equivalent Ratio (SLR)!C' + $
          anlEndDate_GISRS + ' UTC', $
          'dimensionless', $
          outputDir + '/' + PNGFile, $
          /SHOW_LOW, $
          /SHOW_HIGH, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


;---------------------------------;
; 2f. Create snowfall background. ;
;---------------------------------;

  debug_tag = 2600

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s before finalizing snowfall background:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  snflBGGridInches = WEASDGrid / 25.4 * SLRGrid
  ind = WHERE(WEASDGrid eq ndv, count)
  if (count gt 0) then snflBGGridInches[ind] = ndv

;GF 20170309 vv
;  ind = WHERE(origWEASDGrid eq ndv, count)
;  QBSGridInches = origWEASDGrid / 25.4 * SLRGrid ; QBS = QPF-based snowfall
;  if (count gt 0) then QBSGridInches[ind] = ndv
;GF 20170309 ^^


; Impose a floor on the background equal to 0.01 inches.

  ind = WHERE((snflBGGridInches ne ndv) and $
              (snflBGGridInches lt 0.01), count)
  if (count gt 0) then begin
      snflBGGridInches[ind] = 0.0
      if verbose then $
          USR_MSG, 'Adjusted ' + STRCRA(count) + ' cells with a ' + $
                   '"trace" floor of 0.01".'
  endif

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s after finalizing snowfall background:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

 if verbose then begin


;     Display snowfall background.


      title = 'Snowfall Analysis v2 First Guess: ' + subtitle

      X_MAP_GEO_GRID, snflBGGridInches, $
                      minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                      edges_snowfall, $
                      red_snowfall, grn_snowfall, blu_snowfall, $
                      2, $
                      status, $
                      NDV = ndv, $
                      /SHOW_HIGH, $
                      TITLE = title, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'inches', $
                      SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write snowfall background to an image.


;     Color curve working group colors, with an additional color for
;     "trace" amounts between 0.0 and 0.1, and an additional bin at
;     the top.

      title = 'Snowfall Analysis v2 First Guess!C' + longSubTitle

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_06_snowfall_1st_guess.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          snflBGGridInches[extraCols:numColsAnl - extraCols - 1, $
                           extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_snowfall, red_snowfall, grn_snowfall, blu_snowfall, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          title, $
          'inches', $
          outputDir + '/' + PNGFile, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_snowfall, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

  endif

  if produceTIFFs then begin


;     Write background to GeoTIFF.

      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_snowfall_1st_guess.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(snflBGGridInches, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if (updateGISProject) then begin
          ;; cmd = 'ln -fs ' + $
          ;;       '../' + outputDir + '/' + TIFFFile + ' ' + $
          ;;       GISProjectDir + '/' + $
          ;;       'sfav2_' + domainLabel + '_snowfall_1st_guess.tif'
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_snowfall_1st_guess.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


;================================================;
; 3. Get observations and compare with 1st guess ;
;================================================;

  debug_tag = 2700

  if verbose then USR_MSG, '--- 3. GETTING/EVALUATING OBSERVATIONS ---'


;--------------------------------;
; 3a. Get snowfall observations. ;
;--------------------------------;

; The GET_SNOWFALL_OBS procedure only accepts values in the range
; [0.0, 99.5>, so there are no no-data values possible. Nonzero values
; less than 0.1 (limited to the neighborhood of 0.001 in the NOHRSC
; databases) are adjusted to 0.01.


;--------------------------------;
; 3a-1. Get direct observations. ;
;--------------------------------;

  GET_SNOWFALL_OBS, anlEndDate_YYYYMMDDHH, $
                    windowStartDate_YYYYMMDDHH, $
                    windowFinishDate_YYYYMMDDHH, $
                    duration, $
                    minLonAnl + lonResOut, $
                    maxLonAnl - lonResOut, $
                    minLatAnl + latResOut, $
                    maxLatAnl - latResOut, $
                    PGHost, webPGHost, $
                    snowfallReport

  numSnowfall = N_ELEMENTS(snowfallReport)

  if (numSnowfall eq 0) then begin
      ERR_MSG, 'ERROR: No snowfall observations found for ' + $
               anlEndDate_YYYYMMDDHH + '.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if verbose then $
      USR_MSG, 'Found ' + STRCRA(numSnowfall) + ' snowfall reports for ' + $
               anlEndDate_YYYYMMDDHH


; Extract longitude, latitude, and observed snowfall, converting to
; inches for the latter.

  siteID = snowfallReport.station_id
  siteLon = snowfallReport.longitude
  siteLat = snowfallReport.latitude
  siteElev = snowfallReport.elevation
  siteObs = snowfallReport.value_meters * 39.3701 ; observation in inches
; GF 20170329 add this vv
  siteObsIsReal = REPLICATE(1, numSnowfall)
; GF 20170329 ^^


;--------------------------------------------------------------;
; 3a-2. Get subdivided snowfall observations to augment direct ;
; observations.
;--------------------------------------------------------------;

  debug_tag = 2800

  if (QPF_source_base eq 'Missing') then GOTO, SKIP_SUBDIVIDE

;  checkID = ['MI-AT-6']
;  checkID = ['BRSW3', 'ONMM5', 'BRSQ3', 'MN-KD-1']
;  checkID = ['MMTN7', 'MTLT1', 'MI-OC-2', 'BRSW3', 'LBOV2']

; Kent uncommented SKIP_SUBDIVIDE for testing 12z analyses  

; GOTO, SKIP_SUBDIVIDE


; Determine start/finish dates for subdivision. Set up a time range
; for subdivision meeting the following criteria:

;   1. The analysis period includes 12Z-12Z 24-hour durations.
;   2. The subdivision period is the same # hours on both ends of the
;      analysis period.
;   3. The subdivision period adds at least 12 hours on each end of
;      the analysis period.
;   4. Finally, the subdivision period may not extend more than 6
;      hours into the future, because short term QPF needed to do the
;      subdivision will not be available beyond that, but it must be
;      at least 24 hours in length.

  ;; subStartDate_Julian = anlEndDate_Julian - DOUBLE(duration) / 24.0D - 0.5D
  ;; subFinishDate_Julian = anlEndDate_Julian + 0.5D
  ;; subStartDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subStartDate_Julian)
  ;; subFinishDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subFinishDate_Julian)

  anlStartDate_Julian = anlEndDate_Julian - DOUBLE(duration) / 24.0D
  subStartDate_Julian = anlStartDate_Julian
  subStartDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subStartDate_Julian)

  if verbose then $
      USR_MSG, 'For subdivide: analysis period is ' + $
               subStartDate_YYYYMMDDHH + ' to ' + anlEndDate_YYYYMMDDHH

  while (FIX(STRMID(subStartDate_YYYYMMDDHH, 8, 2)) ne 12) do begin
      subStartDate_Julian = subStartDate_Julian - 1.0D / 24.0D
      subStartDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subStartDate_Julian)
  endwhile

  subFinishDate_Julian = anlEndDate_Julian
  subFinishDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subFinishDate_Julian)

  while (FIX(STRMID(subFinishDate_YYYYMMDDHH, 8, 2)) ne 12) do begin
      subFinishDate_Julian = subFinishDate_Julian + 1.0D / 24.0D
      subFinishDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subFinishDate_Julian)
  endwhile

  subStartPad = anlStartDate_Julian - subStartDate_Julian
  subFinishPad = subFinishDate_Julian - anlEndDate_Julian

  pad = subStartPad > subFinishPad ; padding symmetric using larger
  pad = pad > 0.5D ; padding at least 12 hours
  subStartDate_Julian = anlStartDate_Julian - pad
  subFinishDate_Julian = anlEndDate_Julian + pad


; Finally, avoid subdivisions requiring excessive forecast data.

  while ((sysTime_Julian + 6.0D / 24.0D) lt subFinishDate_Julian) do begin
      subFinishDate_Julian = subFinishDate_Julian - 6.0D / 24.0D
      if (subFinishDate_Julian lt anlEndDate_Julian) then begin
          subFinishDate_Julian = subFinishDate_Julian + 6.0D / 24.0D
          BREAK ; avoid breaking through the analysis end time
      endif
  endwhile
  if ((subFinishDate_Julian - 1.0D) lt subStartDate_Julian) then $
      subStartDate_Julian = subFinishDate_Julian - 1.0D

  subStartDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subStartDate_Julian)
  subFinishDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(subFinishDate_Julian)

  if verbose then $
      USR_MSG, 'For subdivide: collection period for disaggregation is ' + $
               subStartDate_YYYYMMDDHH + ' to ' + subFinishDate_YYYYMMDDHH

;+
; Subdivide observations for other durations to augment results of
; GET_SNOWFALL_OBS.
;-
  SUBDIVIDE_SNOWFALL_OBS, subStartDate_YYYYMMDDHH, $
                          subFinishDate_YYYYMMDDHH, $
                          minLonAnl + lonResOut, $
                          maxLonAnl - lonResOut, $
                          minLatAnl + latResOut, $
                          maxLatAnl - latResOut, $
                          targetFcstHour_QPF, $
                          minSubFcstHour_QPF, $
                          maxSubFcstHour_QPF, $
                          ;; QPF_source_base, $
                          ;; QPFDir, $
                          NSAPrefix, $
                          archiveDir, $
                          scratchDir, $
                          ndv, $
                          PGHost, $
                          webPGHost, $
                          subSnowfallReport, $
                          VERBOSE = verbose

  numSimSnowfall = N_ELEMENTS(subSnowfallReport)

  if (numSimSnowfall eq 0) then begin
      ERR_MSG, 'WARNING: No subdivided snowfall observations found for ' + $
               subStartDate_YYYYMMDDHH + ' - ' + $
               subFinishDate_YYYYMMDDHH + '.'
      GOTO, SKIP_SUBDIVIDE
  endif

;  if verbose then $
;      USR_MSG, 'Subdivided ' + STRCRA(numSimSnowfall) + $
;               ' other snowfall reports for ' + $
;               subStartDate_YYYYMMDDHH + ' - ' + $
;               subFinishDate_YYYYMMDDHH + '.'

  aggFinishDateInd = $
      ROUND((anlEndDate_Julian - subStartDate_Julian) * 24.0D) - 1
  aggStartDateInd = aggFinishDateInd - duration + 1

  simObs = MAKE_ARRAY(numSimSnowfall, /FLOAT, VALUE = ndv)

  for sc = 0, numSimSnowfall - 1 do begin
      sim = subSnowfallReport[sc].value_meters
      sim = sim[aggStartDateInd:aggFinishDateInd]
      ind = WHERE(sim ne ndv, count)
      if (count ne duration) then CONTINUE
      simObs[sc] = TOTAL(sim) * 39.3701 ; simulated observation in inches
      if ((simObs[sc] gt 0.0) and (simObs[sc] lt 0.1)) then $
          simObs[sc] = 0.01
  endfor

  ind = WHERE(simObs ne ndv, count)
  if (count eq 0) then begin
      ERR_MSG, 'WARNING: Re-aggregation of subdivided snowfall ' + $
               'observations gave all no-data values.'
      GOTO, SKIP_SUBDIVIDE
  endif

  subSnowfallReport = subSnowfallReport[ind]
  simID = subSnowfallReport.station_id
  simLon = subSnowfallReport.longitude
  simLat = subSnowfallReport.latitude
; GF 20170329 add this vv
;  simElev = subSnowfallReport.elevation
; GF 20170329 ^^
  simObs = simObs[ind]
  numSimSnowfall = count

  if verbose then $
      USR_MSG, 'Subdivided data provided ' + STRCRA(numSimSnowfall) + $
               ' reports covering the ' + STRCRA(duration) + $
               ' hour analysis period ending ' + anlEndDate_YYYYMMDDHH + $
               '.'


; Inventory actual reports that did not have a simulated value at the
; same station. This can happen for two reasons we know of. First,
; when real reports have observation times slightly removed from the
; aggStartDate-to-aggFinishDate period, but where no-data values exist
; outside the observation duration. Second, where reports could not be
; subdivided because the stations were out of the HRRR domain or
; because the reports were nonzero and HRRR did not provide anything
; nonzero in its WEASD and APCP for subdivision.

  noSubCount = 0L
  simRedundant = BYTARR(numSimSnowfall)
  for sc = 0, numSnowfall - 1 do begin
      ind = WHERE(subSnowfallReport.station_id eq $
                  snowfallReport[sc].station_id, count)
; GF 20170329 add this vv
;      ind = WHERE(simID eq siteID[sc], count)
; GF 20170329 ^^
      if (count eq 1) then begin
          simRedundant[ind[0]] = 1B
          CONTINUE
      endif
      if (count ne 0) then begin
          ERR_MSG, 'ERROR: fatal programming error. Multiple simulated ' + $
                   'snowfall reports for station "' + $
                   snowfallReport[sc].station_id + '". ' + $
                   'This should not be possible.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      noSubCount++
  endfor


; Eliminate simulated observations at stations that provided actual
; reports.

  simKeepInd = WHERE(simRedundant eq 0, simKeepCount, $
                     COMPLEMENT = simLoseInd, NCOMPLEMENT = simLoseCount)
  if (simKeepCount eq 0) then begin
      if verbose then $
          USR_MSG, 'All simulated observations are redundant.'
      GOTO, SKIP_SUBDIVIDE
  endif else begin
      if (simLoseCount eq 0) then begin
          ERR_MSG, 'WARNING: No simulated observations are redundant. ' + $
                   'This is HIGHLY unusual.'
      endif else begin
          if verbose then $
              USR_MSG, 'Dropping ' + STRCRA(simLoseCount) + $
                       ' simulated observations that are redundant' + $
                       ' with actual reports.'
          subSnowfallReport = subSnowfallReport[simKeepInd]
; GF 20170329 remove this vv
          simID = subSnowfallReport.station_id
          simLon = subSnowfallReport.longitude
          simLat = subSnowfallReport.latitude
          simElev = subSnowfallReport.elevation
; GF 20170329 ^^
; GF 20170329 add this vv
;          simID = simID[simKeepInd]
;          simLon = simLon[simKeepInd]
;          simLat = simLat[simKeepInd]
;          simElev = simElev[simKeepInd]
; GF 20170329 ^^
          simObs = simObs[simKeepInd]
          numSimSnowfall = simKeepCount
      endelse
  endelse

  if verbose then $
      USR_MSG, 'Addition of subdivided data provided a net additional ' + $
               STRCRA(numSimSnowfall) + ' (simulated) observations.'

  if ((noSubCount gt 0) and verbose) then $
      USR_MSG, 'Subdivided counterparts are not present for ' + $
               STRCRA(noSubCount) + ' actual reports (this is normal).'


; 3a-3. Combine actual reports and simulated observations.

  ;; isSim = [REPLICATE(0B, numSnowfall), REPLICATE(1B, numSimSnowfall)]

  siteID = [siteID, subSnowfallReport.station_id]
  siteLon = [siteLon, simLon]
  siteLat = [siteLat, simLat]
  siteElev = [siteElev, simElev]
  siteObs = [siteObs, simObs]
; GF 20170329 add this vv
  siteObsIsReal = [siteObsIsReal, REPLICATE(0, numSimSnowfall)]
; GF 20170329 ^^
  numSnowfall = numSnowfall + numSimSnowfall

  if verbose then $
      USR_MSG, 'Addition of simulated observations results in ' + $
               STRCRA(numSnowfall) + ' snowfall reports for ' + $
               anlEndDate_YYYYMMDDHH


SKIP_SUBDIVIDE:


; WE NEED TO BE DONE WITH THE "snowfallReport" STRUCTURE AT THIS
; POINT. IT IS ONLY USED HEREAFTER TO PRINT STATION IDENTIFIERS, WHICH
; CAN BE TAKEN FROM THE NEW "siteID" VARIABLE. SINCE THE OBSERVATIONS
; INCLUDE SIMULATIONS, IT WILL BE A BUG IF WE CONTINUE TO USE
; "snowfallReport".

  snowfallReport = !NULL
  subSnowfallReport = !NULL


; STILL NEED TO ELIMINATE ALL USES OF snowfallReport FOLLOWING THIS
; POINT IN THE PROGRAM.

  debug_tag = 2900


; 3a-4. Incorporate manual quality control (QC).


; Check for manual QC files for this analysis.

  siteKeepFlag = MAKE_ARRAY(numSnowfall, /BYTE, VALUE = 0B)

  goodStationFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_good_stations.txt'
;print, outputdir + '/' + goodStationFile
  if FILE_TEST(outputParentDir + '/' + goodStationFile) then begin


;     Identify stations to protect against outlier tests. Note that
;     this protection is overridden by a listing in the "bad_stations"
;     file.

      if verbose then $
          USR_MSG, 'Reading protected stations from ' + goodStationFile

      OPENR, lun, outputParentDir + '/' + goodStationFile, /GET_LUN
      numGoodStationsListed = 0L
      line = ''
      while NOT(EOF(lun)) do begin
          READF, lun, line
          hashPos = STRPOS(line, '#')
          if (hashPos ne -1) then begin
              line = STRMID(line, 0, hashPos)
              if (line eq '') then CONTINUE
          endif
          numGoodStationsListed++
          line = STRCRA(line)
;          ind = WHERE(snowfallReport.station_id eq line, count)
          ind = WHERE(siteID eq line, count)
          if (count eq 0) then CONTINUE ; no match
          if (count ne 1) then begin
              ERR_MSG, 'ERROR: (PROGRAMMING) Good station listing "' + line + $
                       '" had multiple (' + STRCRA(count) + $
                       ') matches in snowfall data.'
              FREE_LUN, lun
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif
          ind = ind[0]
          siteKeepFlag[ind] = 1B
      endwhile
      FREE_LUN, lun

      if ((numGoodStationsListed gt 0) and verbose) then begin
          USR_MSG, 'Manual QC listed ' + $
                   STRCRA(numGoodStationsListed) + $
                   ' protected stations, of which ' + $
                   STRCRA(TOTAL(siteKeepFlag, /INT)) + $
                   ' will contribute to this analysis.'
      endif

  endif

  badStationFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                   anlEndDate_YYYYMMDDHH + $
                   '_bad_stations.txt'

  if FILE_TEST(outputParentDir + '/' + badStationFile) then begin

      flag = MAKE_ARRAY(numSnowfall, /BYTE, VALUE = 1B)

      if verbose then $
          USR_MSG, 'Reading disqualified stations from ' + badStationFile

      OPENR, lun, outputParentDir + '/' + badStationFile, /GET_LUN
      numBadStationsListed = 0L
      line = ''
      while NOT(EOF(lun)) do begin
          READF, lun, line
          hashPos = STRPOS(line, '#')
          if (hashPos ne -1) then begin
              line = STRMID(line, 0, hashPos)
              if (line eq '') then CONTINUE
          endif
          numBadStationsListed++
          line = STRCRA(line)
;          ind = WHERE(snowfallReport.station_id eq line, count)
          ind = WHERE(siteID eq line, count)
          if (count eq 0) then CONTINUE ; no match
          if (count ne 1) then begin
              ERR_MSG, 'ERROR: (PROGRAMMING) Bad station listing "' + line + $
                       '" had multiple (' + STRCRA(count) + $
                       ') matches in snowfall data.'
              FREE_LUN, lun
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif
          ind = ind[0]
          flag[ind] = 0B
      endwhile
      FREE_LUN, lun

      ind = WHERE(flag, count)
      flag = !NULL

      if (count eq 0) then begin
          ERR_MSG, 'ERROR: Manual QC eliminated all data.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      numBadStationsRemoved = numSnowfall - count

      if (numBadStationsRemoved gt 0) then begin

;          snowfallReport = snowfallReport[ind]
          siteID = siteID[ind]
          siteLon = siteLon[ind]
          siteLat = siteLat[ind]
          siteElev = siteElev[ind]
          siteObs = siteObs[ind]
          siteObsIsReal = siteObsIsReal[ind]
          siteKeepFlag = siteKeepFlag[ind]

          if verbose then $
              USR_MSG, 'Manual QC eliminated ' + $
                       STRCRA(numBadStationsRemoved) + $
                       ' stations.'

          numSnowfall = count

      endif

      if (numBadStationsRemoved ne numBadStationsListed) then begin
          if verbose then $
              USR_MSG, 'No reports received for ' + $
                       STRCRA(numBadStationsListed - $
                              numBadStationsRemoved) + $
                       ' of the stations listed in ' + $
                       outputDir + '/' + badStationFile
      endif
                   
  endif


; 3a-5. Find reports at the same geographic location.

; Colocated points will disrupt the kriging process. We could probably
; do this in the section where all_dist_2d is populated, but it is
; simpler to get it out of the way here.

  debug_tag = 3000

  flag = MAKE_ARRAY(numSnowfall, /BYTE, VALUE = 1B)

;  for j = 1UL, numSnowfall - 1UL do begin
  for j = numSnowfall - 1L, 1L, -1L do begin ; preferable removes sim obs

      lonj = siteLon[j]
      latj = siteLat[j]

      for i = 0UL, j - 1UL do begin

          loni = siteLon[i]
          lati = siteLat[i]

          lonDiff = ABS(lonj - loni)
          latDiff = ABS(latj - lati)

          if ((lonDiff lt 1.0D-6) and (latDiff lt 1.0D-6)) then flag[j] = 0B

      endfor

  endfor

  ind = WHERE(flag, count)
  flag = !NULL

  if (count ne numSnowfall) then begin

;      snowfallReport = snowfallReport[ind]
      siteID = siteID[ind]
      siteLon = siteLon[ind]
      siteLat = siteLat[ind]
      siteElev = siteElev[ind]
      siteObs = siteObs[ind]
      siteObsIsReal = siteObsIsReal[ind]
      siteKeepFlag = siteKeepFlag[ind]
;      g = g[ind]

      if verbose then $
          USR_MSG, 'After eliminating co-located data ' + $
                   STRCRA(count) + ' reports remain.'

      numSnowfall = count

  endif


; 3a-6. Sample the background snowfall at observation points, using
;       bilinear interpolation.

  i = (siteLon - minLonAnl) / lonResOut - 0.5D
  j = (siteLat - minLatAnl) / latResOut - 0.5D

  siteGuess = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv) ; background, inches
;GF 20170309 vv
;  siteQBS = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv)
;GF 20170309 ^^
  siteMinTemp = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv)
  siteAveTemp = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv)
  siteWEASDSource = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv)

  for k = 0, numSnowfall - 1 do begin

      ik = i[k]
      jk = j[k]

      i1 = FLOOR(ik)
      i2 = i1 + 1L
      j1 = FLOOR(jk)
      j2 = j1 + 1L

      if ((i1 lt 0L) or $
          (i2 ge numColsAnl) or $
          (j1 lt 0L) or $
          (j2 ge numRowsAnl)) then begin
          ERR_MSG, 'ERROR: fatal programming error. Out of bounds ' + $
                   'snowfall reports found; these should have been ' + $
                   'eliminated earlier.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      gll = snflBGGridInches[i1, j1]
      glr = snflBGGridInches[i2, j1]
      gur = snflBGGridInches[i2, j2]
      gul = snflBGGridInches[i1, j2]

      if ((gll eq ndv) or $
          (glr eq ndv) or $
          (gur eq ndv) or $
          (gul eq ndv)) then $
              siteGuess[k] = ndv $
      else $
          siteGuess[k] = gll * (i2 - ik) * (j2 - jk) + $
                         glr * (ik - i1) * (j2 - jk) + $
                         gur * (ik - i1) * (jk - j1) + $
                         gul * (i2 - ik) * (jk - j1)

;GF 20170309 vv
;      gll = QBSGridInches[i1, j1]
;      glr = QBSGridInches[i2, j1]
;      gur = QBSGridInches[i2, j2]
;      gul = QBSGridInches[i1, j2]

;      if ((gll eq ndv) or $
;          (glr eq ndv) or $
;          (gur eq ndv) or $
;          (gul eq ndv)) then $
;              siteQBS[k] = ndv $
;      else $
;          siteQBS[k] = gll * (i2 - ik) * (j2 - jk) + $
;                       glr * (ik - i1) * (j2 - jk) + $
;                       gur * (ik - i1) * (jk - j1) + $
;                       gul * (i2 - ik) * (jk - j1)
;GF 20170309 ^^

      gll = minTempGrid[i1, j1]
      glr = minTempGrid[i2, j1]
      gur = minTempGrid[i2, j2]
      gul = minTempGrid[i1, j2]

      if ((gll eq ndv) or $
          (glr eq ndv) or $
          (gur eq ndv) or $
          (gul eq ndv)) then $
              siteMinTemp[k] = ndv $
      else $
          siteMinTemp[k] = gll * (i2 - ik) * (j2 - jk) + $
                 glr * (ik - i1) * (j2 - jk) + $
                 gur * (ik - i1) * (jk - j1) + $
                 gul * (i2 - ik) * (jk - j1)

      gll = aveTempGrid[i1, j1]
      glr = aveTempGrid[i2, j1]
      gur = aveTempGrid[i2, j2]
      gul = aveTempGrid[i1, j2]

      if ((gll eq ndv) or $
          (glr eq ndv) or $
          (gur eq ndv) or $
          (gul eq ndv)) then $
              siteAveTemp[k] = ndv $
      else $
          siteAveTemp[k] = gll * (i2 - ik) * (j2 - jk) + $
                           glr * (ik - i1) * (j2 - jk) + $
                           gur * (ik - i1) * (jk - j1) + $
                           gul * (i2 - ik) * (jk - j1)

;+
;     We have to use the maximum value to get siteWEASDSource, which
;     creates a little ambiguity with those results, but it cannot be
;     helped, since nearest neighbor sampling will usually result in
;     some points where the background is nonzero but siteWEASDSource
;     is zero, and we have to avoid that since it does not make
;     sense.
;-
      gll = WEASDSourceGrid[i1, j1]
      glr = WEASDSourceGrid[i2, j1]
      gur = WEASDSourceGrid[i2, j2]
      gul = WEASDSourceGrid[i1, j2]

      if ((gll eq ndv) or $
          (glr eq ndv) or $
          (gur eq ndv) or $
          (gul eq ndv)) then $
              siteWEASDSource[k] = ndv $
      else $
          siteWEASDSource[k] = MAX([gll, glr, gur, gul])

  endfor

  ind = WHERE((siteGuess ne ndv), count)
  if (count eq 0) then begin
      ERR_MSG, 'ERROR: After eliminating first guess no-data values, ' + $
               'no data remain for analysis.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  ndvInd = WHERE(siteGuess eq ndv, ndvCount)
  if ((ndvCount gt 0) and (dryRun eq 0)) then begin


;     List unused points in a CSV file. If anybody asks why did
;     such-and-such observation not get used, and it is in this file,
;     the answer is "because our first guess had a no-data
;     value at that location."

      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_1st_guess_no_data_obs.csv'

      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, 'longitude,latitude,snowfall_obs_inches,station_id'
      for sc = 0, ndvCount - 1 do begin
          PRINTF, lun, $
                  STRCRA(STRING(siteLon[ndvInd[sc]], $
                                FORMAT = '(F13.8)')) + ',' + $
                  STRCRA(STRING(siteLat[ndvInd[sc]], $
                                FORMAT = '(F13.8)')) + ',' + $
                  STRCRA(siteObs[ndvInd[sc]]) + ',' + $
;                  snowfallReport[ndvInd[sc]].station_id
                  siteID[ndvInd[sc]]
      endfor

      FREE_LUN, lun

      if (updateGISProject) then begin
          origFile = outputDir + '/' + csvFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_1st_guess_no_data_obs.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif

  debug_tag = 3100

  if (count ne numSnowfall) then begin

;      snowfallReport = snowfallReport[ind]
      siteID = siteID[ind]
      siteLon = siteLon[ind]
      siteLat = siteLat[ind]
      siteElev = siteElev[ind]
      siteObs = siteObs[ind]
      siteObsIsReal = siteObsIsReal[ind]
      siteGuess = siteGuess[ind]
;GF 20170309 vv
;      siteQBS = siteQBS[ind]
;GF 20170309 ^^
      siteMinTemp = siteMinTemp[ind]
      siteAveTemp = siteAveTemp[ind]
      siteWEASDSource = siteWEASDSource[ind]
      siteKeepFlag = siteKeepFlag[ind]

      if verbose then $
          USR_MSG, 'After eliminating first guess no-data values ' + $
                   STRCRA(count) + ' reports remain.'

      numSnowfall = count

  endif


; GREG AND KENT ADD TEMPERATURE QC TO POINTS HERE
; 3a-7. Perform temperature quality control check.

;  checkID = ['BRSW3', 'ONMM5', 'BRSQ3', 'MN-KD-1']
;  checkID = ['MMTN7', 'MTLT1', 'MI-OC-2', 'BRSW3', 'LBOV2']
;  checkID = ['MI-AT-6']

  if verbose then begin

      foundAny = 0

      for ic = 0, N_ELEMENTS(checkID) - 1 do begin

          csc = WHERE(siteID eq checkID[ic], count)
          if (count eq 0) then CONTINUE
          if (count ne 1) then begin
              ERR_MSG, 'ERROR: fatal programming error. Multiple ' + $
                       'matches for station ID "' + checkID[ic] + '".'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif
          csc = csc[0]

          if NOT(foundAny) then begin

              PRINT, 'Temperature test:'

              PRINT, '      STATION ID ' + $
                     'LONGITUDE ' + $
                     'LATITUDE ' + $
                     ' ELEV ' + $
                     '   SNFL ' + $
                     'MIN TEMP ' + $
                     '  CUTOFF ' + $
                     'AVE TEMP ' + $
                     '  CUTOFF'

              PRINT, '      ---------- ' + $
                     '--------- ' + $
                     '-------- ' + $
                     ' ---- ' + $
                     ' ------ ' + $
                     '-------- ' + $
                     '-------- ' + $
                     '-------- ' + $
                     '--------'

              idFormat = '(A16)'
              lonFormat = '(F9.4)'
              latFormat = '(F8.4)'
              elevFormat = '(I5)'
              valFormat = '(F7.3)'
              tempFormat = '(F6.1)'

              foundAny = 1B

          endif

          PRINT, STRING(siteID[csc], $
                        FORMAT = idFormat) + ' ' + $
                 STRING(siteLon[csc], $
                        FORMAT = lonFormat) + ' ' + $
                 STRING(siteLat[csc], $
                        FORMAT = latFormat) + ' ' + $
                 STRING(siteElev[csc], $
                        FORMAT = elevFormat) + ' ' + $
                 STRING(siteObs[csc], $
                        FORMAT = valFormat) + '   ' + $
                 STRING(siteMinTemp[csc], $
                        FORMAT = tempFormat) + '   ' + $
                 STRING(minTempCutoff, FORMAT = tempFormat) + '   ' + $
                 STRING(siteAveTemp[csc], $
                        FORMAT = tempFormat) + '   ' + $
                 STRING(aveTempCutoff, FORMAT = tempFormat)

      endfor

  endif

;+
; Here, "and" is used for the min/ave temperature criteria to be
; conservative about removing snowfall that previous evidence in the
; analysis (in this case an actual observation) has produced.
;-
  hotInd = WHERE((siteKeepFlag ne 1) and $
                 (siteObs gt 0.0) and $ ; maybe use 0.01 here?
                 ((siteMinTemp ge (minTempCutoff + tempSlack)) and $ ; AND/OR
                  (siteAveTemp ge (aveTempCutoff + tempSlack))), $   ; 20181010
                 hotCount, $
                 COMPLEMENT = coolInd, NCOMPLEMENT = coolCount)

  if ((coolCount + hotCount) ne numSnowfall) then begin
      ERR_MSG, 'ERROR: Fatal programming error. "hotInd" / "coolInd" ' + $
               'determination for stations gives ' + $
               STRCRA(coolCount + hotCount) + ' sites; this should be ' + $
               'the total number of snowfall reports, which is ' + $
               STRCRA(numSnowfall) + '.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if (hotCount gt 0) then begin

      if verbose then begin

          USR_MSG, 'Removing ' + STRCRA(hotCount) + $
                   ' observations due to too-high ' + temperature_source + $
                   ' analysis temperatures.'

          PRINT, '      STATION ID  ' + $
                 'LONGITUDE  ' + $
                 'LATITUDE  ' + $
                 ' ELEV  ' + $
                 '    VAL  ' + $
                 'MIN TEMP  ' + $
                 'AVE TEMP  '

          PRINT, '      ----------  ' + $
                 '---------  ' + $
                 '--------  ' + $
                 ' ----  ' + $
                 '    ---  ' + $
                 '--------  ' + $
                 '--------'

          idFormat = '(A16)'
          lonFormat = '(F9.4)'
          latFormat = '(F8.4)'
          elevFormat = '(I5)'
          valFormat = '(F7.3)'
          tempFormat = '(F6.1)'

          for hc = 0, hotCount - 1 do begin

              PRINT, STRING(siteID[hotInd[hc]], $
                            FORMAT = idFormat) + '  ' + $
                     STRING(siteLon[hotInd[hc]], $
                            FORMAT = lonFormat) + '  ' + $
                     STRING(siteLat[hotInd[hc]], $
                            FORMAT = latFormat) + '  ' + $
                     STRING(siteElev[hotInd[hc]], $
                            FORMAT = elevFormat) + '  ' + $
                     STRING(siteObs[hotInd[hc]], $
                            FORMAT = valFormat) + '    ' + $
                     STRING(siteMinTemp[hotInd[hc]], $
                            FORMAT = tempFormat) + '    ' + $
                     STRING(siteAveTemp[hotInd[hc]], $
                            FORMAT = tempFormat)

          endfor

      endif

      siteID = siteID[coolInd]
      siteLon = siteLon[coolInd]
      siteLat = siteLat[coolInd]
      siteElev = siteElev[coolInd]
      siteObs = siteObs[coolInd]
      siteObsIsReal = siteObsIsReal[coolInd]
      siteGuess = siteGuess[coolInd]
;GF 20170309 vv
;     siteQBS = siteQBS[coolInd]
;GF 20170309 ^^
      siteMinTemp = siteMinTemp[coolInd]
      siteAveTemp = siteAveTemp[coolInd]
      siteWEASDSource = siteWEASDSource[coolInd]
      siteKeepFlag = siteKeepFlag[coolInd]

      if verbose then $
          USR_MSG, 'After eliminating "too-warm" reports ' + $
                   STRCRA(coolCount) + ' reports remain.'

      numSnowfall = coolCount

  endif

  hotInd = !NULL
  coolInd = !NULL

;+
; Point data is ready!
;-

; Set up a structure for diagnostic data at each observing location.

  debug_tag = 3200

  snflPtsDiag = REPLICATE({stationID: '', $
                           stationLon: '', $
                           stationLat: '', $
                           snowfallObs: 0.0, $
                           snowfallObsAboveTrace: -2, $
                           snowfall1stGuess: 0.0, $
                           snowfall1stGuessAboveTrace: -2, $
                           snowfall1stGuessCategory: '', $
                           snowfallObsAssim1: -2, $
                           snowfallObsPassedAssim1QC: -2, $
                           snowfall1stPass: 0.0, $
                           snowfall1stPassAboveTrace: -2, $
                           snowfall1stPassCategory: '', $
                           snowfallObsAssim2: -2, $
                           snowfallObsPassedAssim2QC: -2, $
                           snowfall2ndPass: 0.0, $
                           snowfall2ndPassAboveTrace: -2, $
                           snowfall2ndPassCategory: ''}, $
                          numSnowfall)


  for sc = 0, numSnowfall - 1 do begin
;      snflPtsDiag[sc].stationID = snowfallReport[sc].station_id
      snflPtsDiag[sc].stationID = siteID[sc]
      snflPtsDiag[sc].stationLon = siteLon[sc]
      snflPtsDiag[sc].stationLat = siteLat[sc]
  endfor


; Set up a structure for diagnostic info and skill measures for the
; full snowfall analysis process.

  sfav2Diag = {QPE_used: 0, $
               QPF_source: '', $
               temperature_source: '', $
               num_obs: 0L, $
               background_RMSE: ndv, $
               background_POD: ndv, $
               background_geom_mean_bias: ndv, $
               background_FAR: ndv, $
               pass_1_is_log_ratio: 0, $
               pass_1_semivariogram_num_obs: 0L, $
               pass_1_semivariogram_fit_RMSE: ndv, $
               pass_1_semivariogram_fit_RMSE_in_range: ndv, $
               pass_1_semivariogram_nugget: ndv, $
               pass_1_semivariogram_sill: ndv, $
               pass_1_semivariogram_range_meters: ndv, $
               pass_1_assim_num_obs: 0L, $
               pass_1_assim_uses_radial_means: 0, $
               pass_1_mean_points_per_solver: 0L, $
               pass_1_num_solvers: 0L, $
               pass_1_assim_wall_time: ndv, $
               pass_1_assim_cross_validation_RMSE: ndv, $
               pass_1_RMSE: ndv, $
               pass_1_POD: ndv, $
               pass_1_geom_mean_bias: ndv, $
               pass_1_FAR: ndv, $
               pass_2_semivariogram_num_obs: 0L, $
               pass_2_semivariogram_fit_RMSE: ndv, $
               pass_2_semivariogram_fit_RMSE_in_range: ndv, $
               pass_2_semivariogram_nugget: ndv, $
               pass_2_semivariogram_sill: ndv, $
               pass_2_semivariogram_range_meters: ndv, $
               pass_2_assim_num_obs: 0L, $
               pass_2_assim_uses_radial_means: 0, $
               pass_2_mean_points_per_solver: 0L, $
               pass_2_num_solvers: 0L, $
               pass_2_assim_wall_time: ndv, $
               pass_2_assim_cross_validation_RMSE: ndv, $
               pass_2_RMSE: ndv, $
               pass_2_POD: ndv, $
               pass_2_geom_mean_bias: ndv, $
               pass_2_FAR: ndv, $
               analysis_wall_time: ndv $
              }

  if ISA(QPEGrid) then sfav2Diag.QPE_used = 1
  sfav2Diag.num_obs = numSnowfall
  sfav2Diag.QPF_source = QPF_source
  sfav2Diag.temperature_source = temperature_source

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if verbose then begin

      ind = WHERE(siteObs ge 0.01, count)

      if (count gt 0) then begin


;         Display observations.

          X_MAP_GEO_POINTS, siteLon[ind], siteLat[ind], siteObs[ind], $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_snowfall, $
                            red_snowfall, grn_snowfall, blu_snowfall, $
                            0, $
                            status, $
                            ndv = NDV, $
                            /SHOW_HIGH, $
                            TITLE = 'Observed ' + durationStr + '-hour ' + $
                            'Nonzero Snowfall, ' + anlEndDate_YYYYMMDDHH, $
                            /colorbar, $
                            XSIZE_TARGET = xszt, $
                            UNITS = 'inches', $
                            SHAPE_PATH_LIST = shapePathList

      endif

  endif

  if produceImages then begin


;     Write snowfall background + all observations to an image.

      title = 'Snowfall Analysis v2 First Guess!C' + longSubTitle

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_07_snowfall_1st_guess_obs.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          snflBGGridInches[extraCols:numColsAnl - extraCols - 1, $
                           extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_snowfall, red_snowfall, grn_snowfall, blu_snowfall, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          title, $
          'inches', $
          outputDir + '/' + PNGFile, $
          POINT_LON = siteLon, $
          POINT_LAT = siteLat, $
          POINT_VAL = siteObs, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_snowfall, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

;----------------------------;
; 3b. Evaluate observations. ;
;----------------------------;

  debug_tag = 3300

;--------------------------------------------------------------------;
; 3b-1. Evaluate errors and difference/bias between observations and ;
;       first guess.                                                 ;
;--------------------------------------------------------------------;

; Regarding trace amounts: the NOHRSC database converts "trace"
; reports to 0.001". Sometimes these come through as
; 0.000984001. However, the smallest above-trace amount ever encoded
; is 0.1". In other words, we see values of 0.0, 0.000984001,
; 0.001, and 0.1, and above, but nothing between 0.001 and 0.1.

;Kent modified trace amount to .00100001 for testing 

;+ GF 20180724
; The snflThreshld should be set to something like 1e-6 larger than
; the threshold you would expect (e.g., 0.100001 instead of 0.1). The
; purpose is to prevent values of 0.1 from being judged greater than
; the threshold as a result of floating point round off errors, which
; is very common for 4-byte floats.
;-
  snflThreshold = 0.100001 ; inches


; if useRatio is set, then the log-ratio between first guess and
; observations is evaluated and adjusted first, and the difference of
; the residuals is then corrected. If useRatio is not set, a single
; pass is done to simultaneously correct error and bias.

;  useRatio = 1 - moved up
;  if useRatio then varName = 'log-ratio' else varName = 'difference'
;  - moved down


; If we use snflThreshold as our threshold, the four components of the
; contingency table, with f for the observation and g for the guess,
; are:
;
;   A. f > threshold, g > threshold (hit)
;   B. f <= threshold, g > threshold (false positive / type I error)
;   C. f > threshold, g <= threshold (miss / type II error)
;   D. f <= threshold, g <= threshold (correct negative)
;
; It is not clear whether or not we should do spatial statistics for
; all of these outcomes. Certainly case A qualifies. However, if
; useRatio is set, then we are calculating the ratio g / f, and
; errors (cases B and C) are ambiguous:
;
;   Type I errors will often give division by zero.
;   Type II errors will give a ratio of zero.
;
; Additionally, correct negatives will give an indeterminate
; result.
;
; Consequently, the "useRatio" case implies that the initial kriging
; process will only correct bias in the first guess, and errors (B and
; C) need to be addressed by an additional process.


; Update snflPtsDiag with f and g, corresponding above-threshold
; flags for f and g, and the error category for the first guess value.

  for sc = 0, numSnowfall - 1 do begin

      snflPtsDiag[sc].snowfallObs = siteObs[sc]
      snflPtsDiag[sc].snowfallObsAboveTrace = $
          FIX(siteObs[sc] gt snflThreshold)
      snflPtsDiag[sc].snowfall1stGuess = siteGuess[sc]
      snflPtsDiag[sc].snowfall1stGuessAboveTrace = $
          FIX(siteGuess[sc] gt snflThreshold)

      case (snflPtsDiag[sc].snowfallObsAboveTrace + $
            snflPtsDiag[sc].snowfall1stGuessAboveTrace * 2) of
          0: snflPtsDiag[sc].snowfall1stGuessCategory = 'D' ; correct negative
          1: snflPtsDiag[sc].snowfall1stGuessCategory = 'C' ; miss
          2: snflPtsDiag[sc].snowfall1stGuessCategory = 'B' ; false positive
          3: snflPtsDiag[sc].snowfall1stGuessCategory = 'A' ; hit
      endcase

  endfor


; Index and count contingency table subsets for 1st guess.

  abInd = WHERE(siteGuess gt snflThreshold, abCount)      ; predicted events
  acInd = WHERE(siteObs gt snflThreshold, acCount) ; observed events
  bcInd = WHERE((siteObs gt snflThreshold) xor $
                (siteGuess gt snflThreshold), bcCount)    ; errors
  abcInd = WHERE((siteObs gt snflThreshold) or $
                 (siteGuess gt snflThreshold), abcCount)  ; all events
  aInd = WHERE((siteObs gt snflThreshold) and $
               (siteGuess gt snflThreshold), aCount)      ; hits
  bInd = WHERE((siteGuess gt snflThreshold) and $
               (siteObs le snflThreshold), bCount) ; false positives
  dInd = WHERE((siteGuess le snflThreshold) and $
               (siteObs le snflThreshold), dCount) ; correct negatives
  abcdInd = LINDGEN(numSnowfall)
  abcdCount = numSnowfall

;+
; False alarm trajectory.
; GF 20190805 modified logic
; First bit (1) tells you if it was false positive in the background.
;-
  fat = MAKE_ARRAY(numSnowfall, VALUE = 0)
  ;; if (abCount gt 0) then fat[abInd] = fat[abInd] + 1
  if (bCount gt 0) then fat[bInd] = fat[bInd] + 1
;- GF


; Report on contingency inventory.

  if verbose then begin
      USR_MSG, 'First guess:'
      USR_MSG, '  A (hits): ' + STRCRA(aCount)
      USR_MSG, '  B (false positives): ' + STRCRA(bCount)
      USR_MSG, '  C (misses): ' + STRCRA(acCount - aCount)
      USR_MSG, '  D (correct negatives): ' + STRCRA(dCount)
  endif


; Report on bias and errors in 1st guess.

  if verbose then USR_MSG, 'Bias/error stats for 1st guess:'

  if (aCount gt 0) then begin

      sfav2Diag.background_RMSE = $
          SQRT(TOTAL((siteGuess[aInd] - siteObs[aInd])^2.0) / aCount)

      sfav2Diag.background_POD = FLOAT(aCount) / FLOAT(acCount)

      backgroundLogBias = ALOG10(siteGuess[aInd] / siteObs[aInd])
      ;; sfav2Diag.background_geom_mean_bias = $
      ;;     10.0D^MEAN(ALOG10(siteGuess[aInd] / f[aInd]))
      sfav2Diag.background_geom_mean_bias = $
          10.0D^MEAN(backgroundLogBias)

      ;; sdBias = STDDEV(backgroundLogBias)
      ;; b1 = 10.0D^(MEAN(backgroundLogBias) - sdBias)
      ;; b2 = 10.0D^(MEAN(backgroundLogBias) + sdBias)
      ;; PRINT, 'bias std. dev. range: ', b1, b2

      if verbose then begin
          USR_MSG, '- RMSE (hits): ' + $
                   STRCRA(sfav2Diag.background_RMSE)
          USR_MSG, '- POD: ' + $
                   STRCRA(aCount) + ' / ' + $
                   STRCRA(acCount) + ' = ' + $
                   STRCRA(sfav2Diag.background_POD)
          USR_MSG, '- Geometric mean bias: ' + $
                   STRCRA(sfav2Diag.background_geom_mean_bias)
          if (aCount gt 1) then $
              USR_MSG, '- Geometric std. dev. of bias: ' + $
                       STRCRA(10.0D^(STDDEV(backgroundLogBias)))
      endif

  endif else begin

      if verbose then USR_MSG, '- no hits'

  endelse

  if (bCount gt 0) then begin

      sfav2Diag.background_FAR = FLOAT(bCount) / FLOAT(abCount)

      falseAlarmMean = MEAN(siteGuess[bInd])
      falseAlarmMedian = MEDIAN(siteGuess[bInd], /EVEN)

      ;; if verbose then USR_MSG, '- FAR: ' + $
      ;;                          STRCRA(bCount) + ' / ' + $
      ;;                          STRCRA(abCount) + ' = ' + $
      ;;                          STRCRA(sfav2Diag.background_FAR) + $
      ;;                          ' ; mean false alarm amount = ' + $
      ;;                          STRCRA(falseAlarmMean)

      if verbose then begin
          USR_MSG, '- FAR: ' + $
                   STRCRA(bCount) + ' / ' + $
                   STRCRA(abCount) + ' = ' + $
                   STRCRA(sfav2Diag.background_FAR)
          USR_MSG, '       mean false alarm amount = ' + $
                   STRCRA(falseAlarmMean)
          USR_MSG, '       median false alarm amount = ' + $
                   STRCRA(falseAlarmMedian)
      endif

  endif

  if (abcCount eq 0) then begin

      USR_MSG, 'NOTICE: No above-trace snowfall observed, no hits, no errors.'


;     Crop first guess to output grid and exit.

      outputGrid = $
          snflBGGridInches[extraCols:numColsAnl - extraCols - 1, $
                           extraRows:numRowsAnl - extraRows - 1]

      if NOT(dryRun) then begin
          USR_MSG, 'NOTICE: Exiting with first guess result.'
          GOTO, ANALYSIS_COMPLETE
      endif else begin
          GOTO, DRY_RUN_DONE
      endelse

  endif


; Kent uncommented temporarily 

;GF 20170309 vv
 ; Inventory sites where the QPF-based first guess did not have a type
 ; 1 error but the first guess did.

;;   QBSWinsInd = WHERE((siteObs gt snflThreshold) and $
;;                      (siteGuess le snflThreshold) and $
;;                      (siteQBS gt snflThreshold), count)
;; ;  if ((count gt 0) and verbose) then $
;;   if verbose then $
;;       USR_MSG, '******* QPF based snowfall out-detects 1st guess' + $
;;                ' at '  + STRCRA(count) + ' sites. *******'
;;   St4WinsInd = WHERE((siteObs gt snflThreshold) and $
;;                      (siteGuess gt snflThreshold) and $
;;                      (siteQBS le snflThreshold), count)
;; ;  if ((count gt 0) and verbose) then $
;;   if verbose then $
;;       USR_MSG, '******* First guess out-detects QPF based snowfall' + $
;;                ' at '  + STRCRA(count) + ' sites. *******'


 ;  Calculate FAR and POD for QPF-based snowfall


;;  acInd_ = WHERE(siteObs gt snflThreshold, acCount_) ; observed events
;;  
;;  aInd_ = WHERE((siteObs gt snflThreshold) and $
;;                (siteQBS gt snflThreshold), aCount_) ; hits
;;
;;  abInd_ = WHERE((siteQBS gt snflThreshold), abCount_) ; pedicted events 
;; 
;;  bInd_ = WHERE((siteQBS gt snflThreshold) and $ 
;;               (siteObs le snflThreshold), bCount_) ; false positives  
;;
;;
;;   if verbose then begin
;;
;;       falseAlarmMean = 0.0
;;       if (bCount_ gt 0) then begin 
;;           QBS_FAR = FLOAT(bCount_) / FLOAT(abCount_)
;;           falseAlarmMean = MEAN(siteQBS[bInd_])
;;           USR_MSG, '-QBS_FAR: ' + $
;;           STRCRA(bCount_) + ' / ' + $
;;           STRCRA(abCount_) + ' = ' + $
;;           STRCRA(QBS_FAR) + $
;;          ' ; mean false alarm amount = ' + $
;;          STRCRA(falseAlarmMean)
;;        endif 
;;
;;       if (aCount_ gt 0) then begin 
;;           QBS_POD = FLOAT(aCount_) / FLOAT(acCount_)
;;           USR_MSG, '-QBS_POD: ' + $
;;           STRCRA(aCount_) + ' / ' + $
;;           STRCRA(acCount_) + ' = ' + $
;;           STRCRA(QBS_POD)
;;       endif 
;;
;;    endif 

;GF 20170309 ^^


  if (dryRun eq 0) then begin

;+
; Write observations and differences to csv files.
;-
      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_obs.csv'
      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, $
              'longitude,latitude,snowfall_obs_inches,station_id,' + $
              'site_obs_is_real'
      for sc = 0, numSnowfall - 1 do begin
          PRINTF, lun, $
                  STRCRA(STRING(siteLon[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(STRING(siteLat[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(siteObs[sc]) + ',' + $
;              snowfallReport[sc].station_id
                  siteID[sc] + ',' + $
                  STRCRA(siteObsIsReal[sc])
      endfor
      FREE_LUN, lun
      if (updateGISProject) then begin
          origFile = outputDir + '/' + csvFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_obs.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_obs_diff_1.csv'
      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, $
              'longitude,latitude,snowfall_1st_guess_error_inches,station_id'
      for sc = 0, numSnowfall - 1 do begin
          PRINTF, lun, $
                  STRCRA(STRING(siteLon[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(STRING(siteLat[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(siteGuess[sc] - siteObs[sc]) + ',' + $
;              snowfallReport[sc].station_id
                  siteID[sc]
      endfor
      FREE_LUN, lun
      if (updateGISProject) then begin
          origFile = outputDir + '/' + csvFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_obs_diff_1.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif


; Index observations that will be assimilated.
;
; If useRatio is set, then the log-ratio of g and f will be
; assimilated, and only case A is included (no errors).
;
; If useRatio is not set, the difference between f and g will be
; assimilated, and cases A, B, and C are all included, which
; implicitly addresses errors.

  debug_tag = 3400

  if useRatio then begin

      assimPointsInd = aInd
      assimPointsCount = aCount
      if (aCount eq 0) then begin


;         No hits in 1st guess; try a single-pass analysis

          USR_MSG, 'NOTICE: First guess has no hits. Attempting ' + $
                   'single pass assimilation of differences.'

          useRatio = 0
          assimPointsInd = abcdInd
          assimPointsCount = abcdCount
;          maxLagMeters_P1 = minLagMeters + 1.0D ; HELPMEGREG

      endif

  endif else begin

      assimPointsInd = abcdInd
      assimPointsCount = abcdCount

  endelse

PASS_1_CHECK_ASSIM_POINTS_COUNT:

  if (assimPointsCount lt minAssimPoints) then begin

      if useRatio then begin


;         Switch to assimilating additive differences in a single
;         pass.

          USR_MSG, 'NOTICE: Number of valid, nonzero log-ratio values ' + $
                   'available for assimilation (' + $
                   STRCRA(assimPointsCount) + $
                   ') does not meet minimum threshold (' + $
                   STRCRA(minAssimPoints) + $
                   '). Attempting single-pass ' + $
                   'assimilation of differences.'

          useRatio = 0
          assimPointsInd = abcdInd
          assimPointsCount = abcdCount
          GOTO, PASS_1_CHECK_ASSIM_POINTS_COUNT

      endif

      USR_MSG, 'NOTICE: Number of observations available for ' + $
               'assimilation (' + STRCRA(assimPointsCount) + $
               ') does not meet minimum threshold (' + $
               STRCRA(minAssimPoints) + ').'

;+ GF 20180823
;     Given that there is some observed snowfall, make sure we only
;     exit with the first guess if the first guess is actually based
;     on something real.
;- GF 20180823
      if (NOT(ISA(QPEGrid)) and (QPF_source eq 'Missing')) then begin
          ERR_MSG, 'ERROR: Insufficient information exists to produce ' + $
                   'a usable background. No analysis is possible.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif


;     Crop first guess to output grid and exit.

      outputGrid = $
          snflBGGridInches[extraCols:numColsAnl - extraCols - 1, $
                           extraRows:numRowsAnl - extraRows - 1]

      if NOT(dryRun) then begin
          USR_MSG, 'NOTICE: Exiting with first guess result.'
          GOTO, ANALYSIS_COMPLETE
      endif else begin
          GOTO, DRY_RUN_DONE
      endelse

  endif

  if NOT(useRatio) then begin

      
;     Replace "_P1" variogram/kriging parameters with "_P2"
;     values. Most significantly this means that if useRatio was
;     switched from 1 to 0 above, then the maximum lag considered for
;     variograms is reduced, preventing large variogram ranges and
;     very slow one-pass assimilations.

      maxLagMeters_P1 = maxLagMeters_P2
      minOutlierDiff_P1 = minOutlierDiff_P2

  endif


; Calculate bias (log-ratio or difference) between 1st guess and
; observations.

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s before calculating background bias:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if useRatio then begin

      z_ = ALOG10(siteGuess[assimPointsInd] / siteObs[assimPointsInd])
      minOutlierDiff_P1 = 0.0 ; do not use this parameter for ratios

  endif else begin

      z_ = siteGuess[assimPointsInd] - siteObs[assimPointsInd]


;     Get a subset of z_ that does not include correct negatives for
;     the outlier test.

      abcFlag = (siteObs[assimPointsInd] gt snflThreshold) or $
                (siteGuess[assimPointsInd] gt snflThreshold)

      abcInd = WHERE(abcFlag, abcCount)
      if (abcCount eq 0) then begin
          ERR_MSG, 'ERROR: abcInd failure.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

  endelse

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if useRatio then begin
      zTitle = 'Log-ratio of 1st Guess to Obs. Snowfall'
      zUnits = 'dimensionless'
  endif else begin
      zTitle = 'Difference Between 1st Guess and Obs. Snowfall'
      zUnits = 'inches'
  endelse

  if useRatio then begin

;+
;     Generate a histogram of the data to assimilate (log-ratios) for
;     the first assimilation pass.
;-
      hMin = -2.0
      hMax = 2.0
      binSize = 0.1
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad
      hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize
      zHist = HISTOGRAM(z_, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      if verbose then begin

;+
;         Display the histogram of data to assimilate (log-ratios) for
;         the first assimilation pass.
;-
          WSET_OR_WINDOW, 1     ;, XSIZE = 800, YSIZE = 400

          PLOT, hAxis, zHist[0:numBins - 1], PSYM = 10, $
                TITLE = '1a. ' + zTitle, XTITLE = zUnits

      endif

  endif else begin

;+
;     Generate a histogram of the data to assimilate (differences) for
;     the first (and only) assimilation pass.
;-
      hMin = -5.0
      hMax = 5.0
      binSize = 0.25
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad
      hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize
      zHist = HISTOGRAM(z_, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      if verbose then begin

;+
;         Display the histogram of data to assimilate (differences)
;         for the first (and only) assimilation pass.
;-
          WSET_OR_WINDOW, 1     ;, XSIZE = 800, YSIZE = 400


          PLOT, hAxis, zHist[0:numBins - 1], PSYM = 10, $
                TITLE = '1b. ' + zTitle, XTITLE = zUnits, $
                YRANGE = [1, MAX(zHist)], /YLOG

      endif

  endelse

  if verbose then begin

;+
;     Display difference/bias points
;-
      if (useRatio) then $
          edges = edges_sfRatio $
      else $
          edges = edges_sfDiff

      X_MAP_GEO_POINTS, siteLon[assimPointsInd], siteLat[assimPointsInd], z_, $
                        minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                        edges, red_sfDiff, grn_sfDiff, blu_sfDiff, $
                        0, $
                        status, $
                        NDV = ndv, $
                        /SHOW_LOW, $
                        /SHOW_HIGH, $
                        TITLE = zTitle + ': ' + subTitle, $
                        /COLORBAR, $
                        XSIZE_TARGET = xszt, $
                        UNITS = zUnits, $
                        SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write difference/bias points to an image.

      if (useRatio) then $
          edges = edges_sfRatio $
      else $
          edges = edges_sfDiff

      if useRatio then $
          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_08_obs_to_1st_guess_log_ratio.png' $
      else $
          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_08_obs_to_1st_guess_difference.png'

      killme = snflBGGridInches
      killme[*,*] = ndv

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          killme[extraCols:numColsAnl - extraCols - 1, $
                 extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges, red_sfDiff, grn_sfDiff, blu_sfDiff, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          zTitle + '!C' + longSubTitle, $
          zUnits, $
          outputDir + '/' + PNGFile, $
          POINT_LON = siteLon[assimPointsInd], $
          POINT_LAT = siteLat[assimPointsInd], $
          POINT_VAL = z_, $
          /SHOW_LOW, $
          /SHOW_HIGH, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

      killme = !NULL

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


; 3b-1.5. Evaluate distances between points representing hits and
; errors in the first guess.

; NOTE: we have already established that abcCount is greater than zero.

  ;; if verbose then $
  ;;     USR_MSG, 'Calculating nearest-neighbor distances between events ' + $
  ;;              '(hits and errors).'

  ;; if (abcCount gt 1) then begin
  ;;     FIND_NEAREST_NEIGHBOR, x[abcInd], $
  ;;                            y[abcInd], $
  ;;                            nnInd, $
  ;;                            nnDist, $
  ;;                            DISTANCE_PRECISION = dPrecision, $
  ;;                            HASH = verbose

  ;;     order = SORT(nnDist)
  ;;     if verbose then begin
  ;;         USR_MSG, 'hits + errors median distance: ' + $
  ;;                  STRCRA(nnDist[order[abcCount / 2]]) + ' meters'
  ;;         USR_MSG, 'hits + errors mean distance: ' + $
  ;;                  STRCRA(MEAN(nnDist)) + ' meters'
  ;;     endif
  ;; endif


  if (NOT(useRatio) and (abcCount lt minAssimPoints)) then begin


;     Modify maximum lag (doing it for both "_P1" and "_P2" ensures
;     sanity in distance calculation and weeding of correct negatives
;     to follow) to match the minimum range, effectively fixing the
;     semivariogram range at minRangeMeters.

      USR_MSG, 'NOTICE: Low hits + errors count = ' + STRCRA(abcCount) + $
               '; fixing the semivariogram range at ' + $
               STRCRA(minRangeMeters_P1 / 1000.0) + ' km.'
;+ GF 20180806
;      maxLagMeters_P1 = minRangeMeters
;      maxLagMeters_P2 = minRangeMeters
      maxLagMeters_P1 = minRangeMeters_P1
      maxLagMeters_P2 = minRangeMeters_P2
;- GF 20180806

  endif


;-------------------------------------------------------------------;  
; 3b-2. Calculate and record distances between all obs points up to ;
;      but not including) a maximum separation of                   ;
;      2 x (maxLagMeters_P1 > maxLagMeters_P2).                     ;
;-------------------------------------------------------------------;  

  debug_tag = 3500

; Establish maximum separation to measure between sites and
; approximate corresponding longitude/latitude separations. Note that
; the latter must represent distances greater than the maxSepMeters
; 100% of the time for this methodology to be effective.

  maxSepMeters = 2.0D * (maxLagMeters_P1 > maxLagMeters_P2)

  if verbose then $
      USR_MSG, 'Generating distances between observations up to ' + $
               STRCRA(maxSepMeters / 1000.0) + ' km.'

  maxSepDegLon = 2.0D * maxLagDegLon ; see above
  maxSepDegLat = 2.0D * maxLagDegLat ; see above


; Confirm that approximate separations are valid.

  maxLat = MAX(ABS(siteLat), maxLatInd)

  test = DISTANCE(dPrecision, $
                  siteLon[maxLatInd], siteLat[maxLatInd], $
                  siteLon[maxLatInd] + maxSepDegLon, siteLat[maxLatInd])
  if (test lt maxSepMeters) then begin
      ERR_MSG, 'ERROR: Maximum separation longitude estimate is too small.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  minLat = MIN(ABS(siteLat), minLatInd)

  sign = siteLat[minLatInd] / ABS(siteLat[minLatInd])
  test = DISTANCE(dPrecision, $
                  siteLon[minLatInd], siteLat[minLatInd], $
                  siteLon[minLatInd], siteLat[minLatInd] - sign * maxSepDegLat)
  if (test lt maxSepMeters) then begin
      ERR_MSG, 'ERROR: Maximum separation latitude estimate is too small.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Guess at the maximum number of neighbors for any point, so the #
; columns in the 2-D arrays generated to store distances and indices
; is kept to a minimum.

  n = numSnowfall ; WTF?

  numDistances = numSnowfall * (numSnowfall - 1L) / 2L

  if verbose then USR_MSG, 'Number of observation points: ' + STRCRA(n)

  numDistCalcGuess = 0UL
  maxNumNeighborsGuess = 0UL

  for i = 0UL, numSnowfall - 1UL do begin

      loni = siteLon[i]
      lati = siteLat[i]

      boxMinLon = loni - maxSepDegLon
      boxMaxLon = loni + maxSepDegLon
      boxMinLat = lati - maxSepDegLat
      boxMaxLat = lati + maxSepDegLat

      ind = WHERE((siteLon gt boxMinLon) and $
                  (siteLon lt boxMaxLon) and $
                  (siteLat gt boxMinLat) and $
                  (siteLat lt boxMaxLat), count)

      numDistCalcGuess += count
      if (count gt maxNumNeighborsGuess) then $
          maxNumNeighborsGuess = count

  endfor

  if verbose then begin
      USR_MSG, 'Calculating all separations between observing stations ' + $
               'up to ' + STRCRA(maxSepMeters / 1000.0) + ' km apart.'
      ;; if (numDistCalcGuess lt numDistances) then $
      ;;     USR_MSG, 'Upper estimate for number of distances < ' + $
      ;;              STRCRA(maxSepMeters) + ' meters: ' + $
      ;;              STRCRA(numDistCalcGuess)
      ;; USR_MSG, 'Estimated maximum number of neighbors: ' + $
      ;;          STRCRA(maxNumNeighborsGuess)
  endif


; Generate 2-D arrays of distance and array index.

  ;; allDist_1d = MAKE_ARRAY(numDistCalcGuess < numNeighbors, $
  ;;                         /DOUBLE, VALUE = ndv)
  ;; allVar_1d = MAKE_ARRAY(numDistCalcGuess < numNeighbors, $
  ;;                        /DOUBLE, VALUE = ndv)
 
  all_dist_2d = MAKE_ARRAY(maxNumNeighborsGuess, $
                           numSnowfall, /DOUBLE, VALUE = ndv)
  all_k_2d = MAKE_ARRAY(maxNumNeighborsGuess, $
                        numSnowfall, /LONG, VALUE = -1L)
  all_n_1d = MAKE_ARRAY(numSnowfall, /LONG, VALUE = 0L)

  if verbose then begin
      if (numSnowfall lt 50) then begin
          hashes = numSnowfall
          lastProgress = -1
      endif else begin
          hashes = 50
          lastProgress = 0
      endelse
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif

  numDistCalc = 0UL

  for j = 1UL, numSnowfall - 1UL do begin

      if verbose then begin
          progress = FIX(FLOAT(j) / FLOAT(numSnowfall - 1) * FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      lonj = siteLon[j]
      latj = siteLat[j]

      for i = 0UL, j - 1UL do begin

          loni = siteLon[i]
          lati = siteLat[i]

          lonDiff = ABS(lonj - loni)
          if (lonDiff gt maxSepDegLon) then CONTINUE
          latDiff = ABS(latj - lati)
          if (latDiff gt maxSepDegLat) then CONTINUE

          d = DISTANCE(dPrecision, loni, lati, lonj, latj)
          if (d ge maxSepMeters) then CONTINUE

          ;; allDist_1d[numDistCalc] = d
          ;; allVar_1d[numDistCalc] = 

          if (d lt 1.0D-10) then begin
              if verbose then PRINT, ''
              ERR_MSG, 'ERROR: Fatal programming error. Still have ' + $
                       'station separations near zero.'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif

          all_dist_2d[all_n_1d[j], j] = d
          all_k_2d[all_n_1d[j], j] = i
          all_n_1d[j] = all_n_1d[j] + 1L

          all_dist_2d[all_n_1d[i], i] = d
          all_k_2d[all_n_1d[i], i] = j
          all_n_1d[i] = all_n_1d[i] + 1UL

          numDistCalc = numDistCalc + 1UL

      endfor

  endfor

  maxNumNeighbors = MAX(all_n_1d)

  if verbose then begin
      PRINT, ''
      USR_MSG, 'Number of distance calculations performed/stored: ' + $
               STRCRA(numDistCalc)
      USR_MSG, 'Maximum number of neighbors: ' + STRCRA(maxNumNeighbors)
  endif


; Truncate distance array columns to the maximum number of neighbors.

  all_dist_2d = all_dist_2d[0UL:maxNumNeighbors - 1UL, *]
  all_k_2d = all_k_2D[0UL:maxNumNeighbors - 1UL, *]

  if NOT(useRatio) then begin


    ;---------------------------------;
    ; 3b-3. Filter correct negatives. ;
    ;---------------------------------;

;     Snowfall observations of zero will only affect the process if
;     they are in the neighborhood of a nonzero value on the input
;     grid. Eliminating zero reports that are not in the neighborhood
;     of nonzero gridded snowfall from the first pass should save
;     significant processing time. However, the distance criterion
;     used is twice the estimated neighborhood radius so that analysis
;     locations midway between zero and nonzero data can be influenced
;     equally by both.

;     "CN" means "correct negative"; i.e., zero guessed, zero
;     observed.

      maxLagForCN = 2.0D * maxLagMeters_P1

      if verbose then $
          USR_MSG, 'Filtering correct negatives using a distance of ' + $
                   STRCRA(maxLagForCN / 1000.0) + ' km.'

      xAnl = minLonAnl + (0.5D + DINDGEN(numColsAnl)) * lonResOut
      yAnl = minLatAnl + (0.5d + DINDGEN(numRowsAnl)) * latResOut

      x2Anl = xAnl # REPLICATE(1.0D, numRowsAnl)
      y2Anl = TRANSPOSE(yAnl # REPLICATE(1.0D, numColsAnl))

      xAnl = !NULL & yAnl = !NULL

      ind = $
          SFAV2_FILTER_CORRECT_NEGATIVES(maxLagForCN, $
                                         mPerDegLonRef, $
                                         mPerDegLatRef, $
                                         x2Anl, $
                                         y2Anl, $
                                         snflBGGridInches, $
                                         siteLon, $
                                         siteLat, $
                                         siteObs, $
                                         assimPointsInd, $
                                         assimPointsCount, $
                                         all_dist_2d, $
                                         all_k_2d, $
                                         all_n_1d, $
                                         DISTANCE_PRECISION = dPrecision, $
                                         HASH = verbose)

      assimPointsCount = N_ELEMENTS(ind) - (ind[0] eq -1)
      if (assimPointsCount eq 0) then begin
          ERR_MSG, 'ERROR: After the first filter for correct negatives, ' + $
                   'no observations remain.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      assimPointsInd = assimPointsInd[ind]
      z_ = z_[ind]
      abcFlag = abcFlag[ind]

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

;+
;     Generate a histogram of the data to assimilate (differences)
;     after the first filtering pass to remove unneeded correct
;     negatives.
;-
      hMin = -5.0
      hMax = 5.0
      binSize = 0.25
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad
      hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize
      zHistOld = zHist
      zHist = HISTOGRAM(z_, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      if verbose then begin

;+
;         Display differences after the first filtering pass.
;-
          X_MAP_GEO_POINTS, siteLon[assimPointsInd], $
                            siteLat[assimPointsInd], $
                            z_, $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_sfDiff, $
                            red_sfDiff, grn_sfDiff, blu_sfDiff, $
                            0, $
                            status, $
                            NDV = ndv, $
                            /SHOW_LOW, $
                            /SHOW_HIGH, $
                            TITLE = zTitle + ': ' + subTitle, $
                            /COLORBAR, $
                            XSIZE_TARGET = xszt, $
                            UNITS = zUnits, $
                            SHAPE_PATH_LIST = shapePathList

;+
;         Display the histogram of data to assimilate (differencess)
;         after the first filtering pass.
;-
          WSET_OR_WINDOW, 1     ;, XSIZE = 800, YSIZE = 400

          PLOT, hAxis, zHist[0:numBins - 1], $
                TITLE = '1c. ' + zTitle, XTITLE = zUnits, $
                YRANGE = [1, MAX([zHist, zHistOld])], /YLOG, $
                /NODATA
          OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                 LINESTYLE = 2, COLOR = 100
          OPLOT, hAxis, zHist[0:numBins - 1], PSYM = 10

      endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

  endif


;----------------------------------------------------------------;
; 3b-4. Generate the empirical semivariogram and fit a spherical ;
;     variogram model to it.                                     ;
;----------------------------------------------------------------;

  debug_tag = 3600

  if useRatio then varName = 'log-ratio' else varName = 'difference'

  if verbose then begin
      USR_MSG, 'Generating empirical semivariogram of ' + varName + $
               's between first guess and observations.'
      ;; USR_MSG, 'Maximum lag: ' + STRCRA(maxLagMeters_P1 / 1000.0) + ' km'
      ;; USR_MSG, 'Lag tolerance: ' + STRCRA(lagTolerance_P1) + ' m'
  endif

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'Unhandled math error/s (' + STRCRA(mathErrors) + ') ' + $
               'before 1st pass variogram :'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  vParamsSpher = !NULL

  if verbose then begin

      WSET_OR_WINDOW, 1

      plotTitle = 'Empirical Variogram of ' + zTitle + ': ' + subTitle

      GEO_SEMIVARIOGRAM, siteLon[assimPointsInd], $
                         siteLat[assimPointsInd], $
                         z_, $
                         maxLagMeters_P1, $
                         status, $
                         DISTANCE_PRECISION = dPrecision, $
                         MIN_LAG_BIN_COUNT = 6, $
                         LAG_TOLERANCE = lagTolMeters, $
;+ GF 20180806
;                         SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = minRangeMeters, $
                         SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = minRangeMeters_P1, $
;- GF 20180806
                         SPHERICAL_SEMIVARIOGRAM_PARAMS = vParamsSpher, $
                         SPHERICAL_SEMIVARIOGRAM_RMSE = RMSESpher, $
                         SPHERICAL_SEMIVARIOGRAM_IN_RANGE_RMSE = $
                         RMSESpherInRange, $
                         LAG_OUT = lagMeters, $
                         SEMIVARIOGRAM_OUT = eVario, $ 
                         PLOT_TITLE = plotTitle, $
                         /SHORT_LAG_WEIGHT_BIAS, $
                         /SCALE_INPUT_VARIABLE, $
                         /HASH, $
                         /SHOW_PLOT, $
                         /VERBOSE

  endif else begin

      GEO_SEMIVARIOGRAM, siteLon[assimPointsInd], $
                         siteLat[assimPointsInd], $
                         z_, $
                         maxLagMeters_P1, $
                         status, $
                         DISTANCE_PRECISION = dPrecision, $
                         MIN_LAG_BIN_COUNT = 6, $
                         LAG_TOLERANCE = lagTolMeters, $
;+ GF 20180806
;                         SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = minRangeMeters, $
                         SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = minRangeMeters_P1, $
;- GF 20180806
                         SPHERICAL_SEMIVARIOGRAM_PARAMS = vParamsSpher, $
                         SPHERICAL_SEMIVARIOGRAM_RMSE = RMSESpher, $
                         SPHERICAL_SEMIVARIOGRAM_IN_RANGE_RMSE = $
                         RMSESpherInRange, $
                         LAG_OUT = lagMeters, $
                         SEMIVARIOGRAM_OUT = eVario, $
                         /SHORT_LAG_WEIGHT_BIAS, $
                         /SCALE_INPUT_VARIABLE

  endelse

  if NOT(status) then begin

      USR_MSG, 'NOTICE: Failed to generate empirical variogram for ' + $
               anlEndDate_YYYYMMDDHH

;+ GF 20180823
;     Make sure we only exit with the first guess if the first guess
;     is actually based on something real.
;- GF 20180823
      if (NOT(ISA(QPEGrid)) and (QPF_source eq 'Missing')) then begin
          ERR_MSG, 'ERROR: Insufficient information exists to produce ' + $
                   'a usable background. No analysis is possible.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif


;     Crop first guess to output grid and exit.

      outputGrid = $
          snflBGGridInches[extraCols:numColsAnl - extraCols - 1, $
                           extraRows:numRowsAnl - extraRows - 1]

      if NOT(dryRun) then begin
          USR_MSG, 'NOTICE: Exiting with first guess result.'
          GOTO, ANALYSIS_COMPLETE
      endif else begin
          GOTO, DRY_RUN_DONE
      endelse

  endif

  ;; mathErrorNames = ['Integer divided by zero', $
  ;;                   'Integer overflow', $
  ;;                   'Floating-point divided by zero', $
  ;;                   'Floating-point underflow', $
  ;;                   'Floating-point overflow', $
  ;;                   'Floating-point operand error']

  ;; mathErrors = CHECK_MATH()
  ;; if (mathErrors ne 0) then begin
  ;;     ERR_MSG, 'WARNING: Math errors in semivariogram process:'
  ;;     for i = 0, 5 do begin
  ;;         if ISHFT(mathErrors, -2 - i) and 1 then $
  ;;             ERR_MSG, '         ' + mathErrorNames[i]
  ;;     endfor
  ;; endif

;+
; Tolerate floating point errors after the first-pass (or one-pass)
; variogram fit with just a warning message.
;-
  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'WARNING: unhandled math error/s (' + $
               STRCRA(mathErrors) + ') ' + $
               'after 1st pass variogram:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
      ;if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if produceImages then begin


;     Produce a graphic of the semivariogram.

      plotTitle = 'Semivariogram: ' + zTitle + '!C' + longSubTitle + '!C '

      plotFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_semivariogram_1st_pass'

      oldDevice = !D.Name
      oldFont = !P.Font
      SET_PLOT, 'PS'
      DEVICE, FILE = outputDir + '/' + plotFile + '.ps', ENCAPSULATE = 0
      !P.Font = 1 ; TrueType
      DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT
      PLOT, lagMeters / 1000.0, eVario, $
            XTITLE = 'Separation (km)', $
            YTITLE = 'Semivariance', $
            PSYM = 10, $
            TITLE = plotTitle, $
            POS = [0.1, 0.1, 0.95, 0.85]
      OPLOT, lagMeters / 1000.0, $
             SPHERICAL_SEMIVARIOGRAM_FUNC(lagMeters, vParamsSpher), $
             COLOR = 150
      XYOUTS, 0.6, 0.3, $
              'Spherical semivariogram:!C' + $
              '- nugget: ' + FORMAT_FLOAT(vParamsSpher[0]) + '!C' + $
              '- sill: ' + FORMAT_FLOAT(vParamsSpher[1]) + '!C' + $
              '- range: ' + FORMAT_FLOAT(vParamsSpher[2] / 1000.0) + $
              ' km!C' + $
              '- RMSE of fit: ' + FORMAT_FLOAT(RMSESpher) + '!C' + $
              '- in-range RMSE of fit: ' + FORMAT_FLOAT(RMSESpherInRange), $
              /NORMAL
      DEVICE, /CLOSE
      SET_PLOT, oldDevice
      !P.Font = oldFont
      cmd = utilsDir + '/pstopng ' + outputDir + '/' + plotFile + '.ps'
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'WARNING: Failed to convert ' + $
                   outputDir + '/' + plotFile + '.ps to PNG format'
      endif else begin
          cmd = 'mogrify -trim -border 4% -bordercolor white ' + $
                outputDir + '/' + plotFile + '.png'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: Failed to fine-tune ' + $
                       outputDir + '/' + plotFile + '.png.'
          endif
          FILE_DELETE, outputDir + '/' + plotFile + '.ps'
      endelse

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

  debug_tag = 3700


; Add variogram parameters to diagnostic structure.

  if useRatio then sfav2Diag.pass_1_is_log_ratio = 1
  sfav2Diag.pass_1_semivariogram_num_obs = assimPointsCount
  sfav2Diag.pass_1_semivariogram_fit_RMSE = RMSESpher
  sfav2Diag.pass_1_semivariogram_fit_RMSE_in_range = RMSESpherInRange
  sfav2Diag.pass_1_semivariogram_nugget = vParamsSpher[0]
  sfav2Diag.pass_1_semivariogram_sill = vParamsSpher[1]
  sfav2Diag.pass_1_semivariogram_range_meters = vParamsSpher[2]

  if verbose then begin


;     Print variogram parameters.

      USR_MSG, 'Spherical variogram parameters:'
      USR_MSG, '  # points: ' + STRCRA(assimPointsCount)
      USR_MSG, '  RMSE of fit: ' + STRCRA(RMSESpher)
      USR_MSG, '  In-range RMSE of fit: ' + STRCRA(RMSESpherInRange)
      USR_MSG, '  nugget: ' + STRCRA(vParamsSpher[0])
      USR_MSG, '  sill: ' + STRCRA(vParamsSpher[1])
      USR_MSG, '  range: ' + STRCRA(vParamsSpher[2] / 1000.0) + ' km'

  endif


; Write variogram parameters to a CSV file.

  if (dryRun eq 0) then begin

      if (useRatio) then $
          vParamsFile = 'sfav2_' + domainLabel + $
                        '_' + durationStr + 'h_' + $
                        anlEndDate_YYYYMMDDHH + $
                        '_spherical_semivariogram_params_pass_1.csv' $
      else $
          vParamsFile = 'sfav2_' + domainLabel + $
                        '_' + durationStr + 'h_' + $
                        anlEndDate_YYYYMMDDHH + $
                        '_spherical_semivariogram_params_pass_0.csv'

      CSV_WRITE_SEMIVARIOGRAM_PARAMETERS, $
          outputDir, $
          vParamsFile, $
          anlEndDate_YYYYMMDDHH, $
          sfav2Diag.pass_1_semivariogram_num_obs, $
          sfav2Diag.pass_1_semivariogram_fit_RMSE, $
          sfav2Diag.pass_1_semivariogram_fit_RMSE_in_range, $
          sfav2Diag.pass_1_semivariogram_nugget, $
          sfav2Diag.pass_1_semivariogram_sill, $
          sfav2Diag.pass_1_semivariogram_range_meters, $
          status, $
          VERBOSE = verbose

      if NOT(status) then begin
          ERR_MSG, 'ERROR: Failed to write variogram parameters to CSV file.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

  endif


; Criteria for semivariogram fit:
;
;   1. in-range RMSE must be le 2.0
;   2. sill of fitted function must be at least 10% of mean value of
;      empirical semivariogram.

  if ((sfav2Diag.pass_1_semivariogram_fit_RMSE_in_range gt 2.0) or $
      (sfav2Diag.pass_1_semivariogram_sill lt (0.1 * MEAN(eVario)))) $
      then begin

      USR_MSG, 'NOTICE: Poor semivariogram fit.'

;+ GF 20180823
;     Make sure we only exit with the first guess if the first guess
;     is actually based on something real.
;- GF 20180823
      if (NOT(ISA(QPEGrid)) and (QPF_source eq 'Missing')) then begin
          ERR_MSG, 'ERROR: Insufficient information exists to produce ' + $
                   'a usable background. No analysis is possible.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif


;     Crop first guess to output grid and exit.

      outputGrid = $
          snflBGGridInches[extraCols:numColsAnl - extraCols - 1, $
                           extraRows:numRowsAnl - extraRows - 1]

      if NOT(dryRun) then begin
          USR_MSG, 'NOTICE: Exiting with first guess result.'
          GOTO, ANALYSIS_COMPLETE
      endif else begin
          GOTO, DRY_RUN_DONE
      endelse

  endif


DRY_RUN_DONE:

  debug_tag = 3800

  if (dryRun) then begin
      if (LMGR(/RUNTIME) and verbose) then begin
          PRINT, 'Dry run finished. Press a key to exit.'
          move = GET_KBRD(1)
          sfaStatus = 1
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif else begin
          USR_MSG, 'NOTICE: Dry run complete. Use ".continue" to keep going.'
          STOP
      endelse
  endif

  if NOT(useRatio) then begin

      if ((2.0D * vParamsSpher[2]) lt maxLagForCN) then begin


        ;--------------------------------------------------------------;
        ; 3b-5. Re-filter correct negatives using 2 x variogram range. ;
        ;--------------------------------------------------------------;

;         Filter data again, the same as we did earlier, but using 2x
;         the range of the fitted semivariogram instead of 2x
;         maxLagMeters_P1.

          maxLagForCN = 2.0D * vParamsSpher[2]

          if verbose then $
              USR_MSG, 'Re-filtering correct negatives using 2 * ' + $
                       'variogram range = ' + $
                       STRCRA(maxLagForCN / 1000.0) + ' km.'

          if NOT(ISA(x2Anl)) then begin
              ERR_MSG, 'ERROR: Fatal programming error. 2-D longitude ' + $
                       'grid has not been generated.'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif
          if NOT(ISA(y2Anl)) then begin
              ERR_MSG, 'ERROR: Fatal programming error. 2-D latitude ' + $
                       'grid has not been generated.'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif

          ind = $
              SFAV2_FILTER_CORRECT_NEGATIVES(maxLagForCN, $
                                             mPerDegLonRef, $
                                             mPerDegLatRef, $
                                             x2Anl, $
                                             y2Anl, $
                                             snflBGGridInches, $
                                             siteLon, $
                                             siteLat, $
                                             siteObs, $
                                             assimPointsInd, $
                                             assimPointsCount, $
                                             all_dist_2d, $
                                             all_k_2d, $
                                             all_n_1d, $
                                             DISTANCE_PRECISION = dPrecision, $
                                             HASH = verbose)

          assimPointsCount = N_ELEMENTS(ind) - (ind[0] eq -1)
          if (assimPointsCount eq 0) then begin
              ERR_MSG, 'ERROR: After the second filter for correct ' + $
                       'negatives, no observations remain.'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif
          assimPointsInd = assimPointsInd[ind]
          z_ = z_[ind]
          abcFlag = abcFlag[ind]

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

;+
;         Generate a histogram of the data to assimilate (differences)
;         after the second filtering pass to remove unneeded correct
;         negatives.
;-
          hMin = -5.0
          hMax = 5.0
          binSize = 0.25
          numBins = ROUND((hMax - hMin) / binSize)
          pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
          hMin = hMin - pad
          hMax = hMax + pad
          hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize
          zHistOld = zHist
          zHist = HISTOGRAM(z_, MIN = hMin, MAX = hMax, BINSIZE = binSize)

          if verbose then begin

;+
;             Display differences after the second filtering pass.
;-
              X_MAP_GEO_POINTS, siteLon[assimPointsInd], $
                                siteLat[assimPointsInd], $
                                z_, $
                                minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                                edges_sfDiff, $
                                red_sfDiff, grn_sfDiff, blu_sfDiff, $
                                0, $
                                status, $
                                NDV = ndv, $
                                /SHOW_LOW, $
                                /SHOW_HIGH, $
                                TITLE = zTitle + ': ' + subTitle, $
                                /COLORBAR, $
                                XSIZE_TARGET = xszt, $
                                UNITS = zUnits, $
                                SHAPE_PATH_LIST = shapePathList

;+
;             Display the histogram of data to assimilate
;             (differences) after the second filtering pass.
;-
              WSET_OR_WINDOW, 1 ;, XSIZE = 800, YSIZE = 400

              PLOT, hAxis, zHist[0:numBins - 1], $
                    TITLE = '1d. ' + zTitle, XTITLE = zUnits, $
                    YRANGE = [1, MAX([zHist, zHistOld])], /YLOG, $
                    /NODATA
              OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                     LINESTYLE = 2, COLOR = 100
              OPLOT, hAxis, zHist[0:numBins - 1], PSYM = 10

          endif ; verbose

      endif ; 2nd filtering pass


;     Identify remaining correct negatives.

      ind = WHERE((siteGuess[assimPointsInd] eq 0.0) and $
                  (siteObs[assimPointsInd] eq 0.0), $
                  count)

      nonCNCount = assimPointsCount - count
      if verbose then $
          USR_MSG, 'Assimilating ' + STRCRA(assimPointsCount) + $
                   ' differences, with ' + STRCRA(count) + $
                   ' correct negatives and ' + STRCRA(nonCNCount) + $
                   ' other values.'

  endif ; NOT(useRatio)


ASSIMILATE:


;=============================;
; 4. Assimilate observations. ;
;=============================;

  debug_tag = 3900

  if verbose then USR_MSG, '--- 4. ASSIMILATING OBSERVATIONS ---'

  zGrid = !NULL

; CONUS is about 8.0e6 km^2
; Kriging neighborhood is about !Pi * range^2


; Calculate number of radii for KRM.

  numKRMRadii = $
      CEIL(vParamsSpher[2] / targetKRMRingWidthMeters) > 10

;+ GF 20180806
  if (minLagMeters gt vParamsSpher[2]) then begin
;- GF 20180806


;     Minimum lag exceeds variogram range, which is not acceptable.

      USR_MSG, 'NOTICE: minimum lag ' + STRCRA(minLagMeters) + $
               ' meters cannot exceed variogram range. Adjusting to ' + $
               STRCRA(vParamsSpher[2])
      minLagMeters = vParamsSpher[2]

  endif


;--------------------------------------------------------------------;
; 4a. Simulate the kriging process to guess at the number of solvers ;
;     and the average number of points per solver.                   ;
;--------------------------------------------------------------------;

  if verbose then $
      USR_MSG, 'Estimating solver size and points per solver.'

  xAnlKrige = minLonAnl + $
              (0.5D + DINDGEN(numColsAnlKrige)) * (lonResOut * krigeResFactor)
  yAnlKrige = minLatAnl + $
              (0.5D + DINDGEN(numRowsAnlKrige)) * (latResOut * krigeResFactor)

  rangeDegLat = vParamsSpher[2] / mPerDegLatRef
  rangeDegLon = vParamsSpher[2] / mPerDegLonRef

  lonAssim = siteLon[assimPointsInd]
  latAssim = siteLat[assimPointsInd]

  numSolvers = 0L
  totalSolverPoints = 0L

  if verbose then begin
      if (numRowsAnlKrige lt 50) then begin
          hashes = numRowsAnlKrige
          lastProgress = -1
      endif else begin
          hashes = 50
          lastProgress = 0
      endelse
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif

  for rc = 0L, numRowsAnlKrige - 1L do begin

      if verbose then begin
          progress = FIX(FLOAT(rc) / FLOAT(numRowsAnlKrige - 1) * $
                         FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      gridLat = yAnlKrige[rc]

      for cc = 0L, numColsAnlKrige - 1L do begin

          gridLon = xAnlKrige[cc]

          inBoxInd = WHERE((ABS(lonAssim - gridLon) lt rangeDegLon) and $
                           (ABS(latAssim - gridLat) lt rangeDegLat), $
                           inBoxCount)

          if (inBoxCount lt minHoodPoints) then CONTINUE

          lonAssimInBox = lonAssim[inBoxInd]
          latAssimInBox = latAssim[inBoxInd]

          dInBox = DISTANCE(dPrecision, $
                            gridLon, gridLat, lonAssimInBox, latAssimInBox)

          boxInHoodInd = WHERE(dInBox lt vParamsSpher[2], inHoodCount)
          if (inHoodCount lt minHoodPoints) then CONTINUE

          hoodInd = inBoxInd[boxInHoodInd]
          hoodDist = dInBox[boxInHoodInd]
          numClose = TOTAL(hoodDist lt minLagMeters, /INT)
          if (numClose lt 3) then CONTINUE

          hoodZ = z_[hoodInd]
          minHoodZ = MIN(hoodZ)
          maxHoodZ = MAX(hoodZ)

          if ((maxHoodZ - minHoodZ) le hoodTolerance) then CONTINUE

          numSolvers++
          totalSolverPoints += inHoodCount

      endfor

  endfor

  if verbose then PRINT, ''

  lonAssim = !NULL
  latAssim = !NULL

  if (numSolvers eq 0) then begin
;      ERR_MSG, 'ERROR: Fatal programming error. Estimated # of solvers ' + $
;               'cannot be zero.'
;      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      ERR_MSG, 'WARNING: Estimated number of solvers is zero.'
      meanPointsPerSolver = meanPointsPerSolverLimit - 1
  endif else begin
      meanPointsPerSolver = ROUND(FLOAT(totalSolverPoints) / FLOAT(numSolvers))
      if verbose then begin
          USR_MSG, 'Est. number of solvers: ' + STRCRA(numSolvers)
          USR_MSG, 'Est. mean points per solver: ' + $
                   STRCRA(meanPointsPerSolver)
      endif
  endelse


;------------------------------------------------------------;
; 4b. Get nearest neighbor distances to compare with kriging ;
;     resolution.                                            ;
;------------------------------------------------------------;

  debug_tag = 4000

  if verbose then $
      USR_MSG, 'Calculating nearest-neighbor distances for kriging points.'

  FIND_NEAREST_NEIGHBOR, siteLon[assimPointsInd], $
                         siteLat[assimPointsInd], $
                         nnInd, $
                         nnDist, $
                         DISTANCE_PRECISION = dPrecision, $
                         HASH = verbose

  order = SORT(nnDist)

  if verbose then begin
      USR_MSG, '  median distance: ' + $
               STRCRA(nnDist[order[assimPointsCount / 2]]) + ' meters'
      USR_MSG, '  mean distance: ' + STRCRA(MEAN(nnDist)) + ' meters'
      USR_MSG, '  est. longitudinal kriging resolution: ' + $
               STRCRA(DISTANCE(dPrecision, $
                               0.0D, $
                               0.5D * (maxLatOut + minLatOut), $
                               lonResOutKrige, $
                               0.5D * (maxLatOut + minLatOut))) + $
               ' meters'
      USR_MSG, '  est. latitudinal kriging resolution: ' + $
               STRCRA(DISTANCE(dPrecision, $
                               0.0D, $
                               0.5D * (maxLatOut + minLatOut) - $
                               0.5D * latResOutKrige, $
                               0.0D, $
                               0.5D * (maxLatOut + minLatOut) + $
                               0.5D * latResOutKrige)) + $
               ' meters'
  endif


; Break assimilation points out into new arrays.

  assimID = siteID[assimPointsInd]
  assimLon = siteLon[assimPointsInd]
  assimLat = siteLat[assimPointsInd]
  assimElev = siteElev[assimPointsInd]
  assimObs = siteObs[assimPointsInd]
  zAssim = z_
  assimKeepFlag = siteKeepFlag[assimPointsInd]

  if NOT(useRatio) then begin


;     Write differences being used in kriging pass to csv file.

      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_obs_diff_krige_in.csv'

      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, 'longitude,latitude,snowfall_error_inches'
      for ac = 0L, assimPointsCount - 1L do begin
          PRINTF, lun, $
                  STRCRA(STRING(assimLon[ac], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(STRING(assimLat[ac], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(zAssim[ac])
      endfor
      FREE_LUN, lun


    ;---------------------------------------------------------------;
    ; 4c. Perform "oddball" test, removing isolated nonzero reports ;
    ;     and suspicious zero reports.                              ;
    ;---------------------------------------------------------------;

      oddballThreshold = 1.0 ; check isolated reports above this amount.

      oddballFlag = $
          SFAV2_FLAG_ODDBALLS(assimID, $
                              assimLon, $
                              assimLat, $
                              assimElev, $
                              assimObs, $
                              ndv, $
                              vParamsSpher[2], $
                              minHoodPoints, $
                              snflThreshold, $
                              oddballThreshold, $
                              mPerDegLonRef, $
                              mPerDegLatRef, $
                              DISTANCE_PRECISION = dPrecision, $
                              KEEP_FLAG = assimKeepFlag, $
                              VERBOSE = verbose, $
                              CHECK_ID = checkID)

      useFlag_P1 = 1B - oddballFlag
      oddballFlag = !NULL


    ; 4c. Perform Grubbs outlier test, excluding correct negatives.

      abcInd = WHERE(abcFlag, count) ; relative to assimPointsInd
      if (count eq 0) then begin
          ERR_MSG, 'ERROR: No events remaining. ' + $
                   'We should never have gotten this far!'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      floorOutlierSD_P1 = 0.6

      outlierflag = SFAV2_FLAG_OUTLIERS(assimID[abcInd], $
                                        assimLon[abcInd], $
                                        assimLat[abcInd], $
                                        assimElev[abcInd], $
                                        assimObs[abcInd], $
                                        zAssim[abcInd], $
                                        ndv, $
                                        vParamsSpher[2], $
                                        minHoodPoints, $
                                        minOutlierDiff_P1, $
                                        floorOutlierSD_P1, $
                                        mPerDegLonRef, $
                                        mPerDegLatRef, $
                                        DISTANCE_PRECISION = dPrecision, $
                                        KEEP_FLAG = assimKeepFlag[abcInd], $
                                        VERBOSE = verbose, $
                                        CHECK_ID = checkID)

      allOutlierFlag = BYTARR(assimPointsCount)
      allOutlierFlag[abcInd] = outlierFlag
      outlierFlag = TEMPORARY(allOutlierFlag) ; opposite of useFlag
      useFlag_P1 = useFlag_P1 * (1B - outlierFlag)
      outlierFlag = !NULL

;+
;     Generate artificial zero reports in locations remote from real
;     observations where minimum daily temperatures are too low for
;     snow accumulation to be likely.
;-
      minTempCutoffFakeZero = 2.0
      aveTempCutoffFakeZero = aveTempCutoff + 2.0
      artificialZeroProb = 0.10
      minDistToObs = 0.5D * minLagMeters    ; typically 50 km
      minDistToObs = 10.0D3                 ; tighten things up
      maxDistToObs = 2.0D * vParamsSpher[2] ; typically at least 200 km

      if NOT(ISA(x2Anl)) then begin
          ERR_MSG, 'ERROR: Fatal programming error. 2-D longitude ' + $
                   'grid is missing.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      if NOT(ISA(y2Anl)) then begin
          ERR_MSG, 'ERROR: Fatal programming error. 2-D latitude ' + $
                   'grid is missing.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_obs_diff_krige_in_fake_zero.csv'

; ----- TESTING -----
      if verbose then begin

          fAssim = siteObs[assimPointsInd]

;+
;         Display observations.
;-
          X_MAP_GEO_POINTS, siteLon[assimPointsInd], $
                            siteLat[assimPointsInd], $
                            siteObs[assimPointsInd], $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_snowfall, $
                            red_snowfall, grn_snowfall, blu_snowfall, $
                            5, $
                            status, $
                            ndv = NDV, $
                            /SHOW_HIGH, $
                            TITLE = 'Observed ' + durationStr + '-hour ' + $
                            'Snowfall, ' + anlEndDate_YYYYMMDDHH + $
                            ', w/Artificial Zeroes', $
                            /COLORBAR, $
                            XSIZE_TARGET = xszt * 2, $
                            UNITS = 'inches', $
                            SHAPE_PATH_LIST = shapePathList, $
                            MAP_STRUCT = lonLat, $
                            /OUTLINES, $
                            SYMBOL_SIZE = 1.5

      endif

      SFAV2_CREATE_ARTIFICIAL_ZEROES, minTempGrid,$ 
                                      aveTempGrid, $
                                      snflBGGridInches, $
                                      x2Anl, $
                                      y2Anl, $
                                      assimLon, $
                                      assimLat, $
                                      minTempCutoffFakeZero, $
                                      aveTempCutoffFakeZero, $
                                      artificialZeroProb, $
                                      minDistToObs, $
                                      maxDistToObs, $
                                      mPerDegLonRef, $
                                      mPerDegLatRef, $
                                      ndv, $
                                      warmInd, $
                                      warmLon, $
                                      warmLat, $
                                      warmFakeZ, $
                                      warmCount, $
                                      DISTANCE_PRECISION = dPrecision, $
                                      VERBOSE = verbose, $
                                      CSV_FILE = outputDir + '/' + csvFile, $
                                      MAP_STRUCT = lonLat

  endif else warmCount = 0

  if (warmCount gt 0) then begin

      assimID = [assimID, REPLICATE('ARTIFICIAL', warmCount)]
      assimLon = [assimLon, warmLon]
      assimLat = [assimLat, warmLat]
      assimElev = [assimElev, REPLICATE(-9999, warmCount)]
      zAssim = [zAssim, warmFakeZ]
      assimObs = [assimObs, REPLICATE(ndv, warmCount)]
      assimKeepFlag = [assimKeepFlag, REPLICATE(0B, warmCount)]
      useFlag_P1 = [useFlag_P1, REPLICATE(1B, warmCount)]

  endif


;---------------------------;
; 4c. Perform kriging pass. ;
;---------------------------;

  debug_tag = 4100

  if ((meanPointsPerSolver lt meanPointsPerSolverLimit) or $
      (numSolvers lt numSolversLimit)) then begin


;     Use textbook ordinary kriging.

      sfav2Diag.pass_1_assim_uses_radial_means = 0

      if useRatio then begin

          GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
              assimLon, $
              assimLat, $
              zAssim, $
              ndv, $
              vParamsSpher, $
              minLonAnl, maxLonAnl, $
              minLatAnl, maxLatAnl, $
              lonResOutKrige, latResOutKrige, $
              zGrid, $
              DISTANCE_PRECISION = dPrecision, $
              CROSS_VAL_POINTS = cvz, $
              CROSS_VAL_RMSE = cvRMSE, $
              MIN_LAG_METERS = minLagMeters, $
              MIN_OUTLIER_DIFF = minOutlierDiff_P1, $
              MIN_NEIGHBORHOOD_POINTS = minHoodPoints, $
              NEIGHBORHOOD_TOLERANCE = hoodTolerance, $
              VERBOSE = verbose, $
          ;; ALL_DISTANCES_2D = a_all_dist_2d, $
          ;; ALL_INDICES_2D = a_all_k_2d, $
          ;; ALL_COUNT_1D = a_all_n_1d, $
              KEEP_FLAG = assimKeepFlag, $
              USE_FLAG = useFlag_P1, $
              WALL_TIME = wallTime_P1, $
              MEAN_POINTS_PER_SOLVER = meanPointsPerSolver, $
              NUM_SOLVERS = numSolvers

      endif else begin

          GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
              assimLon, $
              assimLat, $
              zAssim, $
              ndv, $
              vParamsSpher, $
              minLonAnl, maxLonAnl, $
              minLatAnl, maxLatAnl, $
              lonResOutKrige, latResOutKrige, $
              zGrid, $
              ERROR_VARIANCE = zGridVariance, $
              DISTANCE_PRECISION = dPrecision, $
              CROSS_VAL_POINTS = cvz, $
              CROSS_VAL_RMSE = cvRMSE, $
              MIN_LAG_METERS = minLagMeters, $
              MIN_OUTLIER_DIFF = minOutlierDiff_P1, $
              MIN_NEIGHBORHOOD_POINTS = minHoodPoints, $
              NEIGHBORHOOD_TOLERANCE = hoodTolerance, $
              VERBOSE = verbose, $
          ;; ALL_DISTANCES_2D = a_all_dist_2d, $
          ;; ALL_INDICES_2D = a_all_k_2d, $
          ;; ALL_COUNT_1D = a_all_n_1d, $
              ;; KEEP_FLAG = assimKeepFlag, $
              USE_FLAG = useFlag_P1, $
              WALL_TIME = wallTime_P1, $
              MEAN_POINTS_PER_SOLVER = meanPointsPerSolver, $
              NUM_SOLVERS = numSolvers, $
              /SKIP_GRUBBS

      endelse

  endif else begin


;     Use ordinary kriging with radial means (KRM).

      sfav2Diag.pass_1_assim_uses_radial_means = 1

      if useRatio then begin

          GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
              assimLon, $
              assimLat, $
              zAssim, $
              ndv, $
              vParamsSpher, $
              minLonAnl, maxLonAnl, $
              minLatAnl, maxLatAnl, $
              lonResOutKrige, latResOutKrige, $
              zGrid, $
              DISTANCE_PRECISION = dPrecision, $
              /USE_KRM, $
              STAGGER_KRM_MESH = staggerKRMMesh, $
              NUM_KRM_RADII = numKRMRadii, $
              NUM_KRM_ANGLES = numKRMAngles_P1, $
              CROSS_VAL_POINTS = cvz, $
              CROSS_VAL_RMSE = cvRMSE, $
              MIN_LAG_METERS = minLagMeters, $
              MIN_OUTLIER_DIFF = minOutlierDiff_P1, $
              MIN_NEIGHBORHOOD_POINTS = minHoodPoints, $
              NEIGHBORHOOD_TOLERANCE = hoodTolerance, $
              VERBOSE = verbose, $
              KEEP_FLAG = assimKeepFlag, $
              USE_FLAG = useFlag_P1, $
              WALL_TIME = wallTime_P1, $
              MEAN_POINTS_PER_SOLVER = meanPointsPerSolver, $
              NUM_SOLVERS = numSolvers

      endif else begin

          GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
              assimLon, $
              assimLat, $
              zAssim, $
              ndv, $
              vParamsSpher, $
              minLonAnl, maxLonAnl, $
              minLatAnl, maxLatAnl, $
              lonResOutKrige, latResOutKrige, $
              zGrid, $
              ERROR_VARIANCE = zGridVariance, $
              DISTANCE_PRECISION = dPrecision, $
              /USE_KRM, $
              STAGGER_KRM_MESH = staggerKRMMesh, $
              NUM_KRM_RADII = numKRMRadii, $
              NUM_KRM_ANGLES = numKRMAngles_P1, $
              CROSS_VAL_POINTS = cvz, $
              CROSS_VAL_RMSE = cvRMSE, $
              MIN_LAG_METERS = minLagMeters, $
              MIN_OUTLIER_DIFF = minOutlierDiff_P1, $
              MIN_NEIGHBORHOOD_POINTS = minHoodPoints, $
              NEIGHBORHOOD_TOLERANCE = hoodTolerance, $
              VERBOSE = verbose, $
              ;; KEEP_FLAG = assimKeepFlag, $
              USE_FLAG = useFlag_P1, $
              WALL_TIME = wallTime_P1, $
              MEAN_POINTS_PER_SOLVER = meanPointsPerSolver, $
              NUM_SOLVERS = numSolvers, $
              /SKIP_GRUBBS

      endelse

  endelse

  debug_tag = 4200

  if NOT(ISA(zGrid)) then STOP ; GREG 20180925

  if NOT(useRatio) then begin

    ;-----------------------------------------------------------------;
    ; Scale the kriging result. This is an EXPERIMENTAL step meant to ;
    ; force the kriging result to be more strongly correlated with    ;
    ; its inputs. Currently the process estimates bias but does not   ;
    ; apply the bias estimate to zGrid.                               ;
    ;-----------------------------------------------------------------;

;+
;     Sample the kriging result and its error variances.
;-
      r2 = zAssim[0:assimPointsCount - 1]
      i_nn = ROUND((siteLon[assimPointsInd] - minLonAnl) / $
                   lonResOutKrige - 0.5D)
      j_nn = ROUND((siteLat[assimPointsInd] - minLatAnl) / $
                   latResOutKrige - 0.5D)
      r2_hat = zGrid[i_nn, j_nn]
      if ISA(zGridVariance) then r2_hat_var = zGridVariance[i_nn, j_nn]
      i_nn = !NULL
      j_nn = !NULL

      plotFile = outputDir + '/' + $
                 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_kriging_bias'

      title = 'Snowfall Kriging Bias, Single Pass, ' + subTitle
      krigingBias = SFAV2_KRIGING_OUTPUT_BIAS(r2, $
                                              r2_hat, $
                                              r2_hat_var, $
                                              ndv, $
                                              OUTPUT_PNG_PATH = plotFile, $
                                              TITLE = title, $
                                              UTILITIES_DIR = utilsDir, $
                                              VERBOSE = verbose)

      ;; if NOT(ISA(krigingBias)) then krigingBias = 1.0
      ;; ind = WHERE(zGrid ne ndv)
      ;; zGrid[ind] = zGridind] / krigingBias

  endif

;+
; Truncate useFlag_P1 to remove artificial zeroes.
;-
  if (N_ELEMENTS(useFlag_P1) ne (assimPointsCount + warmCount)) then STOP ; GREG 20180925
  if (warmCount gt 0) then useFlag_P1 = useFlag_P1[0:assimPointsCount - 1]


;--------------------------------------------------------------------;
; 4d. Smooth no-data areas in the kriging result with a neighborhood ;
;     boxcar average, effectively treating no-data values in the     ;
;     boxcar subgrid as zeroes.                                      ;
;--------------------------------------------------------------------;

  debug_tag = 4300

  if verbose then $
      USR_MSG, 'Smoothing no-data areas of kriging solution.'

  newZGrid = SFAV2_SMOOTH_NDV_AS_ZERO(zGrid, $
                                      ndv, $
                                      minLatAnl, $
                                      maxLatAnl, $
                                      lonResOutKrige, $
                                      latResOutKrige, $
                                      smoothHoodRadM, $
                                      DISTANCE_PRECISION = dPrecision, $
                                      HASH = verbose)

  if NOT(ISA(newZGrid)) then begin
      ERR_MSG, 'ERROR: Smoother failed for kriging result.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  zGrid = newZGrid
  newZGrid = !NULL


; Add kriging results to diagnostic structure.

  sfav2Diag.pass_1_assim_num_obs = assimPointsCount
  sfav2Diag.pass_1_mean_points_per_solver = meanPointsPerSolver
  sfav2Diag.pass_1_num_solvers = numSolvers
  sfav2Diag.pass_1_assim_wall_time = wallTime_P1
  sfav2Diag.pass_1_assim_cross_validation_RMSE = cvRMSE

  if verbose then $
      USR_MSG, 'Kriging cross-validation RMSE: ' + STRCRA(cvRMSE)
 
  if (krigeResFactor gt 1) then begin


;     Resample kriging output to full resolution.

      arrSize = SIZE(zGrid)
      if (arrSize[0] ne 2) then STOP ; GREG 20180925
      if (arrSize[1] ne (numColsAnlKrige)) then STOP ; GREG 20180925
      if (arrSize[2] ne (numRowsAnlKrige)) then STOP ; GREG 20180925
      flag = FLOAT(zGrid ne ndv)
      ;; flag = REBIN(flag, numColsAnl, numRowsAnl)
      ;; zGrid = REBIN(zGrid, numColsAnl, numRowsAnl)  
      flag = CONGRID(flag, numColsAnl, numRowsAnl, /INTERP)
      zGrid = CONGRID(zGrid, numColsAnl, numRowsAnl, /INTERP)
      ind = WHERE(flag lt 1.0, count)
      if (count gt 0) then zGrid[ind] = ndv

  endif

;;  stop ; debug

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if verbose then begin


;     Display kriging and assimilation results.

      if (useRatio) then $
          edges = edges_sfRatio $
      else $
          edges = edges_sfDiff

      title = 'Gridded ' + zTitle + '!C' + subTitle

      X_MAP_GEO_GRID, zGrid, $
                      minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                      edges, $
                      red_sfDiff, grn_sfDiff, blu_sfDiff, $
                      0, $
                      status, $
                      NDV = ndv, $
                      /SHOW_LOW, $
                      /SHOW_HIGH, $
                      TITLE = title, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = zUnits, $
                      SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write kriging result to an image.

      if (useRatio) then $
          edges = edges_sfRatio $
      else $
          edges = edges_sfDiff

      title = 'Gridded ' + zTitle + '!C' + longSubTitle

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_09_obs_interp_1st_pass.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          zGrid[extraCols:numColsAnl - extraCols - 1, $
                extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges, red_sfDiff, grn_sfDiff, blu_sfDiff, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          title, $
          zUnits, $
          outputDir + '/' + PNGFile, $
          /SHOW_LOW, $
          /SHOW_HIGH, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

  endif

  if produceTIFFs then begin


;     Write kriging result to GeoTIFF.

;      if useRatio then outName = '1_log_ratio' else outName = 'difference'
      if useRatio then $
          outName = '1st_pass_log_ratio' $
      else $
          outName = 'one_pass_difference'
      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_' + outname + '.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(zGrid, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if (updateGISProject) then begin
          ;; cmd = 'ln -fs ' + $
          ;;       '../' + outputDir + '/' + TIFFFile + ' ' + $
          ;;       GISProjectDir + '/' + $
          ;;       'sfav2_' + domainLabel + '_' + outName + '.tif'
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_' + outName + '.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


; Adjust background with assimilation field.

  debug_tag = 4400

  ind = WHERE((snflBGGridInches ne ndv) and (zGrid ne ndv), count)
  if (count eq 0) then STOP ; GREG 20180925

  snflGridAssimInches = snflBGGridInches

  if useRatio then $
      snflGridAssimInches[ind] = snflGridAssimInches[ind] * $
                                 10.0^(-zGrid[ind]) $
  else $
      snflGridAssimInches[ind] = snflGridAssimInches[ind] - zGrid[ind]


; Impose a floor on the results equal to 0.01 inches, which is how
; this program quantifies a trace of snowfall.

  ind = WHERE((snflGridAssimInches ne ndv) and $
              (snflGridAssimInches lt 0.01), count)
  if (count gt 0) then begin
      snflGridAssimInches[ind] = 0.0
      if verbose then $
          USR_MSG, 'Adjusted ' + STRCRA(count) + ' cells with a ' + $
                   '"trace" floor of 0.01".'
  endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if verbose then begin


;     Re-display snowfall background where QPF WEASD used to be.

      title = 'Snowfall Analysis v2 First Guess: ' + subTitle

      X_MAP_GEO_GRID, snflBGGridInches, $
                      minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                      edges_snowfall, $
                      red_snowfall, grn_snowfall, blu_snowfall, $
                      3, $
                      status, $
                      NDV = ndv, $
                      /SHOW_HIGH, $
                      TITLE = title, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'inches', $
                      SHAPE_PATH_LIST = shapePathList


;     Display first-pass assimilated snowfall.

      title = 'Snowfall Analysis v2 Assim. Pass 1: ' + subTitle

      X_MAP_GEO_GRID, snflGridAssimInches, $
                      minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                      edges_snowfall, $
                      red_snowfall, grn_snowfall, blu_snowfall, $
                      2, $
                      status, $
                      NDV = ndv, $
                      /SHOW_HIGH, $
                      TITLE = title, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'inches', $
                      SHAPE_PATH_LIST = shapePathList

  endif

  if produceImages then begin


;     Write first pass snowfall to an image.

      title = 'Snowfall Analysis v2 First Pass:!C' + longSubTitle
;              STRCRA(duration) + '-hour accum. ending ' + $
;              anlEndDate_YYYYMMDDHH + ' UTC'

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_10_snowfall_1st_pass.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          snflGridAssimInches[extraCols:numColsAnl - extraCols - 1, $
                              extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          edges_snowfall, red_snowfall, grn_snowfall, blu_snowfall, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          title, $
          'inches', $
          outputDir + '/' + PNGFile, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_snowfall, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList

  endif

  if produceTIFFs then begin


;     Write assimilated result to GeoTIFF.

      if useRatio then $
          outName = '1st_pass_result' $
      else $
          outName = 'one_pass_result'
      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_' + outName + '.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(snflGridAssimInches, 7), $
                              minLonAnl, $
                              maxLatAnl, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if (updateGISProject) then begin
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_' + outName + '.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


; Update snflPtsDiag with flags for points included in first
; pass. The first flag indicates whether the point was sent in, the
; second flag indicates whether it passed non-ndv, domain membership,
; and outlier tests within the kriging procedure.

  debug_tag = 4500

  apc = 0L
  for sc = 0, numSnowfall - 1 do begin
      if (apc lt assimPointsCount) then begin
          if (assimPointsInd[apc] eq sc) then begin ; point was sent
              snflPtsDiag[sc].snowfallObsAssim1 = 1
              if useFlag_P1[apc] then $
                  snflPtsDiag[sc].snowfallObsPassedAssim1QC = 1 $
              else $
                  snflPtsDiag[sc].snowfallObsPassedAssim1QC = 0
              apc++
          endif else begin
              snflPtsDiag[sc].snowfallObsAssim1 = 0
              snflPtsDiag[sc].snowfallObsPassedAssim1QC = 0
          endelse
      endif else begin
          if (sc le assimPointsInd[apc - 1]) then STOP ; programming error ; GREG 20180925
          snflPtsDiag[sc].snowfallObsAssim1 = 0
          snflPtsDiag[sc].snowfallObsPassedAssim1QC = 0
      endelse
  endfor

;+
;     Impose temperature criteria on low snowfall amounts for one-pass
;     assimilation.
;-
  if NOT(useRatio) then begin

      warmSnowMaximum = 1.0

      SFAV2_REMOVE_WARM_SNOWFALL, $
          snflGridAssimInches, $
          minTempGrid, $
          aveTempGrid, $
          x2Anl, $
          y2Anl, $
          ndv, $
          siteObs, $
          siteLon, $
          siteLat, $
          warmSnowMaximum, $
          minTempCutoff + tempSlack, $
          aveTempCutoff + tempSlack, $
          minDistToObs, $
;          maxDistToObs, $
          mPerDegLonRef, $
          mPerDegLatRef, $
          DISTANCE_PRECISION = dPrecision, $
          VERBOSE = verbose

  endif


; Sample the assimilated snowfall at observation points, using bilinear
; interpolation.

  h1 = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv)

  i = (siteLon - minLonAnl) / lonResOut - 0.5D
  j = (siteLat - minLatAnl) / latResOut - 0.5D

  for k = 0, numSnowfall - 1 do begin

      ik = i[k]
      jk = j[k]
      fk = siteObs[k]

      i1 = FLOOR(ik)
      i2 = i1 + 1L
      j1 = FLOOR(jk)
      j2 = j1 + 1L

      if ((i1 lt 0L) or $
          (i2 ge numColsAnl) or $
          (j1 lt 0L) or $
          (j2 ge numRowsAnl)) then STOP ; should have already avoided ; GREG 20180925

      hll = snflGridAssimInches[i1, j1]
      hlr = snflGridAssimInches[i2, j1]
      hur = snflGridAssimInches[i2, j2]
      hul = snflGridAssimInches[i1, j2]

      if ((hll eq ndv) or $
          (hlr eq ndv) or $
          (hur eq ndv) or $
          (hul eq ndv)) then $
              h1[k] = ndv $
      else $
          h1[k] = hll * (i2 - ik) * (j2 - jk) + $
                  hlr * (ik - i1) * (j2 - jk) + $
                  hur * (ik - i1) * (jk - j1) + $
                  hul * (i2 - ik) * (jk - j1)

  endfor

  ind = WHERE((siteObs ne ndv) and $
              (h1 ne ndv), count)
  if (count eq 0) then begin
      if verbose then $
          ERR_MSG, 'ERROR: After eliminating assimilated no-data values, ' + $
                   'no data remain for analysis.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if (count ne numSnowfall) then begin
      ERR_MSG, 'ERROR: First pass asssimilation introduced no-data values ' + $
               'to the analysis grid.'
      STOP ; GREG 20180925
;      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Update snflPtsDiag with the first pass value, a corresponding
; above-threshold flag, and the error category for the first pass value.

  for sc = 0, numSnowfall - 1 do begin

      snflPtsDiag[sc].snowfall1stPass = h1[sc]
      snflPtsDiag[sc].snowfall1stPassAboveTrace = $
          FIX(h1[sc] gt snflThreshold)

      case (snflPtsDiag[sc].snowfallObsAboveTrace + $
            snflPtsDiag[sc].snowfall1stPassAboveTrace * 2) of
          0: snflPtsDiag[sc].snowfall1stPassCategory = 'D' ; correct negative
          1: snflPtsDiag[sc].snowfall1stPassCategory = 'C' ; miss
          2: snflPtsDiag[sc].snowfall1stPassCategory = 'B' ; false positive
          3: snflPtsDiag[sc].snowfall1stPassCategory = 'A' ; hit
      endcase

  endfor


; Look for large differences between assimilated snowfall and
; observations that participated in the assimilation pass.

  exceedanceCutoff = 12.0 ; inches of difference for warning

  ind = WHERE((snflPtsDiag.snowfallObsPassedAssim1QC eq 1) and $
              (ABS(snflPtsDiag.snowfall1stPass - $
                   snflPtsDiag.snowfallObs) gt exceedanceCutoff), $
              count)

  if (count gt 0) then begin
      ERR_MSG, 'WARNING: first pass results differ significantly ' + $
               'from ' + STRCRA(count) + $
               ' observations used in the pass.'
      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_pass_1_warning.csv'
      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, 'longitude,latitude,snowfall_delta_inches'
      for k = 0, count - 1 do begin
          sc = ind[k]
          PRINTF, lun, $
                  STRCRA(STRING(siteLon[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(STRING(siteLat[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(h1[sc] - siteObs[sc])
      endfor
      FREE_LUN, lun
  endif

  if verbose then begin


;     Determine how many points went in the right direction vs. how
;     many went in the wrong direction, as a basic test of the
;     assimilation.

;     static = unchanged (absolute value of change less than 0.1)
;     improved = closer to zero, same sign
;     overshot = closer to zero, opposite in sign
;     worsened = farther from zero, same sign
;     extreme = farther from zero, opposite in sign

      numStatic = 0L
      numImproved = 0L
      numOvershot = 0L
      numWorsened = 0L
      numExtreme = 0L

      for sc = 0, numSnowfall - 1 do begin

          BGDiff = siteGuess[sc] - siteObs[sc]
          sgnBGDiff = 0
          if (BGDiff ne 0.0) then sgnBGDiff = FIX(BGDiff / ABS(BGDiff))
          P1Diff = h1[sc] - siteObs[sc]
          sgnP1Diff = 0
          if (P1Diff ne 0.0) then sgnP1Diff = FIX(P1Diff / ABS(P1Diff))

          case 1 of
              ((ABS(P1Diff) lt ABS(BGDiff)) and $
               (ABS(BGDiff) - ABS(P1Diff) gt 0.1)) : begin ; improved/overshot
;              (ABS(P1Diff) lt ABS(BGDiff)): begin ; improved/overshot
                  if (sgnP1Diff ne 0) then begin
                      if (sgnP1Diff ne sgnBGDiff) then $
                          numOvershot++ $
                      else $
                          numImproved++
                  endif else numImproved++
              end
              ((ABS(P1Diff) gt ABS(BGDiff)) and $
               (ABS(P1Diff) - ABS(BGDiff) gt 0.1)) : begin ; worsened/extreme
;              (ABS(P1Diff) gt ABS(BGDiff)): begin ; worsened/extreme
                  if (sgnBGDiff ne 0) then begin
                      if (sgnP1Diff ne sgnBGDiff) then begin
                          numExtreme++ 
                      endif else begin
                          numWorsened++
;                          print, 'XXX ', snflPtsDiag[sc]
                      endelse
                  endif else begin
                      numWorsened++
;                      print, 'YYY ', snflPtsDiag[sc]
                  endelse
              end
              else: numStatic++
          endcase

      endfor

      if verbose then begin
          USR_MSG, 'Inventory of qualitative changes in 1st pass:'
          USR_MSG, '- # static: ' + STRCRA(numStatic)
          USR_MSG, '- # improved: ' + STRCRA(numImproved)
          USR_MSG, '- # improved but overshot: ' + STRCRA(numOvershot)
          USR_MSG, '- # worsened: ' + STRCRA(numWorsened)
          USR_MSG, '- # worsened and overshot: ' + STRCRA(numExtreme)
      endif

  endif


; Index and count contingency table subsets for 1st pass result.

  debug_tag = 4600

  abInd = WHERE(h1 gt snflThreshold, abCount)     ; predicted events
  acInd = WHERE(siteObs gt snflThreshold, acCount) ; observed events
  bcInd = WHERE((siteObs gt snflThreshold) xor $
                (h1 gt snflThreshold), bcCount)   ; errors
  abcInd = WHERE((siteObs gt snflThreshold) or $
                 (h1 gt snflThreshold), abcCount) ; all events
  aInd = WHERE((siteObs gt snflThreshold) and $
               (h1 gt snflThreshold), aCount)     ; hits
  bInd = WHERE((h1 gt snflThreshold) and $
               (siteObs le snflThreshold), bCount) ; false positives
  abcdInd = LINDGEN(numSnowfall)
  abcdCount = numSnowfall

;+
; False alarm trajectory.
; GF 20190805 modified logic.
; Second bit (2) tells you if it was false positive after the first pass.
;-
  if (NOT(useRatio) and (bCount gt 0) and verbose) then begin

;+
;     Summarize false alarm trajectories.
;-
      USR_MSG, 'FAR: ' + STRCRA(FLOAT(bCount) / FLOAT(abCount)) + $
               '; false positive (FP) vs. correct negative (CN) ' + $
               'process summary:'
      USR_MSG, '     BG -> P1 (background -> pass 1)'
      ind = WHERE(fat[bInd] eq 0, count)
      USR_MSG, '     CN -> FP %: ' + $
               STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)
      ind = WHERE(fat[bInd] eq 1, count)
      USR_MSG, '     FP -> FP %: ' + $
               STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)

  endif else begin

      ;; if (abCount gt 0) then fat[abInd] = fat[abInd] + 2
      if (bCount gt 0) then fat[bInd] = fat[bInd] + 2

  endelse

  debug_tag = 4700

  if NOT(useRatio) then begin

;+
;     Report false alarm results to a file.
;-
      falseAlarmFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                       anlEndDate_YYYYMMDDHH + $
                       '_false_alarm_report.txt'
      OPENW, lun, outputDir + '/' + falseAlarmFile, /GET_LUN
      PRINTF, lun, 'False alarms for one-pass analysis:'
      if (bCount eq 0) then begin
          if (aCount gt 0) then begin
              PRINTF, lun, 'No false alarms, ' + $
                      STRCRA(aCount) + ' hits, ' + $
                      STRCRA(acCount - aCount) + ' misses.'
          endif else begin
              PRINTF, lun, 'No snowfall predicted at observation locations.'
          endelse
      endif else begin
          PRINTF, lun, $
                  'FAR: ' + STRCRA(bCount) + ' / ' + $
                  STRCRA(abCount) + ' = ' + $
                  STRCRA(FLOAT(bCount) / FLOAT(abCount))
          PRINTF, lun, 'Temperature statistics for all FP points:'
          FPAveTemp = siteAveTemp[bInd]
          PRINTF, lun, 'FP average temperatures (cutoff value ' + $
                  STRCRA(aveTempCutoff) + ' deg C):'
          PRINTF, lun, '   min: ' + STRCRA(MIN(FPAveTemp))
          PRINTF, lun, '   max: ' + STRCRA(MAX(FPAveTemp))
          PRINTF, lun, '   mean: ' + STRCRA(MEAN(FPAveTemp))
          PRINTF, lun, '   median: ' + $
                  STRCRA(MEDIAN(FPAveTemp, /EVEN))
          FPMinTemp = siteMinTemp[bInd]
          PRINTF, lun, 'FP minimum temperatures: (cutoff value ' + $
                  STRCRA(minTempCutoff) + ' deg C):'
          PRINTF, lun, '   min: ' + STRCRA(MIN(FPMinTemp))
          PRINTF, lun, '   max: ' + STRCRA(MAX(FPMinTemp))
          PRINTF, lun, '   mean: ' + STRCRA(MEAN(FPMinTemp))
          PRINTF, lun, '   median: ' + $
                  STRCRA(MEDIAN(FPMinTemp, /EVEN))
          ind = WHERE(siteObsIsReal[bInd] eq 1, count)
          PRINTF, lun, '# of FP sites with direct observations: ' + $
                  STRCRA(count)
          ind = WHERE(siteObsIsReal[bInd] eq 0, count)
          PRINTF, lun, '# of FP sites with indirect observations: ' + $
                  STRCRA(count)
          PRINTF, lun, 'False positive (FP) vs. correct negative (CN) ' + $
                  'process summary:'
          PRINTF, lun, 'BG -> P1 (background -> pass 1)'
          ind = WHERE(fat[bInd] eq 0, count)
          PRINTF, lun, 'CN -> FP %: ' + $
                  STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)
          ind = WHERE(fat[bInd] eq 1, count)
          PRINTF, lun, 'FP -> FP %: ' + $
                  STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)

          if (count gt 0) then begin

;+
;             Additional details on FP -> FP points.
;-
              PRINTF, lun, 'Further details on FP -> FP points:'
              subInd = WHERE((siteWEASDSource[bInd[ind]] eq -1.0) or $
                             (siteWEASDSource[bInd[ind]] eq 0.0), $
                             subCount)
              if (subCount gt 0) then begin
                  ERR_MSG, 'ERROR: Background false positives ' + $
                           'associated with zero background WEASD; ' + $
                           'debugging needed.'
                  if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
              endif
              subInd = WHERE(siteWEASDSource[bInd[ind]] eq 1.0, $
                             WEASD1Count)
              PRINTF, lun, '# of FP -> FP sites with WEASD in QPF: ' + $
                      STRCRA(WEASD1Count)
              subInd = WHERE(siteWEASDSource[bInd[ind]] eq 2.0, $
                             WEASD2Count)
              PRINTF, lun, '# of FP -> FP sites with WEASD missing ' + $
                      'in QPF, added from QPE: ' + $
                      STRCRA(WEASD2Count)
              FPAveTemp = FPAveTemp[ind]
              PRINTF, lun, 'FP -> FP average temperatures:'
              PRINTF, lun, '         min: ' + STRCRA(MIN(FPAveTemp))
              PRINTF, lun, '         max: ' + STRCRA(MAX(FPAveTemp))
              PRINTF, lun, '         mean: ' + STRCRA(MEAN(FPAveTemp))
              PRINTF, lun, '         median: ' + $
                      STRCRA(MEDIAN(FPAveTemp, /EVEN))
              FPMinTemp = FPMinTemp[ind]
              PRINTF, lun, 'FP -> FP minimum temperatures:'
              PRINTF, lun, '         min: ' + STRCRA(MIN(FPMinTemp))
              PRINTF, lun, '         max: ' + STRCRA(MAX(FPMinTemp))
              PRINTF, lun, '         mean: ' + STRCRA(MEAN(FPMinTemp))
              PRINTF, lun, '         median: ' + $
                      STRCRA(MEDIAN(FPMinTemp, /EVEN))
              subInd = WHERE(siteObsIsReal[bInd[ind]] eq 1, subCount)
              PRINTF, lun, '# of FP -> FP sites with ' + $
                      'direct observations: ' + $
                      STRCRA(subCount)
              subInd = WHERE(siteObsIsReal[bInd[ind]] eq 0, subCount)
              PRINTF, lun, '# of FP -> FP sites with ' + $
                      'indirect observations: ' + $
                      STRCRA(subCount)
          endif
      endelse
      FREE_LUN, lun

  endif


; Report on contingency inventory.

  if verbose then begin
      USR_MSG, 'After first pass:'
      USR_MSG, '  A (hits): ' + STRCRA(aCount)
      USR_MSG, '  B (false positives): ' + STRCRA(bCount)
      USR_MSG, '  C (misses): ' + STRCRA(acCount - aCount)
      USR_MSG, '  D (correct negatives): ' + STRCRA(numSnowfall - abcCount)
  endif


; Report on bias and errors in 1st pass result.

  debug_tag = 4800

  if verbose then USR_MSG, 'Bias/error stats for 1st pass:'

  if (aCount gt 0) then begin

      sfav2Diag.pass_1_RMSE = $
          SQRT(TOTAL((h1[aInd] - siteObs[aInd])^2.0) / aCount)
      sfav2Diag.pass_1_POD = FLOAT(aCount) / FLOAT(acCount)

      pass1LogBias = ALOG10(h1[aInd] / siteObs[aInd])
      ;; sfav2Diag.pass_1_geom_mean_bias = $
      ;;     10.0D^MEAN(ALOG10(h1[aInd] / f[aInd]))
      sfav2Diag.pass_1_geom_mean_bias = $
          10.0D^MEAN(pass1LogBias)

      ;; sdBias = STDDEV(pass1LogBias)
      ;; b1 = 10.0D^(MEAN(pass1LogBias) - sdBias)
      ;; b2 = 10.0D^(MEAN(pass1LogBias) + sdBias)
      ;; PRINT, 'bias std. dev. range: ', b1, b2

      if verbose then begin
          USR_MSG, '- RMSE (hits): ' + STRCRA(sfav2Diag.pass_1_RMSE)
          USR_MSG, '- POD: ' + $
                   STRCRA(aCount) + ' / ' + $
                   STRCRA(acCount) + ' = ' + $
                   STRCRA(sfav2Diag.pass_1_POD)
          USR_MSG, '- Geometric mean bias: ' + $
                   STRCRA(sfav2Diag.pass_1_geom_mean_bias)
          if (aCount gt 1) then $
              USR_MSG, '- Geometric std. dev. of bias: ' + $
                       STRCRA(10.0D^(STDDEV(pass1LogBias)))
      endif

  endif else begin

      if verbose then USR_MSG, '- no hits'

  endelse

  if (bCount gt 0) then begin

      sfav2Diag.pass_1_FAR = FLOAT(bCount) / FLOAT(abCount)

      falseAlarmMean = 0.0
      falseAlarmMean = MEAN(h1[bInd])
      falseAlarmMedian = MEDIAN(h1[bInd], /EVEN)

      ;; if verbose then USR_MSG, '- FAR: ' + $
      ;;                          STRCRA(bCount) + ' / ' + $
      ;;                          STRCRA(abCount) + ' = ' + $
      ;;                          STRCRA(sfav2Diag.pass_1_FAR) + $
      ;;                          ' ; mean false alarm amount = ' + $
      ;;                          STRCRA(falseAlarmMean)

      if verbose then begin
          USR_MSG, '- FAR: ' + $
                   STRCRA(bCount) + ' / ' + $
                   STRCRA(abCount) + ' = ' + $
                   STRCRA(sfav2Diag.pass_1_FAR)
          USR_MSG, '       mean false alarm amount = ' + $
                   STRCRA(falseAlarmMean)
          USR_MSG, '       median false alarm amount = ' + $
                   STRCRA(falseAlarmMedian)
      endif

  endif

  debug_tag = 4900

  if useRatio then begin

      if (aCount eq 0) then begin
          ERR_MSG, 'ERROR: no hits after assimilation.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

;+
;     Redo the histogram of log-ratios to show any improvement for
;     the first assimilation pass.
;-
      z_ = ALOG10(h1[aInd] / siteObs[aInd])
      zCount = aCount

      zTitle = 'Log-ratio of 1st Pass Result to Obs. Snowfall'
      zUnits = 'dimensionless'

      hMin = -2.0
      hMax = 2.0
      binSize = 0.1
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad
      hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize
      zHistOld = zHist
      zHist = HISTOGRAM(z_, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      if verbose then begin

;+
;         Display the before/after histogram of log-ratios.
;-
          WSET_OR_WINDOW, 1

          PLOT, hAxis, zHist[0:numBins - 1], $
                TITLE = '2a. ' + zTitle, XTITLE = zUnits, $
                YRANGE = [0, MAX([zHist, zHistOld])], $
                /NODATA
          OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                 LINESTYLE = 2, COLOR = 100
          OPLOT, hAxis, zHist[0:numBins - 1], PSYM = 10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Kent added code below for png file of differences          

      endif

      if produceImages then begin

          oldDevice = !D.Name
          oldFont = !P.Font

          plotFile = 'sfav2_' + domainLabel + $
                     '_' + durationStr + 'h_' + $
                     anlEndDate_YYYYMMDDHH + $
                     '_1st_pass_obs_diff_hist'

          SET_PLOT, 'PS'
          DEVICE, FILE = outputDir + '/' + plotFile + '.ps'
          !P.Font = 1           ; TrueType
          DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT

          PLOT, hAxis, zHist[0:numBins - 1], $
                TITLE = zTitle, XTITLE = zUnits, $
                XRANGE = [0.75 * hMin, 0.75 * hMax], $
                YRANGE = [0, MAX([zHist, zHistOld])], $
                /NODATA
          OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                 LINESTYLE = 2, COLOR = 100, THICK = 2
          OPLOT, hAxis, zHist[0:numBins - 1], PSYM = 10, THICK = 2

          DEVICE, /CLOSE
          SET_PLOT, oldDevice
          !P.Font = oldFont

          cmd = utilsDir + '/pstopng ' + outputDir + '/' + plotFile + '.ps'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: Failed to convert ' + $
                       outputDir + '/' + plotFile + '.ps to PNG format'
          endif else begin
              cmd = 'mogrify -trim -border 4% -bordercolor white ' + $
                    outputDir + '/' + plotFile + '.png'
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then begin
                  ERR_MSG, 'WARNING: Failed to fine-tune ' + $
                           outputDir + '/' + plotFile + '.png.'
              endif
              FILE_DELETE, outputDir + '/' + plotFile + '.ps'
          endelse

      endif

; Kent added code to produce png file of differences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  endif else begin

      if (abcdCount eq 0) then begin
          ERR_MSG, 'ERROR: no data remain after assimilation.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

;+
;     Redo the histogram of differences to show any improvement for
;     the first (and only) assimilation pass.
;-
      z_ = h1[abcdInd] - siteObs[abcdInd]
      zCount = abcdCount

      zTitle = 'Difference Between 1st Pass Result and Obs. Snowfall'
      zUnits = 'inches'

      hMin = -5.0
      hMax = 5.0
      binSize = 0.25
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad
      hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize
      zHistOld = zHist
      zHist = HISTOGRAM(z_, MIN = hMin, MAX = hMax, BINSIZE = binSize)

      if verbose then begin

;+
;         Display the before/after histogram of differences.
;-
          WSET_OR_WINDOW, 1

          PLOT, hAxis, zHist[0:numBins - 1], $
                TITLE = '2b. ' + zTitle, XTITLE = zUnits, $
                YRANGE = [1, MAX([zHist, zHistOld])], /YLOG, $
                /NODATA
          OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                 LINESTYLE = 2, COLOR = 100
          OPLOT, hAxis, zHist[0:numbins - 1], PSYM = 10

      endif

  endelse


; Write differences to csv file.

  debug_tag = 5000

  csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
            anlEndDate_YYYYMMDDHH + $
            '_obs_diff_2.csv'
  OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
  PRINTF, lun, $
          'longitude,latitude,snowfall_1st_pass_error_inches,station_id'
  for sc = 0, numSnowfall - 1 do begin
      PRINTF, lun, $
              STRCRA(STRING(siteLon[sc], FORMAT = '(F11.6)')) + ',' + $
              STRCRA(STRING(siteLat[sc], FORMAT = '(F11.6)')) + ',' + $
              STRCRA(h1[sc] - siteObs[sc]) + ',' + $
;              snowfallReport[sc].station_id
              siteID[sc]
  endfor
  FREE_LUN, lun
  if (updateGISProject) then begin
      origFile = outputDir + '/' + csvFile
      copyFile = GISProjectDir + '/' + $
                 'sfav2_' + domainLabel + '_obs_diff_2.csv'
      if FILE_TEST(copyFile) then begin
          cmd = 'rm -f ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif
      cmd = 'cp -f ' + origFile + ' ' + copyFile
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then $
          ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
  endif

  debug_tag = 5100

  if useRatio then begin


    ;=========================================================;
    ; 5. Make a second pass on residual differences for error ;
    ; correction.                                             ;
    ;=========================================================;

      debug_tag = 5200

      if verbose then USR_MSG, '--- 2ND ASSIMILATION PASS ---'

    ;--------------------------------------------------------------;
    ; 5a. Evaluate difference between observations and first pass. ;
    ;--------------------------------------------------------------;


;     Calculate differences.

      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          errMsg = ['Integer divide by zero', $
                    'Integer overflow', $
                    'Unspecified error 2^2', $
                    'Unspecified error 2^4', $
                    'Floating divide by zero', $
                    'Floating underflow', $
                    'Floating overflow', $
                    'Floating Illegal operand']
          ERR_MSG, 'Unhandled math error/s before 2nd pass differences:'
          for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
              ERR_MSG, errMsg[i]
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif


;     Select points to be assimilated for the second pass and points
;     to be used for the second pass variogram. They might be
;     different. Either index could be aInd (hits only), abcInd (all
;     hits and errors), or abcdInd (all points).

      ;; variogramPointsInd = abcdInd
      ;; variogramPointsCount = abcdCount

      assimPointsInd = abcdInd
      assimPointsCount = abcdCount

      z_ = h1[assimPointsInd] - siteObs[assimPointsInd]

;+ GF 20180831
;+
;     Identify false positives. This should already be done via bInd
;     and bCount, so this is really not needed (especially given that
;     we use bInd and bCount below to modify KrigeWeightAdjust
;     anyway), but we are hewing a bit to the approach used below to
;     get abcFlag (which is also more work than is necessary). At
;     least this way we make certain that our contingency table has
;     not changed...
;-
      bFlag = (h1[assimPointsInd] gt snflThreshold) and $
              (siteObs[assimPointsInd] le snflThreshold)

      FPPointsInd = WHERE(bFlag, FPPointsCount)
      if (FPPointsCount ne bCount) then begin
          ERR_MSG, 'ERROR: (PROGRAMMING) FPPointsCount vs. bCount ' + $
                   'mismatch.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

;+
;     Initialize KrigeWeightAdjust to 1.0 for all points, and increase
;     it to 2.0 for false positives. Careful here: we are using bInd
;     to index false positives, but that index is defined relative to
;     all observations (dimension numSnowfall), though it should
;     really be an index relative to assimPointsInd. That is not
;     necessary, however, because assimPointsInd = abcdInd, and
;     abcdInd simply indexes all numSnowfall points. The benefit of
;     having abcdInd is... not known. Probably there is none. Just to
;     note, this all means the following are true:
;
;         abcdCount eq numSnowfall
;         assimPointsCount eq numSnowfall
;
;     We will check that here just to make sure sanity has survived.
;-
      if (abcdCount ne numSnowfall) then begin
          ERR_MSG, 'ERROR: (PROGRAMMING) abcdCount vs. numSnowfall mismatch.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (assimPointsCount ne numSnowfall) then begin
          ERR_MSG, 'ERROR: (PROGRAMMING) assimPointsCount vs. ' + $
                   'numSnowfall mismatch.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      KrigeWeightAdjust = REPLICATE(1.0, assimPointsCount)
;      if (bCount gt 0) then begin
;          if verbose then $
;              USR_MSG, 'Increasing kriging weights for ' + $
;                       STRCRA(bCount) + ' false positives.'
;          KrigeWeightAdjust[bInd] = 4.0
;      endif
;- GF 20180831


;     Get a subset of z_ that does not include correct negatives, for
;     doing histograms and for outlier test.

      abcFlag = (siteObs[assimPointsInd] gt snflThreshold) or $
                (h1[assimPointsInd] gt snflThreshold)

      histPointsInd = WHERE(abcFlag, histPointsCount)
      if (histPointsCount eq 0) then STOP ; GREG 20180925
      if (histPointsCount ne abcCount) then begin
          ERR_MSG, 'ERROR: (PROGRAMMING) histPointsCount vs. abcCount ' + $
                   'mismatch.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      ;; zVariogram = h1[variogramPointsInd] - f[variogramPointsInd]

      zTitle = 'Difference Between 1st Pass and Obs. Snowfall'
      zUnits = 'inches'


;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;


;+
;     Generate a histogram of residual differences after the first
;     assimilation pass. This will be the "before" histogram for the
;     second assimilation pass.
;-
      hMin = -5.0
      hMax = 5.0
      binSize = 0.25
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad
      hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize

      zHist = HISTOGRAM(z_[histPointsInd], $
                        MIN = hMin, MAX = hMax, BINSIZE = binSize)

      if verbose then begin

;+
;         Display the histogram of residual differences after the
;         first assimilation pass.
;-
          WSET_OR_WINDOW, 1

          PLOT, hAxis, zHist[0:numBins - 1], PSYM = 10, $
                TITLE = '2c. ' + zTitle, XTITLE = zUnits

      endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Kent - added code for png of histogram 

      oldDevice = !D.Name
      oldFont = !P.Font

      plotFile = 'sfav2_' + domainLabel + $
                 '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_1st_pass_obs_diff_hist_inches'

      SET_PLOT, 'PS'
      DEVICE, FILE = outputDir + '/' + plotFile + '.ps'
      !P.Font = 1               ; TrueType
      DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT

      PLOT, hAxis, zHist[0:numBins - 1], PSYM = 10, $
            TITLE = zTitle, XTITLE = zUnits

      DEVICE, /CLOSE
      SET_PLOT, oldDevice
      !P.Font = oldFont

      cmd = utilsDir + '/pstopng ' + outputDir + '/' + plotFile + '.ps'
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'WARNING: Failed to convert ' + $
                   outputDir + '/' + plotFile + '.ps to PNG format'
              ;if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif else begin
          cmd = 'mogrify -trim -border 4% -bordercolor white ' + $
                outputDir + '/' + plotFile + '.png'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: Failed to fine-tune ' + $
                       outputDir + '/' + plotFile + '.png.'
          endif
          FILE_DELETE, outputDir + '/' + plotFile + '.ps'
      endelse


;Kent - end of added code for png of histogram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      debug_tag = 5300

      if verbose then begin

          WSET_OR_WINDOW, 1

          mathErrors = CHECK_MATH()
          if (mathErrors ne 0) then begin
              errMsg = ['Integer divide by zero', $
                        'Integer overflow', $
                        'Unspecified error 2^2', $
                        'Unspecified error 2^4', $
                        'Floating divide by zero', $
                        'Floating underflow', $
                        'Floating overflow', $
                        'Floating Illegal operand']
              ERR_MSG, 'Unhandled math error/s after 2nd pass differences:'
              for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
                  ERR_MSG, errMsg[i]
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif


;         Display differences.

          X_MAP_GEO_POINTS, siteLon[assimPointsInd], $
                            siteLat[assimPointsInd], $
                            z_, $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_sfDiff, $
                            red_sfDiff, grn_sfDiff, blu_sfDiff, $
                            0, $
                            status, $
                            NDV = ndv, $
                            /SHOW_LOW, $
                            /SHOW_HIGH, $
                            TITLE = zTitle + ': ' + subTitle, $
                            /COLORBAR, $
                            XSIZE_TARGET = xszt, $
                            UNITS = zUnits, $
                            SHAPE_PATH_LIST = shapePathList

      endif

      debug_tag = 5400

      if produceImages then begin


;         Write difference points to an image.

          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_11_obs_to_1st_pass_difference.png'

          killme = snflGridAssimInches
          killme[*,*] = ndv

          MAKE_LON_LAT_MAP_PNG_SFAV2, $
              killme[extraCols:numColsAnl - extraCols - 1, $
                     extraRows:numRowsAnl - extraRows - 1], $
              ndv, $
              edges_sfDiff, red_sfDiff, grn_sfDiff, blu_sfDiff, $
              lonResOut, minLonOut, maxLonOut, $
              latResOut, minLatOut, maxLatOut, $
              zTitle + '!C' + longSubTitle, $
              zUnits, $
              outputDir + '/' + PNGFile, $
              POINT_LON = siteLon[assimPointsInd], $
              POINT_LAT = siteLat[assimPointsInd], $
              POINT_VAL = z_, $
              /SHOW_LOW, $
              /SHOW_HIGH, $
              /NO_GRID, /NO_CONTINENTS, /NO_USA, $
              /BLACK_ON_WHITE, $
              MAP_SHAPE_PATH = shapePathList

          killme = !NULL

      endif


;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


    ;-------------------------------;
    ; 5b. Filter correct negatives. ;
    ;-------------------------------;

;     Snowfall observations of zero will only affect the process if
;     they are in the neighborhood of a nonzero value on the input
;     grid. Eliminating zero reports that are not in the neighborhood
;     of nonzero gridded snowfall from the first pass should save
;     significant processing time. However, the distance criterion
;     used is twice the estimated neighborhood radius so that analysis
;     locations midway between zero and nonzero data (observations or
;     first-pass results) can be influenced equally by both.

;     "CN" means "correct negative"; i.e., zero guessed, zero
;     observed.

      debug_tag = 5500

      maxLagForCN = 2.0D * maxLagMeters_P2

      if verbose then $
          USR_MSG, 'Filtering correct negatives using a distance of ' + $
                   STRCRA(maxLagForCN / 1000.0) + ' km.'

      if (NOT(ISA(x2Anl)) or NOT(ISA(y2Anl))) then begin

          xAnl = minLonAnl + (0.5D + DINDGEN(numColsAnl)) * lonResOut
          yAnl = minLatAnl + (0.5D + DINDGEN(numRowsAnl)) * latResOut

          x2Anl = xAnl # REPLICATE(1.0D, numRowsAnl)
          y2Anl = TRANSPOSE(yAnl # REPLICATE(1.0D, numColsAnl))
          xAnl = !NULL & yAnl = !NULL

      endif

      ind = $
          SFAV2_FILTER_CORRECT_NEGATIVES(maxLagForCN, $
                                         mPerDegLonRef, $
                                         mPerDegLatRef, $
                                         x2Anl, $
                                         y2Anl, $
                                         snflGridAssimInches, $
                                         siteLon, $
                                         siteLat, $
                                         siteObs, $
                                         assimPointsInd, $
                                         assimPointsCount, $
                                         all_dist_2d, $
                                         all_k_2d, $
                                         all_n_1d, $
                                         DISTANCE_PRECISION = dPrecision, $
                                         HASH = verbose)

      assimPointsCount = N_ELEMENTS(ind) - (ind[0] eq -1)
      if (assimPointsCount eq 0) then begin
          ERR_MSG, 'ERROR: After the first filter for correct negatives, ' + $
                   'no observations remain.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      assimPointsInd = assimPointsInd[ind]
      z_ = z_[ind]
      bFlag = bFlag[ind] ; this flag has no purpose; we do not need it
      KrigeWeightAdjust = KrigeWeightAdjust[ind]
      abcFlag = abcFlag[ind]
      histPointsInd = WHERE(abcFlag, histPointsCount)
      if (histPointsCount eq 0) then STOP ; GREG 20180925

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

      if verbose then begin

;+
;         Display differences after the first filtering pass.
;-
          X_MAP_GEO_POINTS, siteLon[assimPointsInd], $
                            siteLat[assimPointsInd], $
                            z_, $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_sfDiff, red_sfDiff, grn_sfDiff, blu_sfDiff, $
                            0, $
                            status, $
                            NDV = ndv, $
                            /SHOW_LOW, $
                            /SHOW_HIGH, $
                            TITLE = zTitle + ': ' + subTitle, $
                            /COLORBAR, $
                            XSIZE_TARGET = xszt, $
                            UNITS = zUnits, $
                            SHAPE_PATH_LIST = shapePathList

      endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


    ;--------------------------------------------------------------;
    ; 5c. Generate the empirical semivariogram and fit a spherical ;
    ;      variogram model to it.                                  ;
    ;--------------------------------------------------------------;

      debug_tag = 5600

      if verbose then begin
          USR_MSG, 'Generating empirical semivariogram of differences ' + $
                   'between 1st pass result and observations.'
          ;; USR_MSG, 'Maximum lag: ' + STRCRA(maxLagMeters_P2 / 1000.0) + ' km'
          ;; USR_MSG, 'Lag tolerance: ' + STRCRA(lagTolerance_P2) + ' m'
      endif

      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          errMsg = ['Integer divide by zero', $
                    'Integer overflow', $
                    'Unspecified error 2^2', $
                    'Unspecified error 2^4', $
                    'Floating divide by zero', $
                    'Floating underflow', $
                    'Floating overflow', $
                    'Floating Illegal operand']
          ERR_MSG, 'Unhandled math error/s before 2nd pass variogram:'
          for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
              ERR_MSG, errMsg[i]
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      vParamsSpher = !NULL

      if verbose then begin

          WSET_OR_WINDOW, 1

          plotTitle = 'Empirical Variogram of ' + zTitle + ': ' + subTitle

          GEO_SEMIVARIOGRAM, siteLon[assimPointsInd], $
                             siteLat[assimPointsInd], $
                             z_, $
                             maxLagMeters_P2, $
                             status, $
                             /HASH, $
                             DISTANCE_PRECISION = dPrecision, $
                             MIN_LAG_BIN_COUNT = 6, $
;+ GF 20180806
;                             SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = $
;                               minRangeMeters, $
                             SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = $
                               minRangeMeters_P2, $
;- GF 20180806
                             LAG_TOLERANCE = lagTolMeters, $
                             SPHERICAL_SEMIVARIOGRAM_PARAMS = vParamsSpher, $
                             SPHERICAL_SEMIVARIOGRAM_RMSE = RMSESpher, $
                             SPHERICAL_SEMIVARIOGRAM_IN_RANGE_RMSE = $
                             RMSESpherInRange, $
                             LAG_OUT = lagMeters, $
                             SEMIVARIOGRAM_OUT = eVario, $
                             PLOT_TITLE = plotTitle, $
                             /SHORT_LAG_WEIGHT_BIAS, $
                             /SCALE_INPUT_VARIABLE, $
                             /SHOW_PLOT, $
                             /VERBOSE

      endif else begin

          GEO_SEMIVARIOGRAM, siteLon[assimPointsInd], $
                             siteLat[assimPointsInd], $
                             z_, $
                             maxLagMeters_P2, $
                             status, $
                             DISTANCE_PRECISION = dPrecision, $
                             MIN_LAG_BIN_COUNT = 6, $
;+ GF 20180806
;                             SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = $
;                               minRangeMeters, $
                            SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = $
                               minRangeMeters_P2, $
;- GF 20180806
                             LAG_TOLERANCE = lagTolMeters, $
                             SPHERICAL_SEMIVARIOGRAM_PARAMS = vParamsSpher, $
                             SPHERICAL_SEMIVARIOGRAM_RMSE = RMSESpher, $
                             SPHERICAL_SEMIVARIOGRAM_IN_RANGE_RMSE = $
                             RMSESpherInRange, $
                             LAG_OUT = lagMeters, $
                             SEMIVARIOGRAM_OUT = eVario, $
                             /SHORT_LAG_WEIGHT_BIAS, $
                             /SCALE_INPUT_VARIABLE

      endelse

      if NOT(status) then begin
          USR_MSG, 'NOTICE: Failed to generate empirical variogram for ' + $
                   anlEndDate_YYYYMMDDHH


;         Crop first pass result to output grid and exit.

          outputGrid = $
              snflGridAssimInches[extraCols:numColsAnl - extraCols - 1, $
                                  extraRows:numRowsAnl - extraRows - 1]

          USR_MSG, 'NOTICE: Exiting with first pass result.'
          GOTO, ANALYSIS_COMPLETE

      endif

      debug_tag = 5700

      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          errMsg = ['Integer divide by zero', $
                    'Integer overflow', $
                    'Unspecified error 2^2', $
                    'Unspecified error 2^4', $
                    'Floating divide by zero', $
                    'Floating underflow', $
                    'Floating overflow', $
                    'Floating Illegal operand']
          ERR_MSG, 'Unhandled math error/s after 2nd pass variogram:'
          for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
              ERR_MSG, errMsg[i]
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif


;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

      if produceImages then begin


;         Produce a graphic of the semivariogram.

          plotTitle = 'Semivariogram: ' + zTitle + '!C' + longSubTitle + '!C '

          plotFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                     anlEndDate_YYYYMMDDHH + $
                     '_semivariogram_2nd_pass'

          oldDevice = !D.Name
          oldFont = !P.Font
          SET_PLOT, 'PS'
          DEVICE, FILE = outputDir + '/' + plotFile + '.ps', ENCAPSULATE = 0
          !P.Font = 1           ; TrueType
          DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT
          PLOT, lagMeters / 1000.0, eVario, $
                XTITLE = 'Separation (km)', $
                YTITLE = 'Semivariance', $
                PSYM = 10, $
                TITLE = plotTitle, $
                POS = [0.1, 0.1, 0.95, 0.85]
          OPLOT, lagMeters / 1000.0, $
                 SPHERICAL_SEMIVARIOGRAM_FUNC(lagMeters, vParamsSpher), $
                 COLOR = 150
          XYOUTS, 0.6, 0.3, $
                  'Spherical semivariogram:!C' + $
                  '- nugget: ' + FORMAT_FLOAT(vParamsSpher[0]) + '!C' + $
                  '- sill: ' + FORMAT_FLOAT(vParamsSpher[1]) + '!C' + $
                  '- range: ' + FORMAT_FLOAT(vParamsSpher[2] / 1000.0) + $
                  ' km!C' + $
                  '- RMSE of fit: ' + FORMAT_FLOAT(RMSESpher) + '!C' + $
                  '- in-range RMSE of fit: ' + FORMAT_FLOAT(RMSESpherInRange), $
                  /NORMAL
          DEVICE, /CLOSE
          SET_PLOT, oldDevice
          !P.Font = oldFont
          cmd = utilsDir + '/pstopng ' + outputDir + '/' + plotFile + '.ps'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: Failed to convert ' + $
                       outputDir + '/' + plotFile + '.ps to PNG format.'
          endif else begin
              cmd = 'mogrify -trim -border 4% -bordercolor white ' + $
                    outputDir + '/' + plotFile + '.png'
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then begin
                  ERR_MSG, 'WARNING: Failed to fine-tune ' + $
                           outputDir + '/' + plotFile + '.png.'
              endif
              FILE_DELETE, outputDir + '/' + plotFile + '.ps'
          endelse

      endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


;     Add variogram parameters to diagnostic structure.

      debug_tag = 5800

      sfav2Diag.pass_2_semivariogram_num_obs = assimPointsCount
      sfav2Diag.pass_2_semivariogram_fit_RMSE = RMSESpher
      sfav2Diag.pass_2_semivariogram_fit_RMSE_in_range = RMSESpherInRange
      sfav2Diag.pass_2_semivariogram_nugget = vParamsSpher[0]
      sfav2Diag.pass_2_semivariogram_sill = vParamsSpher[1]
      sfav2Diag.pass_2_semivariogram_range_meters = vParamsSpher[2]

      if verbose then begin


;         Print variogram parameters.

          USR_MSG, 'Spherical variogram parameters:'
          USR_MSG, '  # points: ' + STRCRA(assimPointsCount)
          USR_MSG, '  RMSE of fit: ' + STRCRA(RMSESpher)
          USR_MSG, '  In-range RMSE of fit: ' + STRCRA(RMSESpherInRange)
          USR_MSG, '  nugget: ' + STRCRA(vParamsSpher[0])
          USR_MSG, '  sill: ' + STRCRA(vParamsSpher[1])
          USR_MSG, '  range: ' + STRCRA(vParamsSpher[2] / 1000.0) + ' km'

      endif


;     Write variogram parameters to a CSV file.

      vParamsFile = 'sfav2_' + domainLabel + $
                    '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_spherical_semivariogram_params_pass_2.csv'

      CSV_WRITE_SEMIVARIOGRAM_PARAMETERS, $
          outputDir, $
          vParamsFile, $
          anlEndDate_YYYYMMDDHH, $
          sfav2Diag.pass_2_semivariogram_num_obs, $
          sfav2Diag.pass_2_semivariogram_fit_RMSE, $
          sfav2Diag.pass_2_semivariogram_fit_RMSE_in_range, $
          sfav2Diag.pass_2_semivariogram_nugget, $
          sfav2Diag.pass_2_semivariogram_sill, $
          sfav2Diag.pass_2_semivariogram_range_meters, $
          status, $
          VERBOSE = verbose

      if NOT(status) then begin
          ERR_MSG, 'ERROR: Failed to write variogram parameters to CSV file.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      debug_tag = 5900

      if ((2.0D * vParamsSpher[2]) lt maxLagForCN) then begin


        ;------------------------------------------------------------;
        ; 5d. Re-filter correct negatives using 2 x variogram range. ;
        ;------------------------------------------------------------;

;         Filter data again, the same as we did earlier, but using 2x
;         the range of the fitted semivariogram instead of 2x
;         maxLagMeters_P2.

          maxLagForCN = 2.0D * vParamsSpher[2]

          if verbose then $
              USR_MSG, 'Re-filtering correct negatives using 2 * ' + $
                       'variogram range = ' + $
                       STRCRA(maxLagForCN / 1000.0) + ' km.'

          ind = $
              SFAV2_FILTER_CORRECT_NEGATIVES(maxLagForCN, $
                                             mPerDegLonRef, $
                                             mPerDegLatRef, $
                                             x2Anl, $
                                             y2Anl, $
                                             snflGridAssimInches, $
                                             siteLon, $
                                             siteLat, $
                                             siteObs, $
                                             assimPointsInd, $
                                             assimPointsCount, $
                                             all_dist_2d, $
                                             all_k_2d, $
                                             all_n_1d, $
                                             DISTANCE_PRECISION = dPrecision, $
                                             HASH = verbose)

          assimPointsCount = N_ELEMENTS(ind) - (ind[0] eq -1)
          if (assimPointsCount eq 0) then begin
              ERR_MSG, 'ERROR: After the second filter for correct ' + $
                       'negatives, no observations remain.'
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif
          assimPointsInd = assimPointsInd[ind]
          z_ = z_[ind]
          bFlag = bFlag[ind] ; this flag has no purpose; we do not need it
          KrigeWeightAdjust = KrigeWeightAdjust[ind]
          abcFlag = abcFlag[ind]
          histPointsInd = WHERE(abcFlag, histPointsCount)
          if (histPointsCount eq 0) then STOP ; GREG 20180925

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

;+
;         Generate a histogram of the data to assimilate (differences)
;         after the second filtering pass to remove unneeded correct
;         negatives.
;-
          hMin = -5.0
          hMax = 5.0
          binSize = 0.25
          numBins = ROUND((hMax - hMin) / binSize)
          pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
          hMin = hMin - pad
          hMax = hMax + pad
          hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize
          zHistOld = zHist
          zHist = HISTOGRAM(z_[histPointsInd], $
                            MIN = hMin, MAX = hMax, BINSIZE = binSize)

          if verbose then begin

;+
;             Display differences after second filtering pass.
;-
              X_MAP_GEO_POINTS, siteLon[assimPointsInd], $
                                siteLat[assimPointsInd], $
                                z_, $
                                minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                                edges_sfDiff, $
                                red_sfDiff, grn_sfDiff, blu_sfDiff, $
                                0, $
                                status, $
                                NDV = ndv, $
                                /SHOW_LOW, $
                                /SHOW_HIGH, $
                                TITLE = zTitle + ': ' + subTitle, $
                                /COLORBAR, $
                                XSIZE_TARGET = xszt, $
                                UNITS = zUnits, $
                                SHAPE_PATH_LIST = shapePathList

;+
;             Display the histogram of data to assimilate
;             (differences) after the second filtering pass.
;-
              WSET_OR_WINDOW, 1 ;, XSIZE = 800, YSIZE = 400

              PLOT, hAxis, zHist[0:numBins - 1], $
                    TITLE = '2d. ' + zTitle, XTITLE = zUnits, $
                    YRANGE = [1, MAX([zHist, zHistOld])], $
                    /NODATA
              OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                     LINESTYLE = 2, COLOR = 100
              OPLOT, hAxis, zHist[0:numBins - 1], PSYM = 10

          endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

      endif

      debug_tag = 6000

      if verbose then WSET_OR_WINDOW, 1


    ;---------------------------------------;
    ; 5e. Assimilate remaining differences. ;
    ;---------------------------------------;

      zGrid = !NULL


;     Calculate number of radii for KRM.

      numKRMRadii = $
          CEIL(vParamsSpher[2] / targetKRMRingWidthMeters) > 10

      if (minLagMeters gt vParamsSpher[2]) then begin


;         Minimum lag exceeds variogram range, which is not acceptable

          USR_MSG, 'NOTICE: minimum lag ' + STRCRA(minLagMeters) + $
                   ' meters cannot exceed variogram range. Adjusting to ' + $
                   STRCRA(vParamsSpher[2])
          minLagMeters = vParamsSpher[2]

      endif


    ;---------------------------------------------------------------;
    ; 5e-1.  Simulate the kriging process to guess at the number of ;
    ;        solvers and the average number of points per solver.   ;
    ;---------------------------------------------------------------;

      debug_tag = 6100

      if verbose then $
          USR_MSG, 'Estimating solver size and points per solver.'

      xOutKrige = minLonOut + $
                  (0.5D + DINDGEN(numColsOutKrige)) * $
                  (lonResOut * krigeResFactor)
      yOutKrige = minLatOut + $
                  (0.5D + DINDGEN(numRowsOutKrige)) * $
                  (latResOut * krigeResFactor)

      rangeDegLat = vParamsSpher[2] / mPerDegLatRef
      rangeDegLon = vParamsSpher[2] / mPerDegLonRef

      assimLon = siteLon[assimPointsInd]
      assimLat = siteLat[assimPointsInd]

      numSolvers = 0L
      totalSolverPoints = 0L

      if verbose then begin
          if (numRowsOutKrige lt 50) then begin
              hashes = numRowsOutKrige
              lastProgress = -1
          endif else begin
              hashes = 50
              lastProgress = 0
          endelse
          for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
          PRINT, ''
      endif

      for rc = 0L, numRowsOutKrige - 1L do begin

          if verbose then begin
              progress = FIX(FLOAT(rc) / FLOAT(numRowsOutKrige - 1) * $
                             FLOAT(hashes))
              if (progress gt lastProgress) then begin
                  lastProgress = progress
                  PRINT, FORMAT='($,"#")'
              endif
          endif

          gridLat = yOutKrige[rc]

          for cc = 0L, numColsOutKrige - 1L do begin

              gridLon = xOutKrige[cc]

              inBoxInd = WHERE((ABS(assimLon - gridLon) lt rangeDegLon) and $
                               (ABS(assimLat - gridLat) lt rangeDegLat), $
                               inBoxCount)

              if (inBoxCount lt minHoodPoints) then CONTINUE

              assimLonInBox = assimLon[inBoxInd]
              assimLatInBox = assimLat[inBoxInd]

              dInBox = DISTANCE(dPrecision, $
                                gridLon, gridLat, $
                                assimLonInBox, assimLatInBox)

              boxInHoodInd = WHERE(dInBox lt vParamsSpher[2], inHoodCount)
              if (inHoodCount lt minHoodPoints) then CONTINUE

              hoodInd = inBoxInd[boxInHoodInd]
              hoodDist = dInBox[boxInHoodInd]
              numClose = TOTAL(hoodDist lt minLagMeters, /INT)
              if (numClose lt 3) then CONTINUE

              hoodZ = z_[hoodInd]
              minHoodZ = MIN(hoodZ)
              maxHoodZ = MAX(hoodZ)

              if ((maxHoodZ - minHoodZ) le hoodTolerance) then CONTINUE

              numSolvers++
              totalSolverPoints += inHoodCount

          endfor

      endfor

      if verbose then PRINT, ''

      assimLon = !NULL
      assimLat = !NULL

      if (numSolvers eq 0) then STOP ; GREG 20180925

      meanPointsPerSolver = ROUND(FLOAT(totalSolverPoints) / FLOAT(numSolvers))

      if verbose then begin
          USR_MSG, 'Est. number of solvers: ' + STRCRA(numSolvers)
          USR_MSG, 'Est. mean points per solver: ' + $
                   STRCRA(meanPointsPerSolver)
      endif


    ;--------------------------------------------------------------;
    ; 5e-2. Get nearest neighbor distances to compare with kriging ;
    ;       resolution.                                            ;
    ;--------------------------------------------------------------;

      debug_tag = 6200

      if verbose then $
          USR_MSG, 'Calculating nearest-neighbor distances for ' + $
                   'kriging points.'

      FIND_NEAREST_NEIGHBOR, siteLon[assimPointsInd], $
                             siteLat[assimPointsInd], $
                             nnInd, $
                             nnDist, $
                             DISTANCE_PRECISION = dPrecision, $
                             HASH = verbose

      order = SORT(nnDist)

      if verbose then begin
          USR_MSG, 'Median distance to nearest neighbor: ' + $
                   STRCRA(nnDist[order[assimPointsCount / 2]]) + ' meters'
          USR_MSG, 'Mean distance to nearest neighbor: ' + $
                   STRCRA(MEAN(nnDist)) + ' meters'
          USR_MSG, 'Est. longitudinal kriging resolution: ' + $
                   STRCRA(DISTANCE(dPrecision, $
                                   0.0D, $
                                   0.5D * (maxLatOut + minLatOut), $
                                   lonResOutKrige, $
                                   0.5D * (maxLatOut + minLatOut))) + $
                   ' meters'
          USR_MSG, 'Est. latitudinal kriging resolution: ' + $
                   STRCRA(DISTANCE(dPrecision, $
                                   0.0D, $
                                   0.5D * (maxLatOut + minLatOut) - $
                                   0.5D * latResOutKrige, $
                                   0.0D, $
                                   0.5D * (maxLatOut + minLatOut) + $
                                   0.5D * latResOutKrige)) + $
                   ' meters'
      endif


;     Identify remaining correct negatives.

      ind = WHERE((h1[assimPointsInd] eq 0.0) and $
                  (siteObs[assimPointsInd] eq 0.0), $
                  count)

      nonCNCount = assimPointsCount - count
      if verbose then $
          USR_MSG, 'Assimilating ' + STRCRA(assimPointsCount) + $
                   ' differences, with ' + STRCRA(count) + $
                   ' correct negatives and ' + STRCRA(nonCNCount) + $
                   ' other values.'


;     Break assimilation points out into new arrays.

      assimID = siteID[assimPointsInd]
      assimLon = siteLon[assimPointsInd]
      assimLat = siteLat[assimPointsInd]
      assimElev = siteElev[assimPointsInd]
      assimObs = siteObs[assimPointsInd]
      zAssim = z_
      ;KrigeWeightAdjust = KrigeWeightAdjust (duh)
      assimKeepFlag = siteKeepFlag[assimPointsInd]


;     Write differences being used in kriging pass to csv file.

      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_obs_diff_krige_in.csv'

      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, 'longitude,latitude,snowfall_error_inches'
      for ac = 0L, assimPointsCount - 1L do begin
          PRINTF, lun, $
                  STRCRA(STRING(assimLon[ac], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(STRING(assimLat[ac], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(zAssim[ac])
      endfor
      FREE_LUN, lun


    ;---------------------------------------------------------;
    ; 5e-3. Perform "oddball" outlier test, removing isolated ;
    ;       zero- and high-valued observations.                ;
    ;---------------------------------------------------------;

      debug_tag = 6300

      oddballThreshold = 1.0

      oddballFlag = $
          SFAV2_FLAG_ODDBALLS(assimId, $
                              assimLon, $
                              assimLat, $
                              assimElev, $
                              assimObs, $
                              ndv, $
                              vParamsSpher[2], $
                              minHoodPoints, $
                              snflThreshold, $
                              oddballThreshold, $
                              mPerDegLonRef, $
                              mPerDegLatRef, $
                              DISTANCE_PRECISION = dPrecision, $
                              KEEP_FLAG = assimKeepFlag, $
                              VERBOSE = verbose, $
                              CHECK_ID = checkID)

      useFlag_P2 = 1B - oddballFlag
      oddballFlag = !NULL


    ;--------------------------------------------------------------;
    ; 5e-3. Perform outlier test, excluding correct negatives from ;
    ;       the process.                                           ;
    ;--------------------------------------------------------------;

      abcInd = WHERE(abcFlag, count) ; relative to assimPointsInd
      if (count eq 0) then begin
          ERR_MSG, 'ERROR: No events remaining. ' + $
                   'We should never have gotten this far!'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      floorOutlierSD_P2 = 0.6

      outlierflag = SFAV2_FLAG_OUTLIERS(assimID[abcInd], $
                                        assimLon[abcInd], $
                                        assimLat[abcInd], $
                                        assimElev[abcInd], $
                                        assimObs[abcInd], $
                                        zAssim[abcInd], $
                                        ndv, $
                                        vParamsSpher[2], $
                                        minHoodPoints, $
                                        minOutlierDiff_P2, $
                                        floorOutlierSD_P2, $
                                        mPerDegLonRef, $
                                        mPerDegLatRef, $
                                        DISTANCE_PRECISION = dPrecision, $
                                        KEEP_FLAG = assimKeepFlag[abcInd], $
                                        VERBOSE = verbose, $
                                        CHECK_ID = checkID)

      allOutlierFlag = BYTARR(assimPointsCount)
      allOutlierFlag[abcInd] = outlierFlag
      outlierFlag = TEMPORARY(allOutlierFlag) ; opposite of useFlag
      useFlag_P2 = useFlag_P2 * (1B - outlierFlag)
      outlierFlag = !NULL


;     Generate artificial zero reports in locations remote from real
;     observations where minimum daily temperatures are too low for
;     snow accumulation to be likely.

      minTempCutoffFakeZero = 2.0
      aveTempCutoffFakeZero = aveTempCutoff + 2.0
      artificialZeroProb = 0.10
      minDistToObs = 0.5D * minLagMeters    ; typically 50 km
      minDistToObs = 10.0D3                 ; tighten things up
      maxDistToObs = 2.0D * vParamsSpher[2] ; typically at least 200 km

      if NOT(ISA(x2Anl)) then STOP ; ORG CHECK ; GREG 20180925
      if NOT(ISA(y2Anl)) then STOP ; ORG CHECK ; GREG 20180925

      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_obs_diff_krige_in_fake_zero.csv'

; ----- TESTING -----
      if verbose then begin

          fAssim = siteObs[assimPointsInd]

;+
;         Display observations.
;-
          X_MAP_GEO_POINTS, siteLon[assimPointsInd], $
                            siteLat[assimPointsInd], $
                            siteObs[assimPointsInd], $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_snowfall, $
                            red_snowfall, grn_snowfall, blu_snowfall, $
                            5, $
                            status, $
                            ndv = NDV, $
                            /SHOW_HIGH, $
                            TITLE = 'Observed ' + durationStr + '-hour ' + $
                            'Snowfall, ' + anlEndDate_YYYYMMDDHH + $
                            ', w/Artificial Zeroes', $
                            /COLORBAR, $
                            XSIZE_TARGET = xszt * 2, $
                            UNITS = 'inches', $
                            SHAPE_PATH_LIST = shapePathList, $
                            MAP_STRUCT = lonLat, $
                            /OUTLINES, $
                            SYMBOL_SIZE = 1.5
      endif

      debug_tag = 6400

      SFAV2_CREATE_ARTIFICIAL_ZEROES, minTempGrid, $
                                      aveTempGrid, $
                                      snflGridAssimInches, $
                                      x2Anl, $
                                      y2Anl, $
                                      assimLon, $
                                      assimLat, $
                                      minTempCutoffFakeZero, $
                                      aveTempCutoffFakeZero, $
                                      artificialZeroProb, $
                                      minDistToObs, $
                                      maxDistToObs, $
                                      mPerDegLonRef, $
                                      mPerDegLatRef, $
                                      ndv, $
                                      warmInd, $
                                      warmLon, $
                                      warmLat, $
                                      warmFakeZ, $
                                      warmCount, $
                                      DISTANCE_PRECISION = dPrecision, $
                                      VERBOSE = verbose, $
                                      CSV_FILE = outputDir + '/' + csvFile, $
                                      MAP_STRUCT = lonLat
      ;; if verbose then begin
      ;;     PRINT, 'PRESS A KEY'
      ;;     move = GET_KBRD(1)
      ;; endif
      if (warmCount gt 0) then begin

          assimID = [assimID, REPLICATE('ARTIFICIAL', warmCount)]
          assimLon = [assimLon, warmLon]
          assimLat = [assimLat, warmLat]
          assimElev = [assimElev, REPLICATE(-9999, warmCount)]
          zAssim = [zAssim, warmFakeZ]
          KrigeWeightAdjust = [KrigeWeightAdjust, REPLICATE(1.0, warmCount)]
          assimObs = [assimObs, REPLICATE(ndv, warmCount)]
          assimKeepFlag = [assimKeepFlag, REPLICATE(0B, warmCount)]
          useFlag_P2 = [useFlag_P2, REPLICATE(1B, warmCount)]

      endif


    ;------------------------------------;
    ; 5e-4. Perform second kriging pass. ;
    ;------------------------------------;

      debug_tag = 6500

      if ((meanPointsPerSolver lt meanPointsPerSolverLimit) or $
          (numSolvers lt numSolversLimit)) then begin


;         Use textbook ordinary kriging.

          sfav2Diag.pass_2_assim_uses_radial_means = 0

          GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
              assimLon, $
              assimLat, $
              zAssim, $
              ndv, $
              vParamsSpher, $
              minLonOut, maxLonOut, $
              minLatOut, maxLatOut, $
              lonResOutKrige, latResOutKrige, $
              zGrid, $
              ERROR_VARIANCE = zGridVariance, $
              DISTANCE_PRECISION = dPrecision, $
              CROSS_VAL_POINTS = cvz, $
              CROSS_VAL_RMSE = cvRMSE, $
              MIN_LAG_METERS = minLagMeters, $
              MIN_OUTLIER_DIFF = minOutlierDiff_P2, $
              MIN_NEIGHBORHOOD_POINTS = minHoodPoints, $
              NEIGHBORHOOD_TOLERANCE = hoodTolerance, $
              VERBOSE = verbose, $
              ;; KEEP_FLAG = assimKeepFlag, $
              USE_FLAG = useFlag_P2, $
              KRIGE_WEIGHT_ADJUST = KrigeWeightAdjust, $
              WALL_TIME = wallTime_P2, $
              MEAN_POINTS_PER_SOLVER = meanPointsPerSolver, $
              NUM_SOLVERS = numSolvers, $
              /SKIP_GRUBBS

      endif else begin


;         Use ordinary kriging with radial means (KRM).

          sfav2Diag.pass_2_assim_uses_radial_means = 1

          GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
              assimLon, $
              assimLat, $
              zAssim, $
              ndv, $
              vParamsSpher, $
              minLonOut, maxLonOut, $
              minLatOut, maxLatOut, $
              lonResOutKrige, latResOutKrige, $
              zGrid, $
              ERROR_VARIANCE = zGridVariance, $
              DISTANCE_PRECISION = dPrecision, $
              /USE_KRM, $
              STAGGER_KRM_MESH = staggerKRMMesh, $
              NUM_KRM_RADII = numKRMRadii, $
              NUM_KRM_ANGLES = numKRMAngles_P2, $
              CROSS_VAL_POINTS = cvz, $
              CROSS_VAL_RMSE = cvRMSE, $
              MIN_LAG_METERS = minLagMeters, $
              MIN_OUTLIER_DIFF = minOutlierDiff_P2, $
              MIN_NEIGHBORHOOD_POINTS = minHoodPoints, $
              NEIGHBORHOOD_TOLERANCE = hoodTolerance, $
              VERBOSE = verbose, $
              ;; KEEP_FLAG = assimKeepFlag, $
              USE_FLAG = useFlag_P2, $
              KRIGE_WEIGHT_ADJUST = KrigeWeightAdjust, $
              WALL_TIME = wallTime_P2, $
              MEAN_POINTS_PER_SOLVER = meanPointsPerSolver, $
              NUM_SOLVERS = numSolvers, $
              /SKIP_GRUBBS

      endelse

      if NOT(ISA(zGrid)) then STOP ; GREG 20180925

    ;-----------------------------------------------------------------;
    ; Scale the kriging result. This is an EXPERIMENTAL step meant to ;
    ; force the kriging result to be more strongly correlated with    ;
    ; its inputs. Currently the process estimates bias but does not   ;
    ; apply the bias estimate to zGrid.                               ;
    ;-----------------------------------------------------------------;

;+
;     Sample the kriging result and its error variances.
;-

      debug_tag = 6600

      r2 = zAssim[0:assimPointsCount - 1]
      i_nn = ROUND((siteLon[assimPointsInd] - minLonOut) / $
                   lonResOutKrige - 0.5D)
      j_nn = ROUND((siteLat[assimPointsInd] - minLatOut) / $
                   latResOutKrige - 0.5D)
      r2_hat = zGrid[i_nn, j_nn]
      if ISA(zGridVariance) then r2_hat_var = zGridVariance[i_nn, j_nn]
      i_nn = !NULL
      j_nn = !NULL

      plotFile = outputDir + '/' + $
                 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + $
                 '_kriging_bias'

      title = 'Snowfall Kriging Bias, 2nd Pass, ' + subTitle
      krigingBias = SFAV2_KRIGING_OUTPUT_BIAS(r2, $
                                              r2_hat, $
                                              r2_hat_var, $
                                              ndv, $
                                              OUTPUT_PNG_PATH = plotFile, $
                                              TITLE = title, $
                                              UTILITIES_DIR = utilsDir, $
                                              VERBOSE = verbose)

      ;; if NOT(ISA(krigingBias)) then krigingBias = 1.0
      ;; ind = WHERE(zGrid ne ndv)
      ;; zGrid[ind] = zGrid[ind] / krigingBias

;+
;     Truncate useFlag_P2 to remove artificial zeroes.
;-
      if (N_ELEMENTS(useFlag_P2) ne (assimPointsCount + warmCount)) then STOP ; GREG 20180925
      if (warmCount gt 0) then useFlag_P2 = useFlag_P2[0:assimPointsCount - 1]


    ;-----------------------------------------------------------------;
    ; 5e-5. Smooth no-data areas in the kriging result with a         ;
    ;       neighborhood boxcar average, effectively treating no-data ;
    ;       values in the boxcar subgrid as zeroes.                   ;
    ;-----------------------------------------------------------------;

;GF 20190910 vv
;; PRINT, 'LEVU1'
;; PRINT, zGrid[141, 185]
;GF 20190910 ^^

      debug_tag = 6700

      if verbose then $
          USR_MSG, 'Smoothing no-data areas of kriging solution.'

      newZGrid = SFAV2_SMOOTH_NDV_AS_ZERO(zGrid, $
                                          ndv, $
                                          minLatOut, $
                                          maxLatOut, $
                                          lonResOutKrige, $
                                          latResOutKrige, $
                                          smoothHoodRadM, $
                                          DISTANCE_PRECISION = dPrecision, $
                                          HASH = verbose)

      zGrid = newZGrid
      newZGrid = !NULL

;GF 20190910 vv
;; PRINT, zGrid[141, 185]
;GF 20190910 ^^


;     Add kriging results to diagnostic structure.

      sfav2Diag.pass_2_assim_num_obs = assimPointsCount
      sfav2Diag.pass_2_mean_points_per_solver = meanPointsPerSolver
      sfav2Diag.pass_2_num_solvers = numSolvers
      sfav2Diag.pass_2_assim_wall_time = wallTime_P2
      sfav2Diag.pass_2_assim_cross_validation_RMSE = cvRMSE

      if verbose then $
          USR_MSG, 'Kriging cross-validation RMSE: ' + STRCRA(cvRMSE)
 
      if (krigeResFactor gt 1) then begin


;         Resample kriging output to full resolution.
; Kent - commented out lines below. Need to check on this
          arrSize = SIZE(zGrid)
          if (arrSize[0] ne 2) then STOP ; GREG 20180925
          if (arrSize[1] ne (numColsOutKrige)) then STOP ; GREG 20180925
          if (arrSize[2] ne (numRowsOutKrige)) then STOP ; GREG 20180925
          flag = FLOAT(zGrid ne ndv)
          ;; flag = REBIN(flag, numColsOut, numRowsOut)
          ;; zGrid = REBIN(zGrid, numColsOut, numRowsOut)
          flag = CONGRID(flag, numColsOut, numRowsOut, /INTERP)
          zGrid = CONGRID(zGrid, numColsOut, numRowsOut, /INTERP)
          ind = WHERE(flag lt 1.0, count)
          if (count gt 0) then zGrid[ind] = ndv

      endif

;GF 20190910 vv
;; PRINT, zGrid[353, 463]
;GF 20190910 ^^

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

      if verbose then begin


;         Display 2nd pass results.

          title = 'Gridded ' + zTitle + '!C' + subTitle

          X_MAP_GEO_GRID, zGrid, $
                          minLonOut, maxLonOut, minLatOut, maxLatOut, $
                          edges_sfDiff, red_sfDiff, grn_sfDiff, blu_sfDiff, $
                          0, $
                          status, $
                          NDV = ndv, $
                          /SHOW_LOW, $
                          /SHOW_HIGH, $
                          TITLE = title, $
                          /COLORBAR, $
                          XSIZE_TARGET = xszt, $
                          UNITS = zUnits, $
                          SHAPE_PATH_LIST = shapePathList

      endif

      if produceImages then begin


;         Write 2nd pass result to an image.

          title = 'Gridded ' + zTitle

          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_12_obs_interp_2nd_pass.png'

          MAKE_LON_LAT_MAP_PNG_SFAV2, $
              zGrid, $
              ndv, $
              edges_sfDiff, red_sfDiff, grn_sfDiff, blu_sfDiff, $
              lonResOut, minLonOut, maxLonOut, $
              latResOut, minLatOut, maxLatOut, $
              title, $
              'inches', $
              outputDir + '/' + PNGFile, $
              /SHOW_LOW, $
              /SHOW_HIGH, $
              TICK_NAMES = tickNames_sfDiff, $
              /NO_GRID, /NO_CONTINENTS, /NO_USA, $
              /BLACK_ON_WHITE, $
              MAP_SHAPE_PATH = shapePathList

      endif

      if produceTIFFs then begin


;         Write 2nd pass result to GeoTIFF.

          TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                     anlEndDate_YYYYMMDDHH + $
                     '_2nd_pass_difference.tif'
;                     '_KRM_pass_2_difference.tif'

          MAKE_GEOTIFF_FROM_GRID, ROTATE(zGrid, 7), $
                                  minLonOut, $
                                  maxLatOut, $
                                  lonResOut, $
                                  latResOut, $
                                  outputDir + '/' + TIFFFile, $
                                  NO_DATA_VALUE = ndv, $
                                  COMPRESS = 1

          if (updateGISProject) then begin
              origFile = outputDir + '/' + TIFFFile
              copyFile = GISProjectDir + '/' + $
                         'sfav2_' + domainLabel + '_2nd_pass_difference.tif'
              if FILE_TEST(copyFile) then begin
                  cmd = 'rm -f ' + copyFile
                  SPAWN, cmd, EXIT_STATUS = status
                  if (status ne 0) then $
                      ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
              cmd = 'cp -f ' + origFile + ' ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

      endif

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


;     Crop first-pass assimilated snowfall to output grid.

      debug_tag = 6800

      snflGridAssimInches = $
          snflGridAssimInches[extraCols:numColsAnl - extraCols - 1, $
                              extraRows:numRowsAnl - extraRows - 1]


;     Adjust first-pass assimilated snowfall with second-pass
;     assimilation field.

      ind = WHERE((snflGridAssimInches ne ndv) and (zGrid ne ndv), count)
      if (count eq 0) then STOP ; GREG 20180925

      snflGridAssim2Inches = snflGridAssimInches
      snflGridAssim2Inches[ind] = snflGridAssimInches[ind] - zGrid[ind]


;     Impose a floor on the results equal to 0.01 inches, which is how
;     this program quantifies a trace of snowfall.

      ind = WHERE((snflGridAssim2Inches ne ndv) and $
                  (snflGridAssim2Inches lt 0.01), count)
      if (count gt 0) then begin
          snflGridAssim2Inches[ind] = 0.0
          if verbose then $
              USR_MSG, 'Adjusted ' + STRCRA(count) + ' cells with a ' + $
                       '"trace" floor of 0.01".'
      endif


;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

      if verbose then begin


;         Display second pass results.

          title = 'Snowfall Analysis v2 Assim. Pass 2: ' + subTitle

          X_MAP_GEO_GRID, snflGridAssim2Inches, $
                          minLonOut, maxLonOut, minLatOut, maxLatOut, $
                          edges_snowfall, $
                          red_snowfall, grn_snowfall, blu_snowfall, $
                          1, $
                          status, $
                          NDV = ndv, $
                          /SHOW_HIGH, $
                          TITLE = title, $
                          /COLORBAR, $
                          XSIZE_TARGET = xszt, $
                          UNITS = 'inches', $
                          SHAPE_PATH_LIST = shapePathList

      endif

      if produceImages then begin


;         Write second pass snowfall to an image.

          title = 'Snowfall Analysis v2 Second Pass!C' + longSubTitle

          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_13_snowfall_2nd_pass.png'

          MAKE_LON_LAT_MAP_PNG_SFAV2, $
              snflGridAssim2Inches, $
              ndv, $
              edges_snowfall, red_snowfall, grn_snowfall, blu_snowfall, $
              lonResOut, minLonOut, maxLonOut, $
              latResOut, minLatOut, maxLatOut, $
              title, $
              'inches', $
              outputDir + '/' + PNGFile, $
              /SHOW_HIGH, $
              TICK_NAMES = tickNames_snowfall, $
              /NO_GRID, /NO_CONTINENTS, /NO_USA, $
              /BLACK_ON_WHITE, $
              MAP_SHAPE_PATH = shapePathList

      endif

      if produceTIFFs then begin


;         Write assimilated result to GeoTIFF.

          TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                     anlEndDate_YYYYMMDDHH + $
                     '_2nd_pass_result.tif'

          MAKE_GEOTIFF_FROM_GRID, ROTATE(snflGridAssim2Inches, 7), $
                                  minLonOut, $
                                  maxLatOut, $
                                  lonResOut, $
                                  latResOut, $
                                  outputDir + '/' + TIFFFile, $
                                  NO_DATA_VALUE = ndv, $
                                  COMPRESS = 1 

          if (updateGISProject) then begin
              origFile = outputDir + '/' + TIFFFile
              copyFile = GISProjectDir + '/' + $
                         'sfav2_' + domainLabel + '_2nd_pass_result.tif'
              if FILE_TEST(copyFile) then begin
                  cmd = 'rm -f ' + copyFile
                  SPAWN, cmd, EXIT_STATUS = status
                  if (status ne 0) then $
                      ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
              cmd = 'cp -f ' + origFile + ' ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

      endif


;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GRAPHICS ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


;     Update snflPtsDiag with flags for points included in
;     second pass. The first flag indicates whether the point was sent
;     in, the second flag indicates whether it passed non-ndv, domain
;     membership, and outlier tests within the kriging procedure.

      debug_tag = 6900

      apc = 0L
      for sc = 0, numSnowfall - 1 do begin
          if (apc lt assimPointsCount) then begin
              if (assimPointsInd[apc] eq sc) then begin ; point was sent
                  snflPtsDiag[sc].snowfallObsAssim2 = 1
                  if useFlag_P2[apc] then $
                      snflPtsDiag[sc].snowfallObsPassedAssim2QC = 1 $
                  else $
                      snflPtsDiag[sc].snowfallObsPassedAssim2Qc = 0
                  apc++
              endif else begin
                  snflPtsDiag[sc].snowfallObsAssim2 = 0
                  snflPtsDiag[sc].snowfallObsPassedAssim2QC = 0
              endelse
          endif else begin
              if (sc le assimPointsInd[apc - 1]) then STOP ; programming error ; GREG 20180925
              snflPtsDiag[sc].snowfallObsAssim2 = 0
              snflPtsDiag[sc].snowfallObsPassedAssim2QC = 0
          endelse
      endfor

;+
;     Impose temperature criteria on low snowfall amounts.
;-
      warmSnowMaximum = 1.0

      SFAV2_REMOVE_WARM_SNOWFALL, $
          snflGridAssim2Inches, $
          minTempGrid[extraCols:numColsAnl - extraCols - 1, $
                      extraRows:numRowsAnl - extraRows - 1], $
          aveTempGrid[extraCols:numColsAnl - extraCols - 1, $
                      extraRows:numRowsAnl - extraRows - 1], $
          x2Anl[extraCols:numColsAnl - extraCols - 1, $
                extraRows:numRowsAnl - extraRows - 1], $
          y2Anl[extraCols:numColsAnl - extraCols - 1, $
                extraRows:numRowsAnl - extraRows - 1], $
          ndv, $
          siteObs, $
          siteLon, $
          siteLat, $
          warmSnowMaximum, $
          minTempCutoff + tempSlack, $
          aveTempCutoff + tempSlack, $
          minDistToObs, $
;          maxDistToObs, $
          mPerDegLonRef, $
          mPerDegLatRef, $
          DISTANCE_PRECISION = dPrecision, $
          VERBOSE = verbose


;     Sample the second-pass assimilated snowfall at observation points,
;     using bilinear interpolation.

      h2 = MAKE_ARRAY(numSnowfall, /FLOAT, VALUE = ndv)

      i = (siteLon - minLonOut) / lonResOut - 0.5D ; note minLonOut, not minLonAnl
      j = (siteLat - minLatOut) / latResOut - 0.5D ; note minLatOut, not minLatAnl

      for k = 0, numSnowfall - 1 do begin

          ik = i[k]
          jk = j[k]
          fk = siteObs[k]

          i1 = FLOOR(ik)
          i2 = i1 + 1L
          j1 = FLOOR(jk)
          j2 = j1 + 1L

          if ((i1 lt 0L) or $
              (i2 ge numColsOut) or $
              (j1 lt 0L) or $
              (j2 ge numRowsOut)) then CONTINUE

          hll = snflGridAssim2Inches[i1, j1]
          hlr = snflGridAssim2Inches[i2, j1]
          hur = snflGridAssim2Inches[i2, j2]
          hul = snflGridAssim2Inches[i1, j2]

          if ((hll eq ndv) or $
              (hlr eq ndv) or $
              (hur eq ndv) or $
              (hul eq ndv)) then $
                  STOP $ ; should not happen for remaining in-bounds points ; GREG 20180925
;                  h2[k] = ndv $
          else $
              h2[k] = hll * (i2 - ik) * (j2 - jk) + $
                      hlr * (ik - i1) * (j2 - jk) + $
                      hur * (ik - i1) * (jk - j1) + $
                      hul * (i2 - ik) * (jk - j1)

      endfor

      ind = WHERE((siteObs ne ndv) and $
                  (h2 ne ndv), count)
      if (count eq 0) then begin
          ERR_MSG, 'ERROR: After eliminating assimilated no-data values, ' + $
                   'no points remain for analysis.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

; GREG FIX ME - YOU CAN GET H2 EQ NDV IN 2ND PASS IF OUT OF BOUNDS
      if ((count ne numSnowfall) and verbose) then begin
          USR_MSG, 'Second pass produced ' + STRCRA(numSnowfall - count) + $
                   ' out-of-bounds points.'
      endif


;     Update snflPtsDiag with the second pass value (h2), a
;     corresponding above-threshold flag, and the error category for
;     the second pass value.

      debug_tag = 7000

      for sc = 0, numSnowfall - 1 do begin

          snflPtsDiag[sc].snowfall2ndPass = h2[sc]

          if (h2[sc] eq ndv) then begin
              snflPtsDiag[sc].snowfall2ndPassAboveTrace = 0
              snflPtsDiag[sc].snowfall2ndPassCategory = ''
              CONTINUE
          endif

          snflPtsDiag[sc].snowfall2ndPassAboveTrace = $
              FIX(h2[sc] gt snflThreshold)

          case (snflPtsDiag[sc].snowfallObsAboveTrace + $
                snflPtsDiag[sc].snowfall2ndPassAboveTrace * 2) of
              0: snflPtsDiag[sc].snowfall2ndPassCategory = 'D' ; correct neg.
              1: snflPtsDiag[sc].snowfall2ndPassCategory = 'C' ; miss
              2: snflPtsDiag[sc].snowfall2ndPassCategory = 'B' ; false positive
              3: snflPtsDiag[sc].snowfall2ndPassCategory = 'A' ; hit
          endcase

      endfor


;     Look for large differences between assimilated snowfall and
;     observations that participated in the assimilation pass.
 
      exceedanceCutoff = 12.0    ; inches of difference for warning

      ind = WHERE((snflPtsDiag.snowfallObsPassedAssim2QC eq 1) and $
                  (ABS(snflPtsDiag.snowfall2ndPass - $
                       snflPtsDiag.snowfallObs) gt exceedanceCutoff), $
              count)

      if (count gt 0) then begin
          ERR_MSG, 'WARNING: second pass results differ significantly ' + $
                   'from ' + STRCRA(count) + $
                   ' observations used in the pass.'
          csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_pass_2_warning.csv'
          OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
          PRINTF, lun, 'longitude,latitude,snowfall_delta_inches'
          for k = 0, count - 1 do begin
              sc = ind[k]
              PRINTF, lun, $
                      STRCRA(STRING(siteLon[sc], FORMAT = '(F11.6)')) + ',' + $
                      STRCRA(STRING(siteLat[sc], FORMAT = '(F11.6)')) + ',' + $
                      STRCRA(h2[sc] - siteObs[sc])
          endfor
          FREE_LUN, lun
      endif


;     Index and count contingency table subsets for 2nd pass.

      abInd = WHERE((h2 ne ndv) and $
                    (h2 gt snflThreshold), abCount)     ; predicted events
      acInd = WHERE((h2 ne ndv) and $
                    (siteObs gt snflThreshold), acCount) ; observed events
      bcInd = WHERE(((siteObs gt snflThreshold) xor $
                     (h2 gt snflThreshold)) and $
                    (h2 ne ndv), bcCount)               ; errors
      abcInd = WHERE(((siteObs gt snflThreshold) or $
                      (h2 gt snflThreshold)) and $
                     (h2 ne ndv), abcCount)             ; all events
      aInd = WHERE(((siteObs gt snflThreshold) and $
                    (h2 gt snflThreshold)) and $
                   (h2 ne ndv), aCount)                 ; hits
      bInd = WHERE(((h2 gt snflThreshold) and $
                    (siteObs le snflThreshold)) and $
                   (h2 ne ndv), bCount)                 ; false positives

;+ GFKS 20180723
;     False alarm trajectory.
;-
      if ((bCount gt 0) and verbose) then begin

;+
;         Summarize false alarm trajectories.
;-
          USR_MSG, 'FAR: ' + STRCRA(FLOAT(bCount) / FLOAT(abCount)) + $
                   '; false positive (FP) vs. correct negative (CN) ' + $
                   'process summary:'
          USR_MSG, '     BG -> P1 -> P2 (background -> pass 1 -> pass 2)'
          ind = WHERE(fat[bInd] eq 0, count)
          USR_MSG, '     CN -> CN -> FP %: ' + $ ; 0 + 0 = 0
                   STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)
;; ;+
;; ;         Evaluate the neighborhoods of these points.
;; ;-
          ;; ind2 = WHERE(fat[bind] eq 0, count)
          ;; if (count gt 0) then begin
          ;;     PRINT, '    dist_to_closest   dist_to_closest_nonzero   num_nonzero_within_20_km'
          ;;     ind = bInd[ind2]
          ;;     if (MAX(siteObs[ind]) gt snflThreshold) then STOP ; PROG
          ;;     if (MIN(h2[ind]) le snflThreshold) then STOP ; PROG
          ;;     for k = 0, count - 1 do begin
          ;;         j = ind[k]
          ;;         numNeighbors = all_n_1d[j]
          ;;         if (numNeighbors eq 0) then begin
          ;;             print, '    -'
          ;;             CONTINUE
          ;;         endif
          ;;         neighborDist = all_dist_2d[0:numNeighbors - 1, j]
          ;;         neighborInd = all_k_2d[0:numNeighbors - 1, j]
          ;;         minDist = MIN(neighborDist) ; closest
          ;;         neighborVal = siteObs[neighborInd]
          ;;         nzInd = WHERE(neighborVal gt snflThreshold, nzCt)
          ;;         if (nzCt eq 0) then begin
          ;;             PRINT, '    ', minDist, ' -'
          ;;             CONTINUE
          ;;         endif
          ;;         minnzdist = MIN(neighborDist[nzInd], mnnzi)
          ;;         if (minnzdist lt minDist) then STOP ; PROG
          ;;         nzClInd = WHERE((neighborVal gt snflThreshold) and $
          ;;                         (neighborDist lt 20000.0), nzClCt)
          ;;         if (nzClCt eq 0) then begin
          ;;             PRINT, '    ', minDist, ' ', minnzdist, ' -'
          ;;             CONTINUE
          ;;         endif
          ;;         PRINT, '    ', minDist, ' ', minnzdist, ' ', nzClCt
          ;;     endfor
          ;; endif
          ind = WHERE(fat[bInd] eq 1, count)
          USR_MSG, '     FP -> CN -> FP %: ' + $ ; 1 + 0 = 1
                   STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)
          ind = WHERE(fat[bInd] eq 2, count)
          USR_MSG, '     CN -> FP -> FP %: ' + $ ; 0 + 2 = 2
                   STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)
          ind = WHERE(fat[bInd] eq 3, count)
          USR_MSG, '     FP -> FP -> FP %: ' + $ ; 1 + 2 = 3
                   STRCRA(FLOAT(count) / FLOAT(bCount) * 100.0)
          ;; ind2 = WHERE(fat[bind] eq 3, count)
          ;; if (count gt 0) then begin
          ;;     PRINT, '    dist_to_closest   dist_to_closest_nonzero   num_nonzero_within_20_km'
          ;;     ind = bInd[ind2]
          ;;     if (MAX(siteObs[ind]) gt snflThreshold) then STOP ; PROG
          ;;     if (MIN(h2[ind]) le snflThreshold) then STOP ; PROG
          ;;     for k = 0, count - 1 do begin
          ;;         j = ind[k]
          ;;         numNeighbors = all_n_1d[j]
          ;;         if (numNeighbors eq 0) then begin
          ;;             print, '    -'
          ;;             CONTINUE
          ;;         endif
          ;;         neighborDist = all_dist_2d[0:numNeighbors - 1, j]
          ;;         neighborInd = all_k_2d[0:numNeighbors - 1, j]
          ;;         minDist = MIN(neighborDist) ; closest
          ;;         neighborVal = siteObs[neighborInd]
          ;;         nzInd = WHERE(neighborVal gt snflThreshold, nzCt)
          ;;         if (nzCt eq 0) then begin
          ;;             PRINT, '    ', minDist, ' -'
          ;;             CONTINUE
          ;;         endif
          ;;         minnzdist = MIN(neighborDist[nzInd], mnnzi)
          ;;         if (minnzdist lt minDist) then STOP ; PROG
          ;;         nzClInd = WHERE((neighborVal gt snflThreshold) and $
          ;;                         (neighborDist lt 20000.0), nzClCt)
          ;;         if (nzClCt eq 0) then begin
          ;;             PRINT, '    ', minDist, ' ', minnzdist, ' -'
          ;;             CONTINUE
          ;;         endif
          ;;         PRINT, '    ', minDist, ' ', minnzdist, ' ', nzClCt
          ;;     endfor
          ;; endif

      endif
;- GFKS 20180723

;+
;     Report false alarm results to a file.
;-

      debug_tag = 7100

      falseAlarmFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                       anlEndDate_YYYYMMDDHH + $
                       '_false_alarm_report.txt'
      OPENW, lun, outputDir + '/' + falseAlarmFile, /GET_LUN
            PRINTF, lun, 'False alarms for two-pass analysis:'
      if (bCount eq 0) then begin
          if (aCount gt 0) then begin
              PRINTF, lun, 'No false alarms, ' + $
                      STRCRA(aCount) + ' hits, ' + $
                      STRCRA(acCount - aCount) + ' misses.'
          endif else begin
              PRINTF, lun, 'No snowfall predicted at observation locations.'
          endelse
      endif else begin
          PRINTF, lun, $
                  'FAR: ' + STRCRA(bCount) + ' / ' + $
                  STRCRA(abCount) + ' = ' + $
                  STRCRA(FLOAT(bCount) / FLOAT(abCount))
          PRINTF, lun, 'Temperature statistics for all FP points:'
          FPAveTemp = siteAveTemp[bInd]
          PRINTF, lun, 'FP average temperatures (cutoff value ' + $
                  STRCRA(aveTempCutoff) + ' deg C):'
          PRINTF, lun, '   min: ' + STRCRA(MIN(FPAveTemp))
          PRINTF, lun, '   max: ' + STRCRA(MAX(FPAveTemp))
          PRINTF, lun, '   mean: ' + STRCRA(MEAN(FPAveTemp))
          PRINTF, lun, '   median: ' + $
                  STRCRA(MEDIAN(FPAveTemp, /EVEN))
          FPMinTemp = siteMinTemp[bInd]
          PRINTF, lun, 'FP minimum temperatures: (cutoff value ' + $
                  STRCRA(minTempCutoff) + ' deg C):'
          PRINTF, lun, '   min: ' + STRCRA(MIN(FPMinTemp))
          PRINTF, lun, '   max: ' + STRCRA(MAX(FPMinTemp))
          PRINTF, lun, '   mean: ' + STRCRA(MEAN(FPMinTemp))
          PRINTF, lun, '   median: ' + $
                  STRCRA(MEDIAN(FPMinTemp, /EVEN))
          ind = WHERE(siteObsIsReal[bInd] eq 1, count)
          PRINTF, lun, '# of FP sites with direct observations: ' + $
                  STRCRA(count)
          ind = WHERE(siteObsIsReal[bInd] eq 0, count)
          PRINTF, lun, '# of FP sites with indirect observations: ' + $
                  STRCRA(count)
          PRINTF, lun, 'False positive (FP) vs. correct negative (CN) ' + $
                  'process summary:'
          PRINTF, lun, 'BG -> P1 -> P2 (background -> pass 1 -> pass 2)'
          ind0 = WHERE(fat[bInd] eq 0, count0)
          PRINTF, lun, 'CN -> CN -> FP %: ' + $ ; 0 + 0 = 0
                  STRCRA(FLOAT(count0) / FLOAT(bCount) * 100.0)
          ind1 = WHERE(fat[bInd] eq 1, count1)
          PRINTF, lun, 'FP -> CN -> FP %: ' + $ ; 1 + 0 = 1
                  STRCRA(FLOAT(count1) / FLOAT(bCount) * 100.0)
          ind2 = WHERE(fat[bInd] eq 2, count2)
          PRINTF, lun, 'CN -> FP -> FP %: ' + $ ; 0 + 2 = 2
                   STRCRA(FLOAT(count2) / FLOAT(bCount) * 100.0)
          ind3 = WHERE(fat[bInd] eq 3, count3)
          PRINTF, lun, 'FP -> FP -> FP %: ' + $ ; 1 + 2 = 3
                   STRCRA(FLOAT(count3) / FLOAT(bCount) * 100.0)

          if (count3 gt 0) then begin

;+
;             Additional details on FP -> FP -> FP points.
;-
              PRINTF, lun, 'Further details on FP -> FP -> FP points:'
              subInd = WHERE((siteWEASDSource[bInd[ind3]] eq -1.0) or $
                             (siteWEASDSource[bInd[ind3]] eq 0.0), $
                             subCount)
              if (subCount gt 0) then begin
                  ERR_MSG, 'ERROR: Background false positives ' + $
                           'associated with zero background WEASD; ' + $
                           'debugging needed.'
                  if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
              endif
              subInd = WHERE(siteWEASDSource[bInd[ind3]] eq 1.0, $
                             WEASD1Count)
              PRINTF, lun, '# of FP -> FP -> FP sites with WEASD in QPF: ' + $
                      STRCRA(WEASD1Count)
              subInd = WHERE(siteWEASDSource[bInd[ind3]] eq 2.0, $
                             WEASD2Count)
              PRINTF, lun, '# of FP -> FP -> FP sites with WEASD missing ' + $
                      'in QPF, added from QPE: ' + $
                      STRCRA(WEASD2Count)
              FPAveTemp = FPAveTemp[ind3]
              PRINTF, lun, 'FP -> FP -> FP average temperatures:'
              PRINTF, lun, '               min: ' + STRCRA(MIN(FPAveTemp))
              PRINTF, lun, '               max: ' + STRCRA(MAX(FPAveTemp))
              PRINTF, lun, '               mean: ' + STRCRA(MEAN(FPAveTemp))
              PRINTF, lun, '               median: ' + $
                      STRCRA(MEDIAN(FPAveTemp, /EVEN))
              FPMinTemp = FPMinTemp[ind3]
              PRINTF, lun, 'FP -> FP -> FP minimum temperatures:'
              PRINTF, lun, '               min: ' + STRCRA(MIN(FPMinTemp))
              PRINTF, lun, '               max: ' + STRCRA(MAX(FPMinTemp))
              PRINTF, lun, '               mean: ' + STRCRA(MEAN(FPMinTemp))
              PRINTF, lun, '               median: ' + $
                      STRCRA(MEDIAN(FPMinTemp, /EVEN))
              subInd = WHERE(siteObsIsReal[bInd[ind3]] eq 1, subCount)
              PRINTF, lun, '# of FP -> FP -> FP sites with ' + $
                      'direct observations: ' + $
                      STRCRA(subCount)
              subInd = WHERE(siteObsIsReal[bInd[ind3]] eq 0, subCount)
              PRINTF, lun, '# of FP -> FP -> FP sites with ' + $
                      'indirect observations: ' + $
                      STRCRA(subCount)
          endif
      endelse
      FREE_LUN, lun
      ind0 = !NULL
      ind1 = !NULL
      ind2 = !NULL
      ind3 = !NULL

;     Report on contingency inventory.

      if verbose then begin
          USR_MSG, 'After second pass:'
          USR_MSG, '  A (hits): ' + STRCRA(aCount)
          USR_MSG, '  B (false positives): ' + STRCRA(bCount)
          USR_MSG, '  C (misses): ' + STRCRA(acCount - aCount)
          USR_MSG, '  D (correct negatives): ' + STRCRA(numSnowfall - abcCount)
      endif


;     Report on bias and errors in 2nd pass result.

      debug_tag = 7200

      if verbose then USR_MSG, 'Bias/error stats for 2nd pass:'

      if (aCount gt 0) then begin

          sfav2Diag.pass_2_RMSE = $
              SQRT(TOTAL((h2[aInd] - siteObs[aInd])^2.0) / aCount)
          sfav2Diag.pass_2_POD = FLOAT(aCount) / FLOAT(acCount)

          pass2LogBias = ALOG10(h2[aInd] / siteObs[aInd])
          ;; sfav2Diag.pass_2_geom_mean_bias = $
          ;;     10.0D^MEAN(ALOG10(h2[aInd] / f[aInd]))
          sfav2Diag.pass_2_geom_mean_bias = $
              10.0D^MEAN(pass2LogBias)

          ;; sdBias = STDDEV(pass2LogBias)
          ;; b1 = 10.0D^(MEAN(pass2LogBias) - sdBias)
          ;; b2 = 10.0D^(MEAN(pass2LogBias) + sdBias)
          ;; PRINT, 'bias std. dev. range: ', b1, b2

          if verbose then begin
              USR_MSG, '- RMSE (hits): ' + STRCRA(sfav2Diag.pass_2_RMSE)
              USR_MSG, '- POD: ' + $
                       STRCRA(aCount) + ' / ' + $
                       STRCRA(acCount) + ' = ' + $
                       STRCRA(sfav2Diag.pass_2_POD)
              USR_MSG, '- Geometric mean bias: ' + $
                       STRCRA(sfav2Diag.pass_2_geom_mean_bias)
              if (aCount gt 1) then $
                  USR_MSG, '- Geometric std. dev. of bias: ' + $
                           STRCRA(10.0D^(STDDEV(pass2LogBias)))
          endif

      endif else begin

          if verbose then USR_MSG, '- no hits'

      endelse

      if (bCount gt 0) then begin

          sfav2Diag.pass_2_FAR = FLOAT(bCount) / FLOAT(abCount)

          falseAlarmMean = 0.0
          falseAlarmMean = MEAN(h2[bInd])
          falseAlarmMedian = MEDIAN(h2[bInd], /EVEN)

          if verbose then begin
              USR_MSG, '- FAR: ' + $
                       STRCRA(bCount) + ' / ' + $
                       STRCRA(abCount) + ' = ' + $
                       STRCRA(sfav2Diag.pass_2_FAR)
              USR_MSG, '       mean false alarm amount = ' + $
                       STRCRA(falseAlarmMean)
              USR_MSG, '       median false alarm amount = ' + $
                       STRCRA(falseAlarmMedian)
          endif

      endif

;+
;     Regenerate the histogram of differences to show any improvement.
;-

      debug_tag = 7300

      zTitle = 'Difference Between 2nd Pass and Obs. Snowfall'
      zUnits = 'inches'

      hMin = -5.0
      hMax = 5.0
      binSize = 0.25
      numBins = ROUND((hMax - hMin) / binSize)
      pad = 0.5D * ((binSize * numBins) - (hMax - hMin))
      hMin = hMin - pad
      hMax = hMax + pad
      hAxis = hMin + 0.5 * binSize + FINDGEN(numBins) * binSize

      z_ = h2 - siteObs

      abcFlag = (siteObs gt snflThreshold) or $
                (h2 gt snflThreshold)
      histPointsInd = WHERE(abcFlag, histPointsCount)
      if (histPointsCount eq 0) then STOP ; GREG 20180925

      zHistOld = zHist
      zHist = HISTOGRAM(z_[histPointsInd], $
                            MIN = hMin, MAX = hMax, BINSIZE = binSize)

      if verbose then begin

;+
;         Display the histogram of differences.
;-
          WSET_OR_WINDOW, 1

          PLOT, hAxis, zHist[0:numBins - 1], $
                TITLE = zTitle, XTITLE = zUnits, $
                YRANGE = [1, MAX([zHist, zHistOld])], $
                /NODATA
          OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                 LINESTYLE = 2, COLOR = 100
          OPLOT, hAxis, zHist[0:numBins - 1], PSYM = 10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Kent added code below for png file of differences          

      endif

      if produceImages then begin

          oldDevice = !D.Name
          oldFont = !P.Font

          plotFile = 'sfav2_' + domainLabel + $
                     '_' + durationStr + 'h_' + $
                     anlEndDate_YYYYMMDDHH + $
                     '_2nd_pass_obs_diff_hist'

          SET_PLOT, 'PS'
          DEVICE, FILE = outputDir + '/' + plotFile + '.ps'
          !P.Font = 1           ; TrueType
          DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT

          PLOT, hAxis, zHist[0:numBins - 1], $
                TITLE = zTitle, XTITLE = zUnits, $
                YRANGE = [1, MAX([zHist, zHistOld])], $
                /NODATA
          OPLOT, hAxis, zHistOld[0:numBins - 1], PSYM = 10, $
                 LINESTYLE = 2, COLOR = 100
          OPLOT, hAxis, zHist[0:numBins - 1], PSYM = 10

          DEVICE, /CLOSE
          SET_PLOT, oldDevice
          !P.Font = oldFont

          cmd = utilsDir + '/pstopng ' + outputDir + '/' + plotFile + '.ps'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: Failed to convert ' + $
                       outputDir + '/' + plotFile + '.ps to PNG format'
          endif else begin
              cmd = 'mogrify -trim -border 4% -bordercolor white ' + $
                    outputDir + '/' + plotFile + '.png'
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then begin
                  ERR_MSG, 'WARNING: Failed to fine-tune ' + $
                           outputDir + '/' + plotFile + '.png.'
              endif
              FILE_DELETE, outputDir + '/' + plotFile + '.ps'
          endelse

      endif


; Kent added code to produce png file of differences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;+ GF 20180731
;         Display differences after first pass (window 5) and after
;         second pass (window 1).
;-
      if verbose then begin

          X_MAP_GEO_POINTS, siteLon, $
                            siteLat, $
                            h1 - siteObs, $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_sfDiff, $
                            red_sfDiff, grn_sfDiff, blu_sfDiff, $
                            5, $
                            status, $
                            NDV = ndv, $
                            /SHOW_LOW, $
                            /SHOW_HIGH, $
                            TITLE = $
                            'Diff. Between 1st Pass and Obs. Snowfall', $
                            /COLORBAR, $
                            XSIZE_TARGET = xszt, $
                            UNITS = 'inches', $
                            SHAPE_PATH_LIST = shapePathList

          X_MAP_GEO_POINTS, siteLon, $
                            siteLat, $
                            z_, $
                            minLonAnl, maxLonAnl, minLatAnl, maxLatAnl, $
                            edges_sfDiff, $
                            red_sfDiff, grn_sfDiff, blu_sfDiff, $
                            1, $
                            status, $
                            NDV = ndv, $
                            /SHOW_LOW, $
                            /SHOW_HIGH, $
                            TITLE = $
                            'Diff. Between 2nd Pass and Obs. Snowfall', $
                            /COLORBAR, $
                            XSIZE_TARGET = xszt, $
                            UNITS = 'inches', $
                            SHAPE_PATH_LIST = shapePathList

      endif


;+
;     Now we go down the rabbit hole and generate a semivariogram
;     for snowfall differences after the second pass. The idea is
;     to see if the spatial scale of the differences becomes
;     smaller still than it was for the second pass. Perhaps we
;     need a third...
;-

      debug_tag = 7400

      diagPointsInd = LINDGEN(numSnowfall) ; similar to assimPointsInd
      plotTitle = 'Empirical Variogram of ' + zTitle + ': ' + subTitle
      vParamsSpher = !NULL
      minRangeMeters = 5.0D3    ; much reduced minimum range
      lagTolMeters = 5.0D3
      maxLagMeters_P3 = 125.0D3 ; half the maximum lag for 2nd pass

      if verbose then WSET_OR_WINDOW, 6

      GEO_SEMIVARIOGRAM, siteLon, $
                         siteLat, $
                         z_, $
                         maxLagMeters_P3, $
                         status, $
                         DISTANCE_PRECISION = dPrecision, $
                         MIN_LAG_BIN_COUNT = 6, $
                         LAG_TOLERANCE = lagTolMeters, $
                         SPHERICAL_SEMIVARIOGRAM_MIN_RANGE = $
                             minRangeMeters, $
                         SPHERICAL_SEMIVARIOGRAM_PARAMS = vParamsSpher, $
                         SPHERICAL_SEMIVARIOGRAM_RMSE = RMSESpher, $
                         SPHERICAL_SEMIVARIOGRAM_IN_RANGE_RMSE = $
                             RMSESpherInRange, $
                         LAG_OUT = lagMeters, $
                         SEMIVARIOGRAM_OUT = eVario, $ 
                         PLOT_TITLE = plotTitle, $
                         /SHORT_LAG_WEIGHT_BIAS, $
                         /SCALE_INPUT_VARIABLE, $
                         HASH = verbose, $
                         SHOW_PLOT = verbose, $
                         /VERBOSE

      if verbose then begin
          OPLOT, !X.CRange, [vParamsSpher[0], vParamsSpher[0]], $
                 LINESTYLE = 2  ; nugget
          OPLOT, [vParamsSpher[2] / 1000.0, !X.CRange[1]], $
                 [vParamsSpher[1], vParamsSpher[1]], $
                 LINESTYLE = 2  ; sill
          OPLOT, [vParamsSpher[2] / 1000.0, vParamsSpher[2] / 1000.0], $
                 [0.0, vParamsSpher[1]], LINESTYLE = 2 ; range
      endif

      if produceImages then begin

;+
;         Produce a graphic of the semivariogram.
;-
          plotTitle = 'Semivariogram: ' + zTitle + $
                      '!C' + longSubtitle + '!C '

          plotFile = 'sfav2_' + domainLabel + '_' + $
                     durationStr + 'h_' + $
                     anlEndDate_YYYYMMDDHH + $
                     '_semivariogram_3rd_pass'

          oldDevice = !D.Name
          oldFont = !P.Font
          SET_PLOT, 'PS'
          DEVICE, FILE = outputDir + '/' + plotFile + '.ps', $
                  ENCAPSULATE = 0
          !P.Font = 1           ; TrueType
          DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT
          PLOT, lagMeters / 1000.0, eVario, $
                XTITLE = 'Separation (km)', $
                YTITLE = 'Semivariance', $
                PSYM = 10, $
                TITLE = plotTitle, $
                POS = [0.1, 0.1, 0.95, 0.85]
          OPLOT, lagMeters / 1000.0, $
                 SPHERICAL_SEMIVARIOGRAM_FUNC(lagMeters, vParamsSpher), $
                 COLOR = 150
          XYOUTS, 0.6, 0.3, $
                  'Spherical semivariogram:!C' + $
                  '- nugget: ' + $
                  FORMAT_FLOAT(vParamsSpher[0]) + '!C' + $
                  '- sill: ' + $
                  FORMAT_FLOAT(vParamsSpher[1]) + '!C' + $
                  '- range: ' + $
                  FORMAT_FLOAT(vParamsSpher[2] / 1000.0) + $
                  ' km!C' + $
                  '- RMSE of fit: ' + $
                  FORMAT_FLOAT(RMSESpher) + '!C' + $
                  '- in-range RMSE of fit: ' + $
                  FORMAT_FLOAT(RMSESpherInRange), $
                  /NORMAL
          DEVICE, /CLOSE
          SET_PLOT, oldDevice
          !P.Font = oldFont
          cmd = utilsDir + $
                '/pstopng ' + outputDir + '/' + plotFile + '.ps'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: Failed to convert ' + $
                       outputDir + '/' + plotFile + '.ps to PNG format'
          endif else begin
              cmd = 'mogrify -trim -border 4% -bordercolor white ' + $
                    outputDir + '/' + plotFile + '.png'
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then begin
                  ERR_MSG, 'WARNING: Failed to fine-tune ' + $
                           outputDir + '/' + plotFile + '.png.'
              endif
              FILE_DELETE, outputDir + '/' + plotFile + '.ps'
          endelse

;+ GF 20180731
;         Write difference points after second pass to an image.
;-

          PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_14_obs_to_2nd_pass_difference.png'

          killme = snflGridAssim2Inches
          killme[*,*] = ndv

          MAKE_LON_LAT_MAP_PNG_SFAV2, $
              killme, $
              ndv, $
              edges_sfDiff, red_sfDiff, grn_sfDiff, blu_sfDiff, $
              lonResOut, minLonOut, maxLonOut, $
              latResOut, minLatout, maxLatOut, $
              zTitle + '!C' + longSubTitle, $
              zUnits, $
              outputDir + '/' + PNGFile, $
              POINT_LON = siteLon, $
              POINT_LAT = siteLat, $
              POINT_VAL = z_, $
              /SHOW_LOW, $
              /SHOW_HIGH, $
              /NO_GRID, /NO_CONTINENTS, /NO_USA, $
              /BLACK_ON_WHITE, $
              MAP_SHAPE_PATH = shapePathList

          killme = !NULL

      endif

;     Write differences to csv file.

      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_obs_diff_3.csv'
      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, $
              'longitude,latitude,snowfall_2nd_pass_error_inches,station_id'
      for sc = 0, numSnowfall - 1 do begin
          PRINTF, lun, $
                  STRCRA(STRING(siteLon[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(STRING(siteLat[sc], FORMAT = '(F11.6)')) + ',' + $
                  STRCRA(h2[sc] - siteObs[sc]) + ',' + $
;                  snowfallReport[sc].station_id
                  siteID[sc]
      endfor
      FREE_LUN, lun
      if (updateGISProject) then begin
          origFile = outputDir + '/' + csvFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_obs_diff_3.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

;+
;     Copy final result to output grid.
;-
      outputGrid = snflGridAssim2Inches

  endif else begin              ; if useRatio


;     Impose a floor on the first pass result equal to 0.01 inches,
;     which is how this program quantifies a trace of snowfall.

;+ GF 20180723
;  This already happens for the first pass. It is not needed here.
      ind = WHERE((snflGridAssimInches ne ndv) and $
                  (snflGridAssimInches lt 0.01), count)
      if (count gt 0) then begin
          snflGridAssimInches[ind] = 0.0
          if verbose then $
              USR_MSG, 'Adjusted ' + STRCRA(count) + $
                       ' cells with a "trace" floor.'
      endif else begin
          if verbose then $
              USR_MSG, '(as expected no need to get rid of < 0.01" amounts)'
      endelse
;- GF 20180723


;     Crop first-pass assimilated snowfall to output grid.

      snflGridAssimInches = $
          snflGridAssimInches[extraCols:numColsAnl - extraCols - 1, $
                              extraRows:numRowsAnl - extraRows - 1]

      outputGrid = snflGridAssimInches

  endelse


ANALYSIS_COMPLETE:


; Erase results outside the CONUS.

  debug_tag = 7500

  if (outsideDomainCount gt 0) then outputGrid[outsideDomainInd] = ndv


; Establish the "issuance" time for the results.

  issTime_Julian = SYSTIME(/JULIAN, /UTC)
  issTime_YYYYMMDDHHMMSS = JULIAN_TO_YYYYMMDDHHMMSS(issTime_Julian)
  issTime_UTC = STRMID(issTime_YYYYMMDDHHMMSS, 0, 4) + '-' + $
                STRMID(issTime_YYYYMMDDHHMMSS, 4, 2) + '-' + $
                STRMID(issTime_YYYYMMDDHHMMSS, 6, 2) + ' ' + $
                STRMID(issTime_YYYYMMDDHHMMSS, 8, 2) + ':' + $
                STRMID(issTime_YYYYMMDDHHMMSS, 10, 2) + ':' + $
                STRMID(issTime_YYYYMMDDHHMMSS, 12, 2)


; Write results to NetCDF.

  netCDFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
               anlEndDate_YYYYMMDDHH + '.nc'

  MAKE_SFAV2_NETCDF, ROTATE(outputGrid, 7), $
                     ndv, $
                     outputDir + '/' + netCDFFile, $
                     lonResOut, minLonOut, maxLonout, $
                     latResOut, minLatOut, maxLatOut, $
                     duration, $
                     'inches', $
                     anlEndDate_YYYYMMDDHH, $
                     issTime_UTC, $
                     status, $
                     SLR_CLIM_YEAR_RANGE = SLRYearRange

  if NOT(status) then begin
      ERR_MSG, 'ERROR: Failed to generate output NetCDF file.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if ISA(webOutputDir) then begin
      FILE_COPY, outputDir + '/' + netCDFFile, webOutputDir, /OVERWRITE
      if NOT(FILE_TEST(webOutputDir + '/' + netCDFFile)) then $
          ERR_MSG, 'WARNING: failed to copy ' + $
                   outputDir + '/' + netCDFFile + $
                   ' to ' + webOutputDir + '.'
  endif



; Calculate full analysis wall time.

  debug_tag = 7600

  t2Full = SYSTIME(/UTC, /SECONDS)
  sfav2Diag.analysis_wall_time = FLOAT(t2Full - t1Full)
  if verbose then $
      USR_MSG, 'Full analysis wall time: ' + $
               STRCRA(sfav2Diag.analysis_wall_time) + $
               ' seconds'


;=============;
; 6. Wrap-up. ;
;=============;


; Open a CSV file for location-by-location diagnostic data. Write
; snflPtsDiag results to it.

  ptsDiagFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_snowfall_points.csv'

  OPENW, lun, outputDir + '/' + ptsDiagFile, /GET_LUN

  for sc = 0, numSnowfall - 1 do begin

      PRINTF, lun, $
              snflPtsDiag[sc].stationID + ',' + $
              STRCRA(STRING(snflPtsDiag[sc].stationLon, $
                            FORMAT = '(F13.8)')) + ',' + $
              STRCRA(STRING(snflPtsDiag[sc].stationLat, $
                            FORMAT = '(F13.8)')) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfallObs) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfallObsAboveTrace) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfall1stGuess) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfall1stGuessAboveTrace) + ',' + $
              snflPtsDiag[sc].snowfall1stGuessCategory + ',' + $
              STRCRA(snflPtsDiag[sc].snowfallObsAssim1) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfallObsPassedAssim1QC) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfall1stPass) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfall1stPassAboveTrace) + ',' + $
              snflPtsDiag[sc].snowfall1stPassCategory + ',' + $
              STRCRA(snflPtsDiag[sc].snowfallObsAssim2) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfallObsPassedAssim2QC) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfall2ndPass) + ',' + $
              STRCRA(snflPtsDiag[sc].snowfall2ndPassAboveTrace) + ',' + $
              snflPtsDiag[sc].snowfall2ndPassCategory

  endfor

  FREE_LUN, lun


;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;

  if verbose then begin


;     Display final analysis result.

      title = 'Snowfall Analysis v2: ' + subTitle

      X_MAP_GEO_GRID, outputGrid, $
                      minLonOut, maxLonOut, minLatOut, maxLatOut, $
                      edges_snowfall, $
                      red_snowfall, grn_snowfall, blu_snowfall, $
                      0, $
                      status, $
                      NDV = ndv, $
                      /SHOW_HIGH, $
                      TITLE = title, $
                      /COLORBAR, $
                      XSIZE_TARGET = xszt, $
                      UNITS = 'inches', $
                      SHAPE_PATH_LIST = shapePathList

  endif

  if produceTIFFs then begin


;     Write final analysis result to GeoTIFF.

      TIFFFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                 anlEndDate_YYYYMMDDHH + '.tif'

      MAKE_GEOTIFF_FROM_GRID, ROTATE(outputGrid, 7), $
                              minLonOut, $
                              maxLatOut, $
                              lonResOut, $
                              latResOut, $
                              outputDir + '/' + TIFFFile, $
                              NO_DATA_VALUE = ndv, $
                              COMPRESS = 1

      if ISA(webOutputDir) then begin
          FILE_COPY, outputDir + '/' + TIFFFile, webOutputDir, /OVERWRITE
          if NOT(FILE_TEST(webOutputDir + '/' + TIFFFile)) then $
              ERR_MSG, 'WARNING: failed to copy ' + $
                       outputDir + '/' + TIFFFile + $
                       ' to ' + webOutputDir + '.'
      endif

      if (updateGISProject) then begin
          ;; cmd = 'ln -fs ' + $
          ;;       '../' + outputDir + '/' + TIFFFile + ' ' + $
          ;;       GISProjectDir + '/' + $
          ;;       'sfav2_' + domainLabel + '.tif'
          origFile = outputDir + '/' + TIFFFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif

  endif

  if produceImages then begin


;     Write final analysis result to an image.

      title = 'National Snowfall Analysis: ' + $
              STRCRA(duration) + '-hour accumulation ending ' + $
              anlEndDate_GISRS + ' UTC'

      issStr = STRCRA(numSnowfall) + ' reports; ' + $
               'issued ' + issTime_UTC + ' UTC'

      PNGFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + '.png'

      MAKE_LON_LAT_MAP_PNG_SFAV2, $
          outputGrid, $
          ndv, $
          edges_snowfall, red_snowfall, grn_snowfall, blu_snowfall, $
          lonResOut, minLonOut, maxLonOut, $
          latResOut, minLatOut, maxLatOut, $
          title + '!C!D' + issStr, $
          'inches', $
          outputDir + '/' + PNGFile, $
          /SHOW_HIGH, $
          TICK_NAMES = tickNames_snowfall, $
          /NO_GRID, /NO_CONTINENTS, /NO_USA, $
          /BLACK_ON_WHITE, $
          MAP_SHAPE_PATH = shapePathList, $
          NOAA_LOGO = resourcesDir + $
                      '/noaa_logo_trans_408x408_32_col.png', $
;          '/noaa_logo_trans_400x400.png', $
;          '/noaa_logo_3d_384x384.png', $
          /PROTOTYPE

      if ISA(webOutputDir) then begin
          FILE_COPY, outputDir + '/' + PNGFile, webOutputDir, /OVERWRITE
          if NOT(FILE_TEST(webOutputDir + '/' + PNGFile)) then $
              ERR_MSG, 'WARNING: failed to copy ' + $
                       outputDir + '/' + PNGFile + $
                       ' to ' + webOutputDir + '.'
      endif


;     Make a directory for storing copies of files with the issuance
;     time indicated.

      issDir = outputDir + '/' + STRMID(anlEndDate_YYYYMMDDHH, 0, 8)
      if NOT(FILE_TEST(issDir, /DIRECTORY)) then MKDIR_2775, issDir


;     Make a copy of the PNG image with the issuance time in the
;     filename.

      PNGFileCopy = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + '.' + $
                    issTime_YYYYMMDDHHMMSS + '.png'

      FILE_COPY, outputDir + '/' + PNGFile, $
                 issDir + '/' + PNGFileCopy

  endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvv GRAPHICS vvvvvvvvvvvvvvvvvvvvvvvvvvvvv;


; Write diagnostic results to CSV file.

  debug_tag = 7700

  sfav2DiagFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                  anlEndDate_YYYYMMDDHH + $
                  '_diagnostics.csv'


; Delete any existing diagnostic file. A safer way of doing this would
; be to write diagnostic data to a temporary file, then move the
; temporary file into place, doing any necessary deletes beforehand.

  if FILE_TEST(outputDir + '/' + sfav2DiagFile) then $
      FILE_DELETE, outputDir + '/' + sfav2DiagFile

  OPENW, lun, outputDir + '/' + sfav2DiagFile, /GET_LUN
  PRINTF, lun, 'QPE_used,QPF_source,temperature_source,num_obs,background_RMSE,background_POD,background_geom_mean_bias,background_FAR,pass_1_is_log_ratio,pass_1_semivariogram_num_obs,pass_1_semivariogram_fit_RMSE,pass_1_semivariogram_fit_RMSE_in_range,pass_1_semivariogram_nugget,pass_1_semivariogram_sill,pass_1_semivariogram_range_meters,pass_1_assim_num_obs,pass_1_assim_uses_radial_means,pass_1_mean_points_per_solver,pass_1_num_solvers,pass_1_assim_wall_time,pass_1_assim_cross_validation_RMSE,pass_1_RMSE,pass_1_POD,pass_1_geom_mean_bias,pass_1_FAR,pass_2_semivariogram_num_obs,pass_2_semivariogram_fit_RMSE,pass_2_semivariogram_fit_RMSE_in_range,pass_2_semivariogram_nugget,pass_2_semivariogram_sill,pass_2_semivariogram_range_meters,pass_2_assim_num_obs,pass_2_assim_uses_radial_means,pass_2_mean_points_per_solver,pass_2_num_solvers,pass_2_assim_wall_time,pass_2_assim_cross_validation_RMSE,pass_2_RMSE,pass_2_POD,pass_2_geom_mean_bias,pass_2_FAR,analysis_wall_time'
  PRINTF, lun, $
          STRCRA(sfav2Diag.QPE_used) + ',' + $
          sfav2Diag.QPF_source + ',' + $
          sfav2Diag.temperature_source + ',' + $
          STRCRA(sfav2Diag.num_obs) + ',' + $
          STRCRA(sfav2Diag.background_RMSE) + ',' + $
          STRCRA(sfav2Diag.background_POD) + ',' + $
          STRCRA(sfav2Diag.background_geom_mean_bias) + ',' + $
          STRCRA(sfav2Diag.background_FAR) + ',' + $
          STRCRA(sfav2Diag.pass_1_is_log_ratio) + ',' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_num_obs) + ',' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_fit_RMSE) + ',' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_fit_RMSE_in_range) + ',' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_nugget) + ',' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_sill) + ',' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_range_meters) + ',' + $
          STRCRA(sfav2Diag.pass_1_assim_num_obs) + ',' + $
          STRCRA(sfav2Diag.pass_1_assim_uses_radial_means) + ',' + $
          STRCRA(sfav2Diag.pass_1_mean_points_per_solver) + ',' + $
          STRCRA(sfav2Diag.pass_1_num_solvers) + ',' + $
          STRCRA(sfav2Diag.pass_1_assim_wall_time) + ',' + $
          STRCRA(sfav2Diag.pass_1_assim_cross_validation_RMSE) + ',' + $
          STRCRA(sfav2Diag.pass_1_RMSE) + ',' + $
          STRCRA(sfav2Diag.pass_1_POD) + ',' + $
          STRCRA(sfav2Diag.pass_1_geom_mean_bias) + ',' + $
          STRCRA(sfav2Diag.pass_1_FAR) + ',' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_num_obs) + ',' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_fit_RMSE) + ',' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_fit_RMSE_in_range) + ',' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_nugget) + ',' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_sill) + ',' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_range_meters) + ',' + $
          STRCRA(sfav2Diag.pass_2_assim_num_obs) + ',' + $
          STRCRA(sfav2Diag.pass_2_assim_uses_radial_means) + ',' + $
          STRCRA(sfav2Diag.pass_2_mean_points_per_solver) + ',' + $
          STRCRA(sfav2Diag.pass_2_num_solvers) + ',' + $
          STRCRA(sfav2Diag.pass_2_assim_wall_time) + ',' + $
          STRCRA(sfav2Diag.pass_2_assim_cross_validation_RMSE) + ',' + $
          STRCRA(sfav2Diag.pass_2_RMSE) + ',' + $
          STRCRA(sfav2Diag.pass_2_POD) + ',' + $
          STRCRA(sfav2Diag.pass_2_geom_mean_bias) + ',' + $
          STRCRA(sfav2Diag.pass_2_FAR) + ',' + $
          STRCRA(sfav2Diag.analysis_wall_time)
  FREE_LUN, lun
  FILE_CHMOD, outputDir + '/' + sfav2DiagFile, '0664'o


; Write diagnostic results to text file.

  sfav2DiagFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                  anlEndDate_YYYYMMDDHH + $
                  '_diagnostics.txt'


; Delete any existing diagnostic file. A safer way of doing this would
; be to write diagnostic data to a temporary file, then move the
; temporary file into place, doing any necessary deletes beforehand.

  if FILE_TEST(outputDir + '/' + sfav2DiagFile) then $
      FILE_DELETE, outputDir + '/' + sfav2DiagFile

  OPENW, lun, outputDir + '/' + sfav2DiagFile, /GET_LUN
  PRINTF, lun, 'QPE_used:' + STRCRA(sfav2Diag.QPE_used)
  PRINTF, lun, 'QPF_source:' + sfav2Diag.QPF_source
  PRINTF, lun, 'temperature_source:' + sfav2Diag.temperature_source
  PRINTF, lun, 'num_obs:' + STRCRA(sfav2Diag.num_obs)
  PRINTF, lun, 'background_RMSE:' + STRCRA(sfav2Diag.background_RMSE)
  PRINTF, lun, 'background_POD:' + STRCRA(sfav2Diag.background_POD)
  PRINTF, lun, 'background_geom_mean_bias:' + $
          STRCRA(sfav2Diag.background_geom_mean_bias)
  PRINTF, lun, 'background_FAR:' + STRCRA(sfav2Diag.background_FAR)
  PRINTF, lun, 'pass_1_is_log_ratio:' + $
          STRCRA(sfav2Diag.pass_1_is_log_ratio)
  PRINTF, lun, 'pass_1_semivariogram_num_obs:' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_num_obs)
  PRINTF, lun, 'pass_1_semivariogram_fit_RMSE:' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_fit_RMSE)
  PRINTF, lun, 'pass_1_semivariogram_fit_RMSE_in_range:' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_fit_RMSE_in_range)
  PRINTF, lun, 'pass_1_semivariogram_nugget:' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_nugget)
  PRINTF, lun, 'pass_1_semivariogram_sill:' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_sill)
  PRINTF, lun, 'pass_1_semivariogram_range_meters:' + $
          STRCRA(sfav2Diag.pass_1_semivariogram_range_meters)
  PRINTF, lun, 'pass_1_assim_num_obs:' + $
          STRCRA(sfav2Diag.pass_1_assim_num_obs)
  PRINTF, lun, 'pass_1_assim_uses_radial_means:' + $
          STRCRA(sfav2Diag.pass_1_assim_uses_radial_means)
  PRINTF, lun, 'pass_1_mean_points_per_solver:' + $
          STRCRA(sfav2Diag.pass_1_mean_points_per_solver)
  PRINTF, lun, 'pass_1_num_solvers:' + $
          STRCRA(sfav2Diag.pass_1_num_solvers)
  PRINTF, lun, 'pass_1_assim_wall_time:' + $
          STRCRA(sfav2Diag.pass_1_assim_wall_time)
  PRINTF, lun, 'pass_1_assim_cross_validation_RMSE:' + $
          STRCRA(sfav2Diag.pass_1_assim_cross_validation_RMSE)
  PRINTF, lun, 'pass_1_RMSE:' + STRCRA(sfav2Diag.pass_1_RMSE)
  PRINTF, lun, 'pass_1_POD:' + STRCRA(sfav2Diag.pass_1_POD)
  PRINTF, lun, 'pass_1_geom_mean_bias:' + $
          STRCRA(sfav2Diag.pass_1_geom_mean_bias)
  PRINTF, lun, 'pass_1_FAR:' + STRCRA(sfav2Diag.pass_1_FAR)
  PRINTF, lun, 'pass_2_semivariogram_num_obs:' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_num_obs)
  PRINTF, lun, 'pass_2_semivariogram_fit_RMSE:' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_fit_RMSE)
  PRINTF, lun, 'pass_2_semivariogram_fit_RMSE_in_range:' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_fit_RMSE_in_range)
  PRINTF, lun, 'pass_2_semivariogram_nugget:' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_nugget)
  PRINTF, lun, 'pass_2_semivariogram_sill:' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_sill)
  PRINTF, lun, 'pass_2_semivariogram_range_meters:' + $
          STRCRA(sfav2Diag.pass_2_semivariogram_range_meters)
  PRINTF, lun, 'pass_2_assim_num_obs:' + $
          STRCRA(sfav2Diag.pass_2_assim_num_obs)
  PRINTF, lun, 'pass_2_assim_uses_radial_means:' + $
          STRCRA(sfav2Diag.pass_2_assim_uses_radial_means)
  PRINTF, lun, 'pass_2_mean_points_per_solver:' + $
          STRCRA(sfav2Diag.pass_2_mean_points_per_solver)
  PRINTF, lun, 'pass_2_num_solvers:' + $
          STRCRA(sfav2Diag.pass_2_num_solvers)
  PRINTF, lun, 'pass_2_assim_wall_time:' + $
          STRCRA(sfav2Diag.pass_2_assim_wall_time)
  PRINTF, lun, 'pass_2_assim_cross_validation_RMSE:' + $
          STRCRA(sfav2Diag.pass_2_assim_cross_validation_RMSE)
  PRINTF, lun, 'pass_2_RMSE:' + STRCRA(sfav2Diag.pass_2_RMSE)
  PRINTF, lun, 'pass_2_POD:' + STRCRA(sfav2Diag.pass_2_POD)
  PRINTF, lun, 'pass_2_geom_mean_bias:' + $
          STRCRA(sfav2Diag.pass_2_geom_mean_bias)
  PRINTF, lun, 'pass_2_FAR:' + STRCRA(sfav2Diag.pass_2_FAR)
  PRINTF, lun, 'analysis_wall_time:' + STRCRA(sfav2Diag.analysis_wall_time)
  FREE_LUN, lun
  FILE_CHMOD, outputDir + '/' + sfav2DiagFile, '0664'o


; Write CSV files for:
;
;   1. 1st guess misses (observed value) "error_1c_obs"
;   2. 1st guess false positives (1st guess value) "error_1b_anl"
;   3. 1st pass rejects (difference) "reject_1_diff"
;   4. 1st pass misses (observed value) "error_2c_obs"
;   5. 1st pass false positives (1st pass value) "error_2b_anl"
;   6. 2nd pass rejects (difference) "reject_2_diff"
;   7. 2nd pass misses (observed value) "error_3c_obs"
;   8. 2nd pass false positives (2nd pass value) "error_3c_anl"

  debug_tag = 7800

  ind = WHERE(snflPtsDiag.snowfall1stGuessCategory eq 'C', count)
  if (count gt 0) then begin
      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_error_1c_obs.csv'
      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, 'longitude,latitude,snowfall_obs_inches,station_id'
      for ic = 0, count - 1 do begin
          k = ind[ic]
          PRINTF, lun, $
                  STRCRA(snflPtsDiag[k].stationLon) + ',' + $
                  STRCRA(snflPtsDiag[k].stationLat) + ',' + $
                  STRCRA(snflPtsDiag[k].snowfallObs) + ',' + $
                  snflPtsDiag[k].stationID
      endfor
      FREE_LUN, lun
      if (updateGISProject) then begin
          origFile = outputDir + '/' + csvFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_error_1c_obs.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif
  endif else begin
      if (updateGISProject) then begin
          cmd = 'rm -f ' + GISProjectDir + '/' + $
                'sfav2_' + domainLabel + '_error_1c_obs.csv'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
      endif
  endelse

  ind = WHERE(snflPtsDiag.snowfall1stGuessCategory eq 'B', count)
  if (count gt 0) then begin
      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_error_1b_anl.csv'
      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
      PRINTF, lun, 'longitude,latitude,snowfall_anl_inches,station_id'
      for ic = 0, count - 1 do begin
          k = ind[ic]
          PRINTF, lun, $
                  STRCRA(snflPtsDiag[k].stationLon) + ',' + $
                  STRCRA(snflPtsDiag[k].stationLat) + ',' + $
                  STRCRA(snflPtsDiag[k].snowfall1stGuess) + ',' + $
                  snflPtsDiag[k].stationID
      endfor
      FREE_LUN, lun
      if (updateGISProject) then begin
          origFile = outputDir + '/' + csvFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_error_1b_anl.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif
  endif else begin
      if (updateGISProject) then begin
          cmd = 'rm -f ' + GISProjectDir + '/' + $
                'sfav2_' + domainLabel + '_error_1b_anl.csv'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
      endif
  endelse

  ind = WHERE((snflPtsDiag.snowfallObsAssim1 eq 1) and $
              (snflPtsDiag.snowfallObsPassedAssim1QC eq 0), count)
  if (count gt 0) then begin
      csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                anlEndDate_YYYYMMDDHH + $
                '_reject_1_diff.csv'
      OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
;     PRINTF, lun, 'longitude,latitude,snowfall_delta_inches,station_id'
      if (useRatio) then $
          PRINTF, lun, $
                  'longitude,' + $
                  'latitude,' + $
                  'snowfall_log_ratio,' + $
                  'analysis_first_guess_inches,' + $
                  'snowfall_obs_inches,' + $
                  'station_id' $
      else $
          PRINTF, lun, $
                  'longitude,' + $
                  'latitude,' + $
                  'snowfall_delta_inches,' + $
                  'analysis_first_guess_inches,' + $
                  'snowfall_obs_inches,' + $
                  'station_id'
      for ic = 0, count - 1 do begin
          k = ind[ic]
          if useRatio then begin
              zVal = ALOG10(snflPtsDiag[k].snowfall1stGuess / $
                            snflPtsDiag[k].snowfallObs)
          endif else begin
              zVal = snflPtsDiag[k].snowfall1stGuess - $
                     snflPtsDiag[k].snowfallObs
          endelse
          PRINTF, lun, $
                  STRCRA(snflPtsDiag[k].stationLon) + ',' + $
                  STRCRA(snflPtsDiag[k].stationLat) + ',' + $
                  STRCRA(zVal) + ',' + $
                  STRCRA(snflPtsDiag[k].snowfall1stGuess) + ',' + $
                  STRCRA(snflPtsDiag[k].snowfallObs) + ',' + $
                  snflPtsDiag[k].stationID
      endfor
      FREE_LUN, lun
      if (updateGISProject) then begin
          origFile = outputDir + '/' + csvFile
          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_reject_1_diff.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
          cmd = 'cp -f ' + origFile + ' ' + copyFile
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
      endif
  endif else begin
      if (updateGISProject) then begin
          cmd = 'rm -f ' + GISProjectDir + '/' + $
                'sfav2_' + domainLabel + '_reject_1_diff.csv'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
      endif
  endelse

;;;;;;;;;

  if useRatio then begin ; two-pass

      ind = WHERE(snflPtsDiag.snowfall1stPassCategory eq 'C', count)
      if (count gt 0) then begin
          csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_error_2c_obs.csv'
          OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
          PRINTF, lun, 'longitude,latitude,snowfall_obs_inches,station_id'
          for ic = 0, count - 1 do begin
              k = ind[ic]
              PRINTF, lun, $
                      STRCRA(snflPtsDiag[k].stationLon) + ',' + $
                      STRCRA(snflPtsDiag[k].stationLat) + ',' + $
                      STRCRA(snflPtsDiag[k].snowfallObs) + ',' + $
                      snflPtsDiag[k].stationID
          endfor
          FREE_LUN, lun
          if (updateGISProject) then begin
              origFile = outputDir + '/' + csvFile
              copyFile = GISProjectDir + '/' + $
                         'sfav2_' + domainLabel + '_error_2c_obs.csv'
              if FILE_TEST(copyFile) then begin
                  cmd = 'rm -f ' + copyFile
                  SPAWN, cmd, EXIT_STATUS = status
                  if (status ne 0) then $
                      ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
              cmd = 'cp -f ' + origFile + ' ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
      endif else begin
          if (updateGISProject) then begin
              cmd = 'rm -f ' + GISProjectDir + '/' + $
                    'sfav2_' + domainLabel + '_error_2c_obs.csv'
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then begin
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
          endif
      endelse

;;;;;;;;

      ind = WHERE(snflPtsDiag.snowfall1stPassCategory eq 'B', count)
      if (count gt 0) then begin
          csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_error_2b_anl.csv'
          OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
          PRINTF, lun, 'longitude,latitude,snowfall_anl_inches,station_id'
          for ic = 0, count - 1 do begin
              k = ind[ic]
              PRINTF, lun, $
                      STRCRA(snflPtsDiag[k].stationLon) + ',' + $
                      STRCRA(snflPtsDiag[k].stationLat) + ',' + $
                      STRCRA(snflPtsDiag[k].snowfall1stGuess) + ',' + $
                      snflPtsDiag[k].stationID
          endfor
          FREE_LUN, lun
          if (updateGISProject) then begin
              origFile = outputDir + '/' + csvFile
              copyFile = GISProjectDir + '/' + $
                         'sfav2_' + domainLabel + '_error_2b_anl.csv'
              if FILE_TEST(copyFile) then begin
                  cmd = 'rm -f ' + copyFile
                  SPAWN, cmd, EXIT_STATUS = status
                  if (status ne 0) then $
                      ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
              cmd = 'cp -f ' + origFile + ' ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
      endif else begin
          if (updateGISProject) then begin
              cmd = 'rm -f ' + GISProjectDir + '/' + $
                    'sfav2_' + domainLabel + '_error_2b_anl.csv'
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then begin
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
          endif
      endelse

;;;;;;;;;

      ind = WHERE((snflPtsDiag.snowfallObsAssim2 eq 1) and $
                  (snflPtsDiag.snowfallObsPassedAssim2QC eq 0), count)
      if (count gt 0) then begin
          csvFile = 'sfav2_' + domainLabel + '_' + durationStr + 'h_' + $
                    anlEndDate_YYYYMMDDHH + $
                    '_reject_2_diff.csv'
          OPENW, lun, outputDir + '/' + csvFile, /GET_LUN
          zVal = snflPtsDiag[ind].snowfall1stPass - $
                 snflPtsDiag[ind].snowfallObs
;          PRINTF, lun,
;          'longitude,latitude,snowfall_delta_inches,station_id'
          PRINTF, lun, $
                  'longitude,' + $
                  'latitude,' + $
                  'snowfall_delta_inches,' + $
                  'analysis_first_pass_inches,' + $
                  'snowfall_obs_inches,' + $
                  'station_id'
          for ic = 0, count - 1 do begin
              k = ind[ic]
              PRINTF, lun, $
                      STRCRA(snflPtsDiag[k].stationLon) + ',' + $
                      STRCRA(snflPtsDiag[k].stationLat) + ',' + $
                      STRCRA(zVal[ic]) + ',' + $
                      STRCRA(snflPtsDiag[k].snowfall1stPass) + ',' + $
                      STRCRA(snflPtsDiag[k].snowfallObs) + ',' + $
                      snflPtsDiag[k].stationID
          endfor
          FREE_LUN, lun
          if (updateGISProject) then begin
              origFile = outputDir + '/' + csvFile
              copyFile = GISProjectDir + '/' + $
                         'sfav2_' + domainLabel + '_reject_2_diff.csv'
              if FILE_TEST(copyFile) then begin
                  cmd = 'rm -f ' + copyFile
                  SPAWN, cmd, EXIT_STATUS = status
                  if (status ne 0) then $
                      ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
              cmd = 'cp -f ' + origFile + ' ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif
      endif else begin
          if (updateGISProject) then begin
              cmd = 'rm -f ' + GISProjectDir + '/' + $
                    'sfav2_' + domainLabel + '_reject_2_diff.csv'
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then begin
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
              endif
          endif
      endelse

      if (updateGISProject) then begin


;         Delete any existing project files pertaining to one-pass
;         runs.

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_one_pass_difference.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_one_pass_result.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

      endif

  endif else begin

      if (updateGISProject) then begin


;         Delete any existing project files pertaining to two-pass
;         runs.

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_1st_pass_log_ratio.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_1st_pass_result.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_2nd_pass_difference.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_2nd_pass_result.tif'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_error_2c_obs.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_error_2b_anl.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

          copyFile = GISProjectDir + '/' + $
                     'sfav2_' + domainLabel + '_reject_2_diff.csv'
          if FILE_TEST(copyFile) then begin
              cmd = 'rm -f ' + copyFile
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then $
                  ERR_MSG, 'WARNING: command "' + cmd + '" failed.'
          endif

      endif

  endelse

  debug_tag = 7900

  sfaStatus = 1


  if (LMGR(/RUNTIME) and verbose) then begin
      USR_MSG, 'Analysis finished.'
      ;USR_MSG, 'Analysis finished. Press a key to exit.'
      ;move = GET_KBRD(1)
  endif


BAIL:


  weHaveBailed = 1

  if NOT(LMGR(/RUNTIME)) then begin
      if sfaStatus then begin
          USR_MSG, 'Execution finished.' & STOP
      endif else begin
          ERR_MSG, 'Execution failed.' & STOP
      endelse
  endif

  if NOT(sfaStatus) then begin
      MESSAGE, 'Exiting with error status.', /CONTINUE
      if (message ne '') then MESSAGE, message, /CONTINUE
      EXIT, STATUS = 1
  endif

  EXIT, STATUS = 0

end
