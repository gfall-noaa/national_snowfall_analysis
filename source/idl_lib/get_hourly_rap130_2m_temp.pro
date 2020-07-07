PRO FIND_HOURLY_RAP130_2M_TEMP, CycleDate_YYYYMMDDHH, $
                                TargetFcstHour, $
                                RAPDir, $
                                Verbose, $
                                fhrtStatus, $
                                GRIBDir, $
                                GRIBFile, $
                                fcstString
;+
; Find RAP 2 meter air temperature in archived GRIB files.
;-

;+
; Initialize success/failure flag and output variables.
;-
  fhrtStatus = 0

  GRIBDir = !NULL
  GRIBFile = !NULL
  fcstString = !NULL

  cycleDate_YYYYMMDD = STRMID(CycleDate_YYYYMMDDHH, 0, 8)
  cycleDate_YYYY = STRMID(CycleDate_YYYYMMDDHH, 0, 4)
  cycleDate_MM = STRMID(CycleDate_YYYYMMDDHH, 4, 2)
  cycleDate_DD = STRMID(CycleDate_YYYYMMDDHH, 6, 2)
  cycleDate_HH = STRMID(CycleDate_YYYYMMDDHH, 8, 2)

;+
; Identify the directory from which GRIB files will be sought (applies
; to the entire process).
;-
  GRIBDir_noSubdirs = RAPDir

  GRIBDir_subdirs = RAPDir + $
                    '/' + cycleDate_YYYY + $
                    '/' + cycleDate_MM + $
                    '/' + cycleDate_DD

  cycleDate_YY = STRMID(cycleDate_YYYY, 2, 2)
  Jan1_Julian = YYYYMMDDHH_TO_JULIAN(cycleDate_YYYY + '010100')
  cycleDate_Julian = YYYYMMDDHH_TO_JULIAN(CycleDate_YYYYMMDDHH)
  dayOfYear = FIX(cycleDate_Julian - Jan1_Julian) + 1
  cycleDate_DOY = STRING(dayOfYear, FORMAT = '(I3.3)')

  GRIBFile_YYDOY_pgrb = cycleDate_YY + cycleDate_DOY + $
                        '.rap.t' + cycleDate_HH + $
                        'z.awp130pgrbf' + $
                        STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                        '.grib2'

  GRIBFile_YYYYMMDD_pgrb = 'rap.' + $
                           cycleDate_YYYYMMDD + $
                           '.t' + cycleDate_HH + $
                           'z.awp130pgrbf' + $
                           STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                           '.grib2'

  GRIBFile_YYDOY_bgrb = cycleDate_YY + cycleDate_DOY + $
                        '.rap.t' + cycleDate_HH + $
                        'z.awp130bgrbf' + $
                        STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                        '.grib2'

  GRIBFile_YYYYMMDD_bgrb = 'rap.' + $
                           cycleDate_YYYYMMDD + $
                           '.t' + cycleDate_HH + $
                           'z.awp130bgrbf' + $
                           STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                           '.grib2'

  GRIBDir = ''
  GRIBFile = ''

  if FILE_TEST(GRIBDir_subdirs, /DIRECTORY) then begin

      case 1 of

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYDOY_pgrb) : begin
              use_subdirs = 1
              use_YYDOY = 1
              use_bgrb = 0
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYDOY_pgrb
          end

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYYYMMDD_pgrb) : begin
              use_subdirs = 1
              use_YYDOY = 0
              use_bgrb = 0
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYYYMMDD_pgrb
          end

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYDOY_bgrb) : begin
              use_subdirs = 1
              use_YYDOY = 1
              use_bgrb = 1
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYDOY_bgrb
          end

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYYYMMDD_bgrb) : begin
              use_subdirs = 1
              use_YYDOY = 0
              use_bgrb = 1
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYYYMMDD_bgrb
          end

          else: begin
              GRIBDir = ''
              GRIBFile = ''
          end

      endcase

  endif

  if (FILE_TEST(GRIBDir_noSubdirs) and $
      ((GRIBDir eq '') or (GRIBFile eq ''))) then begin

      case 1 of

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYDOY_pgrb) : begin
              use_subdirs = 0
              use_YYDOY = 1
              use_bgrb = 0
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYDOY_pgrb
          end

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYYYMMDD_pgrb) : begin
              use_subdirs = 0
              use_YYDOY = 0
              use_bgrb = 0
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYYYMMDD_pgrb
          end

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYDOY_bgrb) : begin
              use_subdirs = 0
              use_YYDOY = 1
              use_bgrb = 1
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYDOY_bgrb
          end

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYYYMMDD_bgrb) : begin
              use_subdirs = 1
              use_YYDOY = 0
              use_bgrb = 1
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYYYMMDD_bgrb
          end

          else: begin
              GRIBDir = ''
              GRIBFile = ''
          end

      endcase

  endif

  if ((GRIBDir ne '') and (GRIBFile ne '') and $
      FILE_TEST(GRIBDir + '/' + GRIBFile)) then begin

;+
;     Confirm the presence of a ":TMP:2 m above ground" record in the
;     file and make sure the expected fcstString is provided.
;-
      if (TargetFcstHour eq 0) then $
          fcstString = 'anl' $
      else $
          fcstString = STRCRA(TargetFcstHour) + ' hour fcst'

;+
;     NOTE: wgrib2 exit status is not usually very helpful, since
;     wgrib2 often gives a zero status even when it encounters errors,
;     but here it is checked to at least confirm that wgrib2 was able
;     to run. The output text buffer in GRIBOut should have just one
;     element, though there are cases where the the wgrib2 command
;     matches multiple (usually identical) records. Here, if there is
;     at least one non-empty GRIB record matching our pattern it is
;     treated as a success.
;-
      matchStr = ':TMP:2 m above ground:' + fcstString + ':'
      cmd = 'wgrib2 -g2clib 0 -match "' + $
            matchStr + '" ' + GRIBDir + '/' + GRIBFile
      SPAWN, cmd, GRIBOut, EXIT_STATUS = status

      if (status ne 0) then begin
          if Verbose then $
              ERR_MSG, 'Command "' + cmd + '" returned nonzero exit status.'
          RETURN
      endif

      if (N_ELEMENTS(GRIBOut) eq 0) then begin
          if Verbose then $
              ERR_MSG, 'Command "' + cmd + '" returned no GRIB headers.'
          RETURN
      endif

      if (GRIBOut[0] eq '') then begin
          if Verbose then $
              ERR_MSG, 'Command "' + cmd + '" returned an empty GRIB header.'
          RETURN
      endif

      fhrtStatus = 1

  endif else begin

;+
;     No data found for CycleDate_YYYYMMDDHH and TargetFcstHour.
;-
      if Verbose then USR_MSG, 'No TMP record found for ' + $
                               CycleDate_YYYYMMDDHH + ' f' + $
                               STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                               '.'

      GRIBDir = !NULL
      GRIBFile = !NULL
      fcstString = !NULL

  endelse

  RETURN

end

PRO GET_HOURLY_RAP130_2M_TEMP, $
    HourStartDate_YYYYMMDDHH, $ ; the start of the hour
    TargetFcstHour, $           ; target forecast hour, usually 0
    MinSubFcstHour, $           ; minimum substitute forecast hour, usually 1
    MaxSubFcstHour, $           ; maximum substitute forecast hour, usually 6
    RAPDir, $                   ; location of RAP archive
    ScratchDir, $               ; location for temporary/cache files
    NoDataValue, $              ; no data value
    tmpGrid, $                  ; output temperature grid
    perfect, $
    RAP_GRID_PROJ_INFO = RAPGridProjInfo, $
    FORECAST_HOUR_FOUND = fcstHourFound, $
    VERBOSE = Verbose, $
    NO_SAVE_FILE = no_save_file
;+
; Get hourly Rapid Refresh (RAP) 2-meter air temperatures (TMP).
; The "130" refers to the 13 km CONUS grid for RAP. See
; `http://www.nco.ncep.noaa.gov/pmb/docs/on388/tableb.html#GRID130
; <http://www.nco.ncep.noaa.gov/pmb/docs/on388/tableb.html#GRID130>`
;
; :Params:
;
;     HourStartDate_YYYYMMDDHH : in, required, type=STRING
;         Start time of the hour in UTC, in the form YYYYMMDDHH. If we
;         want data for the 2018-07-03 12Z analysis (f00), then we
;         should set this to "2018070312".
;     TargetFcstHour : in, required, type=INT
;         The forecast hour that is sought for all data.
;     MinSubFcstHour : in, required, type=INT
;         The minimum forecast hour to substitute in place of
;         TargetFcstHour, if data for TargetFcstHour is unavailable.
;     MaxSubFcstHour : in, required, type=INT
;         The maximum forecast hour to substitute in place of
;         TargetFcstHour, if dat for TargetFcstHour is unavailable.
;     RAPDir : in, required, type=STRING
;         The directory where Rapid Refresh (RAP) forecasts are
;         stored.
;     ScratchDir : in, required, type=STRING
;         The directory where temporary files generated by this
;         procedure and the procedures it calls are stored.
;     NoDataValue : in, required, type=FLOAT
;         The value to use for missing/no-data on all grids. Missing
;         values for input data (e.g., 9.999e20 on GRIB data produced
;         at NCEP) are replaced by this value, and missing/no-data is
;         set to this value for all outputs.
;     tmpGrid : out, type=FLTARR(451 x 337)
;         The decoded RAP "TMP:2 m above ground" GRIB record.
;     perfect : out, type=BYTE
;         A flag that indicates whether (1) or not (0) data was found
;         for the TargetFcstHour.
;
; :Keywords:
;
;     RAP_GRID_PROJ_INFO : type=STRUCT
;         A named variable that will capture the structure describing
;         the RAP 130 (Lambert conformal) grid and coordinate
;         system. If this variable is undefined, it will be
;         defined. If it is already defined, it will be verified
;         against the contents of the input GRIB2 file. 
;         Structure tags:
;
;         lonV (type=DOUBLE):
;         The orientation longitude, degrees.
;
;         latD (type=DOUBLE):
;         The latitude where the grid spacing is defined, degrees.
;
;         latSec1 (type=DOUBLE):
;         The first standard parallel for the projection, degrees.
;
;         latSec2 (type=DOUBLE):
;         The second standard parallel for the projection, degrees.
;
;         eRadM (type=DOUBLE):
;         The radius of the spherical earth in the projection, meters.
;
;         lat00 (type=DOUBLE):
;         The lower left grid cell center latitude, degrees.
;
;         lon00 (type=DOUBLE):
;         The lower left grid cell center longitude, degrees.
;
;         nCols (type=LONG):
;         The number of columns in the GRIB record.
;
;         nRows (type=LONG):
;         The number of rows in the GRIB record.
;
;         dx (type=DOUBLE):
;         The grid spacing in x, meters.
;
;         dy (type=DOUBLE):
;         The grid spacing in y, meters.
;
;     FORECAST_HOUR_FOUND : out, type=INT
;         Set this to a named variable in which the forecast hour
;         located and decoded is returned.
;
;     VERBOSE : in, type=BOOLEAN
;         Activates the "verbose" option, causing this procedure to
;         produce some extra output text.
;
;     NO_SAVE_FILE : in, type=BOOLEAN
;         Deactivates the default behavior of this procedure, which
;         stores a version of its results in the ScratchDir as an IDL
;         save file, and reads its data from a save file if one is
;         found.
;-
  COMMON info, Message ; used by USR_MSG and ERR_MSG

;+
; The best way to check for the success of this procedure is to do
; "if NOT(ISA(tmpGrid) then STOP" or something similar in the caller,
; since all will be returned as !NULL if this procedure is unable to
; get data.
;-
  tmpGrid = !NULL
  perfect = 0B

;+
; Error handler for anything the main procedure code misses. Example:
; RESTORE encounters a file that was truncated because a disk filled.
; Comment this section out for debugging.
;-
  CATCH, errorStatus
  if (errorStatus ne 0) then begin
      ERR_MSG, !Error_State.Msg
      RETURN
  endif

;+
; Check arguments for correct type and valid contents.
;-
  if NOT(ISA(HourStartDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Target date/time argument must be a STRING.'
      RETURN
  endif
  if (STRLEN(HourStartDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid target date/time "' + $
               HourStartDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(HourStartDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid target date/time "' + $
               HourStartDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  if ((TargetFcstHour lt 0) or $
      (MinSubFcstHour lt 0) or $
      (MaxSubFcstHour lt 0)) then begin
      ERR_MSG, 'All forecast hour options must be nonnegative.'
      RETURN
  endif

  if (MinSubFcstHour gt MaxSubFcstHour) then begin
      ERR_MSG, 'Minimum substitute forecast hour may not be larger than ' + $
               'maximum substitute forecast hour.'
      RETURN
  endif

  if NOT(ISA(RAPDir, 'STRING')) then begin
      ERR_MSG, 'Location of RAP archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(RAPDir, /DIRECTORY)) then begin
      ERR_MSG, 'RAP archive directory "' + RAPDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(RAPDir, /READ)) then begin
      ERR_MSG, 'RAP archive directory "' + RAPDir + '" not readable.'
      RETURN
  endif

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

  if KEYWORD_SET(RAPGridProjInfo) then begin

;+
;     Verify the RAPGridProjInfo structure.
;-
      sizeRAPGridProjInfo = SIZE(RAPGridProjInfo)
      if (sizeRAPGridProjInfo[0] ne 1) then begin
          ERR_MSG, 'RAP_GRID_PROJ_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((sizeRAPGridProjInfo[1] ne 1) or $
          (sizeRAPGridProjInfo[2] ne 8)) then begin
          ERR_MSG, 'RAP_GRID_PROJ_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      sizeRAPGridProjInfo = !NULL

      structOK = 1

      tagNames = TAG_NAMES(RAPGridProjInfo)
      ind = WHERE(tagNames eq 'LONV', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonV" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATD', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latD" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC1', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec1" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC2', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec2" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'ERADM', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "eRadM" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LAT00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lat00" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LON00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lon00" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'DX', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dx" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'DY', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dy" tag in RAP_GRID_PROJ_INFO.'
          structOK = 0
      endif

      if structOK then begin

          if NOT(ISA(RAPGridProjInfo.lonV, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lonV".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.latD, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latD".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.latSec1, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec1".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.latSec2, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec2".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.eRadM, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"eRadM".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.lat00, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lat00".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.lon00, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lon00".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing LONG element ' + $
                       '"nCols".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing LONG element ' + $
                       '"nRows".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.dx, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dx".'
              structOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo.dy, 'DOUBLE')) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dy".'
              structOK = 0
          endif

      endif

      if NOT(structOK) then begin

          ERR_MSG, 'Unexpected RAP_GRID_PROJ_INFO structure definition.'
          RETURN

      endif

      structOK = !NULL

  endif

;+
; Identify IDL save file.
;-
  savFile = 'RAP_130_TMP' + $
            '_f' + STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
            '_' + HourStartDate_YYYYMMDDHH + '.sav'

  if (NOT(KEYWORD_SET(no_save_file)) and $
      (FILE_TEST(ScratchDir + '/' + savFile))) then begin

;+
;     Get data from cache file rather than reading RAP data
;     directly.
;-
      if KEYWORD_SET(Verbose) then $
          USR_MSG, 'Reading ' + ScratchDir + '/' + savFile

      RESTORE, ScratchDir + '/' + savFile

;+
;     Verify the contents of the save file.
;-
      if NOT(ISA(tmpGrid)) then begin
          ERR_MSG, 'No "tmpGrid" variable in ' + $
                   ScratchDir + '/' + savFile
          RETURN
      endif

      gridSize = SIZE(tmpGrid)
      if (gridSize[0] ne 2) then begin
          ERR_MSG, '"tmpGrid" in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          tmpGrid = !NULL
          RETURN
      endif

      nCols = gridSize[1]
      nRows = gridSize[2]
      gridSize = !NULL

      if NOT(ISA(ndv_)) then begin
          ERR_MSG, 'Missing "ndv_" variable in ' + $
                   ScratchDir + '/' + savFile
          tmpGrid = !NULL
          RETURN
      endif

      if NOT(ISA(RAPGridProjInfo_)) then begin
          ERR_MSG, 'No RAP grid/projection info structure in ' + $
                   ScratchDir + '/' + savFile
          tmpGrid = !NULL
          RETURN
      endif

;+
;     Verify the structure of RAPGridProjInfo_ and grid dimensions.
;-
      foo = SIZE(RAPGridProjInfo_)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'RAPGridProjInfo_ structure mismatch (non-scalar) ' + $
                   'in ' + ScratchDir + '/' + savFile
          tmpGrid = !NULL
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'RAPGridProjInfo_ structure mismatch ' + $
                   '(not a structure) ' + $
                   'in ' + ScratchDir + '/' + savFile
          tmpGrid = !NULL
          RETURN
      endif

      savFileOK = 1

      tagNames = TAG_NAMES(RAPGridProjInfo_)
      ind = WHERE(tagNames eq 'LONV', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonV" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATD', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latD" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC1', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec1" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC2', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec2" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'ERADM', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "eRadM" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LAT00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lat00" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LON00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lon00" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'DX', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dx" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'DY', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dy" tag in "RAPGridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      if savFileOK then begin

          if NOT(ISA(RAPGridProjInfo_.lonV, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"lonV" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.latD, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"latD" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.latSec1, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"latSec1" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.latSec2, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"latSec2" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.eRadM, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"eRadM" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.lat00, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"lat00" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.lon00, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"lon00" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.nCols, 'LONG')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing LONG element ' + $
                       '"nCols" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif else begin
              if (nCols ne RAPGridProjInfo_.nCols) then begin
                  ERR_MSG, 'Grid columns in ' + $
                           ScratchDir + '/' + savFile + $
                           ' (' + STRCRA(nCols) + ') ' + $
                           'do not match "RAPGridProjInfo_" value ' + $
                           '(' + STRCRA(RAPGridProjInfo_.nCols) + ').'
                  savFileOK = 0
              endif
          endelse

          if NOT(ISA(RAPGridProjInfo_.nRows, 'LONG')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing LONG element ' + $
                       '"nRows" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif else begin
              if (nRows ne RAPGridProjInfo_.nRows) then begin
                  ERR_MSG, 'Grid rows in ' + $
                           ScratchDir + '/' + savFile + $
                           ' (' + STRCRA(nRows) + ') ' + $
                           'do not match "RAPGridProjInfo_" value ' + $
                           '(' + STRCRA(RAPGridProjInfo_.nRows) + ').'
                  savFileOK = 0
              endif
          endelse

          if NOT(ISA(RAPGridProjInfo_.dx, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"dx" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RAPGridProjInfo_.dy, 'DOUBLE')) then begin
              ERR_MSG, '"RAPGridProjInfo_" missing DOUBLE element ' + $
                       '"dy" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

      endif

      if (savFileOK and KEYWORD_SET(RAPGridProjInfo)) then begin

;+
;         Verify RAPGridProjInfo structure from caller against
;         RAPGridProjInfo_ structure from save file.
;-
          if NOT(COMPARE(RAPGridProjInfo.lonV, RAPGridProjInfo_.lonV)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "lonV" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.latD, RAPGridProjInfo_.latD)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "latD" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.latSec1, $
                         RAPGridProjInfo_.latSec1)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "latSec1" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.latSec2, $
                         RAPGridProjInfo_.latSec2)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "latSec2" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.eRadM, $
                         RAPGridProjInfo_.eRadM)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "eRadM" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.lat00, RAPGridProjInfo_.lat00)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "lat00" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.lon00, RAPGridProjInfo_.lon00)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "lon00" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (RAPGridProjInfo.nCols ne RAPGridProjInfo_.nCols) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "nCols" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (RAPGridProjInfo.nRows ne RAPGridProjInfo_.nRows) then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "nRows" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.dx, RAPGridProjInfo_.dx)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "dx" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RAPGridProjInfo.dy, RAPGridProjInfo_.dy)) $
              then begin
              ERR_MSG, 'RAP_GRID_PROJ_INFO structure "dy" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

      endif

      if (savFileOK and NOT(KEYWORD_SET(RAPGridProjInfo))) then begin

;+
;         Copy RAPGridProjInfo structure from save file.
;-
          RAPGridProjInfo = RAPGridProjInfo_

      endif

      if NOT(savFileOK) then begin

          ERR_MSG, 'Unexpected structure/content in IDL save file ' + $
                   savFile + '; returning NULL grids.'
          tmpGrid = !NULL
          RETURN

      endif

;+
;     Verify the no-data value is consistent; modify if it is not.
;-
      if (ndv_ ne NoDataValue) then begin
          ERR_MSG, 'WARNING: value of "ndv_" variable in ' + $
                  ScratchDir + '/' + savFile + $
                  ' (' + STRCRA(ndv_) + ') differs from ' + $
                  ' argument "NoDataValue" ' + $
                  '(' + STRCRA(NoDataValue) + '). Will adapt.'
          ind = WHERE(tmpGrid eq ndv_, count)
          if (count gt 0) then tmpGrid[ind] = NoDataValue
          ind = !NULL
      endif

      perfect = 1B
      fcstHourFound = TargetFcstHour ; save file would not have been
                                     ; made otherwise

      RETURN

  endif

;+
; Read RAP data from GRIB file/s. This is really the beginning of the
; process if we do not have a SAVE file.
;-
  perfect_ = 1B ; local flag to indicate that we got our first choice.

  hourStartDate_Julian = YYYYMMDDHH_TO_JULIAN(HourStartDate_YYYYMMDDHH)

  cycleDate_Julian = hourStartDate_Julian - DOUBLE(TargetFcstHour) / 24.0D
  cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
  fcstHourFound_ = TargetFcstHour

  FIND_HOURLY_RAP130_2M_TEMP, cycleDate_YYYYMMDDHH, $
                              TargetFcstHour, $
                              RAPDir, $
                              KEYWORD_SET(Verbose), $
                              status, $
                              GRIBDir, $
                              GRIBFile, $
                              fcstString

  if NOT(status) then begin

      perfect_ = 0B

;+
;     Try a farther-out forecast.
;-
      altFcstHour = TargetFcstHour + 1

      while (altFcstHour le MaxSubFcstHour) do begin

          if (altFcstHour lt MinSubFcstHour) then begin
              altFcstHour = altFcstHour + 1
              CONTINUE
          endif

          cycleDate_Julian = hourStartDate_Julian - $
                             DOUBLE(altFcstHour) / 24.0D
          cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)

          fcstHourFound_ = altFcstHour

          FIND_HOURLY_RAP130_2M_TEMP, cycleDate_YYYYMMDDHH, $
                                      altFcstHour, $
                                      RAPDir, $
                                      KEYWORD_SET(Verbose), $
                                      status, $
                                      GRIBDir, $
                                      GRIBFile, $
                                      fcstString

          if status then BREAK ; Found something that will work.

          altFcstHour = altFcstHour + 1

      endwhile

      if ((altFcstHour le MaxSubFcstHour) and $
          KEYWORD_SET(Verbose)) then $
              USR_MSG, 'WARNING: using ' + $
                       STRCRA(altFcstHour) + $
                       '-hour forecast for ' + $
                       HourStartDate_YYYYMMDDHH + '.'

  endif

  if NOT(status) then begin

;+
;     Try a less-far-out forecast.
;-
      altFcstHour = TargetFcstHour - 1

      while (altFcstHour ge MinSubFcstHour) do begin

          if (altFcstHour gt MaxSubFcstHour) then begin
              altFcstHour = altFcstHour - 1
              CONTINUE
          endif

          cycleDate_Julian = hourStartDate_Julian - $
                             DOUBLE(altFcstHour) / 24.0D
          cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)

          fcstHourFound_ = altFcstHour

          FIND_HOURLY_RAP130_2M_TEMP, cycleDate_YYYYMMDDHH, $
                                      altFcstHour, $
                                      RAPDir, $
                                      KEYWORD_SET(Verbose), $
                                      status, $
                                      GRIBDir, $
                                      GRIBFile, $
                                      fcstString

          if status then BREAK ; Found something that will work.

          altFcstHour = altFcstHour - 1

      endwhile

      if ((altFcstHour ge MinSubFcstHour) and $
          KEYWORD_SET(Verbose)) then $
              USR_MSG, 'WARNING: using ' + $
                       STRCRA(altFcstHour) + $
                       '-hour forecast for ' + $
                       HourStartDate_YYYYMMDDHH + '.'  
  endif

  if NOT(status) then begin
      if KEYWORD_SET(Verbose) then $
          ERR_MSG, 'No data found for hourly RAP temperature for ' + $
                   HourStartDate_YYYYMMDDHH + '.'
      RETURN
  endif

;+
; Something usable was found.
;-
  if KEYWORD_SET(Verbose) then $
      USR_MSG, 'Decoding TMP from "' + fcstString + '" records ' + $
               'in ' + GRIBDir + '/' + GRIBFile

;+
; Decode TMP record.
;-
  matchStr = ':TMP:2 m above ground:' + fcstString + ':'

  DECODE_GRIB2_RECORD, GRIBDir + '/' + GRIBFile, $
                       ScratchDir, $
                       matchStr, $
                       record, vt, lev, ftime, $
                       varAbbrev, varField, varUnits, $
                       nCols_, nRows_, $
                       tmpGrid_, $
                       NO_DATA_VALUE = NoDataValue

  if NOT(ISA(tmpGrid_)) then begin
      ERR_MSG, 'Failed to decode "' + matchStr + '" record from ' + $
               GRIBDir + '/' + GRIBFile
      RETURN
  endif

;+
; Check parameters of TMP record.
;-
  if (varAbbrev ne 'TMP') then begin
      ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
               ' "' + matchStr + '" record has invalid abbreviation ' + $
               '"' + varAbbrev + '".'
      RETURN
  endif

  if (varField ne 'Temperature') then begin
      ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
               ' "' + matchStr + '" record has invalid field name ' + $
               '"' + varField + '".'
      RETURN
  endif

  if (varUnits ne 'K') then begin
      ERR_MSG, 'GRIB file ' + GRIBDir + '/' + GRIBFile + $
               ' "' + matchStr + '" record has invalid units ' + $
               '"' + varUnits + '".'
  endif

;+
; Establish grid dimensions.
;-
  nCols = nCols_
  nRows = nRows_

  if NOT(KEYWORD_SET(RAPGridProjInfo)) then begin

;+
;     Create the RAPGridProjInfo structure to store grid and
;     projection parameters, based on the "TMP" record.
;-
      GET_GRIB2_LAMBERT_CONFORMAL_GRID_INFO, $
          GRIBFile, GRIBDir, ScratchDir, $
          matchStr, $
          nCols_, nRows_, $
          lat00, lon00, lonV, latD, latSec1, latSec2, latsp, lonsp, $
          dx, dy, $
          STATUS

      if NOT(status) then begin
          ERR_MSG, 'Failed to read Lambert conformal projection ' + $
                   'parameters from GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + ' "' + matchStr + $
                   '" record.'
          RETURN
      endif

;+
;     Confirm the NCEP sphere.
;-

;+
;     1. Grid template.
;-
      cmd = 'wgrib2 -match "' + matchStr + '" -get_byte 3 13 2 ' + $
            GRIBDir + '/' + GRIBFile
      SPAWN, cmd, gridDefTemplate, EXIT_STATUS = status
      if ( status ne 0 ) then begin
          ERR_MSG, 'Failed to read grid definition section in GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + '"' + matchStr +'" record.'
          RETURN
      endif
      if ( STRMID ( gridDefTemplate[0], STRLEN ( gridDefTemplate[0] ) - 4, $
           4 ) ne '0,30' ) then begin
          ERR_MSG, 'Grid definition template number (see GRIB2 table 3.1) ' + $
                   '"' + gridDefTemplate[0] + '" not consistent with ' + $
                   ' Lambert Conformal projection.'
          RETURN
      endif

;+
;     2. Shape of the earth.
;-
      cmd = 'wgrib2 -match "' + matchStr + '" -get_byte 3 15 1 ' + $
            GRIBDir + '/' + GRIBFile
      SPAWN, cmd, shapeOfEarth, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'Failed to read Lambert conformal grid template ' + $
                   'in GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + '"' + matchStr + '" record.'
          RETURN
      endif
      if (STRMID(shapeOfEarth[0], STRLEN(shapeOfEarth[0]) - 1, 1 ) ne '6' ) $
      then begin
;         '6' = spherical, radius 6371229.0 m
          ERR_MSG, 'NCEP sphere not detected in GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + '"' + matchStr + '" record.'
          RETURN
      endif

;+
;     Verify grid size is unchanged.
;-
      if (nCols_ ne nCols) then begin
          ERR_MSG, 'Grid columns in GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + ' "' + matchStr + $
                   '" record have changed.'
          RETURN
      endif
      if (nRows_ ne nRows) then begin
          ERR_MSG, 'Grid rows in GRIB file ' + $
                   GRIBDir + '/' + GRIBFile + ' "' + matchStr + $
                   '" record have changed.'
          RETURN
      endif

;+
;     Create data structure for the RAP 130 grid and projection.
;-
      RAPGridProjInfo = $
          {lonV:    lonV, $       ; orientation longitude
           latD:    latD, $       ; latitude where dx and dy are specified
           latSec1: latSec1, $    ; first standard parallel
           latSec2: latSec2, $    ; second standard parallel
           eRadM:   6371229.0D, $ ; NCEP sphere
           lat00:   lat00, $      ; deprojected latitude of LL cell center
           lon00:   lon00, $      ; deprojected longitude of LL cell center
           nCols:   nCols, $      ; # columns
           nRows:   nRows, $      ; # rows
           dx:      dx, $         ; x resolution at latD, meters
           dy:      dy}           ; y resolution at latD, meters

  endif else begin

;+
;     Confirm that nCols and nRows from decoded GRIB2 records match
;     structure.
;-
      dimsOK = 1

      if (nCols ne RAPGridProjInfo.nCols) then begin
          ERR_MSG, 'Grid columns in decoded GRIB2 data ' + $
                   '(' + STRCRA(nCols) + ') ' + $
                   'do not match RAP_GRID_PROJ_INFO value ' + $
                   '(' + STRCRA(RAPGridProjInfo.nCols) + ').'
          dimsOK = 0
      endif

      if (nRows ne RAPGridProjInfo.nRows) then begin
          ERR_MSG, 'Grid rows in decoded GRIB2 data ' + $
                   '(' + STRCRA(nRows) + ') ' + $
                   'do not match RAP_GRID_PROJ_INFO value ' + $
                   '(' + STRCRA(RAPGridProjInfo.nRows) + ').'
          dimsOK = 0
      endif

      if NOT(dimsOK) then RETURN

  endelse

;+
; Replace temporary grid.
;-
  tmpGrid = TEMPORARY(tmpGrid_)

  perfect = TEMPORARY(perfect_)
  fcstHourFound = TEMPORARY(fcstHourFound_)

  if (perfect and NOT(KEYWORD_SET(no_save_file))) then begin

;+
;     Create IDL save file.
;-
      RAPGridProjInfo_ = RAPGridProjInfo
      ndv_ = NoDataValue

      SAVE, tmpGrid, RAPGridProjInfo_, ndv_, $
            FILE = ScratchDir + '/' + savFile

      if KEYWORD_SET(Verbose) then $
          USR_MSG, 'TMP grid saved to ' + ScratchDir + '/' + savFile

  endif

  RETURN

end
