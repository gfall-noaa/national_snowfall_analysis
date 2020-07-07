PRO GET_MIN_MAX_AVE_RTMA_NBM_2M_TEMP, $ 
    AnalysisEndDate_YYYYMMDDHH, $
    DurationHours, $
    MaxMissingHours, $
    RTMADir, $                  ; location of RTMA archive
    ScratchDir, $               ; location for temporary/cache files
    NoDataValue, $              ; no data value
    minTmpGrid, $               ; minimum temperature grid
    maxTmpGrid, $               ; maximum temperature grid
    aveTmpGrid, $               ; average temperature grid
    perfect, $                  ; "perfect" flag indicating 100% good data
    RTMA_NBM_GRID_PROJ_INFO = $ ; RTMA "wexp" grid/projection structure
      RTMA_NBM_GridProjInfo, $
    NUM_MISSING_HOURS = $
      numMissingHours, $
    VERBOSE = verbose, $        ; report on decisions during execution
    NO_SAVE_FILE = $            ; do not read/write data to/from save file
      no_save_file

;+
; Get hourly Real Time Mesoscale Analysis (RTMA) 2-meter air
; temperatures (TMP) from GRIB files, and determine minimum, maximum,
; and mean values on the grid. The "NBM" refers to the 2.5 km CONUS
; grid for RTMA and the National Blend of Models (NBM), which replaced
; grid 184 on 2018-12-04 12Z. See
; `https://www.weather.gov/mdl/nbm_grib2_3
; <https://www.weather.gov/mdl/nbm_grib2_3>`
;
; :Params:
;
;     AnalysisEndDate_YYYYMMDDHH : in, required, type=STRING
;         End time of analysis period in UTC, in the form YYYYMMDDHH.
;     DurationHours : in, required, type=INT
;         Analysis duration in hours; e.g., 24 for min/max/ave grids
;         over a single day.
;     MaxMissingHours : in, required, type=INT
;         Maximum number of missing or bad hours the procedure will
;         tolerate and still calculate min/max/ave values.
;     RTMADir : in, required, type=STRING
;         The directory where Real Time Mesoscale Analysis (RTMA) data
;         are stored.
;     ScratchDir : in, required, type=STRING
;         The directory where temporary files generated by this
;         procedure and the procedures it calls are stored.
;     NoDataValue : in, required, type=FLOAT
;         The value to use for missing/no-data on all grids. Missing
;         values for input data (e.g., 9.999e20 on GRIB data produced
;         at NCEP) are replaced by this value, and missing/no-data is
;         set to this value for all outputs.
;     minTmpGrid : out, type=FLTARR(1073 x 689)
;         The minimum RTMA "2 m above ground" temperature across the
;         analysis period.
;     maxTmpGrid : out, type=FLTARR(1073 x 689)
;         The maximum RTMA "2 m above ground" temperature across the
;         analysis period.
;     aveTmpGrid : out, type=FLTARR(1073 x 689)
;         The average RTMA "2 m above ground" temperature across the
;         analysis period.
;     perfect : out, type=BYTE
;         A flag that indicates whether (1) or not (0) data was found
;         and judged to be valid for every hour of the analysis
;         period.
;
; :Keywords:
;
;     RTMA_NBM_GRID_PROJ_INFO : type=STRUCT
;         A named variable that will capture the structure describing
;         the Lambert conformal 2.5 km CONUS grid and coordinate
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
;     NUM_MISSING_HOURS : out, type=INT
;         Set this to a named variable in which the number of hours
;         missing from the min/max/ave calculation is returned.
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

  COMMON info, message

;+
; The best way to check for the success of this procedure is to do
; "if (NOT(ISA(minTmpGrid)) or NOT(ISA(maxTmpGrid)) or
; NOT(ISA(aveTmpGrid))) then STOP" or something similar in the
; caller, since all will be returned as !NULL if this procedure is
; unable to get data.
;-
  minTmpGrid = !NULL
  maxTmpGrid = !NULL
  aveTmpGrid = !NULL
  perfect = 0B

;+
; Error handler for anything the main procedure code misses. Example:
; RESTORE encounters a file that was truncated because a disk filled.
; This should be commented out during development but is helpful when
; this procedure is used in "production" once it has been judged to
; work; i.e., comment this out when debugging.
;-
  CATCH, errorStatus
  if (errorStatus ne 0) then begin
      ERR_MSG, !Error_State.Msg
      RETURN
  endif

;+
; Check arguments for correct type and valid contents.
;-
  if NOT(ISA(AnalysisEndDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Analysis end date/time argument ' + $
               'must be a STRING.'
      RETURN
  endif
  if (STRLEN(AnalysisEndDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid analysis end date/time "' + $
               AnalysisEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(AnalysisEndDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid analysis end date/time "' + $
               AnalysisEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  if (DurationHours lt 0) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      RETURN
  endif

  if (MaxMissingHours lt 0) then begin
      ERR_MSG, 'Maximum bad/missing hours must be a nonnegative integer ' + $
               'number of hours.'
      RETURN
  endif

  if (MaxMissingHours ge DurationHours) then begin
      ERR_MSG, 'Maximum bad/missing hours must be less than duration.'
      RETURN
  endif

  if NOT(ISA(RTMADir, 'STRING')) then begin
      ERR_MSG, 'Location of RTMA archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(RTMADir, /DIRECTORY)) then begin
      ERR_MSG, 'RTMA archive directory "' + RTMADir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(RTMADir, /READ)) then begin
      ERR_MSG, 'RTMA archive directory "' + RTMADir + '" not readable.'
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

  numMissingHours = 0

  if KEYWORD_SET(RTMA_NBM_GridProjInfo) then begin

;+
;     Verify the RTMA_NBM_GridProjInfo structure.
;-
      sizeRTMA_NBM_GridProjInfo = SIZE(RTMA_NBM_GridProjInfo)
      if (sizeRTMA_NBM_GridProjInfo[0] ne 1) then begin
          ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((sizeRTMA_NBM_GridProjInfo[1] ne 1) or $
          (sizeRTMA_NBM_GridProjInfo[2] ne 8)) then begin
          ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      sizeRTMA_NBM_GridProjInfo = !NULL

      structOK = 1

      tagNames = TAG_NAMES(RTMA_NBM_GridProjInfo)
      ind = WHERE(tagNames eq 'LONV', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonV" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATD', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latD" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC1', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec1" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC2', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec2" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'ERADM', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "eRadM" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LAT00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lat00" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LON00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lon00" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'DX', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dx" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'DY', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dy" tag in RTMA_NBM_GRID_PROJ_INFO.'
          structOK = 0
      endif

      if structOK then begin

          if NOT(ISA(RTMA_NBM_GridProjInfo.lonV, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lonV".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.latD, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latD".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.latSec1, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec1".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.latSec2, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec2".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.eRadM, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"eRadM".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.lat00, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lat00".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.lon00, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lon00".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing LONG element ' + $
                       '"nCols".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing LONG element ' + $
                       '"nRows".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.dx, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dx".'
              structOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo.dy, 'DOUBLE')) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dy".'
              structOK = 0
          endif

      endif

      if NOT(structOK) then begin

          ERR_MSG, 'Unexpected RTMA_NBM_GRID_PROJ_INFO structure definition.'
          RETURN

      endif

      structOK = !NULL

  endif

  savFile = 'RTMA_NBM_MIN_MAX_AVE_TMP' + $
            '_' + STRCRA(DurationHours) + 'h' + $
            '_ending_' + AnalysisEndDate_YYYYMMDDHH + '.sav'

  if (NOT(KEYWORD_SET(no_save_file)) and $
      FILE_TEST(ScratchDir + '/' + savFile)) then begin

;+
;     Get data from cache file rather than reading RTMA data
;     directly.
;-
      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Reading ' + ScratchDir + '/' + savFile

      RESTORE, ScratchDir + '/' + savFile

;+
;     Verify the contents of the save file.
;-
      if NOT(ISA(minTmpGrid)) then begin
          ERR_MSG, 'No "minTmpGrid" variable in ' + $
                   ScratchDir + '/' + savFile
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      gridSize = SIZE(minTmpGrid)
      if (gridSize[0] ne 2) then begin
          ERR_MSG, '"minTmpGrid" in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      nCols = gridSize[1]
      nRows = gridSize[2]

      if NOT(ISA(maxTmpGrid)) then begin
          ERR_MSG, 'No "maxTmpGrid" variable in ' + $
                   ScratchDir + '/' + savFile
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      gridSize = SIZE(maxTmpGrid)
      if (gridSize[0] ne 2) then begin
          ERR_MSG, '"maxTmpGrid" in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      if (gridSize[1] ne nCols) then begin
          ERR_MSG, '"maxTmpGrid" column dimension (' + $
                   STRCRA(gridSize[1]) + ') ' + $
                   'does not match that of "minTmpGrid" (' + $
                   STRCRA(nCols) + ').'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      if (gridSize[2] ne nRows) then begin
          ERR_MSG, '"maxTmpGrid" row dimension (' + $
                   STRCRA(gridSize[2]) + ') ' + $
                   'does not match that of "minTmpGrid (' + $
                   STRCRA(nRows) + ').'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      if NOT(ISA(aveTmpGrid)) then begin
          ERR_MSG, 'No "aveTmpGrid" variable in ' + $
                   ScratchDir + '/' + savFile
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      gridSize = SIZE(aveTmpGrid)
      if (gridSize[0] ne 2) then begin
          ERR_MSG, '"aveTmpGrid" in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      if (gridSize[1] ne nCols) then begin
          ERR_MSG, '"aveTmpGrid" column dimension (' + $
                   STRCRA(gridSize[1]) + ') ' + $
                   'does not match that of "minTmpGrid" (' + $
                   STRCRA(nCols) + ').'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      if (gridSize[2] ne nRows) then begin
          ERR_MSG, '"aveTmpGrid" row dimension (' + $
                   STRCRA(gridSize[2]) + ') ' + $
                   'does not match that of "minTmpGrid" (' + $
                   STRCRA(nRows) + ').'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      gridSize = !NULL

      if NOT(ISA(ndv_)) then begin
          ERR_MSG, 'Missing "ndv_" variable in ' + $
                   ScratchDir + '/' + savFile
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      if NOT(ISA(RTMA_NBM_GridProjInfo_)) then begin
          ERR_MSG, 'No RTMA NBM grid/projection info structure in ' + $
                   ScratchDir + '/' + savFile
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

;+
;     Verify the structure of RTMA_NBM_GridProjInfo_ and grid dimensions.
;-
      foo = SIZE(RTMA_NBM_GridProjInfo_)
      if (foo[0] ne 1) then begin
          ERR_MSG, 'RTMA_NBM_GridProjInfo_ structure mismatch (non-scalar) ' + $
                   'in ' + ScratchDir + '/' + savFile
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      if ((foo[1] ne 1) or (foo[2] ne 8)) then begin
          ERR_MSG, 'RTMA_NBM_GridProjInfo_ structure mismatch ' + $
                   '(not a structure) ' + $
                   'in ' + ScratchDir + '/' + savFile
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
          RETURN
      endif

      savFileOK = 1

      tagNames = TAG_NAMES(RTMA_NBM_GridProjInfo_)
      ind = WHERE(tagNames eq 'LONV', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonV" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATD', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latD" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC1', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec1" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC2', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec2" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'ERADM', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "eRadM" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LAT00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lat00" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'LON00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lon00" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'DX', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dx" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      ind = WHERE(tagNames eq 'DY', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dy" tag in "RTMA_NBM_GridProjInfo_" from ' + $
                   'IDL save file ' + ScratchDir + '/' + savFile
          savFileOK = 0
      endif

      if savFileOK then begin

          if NOT(ISA(RTMA_NBM_GridProjInfo_.lonV, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"lonV" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.latD, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"latD" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.latSec1, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"latSec1" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.latSec2, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"latSec2" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.eRadM, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"eRadM" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.lat00, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"lat00" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.lon00, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"lon00" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.nCols, 'LONG')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing LONG element ' + $
                       '"nCols" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif else begin
              if (nCols ne RTMA_NBM_GridProjInfo_.nCols) then begin
                  ERR_MSG, 'Grid columns in ' + $
                           ScratchDir + '/' + savFile + $
                           ' (' + STRCRA(nCols) + ') ' + $
                           'do not match "RTMA_NBM_GridProjInfo_" value ' + $
                           '(' + STRCRA(RTMA_NBM_GridProjInfo_.nCols) + ').'
                  savFileOK = 0
              endif
          endelse

          if NOT(ISA(RTMA_NBM_GridProjInfo_.nRows, 'LONG')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing LONG element ' + $
                       '"nRows" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif else begin
              if (nRows ne RTMA_NBM_GridProjInfo_.nRows) then begin
                  ERR_MSG, 'Grid rows in ' + $
                           ScratchDir + '/' + savFile + $
                           ' (' + STRCRA(nRows) + ') ' + $
                           'do not match "RTMA_NBM_GridProjInfo_" value ' + $
                           '(' + STRCRA(RTMA_NBM_GridProjInfo_.nRows) + ').'
                  savFileOK = 0
              endif
          endelse

          if NOT(ISA(RTMA_NBM_GridProjInfo_.dx, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"dx" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

          if NOT(ISA(RTMA_NBM_GridProjInfo_.dy, 'DOUBLE')) then begin
              ERR_MSG, '"RTMA_NBM_GridProjInfo_" missing DOUBLE element ' + $
                       '"dy" ' + $
                       'in ' + ScratchDir + '/' + savFile
              savFileOK = 0
          endif

      endif

      if (savFileOK and KEYWORD_SET(RTMA_NBM_GridProjInfo)) then begin

;+
;         Verify RTMA_NBM_GridProjInfo structure from caller against
;         RTMA_NBM_GridProjInfo_ structure from save file.
;-
          if NOT(COMPARE(RTMA_NBM_GridProjInfo.lonV, $
                         RTMA_NBM_GridProjInfo_.lonV)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "lonV" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.latD, $
                         RTMA_NBM_GridProjInfo_.latD)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "latD" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.latSec1, $
                         RTMA_NBM_GridProjInfo_.latSec1)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "latSec1" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.latSec2, $
                         RTMA_NBM_GridProjInfo_.latSec2)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "latSec2" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.eRadM, $
                         RTMA_NBM_GridProjInfo_.eRadM)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "eRadM" ' + $
                       'mismatch between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.lat00, $
                         RTMA_NBM_GridProjInfo_.lat00)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "lat00" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.lon00, $
                         RTMA_NBM_GridProjInfo_.lon00)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "lon00" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (RTMA_NBM_GridProjInfo.nCols ne $
              RTMA_NBM_GridProjInfo_.nCols) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "nCols" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if (RTMA_NBM_GridProjInfo.nRows ne $
              RTMA_NBM_GridProjInfo_.nRows) then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "nRows" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.dx, RTMA_NBM_GridProjInfo_.dx)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "dx" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

          if NOT(COMPARE(RTMA_NBM_GridProjInfo.dy, RTMA_NBM_GridProjInfo_.dy)) $
              then begin
              ERR_MSG, 'RTMA_NBM_GRID_PROJ_INFO structure "dy" mismatch ' + $
                       'between keyword and ' + $
                       ScratchDir + '/' + savFile + ' data.'
              savFileOK = 0
          endif

      endif

      if (savFileOK and NOT(KEYWORD_SET(RTMA_NBM_GridProjInfo))) then begin

;+
;         Copy RTMA_NBM_GridProjInfo structure from save file.
;-
          RTMA_NBM_GridProjInfo = RTMA_NBM_GridProjInfo_

      endif

      if NOT(savFileOK) then begin

          ERR_MSG, 'Unexpected structure/content in IDL save file ' + $
                   savFile + '; returning NULL grids.'
          minTmpGrid = !NULL
          maxTmpGrid = !NULL
          aveTmpGrid = !NULL
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
          ind = WHERE(aveTmpGrid eq ndv_, count)
          if (count gt 0) then aveTmpGrid[ind] = NoDataValue
          ind = WHERE(minTmpGrid eq ndv_, count)
          if (count gt 0) then minTmpGrid[ind] = NoDataValue
          ind = WHERE(maxTmpGrid eq ndv_, count)
          if (count gt 0) then maxTmpGrid[ind] = NoDataValue
          ind = !NULL
      endif

      perfect = 1B
      RETURN

  endif

;+
; Read RTMA data from GRIB file/s. This is really the beginning of the
; process if we do not have a SAVE file.
;-
  perfect_ = 1B ; flag to indicate data was found and good for all hours
  numMissingHours = 0
  analysisEndDate_Julian = YYYYMMDDHH_TO_JULIAN(AnalysisEndDate_YYYYMMDDHH)

  for hc = 0, DurationHours - 1 do begin

      if (numMissingHours gt MaxMissingHours) then BREAK

      hourStartDate_Julian = analysisEndDate_Julian - $
                             DOUBLE(DurationHours) / 24.0D + $
                             DOUBLE(hc) / 24.0D

      hourStartDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(hourStartDate_Julian)

      hourlyTmpGrid = !NULL

      GET_HOURLY_RTMA_NBM_2M_TEMP, $
          hourStartDate_YYYYMMDDHH, $
          RTMADir, $
          ScratchDir, $
          NoDataValue, $
          hourlyTmpGrid, $
          RTMA_NBM_GRID_PROJ_INFO = RTMA_NBM_GridProjInfo, $
          VERBOSE = KEYWORD_SET(verbose)

      if NOT(ISA(hourlyTmpGrid)) then begin
          if KEYWORD_SET(verbose) then $
              ERR_MSG, 'Failed to get RTMA NBM grid ' + $
                       '":TMP:2 m above ground:" for ' + $
                       'hour ' + hourStartDate_YYYYMMDDHH
          perfect_ = 0B ; only takes one bad/missing hour to become imperfect
          numMissingHours++
          CONTINUE
      endif

;+
;     Verify that the grids are not all NoDataValue. This is not
;     something we ever expect to occur; the index is used for the
;     temperature range check below, and as long as we have it we may
;     as well include this check.
;-
      ind = WHERE(hourlyTmpGrid ne NoDataValue, count)
      if (count eq 0) then begin
          ERR_MSG, 'RTMA temperatures for ' + hourStartDate_YYYYMMDDHH + $
                   ' are all no-data values. Skipping this hour.'
          perfect_ = 0B
          numMissingHours++
          CONTINUE
      endif

;+
;     Check temperature range. There are whacked-out results now and
;     then, at least in the 5 km RTMA data. No harm in checking this.
;-
      hourlyMinTmp = MIN(hourlyTmpGrid[ind])
      hourlyMaxTmp = MAX(hourlyTmpGrid[ind])
      if ((hourlyMinTmp lt 180.0) or (hourlyMaxTmp lt 180.0)) then begin
          if KEYWORD_SET(verbose) then $
              ERR_MSG, 'RTMA temperature grid for ' + $
                       hourStartDate_YYYYMMDDHH + $
                       ' includes temperatures below 180 K. ' + $
                       ' Skipping this hour.'
          perfect_ = 0B
          numMissingHours++
          CONTINUE
      endif

      if ((hc - numMissingHours) eq 0) then begin ; initialize

          minTmpGrid_ = hourlyTmpGrid
          maxTmpGrid_ = hourlyTmpGrid
          aveTmpGrid_ = hourlyTmpGrid

      endif else begin

          ind = WHERE(minTmpGrid_ eq NoDataValue, beforeCount)
          ind = WHERE((minTmpGrid_ eq NoDataValue) or $
                      (hourlyTmpGrid eq NoDataValue), count)
          minTmpGrid_ = minTmpGrid_ < hourlyTmpGrid
          if (count gt 0) then minTmpGrid_[ind] = NoDataValue
          if (count gt beforeCount) then $
              USR_MSG, 'WARNING: no-data values added to minimum ' + $
                       '"TMP" grid for ' + hourStartDate_YYYYMMDDHH + '.'

          ind = WHERE(maxTmpGrid_ eq NoDataValue, beforeCount)
          ind = WHERE((maxTmpGrid_ eq NoDataValue) or $
                      (hourlyTmpGrid eq NoDataValue), count)
          maxTmpGrid_ = maxTmpGrid_ > hourlyTmpGrid
          if (count gt 0) then maxTmpGrid_[ind] = NoDataValue
          if (count gt beforeCount) then $
              USR_MSG, 'WARNING: no-data values added to maximum ' + $
                       '"TMP" grid for ' + hourStartDate_YYYYMMDDHH + '.'

          ind = WHERE(aveTmpGrid_ eq NoDataValue, beforeCount)
          ind = WHERE((aveTmpGrid_ eq NoDataValue) or $
                      (hourlyTmpGrid eq NoDataValue), count)
          aveTmpGrid_ = aveTmpGrid_ + hourlyTmpGrid
          if (count gt 0) then aveTmpGrid_[ind] = NoDataValue
          if (count gt beforeCount) then $
              USR_MSG, 'WARNING: no-data values added to average ' + $
                       '"TMP" grid for ' + hourStartDate_YYYYMMDDHH + '.'

          ind = !NULL

      endelse

      ;; LOADCT, 27
      ;; WSET_OR_WINDOW, 0, $
      ;;                 XSIZE = RTMA_NBM_GridProjInfo.nCols / 3, $
      ;;                 YSIZE = RTMA_NBM_GridProjInfo.nRows / 3
      ;; ind = WHERE(aveTmpGrid_ eq NoDataValue, count)
      ;; foo = aveTmpGrid_ / (hc + 1)
      ;; if (count gt 0) then foo[ind] = NoDatavalue
      ;; ind = !NULL
      ;; REAL_TVSCL, REBIN(foo, $
      ;;                   RTMA_NBM_GridProjInfo.nCols / 3, $
      ;;                   RTMA_NBM_GridProjInfo.nRows / 3, $
      ;;                   /SAMPLE), NDV = NoDataValue

  endfor

  hourlyTmpGrid = !NULL

  if (numMissingHours gt MaxMissingHours) then begin
      if KEYWORD_SET(verbose) then $
          ERR_MSG, 'Insufficient data available to calculate ' + $
                   'temperature min./max./ave.'
      RETURN
  endif

;+
; Convert total of temeratures to an average.
;-
  ind = WHERE(aveTmpGrid_ eq NoDataValue, count)
  aveTmpGrid_ = aveTmpGrid_ / (DurationHours - numMissingHours)
  if (count gt 0) then aveTmpGrid_[ind] = NoDataValue

  minTmpGrid = TEMPORARY(minTmpGrid_)
  maxTmpGrid = TEMPORARY(maxTmpGrid_)
  aveTmpGrid = TEMPORARY(aveTmpGrid_)

  perfect = perfect_

  if (perfect and NOT(KEYWORD_SET(no_save_file))) then begin

;+
;     Create IDL save file.
;-
      RTMA_NBM_GridProjInfo_ = RTMA_NBM_GridProjInfo
      ndv_ = NoDataValue

      SAVE, minTmpGrid, maxTmpGrid, aveTmpGrid, $
            RTMA_NBM_GridProjInfo_, ndv_, $
            FILE = ScratchDir + '/' + savFile

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Minimum, maximum, and average ' + $
                   STRCRA(DurationHours) + '-hour air temperature grids ' + $
                   'saved to ' + ScratchDir + '/' + savFile

  endif

  RETURN

end
