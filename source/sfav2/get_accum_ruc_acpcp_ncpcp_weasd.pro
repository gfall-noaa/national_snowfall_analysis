PRO GET_ACCUM_RUC_ACPCP_NCPCP_WEASD, $
    AccumEndDate_YYYYMMDDHH, $  ; accumulation end date/time
    DurationHours, $            ; accumulation duration, usually 24
    TargetFcstHour, $           ; target forecast hour, usually 3
    MinSubFcstHour, $           ; minimum substitute forecast hour, usually 1
    MaxSubFcstHour, $           ; maximum substitute forecast hour, usually 6
    RUCDir, $                   ; location of RUC archive
    ScratchDir, $               ; location for temporary/cache files
    MinLonOut, $
    MaxLonOut, $
    MinLatOut, $
    MaxLatOut, $
    LonResOut, $
    LatResOut, $
    NoDataValue, $
    accumACPCPGrid, $
    accumNCPCPGrid, $
    accumWEASDGrid, $
    perfect, $
    RUC_GRID_PROJ_INFO = RUCGridProjInfo, $
    MIN_FORECAST_HOUR_FOUND = minFcstHourFound, $
    MAX_FORECAST_HOUR_FOUND = maxFcstHourFound, $
    VERBOSE = Verbose, $
    NO_SAVE_FILE = No_save_file
;+
; Get accumulated hourly Rapid Update Cycle (RUC) quantitative
; precipitation forecasts (QPF), including convective precipitation
; (ACPCP), large-scale precipitation(NCPCP), and water equivalent of
; accumulated snow depth (WEASD), from GRIB files, and
; regrid/deproject the data to longitude/latitude coordinates with the
; designated geographic domain and resolution.
;
; There are a few different conventions for storing/archiving RUC
; data that this procedure will need to handle. In the case of
; long-term archives, data are stored in directories such as:
;
;   /nwcdev/archive/RUC_archive/{YYYY}/{MM}/{DD}
;
; In the case of GRIB files handled by operations, data are stored in
; a place like
;
;   /operations/misc/gisrs_incoming/model
;
; In all cases we assume the GRIB files have names following one of
; these conventions:
;
;   ruc.{YYYYMMDD}.t{HH}z.awp130pgrbf{FF}.grib2
;   ruc.{YYYYMMDD}.t{HH}z.awp130bgrbf{FF}.grib2
;   {YY}{DOY}.ruc.t{HH}z.awp130pgrbf{FF}.grib2
;   {YY}{DOY}.ruc.t{HH}z.awp130bgrbf{FF}.grib2
;
; The biggest complication is that for older data, the ACPCP, NCPCP,
; and WEASD fields do not provide accumulations for the "last hour"
; represented by the file, but instead provide accumulations from the
; cycle time, or from the most recent multiple of 3 hours in the
; forecast. That means if you want hour 5 precip, you will need to
; take the fields from an f05 file (accumulations from 3-5 hours) and
; subtract the fields from an f04 file (accumulations from 3-4
; hours). This is not always the case, but with most older data, it
; is. Fortunately, this determination is now made fully within
; FIND_HOURLY_RUC130_ACPCP_NCPCP_WEASD, called by
; GET_HOURLY_RUC130_ACPCP_NCPCP_WEASD.
;
; :Params:
;
;     AccumEndDate_YYYYMMDDHH : in, required, type=STRING
;         End time of accumulation period in UTC, in the form
;         YYYYMMDDHH.
;     DurationHours : in, required, type=INT
;         Accumulation duration in hours; e.g., 24 for a day of
;         precipitation accumulation.
;     TargetFcstHour : in, required, type=INT
;         The forecast hour that is sought for all QPF data.
;     MinSubFcstHour : in, required, type=INT
;         The minimum forecast hour to substitute in place of
;         TargetFcstHour, if QPF for TargetFcstHour is unavailable.
;     MaxSubFcstHour : in, required, type=INT
;         The maximum forecast hour to substitute in place of
;         TargetFcstHour, if QPF for TargetFcstHour is unavailable.
;     RUCDir : in, required, type=STRING
;         The directory where Rapid Update Cycle (RUC) quantitative
;         precipitation forecasts (QPF) are stored.
;     ScratchDir : in, required, type=STRING
;         The directory where temporary files generated by this
;         procedure and the procedures it calls are stored.
;     MinLonOut : in, required, type=DOUBLE
;         The longitude of the westernmost edge of the westernmost
;         output grid column, in degrees.
;     MaxLonOut : in, required, type=DOUBLE
;         The longitude of the easternmost edge of the easternmost
;         output grid column, in degrees.
;     MinLatOut : in, required, type=DOUBLE
;         The latitude of the southernmost edge of the southernmost
;         output grid row, in degrees.
;     MaxLatOut : in, required, type=DOUBLE
;         The latitude of the northernmost edge of the northernmost
;         output grid row, in degrees.
;     LonResOut : in, required, type=DOUBLE
;         The longitudinal grid spacing in degrees.
;     LatResOut : in, required, type=DOUBLE
;         The latitudinal grid spacing in degrees.
;     NoDataValue : in, required, type=FLOAT
;         The value to use for missing/no-data on all grids. Missing
;         values for input data (e.g., 9.999e20 on GRIB data produced
;         at NCEP) are replaced by this value, and missing/no-data is
;         set to this value for all outputs.
;     accumACPCPGrid : out, type=FLTARR(nx x ny)
;         The aggregated RUC "convective precipitation".
;     accumNCPCPGrid : out, type=FLTARR(nx x ny)
;         The aggregated RUC "large scale precipitation".
;     accumWEASDGrid : out, type=FLTARR(nx x ny)
;         The aggregated RUC "water equivalent of accumulated snow
;         depth".
;     perfect : out, type=BYTE
;         A flag that indicates whether (1) or not (0) data was found
;         for the TargetFcstHour for every hour of the accumulations.
;
; :Keywords:
;
;     RUC_GRID_PROJ_INFO : type=STRUCT
;         A named variable that will capture the structure describing
;         the RUC 130 (Lambert conformal) grid and coordinate
;         system. If this variable is undefined, it will be
;         defined. If it is already defined, it will be verified
;         against the contents of all input GRIB2 files. Note that the
;         dimensions of the RUC 130 grid defined by this structure
;         will not match those of the output grids, which are on a
;         geographic (longitude/latitude) grid.
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
;     MIN_FORECAST_HOUR_FOUND : out, type=INT
;         Set this to a named variable in which the minimum forecast
;         hour contributing to the accumulation is returned.
;
;     MAX_FORECAST_HOUR_FOUND : out, type=INT
;         Set this to a named variable in which the maximum forecast
;         hour contributing to the accumulation is returned.
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
; "if (NOT(ISA(accumACPCPGrid)) or NOT(ISA(accumNCPCPGrid)) or
; NOT(ISA(accumWEASDGrid))) then STOP" or something similar in the
; caller, since all will be returned as !NULL if this procedure is
; unable to get data.
;-
  accumACPCPGrid = !NULL
  accumNCPCPGrid = !NULL
  accumWEASDGrid = !NULL

  perfect = 0B

;+
; Error handler for anything the main procedure code misses. Example:
; RESTORE encounters a file that was truncated because a disk filled.
; Comment these lines out for debugging.
;-
  CATCH, errorStatus
  if (errorStatus ne 0) then begin
      ERR_MSG, !Error_State.Msg
      RETURN
  endif

;+
; Check arguments for correct type and valid contents.
;-
  if NOT(ISA(AccumEndDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Accumulation end date/time argument ' + $
               'must be a STRING.'
      RETURN
  endif
  if (STRLEN(AccumEndDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid accumulation end date/time "' + $
               AccumEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      RETURN
  endif
  if NOT(STREGEX(AccumEndDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid accumulation end date/time "' + $
               AccumEndDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      RETURN
  endif

  if (DurationHours lt 1) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      RETURN
  endif

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

  if NOT(ISA(RUCDir, 'STRING')) then begin
      ERR_MSG, 'Location of RUC archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(RUCDir, /DIRECTORY)) then begin
      ERR_MSG, 'RUC archive directory "' + RUCDir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(RUCDir, /READ)) then begin
      ERR_MSG, 'RUC archive directory "' + RUCDir + '" not readable.'
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

  if NOT(ISA(MinLonOut, 'DOUBLE')) then $
      minLonOut_ = DOUBLE(MinLonOut) $
  else $
      minLonOut_ = MinLonOut

  if NOT(ISA(MaxLonOut, 'DOUBLE')) then $
      maxLonOut_ = DOUBLE(MaxLonOut) $
  else $
      maxLonOut_ = MaxLonOut

  if NOT(ISA(MinLatOut, 'DOUBLE')) then $
      minLatOut_ = DOUBLE(MinLatOut) $
  else $
      minLatOut_ = MinLatOut

  if NOT(ISA(MaxLatOut, 'DOUBLE')) then $
      maxLatOut_ = DOUBLE(MaxLatOut) $
  else $
      maxLatOut_ = MaxLatOut

  if NOT(ISA(LonResOut, 'DOUBLE')) then $
      lonResOut_ = DOUBLE(LonResOut) $
  else $
      lonResOut_ = LonResOut

  if NOT(ISA(LatResOut, 'DOUBLE')) then $
      latResOut_ = DOUBLE(LatResOut) $
  else $
      latResOut_ = LatResOut

  nColsOut_ = ROUND((maxLonOut_ - minLonOut_) / lonResOut_)
  nRowsOut_ = ROUND((maxLatOut_ - minLatOut_) / latResOut_)

  xErr = ABS(nColsOut_ * lonResOut_ - (maxLonOut_ - minLonOut_))
  if (xERR gt 1.0D-8) then begin
      ERR_MSG, 'Inconsistent longitudinal domain/resolution.'
      RETURN
  endif
  yErr = ABS(nRowsOut_ * latResOut_ - (maxLatOut_ - minLatOut_))
  if (yErr gt 1.0D-8) then begin
      ERR_MSG, 'Inconsistent latitudinal domain/resolution.'
      RETURN
  endif

  if NOT(ISA(NoDataValue, 'FLOAT')) then $
      ERR_MSG, 'WARNING: no-data value should be a floating point value.'

  if KEYWORD_SET(RUCGridProjInfo) then begin

;+
;     Verify the RUCGridProjInfo structure.
;-
      sizeRUCGridProjInfo = SIZE(RUCGridProjInfo)
      if (sizeRUCGridProjInfo[0] ne 1) then begin
          ERR_MSG, 'RUC_GRID_PROJ_INFO structure mismatch (non-scalar).'
          RETURN
      endif

      if ((sizeRUCGridProjInfo[1] ne 1) or $
          (sizeRUCGridProjInfo[2] ne 8)) then begin
          ERR_MSG, 'RUC_GRID_PROJ_INFO structure mismatch ' + $
                   '(not a structure).'
          RETURN
      endif

      sizeRUCGridProjInfo = !NULL

      structOK = 1

      tagNames = TAG_NAMES(RUCGridProjInfo)
      ind = WHERE(tagNames eq 'LONV', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lonV" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATD', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latD" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC1', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec1" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LATSEC2', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "latSec2" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'ERADM', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "eRadM" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LAT00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lat00" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'LON00', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "lon00" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'NCOLS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nCols" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'NROWS', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "nRows" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'DX', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dx" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      ind = WHERE(tagNames eq 'DY', count)
      if (count eq 0) then begin
          ERR_MSG, 'No "dy" tag in RUC_GRID_PROJ_INFO.'
          structOK = 0
      endif

      if structOK then begin

          if NOT(ISA(RUCGridProjInfo.lonV, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lonV".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.latD, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latD".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.latSec1, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec1".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.latSec2, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"latSec2".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.eRadM, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"eRadM".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.lat00, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lat00".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.lon00, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"lon00".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.nCols, 'LONG')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing LONG element ' + $
                       '"nCols".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.nRows, 'LONG')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing LONG element ' + $
                       '"nRows".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.dx, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dx".'
              structOK = 0
          endif

          if NOT(ISA(RUCGridProjInfo.dy, 'DOUBLE')) then begin
              ERR_MSG, 'RUC_GRID_PROJ_INFO missing DOUBLE element ' + $
                       '"dy".'
              structOK = 0
          endif

      endif

      if NOT(structOK) then begin

          ERR_MSG, 'Unexpected RUC_GRID_PROJ_INFO structure definition.'
          RETURN

      endif

      structOK = !NULL

  endif

  savFile = 'RUC_LONLAT_ACPCP_NCPCP_WEASD' + $
            '_f' + STRING(targetFcstHour, FORMAT = '(I2.2)') + $
            '_' + STRCRA(durationHours) + 'h' + $
            '_ending_' + AccumEndDate_YYYYMMDDHH + '.sav'

  if (NOT(KEYWORD_SET(No_save_file)) and $
      FILE_TEST(ScratchDir + '/' + savFile)) then begin

;+
;     Get data from cache file rather than reading RUC data
;     directly.
;-
      if KEYWORD_SET(Verbose) then $
          USR_MSG, 'Reading ' + ScratchDir + '/' + savFile

;+
;     Rename original output grid parameters. Restoring the cache
;     fill will probably replace them.
;-
      minLonOut__ = TEMPORARY(minLonOut_)
      maxLonOut__ = TEMPORARY(maxLonOut_)
      minLatOut__ = TEMPORARY(minLatOut_)
      maxLatOut__ = TEMPORARY(maxLatOut_)
      lonResOut__ = TEMPORARY(lonResOut_)
      latResOut__ = TEMPORARY(latResOut_)
      nColsOut__ = TEMPORARY(nColsOut_)
      nRowsOut__ = TEMPORARY(nRowsOut_)

      RESTORE, ScratchDir + '/' + savFile

;+
;     Verify the contents of the save file.
;-
      if NOT(ISA(accumACPCPGrid)) then begin
          ERR_MSG, 'No "accumACPCPGrid" variable in ' + $
                   ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      gridSize = SIZE(accumACPCPGrid)
      if (gridSize[0] ne 2) then begin
          ERR_MSG, '"accumACPCPGrid" in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

;+
;     Only compare grid dimensions against each other, not what is
;     expected via nColsOut__ and nRowsOut__.
;-
      nCols = gridSize[1]
      nRows = gridSize[2]

      if NOT(ISA(accumNCPCPGrid)) then begin
          ERR_MSG, 'No "accumNCPCPGrid" variable in ' + $
                   ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      gridSize = SIZE(accumNCPCPGrid)
      if (gridSize[0] ne 2) then begin
          ERR_MSG, '"accumNCPCPGrid" in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (gridSize[1] ne nCols) then begin
          ERR_MSG, '"accumNCPCPGrid" column dimension (' + $
                   STRCRA(gridSize[1]) + ') ' + $
                   'does not match that of "accumACPCPGrid" (' + $
                   STRCRA(nCols) + ').'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (gridSize[2] ne nRows) then begin
          ERR_MSG, '"accumNCPCPGrid" row dimension (' + $
                   STRCRA(gridSize[2]) + ') ' + $
                   'does not match that of "accumACPCPGrid (' + $
                   STRCRA(nRows) + ').'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if NOT(ISA(accumWEASDGrid)) then begin
          ERR_MSG, 'No "accumWEASDGrid" variable in ' + $
                   ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      gridSize = SIZE(accumWEASDGrid)
      if (gridSize[0] ne 2) then begin
          ERR_MSG, '"accumWEASDGrid" in ' + ScratchDir + '/' + saveFile + $
                   ' is not a 2-D array.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (gridSize[1] ne nCols) then begin
          ERR_MSG, '"accumWEASDGrid" column dimension (' + $
                   STRCRA(gridSize[1]) + ') ' + $
                   'does not match that of "accumACPCPGrid" (' + $
                   STRCRA(nCols) + ').'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (gridSize[2] ne nRows) then begin
          ERR_MSG, '"accumWEASDGrid" row dimension (' + $
                   STRCRA(gridSize[2]) + ') ' + $
                   'does not match that of "accumACPCPGrid" (' + $
                   STRCRA(nRows) + ').'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      gridSize = !NULL

      if NOT(ISA(ndv_)) then begin
          ERR_MSG, 'Missing "ndv_" variable in ' + $
                   ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

;+
;     When checking grid bounds, only make sure that the requested
;     grid bounds lie within the grid bounds from the save file. If
;     necessary the grids from the save file will be subsetted later.
;-
      if NOT(ISA(minLonOut_)) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' missing "minLonOut_" variable.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (minLonOut_ gt minLonOut__) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' domain mismatch ("minLonOut_").'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if NOT(ISA(maxLonOut_)) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' missing "maxLonOut_" variable.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (maxLonOut_ lt maxLonOut__) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' domain mismatch ("maxLonOut_").'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if NOT(ISA(minLatOut_)) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' missing "minLatOut_" variable.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (minLatOut_ gt minLatOut__) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' domain mismatch ("minLatOut_").'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if NOT(ISA(maxLatOut_)) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' missing "maxLatOut_" variable.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (maxLatOut_ lt maxLatOut__) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' domain mismatch ("maxLatOut_").'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if NOT(ISA(lonResOut_)) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' missing "lonResOut_" variable.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (lonResOut_ ne lonResOut__) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' "lonResOut_" variable mismatch.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if NOT(ISA(latResOut_)) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' missing "latResOut_" variable.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if (latResOut_ ne latResOut__) then begin
          ERR_MSG, 'Cache file ' + ScratchDir + '/' + savFile + $
                   ' "latResOut_" variable mismatch.'
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

;+
;     Verify that the grids from the save file are internally
;     consistent with their stated domain/resolution.
;-
      nColsOut_ = ROUND((maxLonOut_ - minLonOut_) / lonResOut_)
      xErr = ABS(nColsOut_ * lonResOut_ - (maxLonOut_ - minLonOut_))
      if (xERR gt 1.0D-8) then begin
          ERR_MSG, 'Inconsistent longitudinal domain/resolution in ' + $
                   'cache file ' + ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif
      if (nColsOut_ ne nCols) then begin
          ERR_MSG, 'Longitudinal domain size and grid size mismatch in ' + $
                   'cache file ' + ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      nRowsOut_ = ROUND((maxLatOut_ - minLatOut_) / latResOut_)
      yErr = ABS(nRowsOut_ * latResOut_ - (maxLatOut_ - minLatOut_))
      if (yErr gt 1.0D-8) then begin
          ERR_MSG, 'Inconsistent latitudinal domain/resolution in ' + $
          'cache file ' + ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif
      if (nRowsOut_ ne nRows) then begin
          ERR_MSG, 'Longitudinal domain size and grid size mismatch in ' + $
                   'cache file ' + ScratchDir + '/' + savFile
          accumACPCPGrid = !NULL
          accumNCPCPGrid = !NULL
          accumWEASDGrid = !NULL
          RETURN
      endif

      if ((minLonOut__ ne minLonOut_) or $
          (maxLonOut__ ne maxLonOut_) or $
          (minLatOut__ ne minLatOut_) or $
          (maxLatOut__ ne maxLatOut_)) then begin

;+
;         Get subgrids to match requested bounds. Note that the
;         domains were tested for compatibility (overlap) already.
;-
          numColsLeft = (minLonOut__ - minLonOut_) / lonResOut_
          if (ABS(DOUBLE(ROUND(numColsLeft)) - numColsLeft) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested minimum longitude ' + $
                       'requires a non-integer crop.'
          endif
          numColsLeft = ROUND(numColsLeft)

          numColsRight = (maxLonOut_ - maxLonOut__) / lonResOut_
          if (ABS(DOUBLE(ROUND(numColsRight)) - numColsRight) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested maximum longitude ' + $
                       'requires a non-integer crop.'
          endif
          numColsRight = ROUND(numColsRight)

          numRowsBot = (minLatOut__ - minLatOut_) / latResout_
          if (ABS(DOUBLE(ROUND(numRowsBot)) - numRowsBot) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested minimum latitude ' + $
                       'requires a non-integer crop.'
          endif
          numRowsBot = ROUND(numRowsBot)

          numRowsTop = (maxLatOut_ - maxLatOut__) / latResOut_
          if (ABS(DOUBLE(ROUND(numRowsTop)) - numRowsTop) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested maximum latitude ' + $
                       'requires a non-integer crop.'
          endif
          numRowsTop = ROUND(numRowsTop)

          err = 0

          subgridCols = nColsOut_ - numColsLeft - numColsRight
          if (subgridCols lt 0) then begin
              ERR_MSG, 'Requested longitude bounds result in empty grid.'
              err = 1
          endif

          subgridRows = nRowsOut_ - numRowsBot - numRowsTop
          if (subgridRows lt 0) then begin
              ERR_MSG, 'Requested latitude bounds result in empty grid.'
              err = 1
          endif

          if (subgridCols ne nColsOut__) then STOP ; PROGRAMMING ERROR
          if (subgridRows ne nRowsOut__) then STOP ; PROGRAMMING ERROR

          if err then begin
              if NOT(LMGR(/RUNTIME)) then STOP else begin
                  accumACPCPGrid = !NULL
                  accumNCPCPGrid = !NULL
                  accumWEASDGrid = !NULL
                  RETURN
              endelse
          endif

          accumACPCPGrid = accumACPCPGrid[numColsLeft: $
                                          nColsOut_ - numColsRight - 1L, $
                                          numRowsBot: $
                                          nRowsOut_ - numRowsTop - 1L]
          accumNCPCPGrid = accumNCPCPGrid[numColsLeft: $
                                          nColsOut_ - numColsRight - 1L, $
                                          numRowsBot: $
                                          nRowsOut_ - numRowsTop - 1L]
          accumWEASDGrid = accumWEASDGrid[numColsLeft: $
                                          nColsOut_ - numColsRight - 1L, $
                                          numRowsBot: $
                                          nRowsOut_ - numRowsTop - 1L]

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
          ind = WHERE(accumACPCPGrid eq ndv_, count)
          if (count gt 0) then accumACPCPGrid[ind] = NoDataValue
          ind = WHERE(accumNCPCPGrid eq ndv_, count)
          if (count gt 0) then accumNCPCPGrid[ind] = NoDataValue
          ind = WHERE(accumWEASDGrid eq ndv_, count)
          if (count gt 0) then accumWEASDGrid[ind] = NoDataValue
          ind = !NULL
      endif

      perfect = 1B
      minFcstHourFound = TargetFcstHour ; save file would not have been
      maxFcstHourFound = TargetFcstHour ; made otherwise

      RETURN

  endif

;+
; Read RUC data from GRIB file/s and regrid. This is really the
; beginning of the process if we do not have a SAVE file.
;-
  accumACPCPGrid130 = !NULL
  accumNCPCPGrid130 = !NULL
  accumWEASDGrid130 = !NULL

  GET_ACCUM_RUC130_ACPCP_NCPCP_WEASD, $
      AccumEndDate_YYYYMMDDHH, $
      DurationHours, $
      TargetFcstHour, $
      MinSubFcstHour, $
      MaxSubFcstHour, $
      RUCDir, $
      ScratchDir, $
      NoDataValue, $
      accumACPCPGrid130, $
      accumNCPCPGrid130, $
      accumWEASDGrid130, $
      perfect_, $
      RUC_GRID_PROJ_INFO = RUCGridProjInfo, $
      MIN_FORECAST_HOUR_FOUND = minFcstHourFound_, $
      MAX_FORECAST_HOUR_FOUND = maxFcstHourFound_, $
      VERBOSE = KEYWORD_SET(Verbose), $
      NO_SAVE_FILE = KEYWORD_SET(No_save_file)

  if (NOT(ISA(accumACPCPGrid130)) or $
      NOT(ISA(accumNCPCPGrid130)) or $
      NOT(ISA(accumWEASDGrid130))) then begin
      if KEYWORD_SET(Verbose) then $
          ERR_MSG, 'Failed to get RUC precipitation for ' + $
                   STRCRA(DurationHours) + ' hours ending ' + $
                   AccumEndDate_YYYYMMDDHH
      RETURN
  endif

;+
; Calculate rows and columns of input grid that define output grid
; relative to input grid.
;-
  lonAxis = minLonOut_ + (0.5D + DINDGEN(nColsOut_)) * lonResOut_
  latAxis = minLatOut_ + (0.5D + DINDGEN(nRowsOut_)) * latResOut_
  lonGrid = lonAxis # REPLICATE(1.0D, nRowsOut_)
  latGrid = REPLICATE(1.0D, nColsOut_) # latAxis

  LCC_GRIB2_TO_SNYDER, RUCGridProjInfo.latSec1, $
                       RUCGridProjInfo.latSec2, $
                       RUCGridProjInfo.latD, $
                       RUCGridProjInfo.lonV, $
                       RUCGridProjInfo.lat00, $
                       RUCGridProjInfo.lon00, $
                       RUCGridProjInfo.eRadM, $
                       lonV_rad, $
                       nSny, $
                       FSny, $
                       rho0, $
                       x00, $
                       y00

  degToRad = !DPi / 180.0D
  rho = RUCGridProjInfo.eRadM * FSny / $
        (TAN(!DPi / 4.0D + latGrid * degToRad / 2.0D))^nSny ; Snyder 15-1
  theta_rad = nSny * (lonGrid * degToRad - lonV_rad)
  x = rho * SIN(theta_rad)        ; x location on RUC130 grid
  y = rho0 - rho * COS(theta_rad) ; y location on RUC130 grid

  rho = !NULL
  theta_rad = !NULL

  iGrid_RUC = (x - x00) / RUCGridProjInfo.dx
  jGrid_RUC = (y - y00) / RUCGridProjInfo.dy

  x = !NULL
  y = !NULL

;+
; Perform regridding.
;-
  accumACPCPGrid_ = REGRID_BILIN(accumACPCPGrid130, $
                                 iGrid_RUC, $
                                 jGrid_RUC, $
                                 NoDataValue)
  if NOT(ISA(accumACPCPGrid_)) then RETURN
  accumNCPCPGrid_ = REGRID_BILIN(accumNCPCPGrid130, $
                                 iGrid_RUC, $
                                 jGrid_RUC, $
                                 NoDataValue)
  if NOT(ISA(accumNCPCPGrid_)) then RETURN
  accumWEASDGrid_ = REGRID_BILIN(accumWEASDGrid130, $
                                 iGrid_RUC, $
                                 jGrid_RUC, $
                                 NoDataValue)
  if NOT(ISA(accumWEASDGrid_)) then RETURN

  accumACPCPGrid = TEMPORARY(accumACPCPGrid_)
  accumNCPCPGrid = TEMPORARY(accumNCPCPGrid_)
  accumWEASDGrid = TEMPORARY(accumWEASDGrid_)

  perfect = TEMPORARY(perfect_)
  minFcstHourFound = TEMPORARY(minFcstHourFound_)
  maxFcstHourFound = TEMPORARY(maxFcstHourFound_)

  if (perfect and NOT(KEYWORD_SET(No_save_file))) then begin

;+
;     Create IDL save file.
;-
      ndv_ = NoDataValue

      SAVE, accumACPCPGrid, accumNCPCPGrid, accumWEASDGrid, $
            minLonOut_, maxLonOut_, minLatOut_, maxLatOut_, $
            lonResOut_, latResOut_, $
            ndv_, $
            FILE = ScratchDir + '/' + savFile

      if KEYWORD_SET(Verbose) then $
          USR_MSG, 'ACPCP, NCPCP, AND WEASD lon/lat grids saved to ' + $
                   ScratchDir + '/' + savFile

  endif

  RETURN

end
