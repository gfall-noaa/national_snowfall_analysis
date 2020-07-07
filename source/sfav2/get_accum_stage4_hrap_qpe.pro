PRO GET_ACCUM_STAGE4_HRAP_QPE, $
    AccumEndDate_YYYYMMDDHH, $       ; end of accumulation
    DurationHours, $                 ; number of hours to aggregate
    St4Dir, $                        ; location of GRIB archive
    ScratchDir, $                    ; location for temp files
    NoDataValue, $
    accumQPEGrid, $
    HRAP_GRID_PROJ_INFO = HRAPGridProjInfo, $
    VERBOSE = Verbose
;+
; Accumulate 6-hour Stage IV precipitation grids.
;
; :Params:
;
;     AccumEndDate_YYYYMMDDHH : in, required, type=STRING
;         End time of accumulation period in UTC, in the form
;         YYYYMMDDHH.
;     DurationHours : in, required, type=INT
;         Accumulation duration in hours; e.g., 24 for a day of
;         precipitation accumulation. Must be a multiple of 6.
;     St4Dir : in, required, type=STRING
;         The directory where Stage IV data are stored.
;     ScratchDir : in, required, type=STRING
;         The directory where temporary files generated by this
;         procedure and the procedures it calls are stored.
;     accumQPEGrid : out, type=FLTARR(1121 x 881)
;         The aggregated Stage IV precipitation accumulation.
;
; :Keywords:
;
;     HRAP_GRID_PROJ_INFO : type=STRUCT
;         A named variable that will capture the structure describing
;         the HRAP (polar stereographic) grid and coordinate
;         system. If this variable is undefined, it will be
;         defined. If it is already defined, it will be verified
;         against the contents of the input GRIB file.
;         Structure tags:
;
;         lonV (type=DOUBLE):
;         The orientation longitude, degrees.
;
;         latD (type=DOUBLE):
;         The latitude where the grid spacing is defined, degrees.
;
;         eRadM (type=DOUBLE)
;         The radius of the spherical earth in the projection, meters.
;
;         lat00 (type=DOUBLE)
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
;     VERBOSE : in, type=BOOLEAN
;         Activates the "verbose" option, causing this procedure to
;         produce some extra output text.
;-
  COMMON info, Message ; used by USR_MSG and ERR_MSG

;+
; The best way to check for the success of this procedure is to
; do "if NOT(ISA(accumQPEGrid)) then STOP" or something similar in the
; caller, since the grid will be returned as !NULL if this procedure
; is unable to get data.
;-
  accumQPEGrid = !NULL

;+
; Error handler for anything the main procedure code misses. Example:
; RESTORE encounters a file that was truncated because a disk filled.
; Comment this section out when debugging.
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
      ERR_MSG, 'Accumulation end date/time argument must be a STRING.'
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

  accumEndDate_Julian = YYYYMMDDHH_TO_JULIAN(AccumEndDate_YYYYMMDDHH)
  accumEndDate_YYYY = STRMID(AccumEndDate_YYYYMMDDHH, 0, 4)
  accumEndDate_MM = STRMID(AccumEndDate_YYYYMMDDHH, 4, 2)
  accumEndDate_DD = STRMID(AccumEndDate_YYYYMMDDHH, 6, 2)
  accumEndDate_HH = STRMID(AccumEndDate_YYYYMMDDHH, 8, 2)

  if (FIX(accumEndDate_HH) mod 6 ne 0) then begin
      ERR_MSG, 'Invalid accumulation end date/time "' + $
               AccumEndDate_YYYYMMDDHH + $
               '" (must end in "00", "06", "12", or "18").'
      RETURN
  endif

  if (DurationHours lt 1) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      RETURN
  endif

;  if (DurationHours gt 999) then begin
;      ERR_MSG, 'Duration argument cannot exceed 999 hours.'
;      RETURN
;  endif

  if ((DurationHours mod 6) ne 0) then begin
      ERR_MSG, 'Duration argument must be a multiple of 6 (' + $
               STRCOMPRESS(DurationHours, /REMOVE_ALL) + ' provided).'
      RETURN
  endif

  if NOT(ISA(St4Dir, 'STRING')) then begin
      ERR_MSG, 'Location of Stage IV archive must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(St4Dir, /DIRECTORY)) then begin
      ERR_MSG, 'Stage IV archive directory "' + St4Dir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(St4Dir, /READ)) then begin
      ERR_MSG, 'Stage IV archive directory "' + St4Dir + '" not readable.'
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

;+
; Read Stage IV data from GRIB files.
;-
  numCycles = DurationHours / 6

  for cc = 0, numCycles - 1 do begin

      cycleEndDate_Julian = accumEndDate_Julian - $
                            (numCycles - 1) * 6.0D / 24.0D + $
                            cc * 6.0D / 24.0D
      cycleEndDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleEndDate_Julian)
      cycleEndDate_YYYYMMDD = STRMID(cycleEndDate_YYYYMMDDHH, 0, 8)
      cycleEndDate_YYYY = STRMID(cycleEndDate_YYYYMMDDHH, 0, 4)
      cycleEndDate_MM = STRMID(cycleEndDate_YYYYMMDDHH, 4, 2)
      cycleEndDate_DD = STRMID(cycleEndDate_YYYYMMDDHH, 6, 2)
      cycleEndDate_HH = STRMID(cycleEndDate_YYYYMMDDHH, 8, 2)

      GET_STAGE4_HRAP_QPE, cycleEndDate_YYYYMMDDHH, $
                           6, $
                           St4Dir, $
                           ScratchDir, $
                           NoDataValue, $
                           QPEGrid, $
                           HRAP_GRID_PROJ_INFO = HRAPGridProjInfo, $
                           VERBOSE = KEYWORD_SET(Verbose)

      if NOT(ISA(QPEGrid)) then begin
          if KEYWORD_SET(Verbose) then $
              ERR_MSG, 'Failed to read 6-hour Stage IV QPE for ' + $
                       cycleEndDate_YYYYMMDDHH + '.'
          RETURN
      endif

      if (cc eq 0) then begin

          accumQPEGrid_ = QPEGrid

      endif else begin

          ind = WHERE(accumQPEGrid_ eq NoDataValue, beforeCount)
          ind = WHERE((accumQPEGrid_ eq NoDataValue) or $
                      (QPEGrid eq NoDataValue), count)
          accumQPEGrid_ = accumQPEGrid_ + QPEGrid
          if (count gt 0) then accumQPEGrid_[ind] = NoDataValue
          if (count gt beforeCount) then $
              USR_MSG, 'WARNING: no-data values added to accumulated ' + $
                       '"APCP" grid for 6-hour cycle ending ' + $
                       cycleEndDate_YYYYMMDDHH + '.'

          ind = !NULL

      endelse

      QPEGrid = !NULL

      ;; WSET_OR_WINDOW, 0, $
      ;;                 XSIZE = HRAPGridProjInfo.nCols, $
      ;;                 YSIZE = HRAPGridProjInfo.nRows
      ;; if (cc eq 0) then LOADCT, 27
      ;; REAL_TVSCL, accumQPEGrid_, NDV = NoDataValue, MAX_VALUE = 1000.0

  endfor
 
  accumQPEGrid = TEMPORARY(accumQPEGrid_)

  RETURN

end
