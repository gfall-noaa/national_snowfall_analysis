PRO CSV_WRITE_SEMIVARIOGRAM_PARAMETERS, $
    paramsDir, $
    paramsFile, $
    date_YYYYMMDDHH, $
    numPoints, $                ; # points in variogram
    fitRMSE, $                  ; RMSE of variogram fit
    fitRMSEInRange, $           ; RMSE of variogram fit for below-range lags
    nugget, $                   ; fitted nugget
    sill, $                     ; fitted sill
    range_meters, $             ; fitted range
    cwspStatus, $
    VERBOSE = verbose

; Write semivariogram parameters to a possibly already-existing CSV
; file, creating a new file, appending parameters to an existing file,
; or overwriting parameters in an existing file as needed.

  cwspStatus = 0


; Check arguments.

  if NOT(ISA(date_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Date/time argument must be a STRING.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif
  if (STRLEN(date_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid date/time "' + $
               date_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif
  if NOT(STREGEX(date_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid date/time "' + $
               date_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif


; Set flags.

  dateFound = 0
  paramsChanged = 0
  newParamsFile = 0
  tmpParamsFile = !NULL


; Check for output directory with read and write permissions.

;  if NOT(FILE_TEST(paramsDir, /DIR, /READ, /WRITE)) then begin
  if NOT(FILE_TEST(paramsDir, /DIR, /READ)) then begin
      ERR_MSG, 'Output directory "' + paramsDir + '" not found or ' + $
               'not accessible.'
;      STOP
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  paramsFilePath = paramsDir + '/' + paramsFile

  if FILE_TEST(paramsFilePath) then begin


;     Open the existing parameters file for reading and writing
;     (OPENU).

      OPENU, lun, paramsFilePath, /GET_LUN


;     Create a temporary file so that existing variogram parameters
;     may be overwritten if necessary.

      cmd = 'mktemp ' + paramsFilePath + '.XXX'
      SPAWN, cmd, tmpFilePath, EXIT_STATUS = status
      if ((status ne 0) or (N_ELEMENTS(tmpFilePath) ne 1)) then begin
          ERR_MSG, 'Failed to generate temporary file name for ' + $
                   'variogram parameters.'
          FREE_LUN, lun
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      tmpFilePath = tmpFilePath[0]
      if NOT(FILE_TEST(tmpFilePath)) then begin
          ERR_MSG, 'Temporary variogram parameter file "' + $
                   tmpFilePath + '" not found.'
          FREE_LUN, lun
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif

      OPENU, tLun, tmpFilePath, /GET_LUN


;     Read existing parameters file.

      line = ''
      while NOT(EOF(lun)) do begin

          READF, lun, line
          columns = STRSPLIT(line, ',', /EXTRACT)
          if (N_ELEMENTS(columns) ne 7) then begin
              ERR_MSG, 'Semivariogram parameter file ' + paramsFilePath + $
                       'has unexpected format.'
              FREE_LUN, lun
              FREE_LUN, tLun
              if NOT(LMGR(/RUNTIME)) then STOP else RETURN
          endif

          if (columns[0] eq date_YYYYMMDDHH) then begin


;             Found current date/time. Check parameters against the
;             current ones and issue a warning if they differ.

              dateFound = 1

              if ((columns[1] ne STRCRA(numPoints)) or $
                  (columns[2] ne STRCRA(fitRMSE)) or $
                  (columns[3] ne STRCRA(fitRMSEInRange)) or $
                  (columns[4] ne STRCRA(nugget)) or $
                  (columns[5] ne STRCRA(sill)) or $
                  (columns[6] ne STRCRA(range_meters))) then begin

                  
;                 Stored parameters differ from input arguments. Write
;                 the new parameters.

                  paramsChanged = 1

                  if KEYWORD_SET(verbose) then $
                      USR_MSG, 'The stored variogram parameters ' + $
                               'for this date (' + $
                               '# obs = ' + columns[1] + ', ' + $
                               'RMSE = ' + columns[2] + ', ' + $
                               'in-range RMSE = ' + columns[3] + ', ' + $
                               'nugget = ' + columns[4] + ', ' + $
                               'sill = ' + columns[5] + ', ' + $
                               'range = ' + columns[6] + ') ' + $
                               'differ from the fitted parameters. ' + $
                               'The older parameters will be replaced.'

              endif else begin


;                 Stored parameters match input arguments.

                  if KEYWORD_SET(verbose) then $
                      USR_MSG, 'Variogram parameters for ' + $
                               date_YYYYMMDDHH + $
                               ' already stored in ' + $
                               paramsFilePath + '.'

              endelse


;             Write new parameters to the temporary file. They may
;             match the existing ones or they might be replacing
;             them.

              PRINTF, tLun, $
                      date_YYYYMMDDHH + ',' + $
                      STRCRA(numPoints) + ',' + $
                      STRCRA(fitRMSE) + ',' + $
                      STRCRA(fitRMSEInRange) + ',' + $
                      STRCRA(nugget) + ',' + $
                      STRCRA(sill) + ',' + $
                      STRCRA(range_meters)

          endif else begin


;             Write line from exiting parameters file into temporary
;             file.

              PRINTF, tLun, line

          endelse

      endwhile

      FREE_LUN, tLun

  endif else begin


;     Open a new parameters file (OPENW)

      newParamsFile = 1
      OPENW, lun, paramsFilePath, /GET_LUN

  endelse

  if NOT(dateFound) then begin


;     Write semivariogram parameters from input args into parameters file.

      if KEYWORD_SET(verbose) then $
          USR_MSG, 'Saving semivariogram parameters from ' + $
                   date_YYYYMMDDHH + ' to ' + $
                   paramsFilePath + '.'

      PRINTF, lun, $
              date_YYYYMMDDHH + ',' + $
              STRCRA(numPoints) + ',' + $
              STRCRA(fitRMSE) + ',' + $
              STRCRA(fitRMSEInRange) + ',' + $
              STRCRA(nugget) + ',' + $
              STRCRA(sill) + ',' + $
              STRCRA(range_meters)

  endif

  FREE_LUN, lun


; Make sure the new parameters file has 0664 permissions.

  if (newParamsFile) then $
      FILE_CHMOD, paramsFilePath, '0664'o

  if ISA(tmpFilePath) then begin

      if paramsChanged then begin


;         Replace existing parameters file with contents of temporary
;         file. The FILE_CHMOD -> FILE_DELETE -> FILE_MOVE is needed,
;         because FILE_MOVE by itself cannot overwrite a file with a
;         different owner, even if that file has group write
;         permissions. There is some risk here because between
;         FILE_DELETE and FILE_MOVE, the existing parameters file no
;         longer exists.

          if KEYWORD_SET(verbose) then $
              USR_MSG, 'Replacing variogram parameters in ' + $
                       paramsFilePath + ' with contents of ' + $
                       tmpFilePath
          FILE_CHMOD, tmpFilePath, '0664'o
          FILE_DELETE, paramsFilePath
          FILE_MOVE, tmpFilePath, paramsFilePath

      endif else begin


;         The temporary file is identical to the existing parameters
;         file. It can be deleted.

          if KEYWORD_SET(verbose) then $
              USR_MSG, 'Removing ' + tmpFilePath + $
                       ' (no new content vs. ' + paramsFile + ').'
          FILE_DELETE, tmpFilePath

      endelse

  endif

  cwspStatus = 1

  RETURN

end
