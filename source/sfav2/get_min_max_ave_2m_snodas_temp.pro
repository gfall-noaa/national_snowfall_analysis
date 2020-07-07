PRO GET_MIN_MAX_AVE_2M_SNODAS_TEMP, $
    endDate_YYYYMMDDHH, $       ; end date/time
    durationHours, $            ; # hours, usually 24
;    targetFcstHour, $           ; target forecast hour, usually 0
;    minSubFcstHour, $           ; minimum substitute forecast hour, usually 0
;    maxSubFcstHour, $           ; maximum substitute forecast hour, usually 6
;    HRRRDir, $                  ; location of HRRR archive
    scratchDir, $               ; location for temporary files
    PGHost, $
    minLonOut_, maxLonOut_, $
    minLatOut_, maxLatOut_, $
    xResOut_, yResOut_, $
    Ndv, $                      ; no data value
    minTmpGrid, $
    maxTmpGrid, $
    aveTmpGrid, $
    VERBOSE = Verbose

; Get min/max/ave SNODAS temperatures.

  COMMON info, message

  minTmpGrid = !NULL
  maxTmpGrid = !NULL
  aveTmpGrid = !NULL


; Check arguments for correct type and valid contents.

  if NOT(ISA(endDate_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Target end date/time argument must be a STRING.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif
  if (STRLEN(endDate_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid target end date/time "' + $
               endDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif
  if NOT(STREGEX(endDate_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid target end date/time "' + $
               endDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  endDate_Julian = YYYYMMDDHH_TO_JULIAN(endDate_YYYYMMDDHH)

  if (durationHours lt 0) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  if (durationHours lt 0) then begin
      ERR_MSG, 'Duration must be a positive integer number of hours.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  ;; if (minSubFcstHour gt targetFcstHour) then begin
  ;;     ERR_MSG, 'Minimum substitute forecast hour may not be larger than ' + $
  ;;              'target forecast hour.'
  ;;     RETURN
  ;; endif

  ;; if (maxSubFcstHour lt targetFcstHour) then begin
  ;;     ERR_MSG, 'Maximum substitute forecast hour may not be smaller than ' + $
  ;;              'target forecast hour.'
  ;;     RETURN
  ;; endif

  ;; if NOT(ISA(HRRRDir, 'STRING')) then begin
  ;;     ERR_MSG, 'Location of HRRR archive must be a STRING.'
  ;;     RETURN
  ;; endif

  ;; if NOT(FILE_TEST(HRRRDir, /DIRECTORY)) then begin
  ;;     ERR_MSG, 'HRRR archive directory "' + HRRRDir + '" not found.'
  ;;     RETURN
  ;; endif

  ;; if NOT(FILE_TEST(HRRRDir, /READ)) then begin
  ;;     ERR_MSG, 'HRRR archive directory "' + HRRRDir + '" not readable.'
  ;;     RETURN
  ;; endif

  if NOT(ISA(scratchDir, 'STRING')) then begin
      ERR_MSG, 'Location of scratch directory must be a STRING.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  if NOT(FILE_TEST(scratchDir, /DIRECTORY)) then begin
      ERR_MSG, 'Scratch directory "' + scratchDir + '" not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  if NOT(FILE_TEST(scratchDir, /READ)) then begin
      ERR_MSG, 'Scratch directory "' + scratchDir + '" not readable.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  if NOT(FILE_TEST(scratchDir, /WRITE)) then begin
      ERR_MSG, 'Scratch directory "' + scratchDir + '" not writeable.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  if NOT(ISA(PGHost, 'STRING')) then begin
      ERR_MSG, 'PostgreSQL "PGHOST" argument must be a STRING.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  if NOT(ISA(minLonOut_, 'DOUBLE')) then $
      minLonOut = DOUBLE(minLonOut_) $
  else $
      minLonOut = minLonOut_

  if NOT(ISA(maxLonOut_, 'DOUBLE')) then $
      maxLonOut = DOUBLE(maxLonOut_) $
  else $
      maxLonOut = maxLonOut_

  if NOT(ISA(minLatOut_, 'DOUBLE')) then $
      minLatOut = DOUBLE(minLatOut_) $
  else $
      minLatOut = minLatOut_

  if NOT(ISA(maxLatOut_, 'DOUBLE')) then $
      maxLatOut = DOUBLE(maxLatOut_) $
  else $
      maxLatOut = maxLatOut_

  if NOT(ISA(xResOut_, 'DOUBLE')) then $
      xResOut = DOUBLE(xResOut_) $
  else $
      xResOut = xResOut_

  if NOT(ISA(yResOut_, 'DOUBLE')) then $
      yResOut = DOUBLE(yResOut_) $
  else $
      yResOut = yResOut_

  if (maxLonOut lt minLonOut) then begin
      ERR_MSG, 'Invalid minimum/maximum output longitude arguments.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  if (maxLatOut lt minLatOut) then begin
      ERR_MSG, 'Invalid minimum/maximum output latitude arguments.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif

  nxOut = ROUND((maxLonOut - minLonOut) / xResOut)
  nyOut = ROUND((maxLatOut - minLatOut) / yResOut)

  xErr = ABS(nxOut * xResOut - (maxLonOut - minLonOut))
  if (xERR gt 1.0D-8) then begin
      ERR_MSG, 'Inconsistent output longitudinal domain/resolution.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif
  yErr = ABS(nyOut * yResOut - (maxLatOut - minLatOut))
  if (yErr gt 1.0D-8) then begin
      ERR_MSG, 'Inconsistent output latitudinal domain/resolution.'
      RETURN
  endif

  if NOT(ISA(Ndv, 'FLOAT')) then $
      ERR_MSG, 'WARNING: no-data value should be a floating point value.'

  savFile = 'SNODAS_min_max_ave_2m_temp_anl' + $
            '_' + STRCRA(durationHours) + 'h' + $
            '_ending_' + endDate_YYYYMMDDHH + '.sav'

  if (FILE_TEST(scratchDir + '/' + savFile)) then begin


;     Get data from cache file rather than reading SNODAS data
;     directly.

      RESTORE, scratchDir + '/' + savFile

      if NOT(ISA(ndv__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "ndv__" variable.'
          RETURN
      endif

      if NOT(ISA(minLonOut__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "minLonOut__" variable.'
          RETURN
      endif

;      if (minLonOut__ ne minLonOut) then begin
      if (minLonOut__ gt minLonOut) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "minLonOut__" variable mismatch.'
          RETURN
      endif

      if NOT(ISA(maxLonOut__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "maxLonOut__" variable.'
          RETURN
      endif

;      if (maxLonOut__ ne maxLonOut) then begin
      if (maxLonOut__ lt maxLonOut) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "maxLonOut__" variable mismatch.'
          RETURN
      endif

      if NOT(ISA(minLatOut__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "minLatOut__" variable.'
          RETURN
      endif

;      if (minLatOut__ ne minLatOut) then begin
      if (minLatOut__ gt minLatOut) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "minLatOut__" variable mismatch.'
          RETURN
      endif

      if NOT(ISA(maxLatOut__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "maxLatOut__" variable.'
          RETURN
      endif

;      if (maxLatOut__ ne maxLatOut) then begin
      if (maxLatOut__ lt maxLatOut) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "maxLatOut__" variable mismatch.'
          RETURN
      endif

      if NOT(ISA(xResOut__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "xResOut__" variable.'
          RETURN
      endif

      if (xResOut__ ne xResOut) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "xResOut__" variable mismatch.'
          RETURN
      endif

      if NOT(ISA(yResOut__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "yResOut__" variable.'
          RETURN
      endif

      if (yResOut__ ne yResOut) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "yResOut__" variable mismatch.'
          RETURN
      endif

      if NOT(ISA(minTmpGrid__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "minTmpGrid__" variable.'
          RETURN
      endif

      if NOT(ISA(maxTmpGrid__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "maxTmpGrid__" variable.'
          RETURN
      endif

      if NOT(ISA(aveTmpGrid__)) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' missing "aveTmpGrid__" variable.'
          RETURN
      endif
      

;     Check for perfect grid geometry in save file.

      nxOut__ = ROUND((maxLonOut__ - minLonOut__) / xResOut__)
      nyOut__ = ROUND((maxLatOut__ - minLatOut__) / yResOut__)

      xErr = ABS(nxOut__ * xResOut__ - (maxLonOut__ - minLonOut__))
      if (xERR gt 1.0D-8) then begin
          ERR_MSG, 'Inconsistent longitudinal domain/resolution in ' + $
                   'cache file ' + scratchDir + '/' + savFile
          RETURN
      endif
      yErr = ABS(nyOut__ * yResOut__ - (maxLatOut__ - minLatOut__))
      if (yErr gt 1.0D-8) then begin
          ERR_MSG, 'Inconsistent latitudinal domain/resolution in ' + $
          'cache file ' + scratchDir + '/' + savFile
          RETURN
      endif

      varSize = SIZE(minTmpGrid__)

      if (varSize[0] ne 2) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "minTmpGrid__ has incorrect dimensionality.'
          RETURN
      endif

      if (varSize[1] ne nxOut__) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "minTmpGrid__ has incorrect "columns" dimension.'
          RETURN
      endif

      if (varSize[2] ne nyOut__) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "minTmpGrid__ has incorrect "rows" dimension.'
          RETURN
      endif

      varSize = SIZE(maxTmpGrid__)

      if (varSize[0] ne 2) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "maxTmpGrid__ has incorrect dimensionality.'
          RETURN
      endif

      if (varSize[1] ne nxOut__) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "maxTmpGrid__ has incorrect "columns" dimension.'
          RETURN
      endif

      if (varSize[2] ne nyOut__) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "maxTmpGrid__ has incorrect "rows" dimension.'
          RETURN
      endif

      varSize = SIZE(aveTmpGrid__)

      if (varSize[0] ne 2) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "aveTmpGrid__ has incorrect dimensionality.'
          RETURN
      endif

;      if (varSize[1] ne nxOut) then begin
      if (varSize[1] ne nxOut__) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "aveTmpGrid__ has incorrect "columns" dimension.'
          RETURN
      endif

;      if (varSize[2] ne nyOut) then begin
      if (varSize[2] ne nyOut__) then begin
          ERR_MSG, 'Cache file ' + scratchDir + '/' + savFile + $
                   ' "aveTmpGrid__ has incorrect "rows" dimension.'
          RETURN
      endif

      if ((minLonOut__ ne minLonOut) or $
          (maxLonOut__ ne maxLonOut) or $
          (minLatOut__ ne minLatOut) or $
          (maxLatOut__ ne maxLatOut)) then begin


;         Get cache subgrids to match requested bounds.

          left = ((minLonOut - minLonOut__) / xResOut)
          if (ABS(DOUBLE(ROUND(left)) - left) gt 1.0D-8) then STOP ; data
          left = ROUND(left)

          right = ((maxLonOut__ - maxLonOut) / xResOut)
          if (ABS(DOUBLE(ROUND(right)) - right) gt 1.0D-8) then STOP ; data
          right = ROUND(right)

          if ((left + nxOut + right) ne nxOut__) then STOP ; prog

          bottom = ((minLatOut - minLatOut__) / yResOut)
          if (ABS(DOUBLE(ROUND(bottom)) - bottom) gt 1.0D-8) then STOP ; data
          bottom = ROUND(bottom)

          top = ((maxLatOut__ - maxLatOut) / yResOut)
          if (ABS(DOUBLE(ROUND(top)) - top) gt 1.0D-8) then STOP ; data
          top = ROUND(top)

          if ((bottom + nyOut + top) ne nyOut__) then STOP ; prog

          minTmpGrid = minTmpGrid__[left: nxOut__ - right - 1L, $
                                    bottom: nyOut__ - top - 1L]

          maxTmpGrid = maxTmpGrid__[left: nxOut__ - right - 1L, $
                                    bottom: nyOut__ - top - 1L]

          aveTmpGrid = aveTmpGrid__[left: nxOut__ - right - 1L, $
                                    bottom: nyOut__ - top - 1L]

      endif else begin

          minTmpGrid = minTmpGrid__
          maxTmpGrid = maxTmpGrid__
          aveTmpGrid = aveTmpGrid__

      endelse

      RETURN

  endif

  for hc = 0, durationHours - 1 do begin

      cycleDate_Julian = endDate_Julian - $
                         DOUBLE(durationHours) / 24.0D + $
                         DOUBLE(hc) / 24.0D
      cycleDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(cycleDate_Julian)
      cycleDate_GISRS = JULIAN_TO_GISRS_DATE(cycleDate_Julian)

      GISRSLayer = 'Scaled corrected CRR Surface temperature anl ' + $
                   cycleDate_GISRS

;GFKS 20170530 vv
;USR_MSG, 'Checking ' + PGHost + ' for layer "' + GISRSLayer + '"'
;GFKS 20170530 ^^

      statement = 'psql -d operations -h ' + PGHost + ' -t -A -c ' + $
                  '"' + $
                  'set search_path = gisrs; ' + $
                  'select ' + $
                  'trim(data_file_pathname) ' + $
                  'from airtemp_sm ' + $
                  'where gis_layer = ''' + GISRSLayer + ''';"'

      SPAWN, statement, ncFilePath, EXIT_STATUS = status

;GFKS 20170530 vv
;USR_MSG, 'Reading data file "' + ncFilePath + '"'
;GFKS 20170530 ^^

      if (status ne 0) then begin
          ERR_MSG, 'psql statement failed: ' + statement
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
;GFKS 20170607 vv
;USR_MSG, 'ping01'
;GFKS 20170607 ^^

      if (ncFilePath eq '') then begin
          if KEYWORD_SET(Verbose) then $
              ERR_MSG, 'Layer "' + GISRSLayer + '" not found.'
          RETURN
;          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
;GFKS 20170607 vv
;USR_MSG, 'ping02'
;GFKS 20170607 ^^

      if (N_ELEMENTS(ncFilePath) ne 1) then begin
          ERR_MSG, 'Unexpected query result.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
;GFKS 20170607 vv
;USR_MSG, 'ping03'
;GFKS 20170607 ^^

      ncFilePath = ncFilePath[0]
      if (STRMID(ncFilePath, 2, 3, /REVERSE_OFFSET) ne '.nc') then begin
          ERR_MSG, 'Query result "' + ncFilePath + '" ' + $
                   'does not have the expected ".nc" extension.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
;GFKS 20170607 vv
;USR_MSG, 'ping04'
;GFKS 20170607 ^^

      if NOT(FILE_TEST(ncFilePath)) then begin
          ERR_MSG, 'File "' + ncFilePath + '" associated with layer ' + $
                   '"' + GISRSLayer + '" not found.'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
;GFKS 20170607 vv
;USR_MSG, 'ping05'
;GFKS 20170607 ^^

      err = 0


;     Open NetCDF file with temperature grid.

;GFKS 20170607 vv
;print, 'hello'
;USR_MSG, 'Opening ' + ncFilePath
;GFKS 20170607 ^^
      id = NCDF_OPEN(ncFilePath)
;print, 'goodbye'
;GFKS 20170530 vv
;USR_MSG, 'Opened data file "' + ncFilePath + '"'
;GFKS 20170530 ^^

;     Get dimensions.

      dimID_lat = NCDF_DIMID(id, 'lat')
      NCDF_DIMINQ, id, dimID_lat, dummy, latDimSize_

      dimID_lon = NCDF_DIMID(id, 'lon')
      NCDF_DIMINQ, id, dimID_lon, dummy, lonDimSize_

      dimID_nv = NCDF_DIMID(id, 'nv')
      NCDF_DIMINQ, id, dimID_nv, dummy, nv
      if (nv ne 2) then begin
          ERR_MSG, 'Dimension "nv" must have a size of 2.'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
;GFKS 20170606 vv
;USR_MSG, 'Read dimensions from "' + ncFilePath + '"'
;GFKS 20170606 ^^


;     Check variables.

      varID_lat = NCDF_VARID(id, 'lat')
      varInfo = NCDF_VARINQ(id, varID_lat)
      if (varInfo.ndims ne 1) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lat".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[0] ne dimID_lat) then begin
          ERR_MSG, 'Dimension of variable "lat" must be "lat".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lat" must be DOUBLE.'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif

      NCDF_VARGET, id, 'lat', lat_
      NCDF_ATTGET, id, varID_lat, 'resolution', latRes_

      varID_latBounds = NCDF_VARID(id, 'lat_bounds')
      varInfo = NCDF_VARINQ(id, varID_latBounds)
      if (varInfo.nDims ne 2) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lat_bounds".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[0] ne dimID_nv) then begin
          ERR_MSG, 'First dimension of variable "lat_bounds" must be "nv".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[1] ne dimID_lat) then begin
          ERR_MSG, 'Second dimension of variable "lat_bounds" must be "lat".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif

      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lat_bounds" must be DOUBLE.'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif

      NCDF_VARGET, id, 'lat_bounds', latBounds_

      varID_lon = NCDF_VARID(id, 'lon')
      varInfo = NCDF_VARINQ(id, varID_lon)
      if (varInfo.ndims ne 1) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lon".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[0] ne dimID_lon) then begin
          ERR_MSG, 'Dimension of variable "lon" must be "lon".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lon" must be DOUBLE.'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif

      NCDF_VARGET, id, 'lon', lon_
      NCDF_ATTGET, id, varID_lon, 'resolution', lonRes_

      varID_lonBounds = NCDF_VARID(id, 'lon_bounds')
      varInfo = NCDF_VARINQ(id, varID_lonBounds)
      if (varInfo.nDims ne 2) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lon_bounds".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[0] ne dimID_nv) then begin
          ERR_MSG, 'First dimension of variable "lon_bounds" must be "nv".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[1] ne dimID_lon) then begin
          ERR_MSG, 'Second dimension of variable "lon_bounds" must be "lon".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lon_bounds" must be DOUBLE.'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif

      NCDF_VARGET, id, 'lon_bounds', lonBounds_

      varID_crs = NCDF_VARID(id, 'crs')

      varID_data = NCDF_VARID(id, 'Data')
      varInfo = NCDF_VARINQ(id, varID_data)
      if (varInfo.ndims ne 2) then begin
          ERR_MSG, 'Unexpected dimensions in variable "Data".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[0] ne dimID_lon) then begin
          ERR_MSG, 'First dimension of variable "Data" must be "lon".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
      if (varInfo.dim[1] ne dimID_lat) then begin
          ERR_MSG, 'Second dimension of variable "Data" must be "lat".'
          NCDF_CLOSE, id
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif
;GFKS 20170606 vv
;USR_MSG, 'Inquired variables from "' + ncFilePath + '"'
;GFKS 20170606 ^^


;     Get variable attributes.

      NCDF_ATTGET, id, varID_data, '_FillValue', fillValue_
      NCDF_ATTGET, id, varID_data, 'no_data_value', ndv_

      NCDF_ATTGET, id, 'Data', 'units', units_ & units_ = STRING(units_)
      if (units_ ne 'Kelvins') then begin
          ERR_MSG, 'Invalid "units" attribute of "' + units_ + $
                   '" for input variable "Data". ' + $
                   'Must be "Kelvins".'
          if NOT(LMGR(/RUNTIME)) then STOP else RETURN
      endif

      minLon_ = lonBounds_[0,0]
      maxLon_ = lonBounds_[1,lonDimSize_ - 1]
      maxLat_ = latBounds_[0,0]
      minLat_ = latBounds_[1,latDimSize_ - 1]

      NCDF_ATTGET, id, varID_lat, 'long_name', latLongName_
      latLongName_ = STRING(latLongName_)
      NCDF_ATTGET, id, varID_lat, 'units', latUnits_
      latUnits_ = STRING(latUnits_)
      NCDF_ATTGET, id, varID_lat, 'standard_name', latStandardName_
      latStandardName_ = STRING(latStandardName_)
      NCDF_ATTGET, id, varID_lon, 'long_name', lonLongName_
      lonLongName_ = STRING(lonLongName_)
      NCDF_ATTGET, id, varID_lon, 'units', lonUnits_
      lonUnits_ = STRING(lonUnits_)
      NCDF_ATTGET, id, varID_lon, 'standard_name', lonStandardName_
      lonStandardName_ = STRING(lonStandardName_)
      NCDF_ATTGET, id, varID_data, 'institution', institution_
      institution_ = STRING(institution_)
      NCDF_ATTGET, id, varID_data, 'gisrs_product_code', GISRSProductCode_
      NCDF_ATTGET, id, varID_data, 'long_name', dataLongName_
      dataLongName_ = STRING(dataLongName_)
      NCDF_ATTGET, id, varID_data, 'standard_name', dataStandardName_
      dataStandardName_ = STRING(dataStandardName_)
      NCDF_ATTGET, id, varID_data, 'satellite_data', satelliteData_
      satelliteData_ = STRING(satelliteData_)
      NCDF_ATTGET, id, varID_data, 'thematic', thematic_
      thematic_ = STRING(thematic_)
      NCDF_ATTGET, id, varID_data, 'data_are_elevations', dataAreElevations_
      dataAreElevations_ = STRING(dataAreElevations_)
      NCDF_ATTGET, id, varID_data, 'number_of_color_tables', numColTables_
      NCDF_ATTGET, id, varID_data, 'add_offset', addOffset_
      NCDF_ATTGET, id, varID_data, 'scale_factor', scaleFactor_
;GFKS 20170606 vv
;USR_MSG, 'Read variable attributes from "' + ncFilePath + '"'
;GFKS 20170606 ^^

     if (hc eq 0) then begin


;         Store geometry variables.

          latDimSize = latDimSize_
          lonDimSize = lonDimSize_
          latRes = latRes_
          lat = lat_
          latBounds = latBounds_
          lonRes = lonRes_
          lon = lon_
          lonBounds = lonBounds_
          fillValue = fillValue_
          ;ndv = ndv_ (use no-data value in arguments list)
          minLon = minLon_
          maxLon = maxLon_
          minLat = minLat_
          maxLat = maxLat_


;         Verify geometry attributes are perfect.

          lonDimSize_ = (maxLon - minLon) / lonRes
          if (ABS(DOUBLE(ROUND(lonDimSize)) - lonDimSize) gt 1.0D-8) $
              then begin
              ERR_MSG, 'Longitudinal bounds/resolution mismatch in ' + $
                       'input data file.'
              if NOT(LMGR(/RUNTIME)) then STOP else RETURN
          endif
          lonDimSize_ = ROUND(lonDimSize_)
          if (lonDimSize_ ne lonDimSize) then STOP ; PROGRAMMING ERROR

          latDimSize_ = (maxLat - minLat) / latRes
          if (ABS(DOUBLE(ROUND(latDimSize)) - latDimSize) gt 1.0D-8) $
              then begin
              ERR_MSG, 'Latitudinal bounds/resolution mismatch in ' + $
                       'input data file.'
              if NOT(LMGR(/RUNTIME)) then STOP else RETURN
          endif
          latDimSize_ = ROUND(latDimSize_)
          if (latDimSize_ ne latDimSize) then STOP ; PROGRAMMING ERROR


;         Verify that the domain and resolution can be easily
;         subsetted and resampled to match the output domain and
;         resolution.

          err = 0

          if (minLonOut lt minLon) then begin
              ERR_MSG, 'Requested minimum longitude ' + $
                       'is less than input minimum longitude.'
              err = 1
          endif
          numColsLeft = (minLonOut - minLon) / lonRes
          if (ABS(DOUBLE(ROUND(numColsLeft)) - numColsLeft) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested minimum longitude ' + $
                       'requires a non-integer crop.'
          endif
          numColsLeft = ROUND(numColsLeft)

          if (maxLonOut gt maxLon) then begin
              ERR_MSG, 'Requested maximum longitude ' + $
                       'is greater than input maximum longitude.'
              err = 1
          endif
          numColsRight = (maxLon - maxLonOut) / lonRes
          if (ABS(DOUBLE(ROUND(numColsRight)) - numColsRight) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested maximum longitude ' + $
                       'requires a non-integer crop.'
          endif
          numColsRight = ROUND(numColsRight)

          if (minLatOut lt minLat) then begin
              ERR_MSG, 'Requested minimum latitude ' + $
                       'is less than input minimum latitude.'
              err = 1
          endif
          numRowsBot = (minLatOut - minLat) / latRes
          if (ABS(DOUBLE(ROUND(numRowsBot)) - numRowsBot) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested minimum latitude ' + $
                       'requires a non-integer crop.'
          endif
          numRowsBot = ROUND(numRowsBot)

          if (maxLatOut gt maxLat) then begin
              ERR_MSG, 'Requested maximum latitude ' + $
                       'is greater than input maximum latitude.'
              err = 1
          endif
          numRowsTop = (maxLat - maxLatOut) / latRes
          if (ABS(DOUBLE(ROUND(numRowsTop)) - numRowsTop) gt 1.0D-6) $
              then begin
              ERR_MSG, 'WARNING: requested maximum latitude ' + $
                       'requires a non-integer crop.'
          endif
          numRowsTop = ROUND(numRowsTop)

          subgridCols = lonDimSize - numColsLeft - numColsRight
          if (subgridCols lt 0) then begin
              ERR_MSG, 'Requested longitude bounds result in empty grid.'
              err = 1
          endif

          subgridRows = latDimSize - numRowsBot - numRowsTop
          if (subgridRows lt 0) then begin
              ERR_MSG, 'Requested latitude bounds result in empty grid.'
              err = 1
          endif

          if err then begin
              if NOT(LMGR(/RUNTIME)) then STOP else RETURN
          endif


;         Get global attributes.

          NCDF_ATTGET, id, 'format_version', formatVersion, /GLOBAL
          NCDF_ATTGET, id, 'Conventions', conventions, /GLOBAL
          NCDF_ATTGET, id, 'title', title, /GLOBAL
          NCDF_ATTGET, id, 'source', source, /GLOBAL
;GFKS 20170606 vv
;USR_MSG, 'Read global attributes (1st hour) from "' + ncFilePath + '"'
;GFKS 20170606 ^^


;         Store variable attributes.

          latLongName = latLongName_
          latUnits = latUnits_
          latStandardName = latStandardName_
          lonLongName = lonLongName_
          lonUnits = lonUnits_
          lonStandardName = lonStandardName_
          institution = institution_
          GISRSProductCode = GISRSProductCode_
          dataLongName = dataLongName_
          dataUnits = units_
          dataStandardName = dataStandardName_
          satelliteData = satelliteData_
          thematic = thematic_
          dataAreElevations = dataAreElevations_
          numColTables = numColTables_
          addOffset = addOffset_
          scaleFactor = scaleFactor_


;         Read data.

          NCDF_VARGET, id, 'Data', grid
;GFKS 20170606 vv
;USR_MSG, 'Read data (1st hour) from "' + ncFilePath + '"'
;GFKS 20170606 ^^
          grid = REVERSE(grid, 2) ; make south-to-north

          gridSize = SIZE(grid)
          if (gridSize[0] ne 2) then STOP ; MAJOR FLUB
          if (gridSize[1] ne lonDimSize) then STOP ; MAJOR FLUB
          if (gridSize[2] ne latDimSize) then STOP ; MAJOR FLUB

          grid = grid[numColsLeft:lonDimSize - numColsRight - 1, $
                      numRowsBot:latDimSize - numRowsTop - 1]
          gridSize = SIZE(grid)
          if (gridSize[0] ne 2) then STOP ; PROGRAMMING ERROR
          if (gridSize[1] ne subgridCols) then STOP ; PROGRAMMING ERROR
          if (gridSize[2] ne subgridRows) then STOP ; PROGRAMMING ERROR

          ind = WHERE(grid eq fillValue, count)
          grid = grid * scaleFactor + addOffset
          if (count gt 0) then grid[ind] = Ndv

          ntg = grid ; minimum
          xtg = grid ; maximum
          atg = grid ; average

          grid = !NULL

      endif else begin


;         Verify geometry.

          if (latDimSize_ ne latDimSize) then begin
              ERR_MSG, 'Latitude dimension size mismatch.'
              err = 1
          endif

          if (lonDimSize_ ne lonDimSize) then begin
              ERR_MSG, 'Longitude dimension size mismatch.'
              err = 1
          endif

          if (latRes_ ne latRes) then begin
              ERR_MSG, 'Latitude resolution mismatch.'
              err = 1
          endif

          if (MAX(ABS(lat_ - lat)) gt 1.0D-8) then begin
              ERR_MSG, 'Latitude grid mismatch.'
              err = 1
          endif

          if (MAX(ABS(latBounds_ - latBounds)) gt 1.0D-8) then begin
              ERR_MSG, 'Latitude bounds mismatch.'
              err = 1
          endif

          if (lonRes_ ne lonRes) then begin
              ERR_MSG, 'Longitude resolution mismatch.'
              err = 1
          endif

          if (MAX(ABS(lon_ - lon)) gt 1.0D-8) then begin
              ERR_MSG, 'Laongitude grid mismatch.'
              err = 1
          endif

          if (MAX(ABS(lonBounds_ - lonBounds)) gt 1.0D-8) then begin
              ERR_MSG, 'Longitude bounds mismatch.'
              err = 1
          endif

          if (latLongName_ ne latLongName) then begin
              ERR_MSG, 'WARNING: inconsistent "long_name" attributes ' + $
                       'for variable "lat".'
          endif
          if (latUnits_ ne latUnits) then begin
              ERR_MSG, 'WARNING: inconsistent "units" attributes ' + $
                       'for variable "lat".'
          endif
          if (latStandardName_ ne latStandardName) then begin
              ERR_MSG, 'WARNING: inconsistent "standard_name" attributes ' + $
                       'for variable "lat".'
          endif
          if (lonLongName_ ne lonLongName) then begin
              ERR_MSG, 'WARNING: inconsistent "long_name" attributes ' + $
                       'for variable "lon".'
          endif
          if (lonUnits_ ne lonUnits) then begin
              ERR_MSG, 'WARNING: inconsistent "units" attributes ' + $
                       'for variable "lon".'
          endif
          if (lonStandardName_ ne lonStandardName) then begin
              ERR_MSG, 'WARNING: inconsistent "standard_name" attributes ' + $
                       'for variable "lon".'
          endif
          if (institution_ ne institution) then begin
              ERR_MSG, 'WARNING: inconsistent "institution" attributes ' + $ 
                       'for variable "Data".'
          endif
          if (dataLongName_ ne dataLongName) then begin
              ERR_MSG, 'WARNING: inconsistent "long_name" attributes ' + $
                       'for variable "Data".'
          endif
          if (dataStandardName_ ne dataStandardName) then begin
              ERR_MSG, 'WARNING: inconsistent "standard_name" attributes ' + $
                       'for variable "Data".'
          endif
          if (satelliteData_ ne satelliteData) then begin
              ERR_MSG, 'WARNING: inconsistent "satellite_data" attributes ' + $
                       'for variable "Data".'
          endif
          if (thematic_ ne thematic) then begin
              ERR_MSG, 'WARNING: inconsistent "thematic" attributes ' + $
                       'for variable "Data".'
          endif
          if (dataAreElevations_ ne dataAreElevations) then begin
              ERR_MSG, 'WARNING: inconsistent "data_are_elevations" ' + $
                       'attributes ' + $
                       'for variable "Data".'
          endif
          if (numColTables_ ne numColTables) then begin
              ERR_MSG, 'WARNING: inconsistent "number_of_color_tables" ' + $
                       'attributes ' + $
                       'for variable "Data".'
          endif
          if (numColTables eq 1) then begin
               if (colorTableFile_ ne colorTableFile) then begin
                  ERR_MSG, 'WARNING: inconsistent "color_table_file" ' + $
                           'attributes for variable "Data".'
              endif
               if (colorTableDescriptor_ ne colorTableDescriptor) then begin
                  ERR_MSG, 'WARNING: inconsistent "color_table_descriptor" ' + $
                           'attributes for variable "Data".'
              endif
          endif
          if (addOffset_ ne addOffset) then begin
              ERR_MSG, '"add_offset" attribute mismatch.'
              err = 1
          endif
          if (scaleFactor_ ne scaleFactor) then begin
              ERR_MSG, '"scale_factor" attribute mismatch.'
              err = 1
          endif
          if (fillValue_ ne fillValue) then begin
              ERR_MSG, 'Fill value mismatch.'
              err = 1
          endif
          ;; if (ndv_ ne ndv) then begin
          ;;     ERR_MSG, 'No-data value mismatch.'
          ;;     err = 1
          ;; endif

          if err then begin
              NCDF_CLOSE, id
              if NOT(LMGR(/RUNTIME)) then STOP else RETURN
          endif

          NCDF_VARGET, id, 'Data', grid
;GFKS 20170606 vv
;USR_MSG, 'Read data from "' + ncFilePath + '"'
;GFKS 20170606 ^^
          grid = REVERSE(grid, 2) ; make south-to-north

          gridSize = SIZE(grid)
          if (gridSize[0] ne 2) then STOP ; MAJOR FLUB
          if (gridSize[1] ne lonDimSize) then STOP ; MAJOR FLUB
          if (gridSize[2] ne latDimSize) then STOP ; MAJOR FLUB

          grid = grid[numColsLeft:lonDimSize - numColsRight - 1, $
                      numRowsBot:latDimSize - numRowsTop - 1]
          gridSize = SIZE(grid)
          if (gridSize[0] ne 2) then STOP ; PROGRAMMING ERROR
          if (gridSize[1] ne subgridCols) then STOP ; PROGRAMMING ERROR
          if (gridSize[2] ne subgridRows) then STOP ; PROGRAMMING ERROR

          ind = WHERE(grid eq fillValue, count)
          grid = grid * scaleFactor + addOffset
          if (count gt 0) then grid[ind] = Ndv

          ind = WHERE((atg eq Ndv) or (grid eq Ndv), count)
          ntg = ntg < grid
          xtg = xtg > grid
          atg = atg + grid
          if (count gt 0) then begin
              ntg[ind] = Ndv
              xtg[ind] = Ndv
              atg[ind] = Ndv
          endif

          grid = !NULL

      endelse
;GFKS 20170606 vv
;USR_MSG, 'Verified attribute consistency from "' + ncFilePath + '"'
;GFKS 20170606 ^^

      NCDF_CLOSE, id

;GFKS 20170530 vv
;USR_MSG, 'Read and closed data file "' + ncFilePath + '"'
;GFKS 20170530 ^^

  endfor

  ;; ntg = SUBGRID(ntg, minLon, maxLon, minLat, maxLat, $
  ;;               lonRes, latRes, $
  ;;               minLonOut, maxLonOut, minLatOut, maxLatOut)

  ;; ntgSize = SIZE(ntg)
  ;; if (ntgSize[0] ne 2) then STOP ; PROGRAMMING ERROR
  ;; numColsOut = gridSize[1]
  ;; numRowsOut = gridSize[2]

  ;; xtg = SUBGRID(xtg, minLon, maxLon, minLat, maxLat, $
  ;;               lonRes, latRes, $
  ;;               minLonOut, maxLonOut, minLatOut, maxLatOut)

  ;; xtgSize = SIZE(xtg)

  ;; if (xtgSize[0] ne 2) then STOP ; PROGRAMMING ERROR
  ;; if (xtgSize[1] ne numColsOut) then STOP ; PROGRAMMING ERROR
  ;; if (xtgSize[2] ne numRowsOut) then STOP ; PROGRAMMING ERROR

  ;; atg = SUBGRID(atg, minLon, maxLon, minLat, maxLat, $
  ;;               lonRes, latRes, $
  ;;               minLonOut, maxLonOut, minLatOut, maxLatOut)

  ;; atgSize = SIZE(atg)

  ;; if (atgSize[0] ne 2) then STOP ; PROGRAMMING ERROR
  ;; if (atgSize[1] ne numColsOut) then STOP ; PROGRAMMING ERROR
  ;; if (atgSize[2] ne numRowsOut) then STOP ; PROGRAMMING ERROR

  ind = WHERE(atg ne Ndv, count)
  if (count eq 0) then begin
      ERR_MSG, 'No output cells have data for all ' + $
               STRCRA(durationHours) + ' hours.'
      if NOT(LMGR(/RUNTIME)) then STOP else RETURN
  endif
  atg[ind] = atg[ind] / durationHours


; Resample.

  xRatio = DOUBLE(nxOut) / DOUBLE(subgridCols)
  yRatio = DOUBLE(nyOut) / DOUBLE(subgridRows)
  if (ABS(yRatio - xRatio) gt 1.0D-6) then begin
      ERR_MSG, 'WARNING: resampling with different proportions in ' + $
               'longitudinal and latitudinal dimensions.'
  endif
  flag = CONGRID(FLOAT(atg ne Ndv), nxOut, nyOut, /INTERP)
  ind = WHERE(flag lt 1.0, count)
  atg = CONGRID(atg, nxOut, nyOut, /INTERP)
  ntg = CONGRID(ntg, nxOut, nyOut, /INTERP)
  xtg = CONGRID(xtg, nxOut, nyOut, /INTERP)
  if (count gt 0) then begin
      atg[ind] = Ndv
      ntg[ind] = Ndv
      xtg[ind] = Ndv
  endif

  minTmpGrid = TEMPORARY(ntg)
  maxTmpGrid = TEMPORARY(xtg)
  aveTmpGrid = TEMPORARY(atg)


; Create a cached copy of results for future calls to this procedure
; with the same set of arguments.

  ndv__ = Ndv
  minLonOut__ = minLonOut
  maxLonOut__ = maxLonOut
  minLatOut__ = minLatOut
  maxLatOut__ = maxLatOut
  xResOut__ = xResOut
  yResOut__ = yResOut
  minTmpGrid__ = minTmpGrid
  maxTmpGrid__ = maxTmpGrid
  aveTmpGrid__ = aveTmpGrid

  SAVE, ndv__, $
        minLonOut__, maxLonOut__, minLatOut__, maxLatOut__, $
        xResOut__, yResOut__, $
        minTmpGrid__, maxTmpGrid__, aveTmpGrid__, $
        FILE = scratchDir + '/' + savFile


  RETURN

end
