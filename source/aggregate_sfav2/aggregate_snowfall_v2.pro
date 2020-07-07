
; Perform aggregation of snowfall V2 analysis output.

; Greg Fall, NOHRSC 
; Kent Sparrow, NWC 
; 19 December 2016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO AGGREGATE_SNOWFALL_V2, anlEndDate_YYYYMMDDHH

  COMMON info, message

  !QUIET = 1

  asStatus = 0
  message = ''
  weHaveBailed = 0

  if LMGR(/VM) then begin
      ERR_MSG, 'This program cannot run in the IDL Virtual Machine.'
      GOTO, BAIL
  endif


; Get arguments from environment variables. These are best set with the
; calling script "aggregate_snowfall_v2.sh"

;+
; Establish whether to use the "extended" color map, which
; distinguishes snowfall accumulations from 4 feet to 10 feet for
; aggregations of up to 3 days.
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

;+
; GF 20191002
; Determine whether or not the "FORCE_SEASONAL" environment variable
; is set. If it is, and is set to "1", then all conventions for
; aggregations greater than 3 days are applied to the aggregation,
; even if it is less than 3 days. This is helpful from October 1 to
; October 3 in calculating seasonal snowfall totals.
;-
  forceSeasonal = GETENV('FORCE_SEASONAL')
  if (forceSeasonal eq '') then begin
      forceSeasonal = 0
  endif else begin
      if ((forceSeasonal ne '0') and (forceSeasonal ne '1')) then begin
          ERR_MSG, 'ERROR: Invalid FORCE_SEASONAL value "' + $
                   forceSeasonal + '".'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      forceSeasonal = FIX(forceSeasonal)
  endelse


; Finish date/time of analysis period. Default is 12Z of the current
; day in UTC.

  if NOT(ISA(anlEndDate_YYYYMMDDHH)) then begin
      anlEndDate_YYYYMMDDHH = GETENV('DATE_YYYYMMDDHH')
      if (anlEndDate_YYYYMMDDHH eq '') then begin
          sysTime_Julian = SYSTIME(/JULIAN, /UTC)
          sysTime_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(sysTime_Julian)
          anlEndDate_YYYYMMDDHH = STRMID(sysTime_YYYYMMDDHH, 0, 8) + '12'
          USR_MSG, 'Environment variable DATE_YYYYMMDDHH not provided. ' + $
                   'Using default of ' + anlEndDate_YYYYMMDDHH + '.'
      endif
  endif else begin
      if NOT(STREGEX(anlEndDate_YYYYMMDDHH, '^[0-9]{10}$', /BOOLEAN)) $
      then begin
          ERR_MSG, 'Analysis date value of "' + $
                   anlEndDate_YYYYMMDDHH + '" is invalid.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endelse


; Make sure the analysis date is in the past.

  anlEndDate_Julian = YYYYMMDDHH_TO_JULIAN(anlEndDate_YYYYMMDDHH)
  sysTime_Julian = SYSTIME(/JULIAN, /UTC)

  if (sysTime_Julian lt anlEndDate_Julian) then begin
      ERR_MSG, 'Analysis time is in the future. No analysis is possible. ' + $
               '(system time ' + JULIAN_TO_YYYYMMDDHH(sysTime_Julian) + $
               ', analysis time ' + anlEndDate_YYYYMMDDHH + ').'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Duration of aggregation period in hours.

  aggregateStr = GETENV('AGGREGATE_HOURS')
  if (aggregateStr eq '') then begin
      USR_MSG, 'Environment variable AGGREGATE_HOURS not provided. ' + $
               'Using default of 72.'
      aggregateStr = '72'
      aggregate = 72
  endif else begin
      if NOT(STREGEX(aggregateStr, '^[0-9]+$', /BOOLEAN)) then begin
          ERR_MSG, 'AGGREGATE_HOURS value of "' + aggregateStr + $
                   '" is invalid.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      aggregate = FIX(aggregateStr)
      if (aggregate le 0) then begin
          ERR_MSG, 'AGGREGATE_HOURS (value of "' + aggregateStr + '") ' + $
                   'must be a positive SHORT integer.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endelse
  if ((aggregate mod 24) ne 0) then begin
      ERR_MSG, 'AGGREGATE_HOURS (value of "' + aggregateStr + $
               '" must be a multiple of 24.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Duration of analyses used as input.

  durationStr = GETENV('DURATION_HOURS')
  if (durationStr eq '') then begin
      USR_MSG, 'Environment variable DURATION_HOURS not provided. ' + $
               'Using default of 24.'
      durationStr = '24'
      duration = 24
  endif else begin
      if NOT(STREGEX(durationStr, '^[0-9]+$', /BOOLEAN)) then begin
          ERR_MSG, 'DURATION_HOURS value of "' + duration + '" is invalid.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
      duration = FIX(durationStr)
      if (duration le 0) then begin
          ERR_MSG, 'DURATION_HOURS (value of "' + durationStr + '") ' + $
                   'must be a positive SHORT integer.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif
  endelse


;; Define the directory for utility programs.

  ;; utilsDir = GETENV('SFAV2_UTILS_DIR')
  ;; if (utilsDir eq '') then begin
  ;;     if LMGR(/RUNTIME) then $
  ;;         utilsDir = '/operations/gisrs/idl/snowfall_v2/utils' $
  ;;     else $
  ;;         utilsDir = '/nwcdev/nsadev/snowfall_v2_devel/sfav2'
  ;;     USR_MSG, 'Environment variable SFAV2_UTILS_DIR not provided. ' + $
  ;;              'Using default of ' + utilsDir + '.'
  ;; endif
  ;; if NOT(FILE_TEST(utilsDir, /DIR, /READ)) then begin
  ;;     if FILE_TEST(utilsDir, /DIR) then $
  ;;         ERR_MSG, 'Utility directory ' + utilsDir + $
  ;;                  ' is not readable by this user.' $
  ;;     else $
  ;;         ERR_MSG, 'Utility directory ' + utilsDir + ' not found.'
  ;;     if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  ;; endif


; Define the directory for resources (i.e., static/parameter data sets
; used in the analysis).

  resourcesDir = GETENV('SFAV2_RESOURCES_DIR')
  if (resourcesDir eq '') then begin
      if LMGR(/RUNTIME) then $
          resourcesDir = '/operations/gisrs/idl/snowfall_v2/resources' $
      else $
          resourcesDir = '/nwcdev/nsadev/snowfall_v2_devel/sfav2'
      USR_MSG, 'Environment variable SFAV2_RESOURCES_DIR not provided. ' + $
               'Using default of ' + resourcesDir + '.'
  endif
  if NOT(FILE_TEST(resourcesDir, /DIR, /READ)) then begin
      if FILE_TEST(resourcesDir, /DIR) then $
          ERR_MSG, 'Resources directory ' + resourcesDir + $
                   ' is not readable by this user.' $
      else $
          ERR_MSG, 'Resources directory ' + resourcesDir + ' not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Identify locations of shapefiles needed in analysis.

  vectorDir = resourcesDir + '/shapefiles' ; for maps

  if NOT(FILE_TEST(vectorDir, /DIRECTORY, /READ)) then begin
      if FILE_TEST(vectorDir, /DIRECTORY) then $
          ERR_MSG, 'Vector data directory ' + vectorDir + $
                   ' is not readable by this user.' $
      else $
          ERR_MSG, 'Vector data directory ' + vectorDir + ' not found.'
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


; Define the directory for both input and output files.

  ioParentDir = GETENV('SFAV2_INPUT_OUTPUT_DIR')
  if (ioParentDir eq '') then begin
      if LMGR(/RUNTIME) then $
          ioParentDir = '/operations/misc/snowfall_v2' $
      else $
          ioParentDir = '/nwcdev/nsadev/snowfall_v2_output'
      USR_MSG, 'Environment variable SFAV2_INPUT_OUTPUT_DIR not provided. ' + $
               'Using default of ' + ioParentDir + '.'
  endif
  if NOT(FILE_TEST(ioParentDir, /DIR, /WRITE)) then begin
      if FILE_TEST(ioParentDir, /DIR) then $
          ERR_MSG, 'I/O directory ' + ioParentDir + ' is not writable.' $
      else $
          ERR_MSG, 'I/O directory ' + ioParentDir + ' does not exist.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UPDATE GIS OPTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  loginInfo = GET_LOGIN_INFO()

;  updateGISProject = GETENV('UPDATE_GIS_PROJECT')
;  if (updateGISProject eq '') then begin
;      updateGISProject = 0
;  endif else begin
;      if ((updateGISProject ne '0') and $
;          (updateGISProject ne '1')) then begin
;          ERR_MSG, 'Invalid UPDATE_GIS_PROJECT value "' + $
;                   updateGISProject + '".'
;          if NOT(LMGR(/RUNTIME)) then STOP else if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;      endif
;      updateGISProject = FIX(updateGISProject)
;  endelse;

;  if updateGISProject then begin
;      produceTIFFs = 1
;      GISProjectDir = 'sfav2_' + loginInfo.user_name
;      if NOT(FILE_TEST(GISProjectDir, /DIRECTORY)) then begin
;          FILE_MKDIR, GISProjectDir
;      endif
;  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Define the time window for aggregation.

  anlEndDate = STRMID(anlEndDate_YYYYMMDDHH, 0, 4) + '-' + $
               STRMID(anlEndDate_YYYYMMDDHH, 4, 2) + '-' + $
               STRMID(anlEndDate_YYYYMMDDHH, 6, 2) + ' ' + $
               STRMID(anlEndDate_YYYYMMDDHH, 8, 2) + ':00:00'

  startDate_Julian = anlEndDate_Julian - DOUBLE(aggregate) / 24.0D
  startDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(startDate_Julian)
  startDate = STRMID(startDate_YYYYMMDDHH, 0, 4) + '-' + $
              STRMID(startDate_YYYYMMDDHH, 4, 2) + '-' + $
              STRMID(startDate_YYYYMMDDHH, 6, 2) + ' ' + $
              STRMID(startDate_YYYYMMDDHH, 8, 2) + ':00:00'

  numDays = aggregate / 24

  if ((numDays gt 3) or forceSeasonal) then begin
      aggregateStr = STRCOMPRESS(numDays, /REMOVE_ALL)
      aggregateUnits = 'day'
      aggregateUnitsAbb = 'd'
  endif else begin
      aggregateUnits = 'hour'
      aggregateUnitsAbb = 'h'
  endelse


; Define the spatial domain.

  domainLabel = 'CONUS'


; Loop over days and aggregate snowfall snowfall analysis results.

  for lc = 0, numDays - 1 do begin

      date_Julian = anlEndDate_Julian - DOUBLE(numDays - 1 - lc)
      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)


;     Look for the input NetCDF file.

      inputDir = ioParentDir + '/' + $
                 'sfav2_' + STRMID(date_YYYYMMDDHH, 0, 8)

      if NOT(FILE_TEST(inputDir, /DIR, /READ)) then begin
          if FILE_TEST(inputDir, /DIR) then $
              ERR_MSG, 'Input directory ' + inputDir + ' is not readable.' $
          else $
              ERR_MSG, 'Input directory ' + inputDir + ' does not exist.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      ncFile = inputDir + '/' + $
               'sfav2_' + domainLabel + '_'+ $
               durationStr + 'h_' + $
               STRMID(date_YYYYMMDDHH, 0, 10) + '.nc'

      if NOT(FILE_TEST(ncFile)) then begin
          ERR_MSG, 'Input file ' + ncFile + ' not found.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif


;     Read input NetCDF file.

      err = 0

      iid = NCDF_OPEN(ncFile)

      dimID_lat = NCDF_DIMID(iid, 'lat')
      NCDF_DIMINQ, iid, dimID_lat, dummy, latDimSize_

      dimID_lon = NCDF_DIMID(iid, 'lon')
      NCDF_DIMINQ, iid, dimID_lon, dummy, lonDimSize_

      dimID_nv = NCDF_DIMID(iid, 'nv')
      NCDF_DIMINQ, iid, dimID_nv, dummy, nv
      if (nv ne 2) then begin
          ERR_MSG, 'Dimension "nv" must have a size of 2.'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      varID_lat = NCDF_VARID(iid, 'lat')
      if (varID_lat eq -1) then begin
          ERR_MSG, 'Missing variable "lat".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      varInfo = NCDF_VARINQ(iid, varID_lat)
      if (varInfo.ndims ne 1) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lat".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[0] ne dimID_lat) then begin
          ERR_MSG, 'Dimension of variable "lat" must be "lat".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lat" must be DOUBLE.'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      NCDF_VARGET, iid, 'lat', lat_
      NCDF_ATTGET, iid, varID_lat, 'resolution', latRes_

      varID_latBounds = NCDF_VARID(iid, 'lat_bounds')
      if (varID_latBounds eq -1) then begin
          ERR_MSG, 'Missing variable "lat_bounds".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      varInfo = NCDF_VARINQ(iid, varID_latBounds)
      if (varInfo.nDims ne 2) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lat_bounds".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[0] ne dimID_nv) then begin
          ERR_MSG, 'First dimension of variable "lat_bounds" must be "nv".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[1] ne dimID_lat) then begin
          ERR_MSG, 'Second dimension of variable "lat_bounds" must be "lat".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lat_bounds" must be DOUBLE.'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      NCDF_VARGET, iid, 'lat_bounds', latBounds_

      varID_lon = NCDF_VARID(iid, 'lon')
      if (varID_lon eq -1) then begin
          ERR_MSG, 'Missing variable "lon".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      varInfo = NCDF_VARINQ(iid, varID_lon)
      if (varInfo.ndims ne 1) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lon".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[0] ne dimID_lon) then begin
          ERR_MSG, 'Dimension of variable "lon" must be "lon".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lon" must be DOUBLE.'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      NCDF_VARGET, iid, 'lon', lon_
      NCDF_ATTGET, iid, varID_lon, 'resolution', lonRes_

      varID_lonBounds = NCDF_VARID(iid, 'lon_bounds')
      if (varID_lonBounds eq -1) then begin
          ERR_MSG, 'Missing variable "lon_bounds".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      varInfo = NCDF_VARINQ(iid, varID_lonBounds)
      if (varInfo.nDims ne 2) then begin
          ERR_MSG, 'Unexpected dimensions in variable "lon_bounds".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[0] ne dimID_nv) then begin
          ERR_MSG, 'First dimension of variable "lon_bounds" must be "nv".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[1] ne dimID_lon) then begin
          ERR_MSG, 'Second dimension of variable "lon_bounds" must be "lon".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dataType ne 'DOUBLE') then begin
          ERR_MSG, 'Data type of variable "lon_bounds" must be DOUBLE.'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      NCDF_VARGET, iid, 'lon_bounds', lonBounds_

      varID_crs = NCDF_VARID(iid, 'crs')
      if (varID_crs eq -1) then begin
          ERR_MSG, 'Missing variable "crs".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      varID_data = NCDF_VARID(iid, 'Data')
      if (varID_data eq -1) then begin
          ERR_MSG, 'Missing variable "Data".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      varInfo = NCDF_VARINQ(iid, varID_data)
      if (varInfo.ndims ne 2) then begin
          ERR_MSG, 'Unexpected dimensions in variable "Data".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[0] ne dimID_lon) then begin
          ERR_MSG, 'First dimension of variable "Data" must be "lon".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      if (varInfo.dim[1] ne dimID_lat) then begin
          ERR_MSG, 'Second dimension of variable "Data" must be "lat".'
          NCDF_CLOSE, iid
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      NCDF_ATTGET, iid, varID_data, '_FillValue', ndv_

      NCDF_ATTGET, iid, varID_data, 'no_data_value', ndv__
      if (ndv__ ne ndv_) then begin
          ERR_MSG, $
            'Attributes "_FillValue" and "no_data_value" must be the same.'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      NCDF_ATTGET, iid, 'Data', 'units', units_
      if ((units_ ne 'm') and $
          (units_ ne 'meters') and $
          (units_ ne 'Meters')) then begin
          ERR_MSG, 'Invalid "units" attribute of "' + units_ + $
                   '" for input variable "Data". ' + $
                   'Must be "m", "meters", or "Meters".'
          if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
      endif

      minLon_ = lonBounds_[0,0]
      maxLon_ = lonBounds_[1,lonDimSize_ - 1]
      maxLat_ = latBounds_[0,0]
      minLat_ = latBounds_[1,latDimSize_ - 1]


;     Get variable attributes.

      NCDF_ATTGET, iid, varID_lat, 'long_name', latLongName_
      NCDF_ATTGET, iid, varID_lat, 'units',  latUnits_
      NCDF_ATTGET, iid, varID_lat, 'standard_name',  latStandardName_
      NCDF_ATTGET, iid, varID_lon, 'long_name',  lonLongName_
      NCDF_ATTGET, iid, varID_lon, 'units',  lonUnits_
      NCDF_ATTGET, iid, varID_lon, 'standard_name',  lonStandardName_
      NCDF_ATTGET, iid, varID_data, 'institution',  institution_
      NCDF_ATTGET, iid, varID_data, 'gisrs_product_code', GISRSProductCode_
      NCDF_ATTGET, iid, varID_data, 'long_name',  dataLongName_
      NCDF_ATTGET, iid, varID_data, 'standard_name',  dataStandardName_
      NCDF_ATTGET, iid, varID_data, 'satellite_data', satelliteData_
      NCDF_ATTGET, iid, varID_data, 'thematic',  thematic_
      NCDF_ATTGET, iid, varID_data, 'data_are_elevations', dataAreElevations_
      NCDF_ATTGET, iid, varID_data, 'number_of_color_tables', numColTables_
      if ((numColTables_ ne 0) and $
          (numDays gt 3)) then begin
          ;; ERR_MSG, 'WARNING: no support for color tables for ' + $
          ;;          'durations above 72 hours.'
          numColTables_ = 0
      endif
      if (numColTables_ ne 0) then begin
          if (numColTables_ ne 1) then begin
              ERR_MSG, 'No support for multiple color tables.'
              err = 1
          endif
      endif
      if (numColTables_ eq 1) then begin
          NCDF_ATTGET, iid, varID_data, 'color_table_file', colorTableFile_
          NCDF_ATTGET, iid, varID_data, 'color_table_descriptor', $
                       colorTableDescriptor_
       endif
      
      NCDF_ATTGET, iid, varID_data, 'add_offset', addOffset_
      NCDF_ATTGET, iid, varID_data, 'scale_factor', scaleFactor_

      if (lc eq 0) then begin


;         Store geometry variables.

          latDimSize = latDimSize_
          lonDimSize = lonDimSize_
          latRes = latRes_
          lat = lat_
          latBounds = latBounds_
          lonRes = lonRes_
          lon = lon_
          lonBounds = lonBounds_
          ndv = ndv_
          minLon = minLon_
          maxLon = maxLon_
          minLat = minLat_
          maxLat = maxLat_


;         Get, define, and modify global attributes.

          NCDF_ATTGET, iid, 'format_version', formatVersion, /GLOBAL
          NCDF_ATTGET, iid, 'Conventions', conventions, /GLOBAL
          NCDF_ATTGET, iid, 'title', title, /GLOBAL
          title = title + ' This file provides a ' + $
                  aggregateStr + '-' + aggregateUnits + ' ' + $
                  'aggregation of 24-hour analysis results, not ' + $
                  'an independent ' + $
                  aggregateStr + '-' + aggregateUnits + ' analysis.'

          NCDF_ATTGET, iid, 'source', source, /GLOBAL
          SPAWN, 'date -u "+%Y-%m-%d %H:%M:%S"', issTime
          issTime = issTime[0]
          history = issTime + $
                    ' UTC created by module: aggregate_snowfall_v2.pro'
          comment = issTime + $
                    ' UTC created comment: aggregate_snowfall_v2.pro ' + $
                    'written in IDL by Greg Fall and Kent Sparrow, 2017'


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
          if (numColTables eq 1) then begin
              colorTableFile = colorTableFile_
              colorTableDescriptor = colorTableDescriptor_
          endif
          addOffset = addOffset_
          scaleFactor = scaleFactor_

          NCDF_VARGET, iid, 'Data', grid


;         Inventory no-data values, but replace no-data values with zero,
;         temporarily.

          gridCount = ULONG(grid ne ndv)
          ind = WHERE(grid eq ndv, count)
          if (count gt 0) then grid[ind] = 0.0

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

          if (GISRSProductCode_ ne GISRSProductCode) then begin
              ERR_MSG, 'WARNING: inconsisten "gisrs_product_code" ' + $
                       'attributes ' + $
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

          if (ndv_ ne ndv) then begin
              ERR_MSG, 'No-data value mismatch.'
              err = 1
          endif

          if err then begin
              NCDF_CLOSE, iid
              if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
          endif


;         Read the snowfall grid.

          NCDF_VARGET, iid, 'Data', addGrid


;         Inventory no-data values, but replace no-data values with zero,
;         temporarily.

          gridCount = gridCount + ULONG(addGrid ne ndv)

          ind = WHERE(addGrid eq ndv, count)
          if (count gt 0) then addGrid[ind] = 0.0

;           ind = WHERE((grid eq ndv) or (addGrid eq ndv), count)
          grid = grid + addGrid
          addGrid = -1L
;          if (count gt 0) then grid[ind] = ndv

      endelse

      NCDF_CLOSE, iid

  endfor


; Set the accumulation grid to ndv where there was not enough data.

  cutoff = ROUND(0.95 * numDays)
  ind = WHERE(gridCount lt cutoff, count)
  if (count gt 0) then grid[ind] = ndv


; Create output file.

  outputDir = ioParentDir + '/' + $
              'sfav2_' + STRMID(anlEndDate_YYYYMMDDHH, 0, 8)

  if NOT(FILE_TEST(outputDir, /DIR, /WRITE)) then begin
      if FILE_TEST(outputDir, /DIR) then $
          ERR_MSG, 'Output directory ' + outputDir + ' is not writable.' $
      else $
          ERR_MSG, 'Output directory ' + outputDir + ' does not exist.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

; -- GF 20170406 vv --
  if ((numDays gt 3) or forceSeasonal) then $
      ncFile = outputDir + '/' + $
               'sfav2_' + domainLabel + '_' + $
               startDate_YYYYMMDDHH + '_to_' + $
               anlEndDate_YYYYMMDDHH + '.nc' $
  else $
      ncFile = outputDir + '/' + $
               'sfav2_' + domainLabel + '_' + $
               aggregateStr  + aggregateUnitsAbb + '_' + $ 
               anlEndDate_YYYYMMDDHH  + '.nc'
; -- GF 20170406 ^^ --

  id = NCDF_CREATE(ncFile, /NETCDF4_FORMAT, /CLOBBER)

  dimID_lat = NCDF_DIMDEF(id, 'lat', latDimSize)
  dimID_lon = NCDF_DIMDEF(id, 'lon', lonDimSize)
  dimID_nv = NCDF_DIMDEF(id, 'nv', 2)

  varID_lat = NCDF_VARDEF(id, 'lat', [dimID_lat], /DOUBLE)
  varID_latBounds = NCDF_VARDEF(id, 'lat_bounds', [dimID_nv, dimID_lat], $
                                /DOUBLE)

  varID_lon = NCDF_VARDEF(id, 'lon', [dimID_lon], /DOUBLE)
  varID_lonBounds = NCDF_VARDEF(id, 'lon_bounds', [dimID_nv, dimID_lon], $
                                /DOUBLE)

  varID_crs = NCDF_VARDEF(id, 'crs', /SHORT)

  varID_data = NCDF_VARDEF(id, 'Data', [dimID_lon, dimID_lat], /FLOAT, $
                           GZIP = 1, /SHUFFLE, $
                           CHUNK_DIMENSIONS = [lonDimSize, 1])

  NCDF_ATTPUT, id, /GLOBAL, 'format_version', formatVersion
  NCDF_ATTPUT, id, /GLOBAL, 'Conventions', conventions
  NCDF_ATTPUT, id, /GLOBAL, 'title', title
  NCDF_ATTPUT, id, /GLOBAL, 'source', source
  NCDF_ATTPUT, id, /GLOBAL, 'history', history
  NCDF_ATTPUT, id, /GLOBAL, 'comment', comment
  NCDF_ATTPUT, id, /GLOBAL, 'references', 'Not applicable'

  NCDF_ATTPUT, id, varID_lat, 'long_name', latLongName
  NCDF_ATTPUT, id, varID_lat, 'units', latUnits
  NCDF_ATTPUT, id, varID_lat, 'standard_name', latStandardName

  NCDF_ATTPUT, id, varID_lon, 'long_name', lonLongName
  NCDF_ATTPUT, id, varID_lon, 'units', lonUnits
  NCDF_ATTPUT, id, varID_lon, 'standard_name', lonStandardName

  NCDF_ATTPUT, id, varID_data, 'institution', institution
  NCDF_ATTPUT, id, varID_data, 'gisrs_product_code', GISRSProductCode
  NCDF_ATTPUT, id, varID_data, 'long_name', dataLongName
  NCDF_ATTPUT, id, varID_data, 'standard_name', dataStandardName
  NCDF_ATTPUT, id, varID_data, 'satellite_data', satelliteData
  NCDF_ATTPUT, id, varID_data, 'thematic', thematic

  NCDF_ATTPUT, id, varID_data, 'data_are_elevations', dataAreElevations
  NCDF_ATTPUT, id, varID_data, 'number_of_color_tables', numColTables
  if (numColTables eq 1) then begin
      NCDF_ATTPUT, id, varID_data, 'color_table_file', colorTableFile
      NCDF_ATTPUT, id, varID_data, 'color_table_descriptor', $
                   colorTableDescriptor
  endif
  NCDF_ATTPUT, id, varID_data, '_FillValue', ndv, /FLOAT
  NCDF_ATTPUT, id, varID_data, 'units', dataUnits
  NCDF_ATTPUT, id, varID_data, 'add_offset', addOffset
  NCDF_ATTPUT, id, varID_data, 'scale_factor', scaleFactor
  NCDF_ATTPUT, id, varID_data, 'no_data_value', ndv, /FLOAT

  ind = WHERE(grid ne ndv, count)
  if (count eq 0) then begin
      ERR_MSG, 'WARNING: output grid is all no-data values.'
      minValue = ndv
      maxValue = ndv
  endif else begin
      minValue = MIN(grid[ind])
      maxValue = MAX(grid[ind])
  endelse
  ind = -1L

  NCDF_ATTPUT, id, varID_data, 'minimum_data_value', minValue
  NCDF_ATTPUT, id, varID_data, 'maximum_data_value', maxValue
  NCDF_ATTPUT, id, varID_data, 'start_date', startDate + ' UTC'
  NCDF_ATTPUT, id, varID_data, 'stop_date', anlEndDate + ' UTC'
  NCDF_ATTPUT, id, varID_data, 'grid_mapping', 'crs'

  NCDF_ATTPUT, id, varID_crs, 'horizontal_datum', 'WGS84'
  NCDF_ATTPUT, id, varID_crs, 'grid_mapping_name', 'latitude_longitude'
  NCDF_ATTPUT, id, varID_crs, 'longitude_of_prime_meridian', 0.0D
  NCDF_ATTPUT, id, varID_crs, 'semi_major_axis', 6378137.0D
  NCDF_ATTPUT, id, varID_crs, 'inverse_flattening', 298.257223563D
 
  NCDF_ATTPUT, id, varID_lat, 'resolution', latRes
  NCDF_ATTPUT, id, varID_lat, 'origin_offset', 0.5D * latRes
  NCDF_ATTPUT, id, varID_lat, 'bounds', 'lat_bounds'

  NCDF_ATTPUT, id, varID_lon, 'resolution', lonRes
  NCDF_ATTPUT, id, varID_lon, 'origin_offset', 0.5D * lonRes
  NCDF_ATTPUT, id, varID_lon, 'bounds', 'lon_bounds'

  NCDF_VARPUT, id, varID_lat, lat
  NCDF_VARPUT, id, varID_latBounds, latBounds
  NCDF_VARPUT, id, varID_lon, lon
  NCDF_VARPUT, id, varID_lonBounds, lonBounds
  NCDF_VARPUT, id, varID_data, grid

  NCDF_CLOSE, id

  USR_MSG, 'Created output data file ' + ncFile
  
  if ((numDays gt 3) or forceSeasonal) then begin
      pngFile = outputDir + '/' + $
                'sfav2_' + domainLabel + '_' + $
                startDate_YYYYMMDDHH + '_to_' + $
                anlEndDate_YYYYMMDDHH + '.png'
      title = 'National Snowfall Analysis: ' + $
              'accumulation from ' + $
              STRMID(startDate, 0, 10) + ' to ' + $
              STRMID(anlEndDate, 0, 10) + '!C!D' + $
              'Issued ' + issTime + ' UTC'
  endif else begin
      pngFile = outputDir + '/' + $
                'sfav2_' + domainLabel + '_' + $
                aggregateStr + aggregateUnitsAbb + '_' + $ 
                anlEndDate_YYYYMMDDHH  + '.png'
      title = 'National Snowfall Analysis: ' + $
              aggregateStr + '-' + aggregateUnits + ' ' + $
              'accumulation ending ' + $
              STRMID(anlEndDate, 0, 13) + ' UTC!C!D' + $
              'Issued ' + issTime + ' UTC'
  endelse


; Convert meters to inches and make the grid south-to-north

  ind = WHERE(grid eq ndv, count)
  grid = grid / 0.0254
  if (count gt 0) then grid[ind] = ndv
  grid = ROTATE(grid, 7)

  if ((numDays gt 3) or forceSeasonal) then begin

;+
;     For longer aggregations, relative to the "extended" color map in
;     the "le 3 days" case, the color map below is different in the
;     following ways (FYI "CCWG" stands for Color Curve Working Group):
;
;     1. Where the "extended" color map uses 6 shades of blue for the
;        lowest values, this color map uses only 5, excluding the very
;        light blue shade [228, 238, 245] that we added to the CCWG
;        recommendation to represent trace amounts of snowfall for 1-,
;        2-, and 3-day analysis results. In other words, we are
;        exactly using the CCWG "extended" values for blue shades,
;        though the quantities they represent are adjusted to
;        accommodate values up to 1 foot.
;     2. Where the "extended" color map uses 7 shades of
;        yellow-orange-red for "middle tier" values, this color map
;        uses only 6, leaving out the very dark red [54, 0, 0] from
;        the CCWG "extended" color map. I prefer the symmetry of 6
;        white-blue colors for values up to 1 foot (including pure
;        white for zero), 6 yellow-orange-red colors for values
;        between 1 foot and 10 feet, and 6 purple + cyan colors for
;        values above 10 feet.
;     3. After the five purple/violet colors for values above 10
;        feet, we have a single "sky blue" or "cyan" [64, 223, 255]
;        for values above 50 feet. The CCWG recommended a pink value
;        [255,190,232], but we feel the sky blue stands out much
;        better.

      red = [255, $
             189, 107, 049, 008, 008, $
             255, 255, 255, 219, 158, 105, $
             204, 159, 124, 086, 046, $
            064]

      grn = [255, $
             215, 174, 130, 081, 038, $
             255, 196, 135, 020, 000, 000, $
             204, 140, 082, 028, 000, $
            223]

      blu = [255, $
             231, 214, 189, 156, 148, $
             150, 000, 000, 000, 000, 000, $
             255, 216, 165, 114, 051, $
            255]

      edges = [0.0, $
               1.0e-8, 0.1, 1.0, 2.0, 6.0, 12.0, $
               24.0, 36.0, 48.0, 72.0, 96.0, 120.0, $
               180.0, 240.0, 360.0, 480.0, 600.0, 5000.0]

      tickNames = [' ', $
                   'trace', '0.1 in.', '1 in.', '2 in.', '6 in.', '1 ft.', $
                   '2 ft.', '3 ft.', '4 ft.', '6 ft.', '8 ft.', '10 ft.', $
                   '15 ft.', '20 ft.', '30 ft.', '40 ft.', '50 ft.', $
                   '> 50 ft.']

      units = ''

      if (extendedColors eq 1) then begin

;+
;         Normally extendedColors is ignored for aggregations
;         greater than 3 days, but if there is an environment
;         variable called FORCE_EXTENDED_COLORS set to "1", we use
;         the extended color curve even here. Note that the "extended"
;         colors are only "extended" relative to an earlier color
;         curve that showed all amounts above 48 inches with one
;         color. The above default for longer ("seasonal")
;         aggregations already has numerous colors for amounts above
;         48 inches. Using the "extended" color curve simply overrides
;         that color ramp, which is already adequate for large
;         amounts.
;-
          forceExtendedColors = GETENV('FORCE_EXTENDED_COLORS')
          if (forceExtendedColors eq '1') then begin

;+
;             Color curve working group colors for "Snow Amount
;             (Extended)" but with an additional color for "trace"
;             amounts between 0.0 and 0.1. This change adds several
;             shades of violet/purple for amounts above 48 inches.
;-
              red = [255, $
                     228, 189, 107, 049, $
                     008, 008, $
                     255, 255, 255, 219, 158, 105, 054, 204, $
                     159, 124, 086, 046]
              grn = [255, $
                     238, 215, 174, 130, $
                     081, 038, $
                     255, 196, 135, 020, 000, 000, 000, 204, $
                     140, 082, 028, 000]
              blu = [255, $
                     245, 231, 214, 189, $
                     156, 148, $
                     150, 000, 000, 000, 000, 000, 000, 255, $
                     216, 165, 114, 051]

              edges = [0.0, 1.0e-8, $
                       0.1, 1.0, 2.0, 3.0, $
                       4.0, 6.0, $
                       8.0, 12.0, 18.0, 24.0, 30.0, 36.0, 48.0, 60.0, $
                       72.0, 96.0, 120.0, 500.0]
              
              tickNames = [' ', '0', '0.1', '1', '2', '3', '4', '6', '8', $
                           '12', '18', '24', '30', '36', '48', '60', $
                           '72', '96', '120', '>120']

              units = 'inches'

          endif

      endif

  endif else begin

      if (extendedColors eq 1) then begin


;+
;         Color curve working group colors for "Snow Amount
;         (Extended)" but with an additional color for "trace"
;         amounts between 0.0 and 0.1. This change adds several shades
;         of violet/purple for amounts above 48 inches.
;-
          red = [255, $
                 228, 189, 107, 049, $
                 008, 008, $
                 255, 255, 255, 219, 158, 105, 054, 204, $
                 159, 124, 086, 046]
          grn = [255, $
                 238, 215, 174, 130, $
                 081, 038, $
                 255, 196, 135, 020, 000, 000, 000, 204, $
                 140, 082, 028, 000]
          blu = [255, $
                 245, 231, 214, 189, $
                 156, 148, $
                 150, 000, 000, 000, 000, 000, 000, 255, $
                 216, 165, 114, 051]

          edges = [0.0, 1.0e-8, $
                   0.1, 1.0, 2.0, 3.0, $
                   4.0, 6.0, $
                   8.0, 12.0, 18.0, 24.0, 30.0, 36.0, 48.0, 60.0, $
                   72.0, 96.0, 120.0, 500.0]

          tickNames = [' ', '0', '0.1', '1', '2', '3', '4', '6', '8', $
                       '12', '18', '24', '30', '36', '48', '60', $
                       '72', '96', '120', '>120']

      endif else begin

;+
;         Earlier color curve working group colors, with an additional
;         color for "trace" amounts between 0.0 and 0.1, and an
;         additional bin at the top, but no discrimination of snowfall
;         amounts greater than 48 inches.
;-
          red = [255, $
                 228, 189, 107, 049, $
                 008, 008, $
                 255, 255, 255, 219, 158, 105, 043, 076]
          grn = [255, $
                 238, 215, 174, 130, $
                 081, 038, $
                 255, 196, 135, 020, 000, 000, 000, 000]
          blu = [255, $
                 245, 231, 214, 189, $
                 156, 148, $
                 150, 000, 000, 000, 000, 000, 046, 115]

          edges = [0.0, 1.0e-8, $
                   0.1, 1.0, 2.0, 3.0, $
                   4.0, 6.0, $
                   8.0, 12.0, 18.0, 24.0, 30.0, 36.0, 48.0, 500.0]

          tickNames = [' ', '0', '0.1', '1', '2', '3', '4', '6', '8', $
                       '12', '18', '24', '30', '36', '48', '>48']

      endelse

      units = 'inches'

  endelse

; -- GF 20170406 vv --
  ;; ind = WHERE((grid ne ndv) and (grid gt 0.0), count)
  ;; foo = grid[ind]
  ;; bar = SORT(foo)
  ;; indBar = ind[bar]
  ;; fooBar = foo[bar]
  ;; quantile = [0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95]
  ;; for qc = 0, N_ELEMENTS(quantile) - 1 do begin
  ;;     k = fooBar[ROUND(quantile[qc] * (count - 1))]
  ;;     PRINT, 'quantile ' + STRCRA(quantile[qc]) + ': ' + STRCRA(k) + ' inches'
  ;; endfor
  ;; PRINT, 'max: ' + STRCRA(fooBar[count - 1]) + ' inches'
  ;; maxInd = indBar[count - 1]
  ;; maxIndRow = maxInd / lonDimSize
  ;; maxIndCol = maxInd mod lonDimSize
  ;; if (lat[0] gt lat[latDimSize - 1]) then $
  ;;     SNLat = REVERSE(lat) $
  ;; else $
  ;;     SNLat = lat
  ;; PRINT, 'max at coordinates ' + $
  ;;        STRCRA(lon[maxIndCol]) + ', ' + $
  ;;        STRCRA(SNLat[maxIndRow])
  ;; maxSnowfall = MAX(grid, maxInd)
  ;; maxIndRow = maxInd / lonDimSize
  ;; maxIndCol = maxInd mod lonDimSize
  ;; PRINT, 'max ' + STRCRA(maxSnowfall) + ' at coordinates ' + $
  ;;        STRCRA(lon[maxIndCol]) + ', ' + $
  ;;        STRCRA(SNLat[maxIndRow])
; -- GF 20170406 ^^ --

  MAKE_LON_LAT_MAP_PNG_SFAV2,$ 
      grid, ndv, edges, red, grn, blu, $                        
      lonRes, minLon, maxLon, $
      latRes, minLat, maxLat, $
      title, units, $
      pngFile, $      
      TICK_NAMES = tickNames, $
      /SHOW_HIGH, $
      /NO_GRID, /NO_CONTINENTS, /NO_USA, $
      /BLACK_ON_WHITE, $
      MAP_SHAPE_PATH = shapePathList, $
      NOAA_LOGO = resourcesDir + $
                  '/noaa_logo_trans_408x408_32_col.png', $
;      NOAA_LOGO = resourcesDir + '/noaa_logo_trans_400x400.png', $
;      NOAA_LOGO = resourcesDir + '/noaa_logo_3d_384x384.png', $
      /PROTOTYPE

  USR_MSG, 'Created ' + pngFile


; Create a GeoTIFF version.

; -- GF 20170406 vv --
  if ((numDays gt 3) or forceSeasonal) then $
      TIFFFILE = outputDir + '/' + $
                 'sfav2_' + domainLabel + '_' + $
                 startDate_YYYYMMDDHH + '_to_' + $
                 anlEndDate_YYYYMMDDHH + '.tif' $
  else $
      TIFFFile = outputDir + '/' + $
                 'sfav2_' + domainLabel + '_' + $
                 aggregateStr  + aggregateUnitsAbb + '_' + $ 
                 anlEndDate_YYYYMMDDHH  + '.tif'
; -- GF 20170406 ^^ --

  MAKE_GEOTIFF_FROM_GRID, ROTATE(grid, 7), $
                          minLon, maxLat, lonRes, latRes, $
                          TIFFFile, COMPRESS = 1

  USR_MSG, 'Created ' + TIFFFile


  asStatus = 1


BAIL:


  weHaveBailed = 1

  if NOT(LMGR(/RUNTIME)) then begin
      if asStatus then begin
          USR_MSG, 'Execution finished.' & STOP
      endif else begin
          ERR_MSG, 'Execution failed.' & STOP
      endelse
  endif

  if NOT(asStatus) then begin
      MESSAGE, 'Exiting with error status.', /CONTINUE
      if (message ne '') then MESSAGE, message, /CONTINUE
      EXIT, STATUS = 1
  endif

  EXIT, STATUS = 0

end
