PRO GET_LONLAT_NETCDF_DATA, dir, file, $
                            ncGridInfo, $
                            ncDataGrid, $
                            NO_DATA_VALUE = ndv, $
                            UNITS_PROVIDED = units, $
                            UNITS_EXPECTED = unitsExpected

; Read 2-d "Data" variable in lon/lat coordinates from a CF compliant
; NetCDF file.

  COMMON info, message

  ncDataGrid = !NULL


; Check arguments for correct type and valid contents.

  if NOT(ISA(dir, 'STRING')) then begin
      ERR_MSG, 'Location of NetCDF file must be a STRING.'
      RETURN
  endif

  if NOT(FILE_TEST(dir, /DIRECTORY)) then begin
      ERR_MSG, 'Directory "' + dir + '" not found.'
      RETURN
  endif

  if NOT(FILE_TEST(dir, /READ)) then begin
      ERR_MSG, 'Directory "' + dir + '" not readable.'
      RETURN
  endif

  if NOT(FILE_TEST(dir + '/' + file)) then begin
      ERR_MSG, 'NetCDF file ' + $
               dir + '/' + file + ' not found.'
      RETURN
  endif


; Open the NetCDF file.

  id = NCDF_OPEN(dir + '/' + file)


; Get dimensions.

  dimID_lat = NCDF_DIMID(id, 'lat')
  if (dimID_lat eq -1) then begin
      ERR_MSG, 'NetCDF file has no "lat" dimension.'
      RETURN
  endif
  NCDF_DIMINQ, id, dimID_lat, dummy, numLatIn

  dimID_lon = NCDF_DIMID(id, 'lon')
  if (dimID_lon eq -1) then begin
      ERR_MSG, 'NetCDF file has no "lon" dimension.'
      RETURN
  endif
  NCDF_DIMINQ, id, dimID_lon, dummy, numLonIn

  dimID_nv = NCDF_DIMID(id, 'nv')
  if (dimID_nv eq -1) then begin
      ERR_MSG, 'NetCDF file has no "nv" dimension.'
      RETURN
  endif
  NCDF_DIMINQ, id, dimID_nv, dummy, nv
  if (nv ne 2) then begin
      ERR_MSG, 'NetCDF file "nv" dimension has size ' + $
               STRCRA(nv) + ', 2 required.'
      RETURN
  endif


; Check coordinate variables.

  varID_lat = NCDF_VARID(id, 'lat')
  if (varID_lat eq -1) then begin
      ERR_MSG, 'NetCDF file has no coordinate variable "lat".'
      RETURN
  endif
  varInfo_lat = NCDF_VARINQ(id, varID_lat)
  if (varInfo_lat.DataType ne 'DOUBLE') then begin
      ERR_MSG, 'NetCDF coordinate variable "lat" must be type DOUBLE.'
      RETURN
  endif
  if (varInfo_lat.nDims ne 1) then begin
      ERR_MSG, 'NetCDF coordinate variable "lat" must be 1-D.'
      RETURN
  endif
  if (varInfo_lat.dim[0] ne dimID_lat) then begin
      ERR_MSG, 'NetCDF coordinate variable "lat" must use the "lat" ' + $
               'dimension.'
      RETURN
  endif
  latResInfo = NCDF_ATTINQ(id, varID_lat, 'resolution')
  if (latResInfo.dataType ne 'DOUBLE') then begin
      ERR_MSG, 'NetCDF "resolution" attribute for "lat" variable must be ' + $
               'type DOUBLE.'
      RETURN
  endif
  if (latResInfo.length ne 1) then begin
      ERR_MSG, 'NetCDF "resolution" attribute for "lat" variable must be ' + $
               'a scalar.'
      RETURN
  endif

  NCDF_ATTGET, id, varID_lat, 'resolution', latResIn

  varID_lon = NCDF_VARID(id, 'lon')
  if (varID_lon eq -1) then begin
      ERR_MSG, 'NetCDF file has no coordinate variable "lon".'
      RETURN
  endif

  varInfo_lon = NCDF_VARINQ(id, varID_lon)
  if (varInfo_lon.DataType ne 'DOUBLE') then begin
      ERR_MSG, 'NetCDF coordinate variable "lon" must be type DOUBLE.'
      RETURN
  endif
  if (varInfo_lon.nDims ne 1) then begin
      ERR_MSG, 'NetCDF coordinate variable "lon" must be 1-D.'
      RETURN
  endif
  if (varInfo_lon.dim[0] ne dimID_lon) then begin
      ERR_MSG, 'NetCDF coordinate variable "lon" must use the "lon" ' + $
               'dimension.'
      RETURN
  endif
  lonResInfo = NCDF_ATTINQ(id, varID_lon, 'resolution')
  if (lonResInfo.dataType ne 'DOUBLE') then begin
      ERR_MSG, 'NetCDF "resolution" attribute for "lon" variable must be ' + $
               'type DOUBLE.'
      RETURN
  endif
  if (lonResInfo.length ne 1) then begin
      ERR_MSG, 'NetCDF "resolution" attribute for "lon" variable must be ' + $
               'a scalar.'
      RETURN
  endif

  NCDF_ATTGET, id, varID_lon, 'resolution', lonResIn


; Minimal due diligence on coordinate reference system.

  varID_crs = NCDF_VARID(id, 'crs')
  if (varID_crs eq -1) then begin
      ERR_MSG, 'NetCDF file has no "crs" container variable.'
      RETURN
  endif

  NCDF_ATTGET, id, varID_crs, 'grid_mapping_name', crs_gmn
  crs_gmn = STRING(crs_gmn)
  if (crs_gmn ne 'latitude_longitude') then begin
      ERR_MSG, '"grid_mapping_name" attribute for variable "crs" must be ' + $
               '"latitude_longitude".'
      RETURN
  endif


; Get grid bounds.

  varID_latBounds = NCDF_VARID(id, 'lat_bounds')
  if (varID_latBounds eq -1) then begin
      ERR_MSG, 'NetCDF file has to "lat_bounds" variable.'
      RETURN
  endif
  varInfo_latBounds = NCDF_VARINQ(id, varID_latBounds)
  if (varInfo_latBounds.DataType ne 'DOUBLE') then begin
      ERR_MSG, 'NetCDF "lat_bounds" variable must be type DOUBLE.'
      RETURN
  endif
  if (varInfo_latBounds.Ndims ne 2) then begin
      ERR_MSG, 'NetCDF "lat_bounds" variable must be 2-D.'
      RETURN
  endif
  if (varInfo_latBounds.Dim[0] ne dimID_nv) then begin
      ERR_MSG, 'NetCDF "lat_bounds" variable must have "nv" as its ' + $
               'first dimension.'
      RETURN
  endif
  if (varInfo_latBounds.Dim[1] ne dimID_lat) then begin
      ERR_MSG, 'NetCDF "lat_bounds" variable must have "lat" as its ' + $
               'second dimension.'
      RETURN
  endif

  NCDF_VARGET1, id, 'lat_bounds', minLatIn, OFFSET=[0, 0]
  NCDF_VARGET1, id, 'lat_bounds', maxLatIn, OFFSET=[1, numLatIn - 1]

  northDown = 0
  if (maxLatIn lt minLatIn) then begin
      northDown = 1
;      ERR_MSG, '"lat_bounds" variable suggests north-down orientation. ' + $
;               'This procedure will rotate it to north-up.'
      dummy = minLatIn
      minLatIn = maxLatIn
      maxLatIn = dummy
      dummy = !NULL
  endif

  varID_lonBounds = NCDF_VARID(id, 'lon_bounds')
  if (varID_lonBounds eq -1) then begin
      ERR_MSG, 'NetCDF file has to "lon_bounds" variable.'
      RETURN
  endif
  varInfo_lonBounds = NCDF_VARINQ(id, varID_lonBounds)
  if (varInfo_lonBounds.DataType ne 'DOUBLE') then begin
      ERR_MSG, 'NetCDF "lon_bounds" variable must be type DOUBLE.'
      RETURN
  endif
  if (varInfo_lonBounds.Ndims ne 2) then begin
      ERR_MSG, 'NetCDF "lon_bounds" variable must be 2-D.'
      RETURN
  endif
  if (varInfo_lonBounds.Dim[0] ne dimID_nv) then begin
      ERR_MSG, 'NetCDF "lon_bounds" variable must have "nv" as its ' + $
               'first dimension.'
      RETURN
  endif
  if (varInfo_lonBounds.Dim[1] ne dimID_lon) then begin
      ERR_MSG, 'NetCDF "lat_bounds" variable must have "lon" as its ' + $
               'second dimension.'
      RETURN
  endif

  NCDF_VARGET1, id, 'lon_bounds', minLonIn, OFFSET=[0, 0]
  NCDF_VARGET1, id, 'lon_bounds', maxLonIn, OFFSET=[1, numLonIn - 1]

  if (maxLonIn le minLonIn) then begin
      ERR_MSG, '"lon_bounds" variable suggests east-to-west orientation'
  endif


; Verify bounds.

  test = ROUND((maxLatIn - minLatIn) / latResIn)
  if (test ne numLatIn) then begin
      ERR_MSG, 'NetCDF latitude bounds/resolution/dimension inconsistent.'
      RETURN
  endif
  test = ROUND((maxLonIn - minLonIn) / lonResIn)
  if (test ne numLonIn) then begin
      ERR_MSG, 'NetCDF longitude bounds/resolution/dimension inconsistent.'
      RETURN
  endif

  xErr = ABS(numLonIn * lonResIn - (maxLonIn - minLonIn))
  if (xErr gt 1.0D-8) then begin
      ERR_MSG, 'NetCDF coordinates have inconsistent longitudinal ' + $
               'domain/resolution values.'
      RETURN
  endif
  yErr = ABS(numLatIn * latResIn - (maxLatIn - minLatIn))
  if (yErr gt 1.0D-8) then begin
      ERR_MSG, 'NetCDF coordinates have inconsistent latitudinal ' + $
               'domain/resolution values.'
      RETURN
  endif


; Populate "ncGridInfo" structure.

  ncGridInfo = {minLon: minLonIn, $
                maxLon: maxLonIn, $
                lonRes: lonResIn, $
                numLon: numLonIn, $
                minLat: minLatIn, $
                maxLat: maxLatIn, $
                latRes: latResIn, $
                numLat: numLatIn}


; Verify data variable.

  varID_data = NCDF_VARID(id, 'Data')
  if (varID_data eq -1) then begin
      ERR_MSG, 'NetCDF file has no "Data" variable.'
      RETURN
  endif
  varInfo_data = NCDF_VARINQ(id, varID_data)
  if (varInfo_data.Ndims ne 2) then begin
      ERR_MSG, 'NetCDF "Data" variable must be 2-D.'
      RETURN
  endif
  if (varInfo_data.Dim[0] ne dimID_lon) then begin
      ERR_MSG, 'NetCDF "Data" variable must have "lon" as its first ' + $
               'dimension.'
      RETURN
  endif
  if (varInfo_data.Dim[1] ne dimID_lat) then begin
      ERR_MSG, 'NetCDF "Data" variable must have "lat" as its second ' + $
               'dimension.'
      RETURN
  endif
; varInfo_data.DataType will be "BYTE", "CHAR", "INT", "LONG",
; "FLOAT", or "DOUBLE".



; Get attributes for data variable.

  ndvInfo = NCDF_ATTINQ(id, varID_data, '_FillValue')
  NCDF_ATTGET, id, varID_data, '_FillValue', fillValue

  ;; ndvInfo = NCDF_ATTINQ(id, varID_data, 'no_data_value')
  ;; if (ndvInfo.dataType ne varInfo_data.DataType) then begin
  ;;     ERR_MSG, 'WARNING: NetCDF "no_data_value" attribute for "Data" ' + $
  ;;              'is type "' + ndvInfo.dataType + '", and "Data" is ' + $
  ;;              'type "' + varInfo_data.dataType + '".'
  ;; endif

  ;; if (ndvInfo.dataType ne 'FLOAT') then begin
  ;;     ERR_MSG, 'NetCDF "no_data_value" attribute for "Data" must be ' + $
  ;;              'type FLOAT.'
  ;;     RETURN
  ;; endif

  NCDF_ATTGET, id, varID_data, 'no_data_value', ndv
  sfInfo = NCDF_ATTINQ(id, varID_data, 'scale_factor')
  if (sfInfo.length ne 1) then begin
      ERR_MSG, 'NetCDF "scale_factor" attribute for "Data" must be a ' + $
               'scalar.'
      RETURN
  endif
  NCDF_ATTGET, id, varID_data, 'scale_factor', scaleFactor
  aoInfo = NCDF_ATTINQ(id, varID_data, 'add_offset')
  if (aoInfo.length ne 1) then begin
      ERR_MSG, 'NetCDF "add_offset" attribute for "Data" must be a ' + $
               'scalar.'
      RETURN
  endif
  NCDF_ATTGET, id, varID_data, 'add_offset', addOffset
  uInfo = NCDF_ATTINQ(id, varID_data, 'units')
  if ((uInfo.dataType ne 'STRING') and (uInfo.dataType ne 'CHAR')) $
      then begin
      ERR_MSG, 'NetCDF "units" attribute for "Data" must be a STRING ' + $
               'or a CHAR, but the attribute given is of type "' + $
               uInfo.dataType + '".'
      RETURN
  endif
  NCDF_ATTGET, id, varID_data, 'units', units
  units = STRING(units)
  if (KEYWORD_SET(units) and ISA(unitsExpected, 'STRING')) then begin
      if (units ne unitsExpected) then begin
          ERR_MSG, 'NetCDF "units" for "Data" are "' + $
                   units + '", "' + unitsExpected + '" expected.'
          RETURN
      endif
  endif


; Fetch data.

  NCDF_VARGET, id, 'Data', ncDataGrid

  if ((scaleFactor ne 1) or (addOffset ne 0)) then begin


;     Unpack the data using scaleFactor and addOffset.

      ind = WHERE(ncDataGrid eq fillValue, count)
      ncDataGrid = ncDataGrid * scaleFactor + addOffset
      if (count gt 0) then ncDataGrid[ind] = ndv

  endif

  if northDown then ncDataGrid = ROTATE(ncDataGrid, 7)

  NCDF_CLOSE, id

RETURN

end
