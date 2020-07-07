PRO MAKE_SFAV2_NETCDF, grid_, $ ; north to south, snowfall in inches
                       ndv, $
                       outputPath, $
                       lonRes, minLon, maxLon, $
                       latRes, minLat, maxLat, $
                       durationHours, units, finishDate_YYYYMMDDHH, $
                       issTime_UTC, $ ; YYYY-mm-dd HH:MM:SS
                       msllnStatus, $
                       SLR_CLIM_YEAR_RANGE = SLRYearRange

  msllnStatus = 0

  finishDate = STRMID(finishDate_YYYYMMDDHH, 0, 4) + '-' + $
               STRMID(finishDate_YYYYMMDDHH, 4, 2) + '-' + $
               STRMID(finishDate_YYYYMMDDHH, 6, 2) + ' ' + $
               STRMID(finishDate_YYYYMMDDHH, 8, 2) + ':00:00'
  finishDate_Julian = YYYYMMDDHH_TO_JULIAN(finishDate_YYYYMMDDHH)

  startDate_Julian = finishDate_Julian - DOUBLE(durationHours) / 24.0D
  startDate_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(startDate_Julian)
  startDate = STRMID(startDate_YYYYMMDDHH, 0, 4) + '-' + $
              STRMID(startDate_YYYYMMDDHH, 4, 2) + '-' + $
              STRMID(startDate_YYYYMMDDHH, 6, 2) + ' ' + $
              STRMID(startDate_YYYYMMDDHH, 8, 2) + ':00:00'

  foo = SIZE(grid_)
  if (foo[0] ne 2) then begin
      ERR_MSG, 'Input grid must be two dimensional.'
      RETURN
  endif
  nx = foo[1]
  ny = foo[2]

  if NOT(COMPARE(maxLon, minLon + lonRes * nx)) then begin
      ERR_MSG, 'Longitudinal domain/dimension/resolution mismatch.'
      RETURN
  endif
  if NOT(COMPARE(maxLat, minLat + latRes * ny)) then begin
      ERR_MSG, 'Latitudinal domain/dimension/resolution mismatch.'
      RETURN
  endif

  if (units ne 'inches') then begin
      ERR_MSG, 'Units must be "inches".'
      RETURN
  endif

  ncFile = outputPath

  id = NCDF_CREATE(ncFile, /NETCDF4_FORMAT, /CLOBBER)

  dimID_lat = NCDF_DIMDEF(id, 'lat', ny)
  dimID_lon = NCDF_DIMDEF(id, 'lon', nx)
  dimID_nv = NCDF_DIMDEF(id, 'nv', 2)

  varID_lat = NCDF_VARDEF(id, 'lat', [dimID_lat], /DOUBLE)
  varID_latBounds = NCDF_VARDEF(id, 'lat_bounds', [dimID_nv, dimID_lat], $
                                /DOUBLE)
  varID_lon = NCDF_VARDEF(id, 'lon', [dimID_lon], /DOUBLE)
  varID_lonBounds = NCDF_VARDEF(id, 'lon_bounds', [dimID_nv, dimID_lon], $
                                /DOUBLE)

  varID_crs = NCDF_VARDEF(id, 'crs', /SHORT)

  varID_data = NCDF_VARDEF(id, 'Data', [dimID_lon, dimID_lat], /FLOAT, $
                           GZIP = 1, /SHUFFLE, CHUNK_DIMENSIONS = [nx, 1])

  NCDF_ATTPUT, id, /GLOBAL, 'format_version', 'NOHRSC NetCDF raster file v1.2'
  NCDF_ATTPUT, id, /GLOBAL, 'Conventions', 'CF-1.6'

  if (ISA(SLRYearRange) and (SLRYearRange ne '')) then $
      SLRClimStr = SLRYearRange + ' ' $
  else $
      SLRClimStr = ''

  NCDF_ATTPUT, id, /GLOBAL, 'title', $
               'The National Snowfall Analysis, Version 2.1, is based on a background snowfall estimate generated from Stage IV quantitative precipitation estimates (QPE) (i.e., operational mosaics of QPE generated at River Forecast Centers), with the snow proportion determined by snow water:precipitation ratios in short term High Resolution Rapid Refresh (HRRR), Rapid Refresh (RAP), and Rapid Update Cycle (RUC) forecasts. The resulting accumulated snow water equivalent amount is converted to accumulated snowfall using gridded snowfall-to-liquid-equivalent ratios estimated from a ' + SLRClimStr + 'climatology of daily Global Historical Climatology Network (GHCN-D) precipitation and snowfall observations. Differences between the resulting background estimate and observed 24-hour snowfall observations are interpolated spatially in two ordinary kriging passes, the first for bias adjustment, the second for error correction. This process is performed on a 144 arc-second (0.04 degree) longitude/latitude grid over the Conterminous United States.'

  NCDF_ATTPUT, id, /GLOBAL, 'source', 'surface observations of snowfall; Stage IV QPE; HRRR; GHCN-D'

;  SPAWN, 'date -u "+%Y-%m-%d %H:%M:%S"', sysTime
;  sysTime = sysTime[0]

  NCDF_ATTPUT, id, /GLOBAL, 'history', issTime_UTC + ' UTC created by module: sfav2.pro'

  NCDF_ATTPUT, id, /GLOBAL, 'comment', issTime_UTC + ' UTC created comment: sfav2.pro written in IDL by Greg Fall, Nathan Patrick, and Kent Sparrow; 2016'

  NCDF_ATTPUT, id, /GLOBAL, 'references', 'Not applicable'

  NCDF_ATTPUT, id, varID_lat, 'long_name', 'latitude'
  NCDF_ATTPUT, id, varID_lat, 'units', 'degrees_north'
  NCDF_ATTPUT, id, varID_lat, 'standard_name', 'latitude'

  NCDF_ATTPUT, id, varID_lon, 'long_name', 'longitude'
  NCDF_ATTPUT, id, varID_lon, 'units', 'degrees_east'
  NCDF_ATTPUT, id, varID_lon, 'standard_name', 'longitude'

  NCDF_ATTPUT, id, varID_data, 'institution', 'Office of Water Prediction'

  NCDF_ATTPUT, id, varID_data, 'gisrs_product_code', 623L
  NCDF_ATTPUT, id, varID_data, 'long_name', 'snowfall accumulation'
  NCDF_ATTPUT, id, varID_data, 'standard_name', 'thickness_of_snowfall_amount'
  NCDF_ATTPUT, id, varID_data, 'satellite_data', 'no'
  NCDF_ATTPUT, id, varID_data, 'thematic', 'no'

  NCDF_ATTPUT, id, varID_data, 'data_are_elevations', 'no'
  NCDF_ATTPUT, id, varID_data, 'number_of_color_tables', 0 ; change to 1
;  NCDF_ATTPUT, id, varID_data, 'color_table_file', $
;               '/operations/gisrs/color_tables/snowfall_cr_style.ct'
;  NCDF_ATTPUT, id, varID_data, 'color_table_descriptor', 'default'
  NCDF_ATTPUT, id, varID_data, '_FillValue', ndv, /FLOAT
  NCDF_ATTPUT, id, varID_data, 'units', 'm'
  NCDF_ATTPUT, id, varID_data, 'add_offset', 0.0
  NCDF_ATTPUT, id, varID_data, 'scale_factor', 1.0
  NCDF_ATTPUT, id, varID_data, 'no_data_value', ndv, /FLOAT

  ind = WHERE(grid_ ne ndv, count)
  if (count eq 0) then begin
      NCDF_CLOSE, id
      RETURN
  endif


; Convert inches to meters. CF conventions require it.

  grid = grid_
  grid[ind] = grid[ind] * 0.0254
  minValue = MIN(grid[ind])
  maxValue = MAX(grid[ind])
  ind = -1L
  NCDF_ATTPUT, id, varID_data, 'minimum_data_value', minValue
  NCDF_ATTPUT, id, varID_data, 'maximum_data_value', maxValue
  NCDF_ATTPUT, id, varID_data, 'start_date', startDate + ' UTC'
  NCDF_ATTPUT, id, varID_data, 'stop_date', finishDate + ' UTC'
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

  lonGrid = minLon + (0.5D + DINDGEN(nx)) * lonRes
  lonGridBounds = $
    TRANSPOSE([[minLon + DINDGEN(nx) * lonRes], $
               [minLon + (1.0D + DINDGEN(nx)) * lonRes]])
  latGrid = maxLat - (0.5D + DINDGEN(ny)) * latRes
  latGridBounds = $
    TRANSPOSE([[maxLat - DINDGEN(ny) * latRes], $
               [maxLat - (1.0 + DINDGEN(ny)) * latRes ]])

  NCDF_VARPUT, id, varID_lat, latGrid
  NCDF_VARPUT, id, varID_latBounds, latGridBounds
  NCDF_VARPUT, id, varID_lon, lonGrid
  NCDF_VARPUT, id, varID_lonBounds, lonGridBounds
  NCDF_VARPUT, id, varID_data, grid

  NCDF_CLOSE, id

  msllnStatus = 1

  RETURN

end
