; Compare aggregated National Snowfall Analysis with a corresponding
; aggregation over the (2009-2019) period of record.

  ;; start_date = '20200101'
  ;; finish_date = '20200201'
  ;; period_name = 'January'
  ;; period_year = '2020'
  ;; por_str = '2009-2019'
  ;; por_name = 'January'
  ;; period_abbrev = 'jan'
  ;; period_title = 'January 2020'

  ;; period_file = 'jan_2020'



  ;; start_date = '20190930'
  ;; finish_date = '20191101'
  ;; period_file = '20191001_to_20191031'
  ;; por_str = '2009-2019'
  ;; por_name = 'October'
  ;; period_title = 'October 1-31, 2019'
  ;; period_file = '20191001_to_20191031'

; start_date and finish_date are used to locate input data. By
; convention these generally start the day before the period we are
; interested in and end the day after, which means you get an extra 12
; hours at the beginning and 12 hours at the end.

  ;; start_date = '20190930'
  ;; finish_date = '20191101'
  ;; por_years = '2008-2018'
  ;; por_name = 'October'
  ;; period_title = 'October 2019'

  ;; start_date = '20191031'
  ;; finish_date = '20191201'
  ;; por_years = '2008-2018'
  ;; por_name = 'November'
  ;; period_title = 'November 2019'

  ;; start_date = '20191130'
  ;; finish_date = '20200101'
  ;; por_years = '2008-2018'
  ;; por_name = 'December'
  ;; period_title = 'December 2019'

  start_date = '20191231'
  finish_date = '20200201'
  por_years = '2009-2019'
  por_name = 'January'
  period_title = 'January 2020'

  start_date = '20200131'
  finish_date = '20200224'
  por_years = '2009-2019'
  por_name = 'February 1-24'
  period_title = 'February 1-24, 2020'

  start_date = '20190930'
  finish_date = '20200501'
  por_years = '2009-2019'
  por_name = 'September 30 to May 1'
  period_title = '2019-09-30 to 2020-05-01'
  
  start_date = '20201101'
  finish_date = '20201130'
  por_years = '2009-2019'
  por_name = 'November'
  period_title = 'November 2020'

  ;; start_date = '20190930'  ;; finish_date = '20200224'
  ;; period_file = '20191001_to_20200224'
  ;; por_years = '2009-2019'
  ;; por_name = 'October 1 - February 24'
  ;; period_title = 'October 1, 2019 - February 24, 2020'
  ;; period_file = '20191001_to_20200224'

  ;; start_date = '20200131'
  ;; finish_date = '20200214'
  ;; period_name = 'February 1-14'
  ;; period_file = 'early_feb'
  ;; por_years = '2009-2019'
  ;; por_name = 'February 1-14'
  ;; period_title = 'February 1-14, 2020'
  ;; period_file = 'feb_1_to_14_2020'

  period_file = start_date + '_to_' + finish_date

  input_agg_dir = '/operations/misc/snowfall_v2/sfav2_' + finish_date
  input_agg_file = 'sfav2_CONUS_' + $
                   start_date + '12_to_' + finish_date + '12.tif'

  if NOT(FILE_TEST(input_agg_dir + '/' + input_agg_file)) then begin
      PRINT, 'Need ' + input_agg_dir + '/' + input_agg_file
      STOP
  endif

  real_time = READ_TIFF(input_agg_dir + '/' + input_agg_file, $
                        GEOTIFF = geo_data)

  ndv = -99999.0 ; we just know this...

  ; Aggregate period-of-record results.

  start_date_Julian = YYYYMMDDHH_TO_JULIAN(start_date + '12') + 1.0D
  finish_date_Julian = YYYYMMDDHH_TO_JULIAN(finish_date + '12')

  date_Julian = start_date_Julian
  num_days = 0

  por_mean_acc_grid = !NULL
  por_mean_num_grid = !NULL

  while (date_Julian le finish_date_Julian) do begin

      date_YYYYMMDDHH = JULIAN_TO_YYYYMMDDHH(date_Julian)
      por_mean_daily_file = 'sfav2_por_mean_' + $
                            STRMID(date_YYYYMMDDHH, 4, 4) + '.tif'
      por_mean_daily_grid = $
          READ_TIFF(por_mean_daily_file, GEOTIFF = geo_data_)

      if (geo_data_.modelTiePointTag[0] ne geo_data.modelTiePointTag[0]) $
          then STOP
      if (geo_data_.modelTiePointTag[1] ne geo_data.modelTiePointTag[1]) $
          then STOP
      if (geo_data_.modelTiePointTag[2] ne geo_data.modelTiePointTag[2]) $
          then STOP
      if (geo_data_.modelTiePointTag[3] ne geo_data.modelTiePointTag[3]) $
          then STOP
      if (geo_data_.modelTiePointTag[4] ne geo_data.modelTiePointTag[4]) $
          then STOP
      if (geo_data_.modelTiePointTag[5] ne geo_data.modelTiePointTag[5]) $
          then STOP
      if (geo_data_.modelPixelScaleTag[0] ne geo_data.modelPixelScaleTag[0]) $
          then STOP
      if (geo_data_.modelPixelScaleTag[1] ne geo_data.modelPixelScaleTag[1]) $
          then STOP
      if (geo_data_.modelPixelScaleTag[2] ne geo_data.modelPixelScaleTag[2]) $
          then STOP

      por_daily_count_file = 'sfav2_por_num_nonzero_' + $
                             STRMID(date_YYYYMMDDHH, 4, 4) + '.tif'
      por_daily_count_grid = $
          READ_TIFF(por_daily_count_file, GEOTIFF = geo_data_)

      if (geo_data_.modelTiePointTag[0] ne geo_data.modelTiePointTag[0]) $
          then STOP
      if (geo_data_.modelTiePointTag[1] ne geo_data.modelTiePointTag[1]) $
          then STOP
      if (geo_data_.modelTiePointTag[2] ne geo_data.modelTiePointTag[2]) $
          then STOP
      if (geo_data_.modelTiePointTag[3] ne geo_data.modelTiePointTag[3]) $
          then STOP
      if (geo_data_.modelTiePointTag[4] ne geo_data.modelTiePointTag[4]) $
          then STOP
      if (geo_data_.modelTiePointTag[5] ne geo_data.modelTiePointTag[5]) $
          then STOP
      if (geo_data_.modelPixelScaleTag[0] ne geo_data.modelPixelScaleTag[0]) $
          then STOP
      if (geo_data_.modelPixelScaleTag[1] ne geo_data.modelPixelScaleTag[1]) $
          then STOP
      if (geo_data_.modelPixelScaleTag[2] ne geo_data.modelPixelScaleTag[2]) $
          then STOP

      PRINT, 'Read period of record data for ' + STRMID(date_YYYYMMDDHH, 4, 4)

      if NOT(ISA(por_mean_acc_grid)) then begin
          por_mean_acc_grid = por_mean_daily_grid
      endif else begin
          ind = WHERE((por_mean_acc_grid eq ndv) or $
                      (por_mean_daily_grid eq ndv), count)
          por_mean_acc_grid = por_mean_acc_grid + por_mean_daily_grid
          if (count gt 0) then por_mean_acc_grid[ind] = ndv
      endelse

      if NOT(ISA(por_mean_num_grid)) then begin
          por_mean_num_grid = por_daily_count_grid
      endif else begin
          ind = WHERE((por_mean_num_grid eq -1) or $
                      (por_mean_daily_grid eq -1), count)
          por_mean_num_grid = por_mean_num_grid + por_daily_count_grid
          if (count gt 0) then por_mean_num_grid[ind] = -1
      endelse

      date_Julian = date_Julian + 1.0D
      num_days = num_days + 1

  endwhile

  ind = WHERE(por_mean_num_grid eq -1, count)
  por_mean_num_grid = FLOAT(por_mean_num_grid) / FLOAT(num_days)
  if (count gt 0) then por_mean_num_grid[ind] = ndv
  
; Erase the average accumulations where there is too little snow for a
; reliable calculation. The "por_mean_num_grid lt XXX" condition is
; the one that matters here. Note por_mean_num_grid is the average
; (over days) number of years with nonzero snowfall.
; It might be simpler to just not show averages smaller than some
; threshold.
  ;; ind = WHERE((por_mean_acc_grid ne ndv) and $
  ;;             (por_mean_num_grid ne ndv) and $
  ;;             (por_mean_acc_grid gt 0.0) and $
  ;;             (por_mean_num_grid lt 0.5), count)
  ;; if (count gt 0) then por_mean_acc_grid[ind] = 0.0

; 15 - 1
; 60 - 2
; 135 = 4

; Function to determine the "floor" for a period being treated as
; having a nonzero normal. For locations where por_mean_acc_grid is
; less than this amount, it will be set to zero.
  por_floor = ROUND(0.5 + num_days * 0.025)
  por_floor = ROUND(0.5 + num_days * 0.033333333) ; 1/2" + 1" / 30 days

  ind = WHERE((por_mean_acc_grid ne ndv) and $
              (por_mean_num_grid ne ndv) and $
              (por_mean_acc_grid gt 0.0) and $
              (por_mean_acc_grid lt por_floor), count)
  if (count gt 0) then por_mean_acc_grid[ind] = 0.0

  grid_size = SIZE(por_mean_acc_grid)
  numCols = grid_size[1]
  numRows = grid_size[2]
  lonRes = geo_data.modelPixelScaleTag[0]
  latRes = geo_data.modelPixelScaleTag[1]
  minLon = geo_data.modelTiePointTag[3]
  maxLon = minLon + numCols * lonRes
  maxLat = geo_data.modelTiePointTag[4]
  minLat = maxLat - numRows * latRes
  print, minlon, maxlon, lonres
  print, minlat, maxlat, latres


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
      STOP
      ;; if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif


; Identify locations of shapefiles needed in analysis.



  vectorDir = resourcesDir + '/shapefiles' ; for maps

  if NOT(FILE_TEST(vectorDir, /DIRECTORY, /READ)) then begin
      if FILE_TEST(vectorDir, /DIRECTORY) then $
          ERR_MSG, 'Vector data directory ' + vectorDir + $
                   ' is not readable by this user.' $
      else $
          ERR_MSG, 'Vector data directory ' + vectorDir + ' not found.'
      STOP
      ;; if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
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

  pngFile = 'sfav2_CONUS_por_mean_' + period_file + '.png'

  title = 'National Snowfall Analysis: Mean Accumulation for ' + por_name + $
          '!C!D' + 'Period of Record: ' + por_years + $
          ' (averages !Z(2264) ' + STRCRA(por_floor) + ' inches not shown)'

  MAKE_LON_LAT_MAP_PNG_SFAV2,$ 
      REVERSE(por_mean_acc_grid, 2), ndv, edges, red, grn, blu, $
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





  pngFile = 'sfav2_CONUS_' + start_date + '_to_' + finish_date + '.png'

  title = 'National Snowfall Analysis: Total for ' + $
          period_title

  MAKE_LON_LAT_MAP_PNG_SFAV2,$ 
      REVERSE(real_time, 2), ndv, edges, red, grn, blu, $
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









; Calculate % of normal. Note that a "floor" is applied above to
; por_mean_acc_grid.

  p_norm = MAKE_ARRAY(numCols, numRows, VALUE = ndv)

; Normal is zero, current is greater than zero.
  ind = WHERE((real_time ne ndv) and $
              (por_mean_acc_grid ne ndv) and $
              (por_mean_acc_grid eq 0.0) and $
              (por_mean_acc_grid gt 0.0), count)
  if (count gt 0) then p_norm[ind] = 1000.0

; Normal is zero, current is zero.
  ind = WHERE((real_time ne ndv) and $
              (por_mean_acc_grid ne ndv) and $
              (por_mean_acc_grid eq 0.0) and $
              (por_mean_acc_grid eq 0.0), count)
  if (count gt 0) then p_norm[ind] = ndv

; Regular % normal calculation.
  ind = WHERE((real_time ne ndv) and $
              (por_mean_acc_grid ne ndv) and $
              (por_mean_acc_grid gt 0.0), count)
  if (count gt 0) then $
      p_norm[ind] = real_time[ind] / por_mean_acc_grid[ind] * 100.0









; These are taken from the "percent_swe" case in
; ~scarter/SNODAS_Development/SWEpy/nohrsc_colors.py
  colors = [[0.8431372549019608, 0.7568627450980392, 0.611764705882353], $
            [0.8117647058823529, 0.0, 0.29411764705882354], $
            [0.9647058823529412, 0.44313725490196076, 0.0], $
            [1.0, 0.6627450980392157, 0.0], $
            [1.0, 0.8470588235294118, 0.09019607843137255], $
            [1.0, 1.0, 0.49411764705882355], $
            [1.0, 1.0, 0.7450980392156863], $
            [0.9019607843137255, 0.9019607843137255, 1.0], $
            [0.7450980392156863, 1.0, 1.0], $
            [0.49411764705882355, 1.0, 1.0], $
            [0.09019607843137255, 0.8470588235294118, 1.0], $
            [0.0, 0.6627450980392157, 1.0], $
            [0.0, 0.44313725490196076, 0.9647058823529412], $
            [0.29411764705882354, 0.0, 0.9647058823529412], $
            [0.7294117647058823, 0.611764705882353, 0.8431372549019608]]

  colors = TRANSPOSE(colors) * 255

  red = colors[*, 0]
  grn = colors[*, 1]
  blu = colors[*, 2]
  edges = [-1000.0, 0.0, 5.0, 10.0, 15.0, 25.0, 50.0, 75.0, 150.0, 175.0, 200.0, 250.0, 350.0, 500.0, 750.0, 1000.0]

  red = [215, 207, 246, 255, 255, 255, 255, $
         230, 190, 126, 23, 0, 0, 75, 186]
  grn = [192, 0, 113, 169, 216, 255, 255, $
         230, 255, 255, 216, 169, 113, 0, 156]
  blu = [156, 75, 0, 0, 23, 126, 190, $
         255, 255, 255, 255, 255, 246, 246, 215]
  edges = [0.0, 10.0, 20.0, 30.0, 45.0, 60.0, 75.0, 90.0, $
           110.0, 200.0, 300.0, 400.0, 500.0, 750.0, 1000.0, 10000.0]


  red = [191, 207, 246, 255, 230, 23, 0, 75, 191]
  grn = [143, 0, 113, 216, 230, 216, 113, 0, 96]
  blu = [96, 75, 0, 23, 255, 255, 246, 246, 191]


  red = [191, 207, 246, 255, 255,    210,    150, 23, 0, 75, 191]
  grn = [143, 0, 113, 216, 255,    210,    255, 216, 113, 0, 96]
  blu = [96, 75, 0, 23, 150,    240,    255, 255, 246, 246, 191]
  edges = [0.0, 10.0, 25.0, 50.0, 75.0, 90.0, 110.0, 150.0, 250.0, 500.0, 750.0, 1000.0]


  tickNames = ['0', '10', '25', '50', '75', '90', '110', '150', '250', '500', '750', '>750']

  pngFile = 'sfav2_CONUS_percent_normal_' + $
            period_file + '.png'

  title = 'National Snowfall Analysis: Percent of Normal for ' + $
          period_title + $
          '!C!D' + 'Period of Record: ' + por_years + $
          ' (percentage not shown for normals !Z(2264) ' + $
          STRCRA(por_floor) + ' inches)'

  units = 'Percent of Normal'

  MAKE_LON_LAT_MAP_PNG_SFAV2,$ 
      REVERSE(p_norm, 2), ndv, edges, red, grn, blu, $
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



end
