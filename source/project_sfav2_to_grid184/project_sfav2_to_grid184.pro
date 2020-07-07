; Project snowfall analysis output to NCEP Grid 184.

; Greg Fall, NWC-North

; 2016-02-01, GF - In projection loop, moved
;                  "if (inputGrid[inn, jnn] eq ndv) then CONTINUE"
;                  below assignment of i1Grid, i2Grid, j1Grid, j2Grid
;                  to avoid creating a "mask" the first time the .sav
;                  file is generated.


; 2017-03-11, KS - Project sfav2 onto NCEP grid184.

FUNCTION PROJECT_LON_LAT_TO_LCC, $
  inputGrid, $ ; unprojected lon/lat grid, north up
  ndv, $       ; input/output no-data value
  minLonIn, $  ; minimum input grid longitude (cell edge) [degrees]
  minLatIn, $  ; minimum input grid latitude (cell edge) [degrees]
  lonResIn, $  ; input longitudinal resolution [degrees]
  latResIn, $  ; input latitudinal resolution [degrees]
  lat1Deg, $   ; 1st std. parallel of projection ("latin1" in GRIB) [degrees]
  lat2Deg, $   ; 2nd std. parallel of projection ("latin2" in GRIB) [degrees]
  latdDeg, $   ; "true scale" latitude of projection
               ; where projection resolution is defined [degrees]
  lonvDeg, $   ; "orientation longitude" or projection
               ; meridian parallel to y axis, along which latitude increases
               ; as the y coordinate increases [degrees]
  dx , $       ; projected grid x resolution [meters]
  dy, $        ; projected grid y resolution [meters]
  nx, $        ; number of rows in the projected grid (must accompany CORNER)
  ny, $        ; number of rows in the projected grid (must accompany CORNER)
  CORNER = corner, $ ; projection grid origin lon and lat (as a 2-element
                     ; array) center coordinates. If these are not present,
                     ; a bounding box that contains the full input grid is
                     ; used, and that box determines the output grid
                     ; dimensions.
  ORIGIN_X_Y = originXY, $
  LON_GRID = lonGrid, $
  LAT_GRID = latGrid, $
  INPUT_IS_WGS84 = inputIsWGS84, $
  SAVFILE = saveFile

  COMMON info, message

  outputGrid = !NULL

; print, 'arg_present nx ', arg_present(nx)
; print, 'n_elements nx ', n_elements(nx)
; print, 'arg_present ny ', arg_present(ny)
; print, 'n_elements ny ', n_elements(ny)
; print, 'keyword set corner ', keyword_set(corner)
; print, 'arg_present corner ', arg_present(corner)
; print, 'n_elements corner ', n_elements(corner)

; Have sample of what output data in GRIB should look like,
; rtma2p5.t12z.pcpn_ndfd.grb2
;
;   % wgrib2 -nxny rtma2p5.t12z.pcpn_ndfd.grb2
;   1:0:(2145 x 1377)
;   % wgrib2 -ijlat 1 1 rtma2p5.t12z.pcpn_ndfd.grb2
;   1:0:(1,1),lon=238.445999,lat=20.191999,val=9.999e+20
;   % wgrib2 -ijlat 1 1377 rtma2p5.t12z.pcpn_ndfd.grb2
;   1:0:(1,1377),lon=229.896613,lat=49.939596,val=9.999e+20
;   % wgrib2 -ijlat 2145 1 rtma2p5.t12z.pcpn_ndfd.grb2
;   1:0:(2145,1),lon=290.791608,lat=20.331815,val=9.999e+20
;   % wgrib2 -ijlat 2145 1377 rtma2p5.t12z.pcpn_ndfd.grb2
;   1:0:(2145,1377),lon=299.114090,lat=50.105472,val=9.999e+20
;   % wgrib2 -grid rtma2p5.t12z.pcpn_ndfd.grb2
;   1:0:grid_template=30:winds(grid):
;   	Lambert Conformal: (2145 x 1377) input WE:SN output WE:SN res 8
;   	Lat1 20.191999 Lon1 238.445999 LoV 265.000000
;   	LatD 25.000000 Latin1 25.000000 Latin2 25.000000
;   	LatSP 0.000000 LonSP 0.000000
;   	North Pole (2145 x 1377) Dx 2539.703000 m Dy 2539.703000 m mode 8
;
; For NCEP grid 184:
;
;   lat1Deg = 25.0D
;   lat2Deg = 25.0D
;   latrDeg = 20.192D
;   lonrDeg = 238.446D
;   latdDeg = 25.0D
;   lonvDeg = 265.0D

; Get input grid size.

  foo = SIZE(inputGrid)
  if (foo[0] ne 2) then STOP
  nxIn = foo[1]
  nyIn = foo[2]

  while (minLonIn lt 0.0D) do minLonIn = minLonIn + 360.0D

  lonAxis = minLonIn + (0.5D + DINDGEN(nxIn)) * lonResIn
  latAxis = minLatIn + (0.5D + DINDGEN(nyIn)) * latResIn

; Define projection parameters and origin. Variable names ending in r (or
; "rRad if in radians) are origin coordinates in different systems (lon/lat,
; polar, or projection)

;  lat1Deg = 25.0D ; first standard parallel ("latin1" in wgrib2)
;  lat2Deg = 25.0D ; second standard parallel ("latin2" in wgrib2)
  lat1Rad = lat1Deg * !DPi / 180.0D
  lat2Rad = lat2Deg * !DPi / 180.0D

;  lonvDeg = 265.0D - 360.0D ; orientation longitude ("lov" in wgrib2)
;  latdDeg = 25.0D

  while (lonvDeg lt 0.0D) do lonvDeg = lonvDeg + 360.0D
  lonvRad = lonvDeg * !DPi / 180.0D
  latdRad = latdDeg * !DPi / 180.0D

; Main projection parameters.

  if (lat1Rad eq lat2Rad) then begin
      n = SIN(lat1Rad)
  endif else begin
      n = ALOG(COS(lat1Rad) / COS(lat2Rad)) / $
          ALOG(TAN(!DPi / 4.0D + lat2Rad / 2.0D) / $
               TAN(!DPi / 4.0D + lat1Rad / 2.0D))
  endelse

  F = COS(lat1Rad) * (TAN(!DPI / 4.0D + lat1Rad / 2.0D))^n / n

  Re = 6371229.0D ; NCEP Sphere. Verified by comparing results of
                  ; "Wgrib2 -Get_Byte 3 15 1" on sample GRIB (returns a value
                  ; of 6) with GRIB2 Table 3.2.

  wgs84_inv_flattening = 298.257223563D
  wgs84_flattening = 1.0D / wgs84_inv_flattening

; Calculate projection origin coordinates (polar) in the projected image
; plane.

  rho0 = Re * F / (TAN(!DPi / 4.0D + 0.5 * latdRad))^n
  theta0 = 0.0D ; trivial

; If the CORNER coordinates were not given, we project the border of the
; input grid to find the minimum and maximum x and y values of the projected
; input data.

  if (KEYWORD_SET(corner) and (N_ELEMENTS(corner) eq 2)) then begin

      if (NOT(ARG_PRESENT(nx)) or NOT(ARG_PRESENT(ny))) then begin
          ERR_MSG, 'Projected grid dimensions (nx, ny) must accompany ' + $
                   'CORNER coordinates.'
          RETURN, -1
      endif

      lonrDeg = DOUBLE(corner[0])
      while (lonrDeg lt 0.0D) do lonrDeg = lonrDeg + 360.0D
      latrDeg = DOUBLE(corner[1])

;     lonrDeg = 238.446D - 360.0D ; grid origin longitude ("lon1" in wgrib2)
;     latrDeg = 20.192D           ; grid origin latitude ("lat1" in wgrib2)
      lonrRad = lonrDeg * !DPi / 180.0D
      latrRad = latrDeg * !DPi / 180.0D

;     Calculate grid origin (NOT projection origin) coordinates (polar).

      rhor = Re * F / TAN(!DPi / 4.0D + latrRad / 2.0D)^n
      thetarRad = n * (lonrRad - lonvRad) ; still degrees

;     Convert origin polar coordinates to Cartesian, with the origin of the
;     Cartesian system at the orientation longitude and grid origin latitude.

      xr = rhor * SIN(thetarRad)
      yr = rho0 - rhor * COS(thetarRad)

;       print, '> xr yr ', xr, yr
;       print, 'lonr, latr ', lonrDeg, latrDeg

  endif else begin

;     Determine output grid size nx, ny, and grid origin xr, yr, based on the
;     bounding box needed to hold all the data.

      rc = 0
      lat = latAxis[rc]
      latRad = lat * !Pi / 180.0D
      rho = Re * F / (TAN(!DPi / 4.0D + latRad / 2.0D)^n)
      for cc = 0, nxIn - 1 do begin
          lon = lonAxis[cc]
          lonRad = lon * !Pi / 180.0D
          theta = n * (lonRad - lonvRad)
          x = rho * SIN(theta)
          y = rho0 - rho * COS(theta)
          if (cc eq 0) then begin
              xMin = x
              xMax = x
              yMin = y
              yMax = y
          endif else begin
              if (x lt xMin) then xMin = x
              if (x gt xMax) then xMax = x
              if (y lt yMin) then yMin = y
              if (y gt yMax) then yMax = y
          endelse
;          print, 'bottom ', lon, lat, x, y
      endfor
      rc = nyIn - 1
      lat = latAxis[rc]
      latRad = lat * !Pi / 180.0D
      rho = Re * F / (TAN(!DPi / 4.0D + latRad / 2.0D)^n)
      for cc = 0, nxIn - 1 do begin
          lon = lonAxis[cc]
          lonRad = lon * !Pi / 180.0D
          theta = n * (lonRad - lonvRad)
          x = rho * SIN(theta)
          y = rho0 - rho * COS(theta)
          if (x lt xMin) then xMin = x
          if (x gt xMax) then xMax = x
          if (y lt yMin) then yMin = y
          if (y gt yMax) then yMax = y
;          print, 'top ', x, y
      endfor
      cc = 0
      lon = lonAxis[cc]
      lonRad = lon * !Pi / 180.0D
      theta = n * (lonRad - lonvRad)
      for rc = 0, nyIn - 1 do begin
          lat = latAxis[rc]
          latRad = lat * !Pi / 180.0D
          rho = Re * F / (TAN(!DPi / 4.0D + latRad / 2.0D)^n)
          x = rho * SIN(theta)
          y = rho0 - rho * COS(theta)
          if (x lt xMin) then xMin = x
          if (x gt xMax) then xMax = x
          if (y lt yMin) then yMin = y
          if (y gt yMax) then yMax = y
;          print, 'left ', x, y
      endfor
      cc = nxIn - 1
      lon = lonAxis[cc]
      lonRad = lon * !Pi / 180.0D
      theta = n * (lonRad - lonvRad)
      for rc = 0, nyIn - 1 do begin
          lat = latAxis[rc]
          latRad = lat * !Pi / 180.0D
          rho = Re * F / (TAN(!DPi / 4.0D + latRad / 2.0D)^n)
          x = rho * SIN(theta)
          y = rho0 - rho * COS(theta)
          if (x lt xMin) then xMin = x
          if (x gt xMax) then xMax = x
          if (y lt yMin) then yMin = y
          if (y gt yMax) then yMax = y
;          print, 'right ', x, y
      endfor

      nx_ = CEIL((xMax - xMin) / dx)
      xPad = 0.5D * (nx_ * dx - (xMax - xMin))
      xr = xMin - xPad + 0.5D * dx ; corner cell center x
      ny_ = CEIL((yMax - yMin) / dy)
      yPad = 0.5D * (ny_ * dy - (yMax - yMin))
      yr = yMin - yPad + 0.5D * dy ; corner cell center y

      if (ARG_PRESENT(nx) and (N_ELEMENTS(nx) ne 0)) then $
        ERR_MSG, 'WARNING: input nx value (' + STRCOMPRESS(nx, /REMOVE_ALL) + $
                 ') will be overridden with nx = ' + $
                 STRCOMPRESS(nx_, /REMOVE_ALL)
      nx = nx_

      if (ARG_PRESENT(ny) and (N_ELEMENTS(ny) ne 0)) then $
        ERR_MSG, 'WARNING: input nx value (' + STRCOMPRESS(ny, /REMOVE_ALL) + $
                 ') will be overridden with ny = ' + $
                 STRCOMPRESS(ny_, /REMOVE_ALL)

      ny = ny_

;      print, '> xr yr ', xr, yr

;     For fun calculate latr and lonr from these.

      rhor = n / ABS(n) * SQRT(xr^2.0D + (rho0 - yr)^2.0D)
      thetarRad = ATAN(xr / (rho0 - yr))

      latrRad = 2.0D * ATAN((Re * F / rhor)^(1.0D / n)) - !DPi / 2.0D
      lonrRad = thetarRad / n + lonvRad

      latrDeg = 180.0D / !Pi * latrRad
      lonrDeg = 180.0D / !Pi * lonrRad

;      print, '> lonr, latr ', lonrDeg, latrDeg

  endelse

; Parameters of the projected grid.

;  nx = 2145
;  ny = 1377
;  dx = 2539.703D / 1000.0D ; km
;  dy = 2539.703D / 1000.0D ; km

  outputGrid_ = MAKE_ARRAY(nx, ny, /FLOAT, VALUE = ndv)

  shortcut = 0

  if KEYWORD_SET(saveFile) then begin

      if FILE_TEST(saveFile) then begin

          shortcut = 1
          RESTORE, saveFile

          foo1 = SIZE(lonGrid)
          foo2 = SIZE(latGrid)
          foo3 = SIZE(iGrid)
          foo4 = SIZE(i1Grid)
          foo5 = SIZE(i2Grid)
          foo6 = SIZE(jGrid)
          foo7 = SIZE(j1Grid)
          foo8 = SIZE(j2Grid)
          if ((foo1[0] ne 2) or $
              (foo2[0] ne 2) or $
              (foo3[0] ne 2) or $
              (foo4[0] ne 2) or $
              (foo5[0] ne 2) or $
              (foo6[0] ne 2) or $
              (foo7[0] ne 2) or $
              (foo8[0] ne 2)) then begin
              ERR_MSG, 'Invalid grid dimensions in save file "' + $
                       saveFile + '". Recalculating.'
              shortcut = 0
          endif

          if ((foo1[1] ne nx) or $
              (foo2[1] ne nx) or $
              (foo3[1] ne nx) or $
              (foo4[1] ne nx) or $
              (foo5[1] ne nx) or $
              (foo6[1] ne nx) or $
              (foo7[1] ne nx) or $
              (foo8[1] ne nx)) then begin
              ERR_MSG, 'Invalid number of columns in arrays in save file "' + $
                       saveFile + '". Recalculating.'
              shortcut = 0
          endif

          if ((foo1[2] ne ny) or $
              (foo2[2] ne ny) or $
              (foo3[2] ne ny) or $
              (foo4[2] ne ny) or $
              (foo5[2] ne ny) or $
              (foo6[2] ne ny) or $
              (foo7[2] ne ny) or $
              (foo8[2] ne ny)) then begin
              ERR_MSG, 'Invalid number of rows in arrays in save file "' + $
                       saveFile + '". Recalculating.'
              shortcut = 0
          endif

;           if ((MIN(i1Grid) lt 0) or $
;               (MIN(i2Grid) lt 0) or $
;               (MIN(j1Grid) lt 0) or $
;               (MIN(j2Grid) lt 0) or $
;               (MAX(i1Grid) ge nxIn) or $
;               (MAX(i2Grid) ge nxIn) or $
;               (MAX(j1Grid) ge nyIn) or $
;               (MAX(j2Grid) ge nyIn)) then begin
;               ERR_MSG, 'Invalid row/column dimensions in save file "' + $
;                        saveFile + '". Recalculating.'
;               shortcut = 0
;           endif

      endif ; Assume saveFile is set so we can create it in this execution.

  endif

  if shortcut then begin

      for rc = 0, ny - 1 do begin

          for cc = 0, nx - 1 do begin

              i = iGrid[cc, rc]
              j = jGrid[cc, rc]

              inn = ROUND(i)
              if ((inn lt 0) or (inn ge nxIn)) then CONTINUE
              jnn = ROUND(j)
              if ((jnn lt 0) or (jnn ge nyIn)) then CONTINUE
              if (inputGrid[inn, jnn] eq ndv) then CONTINUE

              i1 = i1Grid[cc, rc]
              i2 = i2Grid[cc, rc]
              j1 = j1Grid[cc, rc]
              j2 = j2Grid[cc, rc]

              if ((i1 lt 0) or (i1 ge nxIn)) then CONTINUE
              if ((i2 lt 0) or (i2 ge nxIn)) then CONTINUE
              if ((j1 lt 0) or (j1 ge nyIn)) then CONTINUE
              if ((j2 lt 0) or (j2 ge nyIn)) then CONTINUE

              gll = inputGrid[i1, j1]
              glr = inputGrid[i2, j1]
              gur = inputGrid[i2, j2]
              gul = inputGrid[i1, j2]

              wll = (i2 - i) * (j2 - j)
              wlr = (i - i1) * (j2 - j)
              wur = (i - i1) * (j - j1)
              wul = (i2 - i) * (j - j1)

              if (gll eq ndv) or (glr eq ndv) or (gur eq ndv) or (gul eq ndv) $
                then begin
                  wtSum = 0.0
                  if (gll ne ndv) then wtSum = wtSum + wll else wll = 0.0
                  if (glr ne ndv) then wtSum = wtSum + wlr else wlr = 0.0
                  if (gur ne ndv) then wtSum = wtSum + wur else wur = 0.0
                  if (gul ne ndv) then wtSum = wtSum + wul else wul = 0.0
                  if (wtSum lt 0.5) then CONTINUE else begin
                      wll = wll / wtSum
                      wlr = wlr / wtSum
                      wur = wur / wtSum
                      wul = wul / wtSum
                  endelse
              endif

              outputGrid_[cc,rc] = gll * wll + $
                                   glr * wlr + $
                                   gur * wur + $
                                   gul * wul

;               if (((gll eq ndv) or $
;                    (glr eq ndv) or $
;                    (gur eq ndv) or $
;                    (gul eq ndv)) and $
;                   (outputGrid_[cc,rc] ne 0) and $
;                   (outputgrid[cc,rc] ne ndv)) $
;                 then begin
;                   print, (i2 - i) * (j2 - j), wll, gll
;                   print, (i - i1) * (j2 - j), wlr, glr
;                   print, (i - i1) * (j - j1), wur, gur
;                   print, (i2 - i) * (j - j1), wul, gul
;                   print, outputgrid[cc, rc]
;               endif

          endfor

      endfor

  endif else begin

      lonGrid = MAKE_ARRAY(nx, ny, /DOUBLE, value = DOUBLE(ndv))
      latGrid = MAKE_ARRAY(nx, ny, /DOUBLE, value = DOUBLE(ndv))
      iGrid = MAKE_ARRAY(nx, ny, /DOUBLE, value = -1.0D)
      i1Grid = MAKE_ARRAY(nx, ny, /LONG, value = -1L)
      i2Grid = i1Grid
      jGrid = iGrid
      j1Grid = i1Grid
      j2Grid = i1Grid

      for rc = 0, ny - 1 do begin

          yPrime = rc * dy
          y = yr + yPrime       ; relative to projection origin

          for cc = 0, nx - 1 do begin

              xPrime = cc * dx
              x = xr + xPrime   ; relative to projection origin

;             Calculate polar coordinates for x, y.

              rho = n / ABS(n) * SQRT(x^2.0D + (rho0 - y)^2.0D)
              theta = ATAN(x / (rho0 - y))

;             Calculate geometric latitude and longitude (radians) for x, y.

              latRad = 2.0D * ATAN((Re * F / rho)^(1.0D / n)) - !DPi / 2.0D
              lonRad = theta / n + lonvDeg * !DPi / 180.0D

;             Convert geocentric latitude and longitude to degrees.

              lon = lonRad * 180.0D / !DPi
              lat = latRad * 180.0D / !DPi

              lonGrid[cc, rc] = lon
              latGrid[cc, rc] = lat

;           if ((cc eq 0) or (cc eq nx - 1)) then begin
;               if ((rc eq 0) or (rc eq ny - 1)) then begin
;                   PRINT, 'column, row: ', cc, rc
;                   PRINT, 'x, y (relative to grid origin): ', $
;                          STRING(xPrime, FORMAT = '(F25.17)'), ' ', $
;                          STRING(yPrime, FORMAT = '(F25.17)')
;                   PRINT, 'x, y (relative to projection origin): ', $
;                          STRING(x, FORMAT = '(F25.15)'), ' ', $
;                          STRING(y, FORMAT = '(F25.15)')
;                   PRINT, 'geocentric lon, lat: ', $
;                          lonRad * 180.0D / !DPi, $
;                          latRad * 180.0D / !DPi
;               endif
;           endif

              if ((cc eq 0) and (rc eq 0)) then originXY = [x, y]

              if KEYWORD_SET(inputIsWGS84) then begin

;                 Convert latitude (which is geocentric) to geodetic latitude.

                  latGeocentricRad = latRad
                  latGeodeticRad = ATAN(TAN(latGeocentricRad) / $
                                        (1.0D - wgs84_flattening)^2.0D)
                  latGeodeticDeg = latGeodeticRad * 180.0D / !DPi
                  j = (latGeodeticDeg - minLatIn) / latResIn - 0.5D

              endif else begin

                  j = (lat - minLatIn) / latResIn - 0.5D

              endelse

              i = (lon - minLonIn) / lonResIn - 0.5D

              iGrid[cc, rc] = i
              jGrid[cc, rc] = j

              inn = ROUND(i)
              if ((inn lt 0) or (inn ge nxIn)) then CONTINUE
              jnn = ROUND(j)
              if ((jnn lt 0) or (jnn ge nyIn)) then CONTINUE

              i1 = FLOOR(i)
              if ((i1 lt 0) or (i1 ge nxIn)) then CONTINUE
              i2 = i1 + 1L
              if (i2 eq nxIn) then i2 = i1
              j1 = FLOOR(j)
              if ((j1 lt 0) or (j1 ge nyIn)) then CONTINUE
              j2 = j1 + 1L
              if (j2 eq nyIn) then j2 = j1

              i1Grid[cc, rc] = i1
              i2Grid[cc, rc] = i2
              j1Grid[cc, rc] = j1
              j2Grid[cc, rc] = j2

              if (inputGrid[inn, jnn] eq ndv) then CONTINUE

              gll = inputGrid[i1, j1]
              glr = inputGrid[i2, j1]
              gur = inputGrid[i2, j2]
              gul = inputGrid[i1, j2]

              wll = (i2 - i) * (j2 - j)
              wlr = (i - i1) * (j2 - j)
              wur = (i - i1) * (j - j1)
              wul = (i2 - i) * (j - j1)

              if (gll eq ndv) or (glr eq ndv) or (gur eq ndv) or (gul eq ndv) $
                then begin
                  wtSum = 0.0
                  if (gll ne ndv) then wtSum = wtSum + wll else wll = 0.0
                  if (glr ne ndv) then wtSum = wtSum + wlr else wlr = 0.0
                  if (gur ne ndv) then wtSum = wtSum + wur else wur = 0.0
                  if (gul ne ndv) then wtSum = wtSum + wul else wul = 0.0
                  if (wtSum lt 0.5) then CONTINUE else begin
                      wll = wll / wtSum
                      wlr = wlr / wtSum
                      wur = wur / wtSum
                      wul = wul / wtSum
                  endelse
              endif

              outputGrid_[cc,rc] = gll * wll + $
                                   glr * wlr + $
                                   gur * wur + $
                                   gul * wul

          endfor

      endfor

      if KEYWORD_SET(saveFile) then $
        SAVE, lonGrid, latGrid, iGrid, i1Grid, i2Grid, jGrid, j1Grid, j2Grid, $
              originXY, FILENAME = saveFile

  endelse

  outputGrid = TEMPORARY(outputGrid_)
  RETURN, outputGrid

end


PRO PROJECT_SFAV2_TO_GRID184

; Project snowfall analysis output in a NetCDF file on a lon/lat grid to NCEP
; grid 184.

  COMMON info, message

  !QUIET = 1

  pstg184Status = 0
  message = ''

;; Error handler.

;  weHaveBailed = 0
;  CATCH, errorStatus
;  if (errorStatus ne 0) then begin
;      MESSAGE, !Error_State.Msg, /CONTINUE
;      if (weHaveBailed) then EXIT, STATUS = 1
;      GOTO, BAIL
;  endif

; Get arguments from environment variables. These are best set with the
; calling script project_sfav2_to_grid184.sh

; Define the directory for resources (i.e., static/parameter data sets
; used at different points in the analysis).

  resourcesDir = GETENV('SFAV2_RESOURCES_DIR')
  if (resourcesDir eq '') then begin
      if LMGR(/RUNTIME) then $
          resourcesDir = '/operations/gisrs/idl/snowfall_v2/resources' $
      else $
          resourcesDir = '/nwcdev/nsadev/snowfall_v2_devel/sfav2'
      USR_MSG, 'Environment variable SFAV2_RESOURCES_DIR not provided. ' + $
               'Using default of ' + resourcesDir + '.'
  endif
  if NOT(FILE_TEST(resourcesDir, /DIR, /WRITE)) then begin
      if FILE_TEST(resourcesDir, /DIR) then $
          ERR_MSG, 'Resources directory ' + resourcesDir + $
                   ' is not writeable by this user.' $
      else $
          ERR_MSG, 'Resources directory ' + resourcesDir + ' not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  inputPath = GETENV('INPUT_NETCDF_PATH')
  if (inputPath eq '') then begin
      ERR_MSG, 'INPUT_NETCDF_PATH not provided.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if NOT(FILE_TEST(inputPath)) then begin
      ERR_MSG, 'Input NetCDF file "' + inputPath + '" not found.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  clobberFlag = GETENV('CLOBBER_EXISTING_184')
  clobber = 0
  if (clobberFlag ne '') then begin
      if (clobberFlag eq 'TRUE') then clobber = 1 else begin
          if (clobberFlag ne 'FALSE') then begin
              USR_MSG, 'Unknown CLOBBER_EXISTING_184 of "' + $
                       clobberFlag + '"; assuming FALSE.'
          endif
      endelse
  endif

  outputPath = GETENV('OUTPUT_NETCDF_PATH')

  if (outputPath eq '') then begin
      ERR_MSG, 'OUTPUT_NETCDF_PATH not provided.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  if ((clobber eq 0) and FILE_TEST(outputPath)) then begin
      ERR_MSG, 'Output NetCDF file "' + outputPath + '" already exists. ' + $
               'Use environment variable "CLOBBER_EXISTING_184=TRUE" ' + $
               'to overwrite.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

;; inputPath = 'sfav2_CONUS_24h_2016120812.nc'
;;             ;GHCND_SLR_clim_CONUS_Winter_1986_to_2016_450_arc_sec.nc 
;; clobberFlag = 'TRUE'
;; clobber = 1
;; outputPath = 'sfav2_CONUS_24h_2016120812_grid184.nc'


; Read the input file.

  iid = NCDF_OPEN(inputPath)

  dimID_lat = NCDF_DIMID(iid, 'lat')
  NCDF_DIMINQ, iid, dimID_lat, dummy, latDimSize

  dimID_lon = NCDF_DIMID(iid, 'lon')
  NCDF_DIMINQ, iid, dimID_lon, dummy, lonDimSize

  dimID_nv = NCDF_DIMID(iid, 'nv')
  NCDF_DIMINQ, iid, dimID_nv, dummy, nv
  if (nv ne 2) then begin
      ERR_MSG, 'Dimension "nv" must have a size of 2.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  varID_lat = NCDF_VARID(iid, 'lat')
  varInfo = NCDF_VARINQ(iid, varID_lat)
  if (varInfo.ndims ne 1) then begin
      ERR_MSG, 'Unexpected dimensions in variable "lat".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[0] ne dimID_lat) then begin
      ERR_MSG, 'Dimension of variable "lat" must be "lat".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dataType ne 'DOUBLE') then begin
      ERR_MSG, 'Data type of variable "lat" must be DOUBLE.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  NCDF_VARGET, iid, 'lat', lat
  NCDF_ATTGET, iid, varID_lat, 'resolution', latRes

  varID_latBounds = NCDF_VARID(iid, 'lat_bounds')
  varInfo = NCDF_VARINQ(iid, varID_latBounds)
  if (varInfo.nDims ne 2) then begin
      ERR_MSG, 'Unexpected dimensions in variable "lat_bounds".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[0] ne dimID_nv) then begin
      ERR_MSG, 'First dimension of variable "lat_bounds" must be "nv".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[1] ne dimID_lat) then begin
      ERR_MSG, 'Second dimension of variable "lat_bounds" must be "lat".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dataType ne 'DOUBLE') then begin
      ERR_MSG, 'Data type of variable "lat_bounds" must be DOUBLE.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  NCDF_VARGET, iid, 'lat_bounds', latBounds

  varID_lon = NCDF_VARID(iid, 'lon')
  varInfo = NCDF_VARINQ(iid, varID_lon)
  if (varInfo.ndims ne 1) then begin
      ERR_MSG, 'Unexpected dimensions in variable "lon".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[0] ne dimID_lon) then begin
      ERR_MSG, 'Dimension of variable "lon" must be "lon".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dataType ne 'DOUBLE') then begin
      ERR_MSG, 'Data type of variable "lon" must be DOUBLE.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  NCDF_VARGET, iid, 'lon', lon
  NCDF_ATTGET, iid, varID_lon, 'resolution', lonRes

  varID_lonBounds = NCDF_VARID(iid, 'lon_bounds')
  varInfo = NCDF_VARINQ(iid, varID_lonBounds)
  if (varInfo.nDims ne 2) then begin
      ERR_MSG, 'Unexpected dimensions in variable "lon_bounds".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[0] ne dimID_nv) then begin
      ERR_MSG, 'First dimension of variable "lon_bounds" must be "nv".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[1] ne dimID_lon) then begin
      ERR_MSG, 'Second dimension of variable "lon_bounds" must be "lon".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dataType ne 'DOUBLE') then begin
      ERR_MSG, 'Data type of variable "lon_bounds" must be DOUBLE.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  NCDF_VARGET, iid, 'lon_bounds', lonBounds

  varID_crs = NCDF_VARID(iid, 'crs')

  varID_data = NCDF_VARID(iid, 'Data')
  varInfo = NCDF_VARINQ(iid, varID_data)
  if (varInfo.ndims ne 2) then begin
      ERR_MSG, 'Unexpected dimensions in variable "Data".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[0] ne dimID_lon) then begin
      ERR_MSG, 'First dimension of variable "Data" must be "lon".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  if (varInfo.dim[1] ne dimID_lat) then begin
      ERR_MSG, 'Second dimension of variable "Data" must be "lat".'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  NCDF_ATTGET, iid, varID_data, '_FillValue', ndv
  NCDF_ATTGET, iid, varID_data, 'no_data_value', ndv_
  if (ndv_ ne ndv) then begin
      ERR_MSG, 'ATTRIBUTES "_FillValue" and "no_data_value" must be the same.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

  NCDF_ATTGET, iid, 'Data', 'units', units
;  if ((units ne 'm') and $
;      (units ne 'meters') and $
;      (units ne 'Meters')) then begin
;      ERR_MSG, 'Invalid "units" attribute of "' + units + $
;               '" for input variable "Data". ' + $
;               'Must be "m", "meters", or "Meters".'
;      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
;  endif

  minLonIn = lonBounds[0,0]
  maxLonIn = lonBounds[1,lonDimSize - 1]
  maxLatIn = latBounds[0,0]
  minLatIn = latBounds[1,latDimSize - 1]

  NCDF_VARGET, iid, 'Data', inputGrid ; should be north-down

; We read maxLatIn and minLatIn as if the grid were north-down (see above
; code). However, if maxLatIn and minLatIn are reversed (i.e., if maxLatIn is
; the smaller of the two), we assume the input data were already north-up when
; we read them. In that case we switch minLatIn and maxLatIn and leave the
; grid alone. Otherwise, flip the grid so it is north-up, which is what we
; expected to do. In the end, the data will be north-up, and minLatIn and
; maxLatIn will reflect that.

  if (maxLatIn lt minLatIn) then begin
      minLatIn = latBounds[0,0]
      maxLatIn = latBounds[1,latDimSize - 1]
  endif else begin
      inputGrid = ROTATE(inputGrid, 7)
  endelse

; NCEP Grid 184 properties:

  latin1 = 25.0D
  latin2 = 25.0D
  latd = 25.0D
  lonv = 265.0D
  lat1 = 20.192D
  lon1 = 238.446D
  dx = 2539.703D
  dy = 2539.703D
  nx = 2145
  ny = 1377

  outputSfav2Grid = $
    PROJECT_LON_LAT_TO_LCC(inputGrid, $
                           ndv, $
                           minLonIn, minLatIn, lonRes, latRes, $
                           latin1, latin2, latd, lonv, $
                           dx, dy, $
                           nx, ny, $
                           CORNER = [lon1, lat1], $
                           ORIGIN_X_Y = originXY, $
                           LON_GRID = lonGrid, $
                           LAT_GRID = latGrid, $
                           SAVFILE = $
                             resourcesDir + $
                             '/sfav2_lonlat_to_NCEP_grid_184.sav')

  if NOT(ISA(outputSfav2Grid)) then begin
      ERR_MSG, 'Projection to grid 184 failed.'
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif

; Set minimum nonzero output value to 0.001 inches.

  ind = WHERE((outputSfav2Grid ne ndv) and $
              (outputSfav2Grid lt (0.001 * 0.0254)), count)
  if (count gt 0) then outputSfav2Grid[ind] = 0.0

; Write projected grid to a NetCDF file.

  oid = NCDF_CREATE(outputPath, /NETCDF4_FORMAT, /CLOBBER)

  dimID_x = NCDF_DIMDEF(oid, 'x', nx)
  dimID_y = NCDF_DIMDEF(oid, 'y', ny)

  varID_x = NCDF_VARDEF(oid, 'x', [dimID_x], /DOUBLE)
  varID_y = NCDF_VARDEF(oid, 'y', [dimID_y], /DOUBLE)

  varID_lon = NCDF_VARDEF(oid, 'lon', [dimID_x, dimID_y], /DOUBLE, $
                          GZIP = 1, /SHUFFLE, CHUNK_DIMENSIONS = [nx, 1])
  varID_lat = NCDF_VARDEF(oid, 'lat', [dimID_x, dimID_y], /DOUBLE, $
                          GZIP = 1, /SHUFFLE, CHUNK_DIMENSIONS = [nx, 1])

  varID_data = NCDF_VARDEF(oid, 'Data', [dimID_x, dimID_y], /FLOAT, $
                           GZIP = 1, /SHUFFLE, CHUNK_DIMENSIONS = [nx, 1])

  gmVarName = 'Lambert_conformal_conic'
  varID_gm = NCDF_VARDEF(oid, gmVarName, /CHAR)

; Parrot relevant global attributes from the source file.

  NCDF_ATTGET, iid, 'format_version', formatVersion, /GLOBAL
  NCDF_ATTPUT, oid, 'format_version', formatVersion, /GLOBAL

  NCDF_ATTGET, iid, 'Conventions', conventions, /GLOBAL
  NCDF_ATTPUT, oid, 'Conventions', conventions, /GLOBAL

  NCDF_ATTGET, iid, 'title', title, /GLOBAL
  NCDF_ATTPUT, oid, 'title', title + ' This file provides the original output grid projected to NCEP Grid 184 (a Lambert conformal conic projection at 2.539703 km grid resolution).', /GLOBAL

  NCDF_ATTGET, iid, 'source', source, /GLOBAL
  NCDF_ATTPUT, oid, 'source', source, /GLOBAL

  NCDF_ATTGET, iid, 'history', history, /GLOBAL
  SPAWN, 'date -u "+%Y-%m-%d %H:%M:%S"', sysTime
  sysTime = sysTime[0]
  NCDF_ATTPUT, oid, 'history', history + '\n' + sysTime + $
               ' modified by module: reproject_snowfall.pro', /GLOBAL

  NCDF_ATTGET, iid, 'comment', comment, /GLOBAL
  NCDF_ATTPUT, oid, 'comment', $
               comment + '\n' + sysTime + $
               ' UTC modified comment: data projected to NCEP grid 184 by ' + $
               'module project_sfav2_to_grid184.pro, written in IDL by ' + $
               'Greg Fall and Kent Sparrow, 2017-03-11', /GLOBAL

; Give attributes to dimension variables.

  NCDF_ATTPUT, oid, varID_x, 'standard_name', 'projection_x_coordinate'
  NCDF_ATTPUT, oid, varID_x, 'long_name', 'x coordinate of projection'
  NCDF_ATTPUT, oid, varID_x, 'units', 'm'

  NCDF_ATTPUT, oid, varID_y, 'standard_name', 'projection_y_coordinate'
  NCDF_ATTPUT, oid, varID_y, 'long_name', 'y coordinate of projection'
  NCDF_ATTPUT, oid, varID_y, 'units', 'm'

; Parrot relevant attributes for data variable.

  NCDF_ATTGET, iid, 'Data', 'institution', institution
  NCDF_ATTPUT, oid, varID_data, 'institution', institution

  NCDF_ATTPUT, oid, varID_data, 'gisrs_product_code', 0L

  NCDF_ATTGET, iid, 'Data', 'long_name', longName
  NCDF_ATTPUT, oid, varID_data, 'long_name', longName

  NCDF_ATTGET, iid, 'Data', 'standard_name', standardName
  NCDF_ATTPUT, oid, varID_data, 'standard_name', 'thickness_of_snowfall_amount'

  NCDF_ATTGET, iid, 'Data', 'satellite_data', satelliteData
  NCDF_ATTPUT, oid, varID_data, 'satellite_data', satelliteData

  NCDF_ATTGET, iid, 'Data', 'thematic', thematic
  NCDF_ATTPUT, oid, varID_data, 'thematic', thematic

  NCDF_ATTGET, iid, 'Data', 'data_are_elevations', dataAreElevations
  NCDF_ATTPUT, oid, varID_data, 'data_are_elevations', dataAreElevations

  NCDF_ATTPUT, oid, varID_data, 'number_of_color_tables', 0

  NCDF_ATTPUT, oid, varID_data, '_FillValue', ndv

  NCDF_ATTPUT, oid, varID_data, 'units', 'm'
  NCDF_ATTGET, iid, 'Data', 'add_offset', addOffset
  NCDF_ATTPUT, oid, varID_data, 'add_offset', addOffset

  NCDF_ATTGET, iid, 'Data', 'scale_factor', scaleFactor
  NCDF_ATTPUT, oid, varID_data, 'scale_factor', 1.0

  NCDF_ATTPUT, oid, varID_data, 'no_data_value', ndv

  ind = WHERE(outputSfav2Grid ne ndv, count)
  if (count eq 0) then begin
      ERR_MSG, 'Empty output grid (all no-data values).'
      NCDF_CLOSE, iid
      NCDF_CLOSE, oid
      if NOT(LMGR(/RUNTIME)) then STOP else GOTO, BAIL
  endif
  minValue = MIN(outputSfav2Grid[ind])
  maxValue = MAX(outputSfav2Grid[ind])

;  NCDF_ATTGET, iid, 'Data', 'minimum_data_value', minValue
  NCDF_ATTPUT, oid, varID_data, 'minimum_data_value', minValue

;  NCDF_ATTGET, iid, 'Data', 'maximum_data_value', maxValue
  NCDF_ATTPUT, oid, varID_data, 'maximum_data_value', maxValue

  NCDF_ATTGET, iid, 'Data', 'start_date', startDate
  NCDF_ATTPUT, oid, varID_data, 'start_date', startDate

  NCDF_ATTGET, iid, 'Data', 'stop_date', stopDate
  NCDF_ATTPUT, oid, 'Data', 'stop_date', stopDate

  NCDF_CLOSE, iid

; Create the "grid_mapping" variable and its attributes.

  NCDF_ATTPUT, oid, varID_data, 'grid_mapping', gmVarName

  NCDF_ATTPUT, oid, varID_gm, 'grid_mapping_name', 'lambert_conformal_conic'
  NCDF_ATTPUT, oid, varID_gm, 'standard_parallel', latin1, /DOUBLE
  NCDF_ATTPUT, oid, varID_gm, 'longitude_of_central_meridian', $
                              lonv, /DOUBLE
  NCDF_ATTPUT, oid, varID_gm, 'latitude_of_projection_origin', $
                              latd, /DOUBLE
  NCDF_ATTPUT, oid, varID_gm, 'false_easting', 0.0D, /DOUBLE
  NCDF_ATTPUT, oid, varID_gm, 'false_northing', 0.0D, /DOUBLE
  NCDF_ATTPUT, oid, varID_gm, 'semi_major_axis', 6371229.0D, /DOUBLE
  NCDF_ATTPUT, oid, varID_gm, 'inverse_flattening', 0.0D, /DOUBLE

  NCDF_VARPUT, oid, varID_x, originXY[0] + DINDGEN(nx) * dx
  NCDF_VARPUT, oid, varID_y, originXY[1] + DINDGEN(ny) * dy
  NCDF_VARPUT, oid, varID_lon, lonGrid
  NCDF_VARPUT, oid, varID_lat, latGrid
  NCDF_VARPUT, oid, varID_data, outputSfav2Grid

  NCDF_CLOSE, oid

  pstg184Status = 1


BAIL:


  weHaveBailed = 1

  if NOT(pstg184Status) then begin
      MESSAGE, 'Exiting with error status.', /CONTINUE
      if (message ne '') then MESSAGE, message, /CONTINUE
      EXIT, STATUS = 1
  endif

  EXIT, STATUS = 0

end
