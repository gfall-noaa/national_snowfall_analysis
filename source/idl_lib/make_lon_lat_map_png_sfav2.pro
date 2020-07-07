PRO MAKE_LON_LAT_MAP_PNG_SFAV2, dataGrid, ndv, $
                                binEdges, redIn, grnIn, bluIn, $
                                xRes, xMin, xMax, $
                                yRes, yMin, yMax, $
                                title, $
                                units, $
                                pngImage, $
                                SHOW_LOW = showLow, $
                                SHOW_HIGH = showHigh, $
                                POINT_LON = pointLon, $
                                POINT_LAT = pointLat, $
                                POINT_VAL = pointVal, $
                                TICK_NAMES = tickName, $
                                NDV_RGB = ndv_rgb, $
                                MAP_SHAPE_PATH = $
                                  mapShapePath, $ ; without ".shp"
                                WHITE_POINT_OUTLINES = whiteOutlines, $
                                NO_POINT_OUTLINES = noOutlines, $
                                POINT_BIN_EDGES = pointBinEdges, $
                                POINT_RED = pointRed, $
                                POINT_GRN = pointGrn, $
                                POINT_BLU = pointBlu, $
                                SHOW_POINT_NDV = showPointNDV, $
                                NO_GRID = noGrid, $
                                NO_CONTINENTS = noContinents, $
                                NO_USA = noUSA, $
                                BLACK_ON_WHITE = blackOnWhite, $
                                NOAA_LOGO = NOAALogo, $
                                PROTOTYPE = prototype


; 2016-09-09, GF: added WHITE_POINT_OUTLINES keyword.
; 2019-06-27, GF: modified to generate 8-bit PNG images.

;+
; Check the grid.
;
  nx = ROUND((xMax - xMin) / xRes)
  ny = ROUND((yMax - yMin) / yRes)

  foo = SIZE(dataGrid)
  if (foo[0] ne 2) then STOP
  if (nx ne foo[1]) then STOP
  if (ny ne foo[2]) then STOP

;+
; Add black, gray, and white to the color tables.
;-
  nc = N_ELEMENTS(binEdges) - 1

  if (N_ELEMENTS(redIn) ne nc) then STOP
  if (N_ELEMENTS(grnIn) ne nc) then STOP
  if (N_ELEMENTS(bluIn) ne nc) then STOP
  if KEYWORD_SET(ndv_rgb) then begin
     if (N_ELEMENTS(ndv_rgb) ne 3) then STOP
     ndv_rgb = BYTE(ndv_rgb)
  endif else begin
     ndv_rgb = [200, 200, 200]
  endelse

  red = [000, ndv_rgb[0], 255, redIn]
  grn = [000, ndv_rgb[1], 255, grnIn]
  blu = [000, ndv_rgb[2], 255, bluIn]

  numPointColors = N_ELEMENTS(pointBinEdges) - 1
  if (numPointColors gt 0) then begin
      if (N_ELEMENTS(pointRed) ne numPointColors) then STOP
      if (N_ELEMENTS(pointGrn) ne numPointColors) then STOP
      if (N_ELEMENTS(pointBlu) ne numPointColors) then STOP
      red = [red, pointRed]
      grn = [grn, pointGrn]
      blu = [blu, pointBlu]
  endif

  blackInd = 0
  grayInd = 1
  whiteInd = 2

  numExtraColorsBelow = 3
  numExtraColorsAbove = 0

;+
; Make a color index grid of the data.
;-
  img = BYTARR(nx, ny)
  img[*,*] = grayInd

  for cc = numExtraColorsBelow, nc + numExtraColorsBelow - 1 do begin

      ind = WHERE((dataGrid ne ndv) and $
                  ((dataGrid) ge $
                   binEdges[cc - numExtraColorsBelow]) and $
                  ((dataGrid) lt $
                   binEdges[cc - numExtraColorsBelow + 1]), $
                  count)

      if (count gt 0) then img[ind] = cc

  endfor

  if (KEYWORD_SET(showLow)) then begin

      ind = WHERE((dataGrid ne ndv) and $
                  (dataGrid lt binEdges[0]), $
                  count)

      if (count gt 0) then img[ind] = numExtraColorsBelow

  endif

  if (KEYWORD_SET(showHigh)) then begin

      ind = WHERE((dataGrid ne ndv) and $
                  (dataGrid ge binEdges[nc]), $
                  count)

      if (count gt 0) then img[ind] = nc + numExtraColorsBelow - 1

  endif

;+
; Set up the map projection.
;-
  lonLat = MAP_PROJ_INIT(8, LIMIT = [yMin, xMin, yMax, xMax])

  uRange = lonLat.uv_box[2] - lonLat.uv_box[0]
  vRange = lonLat.uv_box[3] - lonLat.uv_box[1]

  imageScaleFactor = 4.0
  targetWindowSize = 1500 * imageScaleFactor

  gridPos = [0.04d, 0.05d, 0.96d, 0.85d]
  
  nxRange = gridPos[2] - gridPos[0]
  nyRange = gridPos[3] - gridPos[1]

  scaleFactor1 = uRange / nxRange / targetWindowSize
  scaleFactor2 = vRange / nyRange / targetWindowSize

  scaleFactor = scaleFactor1 > scaleFactor2

  xSize = ROUND(uRange / scaleFactor / nxRange)
  ySize = ROUND(vRange / scaleFactor / nyRange)

  oldDeviceName = !D.Name
  if ( oldDeviceName ne 'Z' ) then SET_PLOT, 'Z'
  TVLCT, red, grn, blu
  DEVICE, SET_RESOLUTION = [xSize, ySize], $
          SET_COLORS = nc + numExtraColorsBelow + numExtraColorsAbove

  DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT

;+
; Draw an empty plot to establish the Cartesian coordinate system that
; MAP_PROJ_INIT defined for the graphics device.
;-
  if KEYWORD_SET(blackOnWhite) then $
      PLOT, lonLat.uv_box[[0,2]], $
            lonLat.uv_box[[1,3]], $
            POSITION = gridPos, $
            /NODATA, $
            XSTYLE = 5, $
            YSTYLE = 5, $
            ;/NOERASE, $
            BACKGROUND = whiteInd, $
            COLOR = blackInd $
  else $
      PLOT, lonLat.uv_box[[0,2]], $
            lonLat.uv_box[[1,3]], $
            POSITION = gridPos, $
            /NODATA, $
            XSTYLE = 5, $
            YSTYLE = 5, $
            /NOERASE, $
            BACKGROUND = blackInd, $
            COLOR = whiteInd

;+
; Display the data grid.
;-
  grid = MAP_IMAGE(img, $
                   gstartx, gstarty, gxsize, gysize, $
                   LATMIN = yMin + 0.5d * yRes, $
                   LATMAX = yMax - 0.5d * yRes, $
                   LONMIN = xMin + 0.5d * xRes, $
                   LONMAX = xMax - 0.5d * xRes, $
                   COMPRESS = 1, $
                   MAP_STRUCTURE = lonLat)

  TV, grid, gstartx, gstarty

;+
; Draw all indicated shapefiles.
;-
  if (N_ELEMENTS(mapShapePath) gt 0) then begin

      for sc = 0, N_ELEMENTS(mapShapePath) - 1 do begin
          if NOT(FILE_TEST(mapShapePath[sc] + '.shp')) then begin
              MESSAGE, 'Requested shapefile path "' + mapShapePath[sc] + $
                 '" does not exist.', /CONTINUE
              CONTINUE
          endif

          MAP_SHAPE, mapShapePath[sc], $
                     COLOR = blackInd, MAP_STRUCTURE = lonLat, $
                     MLINETHICK = 0.5 * imageScaleFactor

      endfor

  endif

;+
; Draw continents.
;-
  if NOT(KEYWORD_SET(noContinents)) then $
      MAP_CONTINENTS, /HIRES, MAP_STRUCTURE = lonLat, COLOR = blackInd, $
                      MLINETHICK = 0.5 * imageScaleFactor

;+
; Draw U.S. state boundaries.
;-
  if NOT(KEYWORD_SET(noUSA)) then $
      MAP_CONTINENTS, /HIRES, MAP_STRUCTURE = lonLat, COLOR = blackInd, /USA, $
                      MLINETHICK = 0.5 * imageScaleFactor

;+
; Draw an outline for the black-on-white case. Both gridPos and
; lonLat.uv_box work here.
;-
  if KEYWORD_SET(blackOnWhite) then $
      PLOTS, [lonLat.uv_box[0], $
              lonLat.uv_box[0], $
              lonLat.uv_box[2], $
              lonLat.uv_box[2], $
              lonLat.uv_box[0]], $
             [lonLat.uv_box[1], $
              lonLat.uv_box[3], $
              lonLat.uv_box[3], $
              lonLat.uv_box[1], $
              lonLat.uv_box[1]], $
             COLOR = blackInd, THICK = 2


;+
; Draw lon/lat lines.
;-
  if NOT(KEYWORD_SET(noGrid)) then $
      MAP_GRID, MAP_STRUCTURE = lonLat, COLOR = blackInd, $
                GLINETHICK = 0.5 * imageScaleFactor

  if (KEYWORD_SET(pointLon) and $
      KEYWORD_SET(pointLat) and $
      KEYWORD_SET(pointVal)) then begin

;+
;     Draw point values as colored dots.
;-
      numPoints = N_ELEMENTS(pointVal)

      if (N_ELEMENTS(pointLon) ne numPoints) then STOP
      if (N_ELEMENTS(pointLat) ne numPoints) then STOP

      outlineCol = blackInd
      if KEYWORD_SET(whiteOutlines) then outlineCol = whiteInd

      uvObs = MAP_PROJ_FORWARD([pointLon], [pointLat], $
                               MAP_STRUCTURE = lonLat)
      x = uvObs[0,*]
      y = uvObs[1,*]

      jump = nc * (numPointColors gt 0)

      ;dotRadius = 0.5 * imageScaleFactor
      dotRadius = 0.8 * imageScaleFactor
      a = 2.0 * !Pi * FINDGEN ( 17 ) / 16

;+
;     Loop randomly through data.
;-
      k = RANDOM_UNIQ_IND(numPoints)

      for i = 0, numPoints - 1 do begin

          sc = k[i] ; random point

          case 1 of
              (pointVal[sc] eq ndv): begin
                  if (KEYWORD_SET(showPointNDV) and $
                      NOT(KEYWORD_SET(noOutlines))) then begin
                      USERSYM, dotRadius * COS(a), $
                               dotRadius * SIN(a)
                      PLOTS, x[sc], y[sc], $
                             COLOR = outlineCol, PSYM = 8
                  endif
              end
              ((pointVal[sc] ne ndv) and $
               (pointVal[sc] lt binEdges[0])): begin
                  if KEYWORD_SET(showLow) then begin
                      USERSYM, dotRadius * COS(a), $
                               dotRadius * SIN(a), /FILL
                      PLOTS, x[sc], y[sc], $
                             COLOR = jump + numExtraColorsBelow, $
                             PSYM = 8
                      if NOT(KEYWORD_SET(noOutlines)) then begin
                          USERSYM, dotRadius * COS(a), $
                                   dotRadius * SIN(a)
                          PLOTS, x[sc], y[sc], $
                                 COLOR = outlineCol, PSYM = 8
                      endif
                  endif
              end
              ((pointVal[sc] ne ndv) and $
               (pointVal[sc] ge binEdges[nc])): begin
                  if KEYWORD_SET(showHigh) then begin
                      USERSYM, dotRadius * COS(a), $
                               dotRadius * SIN(a), /FILL
                      PLOTS, x[sc], y[sc], $
                             COLOR = jump + nc + numExtraColorsBelow - 1, $
                             PSYM = 8
                      if NOT(KEYWORD_SET(noOutlines)) then begin
                          USERSYM, dotRadius * COS(a), $
                                   dotRadius * SIN(a)
                          PLOTS, x[sc], y[sc], COLOR = outlineCol, PSYM = 8
                      endif
                  endif
              end
              else: begin
                  cc = MAX(WHERE(pointVal[sc] ge binEdges, count))
                  if (count eq 0) then STOP ; PROGRAMMING ERROR
                  if (cc ge nc) then STOP   ; PROGRAMMING ERROR
                  USERSYM, dotRadius * COS (a), $
                           dotRadius * SIN (a), /FILL
                  PLOTS, x[sc], y[sc], $
                         COLOR = jump + numExtraColorsBelow + cc, $
                         PSYM = 8
                  if NOT(KEYWORD_SET(noOutlines)) then begin
                      USERSYM, dotRadius * COS(a), $
                               dotRadius * SIN(a)
                      PLOTS, x[sc], y[sc], COLOR = outlineCol, PSYM = 8
                  endif
              end
          endcase

      endfor

  endif

;+
; Add the title.
;-
  if KEYWORD_SET(blackOnWhite) then $
      titleColor = blackInd $
  else $
      titleColor = whiteInd

  XYOUTS, 0.5, 0.92, $
          title, $
          ALIGNMENT = 0.5, /NORMAL, $
          SIZE = 2.0 * imageScaleFactor, $
          COLOR = titleColor, $
          CHARTHICK = 1.5 * imageScaleFactor, $
          FONT = 1

;+
; Add a colorbar in the lower right.
;-
  if NOT(KEYWORD_SET(tickName)) then tickName = FORMAT_FLOAT(binEdges)
  ;[0.2, 0.12, 0.8, 0.14], $     ; position xyxy

  GF_COLORBAR, [gridPos[2] - 0.10 * (gridPos[2] - gridPos[0]), $
                gridPos[1] + 0.07 * (gridPos[3] - gridPos[1]), $
                gridPos[2] - 0.08 * (gridPos[2] - gridPos[0]), $
                gridPos[1] + 0.47 * (gridPos[3] - gridPos[1])], $
               binEdges, $ ; values
               numExtraColorsBelow + BINDGEN ( nc ), $ ; color indices
               blackInd, $ ; outline color
               TITLE = units, $
               SIZE_FACTOR = 1.25 * imageScaleFactor, $
               WEIGHT_FACTOR = 1.25 * imageScaleFactor, $
               /VERTICAL, $
               SHOW_LOW = KEYWORD_SET(showLow), $
               SHOW_HIGH = KEYWORD_SET(showHigh), $
               TICK_NAMES = tickName, $
               FONT = 1

  b = TVRD()

  DEVICE, /CLOSE
  if (oldDeviceName ne 'Z') then SET_PLOT, oldDeviceName

;+
; Draw the PNG.
;-
  WRITE_PNG, pngImage, b, red, grn, blu
  ;USR_MSG, 'Original WRITE_PNG output "' + pngImage + '":'
  ;SPAWN, 'identify ' + pngImage

  if KEYWORD_SET(NOAALogo) then begin

;+
;     Add a NOAA logo.
;-
;      NOAALogo = 'noaa_logo_3d_384x384.png'
      if FILE_TEST(NOAALogo) then begin

;          cmd = 'composite -gravity SouthWest -geometry +366+334 ' + $
          cmd = 'composite -gravity SouthWest -geometry +360+320 ' + $
                NOAALogo + ' ' + pngImage + $
                ' ' + pngImage + 'withlogo.png'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'Command "' + cmd + '" failed; status ' + $
                       STRCRA(status)

          if FILE_TEST(pngImage + 'withlogo.png') then $
              FILE_MOVE, pngImage + 'withlogo.png', pngImage, /OVERWRITE

          ;USR_MSG, 'PNG after adding NOAA logo:'
          ;SPAWN, 'identify ' + pngImage
          ;; FILE_COPY, pngImage, '/net/tmp/checkmeout.png'

      endif

  endif

  if KEYWORD_SET(prototype) then begin

      font = 'DejaVu-Sans-Book'
      cmd = 'convert -list font | grep -q Font:\ *' + font
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then begin
          font = 'Bitstream-Vera-Sans-Roman'
          cmd = 'convert -list font | grep -q Font:\ *' + font
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then font = ''
      endif
      fontStr = ''
      if (font ne '') then $
          fontStr = '-font ' + font + ' '

      pointSize = 14 * imageScaleFactor
      xOffset = 72 * imageScaleFactor
      yOffset = 60 * imageScaleFactor

      cmd = 'convert ' + fontstr + $ ;-font DejaVu-Sans-Book ' + $
            '-pointsize ' + STRCRA(pointSize) + ' ' + $
            '-gravity Southeast ' + $
            '-fill ''#000000'' ' + $
            '-annotate 0x0+' + STRCRA(xOffset) + '+' + STRCRA(yOffset) + $
            ' ''prototype'' ' + pngImage + $
            ' ' + pngImage + 'withproto.png'
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then $
          ERR_MSG, 'Command "' + cmd + '" failed; status ' + $
                   STRCRA(status)

      if FILE_TEST(pngImage + 'withproto.png') then $
          FILE_MOVE, pngImage + 'withproto.png', pngImage, /OVERWRITE

      ;USR_MSG, 'PNG after adding "prototype":'
      ;SPAWN, 'identify ' + pngImage

  endif

  scalePercentStr = STRCOMPRESS ( STRING ( 100.0 / imageScaleFactor, $
                                           FORMAT = '(I2)' ), $
                                  /REMOVE_ALL )
  scaleStr = scalePercentStr + '%x' + scalePercentStr + '%'
  cmd = 'mogrify -scale ' + scaleStr + ' ' + pngImage
  SPAWN, cmd, EXIT_STATUS = status
  if ( status ne 0 ) then MESSAGE, 'Command "' + cmd + '" failed; status ' + $
	STRCOMPRESS(status, /REMOVE_ALL), /CONTINUE
  ;USR_MSG, 'PNG after performing "mogrify -scale ' + scaleStr + ':'
  ;SPAWN, 'identify ' + pngImage

;+
; Convert the final image back to a PseudoColor PNG to reduce file
; size.
;-
  ;image = READ_PNG(pngImage)
  ;image = COLOR_QUAN(image, 1, red, grn, blu, COLORS=255, _EXTRA=extra)
  ;WRITE_PNG, pngImage, image, red, grn, blu
  cmd = 'mogrify -type Palette ' + pngImage
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then $
      ERR_MSG, 'Command "' + cmd + '" failed; status ' + STRCRA(status)

  RETURN


end
