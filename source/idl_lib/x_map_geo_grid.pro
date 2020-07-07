PRO X_MAP_GEO_GRID, lonlatDataGrid, $
                    minLonOut, maxLonOut, $
                    minLatOut, maxLatOut, $
                    binEdges, $
                    red, grn, blu, $
                    windowIndex, $
                    mxmStatus, $
                    POSITION = position, $
                    NDV = ndv, $
                    NO_DATA_RGB = no_data_rgb, $
                    SHOW_LOW = showLow, $
                    SHOW_HIGH = showHigh, $
                    XSIZE_TARGET = targetXDisplaySize, $
                    YSIZE_TARGET = targetYDisplaySize, $
                    SHAPE_PATH_LIST = shapePathList, $
                    TITLE = title, $
                    COLORBAR = colorbar, $
                    TICK_NAMES = tickName, $
                    UNITS = units


  mxmStatus = 0


; Check colors.

  numColors = N_ELEMENTS(binEdges) - 1
  if ((N_ELEMENTS(red) ne numColors) or $
      (N_ELEMENTS(grn) ne numColors) or $
      (N_ELEMENTS(blu) ne numColors)) then $
          MESSAGE, 'RGB arrays must have one element less than ' + $
                   'bin-edges array.'

  if (!D.Table_Size lt (numColors + 2)) then $
      MESSAGE, 'Insufficient elements available for color tables.'


; Identify white and black.

  whiteInd = !D.Table_Size - 1
  blackInd = 0
  if NOT(KEYWORD_SET(no_data_rgb)) then no_data_rgb = [200, 200, 160]


; Procedure to display geographic data on a lon/lat grid. My latest
; attempt to make this easy in IDL.

  foo = SIZE(lonLatDataGrid)
  if (foo[0] ne 2) then MESSAGE, 'Input data is not a 2-D array.'
  nx = foo[1]
  ny = foo[2]


; Set display size for raster data (not the window size, just the
; number of device units (pixels) needed to accommodate the grid.

  if NOT(KEYWORD_SET(targetXDisplaySize)) then $
      targetXDisplaySize = 700
  if NOT(KEYWORD_SET(targetYDisplaySize)) then $
      targetYDisplaySize = 420
  if ((nx gt targetXDisplaySize) and $
      (ny gt targetYDisplaySize)) then begin
      xRatio = FLOAT(nx) / FLOAT(targetXDisplaySize)
      yRatio = FLOAT(ny) / FLOAT(targetYDisplaySize)
      f = xRatio > yRatio
      xDisplaySize = ROUND(nx / f)
      yDisplaySize = ROUND(ny / f)
  endif else begin
      xRatio = FLOAT(targetXDisplaySize) / FLOAT(nx)
      yRatio = FLOAT(targetYDisplaySize) / FLOAT(ny)
      f = xRatio < yRatio
      xDisplaySize = ROUND(nx * f)
      yDisplaySize = ROUND(ny * f)
  endelse
  ;; message, 'target/real x/y display sizes: target ' + $
  ;;          STRCRA(targetXDisplaySize) + '/' + $
  ;;          STRCRA(targetYDisplaySize) + ', real ' + $
  ;;          STRCRA(xDisplaySize) + '/' + $
  ;;          STRCRA(yDisplaySize), /continue


; Set position of grid in normalized coordinates.

  if KEYWORD_SET(position) then begin
      if (N_ELEMENTS(position) ne 4) then $
          MESSAGE, 'POSITION must be a 4-element array; ignoring.', /CONTINUE
      mapPos = DOUBLE(position)
  endif else begin
      mapPos = [0.05D, 0.15D, 0.95D, 0.9D]
  endelse


; Calculate number of device units (pixels) needed to accommodate
; mapPos in x and y dimensions.

  xDevPerNorm = DOUBLE(xDisplaySize) / (mapPos[2] - mapPos[0])
  yDevPerNorm = DOUBLE(yDisplaySize) / (mapPos[3] - mapPos[1])


; Calculate padding around grid data in device units (pixels).

  padLeft = mapPos[0] * xDevPerNorm
  padRight = (1.0D - DOUBLE(mapPos[2])) * xDevPerNorm
  padBottom = mapPos[1] * yDevPerNorm
  padTop = (1.0D - DOUBLE(mapPos[3])) * yDevPerNorm


; Calculate window size needed to accommodate xDisplaySize and
; yDisplaySize for grid data with position mapPos.

  xWindowSize = ROUND(xDisplaySize + padLeft + padRight)
  yWindowSize = ROUND(yDisplaySize + padBottom + padTop)

;  PRINT, 'Window dimensions: ', xWindowSize, yWindowSize


; Identify lower left corner of grid in device units (pixels).

  xllPos = padLeft
  yllPos = padBottom


; Draw the window.

  WSET_OR_WINDOW, windowIndex, XSIZE = xWindowSize, YSIZE = yWindowSize, $
                  /ERASE


; Flood the window with white.

  TV, REPLICATE(whiteInd, xWindowSize, yWindowSize)


; Draw the raster. Note that CMAP_TVSCL, in addition to adding the
; contents of red, grn, and blu to the color table, establishes black
; as color index 0 and white as color index !d.table_size - 1.

  CMAP_TVSCL, CONGRID(lonLatDataGrid, xDisplaySize, yDisplaySize), $
              binEdges, red, grn, blu, $
              xllPos, yllPos, $
              SHOW_LOW = showLow, $
              SHOW_HIGH = showHigh, $
              NDV = ndv, $
              NO_DATA_RGB = no_data_rgb


; Set up the lon/lat map projection for the domain. This basically
; creates a mapping between map coordinates and Cartesian coordinates
; u and v. Just a linear function for lon-lat data.

  lonLat = $
      MAP_PROJ_INIT(8, LIMIT = [minLatOut, minLonOut, maxLatOut, maxLonOut])


; Establish coordinate system for vector data (borders/boundaries).

  PLOT, lonLat.uv_box[[0,2]], $
        lonLat.uv_box[[1,3]], $
        POSITION = mapPos, $
        /NODATA, XSTYLE = 5, YSTYLE = 5, /NOERASE

  if (N_ELEMENTS(shapePathList) gt 0) then begin


;     Draw polygons providied as a list of shapefiles.

      for sc = 0, N_ELEMENTS(shapePathList) - 1 do begin
          if NOT(FILE_TEST(shapePathList[sc] + '.shp')) then begin
              MESSAGE, 'Missing shapefile "' + shapePathList[sc] + '".', $
                       /CONTINUE
              CONTINUE
          endif
          MAP_SHAPE, shapePathList[sc], $
                     MAP_STRUCTURE = lonLat, COLOR = blackInd
      endfor

  endif

  if KEYWORD_SET(title) then begin

;     Add a title.

      XYOUTS, 0.5, 0.95, title, SIZE = 1.25, ALIGNMENT = 0.5, /NORMAL, $
              COLOR = blackInd

  endif

  if KEYWORD_SET(colorbar) then begin

;     Add a colorbar.

      if NOT(KEYWORD_SET(tickName)) then begin
          tickName = FORMAT_FLOAT(binEdges)
          if KEYWORD_SET(showLow) then begin
              tickName[0] = '<' + tickName[0]
          endif
          if KEYWORD_SET(showHigh) then begin
              tickName[numColors] = tickName[numColors] + '+'
          endif
      endif

      if NOT(KEYWORD_SET(units)) then units = ''

      cbx1 = mapPos[0] + 0.1 * (mapPos[2] - mapPos[0])
      cbx2 = mapPos[2] - 0.1 * (mapPos[2] - mapPos[0])
      cby1 = 0.5 * mapPos[1]
      cby2 = 0.75 * mapPos[1]

      if ((cby2 le cby1) or $
          (cbx2 le cbx1)) then begin
          MESSAGE, 'Inadequate space for colorbar.', /CONTINUE
      endif else begin
          GF_COLORBAR, $
              [cbx1, cby1, cbx2, cby2], $
              binEdges, $
              1 + INDGEN(numColors), $
              blackInd, $
              TITLE = units, $
              TICK_NAMES = tickName, $
              SHOW_LOW = showLow, SHOW_HIGH = showHigh
      endelse

  endif

  mxmStatus = 1

  RETURN

end
