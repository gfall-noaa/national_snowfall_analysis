
PRO X_MAP_GEO_POINTS, lon, lat, val, $
                      minLonOut, maxLonOut, $
                      minLatOut, maxLatOut, $
                      binEdges, $
                      red, grn, blu, $
                      windowIndex, $
                      mxmStatus, $
                      POSITION = position, $
                      NDV = ndv, $
                      SHOW_LOW = showLow, $
                      SHOW_HIGH = showHigh, $
                      XSIZE_TARGET = targetXDisplaySize, $
                      YSIZE_TARGET = targetYDisplaySize, $
                      SHAPE_PATH_LIST = shapePathList, $
                      SYMBOL_SIZE = symSize, $
                      TITLE = title, $
                      COLORBAR = colorbar, $
                      UNITS = units, $
                      MAP_STRUCT = lonLat, $
                      OUTLINES = outlines


  mxmStatus = 0


; Procedure to display geographic point data on a lon/lat grid. My latest
; attempt to make this easy in IDL.

  n = N_ELEMENTS(val)
  if (N_ELEMENTS(lon) ne n) then $
      MESSAGE, 'Array of longitudes must match data.'
  if (N_ELEMENTS(lat) ne n) then $
      MESSAGE, 'Array of latitudes must match data.'


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


; Set up color table.

  TVLCT, red_, grn_, blu_, /GET

  red_[blackInd] = 0
  grn_[blackInd] = 0
  blu_[blackInd] = 0

  red_[whiteInd] = 255
  grn_[whiteInd] = 255
  blu_[whiteInd] = 255

  red_[1:numColors] = red
  grn_[1:numColors] = grn
  blu_[1:numColors] = blu

  TVLCT, red_, grn_, blu_


; Set display size for raster data (not the window size, just the
; number of device units (pixels) needed to accommodate the grid.

  if NOT(KEYWORD_SET(targetXDisplaySize)) then $
      targetXDisplaySize = 700
  if NOT(KEYWORD_SET(targetYDisplaySize)) then $
      targetYDisplaySize = 420

  nx = ROUND((maxLonOut - minLonOut) * 120.0) ; simulated 30 arc sec grid
  ny = ROUND((maxLatOut - minLatOut) * 120.0) ; simulated 30 arc sec grid

  if ((nx gt targetXDisplaySize) and $
      (ny gt targetYDisplaySize)) then begin
      xRatio = FLOAT(nx) / FLOAT(targetXDisplaySize)
      yRatio = FLOAT(ny) / FLOAT(targetYDisplaySize)
      f = xRatio < yRatio
      xDisplaySize = ROUND(nx / f)
      yDisplaySize = ROUND(ny / f)
  endif else begin
;      xDisplaySize = nx
;      yDisplaySize = ny
      xRatio = FLOAT(targetXDisplaySize) / FLOAT(nx)
      yRatio = FLOAT(targetYDisplaySize) / FLOAT(ny)
      f = xRatio < yRatio
      xDisplaySize = ROUND(nx * f)
      yDisplaySize = ROUND(ny * f)
  endelse


; Set position of map in normalized coordinates.

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


; Calculate padding around map in device units (pixels).

  padLeft = mapPos[0] * xDevPerNorm
  padRight = (1.0D - DOUBLE(mapPos[2])) * xDevPerNorm
  padBottom = mapPos[1] * yDevPerNorm
  padTop = (1.0D - DOUBLE(mapPos[3])) * yDevPerNorm


; Calculate window size needed to accommodate xDisplaySize and
; yDisplaySize for map with position mapPos.

  xWindowSize = ROUND(xDisplaySize + padLeft + padRight)
  yWindowSize = ROUND(yDisplaySize + padBottom + padTop)


; Identify lower left corner of grid in device units (pixels).

  xllPos = padLeft
  yllPos = padBottom


; Draw the window.

  WSET_OR_WINDOW, windowIndex, XSIZE = xWindowSize, YSIZE = yWindowSize, $
                  /ERASE


; Flood the window with white.

  TV, REPLICATE(whiteInd, xWindowSize, yWindowSize)


; Set up the lon/lat map projection for the domain. This basically
; creates a mapping between map coordinates and Cartesian coordinates
; u and v. Just a linear function for lon-lat data.

  lonLat = $
      MAP_PROJ_INIT(8, LIMIT = [minLatOut, minLonOut, maxLatOut, maxLonOut])
  uRange = lonLat.uv_box[2] - lonLat.uv_box[0]
  vRange = lonLat.uv_box[3] - lonLat.uv_box[1]


; Establish the coordinate system for vector data (borders/boundaries).

  PLOT, lonLat.uv_box[[0,2]], $
        lonLat.uv_box[[1,3]], $
        POSITION = mapPos, $
        /NODATA, XSTYLE = 5, YSTYLE = 5, /NOERASE


; Draw a frame.

  PLOTS, [lonLat.uv_box[0], lonLat.uv_box[2], lonLat.uv_box[2], $
          lonLat.uv_box[0], lonLat.uv_box[0]], $
         [lonLat.uv_box[1], lonLat.uv_box[1], lonLat.uv_box[3], $
          lonLat.uv_box[3], lonLat.uv_box[1]], $
         COLOR = blackInd


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


; Define the plotting symbol, a dot.

  if NOT(KEYWORD_SET(symSize)) then symSize = 0.75
  symX = symSize * COS ( FINDGEN ( 33 ) / 16.0 * !PI )
  symY = symSize * SIN ( FINDGEN ( 33 ) / 16.0 * !PI )
  USERSYM, symX, symY, /FILL


; Get plot device locations of points.

  uv = MAP_PROJ_FORWARD([lon], [lat], MAP_STRUCTURE = lonLat)
  u = REFORM(uv[0,*])
  v = REFORM(uv[1,*])
  ;; PRINT, 'X_MAP_GEO_POINTS u: ', MIN(u), MAX(u), MEAN(u)
  ;; PRINT, 'X_MAP_GEO_POINTS v: ', MIN(v), MAX(v), MEAN(v)
  ;; PRINT, 'X_MAP_GEO_POINTS !X.CRange: ', !X.CRange
  ;; PRINT, 'X_MAP_GEO_POINTS !Y.CRange: ', !Y.CRange

;+
; Define the color for point outlines.
;-
  if KEYWORD_SET(outlines) then outlineCol = blackInd


; Loop randomly through points.

  k = RANDOM_UNIQ_IND(n)

  for i = 0, n - 1 do begin

      lonki = lon[k[i]]
      latki = lat[k[i]]
      valki = val[k[i]]
      uki = u[k[i]]
      vki = v[k[i]]

      case 1 of
          valki lt binEdges[0] : begin
              if KEYWORD_SET(showLow) then begin
                  PLOTS, uki, vki, PSYM = 8, COLOR = 1
                  if KEYWORD_SET(outlines) then begin
                      USERSYM, symX, symY
                      PLOTS, uki, vki, PSYM = 8, COLOR = outlineCol
                      USERSYM, symX, symY, /FILL
                  endif
              endif
          end
          valki ge binEdges[numColors] : begin
              if KEYWORD_SET(showHigh) then begin
                  PLOTS, uki, vki, PSYM = 8, COLOR = nc
                  if KEYWORD_SET(outlines) then begin
                      USERSYM, symX, symY
                      PLOTS, uki, vki, PSYM = 8, COLOR = outlineCol
                      USERSYM, symX, symY, /FILL
                  endif
              endif
          end
          else: begin
              cc = MAX(WHERE(valki ge binEdges, count))
              if (count eq 0) then STOP      ; PROGRAMMING ERROR
              if (cc ge numColors) then STOP ; PROGRAMMING ERROR
              PLOTS, uki, vki, PSYM = 8, COLOR = cc + 1
              if KEYWORD_SET(outlines) then begin
                  USERSYM, symX, symY
                  PLOTS, uki, vki, PSYM = 8, COLOR = outlineCol
                  USERSYM, symX, symY, /FILL
              endif
          end
      endcase

  endfor

;; ; Loop over colors, plotting all points of each color in turn.

;;   for sc = 0, numColors - 1 do begin

;;       case 1 of
;;           (sc eq 0) and KEYWORD_SET(showLow): begin
;;               ind = WHERE(val lt binEdges[1], count)
;;           end
;;           (sc eq (numColors - 1)) and KEYWORD_SET(howHigh): begin
;;               ind = WHERE(val ge binEdges[numColors - 1], count)
;;           end
;;           else: begin
;;               ind = WHERE((val ge binEdges[sc]) and $
;;                           (val lt binEdges[sc + 1]), count)
;;           end
;;       endcase
;;       if (count gt 0) then begin
;;           uv = MAP_PROJ_FORWARD(lon[ind], lat[ind], MAP_STRUCTURE = lonLat)
;;           u = REFORM(uv[0,*])
;;           v = REFORM(uv[1,*])
;;           PLOTS, u, v, PSYM = 8, COLOR = sc + 1, SYMSIZE = symSize
;;       endif        

;;   endfor

  if KEYWORD_SET(title) then begin


;     Add a title.

      textSize = ROUND((xDisplaySize < yDisplaySize) / 500)
      XYOUTS, 0.5, 0.95, title, SIZE = 1.5, ALIGNMENT = 0.5, /NORMAL, $
              COLOR = blackInd

  endif

  if KEYWORD_SET(colorbar) then begin


;     Add a colorbar.

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
              SHOW_LOW = showLow, SHOW_HIGH = showHigh

;+
;         Re-establish plotting space in case other data will be added
;         to the plot after this procedure has finished.
;-
          PLOT, lonLat.uv_box[[0,2]], $
                lonLat.uv_box[[1,3]], $
                POSITION = mapPos, $
                /NODATA, XSTYLE = 5, YSTYLE = 5, /NOERASE

      endelse

  endif

  mxmStatus = 1

  RETURN

end
