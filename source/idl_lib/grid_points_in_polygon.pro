;+
; NAME:
;       GRID_POINTS_IN_POLYGON
;
; PURPOSE:
;
;   Index points from a grid falling in a shapefile polygon.
;
; CALLING SEQUENCE:
;
;   index = GRID_POINTS_IN_POLYGON ( numColumns, $
;                                    numRows, $
;                                    minLongitude, $
;                                    maxLatitude, $
;                                    xRes, $
;                                    yRes, $
;                                    polygon, $
;                                    savFile, $
;                                    COUNT = count )
;
; NOTES:
;
;   Sample code:
;
;   shapeFileDir = '/operations/gisrs/data/common/shapefiles'
;   shapeFileName = 'Snow_Regions'
;
;   shapeFilePath = shapeFileDir + '/' + shapeFileName
;
;   shapeFile = OBJ_NEW ( 'IDLffShape', shapeFilePath + '.shp' )
;   shapeFile->IDLffShape::GetProperty, N_ATTRIBUTES = numAtts
;   shapeFile->IDLffShape::GetProperty, ATTRIBUTE_NAMES = attNames
;
;   nameInd = WHERE ( attNames eq 'NAME', count )
;   if ( count ne 1 ) then STOP
;   nameInd = nameInd[0]
;
;   shapeFile->IDLffShape::GetProperty, N_ENTITIES = numPolygons
;
;   for pc = 0, numPolygons - 1 do begin
;
;       attrib = shapeFile->getAttributes ( pc )
;
;       name = attrib.(nameInd)
;
;       PRINT, 'Finding indices in shape "' + name + '"...'
;
;       name2 = name
;
;       while ( STRPOS ( name2, ' ' ) ne -1 ) do $
;         STRPUT, name2, '_', STRPOS ( name2, ' ' )
;
;       shapeIndSavFile = shapeFileName + '/' + name2 + '.sav'
;
;       if NOT ( FILE_TEST ( shapeFileName, /DIRECTORY ) ) then $
;         FILE_MKDIR, shapeFileName
;
;       polygon = shapeFile->IDLffShape::GetEntity ( pc )
;
;       index = GRID_POINTS_IN_POLYGON ( columns, $
;                                        rows, $
;                                        minLongitude, $
;                                        maxLatitude, $
;                                        xRes, $
;                                        yRes, $
;                                        polygon, $
;                                        shapeIndSavFile, $
;                                        COUNT = numCellsInPolygon )
;
;       ; Do stuff with the points here...
;
;   endfor
;
;   OBJ_DESTROY, shapeFile
;
;
; MODIFICATION HISTORY:
;
;   Greg Fall, UCAR/NOHRSC, 07 Jan 2009
;-

FUNCTION GRID_POINTS_IN_POLYGON, numColumns, $
                                 numRows, $
                                 minLongitude, $
                                 maxLatitude, $
                                 xRes, $
                                 yRes, $
                                 polygon, $
                                 savFile, $
                                 COUNT = count

  COMMON info, message

  count = 0

  if ( FILE_TEST ( savFile ) ) then begin

      numColumns_ = numColumns
      numRows_ = numRows
      minLongitude_ = minLongitude
      maxLatitude_ = maxLatitude
      xRes_ = xRes
      yRes_ = yRes

      RESTORE, savFile

      if ((numColumns ne numColumns_) or $
          (numRows ne numRows_) or $
          (minLongitude ne minLongitude_) or $
          (maxLatitude ne maxLatitude_) or $
          (xRes ne xRes_) or $
          (yRes ne yRes_)) then begin

          ERR_MSG, 'Geometry mismatch in ' + savFile + $
                   '; will proceed with no IDL save file.'
          savFile = ''

          numColumns = numColumns_
          numRows = numRows_
          minLongitude = minLongitude_
          maxLatitude = maxLatitude_
          xRes = xRes_
          yRes = yRes_

      endif else begin

          RETURN, ind

      endelse

  endif

  x = minLongitude + 0.5d * xRes + DINDGEN ( numColumns ) * xRes
  y = maxLatitude - 0.5d * yRes - DINDGEN ( numRows ) * yRes

  flag = BYTARR ( numColumns, numRows )

  okRows = WHERE ( ( y le polygon.bounds[5] ) and $
                   ( y gt polygon.bounds[1] ), okRowsCount )

  if ( okRowsCount eq 0 ) then RETURN, -1L

  okColumns = WHERE ( ( x ge polygon.bounds[0] ) and $
                      ( x lt polygon.bounds[4] ), okColumnsCount )

  if ( okColumnsCount eq 0 ) then RETURN, -1L


; For each row of the raster that falls inside the polygon find any columns
; that are in the polygon.

  OBJECT_INTERVALS, 1, polygon, rc, numIntervals, interval, x[0], y[0], $
                    0, 0, xRes, yRes

  for rc = okRows[0], okRows[okRowsCount - 1] do begin

      OBJECT_INTERVALS, 2, polygon, rc, numIntervals, interval, x[0], y[0], $
                        0, 0, xRes, yRes

      if ( numIntervals eq 0 ) then CONTINUE

      for ic = 0, numIntervals - 1 do begin

          ind1 = interval[2L * ic]
          ind2 = interval[2L * ic + 1]

          if ( ind1 gt ( numColumns - 1 ) ) then CONTINUE
          if ( ind2 lt 0 ) then CONTINUE

          ind1 = ind1 > 0
          ind2 = ind2 < ( numColumns - 1 )

          flag[ind1:ind2, rc] = 1B

      endfor

  endfor

  ind = WHERE ( flag, count )

  if (ISA(savFile)) then begin
      if (savFile ne '') then begin
          SAVE, numColumns, numRows, $
                minLongitude, maxLatitude, $
                xRes, yRes, $
                ind, count, $
                FILENAME = savFile
          USR_MSG, 'Indices saved to ' + savFile
      endif
  endif

  RETURN, ind

end
