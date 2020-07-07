; Write a grid out as a non-projected (WGS84) GeoTIFF. The input grid
; should be oriented NORTH DOWN.

; Greg Fall, NWC-North
; 2012-05-24

; 2016-02-12, GF - Set ORIENTATION to 1 (north up). It used to be 0, but I
;                  have always passed grids to this procedure as north-up as
;                  far as I can remember.
; 2016-10-31, GF - Added the addition of the GDAL no-data value to the
;                  GeoTIFF via gdal_translate
; 2017-03-03, GF - Modified to use a "which" command to find
;                  gdal_translate before attempting to run it if
;                  NO_DATA_VALUE is set.
; 2019-07-31, GF - Changed compression to allow all possibilities, so like
;                  WRITE_TIFF, it is now:
;                  COMPRESS = 1 -> LZW
;                             2 -> PackBits
;                             3 -> JPEG (for "ITIFF" files)
; 2020-01-30, GF - Transmitted COMPRESS keyword setting into call to
;                  gdal_translate when NO_DATA_VALUE keyword is set.

PRO MAKE_GEOTIFF_FROM_GRID, grid, $
                            xMin, $
                            yMax, $
                            xRes, $
                            yRes, $
                            geoTiffPath, $
                            mgfgStatus, $
                            COMPRESS = compress, $
                            DESCRIPTION = description, $
                            NO_DATA_VALUE = ndv


  mgfgStatus = 0

  COMMON info, message


; Ensure that the '.tif' extension is present.

  if ( STRMID ( geoTiffPath, 3, 4, /REVERSE_OFFSET) ne '.tif') then $
    geoTiffPath = geoTiffPath + '.tif'


; Build GeoTiff georeferencing tags.

  modelPixelScaleTag = [xRes, $ ; x resolution
                        yRes, $ ; y resolution
                        0.0d]   ; z resolution

  modelTiePointTag = [0.0d, $ ; x corner index
                      0.0d, $ ; y corner index
                      0.0d, $ ; z corner index
                      xMin, $ ; x corner coordinate
                      yMax, $ ; y corner coordinate
                      0.0d]   ; z corner coordinate

  geoStruct = { modelPixelScaleTag: modelPixelScaleTag, $
                modelTiePointTag: modelTiePointTag, $
                GTModelTypeGeoKey: 2, $                   ; Lat/lon system
                GTRasterTypeGeoKey: 1, $                  ; Pixel is area
                geographicTypeGeoKey: 4326, $             ; GCS_WGS_84
                geogCitationGeoKey: 'WGS 84', $
                geogAngularUnitsGeoKey: 9102, $           ; Angular deg.
                geogSemiMajorAxisGeoKey: 6378137.0D, $    ; Meters
                geogInvFlatteningGeoKey: 298.257223563D }
 

; Set the data type keywords for the call to WRITE_TIFF.

  shortType = 0
  longType = 0
  floatType = 0
  doubleType = 0
  complexType = 0
  dComplexType = 0
  l64Type = 0

  signed = 0

  foo = SIZE ( grid )

  dataType = foo [ N_ELEMENTS ( foo ) - 2 ]

  case dataType of

      1 : begin

;         Byte type (the default for WRITE_TIFF).  No changes.

      end

      2 : begin                 ; (signed) integer
          shortType = 1
          signed = 1
           
      end

      3 : begin                 ; (signed) long
          longType = 1
          signed = 1
      end

      4 : begin                 ; float
          floatType = 1
      end

      5 : begin                 ; double-precision float
          doubleType = 1
      end

      6 : begin                 ; complex
          complexType = 1
      end

      9 : begin                 ; double-precision complex
          dComplexType = 1
      end

      12 : begin                ; unsigned integer
          shortType = 1
      end

      13 : begin                ; unsigned long
          longType = 1
      end

      14 : begin                ; 64-bit integer
          l64Type = 1
          signed = 1
      end

      15 : begin                ; unsigned 64-bit integer
          l64Type = 1
      end

      else : begin

          case dataType of
              7 : typeName = 'STRING'
              8 : typeName = 'STRUCT'
              10 : typeName = 'POINTER'
              11 : typeName = 'OBJREF'
              else : typeName = 'UNKNOWN'
          endcase

          ERR_MSG, 'Data type "' + typeName + '" is not supported.'
          RETURN

      end

  endcase

  if KEYWORD_SET ( compress ) then $
;    compress = 2 $ ; Use PackBits compression
    compress = compress $
  else $
    compress = 0


; Write the TIFF.

  if KEYWORD_SET ( description ) then $
    WRITE_TIFF, geoTiffPath, $
                grid, $
                ORIENTATION = 1, $
                GEOTIFF = geoStruct, $
                COMPRESSION = compress, $
                DESCRIPTION = description, $
                SHORT = shortType, $
                LONG = longType, $
                FLOAT = floatType, $
                DOUBLE = doubleType, $
                COMPLEX = complexType, $
                DCOMPLEX = dComplexType, $
                L64 = l64Type, $
                SIGNED = signed $
  else $
    WRITE_TIFF, geoTiffPath, $
                grid, $
                ORIENTATION = 1, $
                GEOTIFF = geoStruct, $
                COMPRESSION = compress, $
                SHORT = shortType, $
                LONG = longType, $
                FLOAT = floatType, $
                DOUBLE = doubleType, $
                COMPLEX = complexType, $
                DCOMPLEX = dComplexType, $
                L64 = l64Type, $
                SIGNED = signed

  if KEYWORD_SET(ndv) then begin
      cmd = 'which gdal_translate 1>/dev/null 2>&1'
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'No gdal_translate utility located; ' + $
                   'no-data-value assignment will be ignored.'
      endif else begin
          slashInd = STRPOS(geoTIFFPath, '/', /REVERSE_SEARCH)
          if (slashInd ne -1) then begin
              firstPart = STRMID(geoTIFFPath, 0, slashInd)
              secondPart = STRMID(geoTIFFPath, slashInd + 1, $
                                  STRLEN(geoTIFFPath) - slashInd - 1)
              cmd = 'mktemp ' + firstPart + '/' + 'XXX.' + secondPart
          endif else begin
              cmd = 'mktemp XXX.' + geoTIFFPath
          endelse
          SPAWN, cmd, tmpFile, EXIT_STATUS = status   
          if ((status ne 0) or $
              (N_ELEMENTS(tmpFile) ne 1) or $
              (tmpFile[0] eq '')) then begin
              ERR_MSG, 'Command "' + cmd + '" failed.'
              RETURN
          endif
          tmpFile = tmpFile[0]
          gdal_trans_cmd = 'gdal_translate -q -of gtiff '
          case compress of
              1: gdal_trans_cmd = gdal_trans_cmd + ' -co COMPRESS=LZW'
              2: gdal_trans_cmd = gdal_trans_cmd + ' -co COMPRESS=PackBits'
              3: gdal_trans_cmd = gdal_trans_cmd + ' -co COMPRESS=JPEG'
              else: gdal_trans_cmd = gdal_trans_cmd
          endcase
          cmd = gdal_trans_cmd + ' -a_nodata ' + STRCRA(ndv) + $
                ' ' + geoTIFFPath + ' ' + tmpFile
      ;; cmd = ['gdal_translate', $
      ;;        '-q', $                     ; quiet output
      ;;        '-of', 'gtiff', $           ; output format is GeoTIFF
      ;;        '-a_nodata', STRCRA(ndv), $ ; no data value setting
      ;;        geoTIFFPath, $              ; input file
      ;;        tmpFile]                    ; output file
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then begin
              ERR_MSG, 'Command "' + cmd + '" failed.'
              FILE_DELETE, tmpFile
              RETURN
          endif
          FILE_DELETE, geoTIFFPath
          FILE_MOVE, tmpFile, geoTIFFPath
          FILE_CHMOD, geoTIFFPath, '0664'o
      endelse
  endif

  mgfgStatus = 1


BAIL:


  RETURN

end
