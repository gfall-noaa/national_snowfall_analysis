; Older versions of this procedure were called get_grib2_grid_info.

; This procedure was renamed because it is specifically tailored to gathering
; grid parameters for GRIB2-encoded data projected on Lambert Conformal
; coordinates.



; Comments regarding projection definitions:
;
;          Map Projections -
;          A Working Manual    GISRS source           GRIB2 Docs             English
;          -----------------   --------------------   --------------------   --------------------
; lat1     -                   y_origin (but only     Lat1 - "latitude of    first grid point
;                              an "origin" in terms   first grid point"      latitude
;                              the grid)
;
; lon1     -                   x_origin (but only     Lon1 - "longitude of   first grid point
;                              an "origin" in terms   first grid point"      longitude
;                              of the grid)
;
; lov      lambda0             xmap_origin            Lov - "orientation     Orientation
;                                                     of the grid"           longitude
;
; latd     -                   -                      LatD - "latitude
;                                                     where dx and dy are
;                                                     specified
;
; latin1   phi1                std_parallel_1         Latin1                 First standard
;                                                                            parallel
;
; latin2   phi2                std_parallel_2         Latin2                 Second standard
;                                                                            parallel
;
; latsp    -                   -                      LatSP                  Latitude of the
;                                                                            southern pole of
;                                                                            the projection
;
; lonsp    -                   -                      LonSP                  Longitude of the
;                                                                            southern pole of
;                                                                            the projection
;

PRO GET_GRIB2_LAMBERT_CONFORMAL_GRID_INFO, $
  GRIBFile, $                   ; IN - GRIB2 file name (no path)
  GRIBDir, $                    ; IN - Location of GRIB2 file
  tmpDir, $                     ; IN - Scratch directory for header file
  fldPat, $                     ; IN - Expression to match for field
  nx, $                         ; OUT - GRIB data columns
  ny, $                         ; OUT - GRIB data rows
  lat1, $                       ; OUT - latitude of lower left grid cell
  lon1, $                       ; OUT - longitude of lower left grid cell
  lov, $                        ; OUT - orientation lon.
  latd, $                       ; OUT - lat. where grid spacing is defined
  latin1, $                     ; OUT - first std. parallel
  latin2, $                     ; OUT - second std. parallel
  latsp, $                      ; OUT - south pole lat.
  lonsp, $                      ; OUT - south pole lon.
  dx, $                         ; OUT - grid spacing in x, meters
  dy, $                         ; OUT - grid spacing in y, meters
  gggiStatus, $
  VERBOSE = verbose

  gggiStatus = 0

  pidStr = STRCOMPRESS ( GETPID ( ), /REMOVE_ALl )

  tmpGRIBHeader = tmpDir + '/' + 'tmpGRIBHeader.' + pidStr

; Supposedly -V = -vt -lev -ftime -var -ens -stats -grid

  SPAWN, 'wgrib2 -grid -end -order we:sn -match "' + fldPat + '" ' + $
         GRIBDir + '/' + GRIBFile + $
         ' > ' + tmpGRIBHeader, $
         EXIT_STATUS = status
  if ( status ne 0 ) then STOP

;   SPAWN, 'wgrib2 -grid -end -match "' + fldPat + '" ' + $
;          GRIBDir + '/' + GRIBFile + $
;          ' >> ' + tmpGRIBHeader, $
;          EXIT_STATUS = status
;   if ( status ne 0 ) then STOP


; Check the file size.  The wgrib program likes to bomb and report success.

  hInfo = FILE_INFO ( tmpGRIBHeader )

  if ( hInfo.size eq 0L ) then begin
      MESSAGE, 'FATAL: wgrib produced an empty header file (regex "' + fldPat + '" could be failing to match).', /CONTINUE
      RETURN
  endif


; Read the header.

  OPENR, hLun, tmpGRIBHeader, /GET_LUN
  freeHLun = 1

  allFieldsRead = 0

  while NOT ( EOF ( hLun ) ) do begin


;     First line: "grid_template=30"
      inventory = ''
      READF, hLun, inventory

      if NOT ( STREGEX ( inventory, 'grid_template=30', /BOOLEAN ) ) then begin
message, inventory, /continue
          MESSAGE, 'Failed to match "grid_template=30" in GRIB inventory.'
      endif


;     Second line: grid dimensions.

      READF, hLun, inventory

      if NOT ( STREGEX ( inventory, 'Lambert Conformal', /BOOLEAN ) ) $
        then begin
message, inventory, /continue
          MESSAGE, 'Failed to match "Lambert Conformal" in GRIB inventory.'
      endif

      if NOT ( STREGEX ( inventory, '\([0-9]+ x [0-9]+\)', /BOOLEAN ) ) $
        then begin
          MESSAGE, 'FATAL: failed to get grid dimensions from GRIB ' + $
                   'inventory.'
      endif

      dims = STREGEX ( inventory, '\([0-9]+ x [0-9]+\)', /EXTRACT )

      nxStr = STREGEX ( dims, '\([0-9]+ x', /EXTRACT )
      nx = LONG ( STRMID ( nxStr, 1, STRLEN ( nxStr ) - 3 ) )
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'nx: {' + STRCOMPRESS ( nx, /REMOVE_ALL ) + '}'

      nyStr = STREGEX ( dims, 'x [0-9]+\)', /EXTRACT )
      ny = LONG ( STRMID ( nyStr, 2, STRLEN ( nyStr ) - 3 ) )
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'ny: {' + STRCOMPRESS ( ny, /REMOVE_ALL ) + '}'


;     Third line: origin.

      READF, hLun, inventory

      if NOT ( STREGEX ( inventory, 'Lat1 [-0-9.]* ', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate origin latitude in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'Lat1 ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for origin latitude in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRPOS ( inventory, ' ', c1 + 5 )
      if ( c2 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for origin latitude in ' + $
                   'GRIB inventory.'
      endif
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'lat1: {' + STRMID ( inventory, c1 + 5, c2 - c1 - 5 ) + '}'
      lat1 = DOUBLE ( STRMID ( inventory, c1 + 5, c2 - c1 - 5 ) )

      if NOT ( STREGEX ( inventory, 'Lon1 [0-9.]* ', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate origin longitude in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'Lon1 ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for origin longitude in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRPOS ( inventory, ' ', c1 + 5 )
      if ( c2 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for origin longitude in ' + $
                   'GRIB inventory.'
      endif
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'lon1 {' + STRMID ( inventory, c1 + 5, c2 - c1 - 5 ) + '}'
      lon1 = DOUBLE ( STRMID ( inventory, c1 + 5, c2 - c1 - 5 ) )
      if ( lon1 gt 180.0d ) then lon1 = lon1 - 360.0d

      if NOT ( STREGEX ( inventory, 'LoV [0-9.]*$', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate orientation longitude in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'LoV ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for orientation longitude in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRLEN ( inventory )

      if KEYWORD_SET ( verbose ) then $
        PRINT, 'lov: {' + STRMID ( inventory, c1 + 4, c2 - c1 - 4 ) + '}'
      lov = DOUBLE ( STRMID ( inventory, c1 + 4, c2 - c1 - 4 ) )
      if ( lov gt 180.0d ) then lov = lov - 360.0d


;     Fourth line: standard parallel

      READF, hLun, inventory

      if NOT ( STREGEX ( inventory, 'LatD [-0-9.]* ', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate grid spacing latitude in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'LatD ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for grid spacing latitude in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRPOS ( inventory, ' ', c1 + 5 )
      if ( c2 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for grid spacing latitude in ' + $
                   'GRIB inventory.'
      endif
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'latd: {' + STRMID ( inventory, c1 + 5, c2 - c1 - 5 ) + '}'
      latd = DOUBLE ( STRMID ( inventory, c1 + 5, c2 - c1 - 5 ) )

      if NOT ( STREGEX ( inventory, 'Latin1 [-0-9.]* ', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate first standard parallel in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'Latin1 ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for 1st standard parallel in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRPOS ( inventory, ' ', c1 + 7 )
      if ( c2 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for 1st standard parallel in ' + $
                   'GRIB inventory.'
      endif
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'latin1 {' + STRMID ( inventory, c1 + 7, c2 - c1 - 7 ) + '}'
      latin1 = DOUBLE ( STRMID ( inventory, c1 + 7, c2 - c1 - 7 ) )

      if NOT ( STREGEX ( inventory, 'Latin2 [-0-9.]*$', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate second standard parallel in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'Latin2 ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for 2nd standard parallel in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRLEN ( inventory )
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'latin2: {' + STRMID ( inventory, c1 + 7, c2 - c1 - 7 ) + '}'
      latin2 = DOUBLE ( STRMID ( inventory, c1 + 7, c2 - c1 - 7 ) )


;     Fifth line:  south pole.

      READF, hLun, inventory

      if NOT ( STREGEX ( inventory, 'LatSP [-0-9.]* ', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate south pole latitude in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'LatSP ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for south pole latitude in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRPOS ( inventory, ' ', c1 + 6 )
      if ( c2 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for south pole latitude in ' + $
                   'GRIB inventory.'
      endif
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'latsp: {' + STRMID ( inventory, c1 + 6, c2 - c1 - 6 ) + '}'
      latsp = DOUBLE ( STRMID ( inventory, c1 + 6, c2 - c1 - 6 ) )

      if NOT ( STREGEX ( inventory, 'LonSP [0-9.]*$', /BOOLEAN ) ) then begin
          MESSAGE, 'FATAL: Failed to locate south pole longitude in ' + $
                   'GRIB inventory.'
      endif
      c1 = STRPOS ( inventory, 'LonSP ' )
      if ( c1 eq -1 ) then begin
          MESSAGE, 'FATAL: Unexpected format for south pole longitude in ' + $
                   'GRIB inventory.'
      endif
      c2 = STRLEN ( inventory )
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'lonsp: {' + STRMID ( inventory, c1 + 6, c2 - c1 - 6 ) + '}'
      lonsp = DOUBLE ( STRMID ( inventory, c1 + 6, c2 - c1 - 6 ) )


;     Sixth line:  grid spacing.

      READF, hLun, inventory

      if NOT ( STREGEX ( inventory, 'Dx [0-9.]* m Dy [0-9.]* m', /BOOLEAN ) ) $
        then begin
          MESSAGE, 'FATAL: Failed to locate grid spacing in ' + $
                   'GRIB inventory.'
      endif
      dims = STREGEX ( inventory, 'Dx [0-9.]* m Dy [0-9.]* m', /EXTRACT )

      c1 = STRPOS ( dims, 'Dx ' )
      c2 = STRPOS ( dims, ' m Dy ', c1 + 3 )
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'dx: {' + STRMID ( dims, c1 + 3, c2 - c1 - 3 ) + '}'
      dx = DOUBLE ( STRMID ( dims, c1 + 3, c2 - c1 - 3 ) )

      c1 = STRPOS ( dims, 'Dy ' )
      c2 = STRPOS ( dims, ' ', c1 + 3 )
      if KEYWORD_SET ( verbose ) then $
        PRINT, 'dy: {' + STRMID ( dims, c1 + 3, c2 - c1 - 3 ) + '}'
      dy = DOUBLE ( STRMID ( dims, c1 + 3, c2 - c1 - 3 ) )

      allFieldsRead = 1


;     That is all we need from the header.

      FREE_LUN, hLun
      freeHLun = 0
      FILE_DELETE, tmpGRIBHeader

      BREAK

  endwhile

  if NOT ( allFieldsRead ) then STOP

  gggiStatus = 1

  RETURN

end
