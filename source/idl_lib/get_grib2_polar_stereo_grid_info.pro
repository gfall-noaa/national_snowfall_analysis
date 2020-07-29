PRO GET_GRIB2_POLAR_STEREO_GRID_INFO, $
  GRIBFilePath, $ ; IN - GRIB2 file path
  Field, $        ; IN - Expression to match for field
  nx, $           ; OUT - GRIB data columns
  ny, $           ; OUT - GRIB data rows
  lat1, $         ; OUT - latitude of lower left grid cell
  lon1, $         ; OUT - longitude of lower left grid cell
  latd, $         ; OUT - latitude where grid spacing is defined
  lonv, $         ; OUT - orientation longitude
  dx, $           ; OUT - grid spacing in x, meters
  dy, $           ; OUT - grid spacing in y, meters
  ggpsiStatus

;+
; Get geometry for polar stereographic grid data in a GRIB2 file.
;-

;+
; Initialize procedure status to 0 (fail).
;-
  ggpsiStatus = 0

  ;; pidStr = STRCOMPRESS ( GETPID ( ), /REMOVE_ALl )

  ;; tmpGRIBHeader = ScratchDir + '/' + 'tmpGRIBHeader.' + pidStr

  if NOT(FILE_TEST(GRIBFilePath)) then begin
      ERR_MSG, 'File "' + GRIBFilePath + '" not found.'
      RETURN
  endif

  cmd = 'wgrib2 -grid -end -order we:sn -match "' + Field + '" ' + $
         GRIBFilePath
  SPAWN, cmd, hdr, EXIT_STATUS = status
  if (status ne 0) then begin
      ERR_MSG, 'Command "' + cmd + '" failed.'
      RETURN
  endif

;+
; Verify the output header is the expected size.
;-
  if (N_ELEMENTS(hdr) ne 3) then begin
      ERR_MSG, 'Invalid output header.'
      RETURN
  endif

;+
; Sample GRIB2 polar stereographic header:
;
; 1:0:grid_template=20:winds(grid):
; 	polar stereographic grid: (1121 x 881) input WE:SN output WE:SN res 8
; 	North pole lat1 23.117000 lon1 240.976992 latD 60.000000 lonV 255.000000 dx 4762.500000 m dy 4762.500000 m
;-

  if NOT(STREGEX(hdr[0], 'grid_template=20', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to match "grid_template=20" in GRIB inventory.'
      RETURN
  endif

  if NOT(STREGEX(hdr[1], 'polar stereographic grid', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to match "polar stereographic grid" in GRIB inventory.'
      RETURN
  endif

  if NOT(STREGEX(hdr[1], '\([0-9]+ x [0-9]+\)', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to get grid dimensions from GRIB inventory.'
      RETURN
  endif

  dims = STREGEX(hdr[1], '\([0-9]+ x [0-9]+\)', /EXTRACT)
  nxStr = STREGEX(dims, '\([0-9]+ x', /EXTRACT)
  nx_ = LONG(STRMID(nxStr, 1, STRLEN(nxStr) - 3))
  nyStr = STREGEX(dims, 'x [0-9]+\)', /EXTRACT)
  ny_ = LONG(STRMID(nyStr, 2, STRLEN(nyStr) - 3))

  if NOT(STREGEX(hdr[2], 'lat1 [-0-9.]* ', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to locate grid corner latitude in GRIB inventory.'
      RETURN
  endif
  c1 = STRPOS(hdr[2], 'lat1 ')
  ;; if ( c1 eq -1 ) then begin
  ;;     MESSAGE, 'FATAL: Unexpected format for origin latitude in ' + $
  ;;              'GRIB inventory.'
  ;; endif
  c2 = STRPOS(hdr[2], ' ', c1 + 5)
  ;; if ( c2 eq -1 ) then begin
  ;;     MESSAGE, 'FATAL: Unexpected format for origin latitude in ' + $
  ;;              'GRIB inventory.'
  ;; endif
  lat1_ = DOUBLE(STRMID(hdr[2], c1 + 5, c2 - c1 - 5))

  if NOT(STREGEX(hdr[2], 'lon1 [0-9.]* ', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to locate grid corner longitude in GRIB inventory.'
      RETURN
  endif
  c1 = STRPOS(hdr[2], 'lon1 ')
  ;; if ( c1 eq -1 ) then begin
  ;;     MESSAGE, 'FATAL: Unexpected format for origin longitude in ' + $
  ;;              'GRIB inventory.'
  ;; endif
  c2 = STRPOS(hdr[2], ' ', c1 + 5)
  ;; if ( c2 eq -1 ) then begin
  ;;     MESSAGE, 'FATAL: Unexpected format for origin longitude in ' + $
  ;;              'GRIB inventory.'
  ;; endif
  lon1_ = DOUBLE(STRMID(hdr[2], c1 + 5, c2 - c1 - 5))
  if (lon1_ gt 180.0D) then lon1_ = lon1_ - 360.0D

  if NOT(STREGEX(hdr[2], 'latD [-0-9.]* ', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to locate grid spacing latitude in GRIB inventory.'
      RETURN
  endif
  c1 = STRPOS(hdr[2], 'latD ')
  ;; if ( c1 eq -1 ) then begin
  ;;     MESSAGE, 'FATAL: Unexpected format for grid spacing latitude in ' + $
  ;;              'GRIB inventory.'
  ;; endif
  c2 = STRPOS(hdr[2], ' ', c1 + 5)
  ;; if ( c2 eq -1 ) then begin
  ;;     MESSAGE, 'FATAL: Unexpected format for grid spacing latitude in ' + $
  ;;              'GRIB inventory.'
  ;; endif
  latd_ = DOUBLE(STRMID(hdr[2], c1 + 5, c2 - c1 - 5))

  if NOT(STREGEX(hdr[2], 'lonV [0-9.]* ', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to locate orientation longitude in GRIB inventory.'
      RETURN
  endif
  c1 = STRPOS(hdr[2], 'lonV ')
  ;; if ( c1 eq -1 ) then begin
  ;;     MESSAGE, 'FATAL: Unexpected format for orientation longitude in ' + $
  ;;              'GRIB inventory.'
  ;; endif
  c2 = STRPOS(hdr[2], ' ', c1 + 5)
  lonv_ = DOUBLE(STRMID(hdr[2], c1 + 5, c2 - c1 - 5))
  if (lonv_ gt 180.0D) then lonv_ = lonv_ - 360.0D

  if NOT(STREGEX(hdr[2], 'dx [0-9.]* m dy [0-9.]* m', /BOOLEAN)) then begin
      ERR_MSG, 'Failed to locate grid spacing in GRIB inventory.'
      RETURN
  endif
  dims = STREGEX(hdr[2], 'dx [0-9.]* m dy [0-9.]* m', /EXTRACT)
  c1 = STRPOS(dims, 'dx ')
  c2 = STRPOS(dims, ' m dy ', c1 + 3)
  dx_ = DOUBLE(STRMID(dims, c1 + 3, c2 - c1 - 3))
  c1 = STRPOS(dims, 'dy ')
  c2 = STRPOS(dims, ' m', c1 + 3)
  dy_ = DOUBLE(STRMID(dims, c1 + 3, c2 - c1 - 3))

;+
; Generate full set of outputs from internal variables.
;-
  nx = TEMPORARY(nx_)
  ny = TEMPORARY(ny_)
  lat1 = TEMPORARY(lat1_)
  lon1 = TEMPORARY(lon1_)
  latd = TEMPORARY(latd_)
  lonv = TEMPORARY(lonv_)
  dx = TEMPORARY(dx_)
  dy = TEMPORARY(dy_)

  ggpsiStatus = 1

  RETURN

end
