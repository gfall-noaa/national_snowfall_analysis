PRO GET_GRIB1_POLAR_STEREO_GRID_INFO, $
    GRIBFilePath, $
    ScratchDir, $
    Field, $
    nx, $                                     ; GRIB data columns
    ny, $                                     ; GRIB data rows
    lat1, $                                   ; origin latitude
    lon1, $                                   ; origin longitude
    latD, $                                   ; secant latitude
    lonv, $                                   ; orientation longitude
    dx, $                                     ; grid spacing in x, meters
    dy, $                                     ; grid spacing in y, meters
    ggpsiStatus

;+
; Get geometry for polar stereographic grid data in a GRIB1 file.
;
; TODO: complete header documentation
;-

;+
; Initialize procedure status to 0 (fail).
;-
  ggpsiStatus = 0

;+
; TODO: check all arguments for validity.
;-
  if NOT(FILE_TEST(GRIBFilePath)) then begin
      ERR_MSG, 'File "' + GRIBFilePath + '" not found.'
      RETURN
  endif

;+
; Deal with zipped files. If the GRIBFilePath ends with one of the
; extensions below, this procedure will attempt to use the gzip
; utility to decompress a copy of the file before reading it.
;-
  GRIBFile = FILE_BASENAME(GRIBFilePath)

  gz1Pos = STRPOS(GRIBFile, '.gz', 2, /REVERSE_SEARCH, /REVERSE_OFFSET)
  gz2pos = STRPOS(GRIBFile, '-gz', 2, /REVERSE_SEARCH, /REVERSE_OFFSET)
  z1Pos = STRPOS(GRIBFile, '.z', 1, /REVERSE_SEARCH, /REVERSE_OFFSET)
  z2Pos = STRPOS(GRIBFile, '-z', 1, /REVERSE_SEARCH, /REVERSE_OFFSET)
  z3Pos = STRPOS(GRIBFile, '_z', 1, /REVERSE_SEARCH, /REVERSE_OFFSET)
  z4Pos = STRPOS(GRIBFile, '.Z', 1, /REVERSE_SEARCH, /REVERSE_OFFSET)
  zipPos = [gz1Pos, gz2Pos, z1Pos, z2Pos, z3Pos, z4Pos]
  ind = WHERE(zipPos ne -1, count)
  if (count eq 1) then begin

;+
;     Copy the file to the ScratchDir.
;-
      cmd = 'cp --preserve=mode,timestamps ' + $
            GRIBFilePath + ' ' + ScratchDir
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'Failed to copy ' + GRIBFilePath + ' to ' + ScratchDir
          RETURN
      endif
      if NOT(FILE_TEST(ScratchDir + '/' + GRIBFile)) then begin
          ERR_MSG, 'File copy ' + ScratchDir + '/' + GRIBFile + $
                   ' not found.'
          RETURN
      endif

      ind = ind[0]
      SPAWN, 'gzip -d ' + ScratchDir + '/' + GRIBFile, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'Command "gzip -d ' + ScratchDir + '/' + GRIBFile + $
                   '" failed.'
          RETURN
      endif
      tmpGRIBFile = STRMID(GRIBFile, 0, zipPos[ind])
      if NOT(FILE_TEST(ScratchDir + '/' + tmpGRIBFile)) then begin
          ERR_MSG, 'Output file missing after command "gzip -d ' + $
                   ScratchDir + '/' + GRIBFile + '".'
          RETURN
      endif

      inputPath = ScratchDir + '/' + tmpGRIBFile

  endif else begin

      tmpGRIBFile = GRIBFile
      inputPath = GRIBFilePath

  endelse

;+
; Decode the GRIB record header.
;-

  cmd = 'wgrib -v ' + inputPath + $
        ' | grep -E "' + Field + '"' + $
        ' | wgrib -i -V ' + inputPath + $
        ' -o /dev/null'
  SPAWN, cmd, hdr, EXIT_STATUS = status
  if (status ne 0) then begin
      ERR_MSG, 'Command "' + cmd + '" failed.'
      RETURN
  endif

;+
; Delete the temporary copy if gzip was used.
;-
  if (inputPath eq ScratchDir + '/' + tmpGRIBFile) then $
      FILE_DELETE, inputPath

;+
; Verify the output header is the expected size.'
;-
  if (N_ELEMENTS(hdr) ne 8) then begin
      ERR_MSG, 'Invalid output header.'
      RETURN
  endif

;+
; Sample GRIB1 polar stereographic header:
;
;   rec 1:0:date 2017110911 APCP kpds5=61 kpds6=1 kpds7=0 levels=(0,0) grid=255 sfc 0-1hr acc: bitmap: 480079 undef
;     APCP=Total precipitation [kg/m^2]
;     timerange 4 P1 0 P2 1 TimeU 1  nx 1121 ny 881 GDS grid 5 num_in_ave 0 missing 0
;     center 7 subcenter 4 process 182 Table 2 scan: WE:SN winds(grid) 
;     polar stereo: Lat1 23.117000 Long1 -119.023000 Orient -105.000000
;        north pole (1121 x 881) Dx 4763 Dy 4763 scan 64 mode 8
;     min/max data 0 21.88  num bits 12  BDS_Ref 0  DecScale 2 BinScale 0
;-

;+
; Get grid information.
;-
  c1 = STRPOS(hdr[2], 'nx ') + 3
  if (c1 eq 2) then begin
      ERR_MSG, 'FATAL: unknown "nx" format in GRIB inventory ' + $
               '(' + hdr[2] + ').'
      FILE_DELETE, tmpGRIBRaster, /QUIET
      RETURN
  endif
  c2 = STRPOS(hdr[2], ' ', c1)
  if (c2 eq -1) then begin
      ERR_MSG, 'FATAL: unknown "nx" format in GRIB inventory ' + $
               '(' + hdr[2] + ').'
      FILE_DELETE, tmpGRIBRaster, /QUIET
      RETURN
  endif
  nx = LONG(STRMID(hdr[2], c1, c2 - c1))

  c1 = STRPOS(hdr[2], 'ny ', c2) + 3
  if (c1 eq 2) then begin
      ERR_MSG, 'FATAL: unknown "ny" format in GRIB inventory ' + $
               '(' + hdr[2] + ').'
      FILE_DELETE, tmpGRIBRaster, /QUIET
      RETURN
  endif
  c2 = STRPOS(hdr[2], ' ', c1)
  if (c2 eq -1) then begin
      ERR_MSG, 'FATAL: unknown "ny" format in GRIB inventory ' + $
               '(' + hdr[2] + ').'
      FILE_DELETE, tmpGRIBRaster, /QUIET
      RETURN
  endif
  ny = LONG(STRMID(hdr[2], c1, c2 - c1))

;+
; Get projection parameters.
;-
  c1 = STRPOS(hdr[4], 'polar stereo: ') + 14
  if (c1 eq 13) then begin
      ERR_MSG, 'FATAL: missing "polar stereo" in GRIB inventory ' + $
               '(' + hdr[4] + ').'
      RETURN
  endif
  c1 = STRPOS(hdr[4], 'Lat1 ', c1) + 5
  if (c1 eq 4) then begin
      ERR_MSG, 'FATAL: missing "Lat1" in GRIB inventory ' + $
               '(' + hdr[4] + ').'
      RETURN
  endif
  c2 = STRPOS(hdr[4], ' ', c1)
  if (c2 eq -1) then begin
      ERR_MSG, 'FATAL: unknown "Lat1" format in GRIB inventory ' + $
               '(' + hdr[4] + ').'
      RETURN
  endif
  lat1 = DOUBLE(STRMID(hdr[4], c1, c2 - c1))

  c1 = STRPOS(hdr[4], 'Long1 ', c2) + 6
  if (c1 eq 5) then begin
      ERR_MSG, 'FATAL: missing "Long1" in GRIB inventory ' + $
               '(' + hdr[4] + ').'
      RETURN
  endif
  c2 = STRPOS(hdr[4], ' ', c1)
  if (c2 eq -1) then begin
      ERR_MSG, 'FATAL: unknown "Long1" format in GRIB inventory ' + $
               '(' + hdr[4] + ').'
      RETURN
  endif
  lon1 = DOUBLE(STRMID(hdr[4], c1, c2 - c1))

;+
; GRIB documentation:
;   http://rda.ucar.edu/docs/formats/grib/gribdoc/
;   "Grid lengths are in units of meters, at the 60-degree
;   parallel nearest to the pole on the projection plane."
; Also - in the source code for cnvgrib, the latD parameter for polar
; stereographic grids is hard-coded to +/- 60 degrees (see gds2gdt.f).
;-
  latD = 60.0D

  c1 = STRPOS(hdr[4], 'Orient ', c2) + 7
  if (c1 eq 6) then begin
      ERR_MSG, 'FATAL: missing "Orient" in GRIB inventory ' + $
               '(' + hdr[4] + ').'
      RETURN
  endif
  c2 = STRPOS(hdr[4], ' ', c1)
  if (c2 ne -1) then begin
      ERR_MSG, 'FATAL: unknown "Orient" format in GRIB inventory ' + $
               '(' + hdr[4] + ').'
      RETURN
  endif
  lonv = DOUBLE(STRMID(hdr[4], c1, STRLEN(hdr[4]) - c1))

  c1 = STRPOS(hdr[5], 'Dx ') + 3
  if (c1 eq 2) then begin
      ERR_MSG, 'FATAL: missing "Dx" in GRIB inventory ' + $
               '(' + hdr[5] + ').'
      RETURN
  endif
  c2 = STRPOS(hdr[5], ' ', c1)
  if (c2 eq -1) then begin
      ERR_MSG, 'FATAL: unknown "Dx" format in GRIB inventory ' + $
               '(' + hdr[5] + ').'
      RETURN
  endif
  dx = DOUBLE(STRMID(hdr[5], c1, c2 - c1))

  c1 = STRPOS(hdr[5], 'Dy ', c2) + 3
  if (c1 eq 2) then begin
      ERR_MSG, 'FATAL: missing "Dy" in GRIB inventory ' + $
               '(' + hdr[5] + ').'
      RETURN
  endif
  c2 = STRPOS(hdr[5], ' ', c1)
  if (c2 eq -1) then begin
      ERR_MSG, 'FATAL: unknown "Dy" format in GRIB inventory ' + $
               '(' + hdr[5] + ').'
      RETURN
  endif
  dy = DOUBLE(STRMID(hdr[5], c1, c2 - c1))

  ggpsiStatus = 1

  RETURN

end
