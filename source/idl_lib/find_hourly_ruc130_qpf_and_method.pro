PRO FIND_HOURLY_RUC130_QPF_AND_METHOD, CycleDate_YYYYMMDDHH, $
                                       TargetFcstHour, $
                                       RUCDir, $
                                       Verbose, $
                                       fhrpmStatus, $
                                       last_hour, $
                                       from_zero, $
                                       from_prev_m3, $
                                       GRIBDir, $
                                       GRIBFile, $
                                       fcstString, $
                                       prevGRIBFile, $
                                       prevFcstString
;+
; Find RUC precipitation (QPF) in archived GRIB files. Determine what
; method will be needed to generate hourly precipitation grids. The
; possible methods are:
;
; 1. Accumulation during last hour; e.g.,
;    ":ACPCP:surface:4-5 hour acc fcst"
; 2. Accumulation from the beginning of the model cycle, minus the
;    previous hour accumulation from the beginning of the model cycle
;    (this method reads QPF from 2 GRIB files); e.g.,
;    ":ACPCP:surface:0-5 hour acc fcst" -
;    ":ACPCP:surface:0-4 hour acc fcst"
; 3. Accumulation from the most recent multiple of 3 hours in the
;    model cycle, minus the previous hour accumulation from the same
;    starting point (this method reads QPF from 2 GRIB files); e.g.,
;    ":ACPCP:surface:3-5 hour acc fcst" -
;    ":ACPCP:surface:3-4 hour acc fcst"
;-

;+
; Initialize success/failure flag and output variables.
;-
  fhrpmStatus = 0

  GRIBDir = !NULL
  GRIBFile = !NULL
  fcstString = !NULL
  prevGRIBFile = !NULL
  prevFcstString = !NULL

  last_hour = 0    ; flag indicating "last hour" records were found
  from_zero = 0    ; flag indicating "from zero" records were found
  from_prev_m3 = 0 ; flag indicating "from the last multiple of 3"
                   ; records were found

  cycleDate_YYYYMMDD = STRMID(CycleDate_YYYYMMDDHH, 0, 8)
  cycleDate_YYYY = STRMID(CycleDate_YYYYMMDDHH, 0, 4)
  cycleDate_MM = STRMID(CycleDate_YYYYMMDDHH, 4, 2)
  cycleDate_DD = STRMID(CycleDate_YYYYMMDDHH, 6, 2)
  cycleDate_HH = STRMID(CycleDate_YYYYMMDDHH, 8, 2)

  GRIBDir_noSubdirs = RUCDir

  GRIBDir_subdirs = RUCDir + $
                    '/' + cycleDate_YYYY + $
                    '/' + cycleDate_MM + $
                    '/' + cycleDate_DD

  cycleDate_YY = STRMID(cycleDate_YYYY, 2, 2)
  Jan1_Julian = YYYYMMDDHH_TO_JULIAN(cycleDate_YYYY + '010100')
  cycleDate_Julian = YYYYMMDDHH_TO_JULIAN(CycleDate_YYYYMMDDHH)
  dayOfYear = FIX(cycleDate_Julian - Jan1_Julian) + 1
  cycleDate_DOY = STRING(dayOfYear, FORMAT = '(I3.3)')

  GRIBFile_YYDOY_pgrb = cycleDate_YY + cycleDate_DOY + $
                        '.ruc.t' + cycleDate_HH + $
                        'z.awp130pgrbf' + $
                        STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                        '.grib2'

  GRIBFile_YYYYMMDD_pgrb = 'ruc.' + $
                           cycleDate_YYYYMMDD + $
                           '.t' + cycleDate_HH + $
                           'z.awp130pgrbf' + $
                           STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                           '.grib2'

  GRIBFile_YYDOY_bgrb = cycleDate_YY + cycleDate_DOY + $
                        '.ruc.t' + cycleDate_HH + $
                        'z.awp130bgrbf' + $
                        STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                        '.grib2'

  GRIBFile_YYYYMMDD_bgrb = 'ruc.' + $
                           cycleDate_YYYYMMDD + $
                           '.t' + cycleDate_HH + $
                           'z.awp130bgrbf' + $
                           STRING(TargetFcstHour, FORMAT = '(I2.2)') + $
                           '.grib2'

  GRIBDir = ''
  GRIBFile = ''

  if FILE_TEST(GRIBDir_subdirs, /DIRECTORY) then begin

      case 1 of

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYDOY_pgrb) : begin
              use_subdirs = 1
              use_YYDOY = 1
              use_bgrb = 0
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYDOY_pgrb
          end

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYYYMMDD_pgrb) : begin
              use_subdirs = 1
              use_YYDOY = 0
              use_bgrb = 0
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYYYMMDD_pgrb
          end

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYDOY_bgrb) : begin
              use_subdirs = 1
              use_YYDOY = 1
              use_bgrb = 1
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYDOY_bgrb
          end

          FILE_TEST(GRIBDir_subdirs + '/' + GRIBFile_YYYYMMDD_bgrb) : begin
              use_subdirs = 1
              use_YYDOY = 0
              use_bgrb = 1
              GRIBDir = GRIBDir_subdirs
              GRIBFile = GRIBFile_YYYYMMDD_bgrb
          end

          else: begin
              GRIBDir = ''
              GRIBFile = ''
          end

      endcase

  endif

  if (FILE_TEST(GRIBDir_noSubdirs) and $
      ((GRIBDir eq '') or (GRIBFile eq ''))) then begin

      case 1 of

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYDOY_pgrb) : begin
              use_subdirs = 0
              use_YYDOY = 1
              use_bgrb = 0
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYDOY_pgrb
          end

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYYYMMDD_pgrb) : begin
              use_subdirs = 0
              use_YYDOY = 0
              use_bgrb = 0
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYYYMMDD_pgrb
          end

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYDOY_bgrb) : begin
              use_subdirs = 0
              use_YYDOY = 1
              use_bgrb = 1
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYDOY_bgrb
          end

          FILE_TEST(GRIBDir_noSubdirs + '/' + GRIBFile_YYYYMMDD_bgrb) : begin
              use_subdirs = 1
              use_YYDOY = 0
              use_bgrb = 1
              GRIBDir = GRIBDir_noSubdirs
              GRIBFile = GRIBFile_YYYYMMDD_bgrb
          end

          else: begin
              GRIBDir = ''
              GRIBFile = ''
          end

      endcase

  endif

  if ((GRIBDir ne '') and (GRIBFile ne '') and $
      FILE_TEST(GRIBDir + '/' + GRIBFile)) then begin

;print, '<<' + gribdir + '/' +  gribfile + '>>'

;+
;     Begin with the last_hour flag set to 1 for success, then test
;     that premise thoroughly.
;-
      last_hour = 1

;+
;     Confirm the presence of ACPCP, NCPCP, and WEASD records in the
;     file, and find out if "last hour" accumulations are provided.
;-
      fcstString = STRCRA(TargetFcstHour - 1) + '-' + $
                   STRCRA(TargetFcstHour) + $
                   ' hour acc fcst'

;+
;     NOTE: wgrib2 exit status is not usually very helpful, since
;     wgrib2 often gives a zero status even when it encounters errors,
;     but here it is checked to at least confirm that wgrib2 was able
;     to run. The output text buffer in GRIBOut should have just one
;     element, though there are cases (e.g., often for precipitation
;     accumulation in the first hour of a forecast) where the the
;     wgrib2 command matches two (usually identical) records. Here, if
;     there is at least one non-empty GRIB record matching our pattern
;     it is treated as a success.
;-
      matchStr = ':ACPCP:surface:' + fcstString + ':'
      cmd = 'wgrib2 -g2clib 0 -match "' + $
            matchStr + '" ' + GRIBDir + '/' + GRIBFile
      SPAWN, cmd, GRIBOut, EXIT_STATUS = status

      if (status ne 0) then last_hour = 0
      if (N_ELEMENTS(GRIBOut) eq 0) then $
          last_hour = 0 $
      else $
          if (GRIBOut[0] eq '') then last_hour = 0

;if NOT(last_hour) then PRINT, 'last_hour 01' ; DEBUG

      if last_hour then begin

          matchStr = ':NCPCP:surface:' + fcstString + ':'
          cmd = 'wgrib2 -g2clib 0 -match "' + $
                matchStr + '" ' + GRIBDir + '/' + GRIBFile
          SPAWN, cmd, GRIBOut, EXIT_STATUS = status

          if (status ne 0) then last_hour = 0
          if (N_ELEMENTS(GRIBOut) eq 0) then $
              last_hour = 0 $
          else $
              if (GRIBOut[0] eq '') then last_hour = 0

;if NOT(last_hour) then PRINT, 'last_hour 02' ; DEBUG

      endif

      if last_hour then begin

          matchStr = ':WEASD:surface:' + fcstString + ':'
          cmd = 'wgrib2 -g2clib 0 -match "' + $
                matchStr + '" ' + GRIBDir + '/' + GRIBFile
          SPAWN, cmd, GRIBOut, EXIT_STATUS = status

          if (status ne 0) then last_hour = 0
          if (N_ELEMENTS(GRIBOut) eq 0) then $
              last_hour = 0 $
          else $
              if (GRIBOut[0] eq '') then last_hour = 0

;if NOT(last_hour) then PRINT, 'last_hour 03' ; DEBUG

      endif

      if NOT(last_hour) then begin

;+
;         A set of "last hour" records was not found. Check for a
;         "from zero" set; i.e., confirm the presence of APCPC, NCPCP,
;         and WEASD records from the start of the forecast cycle. The
;         idea is to perform differencing between these and
;         corresponding accumulations for the previous forecast hour.
;-

;+
;         Begin with the from_zero flag set to 1 for success, then
;         test that premise thoroughly.
;-
          from_zero = 1

          fcstString = '0-' + STRCRA(TargetFcstHour) + ' hour acc fcst'

          matchStr = ':ACPCP:surface:' + fcstString + ':'
          cmd = 'wgrib2 -g2clib 0 -match "' + $
                matchStr + '" ' + GRIBDir + '/' + GRIBFile
          SPAWN, cmd, GRIBOut, EXIT_STATUS = status

          if (status ne 0) then from_zero = 0
          if (N_ELEMENTS(GRIBOut) eq 0) then $
              from_zero = 0 $
          else $
              if (GRIBOut[0] eq '') then from_zero = 0

;          if NOT(from_zero) then PRINT, 'from_zero 01' ; DEBUG

          if from_zero then begin

              matchStr = ':NCPCP:surface:' + fcstString + ':'
              cmd = 'wgrib2 -g2clib 0 -match "' + $
                    matchStr + '" ' + GRIBDir + '/' + GRIBFile
              SPAWN, cmd, GRIBOut, EXIT_STATUS = status

              if (status ne 0) then from_zero = 0
              if (N_ELEMENTS(GRIBOut) eq 0) then $
                  from_zero = 0 $
              else $
                  if (GRIBOut[0] eq '') then from_zero = 0

;              if NOT(from_zero) then PRINT, 'from_zero 02' ; DEBUG

          endif

          if from_zero then begin

              matchStr = ':WEASD:surface:' + fcstString + ':'
              cmd = 'wgrib2 -g2clib 0 -match "' + $
                    matchStr + '" ' + GRIBDir + '/' + GRIBFile
              SPAWN, cmd, GRIBOut, EXIT_STATUS = status

              if (status ne 0) then from_zero = 0
              if (N_ELEMENTS(GRIBOut) eq 0) then $
                  from_zero = 0 $
              else $
                  if (GRIBOut[0] eq '') then from_zero = 0

;              if NOT(from_zero) then PRINT, 'from_zero 03' ; DEBUG

          endif

          if from_zero then begin

;+
;             Accumulations from the start of the RUC cycle were
;             found. Next, look for a GRIB file that will provide the
;             "from zero" accumulation for the previous forecast
;             hour. Assume that the combination of use_subdirs
;             (implicit in the definition of GRIBDir), use_YYDOY, and
;             use_bgrb established previously will apply. Mixing up
;             archive locations and file types could provide some
;             added flexibility, but would add significant bulk to
;             this code for probably very little benefit.
;-
              case use_YYDOY * 2 + use_bgrb of

                  0 : prevGRIBFile = 'ruc.' + $
                                     cycleDate_YYYYMMDD + $
                                     '.t' + cycleDate_HH + $
                                     'z.awp130pgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'
                  1 : prevGRIBFile = 'ruc.' + $
                                     cycleDate_YYYYMMDD + $
                                     '.t' + cycleDate_HH + $
                                     'z.awp130bgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'
                  2 : prevGRIBFile = cycleDate_YY + cycleDate_DOY + $
                                     '.ruc.t' + cycleDate_HH + $
                                     'z.awp130pgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'
                  3 : prevGRIBFile = cycleDate_YY + cycleDate_DOY + $
                                     '.ruc.t' + cycleDate_HH + $
                                     'z.awp130bgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'

              endcase

              if NOT(FILE_TEST(GRIBDir + '/' + prevGRIBFile)) then $
                  from_zero = 0

              if from_zero then begin 

;print, '[[' + gribdir + '/' +  prevgribfile + ']]'

;+
;                 Accumulations from the start of the RUC cycle to the
;                 forecast hour were found, and the file expected to
;                 contain accumulations from the start of the RUC
;                 cycle to the hour before the forecast hour were
;                 found. Confirm the presence of ACPCP, NCPCP, and
;                 WEASD records from the start of the forecast cycle
;                 in the prevGRIBFile. 
;-
                  prevFcstString = '0-' + STRCRA(TargetFcstHour - 1) + $
                                   ' hour acc fcst'

                  matchStr = ':ACPCP:surface:' + prevFcstString + ':'
                  cmd = 'wgrib2 -g2clib 0 -match "' + $
                        matchStr + '" ' + GRIBDir + '/' + prevGRIBFile
                  SPAWN, cmd, GRIBOut, EXIT_STATUS = status

                  if (status ne 0) then from_zero = 0
                  if (N_ELEMENTS(GRIBOut) eq 0) then $
                      from_zero = 0 $
                  else $
                      if (GRIBOut[0] eq '') then from_zero = 0

;                  if NOT(from_zero) then PRINT, 'from_zero 05' ; DEBUG

                  if from_zero then begin

                      matchStr = ':NCPCP:surface:' + prevFcstString + ':'
                      cmd = 'wgrib2 -g2clib 0 -match "' + $
                            matchStr + '" ' + GRIBDir + '/' + prevGRIBFile
                      SPAWN, cmd, GRIBOut, EXIT_STATUS = status

                      if (status ne 0) then from_zero = 0
                      if (N_ELEMENTS(GRIBOut) eq 0) then $
                          from_zero = 0 $
                      else $
                          if (GRIBOut[0] eq '') then from_zero = 0

;                      if NOT(from_zero) then PRINT, 'from_zero 06' ; DEBUG

                  endif

                  if from_zero then begin

                      matchStr = ':WEASD:surface:' + prevFcstString + ':'
                      cmd = 'wgrib2 -g2clib 0 -match "' + $
                            matchStr + '" ' + GRIBDir + '/' + prevGRIBFile
                      SPAWN, cmd, GRIBOut, EXIT_STATUS = status

                      if (status ne 0) then from_zero = 0
                      if (N_ELEMENTS(GRIBOut) eq 0) then $
                          from_zero = 0 $
                      else $
                          if (GRIBOut[0] eq '') then from_zero = 0

;                      if NOT(from_zero) then PRINT, 'from_zero 07' ; DEBUG

                  endif

              endif

          endif

      endif

      if (last_hour and from_zero) then STOP ; PROGRAMMING ERROR

      if (NOT(last_hour) and NOT(from_zero) and $
          (TargetFcstHour gt 3) and $
          ((TargetFcstHour mod 3) ne 1)) then begin

;+
;         For forecast hours 5, 6, 8, 9, 11, 12, etc., our last chance
;         is to perform  differencing between accumulations starting
;         from the last multiple of 3. For example, for
;         TargetFcstHour = 5, we can subtract the f03-f04 accumulation
;         from the f03-f05 accumulation. However, for TargetFcstHours
;         4, 7, 10, etc., the attempt to use "last_hour" has already
;         been checked and failed, and no differencing is possible
;         anyway.
;-

;+
;         Begin with the from_prev_m3 flag set to 1 for success, then
;         test that premise thoroughly.
;-
          from_prev_m3 = 1

          fcstString = STRCRA((TargetFcstHour - 1) / 3 * 3) + '-' + $
                       STRCRA(TargetFcstHour) + ' hour acc fcst'

          matchStr = ':ACPCP:surface:' + fcstString + ':'
          cmd = 'wgrib2 -g2clib 0 -match "' + $
                matchStr + '" ' + GRIBDir + '/' + GRIBFile
          SPAWN, cmd, GRIBOut, EXIT_STATUS = status

          if (status ne 0) then from_prev_m3 = 0
          if (N_ELEMENTS(GRIBOut) eq 0) then $
              from_prev_m3 = 0 $
          else $
              if (GRIBOut[0] eq '') then from_prev_m3 = 0

          if from_prev_m3 then begin

              matchStr = ':NCPCP:surface:' + fcstString + ':'
              cmd = 'wgrib2 -g2clib 0 -match "' + $
                    matchStr + '" ' + GRIBDir + '/' + GRIBFile
              SPAWN, cmd, GRIBOut, EXIT_STATUS = status

              if (status ne 0) then from_prev_m3 = 0
              if (N_ELEMENTS(GRIBOut) eq 0) then $
                  from_prev_m3 = 0 $
              else $
                  if (GRIBOut[0] eq '') then from_prev_m3 = 0

          endif

          if from_prev_m3 then begin

              matchStr = ':WEASD:surface:' + fcstString + ':'
              cmd = 'wgrib2 -g2clib 0 -match "' + $
                    matchStr + '" ' + GRIBDir + '/' + GRIBFile
              SPAWN, cmd, GRIBOut, EXIT_STATUS = status

              if (status ne 0) then from_prev_m3 = 0
              if (N_ELEMENTS(GRIBOut) eq 0) then $
                  from_prev_m3 = 0 $
              else $
                  if (GRIBOut[0] eq '') then from_prev_m3 = 0

          endif

          if from_prev_m3 then begin

;+
;             Accumulations from the start of the RUC cycle were
;             found. Next, look for a GRIB file that will provide the
;             "from zero" accumulation for the previous forecast
;             hour. Assume that the combination of use_subdirs
;             (implicit in the definition of GRIBDir), use_YYDOY, and
;             use_bgrb established previously will apply. Mixing up
;             archive locations and file types could provide some
;             added flexibility, but would add significant bulk to
;             this code for probably very little benefit.
;-
              case use_YYDOY * 2 + use_bgrb of

                  0 : prevGRIBFile = 'ruc.' + $
                                     cycleDate_YYYYMMDD + $
                                     '.t' + cycleDate_HH + $
                                     'z.awp130pgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'
                  1 : prevGRIBFile = 'ruc.' + $
                                     cycleDate_YYYYMMDD + $
                                     '.t' + cycleDate_HH + $
                                     'z.awp130bgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'
                  2 : prevGRIBFile = cycleDate_YY + cycleDate_DOY + $
                                     '.ruc.t' + cycleDate_HH + $
                                     'z.awp130pgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'
                  3 : prevGRIBFile = cycleDate_YY + cycleDate_DOY + $
                                     '.ruc.t' + cycleDate_HH + $
                                     'z.awp130bgrbf' + $
                                     STRING(TargetFcstHour - 1, $
                                            FORMAT = '(I2.2)') + $
                                     '.grib2'

              endcase

              if NOT(FILE_TEST(GRIBDir + '/' + prevGRIBFile)) then $
                  from_prev_m3 = 0

              if from_prev_m3 then begin 

;print, '((' + gribdir + '/' +  prevgribfile + '))'

;+
;                 Accumulations from the start of the RUC cycle to the
;                 forecast hour were found, and the file expected to
;                 contain accumulations from the start of the RUC
;                 cycle to the hour before the forecast hour were
;                 found. Confirm the presence of ACPCP, NCPCP, and
;                 WEASD records from the start of the forecast cycle
;                 in the prevGRIBFile. 
;-
                  prevFcstString = $
                      STRCRA((TargetFcstHour - 1) / 3 * 3) + $
                      '-' + $
                      STRCRA(TargetFcstHour - 1) + ' hour acc fcst'

                  matchStr = ':ACPCP:surface:' + prevFcstString + ':'
                  cmd = 'wgrib2 -g2clib 0 -match "' + $
                        matchStr + '" ' + GRIBDir + '/' + prevGRIBFile
                  SPAWN, cmd, GRIBOut, EXIT_STATUS = status

                  if (status ne 0) then from_prev_m3 = 0
                  if (N_ELEMENTS(GRIBOut) eq 0) then $
                      from_prev_m3 = 0 $
                  else $
                      if (GRIBOut[0] eq '') then from_prev_m3 = 0

                  if from_prev_m3 then begin

                      matchStr = ':NCPCP:surface:' + prevFcstString + ':'
                      cmd = 'wgrib2 -g2clib 0 -match "' + $
                            matchStr + '" ' + GRIBDir + '/' + prevGRIBFile
                      SPAWN, cmd, GRIBOut, EXIT_STATUS = status

                      if (status ne 0) then from_prev_m3 = 0
                      if (N_ELEMENTS(GRIBOut) eq 0) then $
                          from_prev_m3 = 0 $
                      else $
                          if (GRIBOut[0] eq '') then from_prev_m3 = 0

                  endif

                  if from_prev_m3 then begin

                      matchStr = ':WEASD:surface:' + prevFcstString + ':'
                      cmd = 'wgrib2 -g2clib 0 -match "' + $
                            matchStr + '" ' + GRIBDir + '/' + prevGRIBFile
                      SPAWN, cmd, GRIBOut, EXIT_STATUS = status

                      if (status ne 0) then from_prev_m3 = 0
                      if (N_ELEMENTS(GRIBOut) eq 0) then $
                          from_prev_m3 = 0 $
                      else $
                          if (GRIBOut[0] eq '') then from_prev_m3 = 0

                  endif

              endif

          endif

      endif

;+
;     Verify that the results make sense.
;-
      if (TOTAL(last_hour + from_zero + from_prev_m3) gt 1) $
        then STOP ; PROGRAMMING ERROR

      if (NOT(last_hour) and $
          NOT(from_zero) and $
          NOT(from_prev_m3)) then begin

;+
;         A GRIB file was found for CycleDate_YYYYMMDDHHH and
;         TargetFcstHour, but hourly QPF from APCPC, NCPCP, and WEASD
;         records could not be generated from it.
;-
          GRIBDir = !NULL
          GRIBFile = !NULL
          fcstString = !NULL
          prevGRIBFile = !NULL
          prevFcstString = !NULL

      endif else begin

;+
;         Success.
;-
          if last_hour then begin

              if ISA(prevGRIBFile) then STOP   ; PROGRAMMING ERROR
              if ISA(prevFcstString) then STOP ; PROGRAMMING ERROR

          endif

          fhrpmStatus = 1

      endelse

  endif else begin

;+
;     No data found for CycleDate_YYYYMMDDHH and TargetFcstHour.
;-
      GRIBDir = !NULL
      GRIBFile = !NULL
      fcstString = !NULL
      if ISA(prevGRIBFile) then STOP   ; PROGRAMMING ERROR
      IF ISA(prevFcstString) then STOP ; PROGRAMMING ERROR

  endelse

  RETURN

end
