; Produce snow-to-liquid ratio (SLR) climatology to update results of
; Baxter (2005).

; This program is a nasty memory hog and should be used with caution.

; Greg Fall, NWC-North (Chanhassen)
; Kent Sparrow, NWC (Tuscaloosa)

; June 2016

PRO DRAW_ERROR_BARS, x, y, sigma, $
                     COLOR = color, LINESTYLE = lineStyle

  if (NOT(KEYWORD_SET(color)) and NOT(ARG_PRESENT(color))) then $
      color = 'WHITE'
  if (NOT(KEYWORD_SET(lineStyle)) and NOT(ARG_PRESENT(lineStyle))) then $
      lineStyle = 0

  numPoints = N_ELEMENTS(x)
  if ((N_ELEMENTS(y) ne numPoints) or $
      (N_ELEMENTS(sigma) ne numPoints)) then begin
      MESSAGE, 'Inconsistent x, y, sigma array sizes.'
  endif

  bracketSize = 0.01 * (!X.CRange[1] - !X.CRange[0])
  for i = 0, numPoints - 1 do begin
      PLOTS, [x[i], x[i]], [y[i] - sigma[i], y[i] + sigma[i]], $
             LINESTYLE = lineStyle, COLOR = color
      PLOTS, [x[i] - 0.5 * bracketSize, x[i] + 0.5 * bracketSize], $
             [y[i] + sigma[i], y[i] + sigma[i]], $
             LINESTYLE = lineStyle, COLOR = color
      PLOTS, [x[i] - 0.5 * bracketSize, x[i] + 0.5 * bracketSize], $
             [y[i] - sigma[i], y[i] - sigma[i]], $
             LINESTYLE = lineStyle, COLOR = color
  endfor

  RETURN

end



PRO GATHER_GHCND_SLR_DATA, GHCND_dir, $ ; /nwcdev/archive/GHCN_daily_archive
                           StartYear, $
                           FinishYear, $
                           ClimStartDate_MMDD, $
                           ClimFinishDate_MMDD, $
                           ClimName, $
                           SnflThreshold, $ ; 50.8 mm (Baxter 2005)
                           PrcpThreshold, $ ; 28 tenths of mm (Baxter 2005)
                           Ndv, $
                           NdvFlt, $
                           MinSLRCount, $
                           numStations, $
                           stationData, $
                           prcpData, $
                           snowData, $
                           SLRData

  numStations = !NULL
  stationData = !NULL
  prcpData = !NULL
  snowData = !NULL
  SLRData = !NULL

;+
; Use an IDL save file to get GHCN-D data if possible. Note that the
; ClimName is meant to uniquely describe the period bounded by
; ClimStartDate_MMDD and ClimFinishDate_MMDD.
;-
  ;; climSavFile = 'GHCND_SLR_clim_' + climName + '_' + $
  ;;               STRING(startYear, FORMAT = '(I4.4)') + '_to_' + $
  ;;               STRING(finishYear, FORMAT = '(I4.4)') + '.sav'
  climSavFile = 'GHCND_SLR_data_' + ClimName + '_' + $
                STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
                STRING(finishYear, FORMAT = '(I4.4)') + '.sav'

  if (FILE_TEST(climSavFile)) then begin

;+
;     Make copies of arguments to verify consistency with save file
;     contents.
;-
      GHCND_dir_ = GHCND_dir         ; not needed 20160712
      snflThreshold_ = SnflThreshold ; not needed 20160712
      prcpThreshold_ = PrcpThreshold ; not needed 20160712
      ndv_ = Ndv                     ; not needed 20160712
      ndvFlt_ = NdvFlt
      startYear_ = StartYear
      finishYear_ = FinishYear
      minSLRCount_ = MinSLRCount
      climStartDate_MMDD_ = ClimStartDate_MMDD  ; not needed 20160712
      climFinishDate_MMDD_ = ClimFinishDate_MMDD ; not needed 20160712
      climName_ = ClimName

      RESTORE, climSavFile

      if (GHCND_dir ne GHCND_dir_) then STOP
      if (SnflThreshold ne snflThreshold_) then STOP
      if (PrcpThreshold ne prcpThreshold_) then STOP
      if (Ndv ne ndv_) then STOP
      if (NdvFlt ne ndvFlt_) then STOP
      if (StartYear ne startYear_) then STOP
      if (FinishYear ne finishYear_) then STOP
      if (MinSLRCount ne minSLRCount_) then STOP
      if (ClimStartDate_MMDD ne climStartDate_MMDD_) then STOP
      if (ClimFinishDate_MMDD ne climFinishDate_MMDD_) then STOP
      if (ClimName ne climName_) then STOP

      if NOT(ISA(numStations)) then STOP
      if NOT(ISA(prcpData)) then STOP
      if NOT(ISA(snowData)) then STOP
      if NOT(ISA(SLRData)) then STOP

      RETURN

  endif

;+
; Determine whether climatology period crosses January 1. Start times
; and finish times are in different calendar years in that case.
;-
  climStartMonth = FIX(STRMID(ClimStartDate_MMDD, 0, 2))
  climFinishMonth = FIX(STRMID(ClimFinishDate_MMDD, 0, 2))
  climNumYears = finishYear - StartYear + 1

  finishYear_ = FinishYear ; local copy we can change
  crossNewYear = 0B
  if (climFinishMonth lt climStartMonth) then begin
      crossNewYear = 1B
      finishYear_ = finishYear_ + 1
      PRINT, 'NOTICE: this climatology includes a date range covering ' + $
             'different calendar years.'
  endif

;+
; Read GHCN-D data.
;-
  for year = StartYear, finishYear_ do begin

      READ_GHCND_US_CA_MX_BY_YEAR, GHCND_dir, $
                                   year, $
                                   Ndv, $
                                   numDays, $
                                   numStations, $
                                   GHCNStationID, $
                                   GHCNData, $
                                   /VERBOSE

;+
;     Identify date index range for climatology period.
;-
      YYYY = STRING(Year, FORMAT = '(I4.4)')
      Jan01_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + '010100')
      Dec31_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + '123100')
      Dec31Ind = ROUND(Dec31_Julian - Jan01_Julian)
      if (Dec31Ind ne (numDays - 1)) then STOP

      startDate_YYYYMMDD = YYYY + ClimStartDate_MMDD
      startDate_Julian = YYYYMMDDHH_TO_JULIAN(startDate_YYYYMMDD + '00')
      startDateInd = ROUND(startDate_Julian - Jan01_Julian)

      finishDate_YYYYMMDD = YYYY + climFinishDate_MMDD
      finishDate_Julian = YYYYMMDDHH_TO_JULIAN(finishDate_YYYYMMDD + '00')
      finishDateInd = ROUND(finishDate_Julian - Jan01_Julian)

      if NOT(crossNewYear) then begin

;+
;         Keep only data for climatology period.
;-
          GHCNData = GHCNData[startDateInd:finishDateInd, *]
          numDays = finishDateInd - startDateInd + 1L
          climYearInd = REPLICATE(year - StartYear, numDays)

      endif else begin

          if (finishDateInd ge startDateInd) then STOP ; PROGRAMMING ERROR
          if (numDays ne (Dec31Ind + 1)) then STOP ; PROGRAMMING ERROR
          ;numDays = Dec31Ind + 1

;+
;         Generate an index in the time dimension for all the data we
;         want to keep. This is needed for cases where the date range
;         covers two calendar years (e.g., December 15 to January 15).
;
;         If the start MMDD is greater than the finish MMDD, the
;         following logic applies:
;
;         1. If year is StartYear, keep only days from the start MMDD
;            to December 31.
;
;         2. If year is finishYear_, keep only days from January 1 to
;            the finish MMDD
;
;         3. For other years, keep only days from January 1 to the
;            finish MMDD and from the start MMDD to December 31.
;
;         For example, say we want to do a climatology from 12/15 to
;         1/15, covering 30 periods from Dec/Jan 1986/1987 to Dec/Jan
;         2015/2016. StartYear and FinishYear values sent to this
;         procedure should be 1986 and 2015.
;         First, crossNewYear will be set to 1 and the finishYear_
;         changed to 2016 (above). The following process then occurs.
;         - For 1986, we keep data between 12/15 and 12/31.
;         - For 1987-2015, we keep data between 1/1 and 1/15, and
;           between 12/15 and 12/31.
;         - For 2016, we keep data between 1/1 and 1/15 only.
;         The end result is 30 years x 32 days of data in length, in
;         the time dimension.

          case year of

              StartYear: begin
                  keepInd = startDateInd + $
                            LINDGEN(Dec31Ind - startDateInd + 1L)
                  climYearInd = REPLICATE(year - StartYear, $
                                          Dec31Ind - startDateInd + 1L)
              end

              finishYear_: begin
                  keepInd = LINDGEN(finishDateInd + 1L)
                  climYearInd = REPLICATE(year - StartYear - 1, $
                                          finishDateInd + 1L)
              end

              else: begin
                  keepInd = [LINDGEN(finishDateInd + 1L), $
                             startDateInd + $
                             LINDGEN(Dec31Ind - startDateInd + 1L)]
                  climYearInd = [REPLICATE(year - StartYear - 1, $
                                           finishDateInd + 1L), $
                                 REPLICATE(year - StartYear, $
                                           Dec31Ind - startDateInd + 1L)]
              end

          endcase

          GHCNData = GHCNData[keepInd, *]
          numDays = N_ELEMENTS(keepInd)

      endelse

;+
;     Eliminate sites that reported all no-data values.
;-
      obsCount = BYTARR(numStations)

      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]
          ind = WHERE((GHCNDataRow.prcp ne Ndv) and $
                      (GHCNDataRow.snow ne Ndv), $
                      count)
          obsCount[sc] = count

      endfor

      ind = WHERE(obsCount gt 0, count)
      if (count eq 0) then STOP

      GHCNData = GHCNData[*, ind]
      GHCNStationID = GHCNStationID[ind]
      PRINT, '# precip/snowfall reporters (non-ndv): ' + $
             STRCRA(count)

      numStations = count

;+
;     Create a "year counter" that associates each report with the
;     year-of-the-climatology it comes from. This will enable the
;     program to determing the number of distinct years for which a
;     station provided data. In addition to MinSLRCount, we may
;     require that some fraction and/or range of all available years
;     was covered by any given station.
;-
      climYearInd = TEMPORARY(climYearInd) # REPLICATE(1, numStations)


    ;--------------------------------------------------------------;
    ; Check QC flags mFlag, qFlag, and sFlag. See readme.txt for   ;
    ; the GHCND dataset for details.                               ;
    ;--------------------------------------------------------------;

;+
;     Verify that only known values are present for each flag.
;-
      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]

;+
;         Make sure only known values of mFlag appear.
;-
          ind = WHERE((GHCNDataRow.prcp_mFlag ne '') and $
                      (GHCNDataRow.prcp_mFlag ne 'B') and $
                      (GHCNDataRow.prcp_mFlag ne 'D') and $
                      (GHCNDataRow.prcp_mFlag ne 'H') and $
                      (GHCNDataRow.prcp_mFlag ne 'K') and $
                      (GHCNDataRow.prcp_mFlag ne 'L') and $
                      (GHCNDataRow.prcp_mFlag ne 'O') and $
                      (GHCNDataRow.prcp_mFlag ne 'P') and $
                      (GHCNDataRow.prcp_mFlag ne 'T') and $
                      (GHCNDataRow.prcp_mFlag ne 'W'), $
                      count)

          if (count ne 0) then $
              MESSAGE, 'Unexpected mFlag values for PRCP data.'

          ind = WHERE((GHCNDataRow.snow_mFlag ne '') and $
                      (GHCNDataRow.snow_mFlag ne 'B') and $
                      (GHCNDataRow.snow_mFlag ne 'D') and $
                      (GHCNDataRow.snow_mFlag ne 'H') and $
                      (GHCNDataRow.snow_mFlag ne 'K') and $
                      (GHCNDataRow.snow_mFlag ne 'L') and $
                      (GHCNDataRow.snow_mFlag ne 'O') and $
                      (GHCNDataRow.snow_mFlag ne 'P') and $
                      (GHCNDataRow.snow_mFlag ne 'T') and $
                      (GHCNDataRow.snow_mFlag ne 'W'), $
                      count)

          if (count ne 0) then $
              MESSAGE, 'Unexpected mFlag values for SNOW data.'

;+
;         Make sure only known values of qFlag appear.
;-
          ind = WHERE((GHCNDataRow.prcp_qFlag ne '') and $
                      (GHCNDataRow.prcp_qFlag ne 'D') and $
                      (GHCNDataRow.prcp_qFlag ne 'G') and $
                      (GHCNDataRow.prcp_qFlag ne 'I') and $
                      (GHCNDataRow.prcp_qFlag ne 'K') and $
                      (GHCNDataRow.prcp_qFlag ne 'L') and $
                      (GHCNDataRow.prcp_qFlag ne 'M') and $
                      (GHCNDataRow.prcp_qFlag ne 'N') and $
                      (GHCNDataRow.prcp_qFlag ne 'O') and $
                      (GHCNDataRow.prcp_qFlag ne 'R') and $
                      (GHCNDataRow.prcp_qFlag ne 'S') and $
                      (GHCNDataRow.prcp_qFlag ne 'T') and $
                      (GHCNDataRow.prcp_qFlag ne 'W') and $
                      (GHCNDataRow.prcp_qFlag ne 'X') and $
                      (GHCNDataRow.prcp_qFlag ne 'Z'), $
                      count)

          if (count ne 0) then $
              MESSAGE, 'Unexpected qFlag values for PRCP data.'

          ind = WHERE((GHCNDataRow.snow_qFlag ne '') and $
                      (GHCNDataRow.snow_qFlag ne 'D') and $
                      (GHCNDataRow.snow_qFlag ne 'G') and $
                      (GHCNDataRow.snow_qFlag ne 'I') and $
                      (GHCNDataRow.snow_qFlag ne 'K') and $
                      (GHCNDataRow.snow_qFlag ne 'L') and $
                      (GHCNDataRow.snow_qFlag ne 'M') and $
                      (GHCNDataRow.snow_qFlag ne 'N') and $
                      (GHCNDataRow.snow_qFlag ne 'O') and $
                      (GHCNDataRow.snow_qFlag ne 'R') and $
                      (GHCNDataRow.snow_qFlag ne 'S') and $
                      (GHCNDataRow.snow_qFlag ne 'T') and $
                      (GHCNDataRow.snow_qFlag ne 'W') and $
                      (GHCNDataRow.snow_qFlag ne 'X') and $
                      (GHCNDataRow.snow_qFlag ne 'Z'), $
                      count)

          if (count ne 0) then $
              MESSAGE, 'Unexpected qFlag values for SNOW data.'

;+
;         Make sure only known values of sFlag appear.
;-
          ind = WHERE((GHCNDataRow.prcp_sFlag ne '') and $
                      (GHCNDataRow.prcp_sFlag ne '0') and $
                      (GHCNDataRow.prcp_sFlag ne '6') and $
                      (GHCNDataRow.prcp_sFlag ne '7') and $
                      (GHCNDataRow.prcp_sFlag ne 'A') and $
                      (GHCNDataRow.prcp_sFlag ne 'a') and $
                      (GHCNDataRow.prcp_sFlag ne 'B') and $
                      (GHCNDataRow.prcp_sFlag ne 'b') and $
                      (GHCNDataRow.prcp_sFlag ne 'C') and $
                      (GHCNDataRow.prcp_sFlag ne 'D') and $
                      (GHCNDataRow.prcp_sFlag ne 'E') and $
                      (GHCNDataRow.prcp_sFlag ne 'F') and $
                      (GHCNDataRow.prcp_sFlag ne 'G') and $
                      (GHCNDataRow.prcp_sFlag ne 'H') and $
                      (GHCNDataRow.prcp_sFlag ne 'I') and $
                      (GHCNDataRow.prcp_sFlag ne 'K') and $
                      (GHCNDataRow.prcp_sFlag ne 'M') and $
                      (GHCNDataRow.prcp_sFlag ne 'N') and $
                      (GHCNDataRow.prcp_sFlag ne 'Q') and $
                      (GHCNDataRow.prcp_sFlag ne 'R') and $
                      (GHCNDataRow.prcp_sFlag ne 'r') and $
                      (GHCNDataRow.prcp_sFlag ne 'S') and $
                      (GHCNDataRow.prcp_sFlag ne 's') and $
                      (GHCNDataRow.prcp_sFlag ne 'T') and $
                      (GHCNDataRow.prcp_sFlag ne 'U') and $
                      (GHCNDataRow.prcp_sFlag ne 'u') and $
                      (GHCNDataRow.prcp_sFlag ne 'W') and $
                      (GHCNDataRow.prcp_sFlag ne 'X') and $
                      (GHCNDataRow.prcp_sFlag ne 'Z') and $
                      (GHCNDataRow.prcp_sFlag ne 'z'), $
                      count)

          if (count ne 0) then $
              MESSAGE, 'Unexpected sFlag values for PRCP data.'

          ind = WHERE((GHCNDataRow.snow_sFlag ne '') and $
                      (GHCNDataRow.snow_sFlag ne '0') and $
                      (GHCNDataRow.snow_sFlag ne '6') and $
                      (GHCNDataRow.snow_sFlag ne '7') and $
                      (GHCNDataRow.snow_sFlag ne 'A') and $
                      (GHCNDataRow.snow_sFlag ne 'a') and $
                      (GHCNDataRow.snow_sFlag ne 'B') and $
                      (GHCNDataRow.snow_sFlag ne 'b') and $
                      (GHCNDataRow.snow_sFlag ne 'C') and $
                      (GHCNDataRow.snow_sFlag ne 'D') and $
                      (GHCNDataRow.snow_sFlag ne 'E') and $
                      (GHCNDataRow.snow_sFlag ne 'F') and $
                      (GHCNDataRow.snow_sFlag ne 'G') and $
                      (GHCNDataRow.snow_sFlag ne 'H') and $
                      (GHCNDataRow.snow_sFlag ne 'I') and $
                      (GHCNDataRow.snow_sFlag ne 'K') and $
                      (GHCNDataRow.snow_sFlag ne 'M') and $
                      (GHCNDataRow.snow_sFlag ne 'N') and $
                      (GHCNDataRow.snow_sFlag ne 'Q') and $
                      (GHCNDataRow.snow_sFlag ne 'R') and $
                      (GHCNDataRow.snow_sFlag ne 'r') and $
                      (GHCNDataRow.snow_sFlag ne 'S') and $
                      (GHCNDataRow.snow_sFlag ne 's') and $
                      (GHCNDataRow.snow_sFlag ne 'T') and $
                      (GHCNDataRow.snow_sFlag ne 'U') and $
                      (GHCNDataRow.snow_sFlag ne 'u') and $
                      (GHCNDataRow.snow_sFlag ne 'W') and $
                      (GHCNDataRow.snow_sFlag ne 'X') and $
                      (GHCNDataRow.snow_sFlag ne 'Z') and $
                      (GHCNDataRow.snow_sFlag ne 'z'), $
                      count)

          if (count ne 0) then $
              MESSAGE, 'Unexpected sFlag values for SNOW data.'

      endfor

;+
;     Verify mFlag for PRCP. Acceptable values: '', B, D, T
;-
      obsCount = BYTARR(numStations)

      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]

;+
;         Identify observations that passed mFlag for this station.
;-
          okInd = WHERE((GHCNDataRow.prcp_mFlag ne 'H') and $
                        (GHCNDataRow.prcp_mFlag ne 'K') and $
                        (GHCNDataRow.prcp_mFlag ne 'L') and $
                        (GHCNDataRow.prcp_mFlag ne 'O') and $
                        (GHCNDataRow.prcp_mFlag ne 'P') and $
                        (GHCNDataRow.prcp_mFlag ne 'W'), $
                        okCount, $
                        COMPLEMENT = badInd, $
                        NCOMPLEMENT = badCount)

          obsCount[sc] = okCount
          if (badCount gt 0) then GHCNData[badInd, sc].prcp = Ndv

      endfor

      ind = WHERE(obsCount gt 0, count)
      if (count eq 0) then STOP
      GHCNData = GHCNData[*, ind]
      GHCNStationID = GHCNStationID[ind]
      climYearInd = climYearInd[*, ind]
      PRINT, '# precip/snowfall reporters (PRCP passed mFlag): ' + $
             STRCRA(count)

      numStations = count

;+
;     Verify mFlag for SNOW. Acceptable values: '', B, D, T.
;     Note: many values of zero exist with mFlag set to P (identified
;     as "missing presumed zero" in DSI 3200 and 3206). Here, we are
;     not interested in zeroes anyway, so while converting these
;     zeroes to no-data values may appear to eliminate a lot of data,
;     it is only eliminating zeroes, which are irrelevant to SLR
;     calculations.
;-
      obsCount = BYTARR(numStations)

      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]


;         Identify observations that passed mFlag for this station.

          okInd = WHERE((GHCNDataRow.snow_mFlag ne 'H') and $
                        (GHCNDataRow.snow_mFlag ne 'K') and $
                        (GHCNDataRow.snow_mFlag ne 'L') and $
                        (GHCNDataRow.snow_mFlag ne 'O') and $
                        (GHCNDataRow.snow_mFlag ne 'P') and $
                        (GHCNDataRow.snow_mFlag ne 'W'), $
                        okCount, $
                        COMPLEMENT = badInd, $
                        NCOMPLEMENT = badCount)

          obsCount[sc] = okCount
          if (badCount gt 0) then GHCNData[badInd, sc].snow = Ndv

      endfor

      ind = WHERE(obsCount gt 0, count)
      if (count eq 0) then STOP
      GHCNData = GHCNData[*, ind]
      GHCNStationID = GHCNStationID[ind]
      climYearInd = climYearInd[*, ind]
      PRINT, '# precip/snowfall reporters (SNOW passed mFlag): ' + $
             STRCRA(count)

      numStations = count

;+
;     Verify qFlag for PRCP. Acceptable values: ''
;-
      obsCount = BYTARR(numStations)

      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]

;+
;         Identify observations that passed qFlag for this station.
;-
          okInd = WHERE((GHCNDataRow.prcp_qFlag ne 'D') and $
                        (GHCNDataRow.prcp_qFlag ne 'G') and $
                        (GHCNDataRow.prcp_qFlag ne 'I') and $
                        (GHCNDataRow.prcp_qFlag ne 'K') and $
                        (GHCNDataRow.prcp_qFlag ne 'L') and $
                        (GHCNDataRow.prcp_qFlag ne 'M') and $
                        (GHCNDataRow.prcp_qFlag ne 'N') and $
                        (GHCNDataRow.prcp_qFlag ne 'O') and $
                        (GHCNDataRow.prcp_qFlag ne 'R') and $
                        (GHCNDataRow.prcp_qFlag ne 'S') and $
                        (GHCNDataRow.prcp_qFlag ne 'T') and $
                        (GHCNDataRow.prcp_qFlag ne 'W') and $
                        (GHCNDataRow.prcp_qFlag ne 'X') and $
                        (GHCNDataRow.prcp_qFlag ne 'Z'), $
                        okCount, $
                        COMPLEMENT = badInd, $
                        NCOMPLEMENT = badCount)

          obsCount[sc] = okCount
          if (badCount gt 0) then GHCNData[badInd, sc].prcp = Ndv

      endfor

      ind = WHERE(obsCount gt 0, count)
      if (count eq 0) then STOP
      GHCNData = GHCNData[*, ind]
      GHCNStationID = GHCNStationID[ind]
      climYearInd = climYearInd[*, ind]
      PRINT, '# precip/snowfall reporters (PRCP passed qFlag): ' + $
             STRCRA(count)

      numStations = count

;+
;     Verify qFlag for SNOW. Acceptable values: ''
;-
      obsCount = BYTARR(numStations)

      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]


;         Identify observations that passed qFlag for this station.

          okInd = WHERE((GHCNDataRow.snow_qFlag ne 'D') and $
                        (GHCNDataRow.snow_qFlag ne 'G') and $
                        (GHCNDataRow.snow_qFlag ne 'I') and $
                        (GHCNDataRow.snow_qFlag ne 'K') and $
                        (GHCNDataRow.snow_qFlag ne 'L') and $
                        (GHCNDataRow.snow_qFlag ne 'M') and $
                        (GHCNDataRow.snow_qFlag ne 'N') and $
                        (GHCNDataRow.snow_qFlag ne 'O') and $
                        (GHCNDataRow.snow_qFlag ne 'R') and $
                        (GHCNDataRow.snow_qFlag ne 'S') and $
                        (GHCNDataRow.snow_qFlag ne 'T') and $
                        (GHCNDataRow.snow_qFlag ne 'W') and $
                        (GHCNDataRow.snow_qFlag ne 'X') and $
                        (GHCNDataRow.snow_qFlag ne 'Z'), $
                        okCount, $
                        COMPLEMENT = badInd, $
                        NCOMPLEMENT = badCount)

          obsCount[sc] = okCount
          if (badCount gt 0) then GHCNData[badInd, sc].snow = Ndv

      endfor

      ind = WHERE(obsCount gt 0, count)
      if (count eq 0) then STOP
      GHCNData = GHCNData[*, ind]
      GHCNStationID = GHCNStationID[ind]
      climYearInd = climYearInd[*, ind]
      PRINT, '# precip/snowfall reporters (SNOW passed qFlag): ' + $
             STRCRA(count)

      numStations = count

;+
;     Verify sFlag for PRCP. Acceptable values: '', 0, 6, 7, A, B, C,
;                                               F, G, H, I, K, M, N,
;                                               R, T, U, W, X, Z
;-
      obsCount = BYTARR(numStations)

      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]

;+
;         Identify observations that passed sFlag for this station.
;-
          okInd = WHERE((GHCNDataRow.prcp_sFlag ne 'a') and $
                        (GHCNDataRow.prcp_sFlag ne 'b') and $
                        (GHCNDataRow.prcp_sFlag ne 'E') and $
                        (GHCNDataRow.prcp_sFlag ne 'Q') and $
                        (GHCNDataRow.prcp_sFlag ne 'r') and $
                        (GHCNDataRow.prcp_sFlag ne 'S') and $
                        (GHCNDataRow.prcp_sFlag ne 's') and $
                        (GHCNDataRow.prcp_sFlag ne 'u') and $
                        (GHCNDataRow.prcp_sFlag ne 'z'), $
                        okCount, $
                        COMPLEMENT = badInd, $
                        NCOMPLEMENT = badCount)

          obsCount[sc] = okCount
          if (badCount gt 0) then GHCNData[badInd, sc].prcp = Ndv

      endfor

      ind = WHERE(obsCount gt 0, count)
      if (count eq 0) then STOP
      GHCNData = GHCNData[*, ind]
      GHCNStationID = GHCNStationID[ind]
      climYearInd = climYearInd[*, ind]
      PRINT, '# precip/snowfall reporters (PRCP passed sFlag): ' + $
             STRCRA(count)

      numStations = count

;+
;     Verify sFlag for SNOW. Acceptable values: '', 0, 6, 7, A, B, C,
;                                               F, G, H, I, K, M, N,
;                                               R, T, U, W, X, Z
;-
      obsCount = BYTARR(numStations)

      for sc = 0, numStations - 1 do begin

          GHCNDataRow = GHCNData[*, sc]


;         Identify observations that passed sFlag for this station.

          okInd = WHERE((GHCNDataRow.snow_sFlag ne 'a') and $
                        (GHCNDataRow.snow_sFlag ne 'b') and $
                        (GHCNDataRow.snow_sFlag ne 'E') and $
                        (GHCNDataRow.snow_sFlag ne 'Q') and $
                        (GHCNDataRow.snow_sFlag ne 'r') and $
                        (GHCNDataRow.snow_sFlag ne 'S') and $
                        (GHCNDataRow.snow_sFlag ne 's') and $
                        (GHCNDataRow.snow_sFlag ne 'u') and $
                        (GHCNDataRow.snow_sFlag ne 'z'), $
                        okCount, $
                        COMPLEMENT = badInd, $
                        NCOMPLEMENT = badCount)

          obsCount[sc] = okCount
          if (badCount gt 0) then GHCNData[badInd, sc].snow = Ndv

      endfor

      ind = WHERE(obsCount gt 0, count)
      if (count eq 0) then STOP
      GHCNData = GHCNData[*, ind]
      GHCNStationID = GHCNStationID[ind]
      climYearInd = climYearInd[*, ind]
      PRINT, '# precip/snowfall reporters (SNOW passed sFlag): ' + $
             STRCRA(count)

      numStations = count


    ;--------------------------------------------------------------;
    ; Done checking QC flags. Structure elements mFlag, qFlag, and ;
    ; sFlag are no longer needed.                                  ;
    ;--------------------------------------------------------------;

;+
;     Extract PRCP and SNOW values from GHCN data. The remaining
;     structure elements are no longer needed, and aggregating them
;     costs a huge amount of memory.
;-
      GHCNPrcpData = FIX(GHCNData[*,*].prcp)
      GHCNSnowData = FIX(GHCNData[*,*].snow)
 
      if (year eq StartYear) then begin

          allGHCNPrcpData = GHCNPrcpData
          allGHCNSnowData = GHCNSnowData
          allGHCNStationID = GHCNStationID
          allClimYearInd = climYearInd
          allNumDays = numDays
          allNumStations = numStations

      endif else begin

;+
;         Aggregate data. In this section the allGHCN[Prcp,Snow]Data
;         arrays will probably need to be expanded to accommodate
;         stations that were not present in previous years.
;
;         Verify dimensions of allGHCN[Prcp,Snow]Data, just to verify
;         we do not have programming errors.
;-
          sizeAllGHCNPrcpData = SIZE(allGHCNPrcpData)
          if (sizeAllGHCNPrcpData[0] ne 2) then STOP
          if (sizeAllGHCNPrcpData[1] ne allNumDays) then STOP
          if (sizeAllGHCNPrcpData[2] ne allNumStations) then STOP

          sizeAllGHCNSnowData = SIZE(allGHCNSnowData)
          if (sizeAllGHCNSnowData[0] ne 2) then STOP
          if (sizeAllGHCNSnowData[1] ne allNumDays) then STOP
          if (sizeAllGHCNSnowData[2] ne allNumStations) then STOP

;+
;         Initialize variables for inventory.
;-
          allIDInd = MAKE_ARRAY(numStations, /LONG, VALUE = -1L)
          noIDCount = 0L

;+
;         Inventory stations from current year found in
;         allGHCN[Prcp,Snow]Data (allIDInd) and those not found in
;         allGHCN[Prcp,Snow]Data (noIDInd). The first list (allIDInd)
;         are indices of allGHCN[Prcp,Snow]Data where we should put
;         rows of GHCN[Prcp,Snow]Data, the second (noIDInd) are
;         indices of GHCN[Prcp,Snow]Data that should be added to
;         expanded versions of allGHCN[Prcp,Snow]Data.
;-
          for sc = 0, numStations - 1 do begin

;+
;             Identify index in allGHCNStationID for current station
;             in GHCNStationID.
;-
              ind = WHERE(allGHCNStationID eq GHCNStationID[sc], count)
              if (count eq 0) then begin
                  noIDCount++
                  CONTINUE
              endif
              if (count ne 1) then STOP
              allIDInd[sc] = ind[0]

          endfor

          noIDInd = WHERE(allIDInd eq -1L, count)
          if (count ne noIDCount) then STOP
          PRINT, '# stations to add to aggregate dataset: ' + $
                 STRCRA(noIDCount)

;+
;         Define dimensions of expanded data array.
;-
          oldAllNumDays = allNumDays         ; not needed but helps w/clarity
          oldAllNumStations = allNumStations ; ditto above

          newAllNumDays = allNumDays + numDays
          newAllNumStations = allNumStations + noIDCount

;+
;         Check programming.
;-
          if ((newAllNumDays - oldAllNumDays) ne numDays) then STOP

;+
;         Create expanded arrays and partially populate them with
;         existing data.
;-
          newAllGHCNPrcpData = $
              MAKE_ARRAY(newAllNumDays, newAllNumStations, $
                         /INT, VALUE = Ndv)
          newAllGHCNPrcpData[0:oldAllNumDays - 1, $
                             0:oldAllNumStations - 1] = $
              allGHCNPrcpData
          allGHCNPrcpData = TEMPORARY(newAllGHCNPrcpData)
          ;; newAllGHCNPrcpData = -1L ; free memory

          newAllGHCNSnowData = $
              MAKE_ARRAY(newAllNumDays, newAllNumStations, $
                         /INT, VALUE = Ndv)
          newAllGHCNSnowData[0:oldAllNumDays - 1, $
                             0:oldAllNumStations - 1] = $
              allGHCNSnowData
          allGHCNSnowData = TEMPORARY(newAllGHCNSnowData)
          ;; newAllGHCNSnowData = -1L ; free memory

          newAllGHCNStationID = STRARR(newAllNumStations)
          newAllGHCNStationID[0:allNumStations - 1] = allGHCNStationID
          allGHCNStationID = TEMPORARY(newAllGHCNStationID)
          ;; newAllGHCNStationID = -1L ; free memory

          newAllClimYearInd = $
              MAKE_ARRAY(newAllNumDays, newAllNumStations, $
                         /INT, VALUE = Ndv)
          newAllClimYearInd[0:oldAllNumDays - 1, $
                            0:oldAllNumStations - 1] = $
              allClimYearInd
          allClimYearInd = TEMPORARY(newAllClimYearInd)

;+
;         Add new data to the expanded arrays.
;-
          newIDIndex = oldAllNumStations

          for sc = 0, numStations - 1 do begin

              if (allIDInd[sc] ne -1L) then begin

                  allGHCNPrcpData[oldAllNumDays:newAllNumDays - 1, $
                                  allIDInd[sc]] = $
                      GHCNPrcpData[*, sc]

                  allGHCNSnowData[oldAllNumDays:newAllNumDays - 1, $
                                  allIDInd[sc]] = $
                      GHCNSnowData[*, sc]

                  allClimYearInd[oldAllNumDays:newAllNumDays - 1, $
                                 allIDInd[sc]] = $
                      climYearInd[*, sc]

              endif else begin

                  allGHCNPrcpData[oldAllNumDays:newAllNumDays - 1, $
                                  newIDIndex] = $
                      GHCNPrcpData[*, sc]

                  allGHCNSnowData[oldAllNumDays:newAllNumDays - 1, $
                                  newIDIndex] = $
                      GHCNSnowData[*, sc]

                  allClimYearInd[oldAllNumDays:newAllNumDays - 1, $
                                 newIDIndex] = $
                      climYearInd[*, sc]

                  allGHCNStationID[newIDIndex] = GHCNStationID[sc]
                  newIDIndex++

              endelse

          endfor

          allNumDays = newAllNumDays
          allNumStations = newAllNumStations

      endelse

      PRINT, '# stations in aggregate dataset: ' + STRCRA(allNumStations)

  endfor ; for year = StartYear, finishYear_ do begin

;+
; Free memory.
;-
  GHCNData = !NULL
  GHCNPrcpData = !NULL
  GHCNSnowData = !NULL
  climYearInd = !NULL
  GHCNStationID = !NULL

;+
; Get station metadata.
;-
  stationData = REPLICATE({GHCN_Daily_station, $
                           id: '', $
                           latitude: 0.0D, $
                           longitude: 0.0D, $
                           elevation: 0.0, $
                           state: '', $
                           name: '', $
                           gsn_flag: '', $
                           hcn_crn_flag: '', $
                           wmo_id: ''}, $
                          allNumStations)

  if NOT(FILE_TEST(GHCND_dir + '/ghcnd-stations_US_CA_MX.txt')) then STOP
  OPENR, lun, GHCND_dir + '/ghcnd-stations_US_CA_MX.txt', /GET_LUN

  id = ''
  latitude = ''
  longitude = ''
  elevation = ''
  state = ''
  name = ''
  gsn_flag = ''
  hcn_crn_flag = ''
  wmo_id = ''

  flag = BYTARR(allNumStations)

  while NOT(EOF(lun)) do begin

      READF, lun, $
             id, latitude, longitude, elevation, state, name, $
             gsn_flag, hcn_crn_flag, wmo_id, $
             FORMAT = '(A11, 1X, A8, 1X, A9, 1X, A6, 1X, A2, 1X, A30, ' + $
             '1X, A3, 1X, A3, 1X, A5)'

      ind = WHERE(allGHCNStationID eq id, count)

      if (count eq 0) then CONTINUE
      if (count ne 1) then STOP ; PROGRAMMING ERROR
      ind = ind[0]
      stationData[ind].id = id
      stationData[ind].latitude = DOUBLE(latitude)
      stationData[ind].longitude = DOUBLE(longitude)
      stationData[ind].elevation = FLOAT(elevation)
      stationData[ind].state = state
      stationData[ind].name = name
      stationData[ind].gsn_flag = gsn_flag
      stationData[ind].hcn_crn_flag = hcn_crn_flag
      stationData[ind].wmo_id = wmo_id

      flag[ind] = 1B

  endwhile

  FREE_LUN, lun

  ind = WHERE(flag eq 1B, count)
  if (count ne allNumStations) then begin
      PRINT, '# stations with missing metadata: ' + $
             STRCRA(allNumStations - count)
      ;; MESSAGE, 'WARNING: missing metadata for ' + $
      ;;          STRCRA(allNumStations - count) + ' sites.', /CONTINUE
      allGHCNPrcpData = allGHCNPrcpData[*, ind]
      allGHCNSnowData = allGHCNSnowData[*, ind]
      allGHCNStationID = allGHCNStationID[ind]
      allClimYearInd = allClimYearInd[*, ind]
      stationData = stationData[ind]
      allNumStations = count
  endif

;+
; Inventory aggregate data using Baxter criteria.
;-
  SLRData = MAKE_ARRAY(allNumDays, allNumStations, /FLOAT, VALUE = NdvFlt)

  numSLRStations = 0L
  numSLRByStation = LONARR(allNumStations)
  numSLR10ByStation = LONARR(allNumStations)
  SLR10Suspect = BYTARR(allNumStations)
  numClimYearsRepresented = LONARR(allNumStations)
  numNotEnoughSLR = 0L
  numBelowThreshold = 0L

  for sc = 0L, allNumStations - 1L do begin

;+
;     First verify that the station does not have all no-data values
;     for (implied) SLR. Because QC checks were applied individually
;     on PRCP and SNOW data, this is possible even though the all-ndv
;     test was done each year prior to QC checking.
;
;     Note that if MinSLRCount values of SLR cannot be calculated for
;     a given station, SLRs are NOT calculated for that station, even
;     if it is technically possible to do so.
;-
      prcpData = allGHCNPrcpData[*,sc]
      snowData = allGHCNSnowData[*,sc]
      climYearInd = allClimYearInd[*,sc]

      ind = WHERE((prcpData ne Ndv) and (snowData ne Ndv), count)
      if (count lt MinSLRCount) then begin
          numNotEnoughSLR++
          CONTINUE
      endif

;+
;     Identify cases where prcpData and snowData both exceed
;     thresholds for generating SLR values. Note that ind will be used
;     for the remainder of this iteration of the "for sc" loop.
;-
      ind = WHERE((prcpData gt PrcpThreshold) and $
                  (snowData gt SnflThreshold), count)
      numSLRByStation[sc] = count

      if (count lt MinSLRCount) then begin

          numBelowThreshold++
          CONTINUE              ; SLR from below-threshold PRCP/SNOW
                                ; NEVER goes into SLRData.
      endif

;+
;     Only count climYearInd information where SLR data exists and for
;     stations that have met the MinSLRCount criterion.
;-
      climYears = climYearInd[ind]
      climYears = UNIQ(climYears) ; already sorted by definition
      numClimYearsRepresented[sc] = N_ELEMENTS(climYears)

      SLRData[ind, sc] = 10.0 * FLOAT(snowData[ind]) / FLOAT(prcpData[ind])

;+
;     If a station reports SLR close to 10.0 more than half the time,
;     it gets flagged.
;-
      stationSLR = SLRData[ind, sc]
      ind = WHERE((stationSLR gt 9.99) and (stationSLR lt 10.01), count10)
      numSLR10ByStation[sc] = count10
      if (count10 gt (numSLRByStation[sc] / 2)) then SLR10Suspect[sc] = 1B

      numSLRStations++ ; note that SLR = 10 suspects are still "SLR stations"

  endfor

  PRINT, '# stations with fewer than ' + STRCRA(MinSLRCount) + $
         ' SLR values: ' + STRCRA(numNotEnoughSLR)
  PRINT, '# stations with fewer than ' + STRCRA(MinSLRCount) + $
         ' SLR values based on above-threshold precipitation ' + $
         'and snowfall: ' + $
         STRCRA(numBelowThreshold)

  PRINT, '# stations contributing to initial SLR dataset: ' + $
         STRCRA(numSLRStations)

;-
; Wipe out 10.0 suspects.
;-
  ind = WHERE(SLR10Suspect, count)
  if (count gt 0) then begin
      SLRData[*, ind] = NdvFlt
      PRINT, '# stations suspected of regularly reporting SLR = 10: ' + $
             STRCRA(count)
  endif

;+
; Eliminate sites that met other criteria but did not provide reports
; during at least half the years of the climatology.
; Set minClimYearsRepresented = 1 to disable this check.
; If it is not set to 1, this value probably needs to be in the IDL
; save file. This code was tested in summer 2019 but we decided this
; criterion--demanding that a station have data for a large portion of
; the climatology period in order to be included--was excessive.
;-
  minClimYearsRepresented = 1
  ;; minClimYearsRepresented = (climNumYears + 1) / 2
  ;; minClimYearsRepresented = climNumYears / 3
  ind = WHERE((numClimYearsRepresented lt minClimYearsRepresented) and $
              (numSLRByStation ge MinSLRCount) and $
              (SLR10Suspect eq 0), count)
  if (count gt 0) then begin
      ;; for ssc = 0, count - 1 do begin
      ;;     thissiteslrdata = SLRData[*, ind[ssc]]
      ;;     if (max(thissiteslrdata) ne ndvflt) then begin
      ;;         print, max(thissiteslrdata)
      ;;         stop
      ;;     endif
      ;; endfor
      SLRData[*, ind] = NdvFlt
      PRINT, '# valid SLR reporting stations that did not report in ' + $
             'at least ' + STRCRA(minClimYearsRepresented) + $
             ' years of the ' + STRCRA(climNumYears) + '-year period: ' + $
             STRCRA(count)
      if (minClimYearsRepresented eq 1) then STOP ; should not happen
  endif

;+
; Get index of stations we will use. Extract data for those alone,
; reclaiming variable names without the "all" prefix.
;-
  ind = WHERE((numSLRByStation ge MinSLRCount) and $
              (SLR10Suspect eq 0B) and $
              (numClimYearsRepresented ge minClimYearsRepresented), $
              count)
  if (count eq 0) then STOP

  PRINT, '# of stations meeting all criteria for SLR contribution: ' + $
         STRCRA(count)

;+
; Reduce all relevant data to just the stations we need for SLR.
;-
  numStations = count
  stationData = stationData[ind]
  prcpData = allGHCNPrcpData[*, ind]
  snowData = allGHCNSnowData[*, ind]
  SLRData = SLRData[*, ind]
  allGHCNStationID = !NULL
  allGHCNPrcpData = !NULL
  allGHCNSnowData = !NULL
  allIDInd = !NULL
  flag = !NULL

;+
; Save/restore.
;-
  SAVE, GHCND_dir, SnflThreshold, PrcpThreshold, Ndv, NdvFlt, $
        StartYear, FinishYear, MinSLRCount, $
        ClimStartDate_MMDD, climFinishDate_MMDD, ClimName, $
        numStations, stationData, prcpData, snowData, SLRData, $
        FILE = climSavFile

  PRINT, 'Saved SLR data for "' + ClimName + '" ' + $
         STRCRA(StartYear) + ' - ' + STRCRA(FinishYear) + $
         ' to ' + climSavFile

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP                           

  RETURN

end



PRO GENERATE_SLR_CLIMATOLOGY, GHCND_dir, $ ; /nwcdev/archive/GHCN_daily_archive
                              StartYear_, $
                              FinishYear_, $
                              ClimStartDate_MMDD, $ ; e.g., "0201"
                              ClimFinishDate_MMDD, $ ; e.g., "0228"
                              ClimName, $
                              PREPARE_ONLY = prepareOnly, $
                              LONG_CLIM_NAME = LongClimName

;+
; Use the years associated with the ClimStartDate_MMDD to define the
; climatology period. For example: if you are doing a DJF climatology
; that covers December 2010-2014 and January/February 2011-2015
; (i.e. covers five years of contiguous DJF periods), call this
; subroutine with StartYear_ = 2010 and FinishYear_ = 2014. The
; GATHER_GHCND_SLR_DATA procedure will know that the December data
; should be from from 2010-2014 while the January/February data should
; be from 2011-2015.
;
; If prepareOnly is set by keyword, then the program will stop before
; calling GEO_KRIGE_2D.
;-
  if NOT(KEYWORD_SET(prepareOnly)) then prepareOnly = 0

  if NOT(KEYWORD_SET(LongClimName)) then LongClimName = ClimName

;+
; From Baxter (2005) section 2:
; "Only snowfalls greater than 50.8 mm (2 in.) and liquid
; equivalents greater than 2.8 mm (0.11 in.) were included, following
; the standard set by Roebber et al. (2003)."
;-
  snflThreshold = 50.8 ; 2 inches = 50.8 mm
  prcpThreshold = 28 ; 0.11 inch = 2.8 mm = 28 tenths of mm

  ndv = -9999 ; no data value in IDL save files
  ndvFlt = FLOAT(ndv)

;+
; Set precision for calls to the DISTANCE function.
;   0 : Very crude Eulidean
;   1 : Crude Euclidean
;   2 : Great circle
;   3 : Ellipsoidal
;-
  dPrecision = 3

  startYear = StartYear_
  finishYear = FinishYear_
  numYears = finishYear - startYear + 1

;+
; Calculate the number of SLR values (i.e., concurrent reports of
; daily snowfall and precipitation) required for a station to
; contribute to the SLR climatology. In the current logic, these need
; not occur in different years.
;-
  minSLRCount = (numYears / 2) > 1


  PRINT, '*** GENERATING SLR CLIMATOLOGY FOR ' + $
         STRUPCASE(LongClimName) + ', ' + $
         STRING(startYear, FORMAT = '(I4.4)') + ' to ' + $
         STRING(finishYear, FORMAT = '(I4.4)') + ' ***'

  GATHER_GHCND_SLR_DATA, GHCND_dir, $
                         StartYear_, $
                         FinishYear_, $
                         ClimStartDate_MMDD, $
                         ClimFinishDate_MMDD, $
                         ClimName, $
                         snflThreshold, $
                         prcpThreshold, $
                         ndv, $
                         ndvFlt, $
                         minSLRCount, $
                         numStations, $
                         stationData, $
                         prcpData, $
                         snowData, $
                         SLRData

  if NOT(ISA(numStations)) then STOP
  if NOT(ISA(stationData)) then STOP
  if NOT(ISA(prcpData)) then STOP
  if NOT(ISA(snowData)) then STOP
  if NOT(ISA(SLRData)) then STOP

;+
; Eliminate stations with bad elevation values.
;-
  ind = WHERE(stationData.elevation ne -999.9, count)
  if (count eq 0) then STOP ; got to be some of these!

  stationData = stationData[ind]
  prcpData = prcpData[*, ind]
  snowData = snowData[*, ind]
  SLRData = SLRData[*, ind]

  if (count ne numStations) then begin
      PRINT, 'Removal of sites with bad elevation data reduced station ' + $
             'count from ' + STRCRA(numStations) + ' to ' + STRCRA(count)
  endif

  numStations = count

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Eliminated colocated stations.
;-
  flag = MAKE_ARRAY(numStations, /BYTE, VALUE = 1B)

  for j = 1L, numStations - 1L do begin

      lonj = stationData[j].longitude
      latj = stationData[j].latitude

      for i = 0L, j - 1L do begin

          loni = stationData[i].longitude
          lati = stationData[i].latitude

          lonDiff = ABS(lonj - loni)
          latDiff = ABS(latj - lati)

          if ((lonDiff lt 1.0D-6) and (latDiff lt 1.0D-6)) then begin

              iInd = WHERE(SLRData[*, i] ne ndvFlt, iCount)
              jInd = WHERE(SLRData[*, j] ne ndvFlt, jCount)
              if (iCount gt jCount) then begin
                  keepInd = i
                  keepCount = iCount
                  loseInd = j
                  loseCount = jCount
              endif else begin
                  keepInd = j
                  keepCount = jCount
                  loseInd = i
                  loseCount = iCount
              endelse
              flag[loseInd] = 0B
              PRINT, 'Station "' + stationData[loseInd].id + '" ' + $
                     '(' + STRCRA(loseCount) + ' reports) ' + $
                     'removed due to colocation with "' + $
                     stationData[keepInd].id + '" (' + $
                     STRCRA(keepCount) + $
                     ' reports) at lon ' + $
                     STRCRA(stationData[keepInd].longitude) + ' = ' + $
                     STRCRA(stationData[loseInd].longitude) + ', lat ' + $
                     STRCRA(stationData[keepInd].latitude) + ' = ' + $
                     STRCRA(stationData[loseInd].latitude)
          endif

      endfor

  endfor

  ind = WHERE(flag eq 1B, count)

  if (count gt 0) then begin

      stationData = stationData[ind]
      prcpData = prcpData[*, ind]
      snowData = snowData[*, ind]
      SLRData = SLRData[*, ind]

      if (count ne numStations) then begin
          PRINT, 'Removal of colocated sites reduced station ' + $
                 'count from ' + STRCRA(numStations) + ' to ' + STRCRA(count)
      endif

      numStations = count

  endif

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Set maximum lag and lag tolerance for variogram analysis.
;-
  maxLagMeters = 500.0D3 ; 500 km, pretty standard nowaddays (GF 2019)
  lagTolerance = 5.0D3

;+
; Define the output domain.
;-
  listAreas = 1
  limitToPolygon = -1 ; set to -1 to not limit to a specific polygon

  limitToArea = 'WESTERN CORDILLERA'
  limitToArea = 'OZARK/OUACHITA-APPALACHIAN FORESTS' & limitToPolygon = 8
  limitToArea = 'TEMPERATE PRAIRIES' & limitToPolygon = -1
  limitToArea = 'ATLANTIC HIGHLANDS' & limitToPolygon = 15
  limitToArea = 'MIXED WOOD PLAINS' & limitToPolygon = 222 ; OH to NY
  limitToArea = 'MIXED WOOD PLAINS' & limitToPolygon = 82 ; MI
  limitToArea = 'MIXED WOOD PLAINS' & limitToPolygon = 139  ; MN/WI
  limitToArea = 'COLD DESERTS' & limitToPolygon = 5
  limitToArea = 'OZARK/OUACHITA-APPALACHIAN FORESTS' & limitToPolygon = 8
  limitToArea = 'WESTERN CORDILLERA' & limitToPolygon = 5
  limitToArea = 'MIXED WOOD PLAINS' & limitToPolygon = 139  ; MN/WI

  limitToArea = ''

  if (limitToArea ne '') then begin

;+
;     Limit data to an area defined via shapefile.
;-

;     Identify unique areas in North America ecoregions by name.

      areaType = 'NA_L2NAME'    ; 'NA_L[1,2,3]NAME'
      shapeFilePath = '/net/lfs0data5/Land_Cover/NA_CEC_Eco/' + $
                      'NA_CEC_Eco_Level2_WGS84'
      shapeFile = OBJ_NEW('IDLffShape', shapeFilePath + '.shp')
      shapeFile->IDLffShape::GetProperty, N_ATTRIBUTES = numAtts
      shapeFile->IDLffShape::GetProperty, ATTRIBUTE_NAMES = attNames
      divInd = WHERE(attNames eq areaType, count)
      if (count ne 1) then STOP
      divInd = divInd[0]
      shapeFile->IDLffShape::GetProperty, N_ENTITIES = numPolygons
      PRINT, 'Shapefile contains ' + STRCOMPRESS(numPolygons, /REMOVE_ALL) + $
             ' polygons'


;     Identify unique areas by name. An "area" can have multiple
;     polygons associated with it. This section is a little
;     extraneous, but it is useful because it prints out all the
;     different ecoregion names.

      numAreas = 0

      polygonInArea = BYTARR(numPolygons)

      if (listAreas) then $
          PRINT, 'Areas of type "' + areaType + '" in ecoregions:'
      for pc = 0, numPolygons - 1 do begin
          attrib = shapeFile->getAttributes(pc)
          thisArea = attrib.(divInd)
          if (thisArea eq '') then begin
              PRINT, 'WARNING: no ' + areaType + $
                     ' name (expect a few of these) ' + $
                     'for polygon ', pc
              CONTINUE
          endif
          if (numAreas eq 0) then begin
              area = [thisArea]
              numAreas++
              if (listAreas) then PRINT, thisArea
          endif else begin
              ind = WHERE(area eq thisArea, count)
              if (count eq 0) then begin
                  area = [area, thisArea]
                  if (listAreas) then PRINT, thisArea
                  numAreas++
              endif
          endelse
          if (thisArea eq limitToArea) then polygonInArea[pc] = 1B
      endfor

      PRINT, 'Found ' + STRCOMPRESS(numAreas, /REMOVE_ALL) + $
             ' unique "' + areaType + '" areas'

      areaInd = WHERE(area eq limitToArea, count)
      if (count ne 1) then STOP

      polygonCount = TOTAL(polygonInArea, /INTEGER)
      if (polygonCount eq 0) then STOP

      PRINT, 'Number of polygons with "' + areaType + '" of "' + $
             limitToArea + '" is ' + STRCRA(polygonCount)

      polygonInd = WHERE(polygonInArea eq 1, count)
      if (count ne polygonCount) then STOP

      allCount = 0L

      for pc = 0, polygonCount - 1 do begin
          if ((limitToPolygon ne -1) and $
              (pc ne limitToPolygon)) then CONTINUE
          attrib = shapeFile->getAttributes(polygonInd[pc])
          thisArea = attrib.(divInd)
          if (thisArea ne limitToArea) then STOP
          polygon = shapeFile->IDLffShape::GetEntity(polygonInd[pc])
          inThisPolygon = IN_SHAPE(stationData.longitude, $
                                   stationData.latitude, $
                                   polygon)
          ind = WHERE(inThisPolygon eq 1, count)
          if (count gt 0) then begin
              if (allCount eq 0) then begin
                  allInd = ind
                  allCount = count
              endif else begin
                  allInd = [allInd, ind]
                  allCount += count
              endelse
          endif
          PRINT, '# of points in polygon ' + STRCRA(pc) + ' of "' + $
                 limitToArea + '" is ' + STRCRA(COUNT)
      endfor

      OBJ_DESTROY, shapeFile

      PRINT, 'Found ' + STRCRA(allCount) + ' points in "' + $
             limitToArea + '" ecoregions.'

      ind = allInd
      count = allCount

      if (count eq 0) then STOP

      stationData = stationData[ind]
      prcpData = prcpData[*, ind]
      snowData = snowData[*, ind]
;      targetVar = targetVar[*, ind]
      SLRData = SLRData[*, ind]

      numStations = count

      minLonOut = FLOOR(MIN(stationData.longitude))
      maxLonOut = CEIL(MAX(stationData.longitude))
      minLatOut = FLOOR(MIN(stationData.latitude))
      maxLatOut = CEIL(MAX(stationData.latitude))

  endif else begin

;+
;     National Snowfall Analysis version 1 and 2 output domain.
;-
      minLonOut = -126.0D
      maxLonOut = -66.0D
      minLatOut = 21.0D
      maxLatOut = 55.0D
      domainLabel = 'CONUS'

  endelse

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Set resolution.
;-
  resOut = 0.125D
  resOutArcSec = resOut * 3600.0D
  xResOut = resOut
  yResOut = resOut

  nxOut = ROUND((maxLonOut - minLonOut) / xResOut)
  nyOut = ROUND((maxLatOut - minLatOut) / yResOut)

;+
; Verify grid geometry is perfect.
;-
  xErr = ABS(nxOut * xResOut - (maxLonOut - minLonOut))
  if (xERR gt 1.0D-8) then STOP
  yErr = ABS(nyOut * yResOut - (maxLatOut - minLatOut))
  if (yErr gt 1.0D-8) then STOP

;+
; Read elevation data that will be used to generate the
; elevation-trend-based SLR background.
;-
  GET_RASTER, 'Terrain (GMTED 2010) (30sec) (elevation)', $
              elevGrid, elevHdr, status, $
              nxElev, nyElev, $
              minLonElev, maxLonElev, minLatElev, maxLatElev, $
              MINLON = minLonOut, $
              MAXLON = maxLonOut, $
              MINLAT = minLatOut, $
              MAXLAT = maxLatOut

  if NOT(status) then STOP

  if NOT(COMPARE(minLonElev, minLonOut)) then STOP
  if NOT(COMPARE(maxLonElev, maxLonOut)) then STOP
  if NOT(COMPARE(minLatElev, minLatOut)) then STOP
  if NOT(COMPARE(maxLatElev, maxLatOut)) then STOP

  elevGrid = ROTATE(elevGrid, 7) ; make north-up

;+
; Verify that the resampling of elevation data can be accomplished via
; the IDL REBIN function, due to integer ratios between input and
; output resolution.
;-
  xResFactor = xResOut / elevHdr.x_axis_resolution
  if NOT(COMPARE(DOUBLE(ROUND(xResFactor)), xResFactor)) then STOP
  yResFactor = yResOut / elevHdr.y_axis_resolution
  if NOT(COMPARE(DOUBLE(ROUND(yResFactor)), yResFactor)) then STOP

;+
; Resample elevations. REBIN uses bilinear resampling by default.
; elevGrid will be a key component of the gridded SLR climatology.
;-
  flagGrid = FLOAT(elevGrid ne elevHdr.no_data_value)
  flagGrid = REBIN(flagGrid, nxOut, nyOut)
  elevGrid = REBIN(elevGrid, nxOut, nyOut)
  ind = WHERE(flagGrid lt 1.0, count)
  if (count gt 0) then begin
      MESSAGE, 'WARNING: elevation grid has ' + STRCRA(count) + $
               ' no-data values.', /CONTINUE
      elevGrid[ind] = elevHdr.no_data_value
  endif

;+
; Calculate a lon/lat box associated with maxLagMeters.
;-

;+
; The length of a degree in latitude is smallest at the equator, for the
; typical ellipsoid, so use the 3.6 arc seconds about the equator to
; estimate meters per degree latitude.
;-
  mPerDegLatRef = $
      DOUBLE(FLOOR(DISTANCE(dPrecision, $
                            0.0D, 0.0005D, 0.0D, -0.0005D) * 1000.0D))
  maxLagDegLat = maxLagMeters / mPerDegLatRef

;+
; The length of a degree in longitude is smallest at the highest latitude, so
; use that latitude to estimate meters per degree longitude.
;-
  maxLatRef = (ABS(maxLatOut) > ABS(minLatOut)) + maxLagDegLat
  mPerDegLonRef = $
      DOUBLE(FLOOR(DISTANCE(dPrecision, 0.0D, maxLatRef, 1.0D, maxLatRef)))
  deltaLon = maxLagMeters / mPerDegLonRef

;+
; The input data domain for this process should, if possible, exceed
; the output domain, so that near the boundaries the influence of
; points that are nearby, but not strictly within the bounds of the
; output domain, can be realized.
;
; Make sure padLon and padLat are an integer number of output grid cells.
;-
  extraRows = CEIL(maxLagDegLat / yResOut)
  padLat = yResOut * extraRows
  extraCols = CEIL(deltaLon / xResOut)
  padLon = xResOut * extraCols

  minLonAnl = minLonOut - padLon
  maxLonAnl = maxLonOut + padLon
  minLatAnl = minLatOut - padLat
  maxLatAnl = maxLatOut + padLat

  ind = WHERE(SLRData ne ndvFlt, beforeCount)

  PRINT, 'Complete US/CA/MX dataset with:'
  PRINT, ' - at least ' + STRCRA(minSLRCount) + ' SLR values at each station;'
  PRINT, ' - regular reporters of SLR = 10 eliminated;'
  PRINT, ' - Baxter criteria for snowfall and precipitation amounts met;'
  PRINT, ' - good elevation data;'
  PRINT, ' - colocated stations avoided:'
  PRINT, 'Full # obs: ' + STRCRA(beforeCount)
  PRINT, '# stations: ' + STRCRA(numStations)

  ind = WHERE((stationData.longitude gt minLonAnl) and $
              (stationData.longitude lt maxLonAnl) and $
              (stationData.latitude gt minLatAnl) and $
              (stationData.latitude lt maxLatAnl), $
              count)

  if (count eq 0) then STOP

  stationData = stationData[ind]
  prcpData = prcpData[*, ind]
  snowData = snowData[*, ind]
  SLRData = SLRData[*, ind]
  numStations = count

  ind = WHERE(SLRData ne ndvFlt, afterCount)

  PRINT, 'Dataset for output domain plus analysis halo'
  PRINT, '(longitude ' + STRCRA(minLonAnl) + ' to ' + STRCRA(maxLonAnl) + $
         ','
  PRINT, 'latitude ' + STRCRA(minLatAnl) + ' to ' + STRCRA(maxLatAnl) + ')'
  PRINT, '  Full # obs: ' + STRCRA(afterCount)
  PRINT, '  # stations: ' + STRCRA(numStations)

;+
; Calculate the IQR of the remaining data.
;-
  IQR = GET_QUANTILE(SLRData[ind], [0.25, 0.75])
  PRINT, 'IQR (0.25 to 0.75) after domain adjustment ranges from ' + $
         STRCRA(IQR[0]) + ' to ' + STRCRA(IQR[1]) + $
         ' (IQR = ' + STRCRA(IQR[1] - IQR[0]) + ')'

  beforeCount = afterCount

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Establish whether or not SLR statistics will be based on log SLR vs
; SLR. Since SLR is generally lognormally distributed, "useLog10 = 1"
; is ideal. However, the analysis benefits from greater simplicity if
; "useLog10 = 0" is used. It is worth at least confirming that
; performing the analysis without logarithms does not change the
; results significantly.
;-
  useLog10 = 0

;+
; Eliminate individual observations judged to be outliers.
;-

;+
; Statistics for ALL observations at ALL stations in a pre-defined
; neighborhood of each station are collected and used to perform an
; outlier test on individual observations at that station.
;-
  PRINT, 'Checking full data set for outliers.'

  QCHoodSearchRadiusMeters = 1.0d5 ; 100 km

  ind = WHERE(SLRData ne ndvFLt, count)
  if (count eq 0) then STOP

  if (useLog10) then begin

;+
;     Compute the log of SLR.
;-
      logSLRData = SLRData
      logSLRData[ind] = ALOG10(SLRData[ind])

      targetVar = logSLRData

  endif else begin

      targetVar = SLRData

  endelse

;+
; Get mean and standard deviation for all data.
;-
  targetVarMean = MEAN(targetVar[ind])
  targetVarSD = STDDEV(targetVar[ind])

;+
; Get neighborhood means and standard deviations for test variable.
;-
  PRINT, 'Getting neighborhood statistics.'

  GET_GEO_NEIGHBORHOOD_STATS, stationData.longitude, $
                              stationData.latitude, $
                              targetVar, $
                              ndvFlt, $
                              QCHoodSearchRadiusMeters, $
                              targetVarHoodMean, $
                              targetVarHoodSD, $
                              /HASH

; Kent - check on this below. Should we change the nearest neighbor
;        search radius? 
; For stations that received no data values for neighborhood mean and
; standard deviation, find the results for the nearest neighbor and
; use those values, as long as that neighbor is within 2x the
; QCHoodSearchRadiusMeters parameter.

  ind = WHERE((targetVarHoodMean eq ndvFlt) or $
              (targetVarHoodSD eq ndvFlt), count)

  if (count gt 0) then begin

      PRINT, 'Trying nearest-neighbor mean and std. dev. values for ' + $
             STRCRA(count) + ' sites with sparse or empty neighborhoods.'

      FIND_NEAREST_NEIGHBOR, stationData.longitude, $
                             stationData.latitude, $
                             nnInd, nnDist, $
                             /HASH, $
                             CHECK_INDEX = ind

      for ic = 0, count - 1 do begin
          if (nnDist[ic] gt (2.0 * QCHoodSearchRadiusMeters)) $
          then CONTINUE
          targetVarHoodMean[ind[ic]] = targetVarHoodMean[nnInd[ic]]
          targetVarHoodSD[ind[ic]] = targetVarHoodSD[nnInd[ic]]
      endfor

  endif

  oldCount = count
  ind = WHERE((targetVarHoodMean eq ndvFlt) or $
              (targetVarHoodSD eq ndvFlt), count)

  if (count ne oldCount) then $
      PRINT, 'Nearest neighbor values fixed no-data for ' + $
             STRCRA(oldCount - count) + ' sites.'

  if (count gt 0) then begin
      PRINT, 'Using full population mean (' + STRCRA(targetVarMean) + $
             ') and std. dev. (' + STRCRA(targetVarSD) + ') for '  + $
             STRCRA(count) + ' remaining sites (least preferred option).'
      targetVarHoodMean[ind] = targetVarMean
      targetVarHoodSD[ind] = targetVarSD
  endif

;+
; Perform a rough outlier test on the individual reports. For a given
; site, any reports that are outside
;
;   targetVarHoodMean +/- 2.0 * targetVarHoodSD
;
; are not used to get statistics for that site.
;-
  PRINT, 'Running outlier test on individual reports.'

  ind = WHERE(SLRData ne ndvFlt, beforeCount)


; Calculate the IQR of the data before outliers are removed.

  ;; IQR = GET_QUANTILE(SLRData[ind], [0.25, 0.75])
  ;; PRINT, 'IQR (0.25 to 0.75) before outlier removal ranges from ' + $
  ;;        STRCRA(IQR[0]) + ' to ' + STRCRA(IQR[1]) + $
  ;;        ' (IQR = ' + STRCRA(IQR[1] - IQR[0]) + ')'

;+
; Convert to no-data values for outliers.
;-
  for sc = 0L, numStations - 1L do begin

      if ((targetVarHoodMean[sc] ne ndvFlt) and $
          (targetVarHoodSD[sc] ne ndvFlt)) then begin
          outThreshLow = targetVarHoodMean[sc] - 2.0 * targetVarHoodSD[sc]
          outThreshHigh = targetVarHoodMean[sc] + 2.0 * targetVarHoodSD[sc]
      endif else begin
          STOP ; PROGRAMMING CHECK - THIS SHOULD NEVER HAPPEN
      endelse

      targetVarRow = targetVar[*, sc]
      ind = WHERE((targetVarRow ne ndvFlt) and $
                  ((targetVarRow lt outThreshLow) or $
                   (targetVarRow gt outThreshHigh)), count)
      if (count gt 0) then begin
          targetVar[ind, sc] = ndvFlt
          SLRData[ind, sc] = ndvFlt
          if (useLog10) then logSLRData[ind, sc] = ndvFlt
      endif

  endfor

  ind = WHERE(SLRData ne ndvFlt, afterCount)

  ;; PRINT, 'Removal of individual outlier reports reduced full obs count'
  ;; PRINT, 'from ' + STRCRA(beforeCount) + ' to ' + STRCRA(afterCount)

  beforeCount = afterCount


; Calculate the IQR of the data after outlier removal.

  ;; IQR = GET_QUANTILE(SLRData[ind], [0.25, 0.75])
  ;; PRINT, 'IQR (0.25 to 0.75) AFTER OUTLIER REMOVAL ranges from ' + $
  ;;        STRCRA(IQR[0]) + ' to ' + STRCRA(IQR[1]) + $
  ;;        ' (IQR = ' + STRCRA(IQR[1] - IQR[0]) + ')'

;+
; Eliminate stations that have fallen below the minSLRCount threshold.
;-
  noOutlierCount = LONARR(numStations)

  for sc = 0L, numStations - 1L do begin
      ind = WHERE(SLRData[*, sc] ne ndvFlt, count)
      noOutlierCount[sc] = count
  endfor

  ind = WHERE(noOutlierCount ge minSLRCount, count)
  if (count eq 0) then STOP

  ;; PRINT, 'Removal of outliers reduced station count ' + $
  ;;        'from ' + STRCRA(numStations) + ' to ' + STRCRA(count)

  stationData = stationData[ind]

  targetVarHoodMean = targetVarHoodMean[ind]
  targetVarHoodSD = targetVarHoodSD[ind]

  prcpData = prcpData[*, ind]
  snowData = snowData[*, ind]
  targetVar = targetVar[*, ind]
  SLRData = SLRData[*, ind]
  if (useLog10) then logSLRData = logSLRData[*, ind]

  numStations = count

  ind = WHERE(SLRData ne ndvFlt, afterCount)

  ;; PRINT, 'Removal of outliers reduced full obs count ' + $
  ;;        'from ' + STRCRA(beforeCount) + ' to ' + STRCRA(afterCount)

  ;; PRINT, 'Removal of individual outlier reports reduced full obs count'
  ;; PRINT, 'from ' + STRCRA(beforeCount) + ' to ' + STRCRA(afterCount)

  PRINT, 'Dataset after outlier removal...'
  PRINT, '  Full # obs: ' + STRCRA(afterCount)
  PRINT, '  # stations: ' + STRCRA(numStations)

;+
; Calculate the IQR of the data after outlier removal.
;-
  IQR = GET_QUANTILE(SLRData[ind], [0.25, 0.75])
  PRINT, 'IQR (0.25 to 0.75) after outlier removal ranges from ' + $
         STRCRA(IQR[0]) + ' to ' + STRCRA(IQR[1]) + $
         ' (IQR = ' + STRCRA(IQR[1] - IQR[0]) + ')'

;+
; At this point we have all the snow data, precip data, SLR values,
; and station metadata. Outlier values are gone, and each station
; remaining has enough non-outlier data to contribute to the climatology.
;-
  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Calculate the mean SLR, number of qualifying reports, and quantiles
; for each station.
;-
  stationNumVals = MAKE_ARRAY(numStations, /LONG, VALUE = 0)
  stationMeanVal = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndvFlt)
  stationSDVal = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndvFlt)
  stationQ25Val = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndvFlt)
  stationQ50Val = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndvFlt)
  stationQ75Val = MAKE_ARRAY(numStations, /FLOAT, VALUE = ndvFlt)

  for sc = 0L, numStations - 1L do begin

      stationTargetVar = targetVar[*, sc]
      ind = WHERE(stationTargetVar ne ndvFlt, count)
      if (count eq 0) then STOP
      if (count lt minSLRCount) then STOP ; PROGRAMMING ERROR
      stationNumVals[sc] = count
      stationTargetVar = stationTargetVar[ind]

      stationMeanVal[sc] = MEAN(stationTargetVar)
      stationSDVal[sc] = STDDEV(stationTargetVar)

      quants = GET_QUANTILE(stationTargetVar, [0.25, 0.5, 0.75])
      stationQ25Val[sc] = quants[0]
      stationQ50Val[sc] = quants[1]
      stationQ75Val[sc] = quants[2]

  endfor

  quants = -1L ; free memory

  if (MIN(stationNumVals) lt minSLRCount) then STOP ; PROGRAMMING CHECK

;+
; Eliminate stations that have zero, or low, stationSDVal
; values. These sites are reporting the same SLR all the time.
;-
  badInd = WHERE(stationSDVal lt 1.0e-6, badCount, $
                 COMPLEMENT = ind, NCOMPLEMENT = count)
  if (count eq 0) then STOP

  if (badCount gt 0) then begin

      PRINT, 'Eliminating ' + STRCRA(badCount) + $
             ' stations always reporting the same SLR.'

      stationData = stationData[ind]
      targetVarHoodMean = targetVarHoodMean[ind]
      targetVarHoodSD = targetVarHoodSD[ind]
      prcpData = prcpData[*, ind]
      snowData = snowData[*, ind]
      targetVar = targetVar[*, ind]
      SLRData = SLRData[*, ind]
      if (useLog10) then logSLRData = logSLRData[*, ind]
      stationNumVals = stationNumVals[ind]
      stationMeanVal = stationMeanVal[ind]
      stationSDVal = stationSDVal[ind]
      stationQ25Val = stationQ25Val[ind]
      stationQ50Val = stationQ50Val[ind]
      stationQ75Val = stationQ75Val[ind]
      numStations = count

      PRINT, '  # stations: ' + STRCRA(numStations)

  endif

  ind = !NULL
  badInd = !NULL


;+
; Write station data to csv file.
;-
  station_file = 'GHCND_Stations_' + ClimName  + '_' + $
         STRCRA(startYear) + '_to_' + STRCRA(finishYear) + '.csv'
  OPENW, lun, station_file, /GET_LUN

  PRINTF, lun, 'longitude,latitude,elevation,id,state,name,gsn_flag,hcn_crn_flag,wmo_id'
  for sc = 0, numStations - 1 do begin
      PRINTF, lun, $
              STRCRA(STRING(stationData[sc].longitude, $
                            FORMAT = '(f11.6)')) + ',' + $
              STRCRA(STRING(stationData[sc].latitude, $
                            FORMAT = '(f10.6)')) + ',' + $
              STRCRA(STRING(stationData[sc].elevation, $
                            FORMAT = '(f9.2)')) + ',' + $
              stationData[sc].id + ',' + $
              stationData[sc].state + ',' + $
              stationData[sc].name + ',' + $
              stationData[sc].gsn_flag + ',' + $
              stationData[sc].hcn_crn_flag + ',' + $
              stationData[sc].wmo_id
  endfor

  FREE_LUN, lun
  PRINT, 'Wrote station metadata to ' + station_file

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Display the neighborhood mean.
;-
  ;; red = [200, 000, 000, 000, 000, 000, 127, 240, 204, 255, 255, 255, 237, $
  ;;        238, 204]
  ;; grn = [255, 255, 240, 180, 140, 204, 255, 240, 127, 125, 175, 105, 064, $
  ;;        045, 000]
  ;; blu = [255, 255, 240, 240, 000, 000, 000, 000, 000, 000, 185, 105, 000, $
  ;;        045, 000]
  ;; binEdges = [6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, $
  ;;             17.0, 18.0, 19.0, 20.0, 21.0]
 
  ;; vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector'

  ;; killme = targetVarHoodMean
  ;; ind = WHERE(targetVarHoodMean ne ndvFlt, count)
  ;; if (count eq 0) then STOP
  ;; if (useLog10) then begin
  ;;     remark = '(log space) '
  ;;     killme[ind] = 10.0^killme[ind]
  ;; endif else begin
  ;;     remark = ''
  ;; endelse

  ;; X_MAP_GEO_POINTS, stationData.longitude, $
  ;;                   stationData.latitude, $
  ;;                   killme, $
  ;;                   minLonOut, maxLonOut, minLatOut, maxLatOut, $
  ;;                   binEdges, red, grn, blu, $
  ;;                   2, $
  ;;                   status, $
  ;;                   XSIZE = 800, YSIZE = 800, $ ; just suggestions
  ;;                   NDV = ndvFlt, $
  ;;                   /SHOW_LOW, $
  ;;                   /SHOW_HIGH, $
  ;;                   TITLE = 'Neighborhood (' + $
  ;;                   STRCRA(ROUND(QCHoodSearchRadiusMeters / 1000.0)) + ' km) ' + $
  ;;                   'Average SLR ' + remark + 'for ' + $
  ;;                   ClimName  + ' ' + $
  ;;                   STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
  ;;                   /COLORBAR, $
  ;;                   UNITS = 'Dimensionless', $
  ;;                   SHAPE_PATH_LIST = [vectorDir + '/' + $
  ;;                                      'National_Boundary_Canada', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'Provincial_Boundaries', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'National_Boundary_Mexico_2004', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'National_Boundary_Coterminous_US', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'State_Boundaries_Coterminous_US']


  ;; killMe = !NULL

;; Display the neighborhood standard deviation.

  ;; red = [128, 189, 227, 252, 253, 254, 254, 255, 255]
  ;; grn = [000, 000, 026, 078, 141, 178, 217, 237, 255]
  ;; blu = [038, 038, 028, 042, 060, 076, 118, 160, 204]
  ;; binEdges = FINDGEN(10) / 9.0 * 0.45
 
  ;; vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector'

  ;; X_MAP_GEO_POINTS, stationData.longitude, $
  ;;                   stationData.latitude, $
  ;;                   targetVarHoodSD, $
  ;;                   minLonOut, maxLonOut, minLatOut, maxLatOut, $
  ;;                   binEdges, red, grn, blu, $
  ;;                   3, $
  ;;                   status, $
  ;;                   XSIZE = 800, YSIZE = 800, $ ; just suggestions
  ;;                   NDV = ndvFlt, $
  ;;                   /SHOW_LOW, $
  ;;                   /SHOW_HIGH, $
  ;;                   TITLE = 'Neighborhood Average SLR Std. Dev. for ' + $
  ;;                   ClimName  + ' ' + $
  ;;                   STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
  ;;                   /COLORBAR, $
  ;;                   UNITS = 'Dimensionless', $
  ;;                   SHAPE_PATH_LIST = [vectorDir + '/' + $
  ;;                                      'National_Boundary_Canada', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'Provincial_Boundaries', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'National_Boundary_Mexico_2004', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'National_Boundary_Coterminous_US', $
  ;;                                      vectorDir + '/' + $
  ;;                                      'State_Boundaries_Coterminous_US']

  ;; mathErrors = CHECK_MATH()
  ;; if (mathErrors ne 0) then STOP

;+
; Display mean SLR values on the map.
;-
  red = [200, 000, 000, 000, 000, 000, 127, 240, 204, 255, 255, 255, 237, $
         238, 204]
  grn = [255, 255, 240, 180, 140, 204, 255, 240, 127, 125, 175, 105, 064, $
         045, 000]
  blu = [255, 255, 240, 240, 000, 000, 000, 000, 000, 000, 185, 105, 000, $
         045, 000]
  binEdges = [6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, $
              17.0, 18.0, 19.0, 20.0, 21.0]
 
  vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector'
  vectorDir = '../../../resources/shapefiles'

;+
; Log vs. not-log
;-
  if (useLog10) then begin
      killme = 10.0^stationMeanVal
      quantStr = 'Geometric Mean SLR'
  endif else begin
      killme = stationMeanVal
      quantStr = 'Mean SLR'
  endelse

  X_MAP_GEO_POINTS, stationData.longitude, $
                    stationData.latitude, $
                    killme, $
                    minLonOut, maxLonOut, minLatOut, maxLatOut, $
                    binEdges, red, grn, blu, $
                    0, $
                    status, $
                    XSIZE = 800, YSIZE = 800, $ ; just suggestions
                    NDV = ndvFlt, $
                    /SHOW_LOW, $
                    /SHOW_HIGH, $
                    TITLE = quantStr + ' for ' + LongClimName  + ' ' + $
                            STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
                    /COLORBAR, $
                    UNITS = 'Dimensionless', $
                    SHAPE_PATH_LIST = [vectorDir + '/' + $
                                       'National_Boundary_Canada', $
                                       vectorDir + '/' + $
                                       'Provincial_Boundaries', $
                                       vectorDir + '/' + $
                                       'National_Boundary_Mexico_2004', $
                                       vectorDir + '/' + $
                                       'National_Boundary_Coterminous_US', $
                                       vectorDir + '/' + $
                                       'State_Boundaries_Coterminous_US']

;+
; Generate a PNG map of the station mean SLR values.
;-
  PNGImage = 'GHCND_SLR_station_means_' + $
             domainLabel + '_' + $
             ClimName + '_' + $
             STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
             STRING(finishYear, FORMAT = '(I4.4)') + '.png'

  killMeGrid = MAKE_ARRAY(nxOut, nyOut, VALUE = ndvFlt)
  MAKE_LON_LAT_MAP_PNG, killMeGrid, ndvFlt, $
                        binEdges, red, grn, blu, $
                        xResOut, minLonOut, maxLonOut, $
                        yResOut, minLatOut, maxLatOut, $
                        quantStr + ' for ' + LongClimName + ', ' + $
                        STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
                        'Dimensionless', $
                        PNGImage, $
                        POINT_LON = stationData.longitude, $
                        POINT_LAT = stationData.latitude, $
                        POINT_VAL = killme, $
                        /NO_OUT_OF_BOUNDS, $
                        /SHOW_LOW, $
                        /SHOW_HIGH, $
                        MAP_SHAPE_PATH = [vectorDir + '/' + $
                                          'National_Boundary_Canada', $
                                          vectorDir + '/' + $
                                          'Provincial_Boundaries', $
                                          vectorDir + '/' + $
                                          'National_Boundary_Mexico_2004', $
                                          vectorDir + '/' + $
                                          'National_Boundary_Coterminous_US', $
                                          vectorDir + '/' + $
                                          'State_Boundaries_Coterminous_US'], $
                        /NO_GRID, $
                        /NO_CONTINENTS, $
                        /NO_USA, $
                        /BLACK_ON_WHITE

  PRINT, 'Station SLR means displayed in ' + PNGImage

  killme = !NULL
  killMeGrid = !NULL

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP


;---------------;
; KRIGING START ;
;---------------;

;+
; Draw SLR vs. elevation, with error bars, and fit the trend.
;-
  WSET_OR_WINDOW, 1

  if (useLog10) then begin

;+
;     Set up the plot but with /NODATA.
;-
      lowVal = 10.0^(stationMeanVal - stationSDVal)
      highVal = 10.0^(stationMeanVal + stationSDVal)

      PLOT, stationData.elevation, 10.0^stationMeanVal, /NODATA, /YNOZERO, $
;            YRANGE = [MIN(lowVal), MAX(highVal)] > 0.0, $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    ClimName  + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'GHCN-D Station Elevation (meters)', $
            YTITLE = 'Geometric Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9], CHARSIZE = 1.5

;+
;     Draw error bars.
;-
      bracketSize = 0.01 * (!X.CRange[1] - !X.CRange[0])

      for sc = 0, numStations - 1 do begin
          PLOTS, [stationData[sc].elevation, stationData[sc].elevation], $
                 [lowVal[sc], highVal[sc]], $
                 COLOR = 100
          PLOTS, [stationData[sc].elevation - 0.5 * bracketSize, $
                  stationData[sc].elevation + 0.5 * bracketSize], $
                 [highVal[sc], highVal[sc]], $
                 COLOR = 100
          PLOTS, [stationData[sc].elevation - 0.5 * bracketSize, $
                  stationData[sc].elevation + 0.5 * bracketSize], $
                 [lowVal[sc], lowVal[sc]], $
                 COLOR = 100
      endfor

;+
;     Add mean SLR values.
;-
      OPLOT, stationData.elevation, 10.0^stationMeanVal, PSYM = 4

;+
;     Fit the trend.
;-
      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then STOP

      intSlp = LINFIT(stationData.elevation, stationMeanVal, $
                      MEASURE_ERRORS = stationSDVal, $
                      YFIT = stationMeanValFit, $
                      SIGMA = intSlpSigma)
      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          if (mathErrors ne 32) then STOP ; floating underflow is common here
      endif
      intSlp2 = LINFIT(stationData.elevation, stationMeanVal, $
                       YFIT = stationMeanValFit2)
      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          if (mathErrors ne 32) then STOP ; floating underflow is common here
      endif

      order = SORT(stationData.elevation)
      foo = stationData[order].elevation
      bar = stationMeanValFit[order]
      OPLOT, foo, 10.0^bar, COLOR = 4
      bar2 = stationMeanValFit2[order]
      OPLOT, foo, 10.0^bar2, COLOR = 10

      PRINT, 'intercept/slope (log space, with error bars): ', intSlp
      PRINT, 'intercept/slope (log space, without error bars): ', intSlp2
      PRINT, 'intercept/approx. slope (non-log space, with error bars): ' + $
             STRCRA(10.0^intSlp[0]) + ', ' + $
             STRCRA((10.0^bar[numStations - 1] - 10.0^bar[0]) / $
                    (foo[numStations - 1] - foo[0]) * 1000.0) + $
             ' km^-1'
      PRINT, 'intercept/approx. slope (non-log space, without error bars): ' + $
             STRCRA(10.0^intSlp2[0]) + ', ' + $
             STRCRA((10.0^bar2[numStations - 1] - 10.0^bar2[0]) / $
                    (foo[numStations - 1] - foo[0]) * 1000.0) + $
             ' km^-1'
      ;; PLOTS, [foo[0], foo[numStations - 1]], $
      ;;        [10.0^bar[0], 10.0^bar[numStations - 1]], color = 100

;+
;     Generate the background SLR based on the elevation trend.
;-
      ;; SLRElevTrendGrid = intSlp[0] + elevGrid * intSlp[1]
      ;; ind = WHERE(elevGrid eq elevHdr.no_data_value, count)
      ;; if (count gt 0) then SLRElevTrendGrid[ind] = ndvFLt

      ;; stationValResidual = stationMeanVal - stationMeanValFit

  endif else begin

;+
;     Set up the plot but with /NODATA.
;-
      PLOT, stationData.elevation, stationMeanVal, /NODATA, /YNOZERO, $
            ;; YRANGE = [MIN(stationMeanVal - stationSDVal), $
            ;;           MAX(stationMeanVal + stationSDVal)] > 0.0, $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    ClimName  + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'GHCN-D Station Elevation (meters)', $
            YTITLE = 'Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9], CHARSIZE = 1.5

      DRAW_ERROR_BARS, stationData.elevation, $
                       stationMeanVal, $
                       stationSDVal, $
                       COLOR = 100

;+
;     Add mean SLR values.
;-
      OPLOT, stationData.elevation, stationMeanVal, PSYM = 4

;+
;     Fit the trend.
;-
      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then STOP

      intSlp = LINFIT(stationData.elevation, stationMeanVal, $
                      MEASURE_ERRORS = stationSDVal, $
                      YFIT = stationMeanValFit, $
                      SIGMA = intSlpSigma)

      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          if (mathErrors ne 32) then STOP ; floating underflow is common here
      endif

;+
;     Perform LINFIT without MEASURE_ERRORS for comparison.
;-
      intSlp2 = LINFIT(stationData.elevation, stationMeanVal, $
                 YFIT = stationMeanValFit2)
      mathErrors = CHECK_MATH()
      if (mathErrors ne 0) then begin
          if (mathErrors ne 32) then STOP ; floating underflow is common here
      endif

      order = SORT(stationData.elevation)
      foo = stationData[order].elevation
      bar = stationMeanValFit[order]
      OPLOT, foo, bar, COLOR = 4
      bar2 = stationMeanValFit2[order]
      OPLOT, foo, bar2, COLOR = 10

      PRINT, 'intercept/slope (with error bars): ' + STRCRA(intSlp[0]) + $
             ', ' + STRCRA(1000.0 * intSlp[1]) + ' km^-1'
      PRINT, 'UNCERTAINTY IN intercept/slope (with error bars): ' + $
             STRCRA(intSlpSigma[0]) + ', ' + $
             STRCRA(1000.0 * intSlpSigma[1]) + ' km^-1'
      PRINT, 'intercept/slope (without error bars): ' + STRCRA(intSlp2[0]) + $
             ', ' + STRCRA(1000.0 * intSlp2[1]) + ' km^-1'

;; ;+
;; ;     Explicitly calculate R^2, for fun.
;; ;-
;;       devFromFitSq = (stationMeanVal - stationMeanValFit)^2.0
;;       devFromFitSq2 = (stationMeanVal - stationMeanValFit2)^2.0
;;       meanMeanSLR = MEAN(stationMeanVal)
;;       devFromMeanSq = (stationMeanVal - meanMeanSLR)^2.0

;;       RSquared = 1.0 - TOTAL(devFromFitSq) / TOTAL(devFromMeanSq)
;;       RSquared2 = 1.0 - TOTAL(devFromFitSq2) / TOTAL(devFromMeanSq)

;;       PRINT, 'Fitted R^2 (coefft. of det.; with error bars) = ' + $
;;              STRCRA(RSquared)
;;       PRINT, 'Fitted R^2 (coefft. of det.; without error bars) = ' + $
;;              STRCRA(RSquared2)
;;       PRINT, 'Pearson r^2 = ' + $
;;              STRCRA((CORRELATE(stationData.elevation, stationMeanVal))^2.0)

;; ;+
;; ;     Calculate Pearson r^2 manually.
;; ;-
;;       meanElev = MEAN(stationData.elevation)
;;       rPearson = TOTAL((stationData.elevation - meanElev) * $
;;                        (stationMeanVal - meanmeanSLR)) / $
;;                  (SQRT(TOTAL((stationData.elevation - meanElev)^2.0)) * $
;;                   SQRT(TOTAL((stationMeanVal - meanmeanSLR)^2.0)))
;;       PRINT, 'Manual Pearson r^2 = ' + STRCRA(rPearson^2.0)

  endelse

;+
; Calculate coefficient of determination for fits with and without
; error bars, as well as correlation.
;-
  devFromFitSq = (stationMeanVal - stationMeanValFit)^2.0
  devFromFitSq2 = (stationMeanVal - stationMeanValFit2)^2.0
  meanMean = MEAN(stationMeanVal)
  devFromMeanSq = (stationMeanVal - meanMean)^2.0

  RSquared = 1.0 - TOTAL(devFromFitSq) / TOTAL(devFromMeanSq)
  RSquared2 = 1.0 - TOTAL(devFromFitSq2) / TOTAL(devFromMeanSq)

  PRINT, 'Fitted R^2 (coefft. of det.; with error bars) = ' + $
         STRCRA(rSquared)
  PRINT, 'Fitted R^2 (coefft. of det.; without error bars) = ' + $
         STRCRA(rSquared2)
  PRINT, 'Pearson r^2 = ' + $
         STRCRA(CORRELATE(stationData.elevation, stationMeanVal)^2.0)

;+
; Calculate Pearson r^2 manually.
;-
  meanElev = MEAN(stationData.elevation)
  rPearson = TOTAL((stationData.elevation - meanElev) * $
                   (stationMeanVal - meanMean)) / $
             (SQRT(TOTAL((stationData.elevation - meanElev)^2.0)) * $
              SQRT(TOTAL((stationMeanVal - meanMean)^2.0)))
  PRINT, 'Manual Pearson r^2 = ' + STRCRA(rPearson^2.0)

;+
; Make a presentation-friendly plot of mean SLR vs. elevation.
;-
  oldDevice = !D.Name
  oldFont = !P.Font
  SET_PLOT, 'PS'
  if useLog10 then $
      plotFile = 'SLR_station_log_vs_elevation_' + ClimName + '_' + $
                 STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
                 STRING(finishYear, FORMAT = '(I4.4)') $
  else $
      plotFile = 'SLR_station_vs_elevation_' + ClimName + '_' + $
                 STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
                 STRING(finishYear, FORMAT = '(I4.4)')  
  DEVICE, FILE = plotFile + '.ps'
  !P.Font = 1 ; TrueType
  DEVICE, /COLOR, BITS = 8
  DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT

  if useLog10 then begin

      lowVal = 10.0^(stationMeanVal - stationSDVal)
      highVal = 10.0^(stationMeanVal + stationSDVal)

      PLOT, stationData.elevation, 10.0^stationMeanVal, /NODATA, /YNOZERO, $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    LongClimName  + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'GHCN-D Station Elevation (meters)', $
            YTITLE = 'Geometric Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9]

;+
;     Draw error bars.
;-
      bracketSize = 0.01 * (!X.CRange[1] - !X.CRange[0])

      for sc = 0, numStations - 1 do begin
          PLOTS, [stationData[sc].elevation, stationData[sc].elevation], $
                 [lowVal[sc], highVal[sc]], $
                 COLOR = 100
          PLOTS, [stationData[sc].elevation - 0.5 * bracketSize, $
                  stationData[sc].elevation + 0.5 * bracketSize], $
                 [highVal[sc], highVal[sc]], $
                 COLOR = 100
          PLOTS, [stationData[sc].elevation - 0.5 * bracketSize, $
                  stationData[sc].elevation + 0.5 * bracketSize], $
                 [lowVal[sc], lowVal[sc]], $
                 COLOR = 100
      endfor

      OPLOT, stationData.elevation, 10.0^stationMeanVal, PSYM = 4
      OPLOT, foo, 10.0^bar, THICK = 3

      x1 = !X.CRange[0] + 0.75 * (!X.CRange[1] - !X.CRange[0])
      y1 = !Y.CRange[0] + 0.15 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, $
              'Intercept: ' + STRCRA(STRING(intSlp[0], FORMAT='(F5.2)')) + $
              '!C !C' + $
              'Slope: ' + $
              STRCRA(STRING(1000.0 * intSlp[1], FORMAT='(F4.2)')) + $
              ' km!U-1!N', $
              CHARSIZE = 1.0

  endif else begin

      PLOT, stationData.elevation, stationMeanVal, /NODATA, /YNOZERO, $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    LongClimName  + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'GHCN-D Station Elevation (meters)', $
            YTITLE = 'Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9]

      DRAW_ERROR_BARS, stationData.elevation, $
                       stationMeanVal, $
                       stationSDVal, $
                       COLOR = 100

      OPLOT, stationData.elevation, stationMeanVal, PSYM = 4
      OPLOT, foo, bar, THICK = 3

      x1 = !X.CRange[0] + 0.65 * (!X.CRange[1] - !X.CRange[0])
      y1 = !Y.CRange[0] + 0.15 * (!Y.CRange[1] - !Y.CRange[0])
      charSize = 0.8
      XYOUTS, x1, y1, $
              'Correlation (r!U2!N): ' + $
              STRCRA(STRING(rPearson^2.0, FORMAT='(F5.2)')), $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, $
              'Intercept: ' + $
              STRCRA(STRING(intSlp[0], FORMAT='(F5.2)')), $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, $
              'Slope: ' + $
              STRCRA(STRING(1000.0 * intSlp[1], FORMAT='(F5.2)')) + $
              ' km!U-1!N', $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, $
              'Coefft. of Determination (R!U2!N): ' + $
              STRCRA(STRING(RSquared, FORMAT='(F5.2)')), $
              CHARSIZE = charSize

  endelse

  DEVICE, /CLOSE
  SET_PLOT, oldDevice
  !P.Font = oldFont
  cmd = 'gs -q -sDEVICE=png16m -dSAFER -dNOPAUSE -r600 ' + $
        '-sOutputFile=' + plotFile + '.png -I. -- ' + $
        plotFile + '.ps'
    SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -scale "30%x30%" ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -trim -border 4% -bordercolor white ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  FILE_DELETE, plotFile + '.ps'
  PRINT, 'Plotted elevation trend to ' + plotFile + '.png'

;+
; Generate the background log-SLR or SLR based on the elevation
; trend. Note that this is overwritten by the result from the
; elevation-zone based approach below. It is only kept here for
; sentimental reasons.
;-
  valElevTrendGrid = intSlp[0] + elevGrid * intSlp[1]
  ind = WHERE(elevGrid eq elevHdr.no_data_value, count)
  if (count gt 0) then valElevTrendGrid[ind] = ndvFlt

  stationValResidual = stationMeanVal - stationMeanValFit

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; The trouble with the above approach is that the unequal distribution
; of stations across the elevation range causes stations at higher
; elevations to be more or less ignored.
;
; Before implementing the solution (below), first generate a histogram
; of station density as a function of elevation.
;-

;+
; Find an elevation step size and count that we can work with.
; numElevZones = (elevation range) / elevStep
;-
  targetNumMeanElev = 400
  targetElevStep = 10.0
  if ((MIN(stationData.elevation) lt 10.0) and $
      (MIN(stationData.elevation) gt 0.0)) then $
          hxMin = 0.0 $
  else $
      hxMin = FLOAT(FLOOR(MIN(stationData.elevation)))
  hxMax = FLOAT(CEIL(MAX(stationData.elevation)))
  numElevZones = CEIL((hxMax - hxMin) / targetElevStep) < $
                 targetNumMeanElev
  elevStep = FLOAT(CEIL((hxMax - hxMin) / numElevZones))
  elevZoneCenter = hxMin + (FINDGEN(numElevZones) + 0.5) * elevStep
  elevHist = HISTOGRAM(stationData.elevation, $
                       MIN = hxMin, $
                       MAX = hxMin + numElevZones * elevStep, $
                       BINSIZE = elevStep)
  WSET_OR_WINDOW, 0
  PLOT, elevZoneCenter, elevHist[0:numElevZones - 1], PSYM = 10

;+
; Make a presentation-friendly plot of the histogram.
;-
  oldDevice = !D.Name
  oldFont = !P.Font
  SET_PLOT, 'PS'
  plotFile = 'SLR_station_elevation_histogram_' + ClimName + '_' + $
             STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
             STRING(finishYear, FORMAT = '(I4.4)')
  DEVICE, FILE = plotFile + '.ps'
  !P.Font = 1 ; TrueType
  DEVICE, /COLOR, BITS = 8
  DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT
  PLOT, elevZoneCenter, elevHist[0:numElevZones - 1], PSYM = 10, $
        TITLE = 'SLR Station Elevations for ' + $
             LongClimName  + ', ' + $
             STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
        XTITLE = 'Station Elevation (meters)', $
        YTITLE = 'Number of Stations', $
        POS = [0.1, 0.1, 0.95, 0.9]
  x1 = !X.CRange[0] + 0.65 * (!X.CRange[1] - !X.CRange[0])
  y1 = !Y.CRange[1] - 0.10 * (!Y.CRange[1] - !Y.CRange[0])
  charSize = 0.8
  XYOUTS, x1, y1, $
          'Number of Stations: ' + STRCRA(numStations), $
          CHARSIZE = charSize
  y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
  XYOUTS, x1, y1, $
          'Mean: ' + $
          STRCRA(ROUND(MEAN(stationData.elevation))) + ' m', $
          CHARSIZE = charSize
  y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
  XYOUTS, x1, y1, $
          'Median: ' + $
          STRCRA(ROUND(MEDIAN(stationData.elevation, /EVEN))) + ' m', $
          CHARSIZE = charSize
  y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
  hMax = MAX(elevHist, modeInd)
  XYOUTS, x1, y1, $
          'Mode: ' + $
          STRCRA(ROUND(elevZoneCenter[modeInd])) + ' m', $
          CHARSIZE = charSize
  DEVICE, /CLOSE
  SET_PLOT, oldDevice
  !P.Font = oldFont
  cmd = 'gs -q -sDEVICE=png16m -dSAFER -dNOPAUSE -r600 ' + $
        '-sOutputFile=' + plotFile + '.png -I. -- ' + $
        plotFile + '.ps'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -scale "30%x30%" ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -trim -border 4% -bordercolor white ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  FILE_DELETE, plotFile + '.ps'
  PRINT, 'Plotted histogram of elevations to ' + plotFile + '.png'

;+
; Next we mitigate the negative effects of unequal station
; distribution by elevation by setting up an equal distribution of
; values across the elevation range, each of which represents a local
; average over a small range of elevations, and fit the trend to that.
;-

;+
; Use the elevation zone definitions used for the histogram above.
;-
  SLRMean_mean = MAKE_ARRAY(numElevZones, VALUE = ndvFlt)
  SLRMean_SD = SLRMean_mean
  elevZoneMean = SLRMean_mean

;+
; Generate log(SLR) / SLR means for elevation zones.
;-
  numStationsUsed = 0L
  for ec = 0, numElevZones - 1 do begin
      lowElev = elevZoneCenter[ec] - 0.5 * elevStep
      highElev = elevZoneCenter[ec] + 0.5 * elevStep
      ind = WHERE((stationData.elevation ge lowElev) and $
                  (stationData.elevation lt highElev), $
                  count)
      if (count eq 0) then CONTINUE
      numStationsUsed = numStationsUsed + count
      SLRMean_mean[ec] = MEAN(stationMeanVal[ind])
      elevZoneMean[ec] = MEAN(stationData[ind].elevation)
                                ; variance of mean = sum of variances / n^2
      SLRMean_SD[ec] = SQRT(TOTAL(stationSDVal[ind]^2.0)) / count
  endfor

  ind = WHERE((elevZoneMean ne ndvFlt) and $
              (SLRMean_mean ne ndvFlt), count)
  if (count eq 0) then STOP
    elevZoneMean = elevZoneMean[ind]
  SLRMean_mean = SLRMean_mean[ind]
  SLRMean_SD = SLRMean_SD[ind]
  if (count ne numElevZones) then numElevZones = count

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Fit the trend.
;-
  intSlp = LINFIT(elevZoneMean, SLRMean_mean, $
                  MEASURE_ERRORS = SLRMean_SD, $
                  YFIT = SLRMean_mean_fit, $
                  SIGMA = SLRMean_intSlpSigma)

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      if (mathErrors ne 32) then STOP ; floating underflow is common here
  endif

  PRINT, 'Results by elevation zone:'
  WSET_OR_WINDOW, 2

  if useLog10 then begin

      lowVal = 10.0^(SLRMean_mean - SLRMean_SD)
      highVal = 10.0^(SLRMean_mean + SLRMean_SD)

      PLOT, elevZoneMean, 10.0^SLRMean_mean, /NODATA, /YNOZERO, $
            ;; YRANGE = [MIN(lowVal), MAX(highVal)] > 0.0, $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    LongClimName + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'Elev. Zone Mean GHCN-D Station Elevation (meters)', $
            YTITLE = 'Elev. Zone Geometric Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9], CHARSIZE = 1.5

;+
;     Draw error bars.
;-
      bracketSize = 0.01 * (!X.CRange[1] - !X.CRange[0])

      for ec = 0, numElevZones - 1 do begin
  ;        if (SLRMean_mean[ec] eq ndvFlt) then CONTINUE
          PLOTS, [elevZoneMean[ec], elevZoneMean[ec]], $
                 [lowVal[ec], highVal[ec]], $
                 COLOR = 100
          PLOTS, [elevZoneMean[ec] - 0.5 * bracketSize, $
                  elevZoneMean[ec] + 0.5 * bracketSize], $
                 [highVal[ec], highVal[ec]], $
                 COLOR = 100
          PLOTS, [elevZoneMean[ec] - 0.5 * bracketSize, $
                  elevZoneMean[ec] + 0.5 * bracketSize], $
                 [lowVal[ec], lowVal[ec]], $
                 COLOR = 100
      endfor

;      ind = WHERE(SLRMean_mean ne ndvFlt, count)
;      if (count eq 0) then STOP
      OPLOT, elevZoneMean, 10.0^SLRMean_mean, PSYM = 4
      OPLOT, elevZoneMean, 10.0^SLRMean_mean_fit, COLOR = 4

      PRINT, 'intercept/slope (log space): ', intSlp
      PRINT, 'intercept/approx. slope (non-log space): ' + $
             STRCRA(10.0^intSlp[0]) + ', ' + $
             STRCRA((10.0^SLRMean_mean_fit[numElevZones - 1] - $
                     10.0^SLRMean_mean_fit[0]) / $
                    (elevZoneMean[numElevZones - 1] - $
                     elevZoneMean[0]) * 1000.0) + $
             ' km^-1'

      ;; devFromFitSq = (SLRMean_mean - SLRMean_mean_fit)^2.0
      ;; meanMeanMeanSLR = MEAN(SLRMean_mean)

      ;; SLRElevTrendGrid = intSlp[0] + elevGrid * intSlp[1]
      ;; ind = WHERE(elevGrid eq elevHdr.no_data_value, count)
      ;; if (count gt 0) then SLRElevTrendGrid[ind] = ndvFlt

  endif else begin

      PLOT, elevZoneMean, SLRMean_mean, /NODATA, /YNOZERO, $
            ;; YRANGE = [MIN(SLRMean_mean - SLRMean_SD), $
            ;;           MAX(SLRMean_mean + SLRMean_SD)], $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    LongClimName + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'Elev. Zone Mean GHCN-D Station Elevation (meters)', $
            YTITLE = 'Elev. Zone Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9], CHARSIZE = 1.5
      ;; print, 'a'
      ;; move = get_kbrd(1)
      DRAW_ERROR_BARS, elevZoneMean, $
                       SLRMean_mean, $
                       SLRMean_SD, $
                       COLOR = 100
      ;; print, 'b'
      ;; move = get_kbrd(1)

;+
;     Add mean SLR values.
;-
      ;; ind = WHERE(SLRMean_mean ne ndvFlt, count)
      ;; if (count eq 0) then STOP
      OPLOT, elevZoneMean, SLRMean_mean, PSYM = 4
      ;; print, 'c'
      ;; move = get_kbrd(1)

      OPLOT, elevZoneMean, SLRMean_mean_fit, COLOR = 4
      ;; print, 'd'
      ;; move = get_kbrd(1)

      PRINT, 'intercept/slope (with error bars): ' + STRCRA(intSlp[0]) + $
             ', ' + STRCRA(1000.0 * intSlp[1]) + ' km^-1'
      PRINT, 'UNCERTAINTY IN intercept/slope (with error bars): ' + $
             STRCRA(SLRMean_intSlpSigma[0]) + ', ' + $
             STRCRA(1000.0 * SLRMean_intSlpSigma[1]) + ' km^-1'

  endelse

  devFromFitSq = (SLRMean_mean - SLRMean_mean_fit)^2.0
  meanMeanMeanSLR = MEAN(SLRMean_mean)
  devFromMeanMeanMeanSq = (SLRMean_mean - meanMeanMeanSLR)^2.0

  RSquared = 1.0 - TOTAL(devFromFitSq) / TOTAL(devFromMeanMeanMeanSq)
  rPearson = CORRELATE(elevZoneMean, SLRMean_mean)

  PRINT, 'Fitted R^2 (coefft. of det.) = ' + STRCRA(RSquared)
  PRINT, 'Pearson r^2 = ' + STRCRA(rPearson^2.0)

;+
; Make a presentation-friendly plot of the elevation trend with the
; fit.  
;-
  oldDevice = !D.Name
  oldFont = !P.Font
  SET_PLOT, 'PS'
  if useLog10 then $
      plotFile = 'SLR_log_vs_elevation_' + ClimName + '_' + $
                 STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
                 STRING(finishYear, FORMAT = '(I4.4)') $
  else $
      plotFile = 'SLR_vs_elevation_' + ClimName + '_' + $
                 STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
                 STRING(finishYear, FORMAT = '(I4.4)')
  DEVICE, FILE = plotFile + '.ps'
  !P.Font = 1 ; TrueType
  DEVICE, /COLOR, BITS = 8
  DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT
  
  if useLog10 then begin

      PLOT, elevZoneMean, 10.0^SLRMean_mean, /NODATA, /YNOZERO, $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    LongClimName + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'Elev. Zone Mean GHCN-D Station Elevation (meters)', $
            YTITLE = 'Elev. Zone Geometric Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9]

      bracketSize = 0.01 * (!X.CRange[1] - !X.CRange[0])

      for ec = 0, numElevZones - 1 do begin
          PLOTS, [elevZoneMean[ec], elevZoneMean[ec]], $
                 [lowVal[ec], highVal[ec]], $
                 COLOR = 100
          PLOTS, [elevZoneMean[ec] - 0.5 * bracketSize, $
                  elevZoneMean[ec] + 0.5 * bracketSize], $
                 [highVal[ec], highVal[ec]], $
                 COLOR = 100
          PLOTS, [elevZoneMean[ec] - 0.5 * bracketSize, $
                  elevZoneMean[ec] + 0.5 * bracketSize], $
                 [lowVal[ec], lowVal[ec]], $
                 COLOR = 100
      endfor

      OPLOT, elevZoneMean, 10.0^SLRMean_mean, PSYM = 4
      OPLOT, elevZoneMean, 10.0^SLRMean_mean_fit, COLOR = 100

  endif else begin

      PLOT, elevZoneMean, SLRMean_mean, /NODATA, /YNOZERO, $
            YRANGE = [0.0, 30.0], $
            TITLE = 'SLR vs. Elevation for ' + $
                    LongClimName + ', ' + $
                    STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
            XTITLE = 'Elev. Zone Mean GHCN-D Station Elevation (meters)', $
            YTITLE = 'Elev. Zone Mean Climatological SLR', $
            POS = [0.1, 0.1, 0.95, 0.9]

      DRAW_ERROR_BARS, elevZoneMean, $
                       SLRMean_mean, $
                       SLRMean_SD, $
                       COLOR = 100

      OPLOT, elevZoneMean, SLRMean_mean, PSYM = 4
      OPLOT, elevZoneMean, SLRMean_mean_fit, THICK = 3

      x1 = !X.CRange[0] + 0.65 * (!X.CRange[1] - !X.CRange[0])
      y1 = !Y.CRange[0] + 0.15 * (!Y.CRange[1] - !Y.CRange[0])
      charSize = 0.8
      XYOUTS, x1, y1, $
              'Correlation (r!U2!N): ' + $
              STRCRA(STRING(rPearson^2.0, FORMAT='(F5.2)')), $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, $
              'Intercept: ' + $
              STRCRA(STRING(intSlp[0], FORMAT='(F5.2)')), $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, $
              'Slope: ' + $
              STRCRA(STRING(1000.0 * intSlp[1], FORMAT='(F5.2)')) + $
              ' km!U-1!N', $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, $
              'Coefft. of Determination (R!U2!N): ' + $
              STRCRA(STRING(RSquared, FORMAT='(F5.2)')), $
              CHARSIZE = charSize

  endelse

  DEVICE, /CLOSE
  SET_PLOT, oldDevice
  !P.Font = oldFont
  cmd = 'gs -q -sDEVICE=png16m -dSAFER -dNOPAUSE -r600 ' + $
        '-sOutputFile=' + plotFile + '.png -I. -- ' + $
        plotFile + '.ps'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -scale "30%x30%" ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -trim -border 4% -bordercolor white ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  FILE_DELETE, plotFile + '.ps'
  PRINT, 'Plotted elevation trend for elevation zones to ' + plotFile + '.png'

;+
; Recalculate differences between the fitted trend and the individual
; station averages.
;-
  stationMeanValFit = intSlp[0] + stationData.elevation * intSlp[1]
  stationValResidual = stationMeanVal - stationMeanValFit

;+
; Generate the background log-SLR or SLR based on the elevation
; trend.
;-
  valElevTrendGrid = intSlp[0] + elevGrid * intSlp[1]
  ind = WHERE(elevGrid eq elevHdr.no_data_value, count)
  if (count gt 0) then begin
      MESSAGE, 'WARNING: elevation-based SLR baseline has ' + $
               STRCRA(count) + ' no-data values.', /CONTINUE
      valElevTrendGrid[ind] = ndvFlt
  endif

  stationValResidual = stationMeanVal - stationMeanValFit

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Display the background SLR based on the elevation trend.
;-
  red = [200, 000, 000, 000, 000, 000, 127, 240, 204, 255, 255, 255, 237, $
         238, 204]
  grn = [255, 255, 240, 180, 140, 204, 255, 240, 127, 125, 175, 105, 064, $
         045, 000]
  blu = [255, 255, 240, 240, 000, 000, 000, 000, 000, 000, 185, 105, 000, $
         045, 000]
  binEdges = [6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, $
              17.0, 18.0, 19.0, 20.0, 21.0]
 
  vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector'

  if (useLog10) then begin
      killme = 10.0^valElevTrendGrid
      ind = WHERE(valElevTrendGrid eq ndvFlt, count)
      if (count gt 0) then killme[ind] = ndvFlt
  endif else begin
      killme = valElevTrendGrid
  endelse

  X_MAP_GEO_GRID, killme, $
                  minLonOut, maxLonOut, minLatOut, maxLatOut, $
                  binEdges, red, grn, blu, $
                  0, $
                  status, $
                  XSIZE = 800, YSIZE = 800, $ ; just suggestions
                  NDV = ndvFlt, $
                  /SHOW_LOW, $
                  /SHOW_HIGH, $
                  TITLE = 'SLR Background for ' + LongClimName  + ', ' + $
                  STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
                  /COLORBAR, $
                  UNITS = 'Dimensionless', $
                  SHAPE_PATH_LIST = [vectorDir + '/' + $
                                     'National_Boundary_Canada', $
                                     vectorDir + '/' + $
                                     'Provincial_Boundaries', $
                                     vectorDir + '/' + $
                                     'National_Boundary_Mexico_2004', $
                                     vectorDir + '/' + $
                                     'National_Boundary_Coterminous_US', $
                                     vectorDir + '/' + $
                                     'State_Boundaries_Coterminous_US']

;+
; Generate a PNG map of the background.
;-
  PNGImage = 'GHCND_SLR_clim_background_' + $
             domainLabel + '_' + $
             ClimName + '_' + $
             STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
             STRING(finishYear, FORMAT = '(I4.4)') + '.png'

  MAKE_LON_LAT_MAP_PNG, killme, ndvFlt, $
                        binEdges, red, grn, blu, $
                        xResOut, minLonOut, maxLonOut, $
                        yResOut, minLatOut, maxLatOut, $
                        'SLR Background for ' + LongClimName  + ', ' + $
                        STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
                        'Dimensionless', $
                        PNGImage, $
                        /SHOW_LOW, $
                        /SHOW_HIGH, $
                        MAP_SHAPE_PATH = [vectorDir + '/' + $
                                          'National_Boundary_Canada', $
                                          vectorDir + '/' + $
                                          'Provincial_Boundaries', $
                                          vectorDir + '/' + $
                                          'National_Boundary_Mexico_2004', $
                                          vectorDir + '/' + $
                                          'National_Boundary_Coterminous_US', $
                                          vectorDir + '/' + $
                                          'State_Boundaries_Coterminous_US'], $
                        /NO_GRID, $
                        /NO_CONTINENTS, $
                        /NO_USA, $
                        /BLACK_ON_WHITE

  PRINT, 'SLR background displayed in ' + PNGImage

  killme = !NULL

;+
; Generate the semivariogram of log-SLR or SLR residuals.
;-
  PRINT, 'Generating empirical semivariogram of SLR - elevation trend.'

  PRINT, 'Maximum lag: ' + STRCRA(maxLagMeters / 1000.0) + ' km'
  PRINT, 'Lag tolerance: ' + STRCRA(lagTolerance) + ' m'

  WSET_OR_WINDOW, 3

  paramsSpher = !NULL

  if useLog10 then $
      quantStr = 'log(SLR)' $
  else $
      quantStr = 'SLR'

  plotTitle = 'Empirical Variogram of ' + quantStr + ' (detrended) for ' + $
              LongClimName + ', ' + STRING(startYear, FORMAT = '(I4.4)') + $
              ' - ' + STRING(finishYear, FORMAT = '(I4.4)')

  GEO_SEMIVARIOGRAM, stationData.longitude, $
                     stationData.latitude, $
                     stationValResidual, $
                     maxLagMeters, $
                     status, $
                     DISTANCE_PRECISION = dPrecision, $
                     LAG_TOLERANCE = lagTolerance, $
                     SPHERICAL_SEMIVARIOGRAM_PARAMS = paramsSpher, $
                     SPHERICAL_SEMIVARIOGRAM_RMSE = RMSESpher, $
                     LAG_OUT = lagMeters, $
                     SEMIVARIOGRAM_OUT = eVario, $
                     PLOT_TITLE = plotTitle, $
;                     /SHOW_SD_RANGE, $
;                     /SHORT_LAG_WEIGHT_BIAS, $
;                     /SCALE_INPUT_VARIABLE, $
                     /HASH, $
                     /SHOW_PLOT, $
                     /VERBOSE

  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then STOP

;+
; Make a presentation-friendly plot of the empirical semivariance with
; the fit and indicators for sill and range.
;-
  oldDevice = !D.Name
  oldFont = !P.Font
  SET_PLOT, 'PS'
  if useLog10 then $
      plotFile = 'SLR_log_residual_variogram_' + ClimName + '_' + $
                 STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
                 STRING(finishYear, FORMAT = '(I4.4)') $
  else $
      plotFile = 'SLR_residual_variogram_' + ClimName + '_' + $
                 STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
                 STRING(finishYear, FORMAT = '(I4.4)')
  DEVICE, FILE = plotFile + '.ps'
  !P.Font = 1 ; TrueType
  DEVICE, /COLOR, BITS = 8
  DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT
  PLOT, lagMeters / 1000.0, eVario, $
        TITLE = plotTitle, $
        XTITLE = 'Separation (km)', $
        YTITLE = 'Variogram', $
        PSYM = 10
  SPHERICAL_SEMIVARIOGRAM, lagMeters, paramsSpher, fitVario
  OPLOT, lagMeters / 1000, fitVario, COLOR = 100
  OPLOT, [paramsSpher[2], paramsSpher[2]] / 1000.0, $
         [!Y.CRange[0], paramsSpher[1]], $
         COLOR = [0], LINESTYLE = 2
  OPLOT, [0.0, paramsSpher[2]] / 1000.0, $
         [paramsSpher[1], paramsSpher[1]], $
         COLOR = [0], LINESTYLE = 2
  XYOUTS, !X.CRange[0] + 0.025 * (!X.CRange[1] - !X.CRange[0]), $
          paramsSpher[0] - 0.01 * (!Y.CRange[1] - !Y.CRange[0]), $
          'Nugget: ' + FORMAT_FLOAT(paramsSpher[0]), $
          CHARSIZE = 1.0
  XYOUTS, !X.CRange[0] + 0.025 * (!X.CRange[1] - !X.CRange[0]), $
          paramsSpher[1] + 0.01 * (!Y.CRange[1] - !Y.CRange[0]), $
          'Sill: ' + FORMAT_FLOAT(paramsSpher[1]), $
          CHARSIZE = 1.0
  XYOUTS, paramsSpher[2] / 1000 - 0.01 * (!X.CRange[1] - !X.CRange[0]), $
          0.5 * paramsSpher[1], $
          'Range: ' + FORMAT_FLOAT(paramsSpher[2] / 1000) + ' km', $
          CHARSIZE = 1.0, $
          ALIGNMENT = 0.5, ORIENTATION = 90
  DEVICE, /CLOSE
  SET_PLOT, oldDevice
  !P.Font = oldFont
  cmd = 'gs -q -sDEVICE=png16m -dSAFER -dNOPAUSE -r600 ' + $
        '-sOutputFile=' + plotFile + '.png -I. -- ' + $
        plotFile + '.ps'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -scale "30%x30%" ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  cmd = 'mogrify -trim -border 4% -bordercolor white ' + plotFile + '.png'
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then STOP
  FILE_DELETE, plotFile + '.ps'
  PRINT, 'Plotted semivariogram to ' + plotFile + '.png'

  nuggetSpher = paramsSpher[0]
  sillSpher = paramsSpher[1]
  rangeSpher = paramsSpher[2]

  PRINT, 'Spherical variogram results:'
  PRINT, 'Nugget: ', paramsSpher[0]
  PRINT, 'Sill: ', paramsSpher[1]
  PRINT, 'Range: ' , paramsSpher[2]
  PRINT, 'RMSE: ', RMSESpher

;RETURN

;+
; Calculate RMSE for the search neighborhood defined by
; QCHoodSearchRadiusMeters.
;-
  ind = WHERE(lagMeters lt QCHoodSearchRadiusMeters, count)
  if (count lt 2) then STOP

  xEmp = lagMeters[ind]
  yEmp = eVario[ind]

  ;; EXPONENTIAL_SEMIVARIOGRAM, xEmp, paramsExpo, yExpo
  ;; if (N_ELEMENTS(yExpo) ne count) then STOP
  ;; PRINT, 'Exponential variogram RMSE for lags < ' + $
  ;;        STRCRA(QCHoodSearchRadiusMeters) + ' m: ' + $
  ;;        STRCRA(SQRT(TOTAL((yExpo - yEmp)^2.0) / count))

  SPHERICAL_SEMIVARIOGRAM, xEmp, paramsSpher, ySpher
  if (N_ELEMENTS(ySpher) ne count) then STOP
  PRINT, 'Spherical variogram RMSE for lags < ' + $
         STRCRA(QCHoodSearchRadiusMeters) + ' m: ' + $
         STRCRA(SQRT(TOTAL((ySpher - yEmp)^2.0) / count))

  ;; WSET_OR_WINDOW, 2
  ;; PLOT, xEmp / 1000.0, yEmp, PSYM = 10, $
  ;;       XTITLE = 'Lag (km)', $
  ;;       YTITLE = 'Variogram'
  ;; OPLOT, xEmp / 1000.0, ySpher, COLOR = 100
  ;; XYOUTS, xEmp[count - 1] / 1000.0, ySpher[count - 1], 'S'

  ind = !NULL

  ind = WHERE(lagMeters lt paramsSpher[2], count)
  if (count lt 2) then STOP

  xEmp = lagMeters[ind]
  yEmp = eVario[ind]

  SPHERICAL_SEMIVARIOGRAM, xEmp, paramsSpher, ySpher
  if (N_ELEMENTS(ySpher) ne count) then STOP
  PRINT, 'Spherical variogram RMSE for lags < range (' + $
         STRCRA(paramsSpher[2] / 1000.0) + ' km): ' + $
         STRCRA(SQRT(TOTAL((ySpher - yEmp)^2.0) / count))


  ;; WSET_OR_WINDOW, 2
  ;; PLOT, xEmp / 1000.0, yEmp, PSYM = 10, $
  ;;       XTITLE = 'Lag (km)', $
  ;;       YTITLE = 'Variogram', $
  ;;       TITLE = plotTitle
;, $
;        YRANGE = [0.5, 1.0]
;  OPLOT, xEmp / 1000.0, yExpo, COLOR = 100
;  XYOUTS, xEmp[count - 1] / 1000.0, yExpo[count - 1], 'E'
  ;; OPLOT, xEmp / 1000.0, ySpher, COLOR = 100
  ;; XYOUTS, xEmp[count - 1] / 1000.0, ySpher[count - 1], 'S'

  ind = !NULL

;+
; Find the nearest neighbor to each SLR average and look at statistics
; of that result. These numbers should help decide on things like
; resolution for the kriging process.
;-
  PRINT, 'Finding nearest neighboring pairs and distances.'

  FIND_NEAREST_NEIGHBOR, stationData.longitude, $
                         stationData.latitude, $
                         nnInd, nnDist, $
                         DISTANCE_PRECISION = dPrecision, $
                         /HASH

  nnNumLags = CEIL(MAX(nnDist) / lagTolerance)
  nnLag = (0.5D + DINDGEN(nnNumLags)) * lagTolerance

  nnHist = HISTOGRAM(nnDist, $
                     MIN = 0.0D, MAX = nnNumLags * lagTolerance, $
                     BINSIZE = lagTolerance)

  if (nnHist[nnNumLags] ne 0) then STOP

  ;; WSET_OR_WINDOW, 2

  ;; PLOT, nnLag / 1000.0, nnHist[0:nnNumLags - 1], $
  ;;       TITLE = 'Nearest Neighbor Distance Histogram', $
  ;;       XTITLE = 'Distance (km)', $
  ;;       PSYM = 10

  nnCDF = FLTARR(nnNumLags)
  for lc = 0, nnNumLags - 1 do nnCDF[lc] = TOTAL(nnHist[0:lc]) / TOTAL(nnHist)
  ;; OPLOT, nnLag / 1000.0, nnCDF * !Y.CRange[1], COLOR = 100

;+
; Calculate median distance.
;-
  i1 = MAX(WHERE(nnCDF le 0.5))
  i2 = MIN(WHERE(nnCDF gt 0.5))
  if (i2 eq i1) then STOP
  nnMedian = nnLag[i1] $
             + (nnLag[i2] - nnLag[i1]) $
             / (nnCDF[i2] - nnCDF[i1]) $
             * (0.5 - nnCDF[i1])

  ;; PLOTS, [nnMedian / 1000.0, nnMedian / 1000.0], $
  ;;        [0, 0.5 * !Y.CRange[1]], COLOR = 100

  PRINT, 'Median distance to nearest neighbor is ' + $
         STRCRA(nnMedian) + ' meters'

  if (prepareOnly) then STOP

;+
; Run the kriging procedure.
;-
  GEO_KRIGE_WITH_SPHERICAL_SEMIVARIANCE, $
      stationData.longitude, $
      stationData.latitude, $
      stationValResidual, $
      ndvFlt, $
      paramsSpher, $
      minLonOut, $
      maxLonOut, $
      minLatOut, $
      maxLatOut, $
      xResOut, $
      yResOut, $
      valResidualGrid, $
      DISTANCE_PRECISION = dPrecision, $
      CROSS_VAL_POINTS = cvValResidual, $
      CROSS_VAL_RMSE = cvValResidualRMSE, $
      /VERBOSE

;+
; Remove elevation trend to get straight SLR results (gridded values
; and cross-validated point values).
;-
  valGrid = valElevTrendGrid + valResidualGrid 
  cvValEst = stationMeanValFit + cvValResidual

;+
; Take the SLR data out of log space if necessary, and calculate
; point values and cross-validated SLR estimates and corresponding
; errors.
;
;   stationMeanSLR - average observed SLR for climatological period
;   stationSDSLR - standard deviation in SLR for climatological period
;   stationQ25SLR - 25% quantile SLR for climatological period
;   stationQ50SLR - 50% quantile SLR for climatological period
;                   (median)
;   stationQ75SLR - 75% quantile SLR for climatological period
;   stationElevSL - fitted SLR value from elevation trend
;   stationValResidual - SLR after elevation trend removal
;   stationCVSLREst - SLR cross validation estimate
;   stationCVSLRErr - Error on SLR cross validation estimate
;
; Start with a programming check. There should not be ANY no-data
; values in stationMeanVal, stationSDVal, stationQ25Val,
; stationQ50Val, stationQ75Val, stationMeanValFit, or stationValResidual.
;+
  ind = WHERE(stationMeanVal eq ndvFlt, count)
  if (count ne 0) then STOP
  ind = WHERE(stationSDVal eq ndvFlt, count)
  if (count ne 0) then STOP
  ind = WHERE(stationQ25Val eq ndvFlt, count)
  if (count ne 0) then STOP
  ind = WHERE(stationQ50Val eq ndvFlt, count)
  if (count ne 0) then STOP
  ind = WHERE(stationQ75Val eq ndvFlt, count)
  if (count ne 0) then STOP
  ind = WHERE(stationMeanValFit eq ndvFlt, count)
  if (count ne 0) then STOP
  ind = WHERE(stationValResidual eq ndvFlt, count)
  if (count ne 0) then STOP

  if (useLog10) then begin

      valGrid = 10.0^valGrid

      stationMeanVal = 10.0^stationMeanVal
      stationSDVal = 0.5 * (10.0^(stationMeanVal + stationSDVal) + $
                            10.0^(stationMeanVal - stationSDVal))
      stationQ25Val = 10.0^stationQ25Val
      stationQ50Val = 10.0^stationQ50Val
      stationQ75Val = 10.0^stationQ75Val
      stationMeanValFit = 10.0^stationMeanValFit
      cvValEst = 10.0^cvValEst

  endif

  cvSLRErr = cvValEst - stationMeanVal

;+
; Locate no-data values in the cross-validated results (there will be,
; in general, for points outside the output domain that contributed to
; the analysis and points flagged as outliers in the kriging process).
;-
  pInd = WHERE((cvValResidual eq ndvFlt), pCount)

  if (pCount gt 0) then begin
      cvValEst[pInd] = ndvFlt
      cvSLRErr[pInd] = ndvFlt
  endif
  PRINT, 'Cross validation results were returned for ' + $
         STRCRA(numStations - pCount) + ' locations'

;+
; Locate no-data values in the output valGrid.
;-
  gInd = WHERE((valElevTrendGrid eq ndvFlt) or $
               (valResidualGrid eq ndvFlt), gCount)
  if (gCount gt 0) then begin
      MESSAGE, 'WARNING: output grid has ' + STRCRA(gCount) + $
               ' no-data values.', /CONTINUE
      valGrid[gInd] = ndvFlt
  endif

;+
; Summarize results.
;-
  ind = WHERE(valGrid ne ndvFlt, count)
  if (count eq 0) then STOP

  PRINT, 'Gridded SLR results:'
  PRINT, 'Mean SLR: ' + STRCRA(MEAN(valGrid[ind]))
  PRINT, 'SLR std. dev.: ' + STRCRA(STDDEV(valGrid[ind]))

  ind = WHERE((cvValEst ne ndvFlt) and (cvSLRErr ne ndvFlt), count)
  if (count eq 0) then STOP

  cvSLRMean = MEAN(cvValEst[ind])
  cvSLRRMSE = SQRT(TOTAL(cvSLRErr[ind]^2.0) / count)

  PRINT, 'Cross-validated kriging results:'
  PRINT, 'Mean SLR: ' + STRCRA(cvSLRMean)
  PRINT, 'Estimated RMSE: ' + STRCRA(cvSLRRMSE)


;----------------;
; KRIGING FINISH ;
;----------------;

;+
; Display the output grid.
;-
  red = [200, 000, 000, 000, 000, 000, 127, 240, 204, 255, 255, 255, 237, $
         238, 204]
  grn = [255, 255, 240, 180, 140, 204, 255, 240, 127, 125, 175, 105, 064, $
         045, 000]
  blu = [255, 255, 240, 240, 000, 000, 000, 000, 000, 000, 185, 105, 000, $
         045, 000]
  binEdges = [6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, $
              17.0, 18.0, 19.0, 20.0, 21.0]
 
  vectorDir = '/nwcdev/nwc_forcings/GIS_data/Vector'
  vectorDir = '../../../resources/shapefiles'

  X_MAP_GEO_GRID, valGrid, $
                  minLonOut, maxLonOut, minLatOut, maxLatOut, $
                  binEdges, red, grn, blu, $
                  1, $
                  status, $
                  XSIZE = 800, YSIZE = 800, $ ; just suggestions
                  NDV = ndvFlt, $
                  /SHOW_LOW, $
                  /SHOW_HIGH, $
                  TITLE = 'SLR for ' + LongClimName  + ', ' + $
                  STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
                  /COLORBAR, $
                  UNITS = 'Dimensionless', $
                  SHAPE_PATH_LIST = [vectorDir + '/' + $
                                     'National_Boundary_Canada', $
                                     vectorDir + '/' + $
                                     'Provincial_Boundaries', $
                                     vectorDir + '/' + $
                                     'National_Boundary_Mexico_2004', $
                                     vectorDir + '/' + $
                                     'National_Boundary_Coterminous_US', $
                                     vectorDir + '/' + $
                                     'State_Boundaries_Coterminous_US']

;+
; Generate a PNG map of the SLR climatology result.
;-
  PNGImage = 'GHCND_SLR_clim_' + $
             domainLabel + '_' + $
             ClimName + '_' + $
             STRING(StartYear, FORMAT = '(I4.4)') + '_to_' + $
             STRING(finishYear, FORMAT = '(I4.4)') + '.png'

  MAKE_LON_LAT_MAP_PNG, valGrid, ndvFlt, $
                        binEdges, red, grn, blu, $
                        xResOut, minLonOut, maxLonOut, $
                        yResOut, minLatOut, maxLatOut, $
                        'SLR for ' + LongClimName  + ', ' + $
                        STRCRA(startYear) + ' - ' + STRCRA(finishYear), $
                        'Dimensionless', $
                        PNGImage, $
                        /SHOW_LOW, $
                        /SHOW_HIGH, $
                        MAP_SHAPE_PATH = [vectorDir + '/' + $
                                          'National_Boundary_Canada', $
                                          vectorDir + '/' + $
                                          'Provincial_Boundaries', $
                                          vectorDir + '/' + $
                                          'National_Boundary_Mexico_2004', $
                                          vectorDir + '/' + $
                                          'National_Boundary_Coterminous_US', $
                                          vectorDir + '/' + $
                                          'State_Boundaries_Coterminous_US'], $
                        /NO_GRID, $
                        /NO_CONTINENTS, $
                        /NO_USA, $
                        /BLACK_ON_WHITE

  PRINT, 'SLR climatology displayed in ' + PNGImage

;+
; Write station results to a CSV file.
;-
  climDatCSVFile = 'GHCND_SLR_clim_points_' + $
                   domainLabel + '_' + $
                   ClimName + '_' + $
                   STRING(startYear, FORMAT = '(I4.4)') + '_to_' + $
                   STRING(finishYear, FORMAT = '(I4.4)') + '.csv'


  OPENW, lun, climDatCSVFile, /GET_LUN

  PRINTF, lun, 'longitude,latitude,elevation,id,slr_mean,slr_sd,slr_q25,slr_q50,slr_q75,slr_elev_trend_fit,slr_cross_val_est,slr_cross_val_err'

  for sc = 0, numStations - 1 do begin

      PRINTF, lun, $      
              STRCRA(STRING(stationData[sc].longitude, $
                            FORMAT = '(f11.6)')) + ',' + $
              STRCRA(STRING(stationData[sc].latitude, $
                            FORMAT = '(f10.6)')) + ',' + $
              STRCRA(STRING(stationData[sc].elevation, $
                            FORMAT = '(f9.2)')) + ',' + $
              stationData[sc].id + ',' + $
              STRCRA(stationMeanVal[sc]) + ',' + $
              STRCRA(stationSDVal[sc]) + ',' + $
              STRCRA(stationQ25Val[sc]) + ',' + $
              STRCRA(stationQ50Val[sc]) + ',' + $
              STRCRA(stationQ75Val[sc]) + ',' + $
              STRCRA(stationMeanValFit[sc]) + ',' + $
              STRCRA(cvValEst[sc]) + ',' + $
              STRCRA(cvSLRErr[sc])
  endfor

  FREE_LUN, lun

;+
; Write results to a GeoTIFF.
;-
  climTIFFile = 'GHCND_SLR_clim_' + $
                domainLabel + '_' + $
                LongClimName + '_' + $
                STRING(startYear, FORMAT = '(I4.4)') + '_to_' + $
                STRING(finishYear, FORMAT = '(I4.4)') + '_' + $
                STRCRA(ROUND(resOutArcSec)) + $
                '_arc_sec.tif'

  MAKE_GEOTIFF_FROM_GRID, ROTATE(valGrid, 7), $
                          minLonOut, $
                          maxLatOut, $
                          xResOut, $
                          yResOut, $
                          climTIFFile, $
                          COMPRESS = 1 ; 1 = LZW, 2 = PackBits, 3 = JPEG

;+
; Save data.
;-
  climDatSavFile = 'GHCND_SLR_clim_data_' + $
                   domainLabel + '_' + $
                   ClimName + '_' + $
                   STRING(startYear, FORMAT = '(I4.4)') + '_to_' + $
                   STRING(finishYear, FORMAT = '(I4.4)') + '_' + $
                   STRCRA(ROUND(resOutArcSec)) + $
                   '_arc_sec.sav'

  SAVE, /ALL, FILE = climDatSavFile

;+
; Write results to NetCDF for conversion to a GISRS layer. This will
; be the most authoritative version of the results.
;-
  climNetCDFFile = 'GHCND_SLR_clim_' + $
                   domainLabel + '_' + $
                   LongClimName + '_' + $
                   STRING(startYear, FORMAT = '(I4.4)') + '_to_' + $
                   STRING(finishYear, FORMAT = '(I4.4)') + '_' + $
                   STRCRA(ROUND(resOutArcSec)) + $
                   '_arc_sec.nc'

  id = NCDF_CREATE(climNetCDFFile, /NETCDF4_FORMAT, /CLOBBER)

  NCDF_CONTROL, id, /VERBOSE

  dimID_lat = NCDF_DIMDEF(id, 'lat', nyOut)
  dimID_lon = NCDF_DIMDEF(id, 'lon', nxOut)
  dimID_nv = NCDF_DIMDEF(id, 'nv', 2)

  varID_lat = NCDF_VARDEF(id, 'lat', [dimID_lat], /DOUBLE)
  varID_latBounds = NCDF_VARDEF(id, 'lat_bounds', [dimID_nv, dimID_lat], $
                                /DOUBLE)
  varID_lon = NCDF_VARDEF(id, 'lon', [dimID_lon], /DOUBLE)
  varID_lonBounds = NCDF_VARDEF(id, 'lon_bounds', [dimID_nv, dimID_lon], $
                                /DOUBLE )

  varID_crs = NCDF_VARDEF(id, 'crs', /SHORT) ; coord. ref. system container

  varID_data = NCDF_VARDEF(id, 'Data', [dimID_lon, dimID_lat], /FLOAT, $
                           GZIP = 2, $
                           /SHUFFLE, $
                           CHUNK_DIMENSIONS = [nxOut, 1])

  NCDF_ATTPUT, id, /GLOBAL, 'format_version', 'NOHRSC NetCDF raster file v1.2'
  NCDF_ATTPUT, id, /GLOBAL, 'Conventions', 'CF-1.6'
  NCDF_ATTPUT, id, /GLOBAL, 'title', $
               'This snow-to-liquid ratio (SLR) climatology for ' + $
               LongClimName + ' ' + $
               STRING(startYear, FORMAT = '(I4.4)') + '-' + $
               STRING(finishYear, FORMAT = '(I4.4)') + $
               ' was generated from version 3.27-upd-2019062518 of the Daily Global Historical Climatology Network (GHCN-D) dataset. The process uses the approach described by Baxter (2005) to estimate mean climatological SLR values at GHCN-D sites in the U.S., Canada, and Mexico. In addition to the Baxter (2005) criteria, tests to eliminate SLR outliers and frequent reporters of SLR = 10.0 are included in the data selection process. A linear SLR vs. elevation trend is fitted to station mean SLR values, the detrended SLR residuals are interpolated using ordinary kriging with a spherical semivariogram model, and the result is added to a gridded SLR trend based on the GMTED 2010 digital elevation model. Based on cross-validation of kriging results, the final result has an estimated root-mean-squared error of ' + STRCRA(cvSLRRMSE) + '.'

  NCDF_ATTPUT, id, /GLOBAL, 'references', $
               'Baxter, M. A., Charles E. Graves, and James T. Moore, 2005: A Climatology of Snow-to-Liquid Ratio for the Contiguous United States, Weather and Forecasting, 20, 729-744, DOI: http://dx.doi.org/10.1175/WAF856.1'

  NCDF_ATTPUT, id, /GLOBAL, 'source', 'surface observations'

  SPAWN, 'date -u "+%Y-%m-%d %H:%M:%S"', sysTime
  sysTime = sysTime[0]

  NCDF_ATTPUT, id, /GLOBAL, 'history', sysTime + $
               ' UTC created by module: slr_climatology.pro'

  NCDF_ATTPUT, id, /GLOBAL, 'comment', sysTime + $
               ' UTC created comment: slr_climatology.pro written in IDL' + $
               ' by Greg Fall, August 2019'

  NCDF_ATTPUT, id, varID_lat, 'long_name', 'latitude'
  NCDF_ATTPUT, id, varID_lat, 'units', 'degrees_north'
  NCDF_ATTPUT, id, varID_lat, 'standard_name', 'latitude'

  NCDF_ATTPUT, id, varID_lon, 'long_name', 'longitude'
  NCDF_ATTPUT, id, varID_lon, 'units', 'degrees_east'
  NCDF_ATTPUT, id, varID_lon, 'standard_name', 'longitude'

  NCDF_ATTPUT, id, varID_data, 'institution', 'Office of Water Prediction'
  NCDF_ATTPUT, id, varID_data, 'gisrs_product_code', 0L
  NCDF_ATTPUT, id, varID_data, 'long_name', $
               'snowfall to liquid equivalent ratio'
  NCDF_ATTPUT, id, varID_data, 'standard_name', 'none'
  NCDF_ATTPUT, id, varID_data, 'satellite_data', 'no'
  NCDF_ATTPUT, id, varID_data, 'thematic', 'no'

  NCDF_ATTPUT, id, varID_data, 'data_are_elevations', 'no'
  NCDF_ATTPUT, id, varID_data, 'number_of_color_tables', 0

  NCDF_ATTPUT, id, varID_data, '_FillValue', ndvFlt, /FLOAT
  NCDF_ATTPUT, id, varID_data, 'units', 'dimensionless'
  NCDF_ATTPUT, id, varID_data, 'add_offset', 0.0
  NCDF_ATTPUT, id, varID_data, 'scale_factor', 1.0
  NCDF_ATTPUT, id, varID_data, 'no_data_value', ndvFlt, /FLOAT

  ind = WHERE(valGrid ne ndvFlt, count)
  if (count eq 0) then STOP
  minValue = MIN(valGrid[ind])
  maxValue = MAX(valGrid[ind])
  NCDF_ATTPUT, id, varID_data, 'minimum_data_value', minValue
  NCDF_ATTPUT, id, varID_data, 'maximum_data_value', maxValue
  NCDF_ATTPUT, id, varID_data, 'start_date', sysTime + ' UTC'
  NCDF_ATTPUT, id, varID_data, 'stop_date', sysTime + ' UTC'
  NCDF_ATTPUT, id, varID_data, 'grid_mapping', 'crs'

  NCDF_ATTPUT, id, varID_crs, 'grid_mapping_name', 'latitude_longitude'
  NCDF_ATTPUT, id, varID_crs, 'horizontal_datum', 'WGS84'
  NCDF_ATTPUT, id, varID_crs, 'longitude_of_prime_meridian', 0.0D
  NCDF_ATTPUT, id, varID_crs, 'semi_major_axis', 6378137.0D
  NCDF_ATTPUT, id, varID_crs, 'inverse_flattening', 298.257223563D

  NCDF_ATTPUT, id, varID_lat, 'resolution', yResOut
  NCDF_ATTPUT, id, varID_lat, 'origin_offset', 0.5D * yResOut
  NCDF_ATTPUT, id, varID_lat, 'bounds', 'lat_bounds'

  NCDF_ATTPUT, id, varID_lon, 'resolution', xResOut
  NCDF_ATTPUT, id, varID_lon, 'origin_offset', 0.5D * xResOut
  NCDF_ATTPUT, id, varID_lon, 'bounds', 'lon_bounds'

  yOut = minLatOut + (0.5D + DINDGEN(nyOut)) * yResOut
  yOutBounds = TRANSPOSE([[minLatOut + DINDGEN(nyOut) * yResOut], $
                          [minLatOut + (1.0D + DINDGEN(nyOut)) * yResOut]])
  NCDF_VARPUT, id, varID_lat, yOut
  NCDF_VARPUT, id, varID_latBounds, yOutBounds

  xOut = minLonOut + (0.5D + DINDGEN(nxOut)) * xResOut
  xOutBounds = $
    TRANSPOSE([[minLonOut + DINDGEN(nxOut) * xResOut], $
               [minLonOut + (1.0D + DINDGEN(nxOut)) * xResOut]])
  NCDF_VARPUT, id, varID_lon, xOut
  NCDF_VARPUT, id, varID_lonBounds, xOutBounds

  NCDF_VARPUT, id, varID_data, valGrid

  NCDF_CLOSE, id

  ;; yOut = !NULL
  ;; yOutBounds = !NULL
  ;; xOut = !NULL
  ;; xOutBounds = !NULL

  RETURN

end

GHCND_dir = '/nwcdev/archive/GHCN_daily_archive'

climStartYear = 1990
climFinishYear = 2019

climStartDate_MMDD = '0401'
climFinishDate_MMDD = '1031'
climName = 'Summer'
longClimName = 'Summer'
GENERATE_SLR_CLIMATOLOGY, GHCND_dir, $
                          climStartYear, $
                          climFinishYear, $
                          climStartDate_MMDD, $
                          climFinishDate_MMDD, $
                          climName, $
                          LONG_CLIM_NAME = longClimName

end
