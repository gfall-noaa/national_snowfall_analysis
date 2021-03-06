FUNCTION GET_MAX_AVE_DAILY_TEMP_FOR_SNOW, date_YYYYMMDDHH
  
; This function estimates, as a function of calendar date, the average daily
; temperature above which snowfall is unlikely to be observed.
;
; The hard-coded values below are results for a run of the program
; "snfl_prcp_tair_stats.pro" that examines 24-hour snowfall observations and
; corresponding min/max/ave daily temperatures from GHCN-D.
;
; For a given range of calendar dates, snowfall observations and corresponding
; average daily SNODAS temperatures are collected for all days in that range
; for the years 2005-2019 / 2006-2020 (the former for October - December, the
; latter for January - April). Those temperatures associated with daily
; snowfall totals exceeding 0.1 inch when precipitation exceeding 0.01 inch is
; also observed are effectively placed into a histogram, and temperature
; thresholds associated with various cumulative distribution function levels
; are identified.
  
; For this function, the value CSI_max_tc_degC[4] for each date range is being
; used (here it used to be called "aveTempZero95" because it was the average
; temperature associated with a POD = 95% result). Now it is called
; "aveTempMaxCSI" because it is that - the average temperature associated with
; the maximum critical success index (CSI).

; As we increase the average daily temperature at which we predict
; precipitation to be snowfall, the number of hits increases and the number of
; misses decreases, increasing (improving) the POD. However, the number of
; false alarms also increases as we do this, increasing (degrading) the
; FAR. The maximum CSI is the point at which one can argue that the best
; compromise is being made, i.e., we have raised the temperature enough to
; produce a good number of hits, but not so much that they are coming at the
; expense of too many false alarms.

; In earlier incarnations, the additional condition that the minimum daily
; temperature must be below freezing was applied to this process. However, the
; most recent effort has shown that this condition results in a slight
; degradation in our results, even though it seems a sensible requirement.

; The values of date_MMDD below are the midpoints of the 21-day range
; used to produce the histogram from which "aveTempMaxCSI" values are
; inferred. These ranges overlap by 10 days. The time of day
; associated with the temperature values is 12Z.

  date_MMDD = ['1001', '1011', '1021', '1031', '1110', '1120', '1130', $
               '1210', '1220', '1230', '0109', '0119', '0129', '0208', $
               '0218', '0228', '0310', '0320', '0330', '0409', '0419', $
               '0429']


; No manipulation gave the aveTempZero95 result of 6.91 degrees
; Celsius for both 01 Oct and 29 Apr. It just worked out that way.

;; These are the numbers for 2006-2015.
;  aveTempZero95 = [6.91, 6.24, 5.35, 5.65, 4.72, 3.53, 2.32, 1.69, $
;                   1.54, 1.38, 1.28, 1.38, 1.30, 1.41, 1.97, 2.65, $
;                   3.83, 4.89, 5.00, 6.07, 6.55, 6.91]

; These are the numbers for 2006-2017/18
  aveTempZero95 = [7.72, 6.99, 5.43, 5.35, 4.70, 3.79, 2.41, 1.65, $
                   1.44, 1.26, 1.30, 1.61, 1.49, 1.54, 2.10, 2.90, $
                   3.80, 4.89, 5.17, 5.58, 6.13, 6.59]

; These are the numbers for 2006-2018/19.
; (Date ranges that end after 12/31, the first of which is centered on
; '1230', include 2019 data.)
  aveTempZero95 = [7.18, 6.73, 5.78, 5.04, 4.12, 3.56, 2.41, 1.67, $
                   1.49, 1.33, 1.29, 1.62, 1.46, 1.44, 1.94, 2.65, $
                   3.59, 4.78, 5.14, 5.49, 5.98, 6.46]

; These are the average daily temperatures associated with maximum critical
; success index, for 2005-19 / 2006-20.
  aveTempMaxCSI = [1.5, 1.5, 1.0, 1.5, 1.5, 1.5, 1.5, 1.0, $
                   1.0, 1.0, 1.0, 1.0, 1.5, 1.5, 1.5, 2.0, $
                   2.0, 2.0, 2.5, 2.5, 2.5, 2.5]

; Check arguments for correct type and valid contents.

  if NOT(ISA(date_YYYYMMDDHH, 'STRING')) then begin
      ERR_MSG, 'Date/time argument must be a STRING.'
      STOP
  endif
  if (STRLEN(date_YYYYMMDDHH) ne 10) then begin
      ERR_MSG, 'Invalid date/time "' + $
               date_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, 10 digits).'
      STOP
  endif
  if NOT(STREGEX(date_YYYYMMDDHH, '[0-9]{10}', /BOOLEAN)) $
      then begin
      ERR_MSG, 'Invalid date/time "' + $
               targetDate_YYYYMMDDHH + $
               '" (required form is YYYYMMDDHH, all numeric).'
      STOP
  endif

  date_Julian = YYYYMMDDHH_TO_JULIAN(date_YYYYMMDDHH)
  date_YYYY = STRMID(date_YYYYMMDDHH, 0, 4)
  date_MM = STRMID(date_YYYYMMDDHH, 4, 2)
  date_DD = STRMID(date_YYYYMMDDHH, 6, 2)
  date_HH = STRMID(date_YYYYMMDDHH, 8, 2)


; Work backward until we reach the prior date_MMDD. The limit of 155
; is based on the fact that our MMDD range is from 1001 to 0429, and
; in a leap year, those are 155 days apart.

  daysBack = 0
  dj = date_Julian
  ds = date_YYYYMMDDHH
  mmdd = date_MM + date_DD
  ind = WHERE(date_MMDD eq mmdd, count)
  while ((count eq 0) and (daysBack lt 155)) do begin
      dj = dj - 1.0D
      ds = JULIAN_TO_YYYYMMDDHH(dj)
      mmdd = STRMID(ds, 4, 4)
      ind = WHERE(date_MMDD eq mmdd, count)
      daysBack++
  endwhile
  if (daysBack ne 155) then begin
      prevInd = ind[0]
      prevDate_Julian = dj
      prevDate_YYYYMMDDHH = ds
  endif else begin
      prevInd = -1
  endelse


; Work forward until we reach the next date_MMDD.

  daysFwd = 0
  dj = date_Julian
  ds = date_YYYYMMDDHH
  mmdd = date_MM + date_DD
  ind = WHERE(date_MMDD eq mmdd, count)
  while ((count eq 0) and (daysFwd lt 155)) do begin
      dj = dj + 1.0D
      ds = JULIAN_TO_YYYYMMDDHH(dj)
      mmdd = STRMID(ds, 4, 4)
      ind = WHERE(date_MMDD eq mmdd, count)
      daysFwd++
  endwhile
  if (daysFwd ne 155) then begin
      nextInd = ind[0]
      nextDate_Julian = dj
      nextDate_YYYYMMDDHH = ds
  endif else begin
      nextInd = -1
  endelse

  if ((prevInd eq -1) and (nextInd eq -1)) then begin
      ERR_MSG, 'PROGRAMMIMG ERROR'
      STOP
  endif

  case 1 of
      prevInd eq -1: cutoff = aveTempMaxCSI[nextInd]
      nextInd eq -1: cutoff = aveTempMaxCSI[prevInd]
      prevInd eq nextInd: cutoff = aveTempMaxCSI[prevInd]
      else: cutoff = aveTempMaxCSI[prevInd] + $
                     (aveTempMaxCSI[nextInd] - aveTempMaxCSI[prevInd]) / $
                     (nextDate_Julian - prevDate_Julian) * $
                     (date_Julian - prevDate_Julian)
  endcase

  RETURN, cutoff


end
