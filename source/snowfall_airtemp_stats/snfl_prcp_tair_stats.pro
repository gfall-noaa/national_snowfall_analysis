
FUNCTION REP_DATE, year, startDate_MMDD, finishDate_MMDD

  startMonth = FIX(STRMID(startDate_MMDD, 0, 2))
  finishMonth = FIX(STRMID(finishDate_MMDD, 0, 2))
  startYear = year
  finishYear = year
  crossNewYear = 0B
  if (finishMonth lt startMonth) then finishYear = finishYear + 1
  startDate_Julian = $
      YYYYMMDDHH_TO_JULIAN(STRING(startYear, FORMAT = '(I4.4)') + $
                           startDate_MMDD + '12')
  finishDate_Julian = $
      YYYYMMDDHH_TO_JULIAN(STRING(finishYear, FORMAT = '(I4.4)') + $
                           finishDate_MMDD + '12')
  RETURN, 0.5 * (startDate_Julian + finishDate_Julian)

end

PRO PLOT_SNFL_PRCP_TAIR_STATS, date_Julian, $
                               tair_thresh_name, $
                               all_POD_target_tair_thresh_degC, $
                               all_POD_target_tair_thresh_FAR, $
                               all_CSI_max, $
                               all_CSI_max_tc_degC, $
                               all_CSI_max_POD, $
                               all_CSI_max_FAR, $
                               time_str
  !P.Color = 0
  !P.Background = 255
  !P.CharSize = 2
  dummy = LABEL_DATE(DATE_FORMAT = '%N-%D')
  for i = 0, N_ELEMENTS(tair_thresh_name) - 1 do begin
      ttdgc = REFORM(all_POD_target_tair_thresh_degC[i, *])
      ttfar = REFORM(all_POD_target_tair_thresh_FAR[i, *])

      ttcsi = REFORM(all_CSI_max[i, *])
      ttdgc = REFORM(all_CSI_max_tc_degC[i, *])
      ttpod = REFORM(all_CSI_max_POD[i, *])
      ttfar = REFORM(all_CSI_max_FAR[i, *])
      
      PLOT, date_Julian, ttdgc, $
            YSTYLE = 8, $
            XRANGE = [MIN(date_Julian), MAX(date_Julian)], XSTYLE = 1, $
            XTICKFORMAT = 'LABEL_DATE', XTICKUNITS = 'Month', $
            YTITLE = tair_thresh_name[i] + ' Cutoff (!Uo!NC)', $
            TITLE = tair_thresh_name[i] + ' Cutoff for Snowfall, ' + $
                    time_str, $
            POS = [0.1, 0.1, 0.9, 0.9], $
            YMINOR = 1, $
            CHARSIZE = 2, PSYM = -4, SYMSIZE = 2, /NOCLIP
      PLOT, date_Julian, ttfar, $
            /NOERASE, XSTYLE = 5, YSTYLE = 5, $
            XRANGE = !X.CRange, $
            YRANGE = [0, 1], $
            POS = [0.1, 0.1, 0.9, 0.9], $
            CHARSIZE = 2, PSYM = -7, SYMSIZE = 1.5, /NOCLIP
      OPLOT, date_Julian, ttpod, $
             PSYM = -6, SYMSIZE = 1.5, /NOCLIP
      OPLOT, date_Julian, ttcsi, $
             PSYM = -5, SYMSIZE = 1.5, /NOCLIP
      AXIS, YAXIS = 1, YTITLE = 'POD, FAR, CSI', CHARSIZE = 2

      x1 = 0.15
      wid = 0.10
      y1 = 0.18
      x_gap = 0.02
      y_gap = 0.01
      y_break = 0.05

      PLOTS, [x1, x1 + wid], [y1, y1], PSYM = -7, SYMSIZE = 1.5, /NORMAL
      XYOUTS, x1 + wid + x_gap, y1 - y_gap, 'FAR', /NORMAL, CHARSIZE = 2
;      PLOTS, [0.15, 0.25], [0.18, 0.18], PSYM = -7, SYMSIZE = 1.5, /NORMAL
;      XYOUTS, 0.27, 0.17, 'FAR', /NORMAL, CHARSIZE = 2

      y1 = y1 + y_break
      PLOTS, [x1, x1 + wid], [y1, y1], PSYM = -5, SYMSIZE = 1.5, /NORMAL
      XYOUTS, x1 + wid + x_gap, y1 - y_gap, 'CSI', /NORMAL, CHARSIZE = 2

      y1 = y1 + y_break
      PLOTS, [x1, x1 + wid], [y1, y1], PSYM = -6, SYMSIZE = 1.5, /NORMAL
      XYOUTS, x1 + wid + x_gap, y1 - y_gap, 'POD', /NORMAL, CHARSIZE = 2

      y1 = y1 + y_break
      PLOTS, [x1, x1 + wid], [y1, y1], PSYM = -4, SYMSIZE = 2, /NORMAL
      XYOUTS, x1 + wid + x_gap, y1 - y_gap, $
              tair_thresh_name[i], /NORMAL, CHARSIZE = 2

      if (i lt (N_ELEMENTS(tair_thresh_name) - 1)) then $
          move = GET_KBRD(1)
  endfor

  RETURN
  
end

PRO PRCP_SNFL_TAIR_POD_FAR, prcp, $     ; tenths of mm
                            snow, $     ; mm
                            tair, $     ; tenths of deg C
                            t_cutoff, $ ; tenths of deg C
                            POD_target, $
                            FAR_target, $
                            ndv, $
                            tair_name, $
                            time_str, $
                            prcp_threshold, $
                            snow_threshold, $
                            POD_target_tair_thresh, $      ; output
                            POD_target_tair_thresh_FAR, $  ; output
                            FAR_target_tair_thresh, $      ; output
                            FAR_target_tair_thresh_POD, $  ; output
                            CSI_max, $                     ; output
                            CSI_max_tc, $                  ; output
                            CSI_max_POD, $                 ; output
                            CSI_max_FAR, $                 ; output
                            T_COLD = t_cold, $
                            CUTOFF_T_COLD = cutoff_t_cold

; Determine the value of tair at which a prediction of snowfall when
; the temperature is below that value gives a POD of POD_target, and
; determine the associated FAR.

; Next, determine the value of tair at which a prediction of snowfall
; when the temperature is below that value gives a FAR of FAR_target,
; and determine the associated POD.

; Finally, determine the value of tair at which a prediction of
; snowfall when the temperature is below that value gives the maximum
; possible value of CSI, and determind the associated POD and FAR.
  
; t_cutoff is an array of temperature at which POD and FAR are
; calculated. The idea here is that tair is perhaps an average daily
; temperature, and we hope to identify the value of tair below which
; precipitation should be expected to fall as snow, based on the input
; data.

; T_COLD and CUTOFF_T_COLD allow you to include another set of
; temperatures which must be below CUTOFF_T_COLD for the snowfall
; prediction on which POD and FAR are based to register snowfall. The
; main use of this is T_COLD = minimum daily temperature and
; CUTOFF_T_COLD = 0 deg C. So this imposes a pre-defined condition on
; the prediction of snowfall, in addition to the condition used to
; find POD and FAR as a function of t_cutoff.

  POD_target_tair_thresh = !NULL
  POD_target_tair_thresh_FAR = !NULL
  FAR_target_tair_thresh = !NULL
  FAR_target_tair_thresh_POD = !NULL

  !P.Color = 0
  !P.Background = 255
  
  if NOT(KEYWORD_SET(T_COLD)) then T_COLD = tair
  if NOT(KEYWORD_SET(CUTOFF_T_COLD)) then cutoff_t_cold = MAX(tair) + 1L

  num_snfl = N_ELEMENTS(prcp)
  if (N_ELEMENTS(snow) ne num_snfl) then STOP
  if (N_ELEMENTS(tair) ne num_snfl) then STOP
  
  num_tc = N_ELEMENTS(t_cutoff)
  tc_min = MIN(t_cutoff)
  tc_max = MAX(t_cutoff)

  POD = FLTARR(num_tc) & POD[*] = ndv
  FAR = FLTARR(num_tc) & FAR[*] = ndv
  CSI = FLTARR(num_tc) & CSI[*] = ndv

  for tci = 0, num_tc - 1 do begin
      hit_ind = WHERE((prcp gt prcp_threshold) and $
                      ((tair lt t_cutoff[tci]) and $
                       (t_cold lt cutoff_t_cold)) and $
                      (snow gt snow_threshold), $
                      hit_count)
      fp_ind = WHERE((prcp gt prcp_threshold) and $
                     ((tair lt t_cutoff[tci]) and $
                      (t_cold lt cutoff_t_cold)) and $
                     (snow le snow_threshold), $
                     fp_count)
      miss_ind = WHERE((prcp gt prcp_threshold) and $
                       ((tair ge t_cutoff[tci]) or $
                        (t_cold ge cutoff_t_cold)) and $
                       (snow gt snow_threshold), $
                       miss_count)
      cn_ind = WHERE((prcp gt prcp_threshold) and $
                     ((tair ge t_cutoff[tci]) or $
                      (t_cold ge cutoff_t_cold)) and $
                     (snow le snow_threshold), $
                     cn_count)
      if ((hit_count + miss_count) gt 0) then $
          POD[tci] = FLOAT(hit_count) / FLOAT(hit_count + miss_count)
      if ((hit_count + fp_count) gt 0) then $
          FAR[tci] = FLOAT(fp_count) / FLOAT(hit_count + fp_count)
      if ((hit_count + miss_count + fp_count) gt 0) then $
          CSI[tci] = FLOAT(hit_count) / $
                     FLOAT(hit_count + miss_count + fp_count)
          
  endfor

; Plot POD vs. cutoff temperature
  
  ind = WHERE(POD ne ndv, count)
  if (count eq 0) then STOP
  PLOT, t_cutoff[ind] / 10.0, POD[ind], $
        TITLE = 'POD, FAR, and CSI for ' + tair_name + '; ' + time_str, $
        XTITLE = tair_name + ' (!Uo!NC)', $
        YTITLE = 'POD, FAR, CSI (Dimensionless)', $
        XRANGE = [tc_min / 10.0, tc_max / 10.0], XSTYLE = 1, $
        YRANGE = [0, 1], $
        POS = [0.1, 0.1, 0.9, 0.9], $
        LINESTYLE = 2, $
        CHARSIZE = 2 ;PSYM = -1, SYMSIZE = 2
  AXIS, YAXIS = 1, CHARSIZE = 2
  x_range = !X.CRange[1] - !X.CRange[0]
  y_range = !Y.CRange[1] - !Y.CRange[0]

; Find POD = POD_target
  i1 = MAX(WHERE((POD ne ndv) and (POD lt POD_target)))
  if ((i1 eq -1) or (i1 eq num_tc)) then RETURN
  i2 = i1 + 1
  if (POD[i2] eq ndv) then RETURN
  POD_target_tair_thresh_ = t_cutoff[i1] + $
                FLOAT(t_cutoff[i2] - t_cutoff[i1]) / $
                (POD[i2] - POD[i1]) * $
                            (POD_target - POD[i1])
  ;; ARROW, !X.CRange[0], POD_target, $
  ;;        POD_target_tair_thresh_ / 10.0, POD_target, $
  ;;        /DATA, HSIZE = !D.X_Size * 0.01, COLOR = 180, THICK = 2
  ;; ARROW, POD_target_tair_thresh_ / 10.0, POD_target, $
  ;;        POD_target_tair_thresh_ / 10.0, !Y.CRange[0], $
  ;;        /DATA, HSIZE = !D.X_Size * 0.01, COLOR = 180, THICK = 2
  ;; XYOUTS, !X.CRange[0] + 0.03 * x_range, $
  ;;         POD_target - 0.04 * y_range, $
  ;;         'POD target = ' + FORMAT_FLOAT(POD_target), $
  ;;         CHARSIZE = 2
  PRINT, 'POD = ' + STRCRA(POD_target) + $
         ' occurs at ' + STRCRA(POD_target_tair_thresh_ / 10.0) + ' deg C'
  ind = WHERE(FAR ne ndv, count)
  if (count eq 0) then RETURN
  OPLOT, t_cutoff[ind] / 10.0, FAR[ind], LINESTYLE = 1
                                ;, PSYM = -7, SYMSIZE = 1.5

  if ((FAR[i1] eq ndv) or (FAR[i2]) eq ndv) then RETURN
  POD_target_tair_thresh_FAR_ = FAR[i1] + (FAR[i2] - FAR[i1]) / $
               FLOAT(t_cutoff[i2] - t_cutoff[i1]) * $
               FLOAT(POD_target_tair_thresh_ - t_cutoff[i1])
  ;; PLOTS, [POD_target_tair_thresh_ / 10.0, $
  ;;         !X.CRange[1]], $
  ;;        [POD_target_tair_thresh_FAR_, $
  ;;         POD_target_tair_thresh_FAR_], $
  ;;        LINESTYLE = 2, COLOR = 180, THICK = 2
  ;; XYOUTS, !X.CRange[1] - 0.03 * x_range, $
  ;;         POD_target_tair_thresh_FAR_ + 0.015 * y_range, $
  ;;         'FAR = ' + FORMAT_FLOAT(POD_target_tair_thresh_FAR_), $
  ;;         CHARSIZE = 2, ALIGNMENT = 1
  PRINT, 'FAR = ' + STRCRA(POD_target_tair_thresh_FAR_) + ' at ' + $
         STRCRA(POD_target_tair_thresh_ / 10.0) + ' deg C'

;;   if (tair_name eq 'Tavg') then begin
;; ;     Adjust POD downward (within limits) as needed to achive a FAR
;; ;     not exceeding POD_target_tair_thresh_FAR_min.
;;       POD_compromise = POD_target
;;       POD_target_tair_thresh_compromise = POD_target_tair_thresh_
;;       FAR_compromise = POD_target_tair_thresh_FAR_
;;       POD_step_size = 0.00001
;;       POD_target_tair_thresh_POD_min = 0.8 ; Lowest POD we will tolerate.
;;       POD_target_tair_thresh_FAR_min = 0.5 ; Target minimum value for FAR.
;;       no_break = 0
;;       while ((FAR_compromise gt POD_target_tair_thresh_FAR_min) and $
;;              (POD_compromise ge POD_target_tair_thresh_POD_min)) do begin
;;           no_break = 0
;;           POD_compromise = POD_compromise - POD_step_size
;;           i1 = MAX(WHERE((POD ne ndv) and (POD lt POD_compromise)))
;;           if ((i1 eq -1) or (i1 eq num_tc)) then BREAK
;;           i2 = i1 + 1
;;           if (POD[i2] eq ndv) then BREAK
;;           POD_target_tair_thresh_compromise = t_cutoff[i1] + $
;;                                    FLOAT(t_cutoff[i2] - t_cutoff[i1]) / $
;;                                    (POD[i2] - POD[i1]) * $
;;                                    (POD_compromise - POD[i1])
;;           if ((FAR[i1] eq ndv) or (FAR[i2]) eq ndv) then BREAK
;;           FAR_compromise = $
;;               FAR[i1] + (FAR[i2] - FAR[i1]) / $
;;               FLOAT(t_cutoff[i2] - t_cutoff[i1]) * $
;;               FLOAT(POD_target_tair_thresh_compromise - t_cutoff[i1])
;;           no_break = 1
;;       endwhile
;;       if no_break then begin
;;           PRINT, 'To achieve a FAR <= ' + $
;;                  STRCRA(POD_target_tair_thresh_FAR_min) + $
;;                  ' without allowing POD to fall below ' + $
;;                  STRCRA(POD_target_tair_thresh_POD_min) + $
;;                  ', consider a cutoff "' + tair_name + '" of ' + $
;;                  STRCRA(POD_target_tair_thresh_compromise / 10.0) + $
;;                  ', with POD = ' + $
;;                  STRCRA(POD_compromise) + $
;;                  ' and FAR = ' + $
;;                  STRCRA(FAR_compromise)
;;       endif
;;   endif


  ind = WHERE(CSI ne ndv, count)
  if (count eq 0) then RETURN
  OPLOT, t_cutoff[ind] / 10.0, CSI[ind] ;, LINESTYLE = 6
  if ((CSI[i1] eq ndv) or (CSI[i2]) eq ndv) then RETURN
  POD_target_tair_thresh_CSI_ = CSI[i1] + (CSI[i2] - CSI[i1]) / $
               FLOAT(t_cutoff[i2] - t_cutoff[i1]) * $
               FLOAT(POD_target_tair_thresh_ - t_cutoff[i1])
  PRINT, 'CSI = ' + STRCRA(POD_target_tair_thresh_CSI_) + ' at ' + $
         STRCRA(POD_target_tair_thresh_ / 10.0)  + ' deg C'


; Find FAR = FAR_target
  i1 = MAX(WHERE((FAR ne ndv) and (FAR lt FAR_target)))
  if ((i1 eq -1) or (i1 eq num_tc)) then RETURN
  i2 = i1 + 1
  if (FAR[i2] eq ndv) then RETURN
  FAR_target_tair_thresh_ = t_cutoff[i1] + $
                FLOAT(t_cutoff[i2] - t_cutoff[i1]) / $
                (FAR[i2] - FAR[i1]) * $
                            (FAR_target - FAR[i1])
  ;; ARROW, !X.CRange[1], FAR_target, $
  ;;        FAR_target_tair_thresh_ / 10.0, FAR_target, $
  ;;        /DATA, HSIZE = !D.X_Size * 0.01, COLOR = 180, THICK = 2
  ;; ARROW, FAR_target_tair_thresh_ / 10.0, FAR_target, $
  ;;        FAR_target_tair_thresh_ / 10.0, !Y.CRange[0], $
  ;;        /DATA, HSIZE = !D.X_Size * 0.01, COLOR = 180, THICK = 2
  ;; XYOUTS, !X.CRange[1] - 0.03 * x_range, $
  ;;         FAR_target + 0.015 * y_range, $
  ;;         'FAR target = ' + FORMAT_FLOAT(FAR_target), $
  ;;         CHARSIZE = 2, ALIGNMENT = 1
  PRINT, 'FAR = ' + STRCRA(FAR_target) + $
         ' occurs at ' + STRCRA(FAR_target_tair_thresh_ / 10.0) + ' deg C'

  if ((POD[i1] eq ndv) or (POD[i2]) eq ndv) then RETURN
  FAR_target_tair_thresh_POD_ = POD[i1] + (POD[i2] - POD[i1]) / $
               FLOAT(t_cutoff[i2] - t_cutoff[i1]) * $
                                FLOAT(FAR_target_tair_thresh_ - t_cutoff[i1])
  ;; PLOTS, [FAR_target_tair_thresh_ / 10.0, $
  ;;         FAR_target_tair_thresh_ / 10.0, $
  ;;         !X.CRange[0]], $
  ;;        [FAR_target, $
  ;;         FAR_target_tair_thresh_POD_, $
  ;;         FAR_target_tair_thresh_POD_], $
  ;;        LINESTYLE = 2, COLOR = 180, THICK = 2
  ;; XYOUTS, !X.Crange[0] + 0.03 * x_range, $
  ;;         FAR_target_tair_thresh_POD_ + 0.015 * y_range, $
  ;;         'POD = ' + FORMAT_FLOAT(FAR_target_tair_thresh_POD_), $
  ;;         CHARSIZE = 2
  PRINT, 'POD = ' + STRCRA(FAR_target_tair_thresh_POD_) + ' at ' + $
         STRCRA(FAR_target_tair_thresh_ / 10.0) + ' deg C'



; Find POD and FAR at peak CSI.

  CSI_max = !NULL
  CSI_max_tc = !NULL
  CSI_max_POD = !NULL
  CSI_max_FAR = !NULL
  ind = WHERE(CSI ne ndv, count)
  if (count eq 0) then RETURN
  CSI_max = MAX(CSI[ind], CSI_max_ind)
  CSI_max_tc = t_cutoff[ind[CSI_max_ind]]
  CSI_max_POD = POD[ind[CSI_max_ind]]
  CSI_max_FAR = FAR[ind[CSI_max_ind]]
  PRINT, 'Maximum CSI = ' + STRCRA(CSI_max) + ' occurs at ' + $
         STRCRA(CSI_max_tc / 10.0) + ' deg C, with ' + $
         'POD = ' + STRCRA(CSI_max_POD) + $
         ', FAR = ' + STRCRA(CSI_max_FAR)          
    
  POD_target_tair_thresh = TEMPORARY(POD_target_tair_thresh_)
  POD_target_tair_thresh_FAR = TEMPORARY(POD_target_tair_thresh_FAR_)
  FAR_target_tair_thresh = TEMPORARY(FAR_target_tair_thresh_)
  FAR_target_tair_thresh_POD = TEMPORARY(FAR_target_tair_thresh_POD_)

  ARROW, CSI_max_tc / 10.0, CSI_max, $
         0.5 * (!X.CRange[0] + CSI_max_tc / 10.0), CSI_max, $
         /DATA, HSIZE = !D.X_SIZE * 0.01, THICK = 2
  XYOUTS, 0.5 * (!X.CRange[0] + CSI_max_tc / 10.0) - 0.01 * x_range, $
          CSI_max - 0.0 * y_range, $
          'Max. CSI = ' + FORMAT_FLOAT(CSI_max) + $
          '!Cat ' + FORMAT_FLOAT(CSI_max_tc / 10.0) + '!Uo!NC', $
          CHARSIZE = 2, ALIGNMENT = 1

  PLOTS, [CSI_max_tc / 10.0, CSI_max_tc / 10.0], $
         [CSI_max, CSI_max_POD], COLOR = 180, THICK = 2
  ARROW, CSI_MAX_tc / 10.0, CSI_max_POD, $
         !X.CRange[0], CSI_max_POD, $
         /DATA, HSIZE = !D.X_SIZE * 0.01, COLOR = 180, THICK = 2
  XYOUTS, !X.CRange[0] + 0.05 * x_range, $
          CSI_max_POD + 0.01 * y_range, $
          'POD = ' + FORMAT_FLOAT(CSI_max_POD)
  PLOTS, [CSI_max_tc / 10.0, CSI_max_tc / 10.0], $
         [CSI_max, CSI_max_FAR], COLOR = 180, THICK = 2
  ARROW, CSI_MAX_tc / 10.0, CSI_max_FAR, $
         !X.CRange[1], CSI_max_FAR, $
         /DATA, HSIZE = !D.X_SIZE * 0.01, COLOR = 180, THICK = 2
  XYOUTS, !X.CRange[1] - 0.05 * x_range, $
          CSI_max_FAR + 0.01 * y_range, $
          'FAR = ' + FORMAT_FLOAT(CSI_max_FAR), $
          ALIGNMENT = 1
  RETURN
  

end


PRO SNFL_PRCP_TAIR_STATS, startYear, $        ; IN
                          finishYear, $       ; IN
                          startDate_MMDD, $   ; IN
                          finishDate_MMDD, $  ; IN
                          prcp_threshold, $   ; IN - tenths of mm
                          snow_threshold, $   ; IN - mm $
                          POD_target, $       ; IN - 0 to 1
                          FAR_target, $       ; IN - 0 to 1
                          tair_thresh_name, $ ; OUT
                          POD_target_tair_thresh_degC, $ ; OUT
                          POD_target_tair_thresh_FAR, $  ; OUT
                          FAR_target_tair_thresh_degC, $
                          FAR_target_tair_thresh_POD, $
                          CSI_max, $
                          CSI_max_tc_degC, $
                          CSI_max_POD, $
                          CSI_max_FAR

; Using GHCN-D data, evaluate statistics associated with
; precipitation, snowfall, and minimum/maximum/average daily temperature.

; In snfl_airtemp_stats_batch.pro, we determined the temperature at which 95%
; of observed nonzero snowfall was associated with a lower average daily
; temperature, under the condition that the daily minimum temperature is below
; freezing.

; In this program we will attempt something similar, but will use GHCN-D data,
; and will take false alarms into account.

  COMMON info, message
  message = ''

; Define date range. If START_DATE_MMDD and FINISH_DATE_MMDD imply
; that the date range crosses the calendar year, then START_YEAR and
; FINISH_YEAR should apply to the START_DATE_MMDD.
  ;; startYear = GETENV('START_YEAR')
  ;; if (N_ELEMENTS(startYear) ne 1) then $
  ;;     MESSAGE, 'no start year'
  ;; if (STRLEN(startYear) ne 4) then $
  ;;     MESSAGE, 'bad start year "' + startYear + '"'
  ;; startYear = FIX(startYear)

  ;; finishYear = GETENV('FINISH_YEAR')
  ;; if (N_ELEMENTS(finishYear) ne 1) then $
  ;;     MESSAGE, 'no finish year'
  ;; if (STRLEN(finishYear) ne 4) then $
  ;;     MESSAGE, 'bad finish year'
  ;; finishYear = FIX(finishYear)

  ;; startDate_MMDD = GETENV('START_DATE_MMDD')
  ;; if (N_ELEMENTS(startDate_MMDD) ne 1) then $
  ;;     MESSAGE, 'no start mmdd'
  ;; if (STRLEN(startDate_MMDD) ne 4) then $
  ;;     MESSAGE, 'bad start mmdd'

  ;; finishDate_MMDD = GETENV('FINISH_DATE_MMDD')
  ;; if (N_ELEMENTS(finishDate_MMDD) ne 1) then $
  ;;     MESSAGE, 'no finish mmdd'
  ;; if (STRLEN(finishDate_MMDD) ne 4) then $
  ;;     MESSAGE, 'bad finish mmdd'

  GHCN_dir = '/nwcdev/archive/GHCN_daily_archive'

; Determine whether climatology period crosses January 1. Start times
; and finish times are in different calendar years in that case.
  startMonth = FIX(STRMID(startDate_MMDD, 0, 2))
  finishMonth = FIX(STRMID(finishDate_MMDD, 0, 2))
  crossNewYear = 0B
  finishYear_ = finishYear
  if (finishMonth lt startMonth) then begin
      crossNewYear = 1B
      finishYear_ = finishYear_ + 1
      PRINT, 'This analysis period includes a date range covering '  + $
             'different calendar years.'
  endif

  snflThreshold = 0.1 ; inches

  savFile = 'snfl_prcp_tair_obs_' + $
            startDate_MMDD + '_to_' + $
            finishDate_MMDD + '_' + $
            STRING(startYear, FORMAT = '(i4.4)') + '_to_' + $
            STRING(finishYear, FORMAT = '(i4.4)') + '.sav'

  fullRun = 1
  if FILE_TEST(savFile) then begin
      fullRun = 0
      GOTO, SKIP
  endif

; Define spatial domain.
  minLatOut = 21.0D
  maxLatOut = 55.0D
  minLonOut = -126.0D
  maxLonOut = -66.0D
  ndv = -9999
  date_Julian = SYSTIME(/JULIAN, /UTC)

; Initialize arrays that will be used to generate stats.
  prcp_tmin = []
  snow_tmin = []
  tmin = [] ; TMIN
  prcp_tmax = []
  snow_tmax = []
  tmax = [] ; TMAX
  prcp_tavg = []
  snow_tavg = []
  tavg = [] ; TAVG
  prcp_frz = []
  snow_frz = []
  tavg_frz = [] ; TAVG and TMIN for TAVG
  tmin_frz = [] ; when TMIN < 0 deg C
  prcp_tmid = []
  snow_tmid = []
  tmid = [] ; (TMIN + TMAX) / 2
  
; Loop over years of analysis period.
  for year = startYear, finishYear_ do begin

      YYYY = STRING(year, FORMAT = '(I4.4)')

;     Get GHCN data for this year.
      PRINT, 'Getting GHCN-D data for ' + YYYY + '...'
      READ_GHCND_US_CA_MX_BY_YEAR_SNFL_PRCP_TAIR, $
          GHCN_dir, year, ndv, $
          GHCN_num_days, GHCN_num_stations, GHCN_station_id, GHCN_data;, $
          ;; /VERBOSE

      Jan01_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + '010112')
      
;     Identify days of the analysis period to cover for this year.
      if (crossNewYear) then begin

          Dec31_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + '123112')

          case year of

              startYear: begin
                  t1_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + startDate_MMDD + '12')
                  t2_Julian = Dec31_Julian
                  date_Julian = t1_Julian + $
                                DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D))
              end

              finishYear_: begin
                  t1_Julian = Jan01_Julian
                  t2_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + finishDate_MMDD + '12')
                  date_Julian = t1_Julian + $
                                DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D))
              end

              else: begin
                  t1_Julian = Jan01_Julian
                  t2_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + finishDate_MMDD + '12')
                  t3_Julian = $
                      YYYYMMDDHH_TO_JULIAN(YYYY + startDate_MMDD + '12')
                  t4_Julian = Dec31_Julian
                  date_Julian = $
                      [t1_Julian + $
                       DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D)), $
                       t3_Julian + $
                       DINDGEN(ROUND(t4_Julian - t3_Julian + 1.0D))]
              end

          endcase

      endif else begin

          t1_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + startDate_MMDD + '12')
          t2_Julian = YYYYMMDDHH_TO_JULIAN(YYYY + finishDate_MMDD + '12')
          date_Julian = t1_Julian + $
                        DINDGEN(ROUND(t2_Julian - t1_Julian + 1.0D))

      endelse

      numDays = N_ELEMENTS(date_Julian)

;     Loop over days of analysis period; collect GHCN-D snowfall,
;     precipitation, and temperature data for each day.
      for dc = 0, numDays - 1 do begin

          date_YYYYMMDD = STRMID(JULIAN_TO_YYYYMMDDHH(date_Julian[dc]), 0, 8)
          
;         Extract the GHCN data for the current day.
          gdc = ROUND(date_Julian[dc] - Jan01_Julian)
          if ((gdc lt 0) or (gdc ge GHCN_num_days)) then STOP

          daily_GHCN_data = REFORM(GHCN_data[gdc, *])
          
;         Identify and extract valid data. May want to consider
;         keeping PRCP/SNOW data with mflag values of "P" (missing
;         presumed zero). For now we will ignore the sflag (source
;         flag), since there are 20 acceptable values and they have
;         nothing to do with data quality.
          ind = WHERE((daily_GHCN_data.prcp ne ndv) and $
                      (daily_GHCN_data.prcp ge 0) and $
                      ((daily_GHCN_data.prcp_mflag eq '') or $
                       (daily_GHCN_data.prcp_mflag eq 'B') or $
                       (daily_GHCN_data.prcp_mflag eq 'D') or $
                       (daily_GHCN_data.prcp_mflag eq 'T')) and $
                      (daily_GHCN_data.prcp_qflag eq '') and $
                      (daily_GHCN_data.snow ne ndv) and $
                      (daily_GHCN_data.snow ge 0) and $
                      ((daily_GHCN_data.snow_mflag eq '') or $
                       (daily_GHCN_data.snow_mflag eq 'B') or $
                       (daily_GHCN_data.snow_mflag eq 'D') or $
                       (daily_GHCN_data.snow_mflag eq 'T')) and $
                      (daily_GHCN_data.snow_qflag eq ''), $
                      count)

          if (count eq 0) then begin
              PRINT, 'No nonzero PRCP/SNOW for ' + date_YYYYMMDD
              CONTINUE
          endif
          
          ind_tmin = WHERE((daily_GHCN_data[ind].tmin ne ndv) and $
                           (daily_GHCN_data[ind].tmin gt -700) and $
                           (daily_GHCN_data[ind].tmin lt 700) and $
                           ((daily_GHCN_data[ind].tmin_mflag eq '') or $
                            (daily_GHCN_data[ind].tmin_mflag eq 'H')) and $
                           (daily_GHCN_data[ind].tmin_qflag eq ''), $
                           count_tmin)

          if (count_tmin gt 0) then begin
              prcp_tmin = [prcp_tmin, daily_GHCN_data[ind[ind_tmin]].prcp]
              snow_tmin = [snow_tmin, daily_GHCN_data[ind[ind_tmin]].snow]
              tmin = [tmin, daily_GHCN_data[ind[ind_tmin]].tmin]
          endif
          
          ind_tmax = WHERE((daily_GHCN_data[ind].tmax ne ndv) and $
                           (daily_GHCN_data[ind].tmax gt -700) and $
                           (daily_GHCN_data[ind].tmax lt 700) and $
                           ((daily_GHCN_data[ind].tmax_mflag eq '') or $
                            (daily_GHCN_data[ind].tmax_mflag eq 'H')) and $
                           (daily_GHCN_data[ind].tmax_qflag eq ''), $
                           count_tmax)

          if (count_tmax gt 0) then begin
              prcp_tmax = [prcp_tmax, daily_GHCN_data[ind[ind_tmax]].prcp]
              snow_tmax = [snow_tmax, daily_GHCN_data[ind[ind_tmax]].snow]
              tmax = [tmax, daily_GHCN_data[ind[ind_tmax]].tmax]
          endif
          
          ind_tavg = WHERE((daily_GHCN_data[ind].tavg ne ndv) and $
                           (daily_GHCN_data[ind].tavg gt -700) and $
                           (daily_GHCN_data[ind].tavg lt 700) and $
                           ((daily_GHCN_data[ind].tavg_mflag eq '') or $
                            (daily_GHCN_data[ind].tavg_mflag eq 'H')) and $
                           (daily_GHCN_data[ind].tavg_qflag eq ''), $
                           count_tavg)

          if (count_tavg gt 0) then begin
              prcp_tavg = [prcp_tavg, daily_GHCN_data[ind[ind_tavg]].prcp]
              snow_tavg = [snow_tavg, daily_GHCN_data[ind[ind_tavg]].snow]
              tavg = [tavg, daily_GHCN_data[ind[ind_tavg]].tavg]
          endif else begin
              PRINT, 'No TAVG data for ' + date_YYYYMMDD
              STOP
          endelse

          ind_frz = WHERE((daily_GHCN_data[ind].tmin ne ndv) and $
                          (daily_GHCN_data[ind].tmin gt -700) and $
                          (daily_GHCN_data[ind].tmin lt 700) and $
                          ((daily_GHCN_data[ind].tmin_mflag eq '') or $
                           (daily_GHCN_data[ind].tmin_mflag eq 'H')) and $
                          (daily_GHCN_data[ind].tmin_qflag eq '') and $
                          (daily_GHCN_data[ind].tavg ne ndv) and $
                          (daily_GHCN_data[ind].tavg gt -700) and $
                          (daily_GHCN_data[ind].tavg lt 700) and $
                          ((daily_GHCN_data[ind].tavg_mflag eq '') or $
                           (daily_GHCN_data[ind].tavg_mflag eq 'H')) and $
                          (daily_GHCN_data[ind].tavg_qflag eq ''), $
                          count_frz)

          if (count_frz gt 0) then begin
              prcp_frz = [prcp_frz, daily_GHCN_data[ind[ind_frz]].prcp]
              snow_frz = [snow_frz, daily_GHCN_data[ind[ind_frz]].snow]
              tavg_frz = [tavg_frz, daily_GHCN_data[ind[ind_frz]].tavg]
              tmin_frz = [tmin_frz, daily_GHCN_data[ind[ind_frz]].tmin]
          endif
          
          ind_tmid = WHERE((daily_GHCN_data[ind].tmin ne ndv) and $
                           (daily_GHCN_data[ind].tmin gt -700) and $
                           (daily_GHCN_data[ind].tmin lt 700) and $
                           ((daily_GHCN_data[ind].tmin_mflag eq '') or $
                            (daily_GHCN_data[ind].tmin_mflag eq 'H')) and $
                           (daily_GHCN_data[ind].tmin_qflag eq '') and $
                           (daily_GHCN_data[ind].tmax ne ndv) and $
                           (daily_GHCN_data[ind].tmax gt -700) and $
                           (daily_GHCN_data[ind].tmax lt 700) and $
                           ((daily_GHCN_data[ind].tmax_mflag eq '') or $
                            (daily_GHCN_data[ind].tmax_mflag eq 'H')) and $
                           (daily_GHCN_data[ind].tmax_qflag eq ''), $
                           count_tmid)

          if (count_tmid gt 0) then begin
              prcp_tmid = [prcp_tmid, daily_GHCN_data[ind[ind_tmid]].prcp]
              snow_tmid = [snow_tmid, daily_GHCN_data[ind[ind_tmid]].snow]
              tmid = [tmid, 0.5 * (daily_GHCN_data[ind[ind_tmid]].tmin + $
                                   daily_GHCN_data[ind[ind_tmid]].tmax)]
          endif
          
      endfor ; for dc = 0, numDays - 1 do begin

  endfor ; for year = startYear, finishYear do begin

  SAVE, ndv, $
        prcp_tmin, snow_tmin, tmin, $
        prcp_tmax, snow_tmax, tmax, $
        prcp_tavg, snow_tavg, tavg, $
        prcp_frz, snow_frz, tavg_frz, tmin_frz, $
        prcp_tmid, snow_tmid, tmid, $
        FILE = savFile
        
SKIP:

  if NOT(fullRun) then begin
      PRINT, 'Restoring results from ' + savFile
      RESTORE, savFile
  endif

  ;; prcp_threshold = 0.01 * 25.4 * 10 ; tenths of mm
  ;; snow_threshold = 0.1 * 25.4 ; mm

  tc_min = -250 ; tenths of deg C
  tc_max = 250  ; tenths of deg C
  tc_step = 1
  num_tc = ROUND((tc_max - tc_min) / tc_step) + 1L
  tc = tc_min + LINDGEN(num_tc) * tc_step

; Do POD/FAR analysis for tmin.

  time_str = startDate_MMDD + '-' + finishDate_MMDD + ', ' + $
             STRCRA(startYear) + ' to ' + STRCRA(finishYear)

  PRINT, 'Targeting POD = ' + STRCRA(POD_target) + ' for ' + time_str

  tair_thresh_name = STRARR(5)
  POD_target_tair_thresh_degC = FLTARR(5)
  POD_target_tair_thresh_degC[*] = FLOAT(ndv)
  POD_target_tair_thresh_FAR = FLTARR(5)
  POD_target_tair_thresh_FAR[*] = FLOAT(ndv)
  FAR_target_tair_thresh_degC = FLTARR(5)
  FAR_target_tair_thresh_degC[*] = FLOAT(ndv)
  FAR_target_tair_thresh_POD = FLTARR(5)
  FAR_target_tair_thresh_POD[*] = FLOAT(ndv)
  CSI_max = FLTARR(5)
  CSI_max[*] = ndv
  CSI_max_tc_degC = FLTARR(5)
  CSI_max_tc_degC[*] = ndv
  CSI_max_POD = FLTARR(5)
  CSI_max_POD[*] = ndv
  CSI_max_FAR = FLTARR(5)
  CSI_max_FAR[*] = ndv

  count_tmin = N_ELEMENTS(tmin)
  if (count_tmin gt 0) then begin
      tair_name = 'Tmin'
      PRINT, tair_name + ' results:'
      PRCP_SNFL_TAIR_POD_FAR, prcp_tmin, $
                              snow_tmin, $
                              tmin, $
                              tc, $
                              POD_target, $
                              FAR_target, $
                              ndv, $
                              tair_name, $
                              time_str, $
                              prcp_threshold, $
                              snow_threshold, $
                              POD_target_tair_thresh, $
                              POD_target_tair_thresh_FAR_, $
                              FAR_target_tair_thresh, $
                              FAR_target_tair_thresh_POD_, $
                              CSI_max_, $
                              CSI_max_tc, $
                              CSI_max_POD_, $
                              CSI_max_FAR_
      if (ISA(POD_target_tair_thresh) and $
          ISA(POD_target_tair_thresh_FAR_)) then begin
          tair_thresh_name[0] = tair_name
          POD_target_tair_thresh_degC[0] = POD_target_tair_thresh / 10.0
          POD_target_tair_thresh_FAR[0] = POD_target_tair_thresh_FAR_
      endif
      if (ISA(FAR_target_tair_thresh) and $
          ISA(FAR_target_tair_thresh_POD_)) then begin
          FAR_target_tair_thresh_degC[0] = FAR_target_tair_thresh / 10.0
          FAR_target_tair_thresh_POD[0] = FAR_target_tair_thresh_POD_
      endif
      if (ISA(CSI_max_) and ISA(CSI_max_tc) and $
          ISA(CSI_max_POD_) and ISA(CSI_max_FAR_)) then begin
          CSI_max[0] = CSI_max_
          CSI_max_tc_degC[0] = CSI_max_tc / 10.0
          CSI_max_POD[0] = CSI_max_POD_
          CSI_max_FAR[0] = CSI_max_FAR_
      endif
      move = GET_KBRD(1)
  endif

  count_tmax = N_ELEMENTS(tmax)
  if (count_tmax gt 0) then begin
      tair_name = 'Tmax'
      PRINT, tair_name + ' results:'
      PRCP_SNFL_TAIR_POD_FAR, prcp_tmax, $
                              snow_tmax, $
                              tmax, $
                              tc, $
                              POD_target, $
                              FAR_target, $
                              ndv, $
                              tair_name, $
                              time_str, $
                              prcp_threshold, $
                              snow_threshold, $
                              POD_target_tair_thresh, $
                              POD_target_tair_thresh_FAR_, $
                              FAR_target_tair_thresh, $
                              FAR_target_tair_thresh_POD_, $
                              CSI_max_, $
                              CSI_max_tc, $
                              CSI_max_POD_, $
                              CSI_max_FAR_
      if (ISA(POD_target_tair_thresh) and $
          ISA(POD_target_tair_thresh_FAR_)) then begin
          tair_thresh_name[1] = tair_name
          POD_target_tair_thresh_degC[1] = POD_target_tair_thresh / 10.0
          POD_target_tair_thresh_FAR[1] = POD_target_tair_thresh_FAR_
      endif
      if (ISA(FAR_target_tair_thresh) and $
          ISA(FAR_target_tair_thresh_POD_)) then begin
          FAR_target_tair_thresh_degC[1] = FAR_target_tair_thresh / 10.0
          FAR_target_tair_thresh_POD[1] = FAR_target_tair_thresh_POD_
      endif
      if (ISA(CSI_max_) and ISA(CSI_max_tc) and $
          ISA(CSI_max_POD_) and ISA(CSI_max_FAR_)) then begin
          CSI_max[1] = CSI_max_
          CSI_max_tc_degC[1] = CSI_max_tc / 10.0
          CSI_max_POD[1] = CSI_max_POD_
          CSI_max_FAR[1] = CSI_max_FAR_
      endif
      move = GET_KBRD(1)
  endif

  count_tavg = N_ELEMENTS(tavg)
  if (count_tavg gt 0) then begin
      tair_name = 'Tavg'
      PRINT, tair_name + ' results:'
      PRCP_SNFL_TAIR_POD_FAR, prcp_tavg, $
                              snow_tavg, $
                              tavg, $
                              tc, $
                              POD_target, $
                              FAR_target, $
                              ndv, $
                              tair_name, $
                              time_str, $
                              prcp_threshold, $
                              snow_threshold, $
                              POD_target_tair_thresh, $
                              POD_target_tair_thresh_FAR_, $
                              FAR_target_tair_thresh, $
                              FAR_target_tair_thresh_POD_, $
                              CSI_max_, $
                              CSI_max_tc, $
                              CSI_max_POD_, $
                              CSI_max_FAR_
      if (ISA(POD_target_tair_thresh) and $
          ISA(POD_target_tair_thresh_FAR_)) then begin
          tair_thresh_name[2] = tair_name
          POD_target_tair_thresh_degC[2] = POD_target_tair_thresh / 10.0
          POD_target_tair_thresh_FAR[2] = POD_target_tair_thresh_FAR_
      endif
      if (ISA(FAR_target_tair_thresh) and $
          ISA(FAR_target_tair_thresh_POD_)) then begin
          FAR_target_tair_thresh_degC[2] = FAR_target_tair_thresh / 10.0
          FAR_target_tair_thresh_POD[2] = FAR_target_tair_thresh_POD_
      endif
      if (ISA(CSI_max_) and ISA(CSI_max_tc) and $
          ISA(CSI_max_POD_) and ISA(CSI_max_FAR_)) then begin
          CSI_max[2] = CSI_max_
          CSI_max_tc_degC[2] = CSI_max_tc / 10.0
          CSI_max_POD[2] = CSI_max_POD_
          CSI_max_FAR[2] = CSI_max_FAR_
      endif
      move = GET_KBRD(1)
  endif

  count_frz = N_ELEMENTS(tavg_frz)
  if (count_frz gt 0) then begin
      tair_name = 'Tavg (freezing)'
      PRINT, tair_name + ' results:'
      PRCP_SNFL_TAIR_POD_FAR, prcp_frz, $
                              snow_frz, $
                              tavg_frz, $
                              tc, $
                              POD_target, $
                              FAR_target, $
                              ndv, $
                              tair_name, $
                              time_str, $
                              prcp_threshold, $
                              snow_threshold, $
                              POD_target_tair_thresh, $
                              POD_target_tair_thresh_FAR_, $
                              FAR_target_tair_thresh, $
                              FAR_target_tair_thresh_POD_, $
                              CSI_max_, $
                              CSI_max_tc, $
                              CSI_max_POD_, $
                              CSI_max_FAR__, $
                              T_COLD = tmin_frz, CUTOFF_T_COLD = 0
      if (ISA(POD_target_tair_thresh) and $
          ISA(POD_target_tair_thresh_FAR_)) then begin
          tair_thresh_name[3] = tair_name
          POD_target_tair_thresh_degC[3] = POD_target_tair_thresh / 10.0
          POD_target_tair_thresh_FAR[3] = POD_target_tair_thresh_FAR_
      endif
      if (ISA(FAR_target_tair_thresh) and $
          ISA(FAR_target_tair_thresh_POD_)) then begin
          FAR_target_tair_thresh_degC[3] = FAR_target_tair_thresh / 10.0
          FAR_target_tair_thresh_POD[3] = FAR_target_tair_thresh_POD_
      endif
      if (ISA(CSI_max_) and ISA(CSI_max_tc) and $
          ISA(CSI_max_POD_) and ISA(CSI_max_FAR_)) then begin
          CSI_max[3] = CSI_max_
          CSI_max_tc_degC[3] = CSI_max_tc / 10.0
          CSI_max_POD[3] = CSI_max_POD_
          CSI_max_FAR[3] = CSI_max_FAR_
      endif
      move = GET_KBRD(1)
  endif

  count_tmid = N_ELEMENTS(tmid)
  if (count_tmid gt 0) then begin
      tair_name = 'Tmid'
      PRINT, tair_name + ' results:'
      PRCP_SNFL_TAIR_POD_FAR, prcp_tmid, $
                              snow_tmid, $
                              tmid, $
                              tc, $
                              POD_target, $
                              FAR_target, $
                              ndv, $
                              tair_name, $
                              time_str, $
                              prcp_threshold, $
                              snow_threshold, $
                              POD_target_tair_thresh, $
                              POD_target_tair_thresh_FAR_, $
                              FAR_target_tair_thresh, $
                              FAR_target_tair_thresh_POD_, $
                              CSI_max_, $
                              CSI_max_tc, $
                              CSI_max_POD_, $
                              CSI_max_FAR_
      if (ISA(POD_target_tair_thresh) and $
          ISA(POD_target_tair_thresh_FAR_)) then begin
          tair_thresh_name[4] = tair_name
          POD_target_tair_thresh_degC[4] = POD_target_tair_thresh / 10.0
          POD_target_tair_thresh_FAR[4] = POD_target_tair_thresh_FAR_
      endif
      if (ISA(FAR_target_tair_thresh) and $
          ISA(FAR_target_tair_thresh_POD_)) then begin
          FAR_target_tair_thresh_degC[4] = FAR_target_tair_thresh / 10.0
          FAR_target_tair_thresh_POD[4] = FAR_target_tair_thresh_POD_
      endif
      if (ISA(CSI_max_) and ISA(CSI_max_tc) and $
          ISA(CSI_max_POD_) and ISA(CSI_max_FAR_)) then begin
          CSI_max[4] = CSI_max_
          CSI_max_tc_degC[4] = CSI_max_tc / 10.0
          CSI_max_POD[4] = CSI_max_POD_
          CSI_max_FAR[4] = CSI_max_FAR_
      endif
  endif
  
end


  prcp_threshold = 0.01 * 25.4 * 10 ; tenths of mm
  snow_threshold = 0.1 * 25.4 ; mm
  POD_target = 0.95 
  FAR_target = 0.10

  startYear =  2005
  finishYear = 2019

  all_rep_date_Julian = []

  all_POD_target_tair_thresh_degC = []
  all_POD_target_tair_thresh_FAR = []

  all_CSI_max = []
  all_CSI_max_tc_degC = []
  all_CSI_max_POD = []
  all_CSI_max_FAR = []

  startDate_MMDD = '0921'
  finishDate_MMDD = '1011'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]

  startDate_MMDD = '1001'
  finishDate_MMDD = '1021'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]

  startDate_MMDD = '1011'
  finishDate_MMDD = '1031'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str
  move = GET_KBRD(1)

  startDate_MMDD = '1021'
  finishDate_MMDD = '1110'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str
  move = GET_KBRD(1)

  startDate_MMDD = '1031'
  finishDate_MMDD = '1120'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str
  move = GET_KBRD(1)

  startDate_MMDD = '1110'
  finishDate_MMDD = '1130'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str
  move = GET_KBRD(1)

  startDate_MMDD = '1120'
  finishDate_MMDD = '1210'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str
  move = GET_KBRD(1)

  startDate_MMDD = '1130'
  finishDate_MMDD = '1220'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str
  move = GET_KBRD(1)

  startDate_MMDD = '1210'
  finishDate_MMDD = '1230'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str
  move = GET_KBRD(1)

  startDate_MMDD = '1220'
  finishDate_MMDD = '0109'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '1230'
  finishDate_MMDD = '0119'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2021, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  ;; stop
  
  startYear = 2006
  finishYear = 2020

  startDate_MMDD = '0109'
  finishDate_MMDD = '0129'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0119'
  finishDate_MMDD = '0208'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0129'
  finishDate_MMDD = '0218'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0208'
  finishDate_MMDD = '0228'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0218'
  finishDate_MMDD = '0310'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0228'
  finishDate_MMDD = '0320'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0310'
  finishDate_MMDD = '0330'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0320'
  finishDate_MMDD = '0409'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0330'
  finishDate_MMDD = '0419'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0409'
  finishDate_MMDD = '0429'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str

  startDate_MMDD = '0419'
  finishDate_MMDD = '0509'
  SNFL_PRCP_TAIR_STATS, startYear, $
                        finishYear, $
                        startDate_MMDD, $
                        finishDate_MMDD, $
                        prcp_threshold, $
                        snow_threshold, $
                        POD_target, $
                        FAR_target, $
                        tair_thresh_name, $
                        POD_target_tair_thresh_degC, $
                        POD_target_tair_thresh_FAR, $
                        FAR_target_tair_thresh_degC, $
                        FAR_target_tair_thresh_POD, $
                        CSI_max, $
                        CSI_max_tc_degC, $
                        CSI_max_POD, $
                        CSI_max_FAR
  rep_date_Julian = REP_DATE(2022, startDate_MMDD, finishDate_MMDD)
  all_POD_target_tair_thresh_degC = $
      [[all_POD_target_tair_thresh_degC], [POD_target_tair_thresh_degC]]
  all_POD_target_tair_thresh_FAR = $
      [[all_POD_target_tair_thresh_FAR], [POD_target_tair_thresh_FAR]]
  all_CSI_max = [[all_CSI_max], [CSI_max]]
  all_CSI_max_tc_degC = [[all_CSI_max_tc_degC], [CSI_max_tc_degC]]
  all_CSI_max_POD = [[all_CSI_max_POD], [CSI_max_POD]]
  all_CSI_max_FAR = [[all_CSI_max_FAR], [CSI_max_FAR]]
  all_rep_date_Julian = $
      [all_rep_date_Julian, rep_date_Julian]
  time_str = STRCRA(startYear) + ' to ' + STRCRA(finishYear)
  PLOT_SNFL_PRCP_TAIR_STATS, all_rep_date_Julian, $
                             tair_thresh_name, $
                             all_POD_target_tair_thresh_degC, $
                             all_POD_target_tair_thresh_FAR, $
                             all_CSI_max, $
                             all_CSI_max_tc_degC, $
                             all_CSI_max_POD, $
                             all_CSI_max_FAR, $
                             time_str	
end
