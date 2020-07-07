FUNCTION REMOVE_KRIGING_WEIGHT_UPSWING, weight, covar, $
                                        PRINT_RESULTS = printResults

; Reduce kriging weights at low covariances (large distances) where a
; significant number of weights for larger covariances (shorter
; distances) are smaller. Re-normalize weights with each adjustment to
; prevent dramatic changes (such as a sign change) in the total of
; weights.

; This procedure was originally intended to eliminate an "upswing" in
; kriging weight at large distances  where many weights for smaller
; distances (higher covariances) are already near zero. It still does
; that.

; TODO: consider possibility of changing the hard-coded threshold for
; the number of lower weight / higher covariance points, currently set
; to 4.

;doPlots = 0
;if (ROUND(RANDOMU(seed) * 5000.0) eq 1) then doPlots = 1

  n = N_ELEMENTS(weight) ; covar must be same size.
  weightTotal = TOTAL(weight) ; should be 1
;  if (TOTAL(weight) ne 1.0) then STOP
  newWeight = weight
  order = SORT(covar)

  ;; if KEYWORD_SET(printResults) then PRINT, '----'
  for nc = 0, n - 2 do begin ; all but highest covariance considered
      k = order[nc]
      ws = newWeight[order[nc+1:*]] ; weights for higher covariances
      ind = WHERE(ws lt newWeight[k], count) ; lower weight, higher covariance
      if (count lt 4) then CONTINUE
;      if KEYWORD_SET(printResults) then begin
;          print, 'weight[k]: ', weight[k]
;          print, 'lower weights at higher covar: ', ws[ind]
;          print, 'mean of those: ', MEAN(ws[ind])
;      endif
;      weightChange = newWeight[k] - weight[k]
;nw1 = newweight
      nwk = MEAN(ws[ind]) ; new weight will be mean of lower values
      weightChange = newWeight[k] - nwk ; size of decrease
if (weightChange le 0.0) then STOP ; PROGRAMMING CHECK
      newWeight[k] = nwk
      newWeightTotal = weightTotal - weightChange  ; new total of weights
;nw2 = newWeight
      newWeight = $
          newWeight / newWeightTotal * weightTotal ; re-normalize weights
      if NOT(COMPARE(TOTAL(newWeight), weightTotal)) then STOP
;      print, nc, k, total(newweight)
  endfor
;  print, 'done'

;if doPlots then begin
;    PLOT, weight[REVERSE(order)], /NODATA
;    OPLOT, weight[REVERSE(order)], PSYM = -4, COLOR = 100
;    OPLOT, newWeight[REVERSE(order)], PSYM = -6
;;    move = GET_KBRD(1)
;endif

  ;if KEYWORD_SET(printResults) then move = GET_KBRD(1)

;  if (TOTAL(newWeight) lt 0.0) then STOP ; RETURN, weight ; abort!

;  newWeight = newWeight / TOTAL(newWeight) ; probably 

  RETURN, newWeight

end
