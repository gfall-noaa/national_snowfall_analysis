; Perform a Grubbs test on the deltas.

FUNCTION GRUBBS_TEST, val, $    ; values to be checked
                      confidence, $
                      STD_DEV_FLOOR = SDFloor, $
                      G_CRITICAL_MIN = GCritMin, $
                      G_CRITICAL_GRUBBS = GCritGrubbs, $
                      SAMPLE_SIZE = n, $
                      MAX_G = maxG

  COMMON info, message

  xx = CHECK_MATH()
  if (xx ne 0) then STOP ; need a clean slate here w/r/t math errors

  if ((confidence le 0.0D) or (confidence ge 1.0D)) then $
      ERR_MSG, 'Confidence level must be between 0 and 1. 0.95 is common.'

  numVals = N_ELEMENTS(val)

  if (numVals lt 3) then $
      MESSAGE, 'At least three values are required.'

  alpha = 1.0D - confidence     ; significance level

  outlierFlag = BYTARR(numVals)

  foundOutlier = 1

  while foundOutlier do begin

      ind = WHERE(outlierFlag eq 0B, count)

      if (count eq 0) then BREAK

      if (count lt 3) then begin
          MESSAGE, 'Grubbs test failed. All points flagged.', /CONTINUE
          outlierFlag[ind] = 1B
          BREAK
      endif

      n = DOUBLE(count)

;     Calculate the upper critical value of the t distribution having
;     n - 2 degrees of freedom, such that the probability of a random
;     variable being greater than this value is equal to alpha / n.

      cutoff = T_CVF(alpha / (2.0D * n), n - 2.0D)
      xx = CHECK_MATH()
      if (xx ne 0) then begin
          if (xx ne 32) then begin
          endif                 ; 32 = underflow is common
      endif
      GCritGrubbs = (n - 1.0D) / SQRT(n) * $
                    SQRT(cutoff^2.0D / (n - 2.0D + cutoff^2.0D))
      xx = CHECK_MATH()
      if (xx ne 0) then STOP

      if KEYWORD_SET(GCritMin) then begin
          GCritGrubbs = GCritMin > GCritGrubbs
      endif

      meanVal = MEAN(val, /DOUBLE)
      stdDevVal = STDDEV(val, /DOUBLE)
      if KEYWORD_SET(SDFloor) then stdDevVal = stdDevVal > SDFloor
      G = ABS((val[ind] - meanVal) / stdDevVal)
      maxG = MAX(G, maxInd)
      if (maxG gt GCritGrubbs) then begin
          outlierFlag[ind[maxInd]] = 1B
          foundOutlier = 1
      endif else foundOutlier = 0

  endwhile

  RETURN, outlierFlag

end
