FUNCTION REMOVE_NEGATIVE_KRIGING_WEIGHT, weight, $
                                         covar, $
                                         DEUTSCH = Deutsch

; Eliminate negative kriging weights.

  newWeight = weight
  nInd = WHERE(weight lt 0.0, nCount)

  if KEYWORD_SET(Deutsch) then begin


;     Deutsch, Clayton V., Correcting for negative weights in
;     ordinary kriging, Computers and Geosciences Vol. 22, No. 7,
;     pp. 765-773, 1996.

      if (nCount gt 0) then begin
          meanAbsNegWt = MEAN(ABS(weight[nInd]))
          meanCovNegWt = MEAN(covar[nInd])
          newWeight[nInd] = 0.0
          ind = WHERE((weight gt 0.0) and $
                      (weight lt meanAbsNegWt) and $
                      (covar lt meanCovNegWt), $
                      count)
          if (count gt 0) then newWeight[ind] = 0.0
      endif

  endif else begin


;     Progressively compensate for negative weights, reducing positive
;     weights for the lowest covariances, provided those covariances
;     are lower than that associated with the negative weight being
;     eliminated.

      for nc = 0, nCount - 1 do begin

          k = nInd[nc]          ; simplify index

          posWeightToRemove = -weight[k] ; amount of weight to compensate
          newWeight[k] = 0.0

          pInd = WHERE((newWeight gt 0.0), pCount)
          if (pCount eq 0) then STOP ; PROGRAMMING ERROR

          while (posWeightToRemove gt 0.0) do begin

              mncvpswt = MIN(covar[pInd], pIndInd) ; lowest covariance among
                                                   ; positive weights

              if (mncvpswt gt covar[k]) then BREAK

              kk = pInd[pIndInd] ; simplify index

              weightLoss = posWeightToRemove < newWeight[kk]
              newWeight[kk] = newWeight[kk] - weightLoss
              posWeightToRemove = posWeightToRemove - weightLoss
              pInd = WHERE((newWeight gt 0.0), pCount)
              if (pCount eq 0) then STOP ; PROGRAMMING ERROR

          endwhile

      endfor

  endelse

  newWeight = newWeight / TOTAL(newWeight)

  RETURN, newWeight

end
