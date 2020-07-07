FUNCTION GET_QUANTILE, dataIn, quantile

; Calculate quantile values from dataIn.

  numQuant = N_ELEMENTS(quantile)

  ind = WHERE((quantile le 0.0) and (quantile ge 1.0), count)
  if (count gt 0) then MESSAGE, 'Quantile values must be between 0 and 1.'

  dataInLocal = dataIn ; make a local copy of the data so we can rearrange it

  order = SORT(dataInLocal)
  dataInLocal = dataInLocal[order]
  numData = N_ELEMENTS(dataInLocal)

  dataQuant = FLTARR(numQuant)

  for qc = 0, numQuant - 1 do begin


;     Quantile values range from 0.0 for the first value in the
;     (sorted) array to 1.0 for the last (numData - 1) value in the
;     (sorted) array.

      ind1 = FLOOR(quantile[qc] * (numData - 1))
      ind2 = CEIL(quantile[qc] * (numData - 1))

      quant1 = FLOAT(ind1) / FLOAT(numData - 1)
      quant2 = FLOAT(ind2) / FLOAT(numData - 1)

      if (quant1 eq quantile[qc]) then begin
          dataQuant[qc] = dataInLocal[ind1]
          CONTINUE
      endif

      if (quant2 eq quantile[qc]) then begin
          dataQuant[qc] = dataInLocal[ind2]
          CONTINUE
      endif

      if (dataInLocal[ind2] eq dataInLocal[ind1]) then begin
          dataQuant[qc] = dataInLocal[ind1]
          CONTINUE
      endif

      dataQuant[qc] = dataInLocal[ind1] + $
                      (quant2 - quant1) / $
                      (dataInLocal[ind2] - dataInLocal[ind1]) * $
                      (quantile[qc] - quant1)

  endfor

  RETURN, dataQuant

end
