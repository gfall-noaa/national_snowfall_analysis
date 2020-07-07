FUNCTION RANDOM_UNIQ_IND, numVals

  hit = BYTARR(numVals) ; flag for whether placed yet
  k = LONARR(numVals)   ; randomized order index
  count = 0L
  allCount = 0L
  while (count lt numVals) do begin
;  while (TOTAL(hit, /INT) ne numVals) do begin
    ind = ROUND(RANDOMU(seed) * (numVals - 1))
    if NOT(hit[ind]) then begin
      k[count] = ind
      hit[ind] = 1B
      count++
    endif
    allCount++
  endwhile

  RETURN, k

end
