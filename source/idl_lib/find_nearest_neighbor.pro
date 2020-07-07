PRO FIND_NEAREST_NEIGHBOR, $
    lon, lat, $
    nearestNeighborInd, $       ; index of nearest neighbor
    nearestNeighborDistance, $  ; distance to nearest neighbor (meters)
    DISTANCE_PRECISION = dPrecision, $
    HASH = hash, $
    CHECK_INDEX = checkIndex    ; index of lon and lat to consider: if
                                ; included then nearest neighbor
                                ; calculation is done only for these
                                ; elements of lon and lat

; Given a set of points (1-d arrays lon and lat), find the nearest neighbor of
; each.

; This procedure is probably very slow compared to a k-d tree, but I do not
; want to learn to do k-d tree.

; Greg Fall, NOHRSC
; 2014-04-01

  if NOT(KEYWORD_SET(dPrecision)) then dPrecision = 2

  foo = SIZE(lon)
  bar = SIZE(lat)
  if ((foo[0] ne 1) or (bar[0] ne 1) or (bar[1] ne foo[1])) then $
    MESSAGE, 'Input arrays must match and be 1-d lists of longitudes and ' + $
             'latitudes.'
  numPoints = N_ELEMENTS(lon)


; Determine degrees longitude and latitude to m for all latitudes. Meters per
; degree latitude increases slightly with latitude. Meters per degree
; longitude decreases dramatically with latitude.

  minLat = DOUBLE(FLOOR(MIN(ABS(lat))))
  maxLat = DOUBLE(CEIL(MAX(ABS(lat))))
  latRef = DINDGEN(181) / 2.0
  mPerDegLonRef = DISTANCE(dPrecision, 0.0D, latRef, 1.0D, latRef)


; Use near-equator meters-per-degree ratio (the smallest) at all latitudes.

  mPerDegLat = $
    DOUBLE(FLOOR(DISTANCE(dPrecision, 0.0D, 0.005D, 0.0D, -0.005D) * 100.0D))

  if (N_ELEMENTS(checkIndex) gt 0) then begin
      if ((MIN(checkIndex) lt 0) or (MAX(checkIndex) ge numPoints)) then $
          MESSAGE, 'CHECK_INDEX keyword indicates out-of-bounds indices.'
  endif else begin
      checkIndex = LINDGEN(numPoints)
  endelse

  numCheck = N_ELEMENTS(checkIndex)
  nearestNeighborInd = LONARR(numCheck)
  nearestNeighborDistance = DBLARR(numCheck)


; Find nearest neighbors.

  if (numCheck lt 50) then begin
      hashes = numCheck
      lastProgress = -1
  endif else begin
      hashes = 50
      lastProgress = 0
  endelse
  if KEYWORD_SET(hash) then begin
      for rc = 0, hashes - 1 do PRINT, FORMAT="($,'#')"
      PRINT, ''
  endif

  for i = 0, numCheck - 1 do begin

      if KEYWORD_SET(hash) then begin
          progress = FIX(FLOAT(i) / (FLOAT(numCheck) - 1.0) * FLOAT(hashes))
          if (progress gt lastProgress) then begin
              lastProgress = progress
              PRINT, FORMAT='($,"#")'
          endif
      endif

      loni = lon[checkIndex[i]]
      lati = lat[checkIndex[i]]

      dLon1 = 0.0D             ; lon search distance deg
      dLat1 = 0.0D             ; lat search distance deg
      nn1 = 0


;     Find the smallest box around site i that has at least one site inside
;     (not including site i).

      while (nn1 lt 1) do begin

          dLon1 = dLon1 + 0.1D
          dLat1 = dLat1 + 0.1D

          ind1 = WHERE((ABS(loni - lon) lt dLon1) and $
                       (ABS(loni - lon) gt 1.0D-8) and $
                       (ABS(lati - lat) lt dLat1) and $
                       (ABS(lati - lat) gt 1.0D-8), nn1)

      endwhile


;     Find closest site inside box, label as "j".

      d1 = DISTANCE(dPrecision, lon[ind1], lat[ind1], loni, lati)
      dMin = MIN(d1, ind)
      j = ind1[ind]


;     It is possible for sites outside the box to be closer than dMin,
;     which is why we are not finished yet. Now we have to search in a
;     second box that has been expanded slightly.


;     Determine degrees longitude associated with the highest latitude we
;     will search for more points.

      dLat2 = dMin / mPerDegLat
      ind = MIN(WHERE(latRef ge (ABS(lati) + dLat2)))
      if (ind eq -1) then STOP ; programming error?
      dLon2 = dMin / mPerDegLonRef[ind]

;     Latitude is tricky. Need to converge on a dLat2 that is *guaranteed* to
;     capture anything less than dMin meters away.
;      ind = MAX(WHERE(latRef le ABS(lati)))
;      if (ind eq -1) then STOP ; programming error?
;       ind2 = MAX(WHERE(latRef le ABS(lati) - dLat2)) ; check "bottom" of box
;       while (ind2 ne ind) do begin
;           ind = ind2
;           dLat2 = dMin / mPerDegLatRef[ind]
;           ind2 = MAX(WHERE(latRef le ABS(lati) - dLat2))
;       endwhile


;     Only search again if at least one dimension of the box grew.

      if ((dLat2 gt dLat1) or $
          (dLon2 gt dLon1)) then begin

          dLat2 = dLat2 > dLat1
          dLon2 = dLon2 > dLon1


;         Make sure box expansion was adequate.

          if (DISTANCE(dPrecision, loni, lati, loni + dlon2, lati) lt dMin) $
          then STOP
          if (DISTANCE(dPrecision, loni, lati, loni - dlon2, lati) lt dMin) $
          then STOP
          if (DISTANCE(dPrecision, loni, lati, loni, lati + dlat2) lt dMin) $
          then STOP
          if (DISTANCE(dPrecision, loni, lati, loni, lati - dlat2) lt dMin) $
          then STOP


;         Find closest site in expanded box, avoiding recalculation of
;         distances.

          ind2 = WHERE((ABS(loni - lon) lt dLon2) and $
                       (ABS(loni - lon) gt 1.0D-8) and $
                       (ABS(lati - lat) lt dLat2) and $
                       (ABS(lati - lat) gt 1.0D-8), nn2)

          if (nn2 lt nn1) then STOP ; unpossible; it is a bigger box

          d2 = DBLARR(nn2)

          if (nn2 gt nn1) then begin ; expanded box caught some sites
              for k = 0, nn2 - 1 do begin
                  ind = WHERE(ind1 eq ind2[k], count)
                  if (count eq 1) then $
                    d2[k] = d1[ind[0]] $
                  else $
                    d2[k] = DISTANCE(dPrecision, $
                                     lon[ind2[k]], lat[ind2[k]], loni, lati)
              endfor
              olddmin = dmin
              dMin = MIN(d2, ind)
              if (ind2[ind] ne j) then j = ind2[ind]
          endif 
      endif

      nearestNeighborInd[i] = j
      nearestNeighborDistance[i] = dMin

  endfor

  if KEYWORD_SET(hash) then PRINT, ''

  RETURN

end
