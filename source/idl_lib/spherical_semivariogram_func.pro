FUNCTION SPHERICAL_SEMIVARIOGRAM_FUNC, lag, params

; Procedure to compute a spherical semivariogram, designed to be used
; as a general-purpose function.
;
; Parameters:
;
;            y-axis (semivariogram)
;     sill __|    __________________
;            |   /
;            |  / |
;            | /
;   nugget __|/   |    
;            |  
;            |____|_______________________ x-axis (lag distance)
;                 V
;               range
;
; The calling function should initialize params = [nugget, sill, range]
;
; Simple naming convention: g = semivariance, h = lag;
; n = nugget, s = sill, r = range,
;
; Spherical semivariogram model:
;
;   g = n + (s - n) * ((3 * h) / (2 * r) - h^3 / (2 * r^3))
;
; Where the first term only applies when h > 0
; and the second term only applies when h < r
; and g = s when h >= r

  COMMON info, Message

  semivariance = !NULL

  nugget = params[0] ; n
  sill = params[1]   ; s
  range = params[2]  ; r

  if (range eq 0.0) then begin
      ERR_MSG, 'Invalid variogram range ' + STRCRA(range) + '.'
  endif else begin
      if (MIN(lag) lt 0.0) then $
          ERR_MSG, 'Negative lags are not allowed.' $
      else begin
          ;; semivariance = nugget + $
          ;;                (sill - nugget) * (lag lt range) * $
          ;;                (1.5D * lag / range - $
          ;;                 0.5D * (lag / range)^3.0D) + $
          ;;                (sill - nugget) * (lag ge range)
          r = (lag / range) < 1.0D
          semivariance = nugget + $
                         (sill - nugget) * $
                         (1.5D * r - 0.5d * r^3.0D)
          semivariance[WHERE(r eq 0.0D, /NULL)] = 0.0D
      endelse
  endelse

  RETURN, semivariance

end

