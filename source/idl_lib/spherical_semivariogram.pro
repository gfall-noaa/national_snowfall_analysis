PRO SPHERICAL_SEMIVARIOGRAM, lag, params, semivariance, dgdp

; Procedure to compute a spherical semivariogram, designed to be used
; with the IDL CURVEFIT and MPCURVEFIT functions.
;
; This procedure needs to be called with the /NODERIVATIVES option if
; called by CURVEFIT.
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

  nugget = params[0] ; n
  sill = params[1]   ; s
  range = params[2]  ; r
 
  ind = WHERE(lag le 0.0, count)
  if (count gt 0) then MESSAGE, 'Zero and negative lags are not allowed.'

  semivariance = DBLARR(N_ELEMENTS(lag))
  dgdn = semivariance
  dgds = semivariance
  dgdr = semivariance

  ind = WHERE(lag lt range, count)
  if (count gt 0) then begin
      ;; semivariance[ind] = nugget + $
      ;;                     (sill - nugget) * $
      ;;                     (1.5D * lag[ind] / range - $
      ;;                      0.5D * (lag[ind] / range)^3.0D)
      dgds[ind] = (1.5D * lag[ind] / range - $
                   0.5D * (lag[ind] / range)^3.0D)
      dgdn[ind] = 1.0D - dgds[ind]
      semivariance[ind] = nugget + (sill - nugget) * dgds[ind]
      dgdr[ind] = (sill - nugget) * 1.5D * (lag[ind]^3.0D / range^4.0D - $
                                            lag[ind] / range^2.0D)
  endif

  ind = WHERE(lag ge range, count)
  if (count gt 0) then begin
      dgds[ind] = 1.0D
      dgdn[ind] = 0.0D
      dgdr[ind] = 0.0D
      semivariance[ind] = sill
  endif

  dgdp = [[dgdn], [dgds], [dgdr]]

  RETURN

end

