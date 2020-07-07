PRO LCC_GRIB2_TO_SNYDER, latSec1, $  ; IN - 1st secant lat, deg ("Latin1")
                         latSec2, $  ; IN - 2nd secant lat, deg ("Latin2")
                         latD, $     ; IN - true scale lat, deg ("LatD")
                         lonV, $     ; IN - orientation lon, deg ("LoV")
                         lat00, $    ; IN - grid 0,0 center lat, deg ("Lat1")
                         lon00, $    ; IN - grid 0,0 center lon, deg ("Lon1")
                         eRadM, $    ; IN - earth radius, meters
                         lonV_rad, $ ; OUT - orientation lon, rad
                         nSny, $     ; OUT - Snyder "n"
                         FSny, $     ; OUT - Snyder "F"
                         rho0, $     ; OUT - Snyder "rho-naught"
                         x00, $      ; OUT - grid 0,0 center x
                         y00         ; OUT - grid 0,0 center y


; Take projection and grid parameters for Lambert Conformal Conic data
; as typically provided by a GRIB inventory (i.e., wgrib and wgrib2
; "-grid" output), and convert them to parameters defined in chapter
; 15 of Map Projections--A Working Manual (John P. Snyder).


; Convert input arguments to radians.

  degToRad = !DPi / 180.0D

  latSec1_rad = latSec1 * degToRad
  latSec2_rad = latSec2 * degToRad
  latD_rad = latD * degToRad
  lonV_rad = lonV * degToRad
  lat00_rad = lat00 * degToRad
  lon00_rad = lon00 * degToRad


; Compute Snyder parameters.

  tan1 = TAN(!DPi / 4.0D + latSec1_rad / 2.0D)
  tan2 = TAN(!DPi / 4.0D + latSec2_rad / 2.0D)
  tanD = TAN(!DPi / 4.0D + latD_rad / 2.0D)
  tan00 = TAN(!DPi / 4.0D + lat00_rad / 2.0D)

  if (latSec1_rad eq latSec2_rad) then $
      nSny = SIN(latSec1_rad) $
  else $
      nSny = ALOG(COS(latSec1_rad) / COS(latSec2_rad)) / $ ; Snyder eq. 15-3
             ALOG(tan2 / tan1)

  FSny = COS(latSec1_rad) * tan1^nSny / nSny                          ; 15-2
  rho0 = eRadM * FSny / tanD^nSny                                     ; 15-1a


; Compute grid 0,0 center (a.k.a. "origin", or "orig", coordinates).

  rho00 = eRadM * FSny / tan00^nSny                                   ; 15-1
  theta00_rad = nSny * (lon00_rad - lonV_rad)                         ; 14-4

  x00 = rho00 * SIN(theta00_rad)                                      ; 14-1
  y00 = rho0 - rho00 * COS(theta00_rad)                               ; 14-2

  RETURN

end
