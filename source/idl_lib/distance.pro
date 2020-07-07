;*****************************************************************************
;*
;*  DISTANCE
;*
;*  This function calculates the distance between two points using one of
;*  several methods. 
;*
;*  USAGE:
;*
;*  dist = distance ( precision, 
;*                    longitude_1,
;*                    latitude_1,
;*                    longitude_2,
;*                    latitude_2   )
;*
;*   Name            Description
;*  =======================================
;*
;*  precision    >  0 : Very crude Eulidean
;*                  1 : Crude Euclidean
;*                  2 : Great circle
;*                  3 : Ellipsoidal
;*  longitude_1  >  Longitude
;*  latitude_1   >  Latitude
;*  longitude_2  >  Longitude
;*  latitude_2   >  Latitude
;*
;*  History:
;*  Anders Nilsson, VV,   07/29/04, Taken from GIS/RS distance function
;*  Anders Nilsson, VV,   08/05/04, Attempted to optimize
;*  Anders Nilsson, UCAR, 03/23/10, Fixed longitude replication
;*
;****************************************************************************

    FUNCTION DISTANCE, precision, longitude_1, latitude_1, $
                       longitude_2, latitude_2

;** Constants **

    COMMON DIST_CONSTANTS, GC_WGS84_SEMI_MINOR, GC_WGS84_SEMI_MAJOR, $
                           GC_CRUDE_M_PER_DEGREE, GDC_DEG_TO_RAD, $
                           GDC_ECCENTRICITY, GDC_ELLIPSOIDAL, $
                           GDC_FLATTENING, GDC_GREAT_CIRCLE, $
                           GDC_LAT_ARC_TO_LEN, GDC_LON_ARC_TO_LEN, $
                           GDC_RAD_TO_DEG


    if ( n_elements ( GC_WGS84_SEMI_MINOR ) eq 0 ) then $
    begin

       GC_WGS84_SEMI_MINOR = 6356752.31414d
       GC_WGS84_SEMI_MAJOR = 6378137.0d
       GC_CRUDE_M_PER_DEGREE = 98200.0d 
       GDC_DEG_TO_RAD = !DPI / 180.0d 
       GDC_ECCENTRICITY = GC_WGS84_SEMI_MINOR / GC_WGS84_SEMI_MAJOR 
       GDC_ELLIPSOIDAL = 1.0d / $
                         ( GC_WGS84_SEMI_MINOR / GC_WGS84_SEMI_MAJOR ) / $
                         ( GC_WGS84_SEMI_MINOR / GC_WGS84_SEMI_MAJOR ) - $
                         1.0d 
       GDC_FLATTENING  = 1.0d - ( GC_WGS84_SEMI_MINOR / GC_WGS84_SEMI_MAJOR )
       GDC_GREAT_CIRCLE = !DPI * $
                          ( GC_WGS84_SEMI_MAJOR + GC_WGS84_SEMI_MINOR ) / $
                          360.0d
       GDC_LAT_ARC_TO_LEN = !DPI * GC_WGS84_SEMI_MINOR / 180.0d
       GDC_LON_ARC_TO_LEN = !DPI * GC_WGS84_SEMI_MAJOR / 180.0d
       GDC_RAD_TO_DEG = 180.0d / !DPI 

    endif    

;** Figure out common array size **

    length1 = n_elements ( longitude_1 )
    length2 = n_elements ( latitude_1 )
    length3 = n_elements ( longitude_2 )
    length4 = n_elements ( latitude_2 )

    length = length1 > length2 > length3 > length4

    if ( length1 eq length ) then longitude_1i = longitude_1 $
    else longitude_1i = REPLICATE ( longitude_1, length )
    if ( length2 eq length ) then latitude_1i = latitude_1 $
    else latitude_1i = REPLICATE ( latitude_1, length )
    if ( length3 eq length ) then longitude_2i = longitude_2 $
    else longitude_2i = REPLICATE ( longitude_2, length )
    if ( length4 eq length ) then latitude_2i = latitude_2 $
    else latitude_2i = REPLICATE ( latitude_2, length )
    
    if ( length gt 1 ) then returned_dist = DBLARR ( length ) $
    else returned_dist = 0.0d

;** Check for same point **

    test2 = where ( longitude_1i eq longitude_2i and $
                    latitude_1i eq latitude_2i, COMPLEMENT=test1 )
    test2 = 0

;** Process other data **

    if ( test1[0] ne -1 ) then $
    begin
    
       if ( precision[0] eq 0 ) then $
       begin

;**       Very crude Euclidean **

          dlat = latitude_1i[test1] - latitude_2i[test1]
          dlon = longitude_1i[test1] - longitude_2i[test1]

          returned_dist[test1] = sqrt ( dlat * dlat + dlon * dlon ) * $
                                 GC_CRUDE_M_PER_DEGREE
       endif $
       else if ( precision[0] eq 1 ) then $
       begin

;**       Crude Euclidean **

          mean_latitude = ( latitude_1i[test1] + latitude_2i[test1] ) / 2.0d ;
          rad_mean_latitude =  mean_latitude * GDC_DEG_TO_RAD ;

          dlat = abs ( latitude_1i[test1] - latitude_2i[test1] ) * $
                 GDC_LAT_ARC_TO_LEN ;
          dlon = abs ( longitude_1i[test1] - longitude_2i[test1] ) * $
                 GDC_LON_ARC_TO_LEN * cos ( rad_mean_latitude ) ;

          returned_dist[test1] = sqrt ( dlat * dlat + dlon * dlon )

       endif $
       else if ( precision[0] eq 2 ) then $
       begin

;**       Great Circle **

          rad_longitude_1 = longitude_1i[test1] * GDC_DEG_TO_RAD
          rad_latitude_1 = latitude_1i[test1] * GDC_DEG_TO_RAD
          rad_longitude_2 = longitude_2i[test1] * GDC_DEG_TO_RAD
          rad_latitude_2 = latitude_2i[test1] * GDC_DEG_TO_RAD

          dlat = ( rad_latitude_1 - rad_latitude_2 ) / 2.0d
          dlon = ( rad_longitude_1 - rad_longitude_2 ) / 2.0d

          dist = sqrt ( sin ( dlat ) * sin ( dlat ) + $
                        cos ( rad_latitude_1 ) * cos ( rad_latitude_2 ) * $
                        sin ( dlon ) * sin ( dlon ) ) ;

          scratch = 2.0d * asin ( dist )
          dist = scratch * GDC_RAD_TO_DEG

          returned_dist[test1] = dist * GDC_GREAT_CIRCLE

       endif $
       else $
       begin

;**    Ellipsoidal **

          rad_longitude_1 = longitude_1i[test1] * GDC_DEG_TO_RAD
          rad_latitude_1 = latitude_1i[test1] * GDC_DEG_TO_RAD
          rad_longitude_2 = longitude_2i[test1] * GDC_DEG_TO_RAD
          rad_latitude_2 = latitude_2i[test1] * GDC_DEG_TO_RAD

          TU1 = GDC_ECCENTRICITY * sin ( rad_latitude_1 ) / $
                                   cos ( rad_latitude_1 )
          TU2 = GDC_ECCENTRICITY * sin ( rad_latitude_2 ) / $
                                   cos ( rad_latitude_2 )
          CU1 = 1.0d / sqrt ( TU1 * TU1 + 1.0d )
          SU1 = CU1 * TU1
          CU2 = 1.0d / sqrt ( TU2 * TU2 + 1.0d )
          dist = CU1 * CU2
          BAZ = dist * TU2
          FAZ = BAZ * TU1
          X = rad_longitude_2 - rad_longitude_1 

          x_length = n_elements ( X )
          test2 = LINDGEN ( x_length )
          C2A = MAKE_ARRAY ( x_length, /DOUBLE, /NOZERO )
          E = MAKE_ARRAY ( x_length, /DOUBLE, /NOZERO )
          CY = MAKE_ARRAY ( x_length, /DOUBLE, /NOZERO )
          SY = MAKE_ARRAY ( x_length, /DOUBLE, /NOZERO )
          CZ = MAKE_ARRAY ( x_length, /DOUBLE, /NOZERO )
          Y = MAKE_ARRAY ( x_length, /DOUBLE, /NOZERO )

;**       Iterate **

          while ( test2[0] ne -1 ) do $
          begin

             Xi = X[test2]
             FAZi = FAZ[test2]
             SX = sin ( Xi )
             CX = cos ( Xi )
             TU1 = CU2[test2] * SX
             TU2 = BAZ[test2] - SU1[test2] * CU2[test2] * CX
             SYi = sqrt ( TU1 * TU1 + TU2 * TU2 )
             CYi = dist * CX + FAZi
             Yi = atan ( SYi, CYi )
             SA = dist[test2] * SX / SYi
             C2Ai = -SA * SA + 1.0d
             CZi = FAZi + FAZi

             test3 = where ( C2Ai gt 0d )
             if ( test3[0] ne -1 ) then $
             begin
                CZi[test3] = -CZi[test3] / C2Ai[test3] + CYi[test3]
                test3 = 0
             endif
          
             Ei = CZi * CZi * 2.0d - 1.0d
             C = ( ( -3.0d * C2Ai + 4.0d ) * GDC_FLATTENING + 4.0d ) * $
                 C2Ai * GDC_FLATTENING / 16.0d ;
             D = Xi ;
             Xi = ( ( Ei * CYi * C + CZi ) * SYi * C + Yi ) * SA ;
             Xi = ( 1.0d - C ) * Xi * GDC_FLATTENING + $
                        rad_longitude_2[test2] - rad_longitude_1[test2] ;

             test3 = where ( abs ( D - Xi ) gt 0.00000000000005d, $
                             COMPLEMENT=test4 )

;**          Update **

             X[test2] = Xi

             if ( test4[0] ne -1 ) then $
             begin             
                test5 = test2[test4]
                C2A[test5] = C2Ai[test4]
                E[test5] = Ei[test4]
                CY[test5] = CYi[test4]
                SY[test5] = SYi[test4] 
                CZ[test5] = CZi[test4]
                Y[test5] = Yi[test4]
                test4 = 0
                test5 = 0
             endif
             if ( test3[0] ne -1 ) then $
             begin
                test2 = test2[test3] 
                test3 = 0
             endif $
             else test2 = -1

          endwhile

          X = sqrt ( GDC_ELLIPSOIDAL * C2A + 1.0d ) + 1.0d ;
          X = ( X - 2.0d ) / X ;
          C = ( X * X / 4.0d + 1.0d ) / ( 1.0d - X ) ;
          D = ( 0.375d * X * X - 1.0d ) * X ;
          X = E * CY ;
          dist = 1.0d - E - E ;
          returned_dist[test1] = ( ( ( ( SY * SY * 4.0d - 3.0d ) * $
                                       dist * CZ * D / 6.0 - X ) * D / 4.0 + $
                                     CZ ) * SY * D + Y ) * C * $
                                 GC_WGS84_SEMI_MINOR ;

       endelse

    endif

;** Free memory **

    longitude_1i = 0
    latitude_1i = 0
    longitude_2i = 0
    latitude_2i = 0

;** Return to calling routine **

    RETURN, returned_dist


    END


