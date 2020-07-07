#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <string.h>
#include <grib2.h>
#include <netcdf.h>
#include <math.h>
#include <values.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

int deconstruct_date ( const char *dateStr,
                       int *year, int *month, int *day,
                       int *hour, int *minute, int *second ) {

  /*************************************************************************
   *                                                                       *
   *  Convert a date/time string of the form YYYY-mm-dd HH:MM:SS into its  *
   *  components.                                                          *
   *                                                                       *
   *  Output value: success/failure flag                                   *
   *                                                                       *
   *  Input argument:                                                      *
   *                                                                       *
   *    dateStr  Date/time string                                          *
   *                                                                       *
   *  Output arguments:                                                    *
   *                                  
                                     *
   *    Extracted year, month, day, hour, minute, second.                  *
   *                                                                       *
   *  Greg Fall, NOHRSC, 2011-03-24                                        *
   *                                                                       *
   *************************************************************************/

   regex_t compRegEx ;    // Compiled regular expression
   size_t  stringLength ; // String length
   char    text[256] ;    // Multi-purpose string

   const char funcName[] = "deconstruct_date" ;

/* Confirm length of dateStr */

   stringLength = strlen ( dateStr ) ;

   if ( stringLength != 19 ) {
     fprintf ( stderr,
               "%s: input date \"%s\" has length %d; expecting 19.\n",
               funcName, dateStr, (int) stringLength ) ;
     return ( 0 ) ;
   }

/* Confirm puncuation in dateStr */

   if ( ( dateStr[4] != '-') ||
        ( dateStr[7] != '-') ||
        ( dateStr[10] != ' ') ||
        ( dateStr[13] != ':') ||
        ( dateStr[16] != ':') ) {
     strcpy ( text, "YYYY-mm-dd HH:MM:SS" ) ;
     fprintf ( stderr,
               "%s: input date \"%s\" is not formatted as \"%s\".\n",
               funcName, dateStr, text ) ;
     return ( 0 ) ;
   }

/* Confirm "YYYY" is numeric. */

   strncpy ( text, &dateStr[0], ( size_t ) 4 ) ;
   text[4] = '\0' ;

   if ( regcomp ( &compRegEx, "[0-9]{4}", REG_EXTENDED ) != 0 ) {
     fprintf ( stderr,
               "%s: failed to compile extended regex \"[0-9]{4}\".\n",
               funcName ) ;
     return ( 0 ) ;
   }

   if ( regexec ( &compRegEx, text, 0, NULL, 0 ) != 0 ) {
     fprintf ( stderr,
               "%s: input date \"%s\" has non-numeric \"YYYY\" portion.\n",
               funcName, dateStr ) ;
     return ( 0 ) ;
   }

   *year = atoi ( text ) ;

/* Confirm "mm" is numeric. */

   strncpy(text, &dateStr[5], (size_t) 2);
   text[2] = '\0' ;

   if ( regcomp ( &compRegEx, "[0-9]{2}", REG_EXTENDED ) != 0 ) {
     fprintf ( stderr,
               "%s: failed to compile extended regex \"[0-9]{2}\".\n",
               funcName ) ;
     return ( 0 ) ;
   }

   if ( regexec ( &compRegEx, text, 0, NULL, 0 ) != 0 ) {
     fprintf ( stderr,
               "%s: input date \"%s\" has non-numeric \"mm\" portion.\n",
               funcName, dateStr ) ;
     return ( 0 ) ;
   }

   *month = atoi ( text ) ;

/* Confirm "dd" is numeric. */

   strncpy ( text, &dateStr[8], ( size_t ) 2 ) ;
   text[2] = '\0' ;

   //if ( regcomp ( &compRegEx, "[0-9]{2}", REG_EXTENDED ) != 0 ) {
   //  fprintf ( stderr,
   //            "%s: failed to compile extended regex \"[0-9]{2}\".\n",
   //            funcName ) ;
   //  return ( 0 ) ;
   //}

   if ( regexec ( &compRegEx, text, 0, NULL, 0 ) != 0 ) {
     fprintf ( stderr,
               "%s: input date \"%s\" has non-numeric \"dd\" portion.\n",
               funcName, dateStr ) ;
     return ( 0 ) ;
   }

   *day = atoi(text);

/* Confirm "HH" is numeric. */

   strncpy ( text, &dateStr[11], ( size_t ) 2 ) ;
   text[2] = '\0' ;

   //if ( regcomp ( &compRegEx, "[0-9]{2}", REG_EXTENDED ) != 0 ) {
   //  fprintf ( stderr,
   //            "%s: failed to compile extended regex \"[0-9]{2}\".\n",
   //            funcName ) ;
   //  return ( 0 ) ;
   //}

   if ( regexec ( &compRegEx, text, 0, NULL, 0 ) != 0 ) {
     fprintf ( stderr,
               "%s: input date \"%s\" has non-numeric \"HH\" portion.\n",
               funcName, dateStr ) ;
     return ( 0 ) ;
   }

   *hour = atoi ( text ) ;

/* Confirm "MM" is numeric. */

   strncpy ( text, &dateStr[14], ( size_t ) 2 ) ;
   text[2] = '\0' ;

   //if ( regcomp ( &compRegEx, "[0-9]{2}", REG_EXTENDED ) != 0 ) {
   //  fprintf ( stderr,
   //            "%s: failed to compile extended regex \"[0-9]{2}\".\n",
   //            funcName ) ;
   //  return ( 0 ) ;
   //}

   if ( regexec ( &compRegEx, text, 0, NULL, 0 ) != 0 ) {
     fprintf ( stderr,
               "%s: input date \"%s\" has non-numeric \"MM\" portion.\n",
               funcName, dateStr ) ;
     return ( 0 ) ;
   }

   *minute = atoi ( text ) ;

/* Confirm "SS" is numeric. */

   strncpy ( text, &dateStr[17], ( size_t ) 2 ) ;
   text[2] = '\0' ;

   //if ( regcomp ( &compRegEx, "[0-9]{2}", REG_EXTENDED ) != 0 ) {
   //  fprintf ( stderr,
   //            "%s: failed to compile extended regex \"[0-9]{2}\".\n",
   //            funcName ) ;
   //  return ( 0 ) ;
   //}

   if ( regexec ( &compRegEx, text, 0, NULL, 0 ) != 0 ) {
     fprintf ( stderr,
               "%s: input date \"%s\" has non-numeric \"SS\" portion.\n",
               funcName, dateStr ) ;
     return ( 0 ) ;
   }

   *second = atoi ( text ) ;

   return ( 1 ) ;

}


time_t datetime_to_epoch_utc ( int year, int month, int day,
                               int hour, int minute, int second ) {

  /*************************************************************************
   *                                                                       *
   *  Convert a deconstructed UTC date/time to an epoch time.              *
   *                                                                       *
   *  Output value: UTC calendar time, seconds elapsed since Unix epoch.   *
   *                Returns -1 to indicate an error.                       *
   *                                                                       *
   *  Input arguments: UTC year, month, day, hour, minute, second.         *
   *                                                                       *
   *  NOTES:                                                               *
   *                                                                       *
   *  What makes this function special and necessary is its manipulation   *
   *  of the value returned by mktime (which is local) into a GMT result.  *
   *  Allows us to work exclusively in GMT, which is not the Unix way.     *
   *                                                                       *
   *  WARNING: This function treats time_t as an arithmetic type (POSIX).  *
   *                                                                       *
   *  Greg Fall, NOHRSC, 2011-03-24                                        *
   *                                                                       *
   *************************************************************************/

   int       epochDay = 1 ;     // Epoch day
   int       epochHour = 0 ;    // Epoch hour
   int       epochMinute = 0 ;  // Epoch minute
   int       epochMonth = 1 ;   // Epoch month
   int       epochSecond = 0 ;  // Epoch second
   int       epochYear = 1970 ; // Epoch year

   struct tm epochTimeStruct ;  // Broken-down Unix epoch time
   struct tm inputTimeStruct ;  // Broken-down input time

   time_t    inputTimeLocal ;   // Local calendar value of input time
   time_t    inputTimeUTC ;     // UTC value of input time
   time_t    tzOffset ;         // Local time zone offset from UTC

   const char funcName[] = "datetime_to_epoch_utc" ;

/* Get the difference between the Unix epoch and the same time locally. */

   epochTimeStruct.tm_sec = epochSecond ;
   epochTimeStruct.tm_min = epochMinute ;
   epochTimeStruct.tm_hour = epochHour ;
   epochTimeStruct.tm_mday = epochDay ;
   epochTimeStruct.tm_mon = epochMonth - 1 ;
   epochTimeStruct.tm_year = epochYear - 1900 ;
   epochTimeStruct.tm_isdst = -1 ;

   tzOffset = mktime ( &epochTimeStruct ) ;

   if ( tzOffset == -1 ) {
     fprintf ( stderr,
               "%s: mktime failed for \"local\" epoch time.\n",
               funcName ) ;
     return ( -1 ) ;
   }

/* Get the local version of the input UTC and convert by epoch offset. */

   inputTimeStruct.tm_sec = second ;
   inputTimeStruct.tm_min = minute ;
   inputTimeStruct.tm_hour = hour ;
   inputTimeStruct.tm_mday = day ;
   inputTimeStruct.tm_mon = month - 1 ;
   inputTimeStruct.tm_year = year - 1900 ;
   inputTimeStruct.tm_isdst = -1 ;

   inputTimeLocal = mktime ( &inputTimeStruct ) ;

   if ( inputTimeLocal == -1 ) {
     fprintf ( stderr,
               "%s: mktime failed for input time.\n",
               funcName ) ;
     return ( -1 ) ;
   }

/* Time zone adjustment. */

   inputTimeUTC = inputTimeLocal - tzOffset ;

/* Daylight saving adjustment. */

   if ( inputTimeStruct.tm_isdst ) inputTimeUTC += ( time_t ) 3600 ;

   return ( inputTimeUTC ) ;

}

/**************************************************/

int nc_get_att_char_or_string(int ncid,
                              int varID,
                              const char *attName,
                              const char *varName,
                              char *attValue
                              ) {

   size_t      attLength;
   nc_type     attType;
   const char  funcName[] = "nc_get_att_char_or_string";
   char       *ncstrAttValue;
   int         status;

   status = nc_inq_att(ncid, varID, attName, &attType, &attLength);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Inquire failed for \"%s\" attribute for variable "
             "\"%s\".\n",
             funcName, attName, varName);
     return(-1);
   }

   if ((attType != NC_CHAR) && (attType != NC_STRING)) {
     fprintf(stderr,
             "%s: \"%s\" attribute for variable \"%s\" is neither "
             "NC_CHAR nor NC_STRING.\n",
             funcName, attName, varName);
     return(-1);
   }

   if (attType == NC_CHAR) {

     status = nc_get_att_text(ncid, varID, attName, attValue);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Failed to get NC_CHAR \"%s\" attribute for "
               "variable \"%s\".\n",
               funcName, attName, varName);
       return(-1);
     }

     attValue[attLength] = '\0';

   }

   if (attType == NC_STRING) {

     status = nc_get_att_string(ncid, varID, attName, &ncstrAttValue);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Failed to get NC_STRING \"%s\" attribute for "
               "variable \"%s\".\n",
               funcName, attName, varName);
       return(-1);
     }

     strcpy(attValue, ncstrAttValue);

     status = nc_free_string(attLength, &ncstrAttValue);
     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Failed to free NC_STRING for \"%s\" attribute for "
               "variable \"%s\".\n",
               funcName, attName, varName);
       return(-1);
     }

   }

   return(NC_NOERR);

}

/**************************************************/

int gln_inquire_netcdf (const char *ncFilePath,
                        const char *varNameIn,
                        int *numVars,
                        int *numDims,
                        size_t *dimSize_x,
                        size_t *dimSize_y,
                        double *attXRes,
                        double *attYRes,
                        double *attXCornerCtr,
                        double *attYCornerCtr,
                        char *scanningModeX,
                        char *scanningModeY,
                        double *semiMajorAxis,
                        double *invFlattening,
                        double *lonVDeg,
                        double *latDDeg,
                        int *numStdParallels,
                        double **stdParallel,
                        char *varProcessFlag) {

  // double *semiMajorAxis
  // double *invFlattening
  // double *lonVDeg
  // double *latDDeg
  // int *numStdParallels;
  // double *stdParallel;

  /*************************************************************************
   *                                                                       *
   *  Open the requested NetCDF file and confirm it is suitable for        *
   *  conversion to a LCC-projected GRIB2. Identify variables that will    *
   *  be converted to grids in a GRIB2 file.                               *
   *                                                                       *
   *  Output value: ID of open NetCDF file, or -1 if we fail.              *
   *                                                                       *
   *  Input arguments:                                                     *
   *                                                                       *
   *    ncFilePath  NetCDF file path                                       *
   *    varNameIn   Requested NetCDF variable name, or "All"               *
   *                                                                       *
   *  Output arguments:                                                    *
   *                                                                       *
   *    numVars         Number of variables in the NetCDF file             *
   *    numDims         Number of dimensions in the NetCDF file            *
   *    dimSize_x       Size of x dimension "x"                            *
   *    dimSize_y       Size of y dimension "y"                            *
   *    attXRes         resolution of dimension "x" [meters]               *
   *    attYRes         resolution of dimension "y" [meters]               *
   *    attXCornerCtr   First column cell center x coordinate [meters]     *
   *    attYCornerCtr   First row cell center y coordinate [meters]        *
   *    scanningModeX   Flag, set to 1 if the grid rows run in the         *
   *                    negative x direction (rare).                       *
   *    scanningModeY   Flag, set to 1 if the grid columns run in the      *
   *                    positive y direction (common but not universal)    *
   *    semiMajorAxis   Ellipsoid semi-major axis, or earth radius if      *
   *                    spherical earth [meters]                           *
   *    invFlattening   Ellipsoid inverse flattening, 0 for spherical      *
   *                    earth.                                             *
   *    lonVDeg         Longitude of central meridian for Lambert          *
   *                    conformal conic projection [degrees]               *
   *    latDDeg         Latitude of the projection origin, where x and y   *
   *                    resolutions are are at their true scale.           *
   *    numStdParallels Number of standard parallels in the projection;    *
   *                    i.e. number of latitudes at which the projection   *
   *                    cone intersects with the sphere (1 or 2)           *
   *    stdParallel     One- or two-element array of Lambert conformal     *
   *                    conic projection standard parallels [degrees]      *
   *    varProcessFlag  Array of flags (numVars elements), set to 1 for    *
   *                    variable IDs (indices) we want to GRIBify          *
   *                                                                       *
   *  Greg Fall, NOHRSC, 2014-12-18                                        *
   *                                                                       *
   *************************************************************************/

/* Local Variables */

   size_t  attLength ; // Size of NetCDF attribute
   nc_type attType ;   // Type of NetCDF attribute
   int     dimID_y ;       // "y" dimension ID
   int     dimID_x ;       // "x" dimension ID
   int     dimID_unlimited ; // NetCDF unlimited dimension ID
   double *yVar ; // Latitude dimension variable
   double *xVar ; // Longitude dimension variable
   int     ncid ;            // NetCDF ID
   size_t  loc;
   int     numGlobalAtts;    // NetCDF # of global attributes
   int     numGridVars ;     // Number of gridded variables to process
  //   float   scratchFlt ;      // Scratch floating point variable
   int     status ;          // Return value of called function
   nc_type varDataType ;  // NetCDF data type
   int    *varDimID ;        // Array of dimension IDs for a NetCDF variable
   int     varID ;           // NetCDF Variable ID
   int     varID_GM;   // Variable ID for variable named in grid_mapping
   char    varName[NC_MAX_NAME+1] ; // NetCDF variable name
   int     varNumAtts ;      // # of attributes in NetCDF variable
   int     varNumDims ;      // # of dimensions in NetCDF variable
   size_t  i;
   char    firstGridMapping[NC_MAX_NAME+1]; // Name of grid_mapping variable.
   char    gridMapping[NC_MAX_NAME+1]; // Name of grid_mapping variable.
   double  earthRadius;
   double  falseEasting;
   double  falseNorthing;
   double  lonOfPrimeMeridian;
   char    units[NC_MAX_NAME+1];
   //char   *unitsStr;
   char    gridMappingRead; // Flag
  //char    gridMapping[NC_MAX_NAME+1];
  //char   *gridMappingSttr

  /*
    double  *semiMajorAxis;
    double  *invFlattening;
    double  *lonVDeg;
    double  *latDDeg;
    int     *numStdParallels;
    double  *stdParallel;

    semiMajorAxis = (double *) malloc(sizeof(double));
    invFlattening = (double *) malloc(sizeof(double));
    lonVDeg = (double *) malloc(sizeof(double));
    latDDeg = (double *) malloc(sizeof(double));
    numStdParallels = (int *) malloc(sizeof(int));
  */

   const char funcName[] = "gln_inquire_netcdf";

/* Open the NetCDF file. */

   status = nc_open(ncFilePath, NC_NOWRITE, &ncid);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to open \"%s\" as NetCDF\n",
             funcName, ncFilePath);
     return(-1);
   }

/* Do a general inquire on the NetCDF file. */

   status = nc_inq(ncid, numDims, numVars, &numGlobalAtts,
                   &dimID_unlimited);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: nc_inq failed for %s\n",
             funcName, ncFilePath);
     nc_close(ncid);
     return(-1);
   }

   if (*numDims < 2) {
     fprintf(stderr,
             "%s: Found %d dimensions; at least 2 expected.\n",
             funcName, *numDims);
     nc_close(ncid);
     return(-1);
   }

/***************************************************
 * Get geometry-related dimensions and attributes. *
 ***************************************************/

/* Find ID for "x" dimension. */

   status = nc_inq_dimid(ncid, "x", &dimID_x);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to find dimension \"x\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

/* Get size of "x" dimension. */

   status = nc_inq_dimlen(ncid, dimID_x, dimSize_x);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to find size of dimension \"x\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

/* Find ID for "y" dimension. */

   status = nc_inq_dimid(ncid, "y", &dimID_y);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to find dimension \"y\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

/* Get size of "y" dimension. */

   status = nc_inq_dimlen(ncid, dimID_y, dimSize_y);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to find size of dimension \"y\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

/* Allocate memory for dimension arrays. */

   varDimID = (int *) malloc(*numDims * sizeof(int));

   if (varDimID == NULL) {
     fprintf(stderr,
             "%s: Failed to allocate memory for dimension flags.\n",
             funcName);
     nc_close(ncid);
     exit(1);
   }

/* Get x dimension variable "x". */

   status = nc_inq_varid(ncid, "x", &varID);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to find dimension variable \"x\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   status = nc_inq_var(ncid, varID, varName, &varDataType,
                       &varNumDims, varDimID, &varNumAtts);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Inquire failed for dimension variable \"x\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   if (varNumDims != 1) {
     fprintf(stderr, "%s: Dimension variable \"x\" has %d dims; "
             "expecting 1.\n",
             funcName, varNumDims);
     nc_close(ncid);
     return(-1);
   }

   if (varDimID[0] != dimID_x) {
     fprintf(stderr,
             "%s: Dimension variable \"x\" does not vary with "
             "dimension \"x\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   if (varDataType != NC_DOUBLE) {
     fprintf(stderr,
             "%s: Dimension variable \"x\" is not a double.\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   status = nc_get_att_char_or_string(ncid, varID, "units", "x", units);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to get \"units\" attribute for variable \"x\".\n",
             funcName);
     return(-1);
   }

   if (strcmp(units, "m") &&
       strcmp(units, "meters") &&
       strcmp(units, "Meters")) {
     fprintf(stderr,
             "%s: \"units\" attribute for variable \"x\" must "
             "have units of \"m\", \"meters\", or \"Meters\".\n",
             funcName);
     return(-1);
   }

   xVar = (double *) malloc(*dimSize_x * sizeof(double));

   if (xVar == NULL) {
     fprintf(stderr,
             "%s: Failed to allocate memory for x dimension variable.\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   status = nc_get_var_double(ncid, varID, xVar);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to get x dimension variable \"x\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

/* Derive x resolution, corner coordinates, and scanning mode. */

   *attXCornerCtr = xVar[0];
   *attXRes = xVar[1] - xVar[0];
   for (i = 1; i < (*dimSize_x - 1); i++) {
     if (fabs(xVar[i+1] - xVar[i] - *attXRes) > 1.0e-8) {
       fprintf(stderr,
               "%s: non-uniform x resolution.\n", funcName);
       nc_close(ncid);
       return(-1);
     }
   }

   if (*attXRes < 0) {
     *scanningModeX = 1;
   } else {
     *scanningModeX = 0;
   }

/* Get y dimension variable "y". */

   status = nc_inq_varid(ncid, "y", &varID);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to find dimension variable \"y\".\n",
             funcName ) ;
     nc_close(ncid);
     return(-1);
   }

   status = nc_inq_var(ncid, varID, varName, &varDataType,
                       &varNumDims, varDimID, &varNumAtts);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Inquire failed for dimension variable \"y\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   if (varNumDims != 1) {
     fprintf(stderr, "%s: Dimension variable \"y\" has %d dims; "
             "expecting 1.\n",
             funcName, varNumDims);
     nc_close(ncid);
     return(-1);
   }

   if (varDimID[0] != dimID_y) {
     fprintf(stderr,
             "%s: Dimension variable \"y\" does not vary with "
             "dimension \"y\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   if (varDataType != NC_DOUBLE) {
     fprintf(stderr,
             "%s: Dimension variable \"y\" is not a double.\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   status = nc_get_att_char_or_string(ncid, varID, "units", "y", units);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to get \"units\" attribute for variable \"y\".\n",
             funcName);
     return(-1);
   }

   if (strcmp(units, "m") &&
       strcmp(units, "meters") &&
       strcmp(units, "Meters")) {
     fprintf(stderr,
             "%s: \"units\" attribute for variable \"y\" must "
             "have units of \"m\", \"meters\", or \"Meters\".\n",
             funcName);
     return(-1);
   }

   yVar = (double *) malloc (*dimSize_y * sizeof(double));

   if (yVar == NULL) {
     fprintf(stderr,
             "%s: Failed to allocate memory for y dimension variable.\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

   status = nc_get_var_double(ncid, varID, yVar);

   if (status != NC_NOERR) {
     fprintf(stderr,
             "%s: Failed to get y dimension variable \"y\".\n",
             funcName);
     nc_close(ncid);
     return(-1);
   }

/* Derive y resolution, corner coordinates, and scanning mode. */

   *attYCornerCtr = yVar[0];
   *attYRes = yVar[1] - yVar[0];
   for (i = 1; i < (*dimSize_y - 1); i++) {
     if (fabs(yVar[i+1] - yVar[i] - *attYRes) > 1.0e-8) {
       fprintf(stderr,
               "%s: non-uniform y resolution.\n", funcName);
       nc_close(ncid);
       return(-1);
     }
   }

   if (*attYRes < 0) {
     *scanningModeY = 0;
   } else {
     *scanningModeY = 1;
   }

/* Determine grid orientation/scanning mode */

/* NOTE: variable inquiries that follow this section will confirm that *
 * gridded variables are row major.                                    */
/*
   *scanningModeX = 0 ;

   if ( xVar[0] > xVar[*dimSize_x - 1] ) {
     *scanningModeX = 1 ;
     scratchFlt = *attXCornerCtr + ( *dimSize_x - 1 ) * *attXRes ;
     if ( xVar[0] != scratchFlt ) {
       fprintf ( stderr,
                 "%s: First grid longitude %f; expecting %f.\n",
                 funcName, xVar[0], scratchFlt ) ;
       nc_close ( ncid ) ;
       return ( -1 ) ;
     }
   } else {
     if ( xVar[0] != *attXCornerCtr ) {
       fprintf ( stderr,
                 "%s: First grid longitude %f; expecting %f.\n",
                 funcName, xVar[0], *attXCornerCtr ) ;
       nc_close ( ncid ) ;
       return ( -1 ) ;
     }
   }

   *scanningModeY = 0 ;

   if ( yVar[*dimSize_y - 1] > yVar[0] ) {
     *scanningModeY = 1 ;
     if ( yVar[0] != *attYCornerCtr ) {
       fprintf ( stderr,
                 "%s: First grid latitude %f; expecting %f.\n",
                 funcName, yVar[0], *attYCornerCtr ) ;
       nc_close ( ncid ) ;
       return ( -1 ) ;
     }
   } else {
     scratchFlt = *attYCornerCtr + ( *dimSize_y - 1 ) * *attYRes ;
     if ( yVar[0] != scratchFlt ) {
       fprintf ( stderr,
                 "%s: First grid latitude %f; expecting %f.\n",
                 funcName, yVar[0], scratchFlt ) ;
       nc_close ( ncid ) ;
       return ( -1 ) ;
     }
   }
*/

/* Loop over variables. Confirm expected attributes, dimensions, etc. */

   numGridVars = 0;
   gridMappingRead = 0;

   for (varID = 0 ; varID < *numVars ; varID++) {

     varProcessFlag[varID] = 0;

     status = nc_inq_var(ncid, varID, varName, &varDataType,
                         &varNumDims, varDimID, &varNumAtts);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Inquire failed for variable ID %d\n",
               funcName, varID);
       nc_close(ncid);
       return(-1);
     }

     if ((strcmp(varNameIn, "All")) &&    // Not doing "All"
         (strcmp(varName, varNameIn))) {  // Not requested variable
       continue;
     }

  /* Skip unsupported variables. These have no support in the GRIB spec. */
     /*
     if ( ( ! strcmp ( varName, "RootMoist" ) ) ||
          ( ! strcmp ( varName, "RootTemp" ) ) ||
          ( ! strcmp ( varName, "TotalMoist" ) ) ||
          ( ! strcmp ( varName, "AveTemp" ) ) ) {

       fprintf ( stderr,
                 "%s: WARNING: variable \"%s\" unsupported.\n",
                 funcName, varName ) ;
       continue ;

     }
     */
  /* Confirm the variable is on the x-y grid and is row major. */

     if (varNumDims != 2) continue;
     if (varDimID[0] != dimID_y) continue;
     if (varDimID[1] != dimID_x) continue;

  /* Skip "lon" and "lat" */

     if ((!strcasecmp(varName, "lon")) ||
         (!strcasecmp(varName, "lat"))) {
       continue;
     }

  /* Confirm the variable is NC_FLOAT */

     if (varDataType != NC_FLOAT) {
       fprintf(stderr,
               "%s: Skipping non-float variable \"%s\".\n",
               funcName, varName);
      continue;
     }

  /* Confirm existence of attribute "units" of NC_CHAR or NC_STRING type. */

     status = nc_inq_att(ncid, varID, "units", &attType, &attLength);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - no \"units\" attribute.\n",
               funcName, varName);
       continue;
     }

     if ((attType != NC_CHAR) && (attType != NC_STRING)) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - attribute \"units\" "
               "is neither NC_CHAR nor NC_STRING.\n",
               funcName, varName);
       continue;
     }

  /* Confirm existence of attribute "standard_name" of NC_CHAR or *
   * NC_STRING type.                                              */

     status = nc_inq_att(ncid, varID, "standard_name", &attType, &attLength);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - no \"standard_name\" "
               "attribute.\n",
               funcName, varName);
       continue;
     }

  /* Confirm existence of attribute "start_date" of NC_CHAR or NC_STRING *
   * type.                                                               */

     status = nc_inq_att(ncid, varID, "start_date",
                         &attType, &attLength);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - no \"start_date\" "
               "attribute.\n",
               funcName, varName);
       continue ;
     }

     if ((attType != NC_CHAR) && (attType != NC_STRING)) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - attribute \"start_date\" "
               "is neither NC_CHAR nor NC_STRING.\n",
               funcName, varName);
       continue;
     }

  /* Attribute "start_time" must be 23 characters long of the form *
   * "YYYY-mm-dd HH:MM:SS UTC"                                     */
     /*
     if (attLength != 23) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - attribute \"start_date\" "
               "is not of the form YYYY-mm-dd HH:MM:SS. %d\n",
               funcName, varName, attLength) ;
       continue;
     }
     */


  /* Confirm existence of attribute "stop_date" of NC_CHAR or NC_STRING *
   * type.                                                               */

     status = nc_inq_att(ncid, varID, "stop_date",
                         &attType, &attLength);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - no \"stop_date\" "
               "attribute.\n",
               funcName, varName);
       continue ;
     }

     if ((attType != NC_CHAR) && (attType != NC_STRING)) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - attribute \"stop_date\" "
               "is neither NC_CHAR nor NC_STRING.\n",
               funcName, varName);
       continue;
     }

  /* Attribute "finish_time" must be 19 characters long. */
     /*
     if ( attLength != 19 ) {
       fprintf ( stderr,
                 "%s: Skipping variable \"%s\" - attribute \"finish_time\" "
                 "is not of the form YYYY-mm-dd HH:MM:SS\n",
                 funcName, varName ) ;
       continue ;
     }
     */

  /* Confirm existence of attribute "missing_value". */
     /*
     status = nc_inq_att ( ncid, varID, "missing_value",
                           &attType, &attLength ) ;

     if ( status != NC_NOERR ) {
       fprintf ( stderr,
                 "%s: Skipping variable \"%s\" - no \"missing_value\" "
                 "attribute.\n",
                 funcName, varName ) ;
       continue ;
     }

     if ( attType != NC_FLOAT ) {
       fprintf ( stderr,
                 "%s: Skipping variable \"%s\" - attribute \"missing_value\" "
                 "is not a float.\n",
                 funcName, varName ) ;
       continue ;
     }
     */
  /* Attribute "missing_value" must be scalar. */
     /*
     if ( attLength != 1 ) {
       fprintf ( stderr,
                 "%s: Skipping variable \"%s\" - attribute \"missing_value\" "
                 "is non-scalar.\n",
                 funcName, varName ) ;
       continue ;
     }
     */

  /* Confirm existence of attribute "_FillValue". */

     status = nc_inq_att(ncid, varID, "_FillValue",
                         &attType, &attLength);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - no \"_FillValue\" "
               "attribute.\n",
               funcName, varName);
       continue;
     }

     if (attType != NC_FLOAT) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - attribute \"_FillValue\" "
               "is not a float.\n",
               funcName, varName);
       continue;
     }

  /* Attribute "_FillValue" must be scalar. */

     if (attLength != 1) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - attribute \"_FillValue\" "
               "is non-scalar.\n",
               funcName, varName);
       continue;
     }

  /* Get grid_mapping attribute. */

     for (loc = 0; loc < NC_MAX_NAME; loc++) {
       gridMapping[loc] = '\0';
     }

     status = nc_get_att_char_or_string(ncid, varID, "grid_mapping",
                                        varName, gridMapping);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Skipping variable \"%s\" - failed to get "
               "\"grid_mapping\" attribute.\n",
               funcName, varName);
       continue;
     }

     if (gridMappingRead) {

    /* Verify grid mapping name for this variable matches the first. */

       if (strcmp(gridMapping, firstGridMapping)) {
         fprintf(stderr,
                 "%s: Skipping variable \"%s\" - uses \"grid_mapping\" "
                 "attribute \"%s\", but have already established "
                 "\"%s\" as the grid mapping variable for this file.\n",
                 funcName, varName, gridMapping, firstGridMapping);
         continue;
       }

     } else {

    /* Get the variable ID for the grid_mapping container variable. */

       status = nc_inq_varid(ncid, gridMapping, &varID_GM);

       if (status != NC_NOERR) {
         fprintf(stderr,
                 "%s: Failed to get ID for \"grid_mapping\" "
                 "variable \"%s\".\n",
                 funcName, gridMapping);
         return(-1);
       }

    /* Get grid_mapping attributes. */

    /* If no "earth_radius" attribute is found, then look for *
     * "semi_major_axis" and "inverse_flattening".            */

       status = nc_inq_att(ncid, varID_GM, "earth_radius",
                           &attType, &attLength);

       if (status != NC_NOERR) {

         status = nc_inq_att(ncid, varID_GM, "semi_major_axis",
                             &attType, &attLength);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"earth_radius\" or "
                   "\"semi_major_axis\" attribute for "
                   "\"%s\" grid_mapping variable \"%s\".\n",
                   funcName, varName, gridMapping);
           return(-1);
         }

         if (attType != NC_DOUBLE) {
           fprintf(stderr, "%s: \"semi_major_axis\" attribute must be "
                   "NC_DOUBLE\n",
                   funcName);
           return(-1);
         }

         if (attLength != 1) {
           fprintf(stderr,
                   "%s:\"semi_major_axis\" attribute has unexpected "
                   "length %ld.\n",
                   funcName, (unsigned long) attLength);
           return(-1);
         }

         status = nc_get_att_double(ncid, varID_GM, "semi_major_axis",
                                    semiMajorAxis);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"semi_major_axis\" attribute for "
                   "\"grid_mapping\" variable \"%s\".\n",
                   funcName, gridMapping);
           return(-1);
         }

         status = nc_inq_att(ncid, varID_GM, "inverse_flattening",
                             &attType, &attLength);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"inverse_flattening\" attribute for "
                   "\"%s\" grid_mapping variable \"%s\".\n",
                   funcName, varName, gridMapping);
           return(-1);
         }

         if (attType != NC_DOUBLE) {
           fprintf(stderr, "%s: \"inverse_flattening\" attribute must be "
                   "NC_DOUBLE\n",
                   funcName);
           return(-1);
         }

         if (attLength != 1) {
           fprintf(stderr,
                   "%s:\"inverse_flattening\" attribute has unexpected "
                   "length %ld.\n",
                   funcName, (unsigned long) attLength);
           return(-1);
         }

         status = nc_get_att_double(ncid, varID_GM, "inverse_flattening",
                                    invFlattening);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"inverse_flattening\" attribute for "
                   "\"grid_mapping\" variable \"%s\".\n",
                   funcName, gridMapping);
           return(-1);
         }

       } else {

         if (attType != NC_DOUBLE) {
           fprintf(stderr, "%s: \"earth_radius\" attribute must be "
                   "NC_DOUBLE\n",
                   funcName);
           return(-1);
         }

         if (attLength != 1) {
           fprintf(stderr,
                   "%s:\"earth_radius\" attribute has unexpected "
                   "length %ld.\n",
                   funcName, (unsigned long) attLength);
           return(-1);
         }

         status = nc_get_att_double(ncid, varID_GM, "earth_radius",
                                    &earthRadius);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"earth_radius\" attribute for "
                   "\"grid_mapping\" variable \"%s\".\n",
                   funcName, gridMapping);
           return(-1);
         }

         memcpy(semiMajorAxis, &earthRadius, sizeof(double));
         *invFlattening = (double) 0.0;

       }

     //killme
       //       printf("semi major axis %f\n", *semiMajorAxis);
     //killme
       //       printf("inverse flattening %f\n", *invFlattening);

    /* The "longitude_of_central_meridian" attribute is required. */

       status = nc_inq_att(ncid, varID_GM, "longitude_of_central_meridian",
                           &attType, &attLength);

       if (status != NC_NOERR) {
         fprintf(stderr,
                 "%s: No \"longitude_of_central_meridian\" attribute for "
                 "\"grid_mapping\" variable \"%s\".\n",
                 funcName, gridMapping);
         return(-1);
       }

       if (attType != NC_DOUBLE) {
         fprintf(stderr,
                 "%s: \"longitude_of_central_meridian\" attribute "
                 "must be NC_DOUBLE\n",
                 funcName);
         return(-1);
       }

       if (attLength != 1) {
         fprintf(stderr,
                 "%s:\"longitude_of_central_meridian\" attribute has "
                 "unexpected length %ld.\n",
                 funcName, (unsigned long) attLength);
         return(-1);
       }

       status = nc_get_att_double(ncid, varID_GM,
                                  "longitude_of_central_meridian",
                                  lonVDeg);

       if (status != NC_NOERR) {
         fprintf(stderr,
                 "%s: Failed to get \"longitude_of_central_meridian\" "
                 "attribute for \"grid_mapping\" variable \"%s\".\n",
                 funcName, gridMapping);
         return(-1);
       }

       //killme
       //       printf("longitude_of_central_meridian %f\n", *lonVDeg);

    /* The "latitude_of_projection_origin" attribute is required. */

       status = nc_inq_att(ncid, varID_GM, "latitude_of_projection_origin",
                           &attType, &attLength);

       if (status != NC_NOERR) {
         fprintf(stderr,
                 "%s: No \"latitude_of_projection_origin\" attribute for "
                 "\"grid_mapping\" variable \"%s\".\n",
                 funcName, gridMapping);
         return(-1);
       }

       if (attType != NC_DOUBLE) {
         fprintf(stderr,
                 "%s: \"latitude_of_projection_origin\" attribute "
                 "must be NC_DOUBLE\n",
                 funcName);
         return(-1);
       }

       if (attLength != 1) {
         fprintf(stderr,
                 "%s:\"latitude_of_projection_origin\" attribute has "
                 "unexpected length %ld.\n",
                 funcName, (unsigned long) attLength);
         return(-1);
       }

       status = nc_get_att_double(ncid, varID_GM,
                                  "latitude_of_projection_origin",
                                  latDDeg);

       if (status != NC_NOERR) {
         fprintf(stderr,
                 "%s: Failed to get \"latitude_of_projection_origin\" "
                 "attribute for \"grid_mapping\" variable \"%s\".\n",
                 funcName, gridMapping);
         return(-1);
       }

       //killme
       //       printf("latitude_of_projection_origin %f\n", *latDDeg);

    /* Either one or two "standard_parallel" attributes is required. */

       status = nc_inq_att(ncid, varID_GM, "standard_parallel",
                           &attType, &attLength);

       if (status != NC_NOERR) {
         fprintf(stderr,
                 "%s: No \"standard parallel\" attribute for "
                 "\"grid_mapping\" variable \"%s\".\n",
                 funcName, gridMapping);
         return(-1);
       }

       if (attType != NC_DOUBLE) {
         fprintf(stderr,
                 "%s: \"standard_parallel\" attribute "
                 "must be NC_DOUBLE\n",
                 funcName);
         return(-1);
       }

       if ((attLength != 1) && (attLength != 2)) {
         fprintf(stderr,
                 "%s:\"standard_parallel\" attribute has unexpected "
                 "length %ld.\n",
                 funcName, (unsigned long) attLength);
         return(-1);
       }

       *numStdParallels = (int) attLength;

       *stdParallel =
         (double *) malloc(*numStdParallels * sizeof(double));

       status = nc_get_att_double(ncid, varID_GM,
                                  "standard_parallel",
                                  *stdParallel);

       if (status != NC_NOERR) {
         fprintf(stderr,
                 "%s: Failed to get \"standard_parallel\" "
                 "attribute for \"grid_mapping\" variable \"%s\".\n",
                 funcName, gridMapping);
         return(-1);
       }

       //killme
       //       for (i = 0; i < *numStdParallels; i++) {
       //         printf("standard parallel %d: %f\n", (int) i, *stdParallel[i]);
       //       }

    /* If "false_easting" or "false_northing" are present, make sure *
     * they are 0.                                                   */

       status = nc_inq_att(ncid, varID_GM, "false_easting",
                           &attType, &attLength);

       if (status != NC_NOERR) {

         if (attType != NC_DOUBLE) {
           fprintf(stderr, "%s: \"false_easting\" attribute must be "
                   "NC_DOUBLE\n",
                   funcName);
           return(-1);
         }

         if (attLength != 1) {
           fprintf(stderr,
                   "%s:\"false_easting\" attribute has unexpected "
                   "length %ld.\n",
                   funcName, (unsigned long) attLength);
           return(-1);
         }

         status = nc_get_att_double(ncid, varID_GM, "false_easting",
                                    &falseEasting);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"false_easting\" attribute for "
                   "\"grid_mapping\" variable \"%s\".\n",
                   funcName, gridMapping);
           return(-1);
         }

         if (falseEasting != 0.0) {
           fprintf(stderr,
                   "%s: No support for nonzero \"false_easting\" attribute "
                   "value of %f\n",
                   funcName, falseEasting);
           return(-1);
         }

       } // else { printf("no \"false easting\"\n"); }

       status = nc_inq_att(ncid, varID_GM, "false_northing",
                           &attType, &attLength);

       if (status != NC_NOERR) {

         if (attType != NC_DOUBLE) {
           fprintf(stderr, "%s: \"false_northing\" attribute must be "
                   "NC_DOUBLE\n",
                   funcName);
           return(-1);
         }

         if (attLength != 1) {
           fprintf(stderr,
                   "%s:\"false_northing\" attribute has unexpected "
                   "length %ld.\n",
                   funcName, (unsigned long) attLength);
           return(-1);
         }

         status = nc_get_att_double(ncid, varID_GM, "false_northing",
                                    &falseNorthing);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"false_northing\" attribute for "
                   "\"grid_mapping\" variable \"%s\".\n",
                   funcName, gridMapping);
           return(-1);
         }

         if (falseNorthing != 0.0) {
           fprintf(stderr,
                   "%s: No support for nonzero \"false_northing\" attribute "
                   "value of %f\n",
                   funcName, falseNorthing);
           return(-1);
         }

       }

    /* If "longitude_of_prime_meridian" is present, make sure it is zero. */

       status = nc_inq_att(ncid, varID_GM, "longitude_of_prime_meridian",
                           &attType, &attLength);

       if (status == NC_NOERR) {

         if (attType != NC_DOUBLE) {
           fprintf(stderr, "%s: \"longitude_of_prime_meridian\" attribute "
                   "must be NC_DOUBLE\n",
                   funcName);
           return(-1);
         }

         if (attLength != 1) {
           fprintf(stderr,
                   "%s:\"longitude_of_prime_meridian\" attribute has "
                   "unexpected length %ld.\n",
                   funcName, (unsigned long) attLength);
           return(-1);
         }

         status = nc_get_att_double(ncid, varID_GM,
                                    "longitude_of_prime_meridian",
                                    &lonOfPrimeMeridian);

         if (status != NC_NOERR) {
           fprintf(stderr,
                   "%s: Failed to get \"longitude_of_prime_meridian\" "
                   "attribute for \"grid_mapping\" variable \"%s\".\n",
                   funcName, gridMapping);
           return(-1);
         }

         if (falseEasting != 0.0) {
           fprintf(stderr,
                   "%s: No support for nonzero "
                   "\"longitude_of_prime_meridian\" "
                   "attribute value of %f\n",
                   funcName, lonOfPrimeMeridian);
           return(-1);
         }

       }

       strcpy(firstGridMapping, gridMapping);
       gridMappingRead = 1;

     }

  /* Variable is good to process. */

     varProcessFlag[varID] = 1;
     numGridVars++;

   }

   if (numGridVars == 0) {
     fprintf(stderr,
             "%s: No variables to convert.\n",
             funcName);
     return(-1);
   }

   return(ncid) ;

}

time_t gln_get_start_date(int ncid,
                          int numVars,
                          int numDims,
                          char *varProcessFlag,
                          int *startYear,
                          int *startMonth,
                          int *startDay,
                          int *startHour,
                          int *startMinute,
                          int *startSecond) {

  /*************************************************************************
   *                                                                       *
   *  For variables in open NetCDF file indicated by varProcessFlag,       *
   *  get "start_date" attributes. Confirm all are the same for variables  *
   *  identified by varProcessFlag for processing, and convert this        *
   *  date/time to a calendar date in seconds since Unix epoch, in UTC.    *
   *                                                                       *
   *  Output value: Start date in UTC seconds since Unix epoch.            *
   *                Returns -1 to indicate an error.                       *
   *                                                                       *
   *  Input arguments:                                                     *
   *                                                                       *
   *    ncid            ID of open NetCDF file                             *
   *    numVars         Number of variables in the NetCDF file             *
   *    numDims         Number of dimensions in the NetCDF file            *
   *    varProcessFlag  Array of flags (numVars elements), set to 1 for    *
   *                    varable IDs (indices) we want to GRIBify           *
   *                                                                       *
   *  Output arguments:                                                    *
   *                                                                       *
   *    Start time deconstructed into startYear, startMonth, startDay,     *
   *    startHour, startMinute, startSecond                                *
   *                                                                       *
   *  Greg Fall, NOHRSC, 2014-12-23, adapted from gng_get_ref_time,        *
   *                                 written 2011-03-24                    *
   *                                                                       *
   *************************************************************************/

/* Local variables */

   int     status ;                 // Return value of called function
   time_t  startDate ;               // Reference time
   char    startDateRead ;           // Reference time found flag
   nc_type varDataType ;            // NetCDF data type
   int    *varDimID ;               // Array of dimension IDs for a NetCDF variable
   int     varID ;                  // NetCDF Variable ID
   char    varName[NC_MAX_NAME+1] ; // NetCDF variable name
   int     varNumAtts ;             // # of attributes in NetCDF variable
   int     varNumDims ;             // # of dimensions in NetCDF variable
   char    varStartDate[24] ;        // "start date" "YYYY-mm-dd HH:MM:SS UTC"
   int     varStartDay ;             // Variable finish day
   int     varStartHour ;            // Variable finish hour
   int     varStartMinute ;          // Variable finish minute
   int     varStartMonth ;           // Variable finish month
   int     varStartSecond ;          // Variable finish second
   int     varStartYear ;            // Variable finish year

   const char funcName[] = "gln_get_start_date";

   startDateRead = 0;

   varDimID = (int *) malloc(numDims * sizeof(int));

   if (varDimID == NULL) {
     fprintf(stderr,
             "%s: Failed to allocate memory for dimension flags.\n",
             funcName);
     return(-1);
   }

   for (varID = 0; varID < numVars; varID++) {

     if (!varProcessFlag[varID]) continue;

  /* Get the variable name (only needed for error messages). */

     status = nc_inq_var(ncid, varID, varName, &varDataType,
                         &varNumDims, varDimID, &varNumAtts);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Inquire failed for variable %d.\n",
               funcName, varID);
       return(-1);
     }

  /* Get the "start_date" attribute */

     status = nc_get_att_char_or_string(ncid, varID, "start_date", varName,
                                        varStartDate);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Failed to get \"start_date\" attribute for variable "
               "\"%s\".\n",
               funcName, varName);
       return(-1);
     }

  /* Chop the " UTC" off the "start_date". */

     varStartDate[19] = '\0';

  /* Break the date into components. */

     if (!deconstruct_date(varStartDate, &varStartYear, &varStartMonth,
                           &varStartDay, &varStartHour, &varStartMinute,
                           &varStartSecond)) {
       fprintf(stderr,
               "%s: Function deconstruct_date failed for attribute "
               "start_date=\"%s\" for variable \"%s\".\n",
               funcName, varStartDate, varName);
       return(-1);
     }

     if (!startDateRead) {

    /* Set start time */

       *startYear = varStartYear;
       *startMonth = varStartMonth;
       *startDay = varStartDay;
       *startHour = varStartHour;
       *startMinute = varStartMinute;
       *startSecond = varStartSecond;

       startDateRead = 1;

     } else {

    /* Confirm consistency */

       if ((varStartYear != *startYear) ||
           (varStartMonth != *startMonth) ||
           (varStartDay != *startDay) ||
           (varStartHour != *startHour) ||
           (varStartMinute != *startMinute) ||
           (varStartSecond != *startSecond)) {

         fprintf(stderr,
                 "%s: Attribute start_date=\"%s\" for variable %s does not "
                 "match reference \"%04d-%02d-%02d %02d:%02d:%02d\"\n",
                 funcName, varStartDate, varName,
                 *startYear, *startMonth, *startDay, *startHour, *startMinute,
                 *startSecond);
         return(-1);

       }

     }

   }

/* Convert start time to time_t **/

   startDate = datetime_to_epoch_utc(*startYear, *startMonth, *startDay,
                                    *startHour, *startMinute, *startSecond);

   if (startDate == -1) {

     fprintf(stderr,
             "%s: Failed to convert start date/time "
             "\"%04d-%02d-%02d %02d:%02d:%02d\" to an epoch time.\n",
             funcName, *startYear, *startMonth, *startDay, *startHour, *startMinute,
             *startSecond);
     return(-1);

   }

   return(startDate) ;

}





time_t gln_get_stop_date(int ncid,
                         int numVars,
                         int numDims,
                         char *varProcessFlag,
                         int *stopYear,
                         int *stopMonth,
                         int *stopDay,
                         int *stopHour,
                         int *stopMinute,
                         int *stopSecond) {

  /*************************************************************************
   *                                                                       *
   *  For variables in open NetCDF file indicated by varProcessFlag,       *
   *  get "stop_date" attributes. Confirm all are the same for variables   *
   *  identified by varProcessFlag for processing, and convert this        *
   *  date/time to a calendar date in seconds since Unix epoch, in UTC.    *
   *                                                                       *
   *  Output value: Stop date in UTC seconds since Unix epoch.             *
   *                Returns -1 to indicate an error.                       *
   *                                                                       *
   *  Input arguments:                                                     *
   *                                                                       *
   *    ncid            ID of open NetCDF file                             *
   *    numVars         Number of variables in the NetCDF file             *
   *    numDims         Number of dimensions in the NetCDF file            *
   *    varProcessFlag  Array of flags (numVars elements), set to 1 for    *
   *                    varable IDs (indices) we want to GRIBify           *
   *                                                                       *
   *  Output arguments:                                                    *
   *                                                                       *
   *    Stop time deconstructed into stopYear, stopMonth, stopDay,         *
   *    stopHour, stopMinute, stopSecond                                   *
   *                                                                       *
   *  Greg Fall, NOHRSC, 2014-12-23, adapted from gng_get_ref_time,        *
   *                                 written 2011-03-24                    *
   *                                                                       *
   *************************************************************************/

/* Local variables */

   int     status ;                 // Return value of called function
   time_t  stopDate ;               // Reference time
   char    stopDateRead ;           // Reference time found flag
   nc_type varDataType ;            // NetCDF data type
   int    *varDimID ;               // Array of dimension IDs for a NetCDF variable
   int     varID ;                  // NetCDF Variable ID
   char    varName[NC_MAX_NAME+1] ; // NetCDF variable name
   int     varNumAtts ;             // # of attributes in NetCDF variable
   int     varNumDims ;             // # of dimensions in NetCDF variable
   char    varStopDate[24] ;        // "stop date" "YYYY-mm-dd HH:MM:SS UTC"
   int     varStopDay ;             // Variable finish day
   int     varStopHour ;            // Variable finish hour
   int     varStopMinute ;          // Variable finish minute
   int     varStopMonth ;           // Variable finish month
   int     varStopSecond ;          // Variable finish second
   int     varStopYear ;            // Variable finish year

   const char funcName[] = "gln_get_stop_date";

   stopDateRead = 0;

   varDimID = (int *) malloc(numDims * sizeof(int));

   if (varDimID == NULL) {
     fprintf(stderr,
             "%s: Failed to allocate memory for dimension flags.\n",
             funcName);
     return(-1);
   }

   for (varID = 0; varID < numVars; varID++) {

     if (!varProcessFlag[varID]) continue;

  /* Get the variable name (only needed for error messages). */

     status = nc_inq_var(ncid, varID, varName, &varDataType,
                         &varNumDims, varDimID, &varNumAtts);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Inquire failed for variable %d.\n",
               funcName, varID);
       return(-1);
     }

  /* Get the "stop_date" attribute */

     status = nc_get_att_char_or_string(ncid, varID, "stop_date", varName,
                                        varStopDate);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "%s: Failed to get \"stop_date\" attribute for variable "
               "\"%s\".\n",
               funcName, varName);
       return(-1);
     }

  /* Chop the " UTC" off the "stop_date". */

     varStopDate[19] = '\0';

  /* Break the date into components. */

     if (!deconstruct_date(varStopDate, &varStopYear, &varStopMonth,
                           &varStopDay, &varStopHour, &varStopMinute,
                           &varStopSecond)) {
       fprintf(stderr,
               "%s: Function deconstruct_date failed for attribute "
               "stop_date=\"%s\" for variable \"%s\".\n",
               funcName, varStopDate, varName);
       return(-1);
     }

     if (!stopDateRead) {

    /* Set stop time */

       *stopYear = varStopYear;
       *stopMonth = varStopMonth;
       *stopDay = varStopDay;
       *stopHour = varStopHour;
       *stopMinute = varStopMinute;
       *stopSecond = varStopSecond;

       stopDateRead = 1;

     } else {

    /* Confirm consistency */

       if ((varStopYear != *stopYear) ||
           (varStopMonth != *stopMonth) ||
           (varStopDay != *stopDay) ||
           (varStopHour != *stopHour) ||
           (varStopMinute != *stopMinute) ||
           (varStopSecond != *stopSecond)) {

         fprintf(stderr,
                 "%s: Attribute stop_date=\"%s\" for variable %s does not "
                 "match reference \"%04d-%02d-%02d %02d:%02d:%02d\"\n",
                 funcName, varStopDate, varName,
                 *stopYear, *stopMonth, *stopDay, *stopHour, *stopMinute,
                 *stopSecond);
         return(-1);

       }

     }

   }

/* Convert stop time to time_t **/

   stopDate = datetime_to_epoch_utc(*stopYear, *stopMonth, *stopDay,
                                    *stopHour, *stopMinute, *stopSecond);

   if (stopDate == -1) {

     fprintf(stderr,
             "%s: Failed to convert stop date/time "
             "\"%04d-%02d-%02d %02d:%02d:%02d\" to an epoch time.\n",
             funcName, *stopYear, *stopMonth, *stopDay, *stopHour, *stopMinute,
             *stopSecond);
     return(-1);

   }

   return(stopDate) ;

}






int main (int argc, char *argv[]) {

  /* ARGUMENTS THAT SHOULD COME FROM THE COMMAND LINE */

  char varNameIn[] = "All";

  /* GENERIC ARGUMENTS */

  long col;
  char expectedUnits[NC_MAX_NAME+1];
  double F;
  unsigned char *grib2Buffer;
  size_t grib2BufferSize;
  g2float *grib2Data;
  g2int *grib2Mask;
  long grib2Size;
  int ierr;
  double invFlattening;
  g2int latCornerCtrGRIB;
  double latDDeg;
  g2int latDGRIB;
  double latDRad;
  long loc;
  g2int lonCornerCtrGRIB;
  double lonVDeg;
  g2int lonVGRIB;
  g2int missing;
  float *myVar;
  double n;
  g2float ndvGRIB2 = -99999.0 ;
  float ndvNetCDF;
  long numGridPoints;
  int numStdParallels;
  const double Pi = 3.141592653589793;
  double R;
  double rho;
  double rho0;
  long row;
  g2int scratch_g2int ;
  double semiMajorAxis;
  int startDay;
  int startHour;
  int startMinute;
  int startMonth;
  int startSecond;
  time_t startTime;
  //  char startTimeStr[24];
  int startYear;


  time_t stopTime;
  int stopYear;
  int stopMonth;
  int stopDay;
  int stopHour;
  int stopMinute;
  int stopSecond;
  int status;
  double stdPar1Rad;
  double stdPar2Rad;
  double *stdParallel;
  double timeDiff;
  g2int timeDiffHours;
  double remainder;
  char units[NC_MAX_NAME+1];
  float value ;
  double xRes ;
  g2int  xResGRIB ;
  double yRes ;
  g2int  yResGRIB ;

  double theta;
  double lonVRad;
  double latCornerCtr;
  double lonCornerCtr;

  g2int  latin1GRIB;
  g2int  latin2GRIB;

   double   attXRes ; // Grid x resolution
   double  attYRes ; // Grid x resolution
   size_t  dimSize_y ; // Latitude dimension size
   size_t  dimSize_x ; // Longitude dimension size
   char    scanningModeX ; // East-to-west grid orientation flag
   char    scanningModeY ; // South-to-north grid orientation flag
   int     ncid ;    // NetCDF ID */
   int     numDims ; // Number of dimensions in the NetCDF file
   int     numVars ; // Number of variables in the NetCDF file
   nc_type varDataType ;  // NetCDF data type
   int    *varDimID ;        // Array of dimension IDs for a NetCDF variable
   int     varID ;           // NetCDF Variable ID
   int     varNumAtts ;      // # of attributes in NetCDF variable
   int     varNumDims ;      // # of dimensions in NetCDF variable

   char    varName[NC_MAX_NAME+1] ; // NetCDF variable name
   char    varProcessFlag[NC_MAX_VARS];
   double  attXCornerCtr ; // SW grid cell center longitude
   double  attYCornerCtr ; // SW grid cell center latitude


   char    inputPath[_POSIX_PATH_MAX] ;
   char    outputPath[_POSIX_PATH_MAX] ;
   char    standardName[NC_MAX_NAME+1];


   /*
   fprintf ( stderr, "This is a very long line long line long line "
             "long line long line long line long line long line long "
             "line long line long line long line long line long line "
             "long line long line long line long line long line long "
             "line long line long line long line long line long line\n" ) ;
   */


   /****
    * Read command line *
    ***/

   // gribify_netcdf_grid {input file} {output file}

   if ( argc != 3 ) {
     fprintf ( stderr, "Fix me %d.\n", argc ) ;
     exit ( 1 ) ;
   }

   if ( strlen ( argv[1] ) >= _POSIX_PATH_MAX ) {
     fprintf ( stderr, "Fix me 2.\n" ) ;
   }

   strcpy(inputPath, argv[1]);

   if ( strlen ( argv[2] ) >= _POSIX_PATH_MAX ) {
     fprintf ( stderr, "Fix me 3.\n" ) ;
   }

   strcpy(outputPath, argv[2]);

/*************************************
 * Inquire/confirm NetCDF contents
 *************************************/

   ncid = gln_inquire_netcdf(inputPath,
                             varNameIn,
                             &numVars,
                             &numDims, 
                             &dimSize_x,
                             &dimSize_y,
                             &attXRes,
                             &attYRes,
                             &attXCornerCtr,
                             &attYCornerCtr,
                             &scanningModeX,
                             &scanningModeY,
                             &semiMajorAxis,
                             &invFlattening,
                             &lonVDeg,
                             &latDDeg,
                             &numStdParallels,
                             &stdParallel,
                             varProcessFlag);

   if (ncid == -1) {
     fprintf(stderr, "NetCDF confirmation failed.\n");
     exit(1);
   }

   /*
   printf("attXRes %f\n", attXRes);
   printf("attYRes %f\n", attYRes);
   printf("attXCornerCtr %f\n", attXCornerCtr);
   printf("attYCornerCtr %f\n", attYCornerCtr);
   printf("scanningModeX %d\n", scanningModeX);
   printf("scanningModeY %d\n", scanningModeY);
   printf("semiMajorAxis %f\n", semiMajorAxis);
   printf("invFlattening %f\n", invFlattening);
   printf("lonVDeg %f\n", lonVDeg);
   printf("latDDeg %f\n", latDDeg);
   printf("numStdParallels %d\n", numStdParallels);
   for (i = 0; i < numStdParallels; i++) {
     printf("standard parallel %d: %f\n", i, stdParallel[i]);
   }
   */

/***************************************************************
 * Get stop time from NetCDF file for GRIB identifier section. *
 ***************************************************************/

   /*
   strcpy(finishTimeStr, "2014-12-12 12:00:00");
   if (!deconstruct_date(finishTimeStr, &stopYear, &stopMonth, &stopDay,
                         &stopHour, &stopMinute, &stopSecond)) {
     fprintf(stderr, "Time conversion failed for finish time \"%s\".\n",
             finishTimeStr);
     exit(1);
   }

   stopTime = datetime_to_epoch_utc(stopYear, stopMonth, stopDay, stopHour, stopMinute, stopSecond);
   if (stopTime == -1) {
     fprintf ( stderr,
               "Failed to convert reference date/time "
               "\"%04d-%02d-%02d %02d:%02d:%02d\" to an epoch time.\n",
               stopYear, stopMonth, stopDay, stopHour, stopMinute,
               stopSecond ) ;
     exit(1);
   }

   */

   startTime = gln_get_start_date(ncid,
                                  numVars,
                                  numDims,
                                  varProcessFlag,
                                  &startYear,
                                  &startMonth,
                                  &startDay,
                                  &startHour,
                                  &startMinute,
                                  &startSecond);

   if (startTime == -1) {
     fprintf(stderr, "Failed to get start time for NetCDF variables.\n");
     nc_close(ncid);
     exit(1);
   }

   stopTime = gln_get_stop_date(ncid,
                                numVars,
                                numDims,
                                varProcessFlag,
                                &stopYear,
                                &stopMonth,
                                &stopDay,
                                &stopHour,
                                &stopMinute,
                                &stopSecond);

   if (stopTime == -1) {
     fprintf(stderr, "Failed to get stop time for NetCDF variables.\n");
     nc_close(ncid);
     exit(1);
   }

   //killme
   //   printf("ref time %d-%d-%d %d:%d:%d\n", stopYear, stopMonth, stopDay,
   //          stopHour, stopMinute, stopSecond);

/**************************************
 * Refine precision of grid geometry. *
 **************************************/

/* Round resolution attributes to the nearest 1/10 arc-second. */
   /*
   xRes = round ( ( double ) attXRes * 36000.0 ) / 36000.0 ;
   yRes = round ( ( double ) attYRes * 36000.0 ) / 36000.0 ;
   */

/* Remove signs from resolution attributes. */

   xRes = fabs(attXRes);
   yRes = fabs(attYRes);

   xResGRIB = (g2int) (xRes / 1.0e-3 + 0.5);
   yResGRIB = (g2int) (yRes / 1.0e-3 + 0.5);

/***********************************q*********************************** *
 * Prepare to call GRIB2 functions g2_create and g2_addgrid, to create  *
 * the output GRIB2 buffer and define the data grid. This will require  *
 * knowing in advance the sizes of several sections of the GRIB2 buffer *
 ************************************************************************/

   /* NOTE: Modeled after sm_products.pgc                     *
    *       Variable names after NCEP documentation on g2clib */

   long  listsec0[3]; // Info for GRIB2 Indicator Section
   long  listsec1[14]; // Info for GRIB2 Identifier section
   long  igds[6]; // Info for GRIB2 Grid Definition Section
   g2int igdstmpl[22] ; // Info for grid definition template 3.30

/* Set possible "terminating" bytes. */

   listsec0[2] = 0 ; 
   listsec1[13] = 0 ;
   igds[5] = 0 ;
   igdstmpl[21] = 0 ;

/* Initialize the missing value. Loop over all bits and turn them on. */

   // Probably this will mean looping over 4 bytes (32 bits), and the value
   // will go from 0 to 1, 3, 7, 15, 31, 63, ..., 2147483647, and will finally
   // flip to -1 as the last bit is turned on.

   missing = 0 ;

   for ( loc = 0 ; loc < ( sizeof ( g2int ) * 8  ) ; loc++ ) {
     missing |= ( 1 << loc ) ;
   }

/** SECTION 0. INDICATOR SECTION **/

   // 16 bytes

   grib2BufferSize = 16 ;

/* Discipline - GRIB master table number (Table 0.0) */

   listsec0[0] = 0 ; // Meteorological Products (Table 4.1)

/* GRIB version */

   listsec0[1] = 2 ;

/** SECTION 1. IDENTIFIER SECTION **/

   // 21 bytes

   grib2BufferSize += 21 ;

/* Originating center */

   listsec1[0] = 9 ; // US NWS - Other **/

/* Originating Subcenter */

   listsec1[1] = 163 ; // Anders' best guess

/* GRIB master tables version */

   listsec1[2] = 2 ; // Version implemented on 4 November 2003

/* GRIB local tables version */

   listsec1[3] = 0 ; // Local tables not used

/* Significance of reference time */

   listsec1[4] = 0; // Analysis

/* Reference time */

   listsec1[5] = ( long ) startYear;
   listsec1[6] = ( long ) startMonth;
   listsec1[7] = ( long ) startDay;
   listsec1[8] = ( long ) startHour;
   listsec1[9] = ( long ) startMinute;
   listsec1[10] = ( long ) startSecond;

/* Production status of data */

   listsec1[11] = 3; // Research Products

/* Type of data */

   listsec1[12] = 0; // Analysis Products

/** SECTION 2. LOCAL USE SECTION (not used) **/

/** SECTION 3. GRID DEFINITION SECTION **/

   // 81 bytes for Lambert conformal.

   grib2BufferSize += 81;

/* Source of grid definition */

   igds[0] = 0; // Grid definition specified in Table 3.1 **/

/* Number of grid points in the defined grid */

   numGridPoints = (long) dimSize_y * (long) dimSize_x;
   igds[1] = numGridPoints;

/* Number of octets needed for each additional grid points definition */

   igds[2] = 0; // Regular grid

/* Interpretation of list for optional points definition (Table 3.11) */

   igds[3] = 0; // No appended list

/* Grid definition template number (Table 3.1) */

   igds[4] = 30; // Lambert Conformal - Grid Template 3.30 **/

/* Grid definition template */

   // igdstmpl will hold values for Grid Template 3.30

/* Shape of the earth: */

   // In keeping with NCEP grid 184, the earth shape for this grid is a sphere
   // with radius 6371229.0 m. Support for e.g. WGS84 is possible, just not
   // bothering with it. Also support for a sphere with radius 6367470.0 m
   // is possible or at least supported by GRIB...

   if (semiMajorAxis != 6371229.0) {
     fprintf(stderr,
             "Semi-major axis %f is not supported.\n", semiMajorAxis);
     exit(1);
   }

   if (invFlattening != 0.0) {
     fprintf(stderr,
             "Non-spherical earth shape is not supported.\n");
     exit(1);
   }

   igdstmpl[0] = 6; // Earth assumed spherical with radius 6371229.0 m

/* Scale factor of radius of spherical earth */

   igdstmpl[1] = missing;

/* Scale value of radius of spherical earth */

   igdstmpl[2] = missing;

/* Scale factor of major axis of oblate spheroid earth */

   igdstmpl[3] = missing;

/* Scaled value of major axis of oblate spheroid earth */

   igdstmpl[4] = missing;

/* Scale factor of minor axis of oblate spheroid earth */

   igdstmpl[5] = missing;

/* Scaled value of minor axis of oblate spheroid earth */

   igdstmpl[6] = missing;

/* Number of points along the x-axis. */

   igdstmpl[7] = (g2int) dimSize_x;

/* Number of points along the y-axis. */

   igdstmpl[8] = (g2int) dimSize_y;

/* Latitude/Longitude of the first grid point ("lat1"/"lon1") */

   R = semiMajorAxis;

   stdPar1Rad = stdParallel[0] * Pi / 180.0;
   if (numStdParallels == 1) {
     n = sin(stdPar1Rad);
   } else {
     stdPar2Rad = stdParallel[1] * Pi / 180.0;
     n = log(cos(stdPar1Rad) / cos(stdPar2Rad))
       / log(tan(Pi / 4.0 + stdPar2Rad / 2.0) /
             tan(Pi / 4.0 + stdPar1Rad / 2.0));
   }
   F = cos(stdPar1Rad) * pow(tan(Pi / 4.0 + stdPar1Rad / 2.0), n) / n;
   latDRad = latDDeg * Pi / 180.0;
   rho0 = R * F / pow(tan(Pi / 4.0 + latDRad / 2.0), n);
   rho = sqrt((attXCornerCtr * attXCornerCtr) + 
              (rho0 - attYCornerCtr) * (rho0 - attYCornerCtr)) * n / fabs(n);
   theta = atan(attXCornerCtr / (rho0 - attYCornerCtr));
   lonVRad = lonVDeg * Pi / 180.0;
   latCornerCtr = 2.0 * atan(pow(R * F / rho, 1.0 / n)) - Pi / 2.0;
   lonCornerCtr = lonVRad + theta / n;
   //killme
   //printf("%f %f %f %f %f %f %f %f\n",
   //       R, n, F, latDRad, rho0, rho, theta, lonVRad);
   //killme
   //printf("%23.7f %23.7f\n", lonCornerCtr * 180.0 / Pi, latCornerCtr * 180.0 / Pi);
   //killme
   //printf("%d %d\n", scanningModeX, scanningModeY);
   
/* La1 - latitude of first grid point */

   latCornerCtrGRIB = (g2int) (latCornerCtr * 180.0 / Pi / 1.0e-6);
   //killme
   //printf("corner center lat %ld\n", latCornerCtrGRIB);
   igdstmpl[9] = latCornerCtrGRIB;

/* Lo1 - longitude of first grid point */

   lonCornerCtrGRIB = (g2int) (lonCornerCtr * 180.0 / Pi / 1.0e-6);
   //killme
   //printf("corner center lon %ld\n", lonCornerCtrGRIB);
   igdstmpl[10] = lonCornerCtrGRIB;

/* Resolution and component flags - Table 3.3 */

   // BEGIN OLD WAY
   // Value is 00111000, which means:

   //   (first two bits reserved)
   //   "i direction increments given",
   //   "j direction increments given",
   //   "resolved u and v components of vector quantities relative to the
   //    defined grid in the direction of increasing x and y (or i and j)
   //    coordinates, respectively"

   scratch_g2int = 0 ;
   scratch_g2int = ( 1 << 3 ) | ( 1 << 4 ) | ( 1 << 5 ) ;
   //killme
   //   printf("old rcf %d\n", scratch_g2int);
   // END OLD WAY

   // Value is 00001000, which means:
   //   (first two bits reserved)
   //   "i direction increments not given",
   //   "j direction increments not given",
   //   "resolved u and v components of vector quantities relative to the
   //    defined grid in the direction of increasing x and y (or i and j)
   //    coordinates, respectively"

   scratch_g2int = 0 ;
   scratch_g2int = ( 1 << 3 ) ;
   //killme
   //   printf("new rcf %d\n", scratch_g2int);

   igdstmpl[11] = scratch_g2int;

/* LaD - latitude where dx and dy are specified [micro-degrees] */

   latDGRIB = (g2int) (latDDeg / 1.0e-6);
   //killme
   //printf("latDDeg %f\n", latDDeg);
   //printf("latd %ld\n", latDGRIB);
   igdstmpl[12] = latDGRIB;

/* LoV - longitude where meridian parallel to y-axis along which latitude *
 * increases as the y-coordinate increases [micro-degrees]                */
   lonVGRIB = (g2int) (lonVDeg / 1.0e-6);
   //killme
   //printf("lonv %ld\n", lonVGRIB);
   igdstmpl[13] = lonVGRIB;

/* Dx - x direction grid length [mm] */

   igdstmpl[14] = xResGRIB;

/* Dy - y direction grid length [mm] */

   igdstmpl[15] = yResGRIB;

/* Projection center flag */

   // Value is 00000000, which means:

   //   "north pole is on the projection plane"
   //   "only one projection center is used"
   //   (last 6 bits reserved)

   scratch_g2int = 0;
   igdstmpl[16] = scratch_g2int;

/* Scanning mode - Table 3.4 */

   // Bit 1 - west-to-east (0) or east-to-west (1)
   // Bit 2 - north-to-south (0) or south-to-north (1)
   // Bit 3 = 0 (row major)
   // Bit 4 = 0 (adjacent rows scan in the same direction)

   scratch_g2int = 0 ;
   //killme
   //   printf("sm 1 ---> %d\n", scratch_g2int);
   if (scanningModeX) {
     // 00000000 -> 10000000
     scratch_g2int |= (1 << 7);
   }
   //killme
   //   printf("sm 1 ---> %d\n", scratch_g2int);

   if (scanningModeY) {
     // x0000000 -> x1000000
     scratch_g2int |= (1 << 6);
   }
   //killme
   //  printf("sm 2 ---> %d\n", scratch_g2int);

   igdstmpl[17] = scratch_g2int ;

 /* Latin 1 - first latitude from the pole at which the secant cone cuts *
  * the sphere                                                           */

   latin1GRIB = (g2int) (stdParallel[0] / 1.0e-6);

   igdstmpl[18] = latin1GRIB;

   if (numStdParallels == 1) {
     latin2GRIB = latin1GRIB;
   } else {
     latin2GRIB = (g2int) (stdParallel[1] / 1.0e-6);
   }

   igdstmpl[19] = latin2GRIB;

/* Latitude of the southern pole of projection */

   igdstmpl[20] = (g2int) 0;

/* Longitude of the southern pole of projection */

   igdstmpl[21] = (g2int) 0;

/* Determine the size of the GRIB2 buffer, accounting for a product/data *
 * section for each variable.                                            */

   for (varID = 0 ; varID < numVars ; varID++) {

     if (! varProcessFlag[varID]) continue;

 /** SECTION 4. PRODUCT DEFINITION SECTION **/

     // Product template 4.0 - analysis or forecast at a horizontal level
     // or in a horizontal layer at a point in time (specified below)

     // Section 4 is 34 bytes for Template 4.0)

     grib2BufferSize += 34;

 /** SECTION 5. DATA REPRESENTATION SECTION **/

     // Grid template 5.0 - grid
     // Section 5 is 21 bytes for Template 5.0 (simple packing),
     // 23 bytes for Template 5.40 (JPEG2000 compression)
     // (specified below)

     grib2BufferSize += 23;

 /** SECTION 6. BIT MAP SECTION **/

     // Size: 6 + (grid size / 8) bytes, plus an extra byte if the grid
     //       size is not a multiple of 8.

     grib2BufferSize += 6;

     if ((numGridPoints % 8) == 0) {
       grib2BufferSize += (numGridPoints / 8) ;
     } else {
       grib2BufferSize += (numGridPoints / 8) + 1;
     }

     /** SECTION 6. DATA SECTION **/

     // Size: 5 + (grid size * bytes per pixel) bytes.

     grib2BufferSize += 5 + (numGridPoints * sizeof (g2float)) ;

   }

/** SECTION 7. END SECTION **/

   // 4 bytes

   grib2BufferSize += 4;

/* Allocate memory for GRIB2 buffer. */

   grib2Buffer = (unsigned char *) malloc(grib2BufferSize);

   if (grib2Buffer == NULL) {
     fprintf(stderr, "Failed to allocate %d bytes for GRIB2 buffer.\n",
             (int) grib2BufferSize);
     nc_close(ncid);
     exit(1);
   }

   grib2Data = (g2float *) malloc((size_t) numGridPoints *
                                  sizeof(g2float));
   if (grib2Data == NULL) {
     fprintf(stderr, "Failed to allocate %ld bytes for GRIB2 data grid.\n",
             (long) ((size_t) numGridPoints * sizeof(g2float)));
     nc_close(ncid);
     exit(1);
   }

   grib2Mask = (g2int *) malloc((size_t) numGridPoints *
                                sizeof(g2int));

   if (grib2Mask == NULL) {
     fprintf(stderr, "Failed to allocate %ld bytes for GRIB2 bit mask.\n",
             (long) ((size_t) numGridPoints * sizeof(g2int)));
     nc_close(ncid);
     exit(1);
  }

/* Initialize GRIB2 buffer. */

   for (loc = 0; loc < grib2BufferSize; loc++) {

     grib2Buffer[loc] = 0;

   }

/* Create new GRIB2 buffer. */

   ierr = g2_create(grib2Buffer, listsec0, listsec1);

   if (ierr <= 0) {
     fprintf(stderr, "g2_create failed.\n");
     nc_close(ncid);
     exit(1);
   }

   grib2Size = ierr;
   //killme
   //   printf ( "g2_create sez size of GRIB2 message is now %ld\n", grib2Size ) ;

/* Perform the grid definition. */

   ierr = g2_addgrid(grib2Buffer, igds, igdstmpl, NULL, 0);

   if (ierr <= 0) {
     fprintf(stderr, "g2_addgrid failed.\n");
     nc_close(ncid);
     exit(1);
   }

   grib2Size = ierr;
   //killme
   //   printf ( "g2_addgrid sez size of GRIB2 message is now %ld\n", grib2Size ) ;

/* Define GRIB2 product. */

   double unitSlope ; // Conversion slope for correct GRIB2 units
   double unitIntercept ; // Conversion intercept for correct GRIB2 units */
   g2int ipdsnum ;
   g2int ipdstmpl[29];
   g2int idrsnum ;
   g2int idrstmpl[7] ;

   g2int D_packing ; // Value "D" in simple packing; decimal precision
   double pow10D ;   // pow ( 10.0, D_packing )
   g2int E_packing ; // Value "E" in simple packing; binary scale factor
   g2int R_packing ; // Value "R" in simple packing; reference value
   double newVal ;

   double minVal ;
   double maxVal ;

/* Allocate memory */

   varDimID = (int *) malloc(numDims * sizeof(int));

   if (varDimID == NULL) {
     fprintf(stderr, "Failed to allocate memory for dimension flags.\n");
     nc_close(ncid);
     exit(1);
   }

   myVar = (float *) malloc((size_t) numGridPoints * sizeof(float));

   if (myVar == NULL) {
     fprintf(stderr, "Failed to allocate memory for NetCDF variables.\n");
     nc_close(ncid);
     exit(1);
   }

/* Create the GRIB2 fields. */

   for (varID = 0 ; varID < numVars ; varID++) {

     if (!varProcessFlag[varID]) continue ;

  /* Get variable info. */

     status = nc_inq_var(ncid, varID, varName, &varDataType,
                         &varNumDims, varDimID, &varNumAtts);

     //killme
     //     printf ( "Converting variable \"%s\" to GRIB2.\n", varName ) ;

  /* Read "start_date" attribute. */

     //     status = nc_get_att_char_or_string(ncid, varID, "start_date", varName,
     //                                        Starttimestr);
     //status = nc_get_att_text(ncid, varID, "start_date", startTimeStr);
   //     if (status != NC_NOERR) {
   //       fprintf(stderr,
   //               "Failed to get \"start_date\" attribute for "
   //               "variable \"%s\".\n",
   //               varName);
   //       nc_close(ncid);
   //       exit(1);
   //     }

   //     if (strlen(startTimeStr) != 23) {
   //       fprintf(stderr,
   //               "\"start_date\" attribute for variable \"%s\" "
   //               "is not of the form \"YYYY-mm-dd HH:MM:SS UTC\".\n",
   //               varName);
   //       nc_close(ncid);
   //       exit(1);
   //     }

  /* Chop the " UTC" off the "start_date". */

   //     startTimeStr[19] = '\0';

  /* Break the date into components. */

   //     if (! deconstruct_date(startTimeStr, &startYear, &startMonth,
   //                            &startDay, &startHour, &startMinute,
   //                            &startSecond)) {
   //       fprintf(stderr,
   //               "Function deconstruct_date failed for \"start_date\" "
   //               "attribute \"%s\" of variable \"%s\".\n",
   //               startTimeStr, varName);
   //       nc_close(ncid);
   //       exit(1);
   //     }

   //     startTime = datetime_to_epoch_utc(startYear, startMonth, startDay,
   //                                       startHour, startMinute,
   //                                       startSecond);

  /* Read "units" attribute. */

     for (loc = 0 ; loc < NC_MAX_NAME ; loc++) {
       units[loc] = '\0';
     }

     status = nc_get_att_char_or_string(ncid, varID, "units", varName,
                                        units);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "Failed to get \"units\" attribute for variable "
               "\"%s\".\n",
               varName);
       nc_close(ncid);
       exit(1);
     }

  /* Read "_FillValue" attribute. */

     status = nc_get_att_float(ncid, varID, "_FillValue", &ndvNetCDF);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "Failed to get \"_FillValue\" attribute for variable "
               "\"%s\".\n",
               varName);
       nc_close(ncid);
       exit(1);
     }

     /* Read CF conventions "standard_name" attribute. */

     status = nc_get_att_char_or_string(ncid, varID, "standard_name",
                                        varName, standardName);

     if (status != NC_NOERR) {
       fprintf(stderr,
               "Failed to get \"standard_name\" attribute for variable "
               "\"%s\".\n",
               varName);
       nc_close(ncid);
       exit(1);
     }

  /****************************************************************
   * Prepare to call GRIB2 function g2_addfield for each variable *
   ****************************************************************/

     /** SECTION 4. PRODUCT DEFINITION SECTION **/

     /* Product definition template number */

     unitSlope = 1.0;
     unitIntercept = 0.0;
     D_packing = 6; 

  /* Set parameter category/number (code table 4.1/4.2) */

     if (!strcmp(standardName, "thickness_of_snowfall_amount")) {

       ipdsnum = 8; /* Average, accumulation, extreme values or other *
                     * statistically processed values at a horizontal *
                     * level or in a horizontal layer in a continuous *
                     * or non-continuous time interval.               */

       ipdstmpl[0] = 1; // Moisture
       ipdstmpl[1] = 29; // Total Snowfall, m, "ASNOW"
       D_packing = 5; // 10^-2 mm precision
       strcpy(expectedUnits, "m");

    /* Set type of generating process */

       ipdstmpl[2] = 0; // Analysis

    /* Background generating process identifier *
       (defined by originating center)          */

       ipdstmpl[3] = missing;

    /* Analysis or forecast generating process identifier */

       ipdstmpl[4] = missing;

       timeDiff = difftime(stopTime, startTime);
       remainder = fmod(timeDiff, 3600.0);
       if (remainder != 0) {
         fprintf(stderr,
                 "Time range must be integer hours (%d sec is not).\n",
                 (int) timeDiff);
         nc_close(ncid);
         exit(1);
       }
       timeDiffHours = (g2int) (timeDiff / 3600.0);

    /* Hours of observational data cutoff after reference time */

       ipdstmpl[5] = missing;

    /* Minutes of observational data cutoff after reference time */

       ipdstmpl[6] = missing;

    /* Indicator of unit of time range (must be hours) */

       ipdstmpl[7] = 1;

    /* Forecast time in units defined by previous value */

       ipdstmpl[8] = 0;

    /* Type of first fixed surface */

       ipdstmpl[9] = 1 ; // Ground or water surface

    /* Scale factor of first fixed surface */

       ipdstmpl[10] = 0 ;

    /* Scaled value of first fixed surface */

       ipdstmpl[11] = 0 ;

    /* Type of second fixed surface */

       ipdstmpl[12] = missing ;

    /* Scale factor of second fixed surface */

       ipdstmpl[13] = missing ;

    /* Scaled value of second fixed surface */

       ipdstmpl[14] = missing ;

    /* Year/Month/Day/Hour/Minute/Second - end of overall time interval */

       ipdstmpl[15] = (g2int) stopYear;
       ipdstmpl[16] = (g2int) stopMonth;
       ipdstmpl[17] = (g2int) stopDay;
       ipdstmpl[18] = (g2int) stopHour;
       ipdstmpl[19] = (g2int) stopMinute;
       ipdstmpl[20] = (g2int) stopSecond;

    /* Number of time range specifications describing the time intervals *
     * used to calculate the statistically-processed field.              */

       ipdstmpl[21] = 1;

    /* Total number of data values missing in statistical process. */

       ipdstmpl[22] = 0;

  /*** The next 6 fields define octets 47-58  for product definition ***
   *** template 4.8, and are tricky.                                 ***/

    /* Statistical process used to calculate the processed field from the *
     * field at each time increment during the time range.                */

       ipdstmpl[23] = 1; // Accumulation

    /* Type of time increment between successive fields used in the *
     * statistical processing.                                      */

       ipdstmpl[24] = 2; /* Successive times processed have same start time *
                          * of forecast, forecast time is incremented.      */

    /* Indicator of unit of time for range over which statistical processing *
     * is done.                                                              */

       ipdstmpl[25] = 1; // Hours

    /* Length of time range over which statistical processing is done. */

       ipdstmpl[26] = timeDiffHours;

    /* Indicator of unit of time for the increment between the succesive *
     * fields used.                                                      */

       ipdstmpl[27] = 1; // Hours

    /* Time increment between successive fields. */

       ipdstmpl[28] = 0;

     // END OF PRODUCT DEFINITION TEMPLATE 4.8

     /*
     if ( ! strcmp ( varName, "Snowf" ) ) {

       ipdstmpl[0] = 1 ;  // Moisture
       ipdstmpl[1] = 66 ; // Snow Precipitation Rate, kg/m2s, "SPRATE"
       D_packing = 6 ;

       strcpy ( expectedUnits, "kg/m2s" ) ;

     } else if ( ! strcmp ( varName, "Rainf" ) ) {

       ipdstmpl[0] = 1 ;  // Moisture
       ipdstmpl[1] = 65 ; // Rain Precipitation Rate, kg/m2s, "RPRATE"
       D_packing = 6 ;

       strcpy ( expectedUnits, "kg/m2s" ) ;

     } else if ( ! strcmp ( varName, "AvgSurfT" ) ) {

       ipdstmpl[0] = 0 ;  // Temperature
       ipdstmpl[1] = 17 ; // Skin Temperature, K, "SKINT"

       strcpy ( expectedUnits, "K" ) ;

     } else if ( ! strcmp ( varName, "SWE" ) ) {

       ipdstmpl[0] = 1 ;  // Moisture
       ipdstmpl[1] = 60 ; // Snow Depth Water Equivalent, kg/m2, "SDWE"

       strcpy ( expectedUnits, "kg/m2" ) ;

     } else if ( ! strcmp ( varName, "SnowDepth" ) ) {

       ipdstmpl[0] = 1 ;  // Moisture
       ipdstmpl[1] = 11 ; // Snow Depth, m, "SNOD"
       D_packing = 3 ;

       strcpy ( expectedUnits, "m" ) ;

     } else if ( ! strcmp ( varName, "Rainf_f" ) ) {

       ipdstmpl[0] = 1 ;  // Moisture
       ipdstmpl[1] = 7 ;  // Precipitation Rate, kg/m2s, "PRATE"
       D_packing = 6 ;

       strcpy ( expectedUnits, "kg/m2s" ) ;

     } else if ( ! strcmp ( varName, "Tair_f" ) ) {

       ipdstmpl[0] = 0 ;  // Temperature
       ipdstmpl[1] = 0 ;  // Tmperature, K, "TMP"

       strcpy ( expectedUnits, "K" ) ;
     */


     } else if (!strcmp(standardName, "snow_density")) {

       ipdsnum = 8; /* Average, accumulation, extreme values or other *
                     * statistically processed values at a horizontal *
                     * level or in a horizontal layer in a continuous *
                     * or non-continuous time interval.               */

       ipdstmpl[0] = 1; // Moisture
       ipdstmpl[1] = 61; // Snow Density, kg m-3, "SDEN"
       D_packing = 5; // 10^-2 mm precision
       strcpy(expectedUnits, "kg m-3");

    /* Set type of generating process */

       ipdstmpl[2] = 0; // Analysis

    /* Background generating process identifier *
       (defined by originating center)          */

       ipdstmpl[3] = missing;

    /* Analysis or forecast generating process identifier */

       ipdstmpl[4] = missing;

       timeDiff = difftime(stopTime, startTime);
       remainder = fmod(timeDiff, 3600.0);
       if (remainder != 0) {
         fprintf(stderr,
                 "Time range must be integer hours (%d sec is not).\n",
                 (int) timeDiff);
         nc_close(ncid);
         exit(1);
       }
       timeDiffHours = (g2int) (timeDiff / 3600.0);

    /* Hours of observational data cutoff after reference time */

       ipdstmpl[5] = missing;

    /* Minutes of observational data cutoff after reference time */

       ipdstmpl[6] = missing;

    /* Indicator of unit of time range (must be hours) */

       ipdstmpl[7] = 1;

    /* Forecast time in units defined by previous value */

       ipdstmpl[8] = 0;

    /* Type of first fixed surface */

       ipdstmpl[9] = 1 ; // Ground or water surface

    /* Scale factor of first fixed surface */

       ipdstmpl[10] = 0 ;

    /* Scaled value of first fixed surface */

       ipdstmpl[11] = 0 ;

    /* Type of second fixed surface */

       ipdstmpl[12] = missing ;

    /* Scale factor of second fixed surface */

       ipdstmpl[13] = missing ;

    /* Scaled value of second fixed surface */

       ipdstmpl[14] = missing ;

    /* Year/Month/Day/Hour/Minute/Second - end of overall time interval */

       ipdstmpl[15] = (g2int) stopYear;
       ipdstmpl[16] = (g2int) stopMonth;
       ipdstmpl[17] = (g2int) stopDay;
       ipdstmpl[18] = (g2int) stopHour;
       ipdstmpl[19] = (g2int) stopMinute;
       ipdstmpl[20] = (g2int) stopSecond;

    /* Number of time range specifications describing the time intervals *
     * used to calculate the statistically-processed field.              */

       ipdstmpl[21] = 1;

    /* Total number of data values missing in statistical process. */

       ipdstmpl[22] = 0;

  /*** The next 6 fields define octets 47-58  for product definition ***
   *** template 4.8, and are tricky.                                 ***/

    /* Statistical process used to calculate the processed field from the *
     * field at each time increment during the time range.                */

       ipdstmpl[23] = 1; // Accumulation

    /* Type of time increment between successive fields used in the *
     * statistical processing.                                      */

       ipdstmpl[24] = 2; /* Successive times processed have same start time *
                          * of forecast, forecast time is incremented.      */

    /* Indicator of unit of time for range over which statistical processing *
     * is done.                                                              */

       ipdstmpl[25] = 1; // Hours

    /* Length of time range over which statistical processing is done. */

       ipdstmpl[26] = timeDiffHours;

    /* Indicator of unit of time for the increment between the succesive *
     * fields used.                                                      */

       ipdstmpl[27] = 1; // Hours

    /* Time increment between successive fields. */

       ipdstmpl[28] = 0;

     // END OF PRODUCT DEFINITION TEMPLATE 4.8

     } else {

       fprintf(stderr, "Unsupported standard name \"%s\".\n", standardName);
       nc_close(ncid);
       exit(1);

     }

     if (strcmp(units, expectedUnits)) {
       fprintf(stderr,
               "Variable \"%s\" with standard name \"%s\" has units "
               "\"%s\"; expected \"%s\".\n",
               varName, standardName, units, expectedUnits);
       nc_close(ncid);
       exit(1);
     }

 /** SECTION 5. DATA REPRESENTATION SECTION **/

  /* Data representation template number */

     // 0 : "Simple Packing"
     //     Product template has 11 values, described by template 5.0.
     //     Data are stored as integers X, and output values Y are calculated
     //     by
     //         Y = ( R + X * 2^E ) / 10^D

     // 40 : JPEG2000 compression
     //      Product template has 11 values, described by template 5.40.

     idrsnum = 40 ;

 /** READ DATA **/

     status = nc_get_var_float ( ncid, varID, myVar ) ;

     if (status != NC_NOERR) {
       fprintf ( stderr, "Failed to read variable \"%s\".\n", varName ) ;
       nc_close ( ncid ) ;
       exit ( 1 ) ;
     }


  /* Initialize data and bitmask arrays for GRIB2. */

     for ( loc = 0 ; loc < numGridPoints ; loc++ ) {

       grib2Data[loc] = ndvGRIB2 ;
       grib2Mask[loc] = 0 ;

     }

  /* Copy data to GRIB2 array. */

     // We confirmed above that the NetCDF data run south-to-north.
     // For our GRIB2 output that array must be flipped.

     loc = 0 ;
     E_packing = 0 ;
     pow10D = pow ( 10.0, D_packing ) ;

     for ( row = 0 ; row < dimSize_y ; row++ ) {

       for ( col = 0 ; col < dimSize_x ; col++ ) {

         value = myVar[row * dimSize_x + col] ;
 
         if ( value != ndvNetCDF ) {

           grib2Mask[loc] = 1 ;

           newVal = ( double ) ( long ) ( ( unitSlope * value +
                                            unitIntercept ) * pow10D + 0.5 ) /
                    pow10D ;

           grib2Data[loc] = ( g2float ) newVal ;

           if ( newVal > maxVal ) maxVal = newVal ;
           if ( newVal < minVal ) minVal = newVal ;

         }

         loc++ ;

       }

     }

     if ( minVal > maxVal ) {

       minVal = 0.0 ;
       maxVal = 0.0 ;

     }

  /* Reference value (R) */

     R_packing = ( g2int ) minVal ;

     idrstmpl[0] = R_packing ;

  /* Binary scale factor (E) */

     idrstmpl[1] = E_packing ;

  /* Decimal scale factor (D) */

     idrstmpl[2] = D_packing ;

  /* Bits used for each packed value */

     idrstmpl[3] = 32 ;

  /* Type of original field values */

     idrstmpl[4] = 0 ; // Floating point

  /* Type of compression used */

     idrstmpl[5] = 0 ; // Lossless compression

  /* Target compression ratio */

     idrstmpl[6] = missing ; // Only used for lossy compression


 /** SECTION 6. BITMAP SECTION **/

     g2int ibmap ; // Bitmap indicator

     ibmap = 0 ;   // A bit map applies to this product and is specified here.

     ierr = g2_addfield ( grib2Buffer,
                          ipdsnum, ipdstmpl,
                          NULL, 0,
                          idrsnum, idrstmpl,
                          grib2Data, ( g2int ) numGridPoints,
                          ibmap, grib2Mask ) ;

     if ( ierr <= 0 ) {
       fprintf ( stderr,
                 "Failed to add GRIB2 field for \"%s\".\n",
                 varName ) ;
       nc_close ( ncid ) ;
       exit ( 1 ) ;
     }

     grib2Size = ierr ;

     //printf ( "g2_addfield sez size of GRIB2 message is now %ld\n",
     //         grib2Size ) ;

   }

  
/* Finish GRIB2 buffer. */

   ierr = g2_gribend ( grib2Buffer ) ;

   if ( ierr <= 0 ) {
     fprintf ( stderr, "g2_gribend failed.\n" ) ;
     nc_close ( ncid ) ;
     exit ( 1 ) ;
  }

   grib2Size = ierr ;

   //printf ( "g2_gribend sez size of GRIB2 messasge is now %ld\n", grib2Size ) ;


/* Close NetCDF file. */

   nc_close ( ncid ) ;



/* Write the GRIB2 file. */

   int grib2File ;
   //   char grib2FilePath[] = "out.grib2";

   grib2File = -1 ;

   grib2File = open ( outputPath, O_CREAT | O_RDWR, 0664 ) ;
   if ( grib2File == -1 ) {
     fprintf ( stderr,
               "Failed to open output GRIB2 file %s.\n", outputPath ) ;
     exit ( 1 ) ;
   }

   if ( write ( grib2File, grib2Buffer, grib2Size ) == -1 ) {
     fprintf ( stderr,
               "Failed to write GRIB2 buffer to file %s.\n", outputPath ) ;
     exit ( 1 ) ;
   }


   close ( grib2File ) ;

   exit ( 0 ) ;

}
