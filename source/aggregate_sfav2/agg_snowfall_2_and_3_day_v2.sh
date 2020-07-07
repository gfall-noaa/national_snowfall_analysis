#!/bin/sh

# Script to produce 48- and 72-hour snowfall aggregations.

# Greg Fall, OWP-Chanhassen
# Kent Sparrow, NWC, Tuscaloosa
# January 2017
#
# Changes:
#
#   2019-06-28, GF: Moved sourcing of /etc/bashrc above function
#                   definitions to ensure pathmunge and ldpathmunge
#                   functions are not unset.
#   2019-09-30, GF: Added a fancier usage message (including the
#                   "wrecho_hang" function).
#                   Added the -m option for email recipient/s.
#                   Modified logic for generating output layers, so
#                   that both "os" and "ds" hostnames DO generate them
#                   by default.

# Source the file used to set up login shells.

. /etc/bashrc

wrecho_hang() {
    if [ -z $HANG_COLS ] ; then
        HANG_COLS=0
    fi
    if ! tty -s ; then
        TERMINAL_WIDTH=80
    else
        TERMINAL_WIDTH=$(tput cols)
    fi
    while [ $((TERMINAL_WIDTH-HANG_COLS)) -lt 8 ] && \
          [ $HANG_COLS -gt 0 ] ; do
        HANG_COLS=$((HANG_COLS-1))
    done
    echo $* | fmt -t -w $((TERMINAL_WIDTH-HANG_COLS)) | \
      sed s/^\ \ \ /"$(printf '%*s' $HANG_COLS)"/g
}

usage() {
    echo ""
    echo -n "Usage: agg_snowfall_2_and_3_day_v2.sh "
    HANG_COLS=38
    wrecho_hang "[-h,-?] [-l] [-u] [-e] [-m EMAIL]" \
                "[DATE_YYYYMMDDHH]"
    echo ""
    echo -n "Options: -h  "
    HANG_COLS=13
    wrecho_hang "Show this message (-? has the same effect)."
    echo -n "         -l  "
    wrecho_hang "Disable generation of GISRS layers from output" \
                "grids."
    echo -n "         -u  "
    wrecho_hang "Use a user-specific \"busy\" file. The number of" \
                "simultaneous instances of the program will still" \
                "be limited to MAX_INSTANCES, but those will be" \
                "counted separately for each user."
    echo -n "         -e  "
    wrecho_hang "Use the \"extended\" color ramp introduced in late" \
                "2018, which includes a series of levels for" \
                "snowfall amounts exceeding 48 inches."
    echo -n "         -m {EMAIL}  "
    HANG_COLS=21
    wrecho_hang "Use the indicated email, rather than the hard-coded" \
                "default."
    echo ""
    echo -n "         "
    HANG_COLS=9
    wrecho_hang "Including the [DATE_YYYYMMDDHH] argument" \
                "performs aggregations ending on the indicated" \
                "date/time, rather than the default, which is" \
                "the most recent 12Z crossing (though this must" \
                "have occurred at least one hour ago to use the" \
                "current date)."
    echo ""
}

pathmunge () {
  if ! echo $PATH | /bin/egrep -q "(^|:)$1($|:)" ; then
    if [ "$2" = "after" ] ; then
      export PATH=$PATH:$1
    else
      export PATH=$1:$PATH
    fi
  fi
}

ldpathmunge () {
  if [ -z "$LD_LIBRARY_PATH" ] ; then
    export LD_LIBRARY_PATH=$1
  else
    if ! echo $LD_LIBRARY_PATH | /bin/egrep -q "(^|:)$1($|:)" ; then
      if [ "$2" = "after" ] ; then
        export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$1
      else
        export LD_LIBRARY_PATH=$1:${LD_LIBRARY_PATH}
      fi
    fi
  fi
}

check_rt_idl () {
    if [ -z "$LMSTAT" ] ; then
        return 1
    fi
    if [ ! -x "$LMSTAT" ] ; then
        return 1
    fi
    LINE=`$LMSTAT -f idl_rt | \
          grep "Users of idl_rt.*licenses issued.*licenses in use"`
    STATUS=$?
    if [ $STATUS != 0 ] ; then
        return 1
    else
        IDL_RT_ISSUED=`echo "$LINE" | \
                sed 's/^[^0-9]*//g' | \
                sed 's/\ licenses\ issued.*//g'`
        if [ -z "$IDL_RT_ISSUED" ] ; then
            return 1
        fi
        TEST=`echo "$IDL_RT_ISSUED" | sed 's/[0-9]//g'`
        if [ -n "$TEST" ] ; then
            return 1
        fi
        IDL_RT_ISSUED=$((IDL_RT_ISSUED/6))
#       Override inaccurate result for ds machines.
        if [ "${HOSTNAME:0:2}" = "ds" ] && [ $IDL_RT_ISSUED -eq 38 ] ; then
            IDL_RT_ISSUED=6
        fi
        IDL_RT_IN_USE=`echo "$LINE" | \
                sed 's/.*licenses\ issued[^0-9]*//g' | \
                sed 's/[^0-9]*$//g'`
        if [ -z "$IDL_RT_IN_USE" ] ; then
            return 1
        fi
        TEST=`echo "$IDL_RT_IN_USE" | sed 's/[0-9]//g'`
        if [ -n "$TEST" ] ; then
            return 1
        fi
        IDL_RT_IN_USE=$((IDL_RT_IN_USE/6))
        IDL_RT_AVAILABLE=$((IDL_RT_ISSUED-IDL_RT_IN_USE))
        return 0
    fi
}

# Check command line arguments.

MAX_INSTANCES=4

# Parse command line.

while getopts "h?luem:" OPT ; do
    case $OPT in
        h|\?)
            usage
            exit
            ;;
        l)
            MAKE_LAYERS=0
            ;;
        u)
            BUSY_FILE=AGG_SNOWFALL_V2_$(echo $USER | tr [a-z] [A-Z])_BUSY
            ;;
        e)
            EXTENDED_COLORS=1
            ;;
        m)
            EMAIL="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))

# Set key environment variables for process/log management.

if [ -z "$EMAIL" ] ; then
    if [ -z "$GISRS_MAIL" ] ; then
        EMAIL=Gregory.Fall@noaa.gov
    else
        EMAIL="$GISRS_MAIL"
    fi
fi
if [ -z "$BUSY_FILE" ] ; then
    BUSY_FILE=AGG_SNOWFALL_V2_BUSY
fi
if [ -z $MAKE_LAYERS ] ; then
  if [ "${HOSTNAME:0:2}" = "ds" ] || \
     [ "${HOSTNAME:0:2}" = "os" ] ; then
    MAKE_LAYERS=1
  else
    MAKE_LAYERS=0
  fi
fi

if [ "${HOSTNAME:0:2}" = "ds" ] || \
   [ "${HOSTNAME:0:2}" = "os" ] ; then

    if [ -z "$GISRS_SCHEMA" ] ; then
        echo "Missing GISRS_SCHEMA environment variable." 1>&2
        exit 1
    fi

    if [ $MAKE_LAYERS -eq 1 ] ; then
        pathmunge ${NSA_PREFIX}/gisrs/bin/iwrss
        if ! which static_layer_loader 1>/dev/null 2>&1 ; then
            echo "Missing static_layer_loader in PATH." 1>&2
            exit 1
        fi
    fi
fi

# Locate shell utilities.

if [ -z "$NSA_PREFIX" ] ; then
    echo "Missing NSA_PREFIX environment variable." 1>&2
    exit 1
fi

EXEC_DIR="${NSA_PREFIX}/gisrs/idl/snowfall_v2"
if [ ! -d "$EXEC_DIR" ] ; then
    echo "Bad configuration - missing ${EXEC_DIR}" \
         "directory" 1>&2
    exit 1
fi

AGG_SAV_PATH=${EXEC_DIR}/aggregate_snowfall_v2.sav
if [ ! -f "$AGG_SAV_PATH" ] ; then
    echo "Missing $AGG_SAV_PATH" 1>&2
    exit 1
fi

UTILS_DIR="${EXEC_DIR}/utils"
if [ ! -d "$UTILS_DIR" ] ; then
    echo "Bad configuration - missing ${UTILS_DIR}" \
         "directory" 1>&2
    exit 1
fi

# Add SFAV2_UTILS_DIR to the PATH so items in that directory
# (e.g. gdal_translate) are available.

pathmunge $SFAV2_UTILS_DIR

# Source common functions.

INCLUDE_PATH=${UTILS_DIR}/cron_script_functions.sh
if [ ! -f "$INCLUDE_PATH" ] ; then
    echo "Missing $INCLUDE_PATH" 1>&2
    exit 1
fi
. "$INCLUDE_PATH"

# Source key process/log management "start" code.

INCLUDE_PATH=${UTILS_DIR}/nwcdev_script_top.sh
if [ ! -f "$INCLUDE_PATH" ] ; then
    err_msg "Missing $INCLUDE_PATH"
    err_out
fi
. "$INCLUDE_PATH"

# Verify nwcdev_script_top.sh has defined and created the LOG_FILE_PATH.

if [ -z "$LOG_FILE_PATH" ] || \
   [ ! -f ${LOG_FILE_PATH} ] ; then
    err_msg "Missing LOG_FILE_PATH variable."
    err_out
fi


###############
# MAIN SCRIPT #
###############


# Define required directories.

SFAV2_RESOURCES_DIR="${EXEC_DIR}/resources"
if [ ! -d "$SFAV2_RESOURCES_DIR" ] ; then
    err_msg "Bad configuration - missing ${SFAV2_RESOURCES_DIR}" \
            "directory."
    err_out
fi
export SFAV2_RESOURCES_DIR

SFAV2_INPUT_OUTPUT_DIR="${NSA_PREFIX}/misc/snowfall_v2"
if [ ! -d "$SFAV2_INPUT_OUTPUT_DIR" ] ; then
    err_msg "Bad configuration - missing ${SFAV2_INPUT_OUTPUT_DIR}" \
            "directory."
    err_out
fi
export SFAV2_INPUT_OUTPUT_DIR

# System information, needed for logging/locking and for deciding whether
# to copy data to web server.

if [ -z "$ENVIRONMENT" ] ; then
    HOSTNAME=`hostname -s`
    SYSTEM=`hostname -f | awk -F "." '{print $3}'`
    ENVIRONMENT=`hostname | cut -c 1` # Either "o" or "d"
    if [ "$ENVIRONMENT" == "o" ] ; then
        ENVIRONMENT=operations
    elif [ "$ENVIRONMENT" == "d" ] ; then
        ENVIRONMENT=development
    else
        err_msg "Server name $HOSTNAME does not suggest a supported" \
                "environment."
        err_out
    fi
    export ENVIRONMENT
fi

# Check for an output directory for web imagery.

if [ "$ENVIRONMENT" = "operations" ] ; then
  WEB_DIR=${NSA_PREFIX}/web/snowfall_v2/data
else
  WEB_DIR=/net/http0/htdocs/snowfall_v2/data
fi
if [ ! -d $WEB_DIR ] || [ ! -w $WEB_DIR ] ; then
    warning_msg "web output directory ${WEB_DIR} not found or not writable."
    WEB_DIR=
fi

# Reset the LD_LIBRARY_PATH to avoid "owp_common" versions of HDF and
# NetCDF libraries, which interfere with certain utilities (e.g.,
# gdal_translate).

OLD_LD_LIBRARY_PATH=$LD_LIBRARY_PATH
MY_LD_LIBRARY_PATH=/usr/lib64
export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH

# Set aggregation parameters. Currently this is limited to a check for
# using extended colors.

if [ -z $EXTENDED_COLORS ] ; then
    EXTENDED_COLORS=0
fi
export EXTENDED_COLORS

# Impose a lag to prevent processing 12Z too early.

LAG_SECONDS=3600

# Check for a command line argument for DATE_YYYYMMDDHH.

if [ $# -eq 1 ] ; then
    DATE_YYYYMMDDHH=$1
    TEST=`echo "$DATE_YYYYMMDDHH" | sed 's/[0-9]//g'`
    if [ ${#DATE_YYYYMMDDHH} -ne 10 ] || [ ${#TEST} -ne 0 ] ; then
        err_msg "Invalid DATE_YYYYMMDDHH argument \"${DATE_YYYYMMDDHH}\"."
        err_out
    fi
else
    EFF_LAG_SEC=$((LAG_SECONDS+43200))
    DATE_YYYYMMDDHH=`date -u --date="-${EFF_LAG_SEC} seconds" "+%Y%m%d12"`
fi
DATE_YYYY=${DATE_YYYYMMDDHH:0:4}
DATE_MM=${DATE_YYYYMMDDHH:4:2}
DATE_DD=${DATE_YYYYMMDDHH:6:2}
DATE_HH=${DATE_YYYYMMDDHH:8:2}

export DATE_YYYYMMDDHH
export DURATION_HOURS=24

# Set up idl and lmstat.

IDL_DIR=/opt/rsi/idl
if [ ! -d ${IDL_DIR} ] ; then
    err_msg "IDL directory $IDL_DIR not found."
    err_out
fi
IDL=${IDL_DIR}/bin/idl
if [ ! -x ${IDL} ] ; then
    err_msg "IDL executable $IDL not found."
    err_out
fi
LMSTAT=${IDL_DIR}/bin/lmstat
if [ ! -x ${LMSTAT} ] ; then
    err_msg "lmstat executable $LMSTAT not found."
    err_out
fi

# Only proceed if at least 3 IDL runtime licenses are available.

if ! check_rt_idl ; then
    err_msg "Failed to check IDL runtime licenses."
    err_out
fi
TIME_START=`date -u +%s`
TIME_FINISH=`date -u +%s`
WAIT_TIME=$((TIME_FINISH-TIME_START))
MAX_WAIT_TIME=14400
while [ $IDL_RT_AVAILABLE -lt 3 ] && \
    [ $WAIT_TIME -lt $MAX_WAIT_TIME ] ; do
    usr_msg "Waiting for IDL runtime license."
    sleep 60
    if ! check_rt_idl ; then
        err_msg "Failed to check IDL runtime licenses."
        err_out
    fi
    TIME_FINISH=`date -u +%s`
    WAIT_TIME=$((TIME_FINISH-TIME_START))
done
if [ $WAIT_TIME -ge $MAX_WAIT_TIME ] ; then
    err_msg "Failed to get an IDL runtime license."
    err_out
fi

# Perform 48-hour aggregation.

export AGGREGATE_HOURS=48
$IDL -quiet -rt=${AGG_SAV_PATH}

STATUS=$?
if [ $STATUS -ne 0 ] ; then
    err_msg "FATAL: ${AGGREGATE_HOURS}-hour snowfall aggregation failed" \
            "for ${DATE_YYYYMMDDHH}."
    err_out
fi

# Verify 48-hour output files were created.

OUTPUT_DIR=${SFAV2_INPUT_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}
if [ ! -d $OUTPUT_DIR ] ; then
    err_msg "Missing output directory $OUTPUT_DIR"
    err_out
fi
OUTPUT_FILE=sfav2_CONUS_${AGGREGATE_HOURS}h_${DATE_YYYYMMDDHH}
OUTPUT_NC_FILE=${OUTPUT_FILE}.nc
OUTPUT_PNG_FILE=${OUTPUT_FILE}.png
OUTPUT_TIFF_FILE=${OUTPUT_FILE}.tif
if [ ! -e ${OUTPUT_DIR}/${OUTPUT_NC_FILE} ] ; then
    err_msg "Missing output NetCDF file ${OUTPUT_DIR}/${OUTPUT_NC_FILE}"
    err_out
fi
if [ ! -e ${OUTPUT_DIR}/${OUTPUT_PNG_FILE} ] ; then
    err_msg "Missing output PNG file ${OUTPUT_DIR}/${OUTPUT_PNG_FILE}"
    err_out
fi
if [ ! -e ${OUTPUT_DIR}/${OUTPUT_TIFF_FILE} ] ; then
    err_msg "Missing output GeoTIFF file ${OUTPUT_DIR}/${OUTPUT_TIFF_FILE}"
    err_out
fi

if [ $MAKE_LAYERS -eq 1 ] ; then

#   Generate a GISRS layer.

    OUTPUT_SNFL_NC_PATH="${OUTPUT_DIR}/${OUTPUT_NC_FILE}"
    OUTPUT_GISRS_LAYER="CONUS Snowfall (${AGGREGATE_HOURS} hours) ${DATE_YYYY}-${DATE_MM}-${DATE_DD} ${DATE_HH} Snowf"
    CONFIG_PATH="${OUTPUT_SNFL_NC_PATH}.conf"

    echo "product_group = \"OPPS Snowfall\"
          connection_string = \"\"
          schema = \"${GISRS_SCHEMA}\"
          force_write = true
          fate = temporary 30" >${CONFIG_PATH}

    export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
    ldpathmunge ${NSA_PREFIX}/gisrs/bin/iwrss
    static_layer_loader -l ${LOG_FILE_PATH} \
                        -c "${CONFIG_PATH}" \
                        "${OUTPUT_SNFL_NC_PATH}" \
                        "${OUTPUT_GISRS_LAYER}"
    STATUS=$?
    rm -f "${CONFIG_PATH}"
    if [ $STATUS -ne 0 ] ; then
        err_msg "FATAL: Conversion of ${AGGREGATE_HOURS}-hour" \
                "aggregation to GISRS layer failed for ${DATE_YYYYMMDDHH}."
        err_out
    fi
    export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
fi

if [ -n "$WEB_DIR" ] ; then

#   Copy 48-hour output files to the web.

    WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
    if [ ! -d $WEB_OUTPUT_DIR ] ; then
        mkdir -m 2775 $WEB_OUTPUT_DIR
    fi
    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_NC_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_NC_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi

    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_NC_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_NC_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi


    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_PNG_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_PNG_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi

    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_TIFF_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_TIFF_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi
    usr_msg "Copied NetCDF, PNG, and GeoTIFF files to ${WEB_OUTPUT_DIR}"

fi

# Only proceed if at least 3 IDL runtime licenses are available.

if ! check_rt_idl ; then
    err_msg "Failed to check IDL runtime licenses."
    err_out
fi
TIME_START=`date -u +%s`
TIME_FINISH=`date -u +%s`
WAIT_TIME=$((TIME_FINISH-TIME_START))
MAX_WAIT_TIME=14400
while [ $IDL_RT_AVAILABLE -lt 3 ] && \
    [ $WAIT_TIME -lt $MAX_WAIT_TIME ] ; do
    usr_msg "Waiting for IDL runtime license."
    sleep 60
    if ! check_rt_idl ; then
        err_msg "Failed to check IDL runtime licenses."
        err_out
    fi
    TIME_FINISH=`date -u +%s`
    WAIT_TIME=$((TIME_FINISH-TIME_START))
done
if [ $WAIT_TIME -ge $MAX_WAIT_TIME ] ; then
    err_msg "Failed to get an IDL runtime license."
    err_out
fi

# Perform 72-hour aggregation.

export AGGREGATE_HOURS=72
$IDL -quiet -rt=${AGG_SAV_PATH}

STATUS=$?
if [ $STATUS -ne 0 ] ; then
    err_msg "FATAL: ${AGGREGATE_HOURS}-hour snowfall aggregation failed" \
            "for ${DATE_YYYYMMDDHH}."
    err_out
fi

# Verify 72-hour output files were created.

OUTPUT_DIR=${SFAV2_INPUT_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}
if [ ! -d $OUTPUT_DIR ] ; then
    err_msg "Missing output directory $OUTPUT_DIR"
    err_out
fi
OUTPUT_FILE=sfav2_CONUS_${AGGREGATE_HOURS}h_${DATE_YYYYMMDDHH}
OUTPUT_NC_FILE=${OUTPUT_FILE}.nc
OUTPUT_PNG_FILE=${OUTPUT_FILE}.png
OUTPUT_TIFF_FILE=${OUTPUT_FILE}.tif
if [ ! -e ${OUTPUT_DIR}/${OUTPUT_NC_FILE} ] ; then
    err_msg "Missing output NetCDF file ${OUTPUT_DIR}/${OUTPUT_NC_FILE}"
    err_out
fi
if [ ! -e ${OUTPUT_DIR}/${OUTPUT_PNG_FILE} ] ; then
    err_msg "Missing output PNG file ${OUTPUT_DIR}/${OUTPUT_PNG_FILE}"
    err_out
fi
if [ ! -e ${OUTPUT_DIR}/${OUTPUT_TIFF_FILE} ] ; then
    err_msg "Missing output GeoTIFF file ${OUTPUT_DIR}/${OUTPUT_TIFF_FILE}"
    err_out
fi

if [ $MAKE_LAYERS -eq 1 ] ; then

#   Generate a GISRS layer.

    OUTPUT_SNFL_NC_PATH="${OUTPUT_DIR}/${OUTPUT_NC_FILE}"
    OUTPUT_GISRS_LAYER="CONUS Snowfall (${AGGREGATE_HOURS} hours) ${DATE_YYYY}-${DATE_MM}-${DATE_DD} ${DATE_HH} Snowf"
    CONFIG_PATH="${OUTPUT_SNFL_NC_PATH}.conf"

    echo "product_group = \"OPPS Snowfall\"
          connection_string = \"\"
          schema = \"${GISRS_SCHEMA}\"
          force_write = true
          fate = temporary 30" >${CONFIG_PATH}

    export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
    ldpathmunge ${NSA_PREFIX}/gisrs/bin/iwrss
    static_layer_loader -l ${LOG_FILE_PATH} \
                        -c "${CONFIG_PATH}" \
                        "${OUTPUT_SNFL_NC_PATH}" \
                        "${OUTPUT_GISRS_LAYER}"
    STATUS=$?
    rm -f "${CONFIG_PATH}"
    if [ $STATUS -ne 0 ] ; then
        err_msg "FATAL: Conversion of ${AGGREGATE_HOURS}-hour" \
                "aggregation to GISRS layer failed for ${DATE_YYYYMMDDHH}."
        err_out
    fi
    export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
fi

if [ -n "$WEB_DIR" ] ; then

#   Copy 72-hour output files to the web.

    WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
    if [ ! -d $WEB_OUTPUT_DIR ] ; then
        mkdir -m 2775 $WEB_OUTPUT_DIR
    fi
    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_NC_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_NC_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi

    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_NC_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_NC_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi


    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_PNG_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_PNG_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi

    if ( ! cp_gw ${OUTPUT_DIR}/${OUTPUT_TIFF_FILE} ${WEB_OUTPUT_DIR}/ ) ; then
        err_msg "Failed to copy ${OUTPUT_TIFF_FILE} to ${WEB_OUTPUT_DIR}."
        err_out
    fi
    usr_msg "Copied NetCDF, PNG, and GeoTIFF files to ${WEB_OUTPUT_DIR}"

fi


###################
# MAIN SCRIPT END #
###################

# Source key process/log management "finish" code and exit.

INCLUDE_PATH=${UTILS_DIR}/nwcdev_script_bottom.sh
if [ ! -f "$INCLUDE_PATH" ] ; then
    err_msg "Missing ${INCLUDE_PATH} file"
    err_out
fi
. $INCLUDE_PATH

