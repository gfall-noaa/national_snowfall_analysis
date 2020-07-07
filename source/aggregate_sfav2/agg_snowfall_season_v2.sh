#!/bin/sh

# Custom snowfall aggregation, intended mainly to do seasonal totals.

# Greg Fall, OWP-Chanhassen
# Kent Sparrow, NWC, Tuscaloosa
# January 2017

# Changes:
#
# 2017-10-02, GF & KS - removed "usr_out" call when program refuses to
#             accumulate for periods under 4 days (i.e., prior to
#             October 4). Program should exit quietly in that case.
# 2019-06-28, GF: Added definitions for pathmunge and ldpathmunge;
#                 moved sourcing of /etc/bashrc above those function
#                 definitions to ensure they are not unset.
# 2019-09-30, GF: Added a fancier usage message (including the
#                 "wrecho_hang" function).
#                 Added the -m option for email recipient/s.
#                 Modified logic for generating output layers, so that
#                 both "os" and "ds" hostnames DO generate them by
#                 default.
# 2019-10-02, GF: Only require that AGGREGATE_DAYS be greater than 3
#                 if FORCE_SEASONAL is unset. This allows us to use
#                 export FORCE_SEASONAL=1 in the calling process to
#                 produce seasonal totals for October 1-3.

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
    echo -n "Usage: agg_snowfall_season_v2.sh "
    HANG_COLS=33
    wrecho_hang "[-h,-?] [-l] [-u] [-m EMAIL] [DATE_YYYYMMDDHH]"
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

while getopts "h?lum:" OPT ; do
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
        m)
            EMAIL="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))

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
  if [ ${DATE_YYYYMMDDHH:8:2} -ne 12 ] ; then
    err_msg "WARNING: I REFUSE to aggregate to hours other than 12Z."
    err_out
  fi
else
  DATE_YYYYMMDDHH=`date -u --date="-${LAG_SECONDS} seconds" "+%Y%m%d12"`
fi

export DATE_YYYYMMDDHH

# Identify the start of the season as 12Z on 30 September prior to
# DATE_YYYYMMDDHH.

YYYY=${DATE_YYYYMMDDHH:0:4}
MM=${DATE_YYYYMMDDHH:4:2}
DD=${DATE_YYYYMMDDHH:6:2}
HH=${DATE_YYYYMMDDHH:8:2}
DATE="${YYYY}-${MM}-${DD} ${HH}:00:00"
HOURS_BACK=0
BROKE=0
while [ $HOURS_BACK -lt 8784 ] ; do # 366 * 24 = 8784
    HOURS_BACK=$((HOURS_BACK+1))
    START_DATE_YYYYMMDDHH=`date -u --date="-${HOURS_BACK} hours $DATE" \
                           "+%Y%m%d%H"`
    if [ "${START_DATE_YYYYMMDDHH:4:6}" == "093012" ] ; then
        BROKE=1
        break
    fi
done
if [ $BROKE -eq 0 ] ; then
    err_msg "FATAL: failed to find a START_DATE less than one year" \
            "previous to $DATE_YYYYMMDDHH}"
    err_out
fi

export DURATION_HOURS=24
D2="${YYYY}-${MM}-${DD} ${HH}:00:00"
D2_EPOCH=`date -u --date="$D2" +%s`
YYYY=${START_DATE_YYYYMMDDHH:0:4}
MM=${START_DATE_YYYYMMDDHH:4:2}
DD=${START_DATE_YYYYMMDDHH:6:2}
HH=${START_DATE_YYYYMMDDHH:8:2}
D1="${YYYY}-${MM}-${DD} ${HH}:00:00"
D1_EPOCH=`date -u --date="$D1" +%s`
AGGREGATE_SECONDS=$((D2_EPOCH-D1_EPOCH))
export AGGREGATE_HOURS=$((AGGREGATE_SECONDS/3600))
AGGREGATE_DAYS=$((AGGREGATE_HOURS/24))
if [ -z $FORCE_SEASONAL ] && [ $AGGREGATE_DAYS -le 3 ] ; then
  usr_msg "Aggregation must be a minimum of 4 days for seasonal snowfall."
#  usr_out
else

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
$IDL -quiet -rt=${AGG_SAV_PATH}

STATUS=$?
if [ $STATUS -ne 0 ] ; then
    err_msg "FATAL: ${AGGREGATE_DAYS}-day snowfall aggregation failed" \
            "for ${DATE_YYYYMMDDHH}."
    err_out
fi


# Verify output files were created.

OUTPUT_DIR=${SFAV2_INPUT_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}
if [ ! -d $OUTPUT_DIR ] ; then
    err_msg "Missing output directory $OUTPUT_DIR"
    err_out
fi
OUTPUT_FILE=sfav2_CONUS_${START_DATE_YYYYMMDDHH}_to_${DATE_YYYYMMDDHH}
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

    OUTPUT_SNFL_NC_PATH=${OUTPUT_DIR}/${OUTPUT_NC_FILE}

    DATE_YYYY=${DATE_YYYYMMDDHH:0:4}
    DATE_MM=${DATE_YYYYMMDDHH:4:2}
    DATE_DD=${DATE_YYYYMMDDHH:6:2}
    DATE_HH=${DATE_YYYYMMDDHH:8:2}

    OUTPUT_GISRS_LAYER="CONUS Seasonal Snowfall ${DATE_YYYY}-${DATE_MM}-${DATE_DD} ${DATE_HH} Snowf"

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
        err_msg "FATAL: Conversion of aggregate seasonal" \
            "analysis to GISRS layer failed for" \
            "${DATE_YYYYMMDDHH}."
        err_out
    fi
    export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
fi

if [ -n "$WEB_DIR" ] ; then


#   Copy output files to the web.

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

