#!/bin/sh

# Script to perform National Snowfall Analysis, version 2, with 48- and
# 72-hour aggregations, projection, GRIB generation, and GISRS layer
# conversion included.

# This version is set up to run analysis for the current calendar day
# (see LAG_MINUTES and BASE_TIME)

# Greg Fall, OWP-Chanhassen
# Kent Sparrow, NWC, Tuscaloosa
# January 2017
#
# Changes:
#
#   2018-10-25, GF: Added a usage function and several command-line
#                   arguments and options.
#   2018-10-26, GF: Added some support for running on operations.
#                   Added -l, -g, and -u command-line options.
#                   Removed the condition that restricted the
#                   generation of GRIB files to "ds" machines.
#   2019-02-14, GF: Added the -e option to use the "extended"
#                   color ramp for snowfall amounts.
#   2019-06-28, GF: Moved sourcing of /etc/bashrc above function
#                   definitions to ensure pathmunge and ldpathmunge
#                   functions are not unset.
#   2019-09-18, GF: Improvements to accommodate operations, including
#                   replacement of aliases with variables and better
#                   management of the LD_LIBRARY_PATH.
#   2019-09-19, GF: Added a fancier usage message (including the
#                   "wrecho_hang" function).
#   2019-09-26, GF: Added the -m option for email recipient/s.

# Source the file used to set up login shells.

. /etc/bashrc

wrecho() {
    echo $* | fold -w $(tput cols) -s
}

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
    echo -n "Usage: do_sfav2_current_day.sh "
    HANG_COLS=31
    wrecho_hang "[-h,-?] [-q] [-f] [-l] [-g] [-u] [-e] [-m EMAIL]" \
                "[analysis YYYYMMDDHH]"
    echo ""
    echo -n "Options: -h  "
    HANG_COLS=13
    wrecho_hang "Show this message (-? has the same effect)."
    echo -n "         -q  "
    wrecho_hang "Do not produce verbose messages or generate plots" \
                "while executing the analysis, even if a TTY" \
                "(terminal) is defined."
    echo -n "             "
    wrecho_hang "Default: not set if TTY defined, set if no TTY."
    echo -n "         -f  "
    wrecho_hang "\"Force\" execution by increasing MAX_INSTANCES" \
                "by 1 for this run. This is no guarantee that the" \
                "program will run--if the number of instances" \
                "currently running already exceeds MAX_INSTANCES," \
                "for example, the script will still refuse to run" \
                "the analysis. See also -u."
    echo -n "         -l  "
    wrecho_hang "Disable generation of GISRS layers from analysis" \
                "results."
    echo -n "             "
    wrecho_hang "Default: make layers on machines named \"ds*\" or" \
                "\"os*\"."
    echo -n "         -g  "
    wrecho_hang "Disable 48-hour, 72-hour, and seasonal aggregation" \
                "of analysis results."
    echo -n "             "
    wrecho_hang "Default: generate aggregations for all 12Z analyses."
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
    HANG_COLS=9
    echo -n "         "
    wrecho_hang "Including the [analysis YYYYMMDDHH] argument" \
                "performs an analysis for the 24-hour period" \
                "ending at the indicated date/time, rather than" \
                "the default, which is to generate 24-hour" \
                "analyses for all reasonable 00Z and 12Z crossings" \
                "during (roughly) the last 24-hour period."
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

# Parse command line.

MAX_INSTANCES=1

while getopts "h?qflguem:" OPT; do
    case $OPT in
        h|\?)
            usage
            exit 
            ;;
        q)
            QUIET_RUN=1
            ;;
        f)
            MAX_INSTANCES=$((MAX_INSTANCES+1))
            ;;
        l)
            MAKE_LAYERS=0
            ;;
        g)
            DO_AGGREGATIONS=0
            ;;
        u)
            BUSY_FILE=SNOWFALL_V2_CURRENT_$(echo $USER | tr [a-z] [A-Z])_BUSY
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
    BUSY_FILE=SNOWFALL_V2_CURRENT_BUSY
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

SFAV2_SAV_PATH=${EXEC_DIR}/sfav2.sav
if [ ! -f "$SFAV2_SAV_PATH" ] ; then
    echo "Missing $SFAV2_SAV_PATH" 1>&2
    exit 1
fi

SFAV2_UTILS_DIR="${EXEC_DIR}/utils"
if [ ! -d "$SFAV2_UTILS_DIR" ] ; then
    echo "Bad configuration - missing ${SFAV2_UTILS_DIR}" \
         "directory" 1>&2
    exit 1
fi
export SFAV2_UTILS_DIR

# Add SFAV2_UTILS_DIR to the PATH so items in that directory
# (e.g. gdal_translate) are available.

pathmunge $SFAV2_UTILS_DIR

# Source common functions.

INCLUDE_PATH=${SFAV2_UTILS_DIR}/cron_script_functions.sh
if [ ! -f "$INCLUDE_PATH" ] ; then
    echo "Missing $INCLUDE_PATH" 1>&2
    exit 1
fi
. "$INCLUDE_PATH"

# Source key process/log management "start" code.

INCLUDE_PATH=${SFAV2_UTILS_DIR}/nwcdev_script_top.sh
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

# Deactivate BUSY_FILE stuff.
#rm -vf /operations/runlog/logs/${BUSY_FILE}


#####################
# MAIN SCRIPT BEGIN #
#####################


# Define required directories. The "SFAV2_" variables have defaults in
# sfav2.sav, but the ideal is to set them here.

SFAV2_RESOURCES_DIR="${EXEC_DIR}/resources"
if [ ! -d "$SFAV2_RESOURCES_DIR" ] ; then
    err_msg "Bad configuration - missing ${SFAV2_RESOURCES_DIR}" \
            "directory."
    err_out
fi
export SFAV2_RESOURCES_DIR

SFAV2_OUTPUT_DIR="${NSA_PREFIX}/misc/snowfall_v2"
if [ ! -d "$SFAV2_OUTPUT_DIR" ] ; then
    err_msg "Bad configuration - missing ${SFAV2_OUTPUT_DIR}" \
            "directory."
    err_out
fi
export SFAV2_OUTPUT_DIR

SFAV2_SCRATCH_DIR="${SFAV2_OUTPUT_DIR}/scratch"
if [ ! -d "$SFAV2_SCRATCH_DIR" ] ; then
    err_msg "Bad configuration - missing ${SFAV2_SCRATCH_DIR}" \
            "directory."
    err_out
fi
export SFAV2_SCRATCH_DIR

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

# Check for archived input data.

SFAV2_ARCHIVE_DIR="/nwcdev/archive"
if [ ! -d "$SFAV2_ARCHIVE_DIR" ] && [ "$ENVIRONMENT" = "development" ] ; then
    err_msg "Bad configuration - missing ${SFAV2_ARCHIVE_DIR}" \
            "directory."
    err_out
fi
export SFAV2_ARCHIVE_DIR

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

# Set snowfall analysis parameters.

TARGET_KRM_RING_WIDTH=4000
NUM_KRM_ANGLES=4
PARAMETER_TESTING=0
DRY_RUN=0
UPDATE_GIS_PROJECT=1
SKIP_QPE_ANALYSIS=0
DURATION_HOURS=24
WINDOW_HOURS_BACK=3
WINDOW_HOURS_FORWARD=3
if [ -z $QUIET_RUN ] ; then
    if [ $TTY == "TRUE" ] ; then
        QUIET_RUN=0
    else
        QUIET_RUN=1
    fi
fi
if [ -z $EXTENDED_COLORS ] ; then
   EXTENDED_COLORS=0
fi
if [ -z $DO_AGGREGATIONS ] ; then
  DO_AGGREGATIONS=1
fi

export TARGET_KRM_RING_WIDTH
export NUM_KRM_ANGLES
export PARAMETER_TESTING
export QUIET_RUN
export EXTENDED_COLORS
export DRY_RUN
export UPDATE_GIS_PROJECT
export SKIP_QPE_ANALYSIS
export DURATION_HOURS
export WINDOW_HOURS_BACK
export WINDOW_HOURS_FORWARD

# Check for a command line argument for DATE_YYYYMMDDHH.

if [ $# -eq 1 ] ; then
  DATE_YYYYMMDDHH=$1
  TEST=`echo "$DATE_YYYYMMDDHH" | sed 's/[0-9]//g'`
  if [ ${#DATE_YYYYMMDDHH} -ne 10 ] || [ ${#TEST} -ne 0 ] ; then
    err_msg "Invalid DATE_YYYYMMDDHH argument \"${DATE_YYYYMMDDHH}\"."
    usage
    err_out
  fi
fi

# Use the DATE string defined when INCLUDE_PATH was sourced to set BASE_TIME. 

LAG_MINUTES=20
BASE_TIME=`date -u --date="-${LAG_MINUTES} minutes ${DATE}" \
           "+%Y-%m-%d %H:%M:%S"`

MIN_HOURS_BACK=0
MAX_HOURS_BACK=24
if [ $# -eq 1 ] ; then
  MAX_HOURS_BACK=1
fi

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

HOURS_BACK=$MIN_HOURS_BACK

while [ $HOURS_BACK -lt $MAX_HOURS_BACK ] ; do

    if [ $# -ne 1 ] ; then
        DATE_YYYYMMDDHH=`date -u --date="-${HOURS_BACK} hours ${BASE_TIME}" \
                         +%Y%m%d%H`
    fi
    DATE_YYYY=${DATE_YYYYMMDDHH:0:4}
    DATE_YYYYMM=${DATE_YYYYMMDDHH:0:6}
    DATE_MM=${DATE_YYYYMMDDHH:4:2}
    DATE_DD=${DATE_YYYYMMDDHH:6:2}
    DATE_HH=${DATE_YYYYMMDDHH:8:2}

    if [ "$DATE_HH" = "00" ] || [ "$DATE_HH" = "12" ] ; then

        export DATE_YYYYMMDDHH

#       Only proceed if at least 3 IDL runtime licenses are available.

        if ! check_rt_idl ; then
            err_msg "Failed to check IDL runtime licenses."
            err_out
        fi
        TIME_START=`date -u +%s`
        TIME_FINISH=`date -u +%s`
        WAIT_TIME=$((TIME_FINISH-TIME_START))
        MAX_WAIT_TIME=1800
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

#       Run the analysis for DATE_YYYYMMDDHH

        usr_msg "Running sfav2.sav for $DATE_YYYYMMDDHH"
        $IDL -quiet -rt=${SFAV2_SAV_PATH}
#        /bin/true
        STATUS=$?
        if [ $STATUS -ne 0 ] ; then
            err_msg "FATAL: Snowfall analysis failed for ${DATE_YYYYMMDDHH}."
            err_out
        fi

        if [ $DRY_RUN -eq 1 ] ; then
            usr_out
        fi

#       Verify the output file.

        OUTPUT_SNFL_NC_PATH=${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_${DURATION_HOURS}h_${DATE_YYYYMMDDHH}.nc
        if [ ! -e $OUTPUT_SNFL_NC_PATH ] ; then
            err_msg "FATAL: Missing output ${OUTPUT_SNFL_NC_PATH} from" \
                    "${THIS_SCRIPT}"
            err_out
        fi

        if [ $MAKE_LAYERS -eq 1 ] ; then

#           Generate a GISRS layer.

            OUTPUT_GISRS_LAYER="CONUS Snowfall (${DURATION_HOURS} hours) ${DATE_YYYY}-${DATE_MM}-${DATE_DD} ${DATE_HH} Snowf"
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
                err_msg "FATAL: Conversion of ${DURATION_HOURS}-hour" \
                        "analysis to GISRS layer failed for" \
                        "${DATE_YYYYMMDDHH}."
                err_out
            fi
            export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
        fi

#       Project to NCEP grid 184 (double-resolution NDFD grid at ~2.54 km).

        PROJECT_184_SAV_PATH=${EXEC_DIR}/project_sfav2_to_grid184.sav
        if [ -f ${PROJECT_184_SAV_PATH} ] ; then
            export INPUT_NETCDF_PATH="$OUTPUT_SNFL_NC_PATH"
            export OUTPUT_NETCDF_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_${DURATION_HOURS}h_${DATE_YYYYMMDDHH}_grid184.nc"
            export CLOBBER_EXISTING_184=TRUE
            $IDL -quiet -rt=${PROJECT_184_SAV_PATH}
            STATUS=$?
            if [ $STATUS -ne 0 ] ; then
                err_msg "project_sfav2_to_grid184.sav failed for" \
                        "${INPUT_NETCDF_PATH}."
            else

                if [ -n "$WEB_DIR" ] ; then

#                   Copy projected file to the web.

                    WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
                    if [ ! -d $WEB_OUTPUT_DIR ] ; then
                        mkdir -m 2775 $WEB_OUTPUT_DIR
                    fi
                    if ( ! cp_gw ${OUTPUT_NETCDF_PATH} \
                         $WEB_OUTPUT_DIR/ ) ; then
                        err_msg "Failed to copy ${OUTPUT_NETCDF_PATH}" \
                                "to ${WEB_OUTPUT_DIR}."
                    fi

                fi

            fi
        else
            err_msg "${PROJECT_184_SAV_PATH} not found"
        fi

#       Convert projected data from NetCDF to GRIB2.

        GRIBIFY_PATH=${EXEC_DIR}/gribify_lcc_netcdf

        if [ -f ${GRIBIFY_PATH} ] ; then

            PROJ_NETCDF_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_${DURATION_HOURS}h_${DATE_YYYYMMDDHH}_grid184.nc"

            if [ ! -f ${PROJ_NETCDF_PATH} ] ; then
                warning_msg "${PROJ_NETCDF_PATH} missing;" \
                            "no conversion to GRIB2 possible."
            else

                PROJ_GRIB2_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_${DURATION_HOURS}h_${DATE_YYYYMMDDHH}_grid184.grb2"

                export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
                ldpathmunge ${NSA_PREFIX}/gisrs/bin/iwrss
                ${GRIBIFY_PATH} "${PROJ_NETCDF_PATH}" \
                                "${PROJ_GRIB2_PATH}"
                STATUS=$?
                export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
                if [ $STATUS -ne 0 ] || \
                    [ ! -f "${PROJ_GRIB2_PATH}" ] ; then
                    warning_msg "Failed to convert" \
                                "${PROJ_NETCDF_PATH} to GRIB2."
                else

                    if [ -n "$WEB_DIR" ] ; then

#                       Copy projected file to the web.

                        WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
                        if [ ! -d $WEB_OUTPUT_DIR ] ; then
                            mkdir -m 2775 $WEB_OUTPUT_DIR
                        fi
                        if ( ! cp_gw ${PROJ_GRIB2_PATH} \
                                     $WEB_OUTPUT_DIR/ ) ; then
                            err_msg "Failed to copy ${PROJ_GRIB2_PATH}" \
                                    "to ${WEB_OUTPUT_DIR}."
                        fi

                    fi

                fi

            fi

        else

            err_msg "${GRIBIFY_PATH} not found"

        fi

        if [ $DO_AGGREGATIONS -eq 1 ] ; then

#           Aggregate 2- and 3-day snowfall.

            AGG_SNOWFALL_2_AND_3_PATH=${EXEC_DIR}/agg_snowfall_2_and_3_day_v2.sh
            AGG_SNOWFALL_2_AND_3_CMD=${AGG_SNOWFALL_2_AND_3_PATH}
            if [ $MAKE_LAYERS -eq 0 ] ; then
                DUMMY="$AGG_SNOWFALL_2_AND_3_CMD -l"
                AGG_SNOWFALL_2_AND_3_CMD="$DUMMY"
            fi
            if [ $EXTENDED_COLORS -eq 1 ] ; then
                DUMMY="$AGG_SNOWFALL_2_AND_3_CMD -e"
                AGG_SNOWFALL_2_AND_3_CMD="$DUMMY"
            fi
            if [ ! -z "$EMAIL" ] ; then
                DUMMY="$AGG_SNOWFALL_2_AND_3_CMD -m $EMAIL"
            fi
            if [ -f ${AGG_SNOWFALL_2_AND_3_PATH} ] ; then
                export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
                ${AGG_SNOWFALL_2_AND_3_CMD} ${DATE_YYYYMMDDHH}
                STATUS=$?
                export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
                if [ $STATUS -ne 0 ] ; then
                    err_msg "\"${AGG_SNOWFALL_2_AND_3_CMD}\" failed for" \
                            "${DATE_YYYYMMDDHH}."
                fi
            else
                err_msg "${AGG_SNOWFALL_2_AND_3_PATH} not found"
            fi

#           Project 2- and 3-day aggregations to NCEP grid 184
#           (double-resolution NDFD grid at ~2.54 km).

            if [ -f ${PROJECT_184_SAV_PATH} ] ; then

                INPUT_NETCDF_PATH=${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_48h_${DATE_YYYYMMDDHH}.nc

                if [ -f "$INPUT_NETCDF_PATH" ] ; then
                    export INPUT_NETCDF_PATH
                    export OUTPUT_NETCDF_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_48h_${DATE_YYYYMMDDHH}_grid184.nc"
                    export CLOBBER_EXISTING_184=TRUE
                    $IDL -quiet -rt=${PROJECT_184_SAV_PATH}
                    STATUS=$?
                    if [ $STATUS -ne 0 ] ; then
                        err_msg "project_sfav2_to_grid184.sav failed for" \
                                "${INPUT_NETCDF_PATH}."
                    else
                        if [ -n "$WEB_DIR" ] ; then

#                           Copy projected file to the web.

                            WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
                            if [ ! -d $WEB_OUTPUT_DIR ] ; then
                                mkdir -m 2775 $WEB_OUTPUT_DIR
                            fi
                            if ( ! cp_gw ${OUTPUT_NETCDF_PATH} \
                                $WEB_OUTPUT_DIR/ ) ; then
                                err_msg "Failed to copy ${OUTPUT_NETCDF_PATH}" \
                                        "to ${WEB_OUTPUT_DIR}."
                            fi
                        fi
                    fi
                else
                    warning_msg "${INPUT_NETCDF_PATH} not found."
                fi

                INPUT_NETCDF_PATH=${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_72h_${DATE_YYYYMMDDHH}.nc

                if [ -f "$INPUT_NETCDF_PATH" ] ; then
                    export INPUT_NETCDF_PATH
                    export OUTPUT_NETCDF_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_72h_${DATE_YYYYMMDDHH}_grid184.nc"
                    export CLOBBER_EXISTING_184=TRUE
                    $IDL -quiet -rt=${PROJECT_184_SAV_PATH}
                    STATUS=$?
                    if [ $STATUS -ne 0 ] ; then
                        err_msg "project_sfav2_to_grid184.sav failed for" \
                                "${INPUT_NETCDF_PATH}."
                    else
                        if [ -n "$WEB_DIR" ] ; then

#                           Copy projected file to the web.

                            WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
                            if [ ! -d $WEB_OUTPUT_DIR ] ; then
                                mkdir -m 2775 $WEB_OUTPUT_DIR
                            fi
                            if ( ! cp_gw ${OUTPUT_NETCDF_PATH} \
                                $WEB_OUTPUT_DIR/ ) ; then
                                err_msg "Failed to copy ${OUTPUT_NETCDF_PATH}" \
                                        "to ${WEB_OUTPUT_DIR}."
                            fi
                        fi
                    fi
                else
                    warning_msg "${INPUT_NETCDF_PATH} not found."
                fi
            fi

#           Convert projected data from NetCDF to GRIB2.

            GRIBIFY_PATH=${EXEC_DIR}/gribify_lcc_netcdf

            if [ -f ${GRIBIFY_PATH} ] ; then

                PROJ_NETCDF_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_48h_${DATE_YYYYMMDDHH}_grid184.nc"

                if [ ! -f ${PROJ_NETCDF_PATH} ] ; then
                    warning_msg "${PROJ_NETCDF_PATH} missing;" \
                                "no conversion to GRIB2 possible."
                else

                    PROJ_GRIB2_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_48h_${DATE_YYYYMMDDHH}_grid184.grb2"

                    export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
                    ldpathmunge ${NSA_PREFIX}/gisrs/bin/iwrss
                    ${GRIBIFY_PATH} "${PROJ_NETCDF_PATH}" \
                                    "${PROJ_GRIB2_PATH}"
                    STATUS=$?
                    export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
                    if [ $STATUS -ne 0 ] || \
                        [ ! -f "${PROJ_GRIB2_PATH}" ] ; then
                        warning_msg "Failed to convert" \
                                    "${PROJ_NETCDF_PATH} to GRIB2."
                    else

                        if [ -n "$WEB_DIR" ] ; then

#                           Copy projected file to the web.

                            WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
                            if [ ! -d $WEB_OUTPUT_DIR ] ; then
                                mkdir -m 2775 $WEB_OUTPUT_DIR
                            fi
                            if ( ! cp_gw ${PROJ_GRIB2_PATH} \
                                $WEB_OUTPUT_DIR/ ) ; then
                                err_msg "Failed to copy ${PROJ_GRIB2_PATH}" \
                                        "to ${WEB_OUTPUT_DIR}."
                            fi

                        fi

                    fi

                fi

                PROJ_NETCDF_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_72h_${DATE_YYYYMMDDHH}_grid184.nc"

                if [ ! -f ${PROJ_NETCDF_PATH} ] ; then
                    warning_msg "${PROJ_NETCDF_PATH} missing;" \
                                "no conversion to GRIB2 possible."
                else

                    PROJ_GRIB2_PATH="${SFAV2_OUTPUT_DIR}/sfav2_${DATE_YYYYMMDDHH:0:8}/sfav2_CONUS_72h_${DATE_YYYYMMDDHH}_grid184.grb2"

                    export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
                    ldpathmunge ${NSA_PREFIX}/gisrs/bin/iwrss
                    ${GRIBIFY_PATH} "${PROJ_NETCDF_PATH}" \
                                    "${PROJ_GRIB2_PATH}"
                    STATUS=$?
                    export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
                    if [ $STATUS -ne 0 ] || \
                        [ ! -f "${PROJ_GRIB2_PATH}" ] ; then
                        warning_msg "Failed to convert" \
                            "${PROJ_NETCDF_PATH} to GRIB2."
                    else

                        if [ -n "$WEB_DIR" ] ; then

#                           Copy projected file to the web.

                            WEB_OUTPUT_DIR=${WEB_DIR}/${DATE_YYYYMMDDHH:0:6}
                            if [ ! -d $WEB_OUTPUT_DIR ] ; then
                                mkdir -m 2775 $WEB_OUTPUT_DIR
                            fi
                            if ( ! cp_gw ${PROJ_GRIB2_PATH} \
                                $WEB_OUTPUT_DIR/ ) ; then
                                err_msg "Failed to copy ${PROJ_GRIB2_PATH}" \
                                        "to ${WEB_OUTPUT_DIR}."
                            fi

                        fi

                    fi

                fi

            else

                err_msg "${GRIBIFY_PATH} not found"

            fi

            if [ "$DATE_HH" = "12" ] ; then

#               Aggregate seasonal snowfall.

                AGG_SNOWFALL_SEASON_PATH=${EXEC_DIR}/agg_snowfall_season_v2.sh
                AGG_SNOWFALL_SEASON_CMD=${AGG_SNOWFALL_SEASON_PATH}
                if [ $MAKE_LAYERS -eq 0 ] ; then
                    DUMMY="$AGG_SNOWFALL_SEASON_CMD -l"
                    AGG_SNOWFALL_SEASON_CMD="$DUMMY"
                fi
                if [ ! -z "$EMAIL" ] ; then
                    DUMMY="$AGG_SNOWFALL_SEASON_CMD -m $EMAIL"
                    AGG_SNOWFALL_SEASON_CMD="$DUMMY"
                fi

#               Supress seasonal snowfall on operations for dates
#               earlier than 2019-10-01.

                DATE_EPOCH=`date -u \
                  --date="${DATE_YYYY}-${DATE_MM}-${DATE_DD} ${DATE_HH}" \
                  +%s`
                OCT1_EPOCH=`date -u \
                  --date="2019-10-01 00" +%s`
                if [ "$ENVIRONMENT" == "operations" ] && \
                   [ $DATE_EPOCH -lt $OCT1_EPOCH ] ; then
                    AGG_SNOWFALL_SEASON_CMD="true"
                fi

                if [ -f ${AGG_SNOWFALL_SEASON_PATH} ] ; then
                    export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
                    export FORCE_SEASONAL=1
                    ${AGG_SNOWFALL_SEASON_CMD} ${DATE_YYYYMMDDHH}
                    STATUS=$?
                    export LD_LIBRARY_PATH=$MY_LD_LIBRARY_PATH
                    if [ $STATUS -ne 0 ] ; then
                        err_msg "\"${AGG_SNOWFALL_SEASON_CMD}\" failed for" \
                                "${DATE_YYYYMMDDHH}."
                    fi
                else
                    err_msg "${AGG_SNOWFALL_SEASON_PATH} not found"
                fi
            fi

        fi

    fi

    HOURS_BACK=$((HOURS_BACK+1))

done

# Clean scratch disk/s.
#
#   1. Temporary files used by GRIB decoders (keep for 5 days):
#      tmpGRIBRaster.[0-9]{14}.*
#      tmpGRIBHeader.[0-9]{14}.*
#   2. Daily min/max temperatures (keep for 30 days)
#      ".*HRRR_LC_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*HRRR_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RAP_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RAP_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RUC_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RUC_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#   3. HRRR, RAP, and RUC precipitation accumulations (keep for 30 days):
#      ".*HRRR_LC_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*HRRR_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RAP_130_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RAP_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RUC_130_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"
#      ".*RUC_LONLAT_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav"

if [ -d /net/scratch/${USER} ] ; then

    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +5 \
         -regextype posix-basic \
         -regex '.*tmpGRIBRaster\.[[:digit:]]\{14\}\.[[:digit:]]\+' \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +5 \
         -regextype posix-basic \
         -regex '.*tmpGRIBHeader\.[[:digit:]]\{14\}\.[[:digit:]]\+' \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LC_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LC_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_130_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_130_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find /net/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_LONLAT_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null

fi

if [ -d /disks/scratch/${USER} ] ; then

    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +5 \
         -regextype posix-basic \
         -regex '.*tmpGRIBRaster\.[[:digit:]]\{14\}\.[[:digit:]]\+' \
         -exec rm -f {} \;
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +5 \
         -regextype posix-basic \
         -regex '.*tmpGRIBHeader\.[[:digit:]]\{14\}\.[[:digit:]]\+' \
         -exec rm -f {} \;
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LC_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LC_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_130_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_130_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find /disks/scratch/${USER} \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_LONLAT_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null

fi

if [ -d $SFAV2_SCRATCH_DIR ] ; then

    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +5 \
         -regextype posix-basic \
         -regex '.*tmpGRIBRaster\.[[:digit:]]\{14\}\.[[:digit:]]\+' \
         -exec rm -f {} \;
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +5 \
         -regextype posix-basic \
         -regex '.*tmpGRIBHeader\.[[:digit:]]\{14\}\.[[:digit:]]\+' \
         -exec rm -f {} \;
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LC_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_130_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_LONLAT_MIN_MAX_AVE_TMP_f00_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LC_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*HRRR_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
         -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_130_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RAP_LONLAT_APCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_130_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null
    find $SFAV2_SCRATCH_DIR \
         -ignore_readdir_race \
         -maxdepth 1 \
         -type f \
         -mtime +30 \
         -regextype posix-basic \
         -regex \
           ".*RUC_LONLAT_ACPCP_NCPCP_WEASD_f[[:digit:]]\{2\}_${DURATION_HOURS}h_ending_[[:digit:]]\{10\}\.sav" \
        -exec rm -f {} \; 2>/dev/null

fi

###################
# MAIN SCRIPT END #
###################

# Source key process/log management "finish" code and exit.

INCLUDE_PATH=${SFAV2_UTILS_DIR}/nwcdev_script_bottom.sh
if [ ! -f "$INCLUDE_PATH" ] ; then
    err_msg "Missing $INCLUDE_PATH"
    err_out
fi
. $INCLUDE_PATH
