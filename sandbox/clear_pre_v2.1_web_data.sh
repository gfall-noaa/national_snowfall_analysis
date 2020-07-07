#!/bin/sh

# Clear out National Snowfall Analysis data from pre-2.1 versions of the
# program covering the October 2008 - September 2018 period.

SFAV2_DIR=/net/http0/htdocs/snowfall_v2/data
if [ ! -d $SFAV2_dir ] ; then
    echo "${SFAV2_DIR} not visible" 1>&2
    exit 1
fi
OUTPUT_DIR=${SFAV2_DIR}/old_versions
if [ ! -d $OUTPUT_DIR ] ; then
    mkdir $OUTPUT_DIR
fi
cd $SFAV2_DIR
for YYYYMM in [0-9][0-9][0-9][0-9][0-9][0-9] ; do
    if [ ! -d $YYYYMM ] ; then
        echo "${SFAV2_DIR}/${YYYYMM} is not a directory. Quitting." 1>&2
        exit 1
    fi
    if [ "$YYYYMM" == "201810" ] ; then
        break
    fi
    cd $YYYYMM
    if [ ! -d ${OUTPUT_DIR}/${YYYYMM} ] ; then
        mkdir ${OUTPUT_DIR}/${YYYYMM}
    fi
    NUM_FILES_MOVED=0
    # 48-hour aggregates on Grid 184 in GRIB2
    for OLD_FILE in `ls sfav2_CONUS_48h_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_grid184.grb2 2>/dev/null` ; do
        mv -v $OLD_FILE ${OUTPUT_DIR}/${YYYYMM}/
        NUM_FILES_MOVED=$((NUM_FILES_MOVED+1))
    done
    # 72-hour aggregates on Grid 184 in GRIB2
    for OLD_FILE in `ls sfav2_CONUS_72h_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_grid184.grb2 2>/dev/null` ; do
        mv -v $OLD_FILE ${OUTPUT_DIR}/${YYYYMM}/
        NUM_FILES_MOVED=$((NUM_FILES_MOVED+1))
    done
    # All aggregates on Grid 184 in NetCDF (appear to be none of these)
    for OLD_FILE in `ls sfav2_CONUS_[0-9][0-9]h_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_grid184.nc 2>/dev/null` ; do
        mv -v $OLD_FILE ${OUTPUT_DIR}/${YYYYMM}/
        NUM_FILES_MOVED=$((NUM_FILES_MOVED+1))
    done
    # All 00Z analysis (appear to be none of these)
    for OLD_FILE in `ls sfav2_CONUS_*h_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]00* 2>/dev/null` ; do
        mv -v $OLD_FILE ${OUTPUT_DIR}/${YYYYMM}/
        NUM_FILES_MOVED=$((NUM_FILES_MOVED+1))
    done
    # All 6-hour analysis (appear to be none of these)
    for OLD_FILE in `ls sfav2_CONUS_6h_* 2>/dev/null`; do
        mv -v $OLD_FILE ${OUTPUT_DIR}/${YYYYMM}/
        NUM_FILES_MOVED=$((NUM_FILES_MOVED+1))
    done
    if [ $NUM_FILES_MOVED -eq 0 ] ; then

        rmdir ${OUTPUT_DIR}/${YYYYMM}
    fi
    cd ..
done
