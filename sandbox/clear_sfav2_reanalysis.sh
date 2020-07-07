#!/bin/sh

# For October 2008 - September 2018, go through images in
# /operations/misc/snowfall_v2
# and delete extra PNG files from early attempts at reanalysis.
# Assume we are running this during the 2019 calendar year.
# *** NOTE: NOT USING THIS SCRIPT RIGHT NOW. THERE IS TOO MUCH ***
# *** OVERLAP BETWEEN EARLY REANALYSIS WE WANT TO THROW OUT    ***
# *** AND "OPERATIONAL" VERSION 2 REANALYSIS FROM 2017-18,     ***
# *** NOT TO MENTION 6-HOUR ANALYSES, 00Z ANALYSES, ETC. FOR   ***
# *** THE NEXT SFAV2 VERSION, WHEN WE DO THE REANALYSIS AGAIN, ***
# *** WE SHOULD EMPTY OUT EVERYTHING FIRST.                    ***

SFAV2_DIR=/operations/misc/snowfall_v2
CLEAROUT_DIR=/net/tmp/misc_snowfall_v2_outgoing
if [ ! -d $CLEAROUT_DIR ] ; then
    mkdir $CLEAROUT_DIR
fi
# Leave September 29 and 30 aalone, because the full reanalysis did not
# include those days.
# The reanalysis for October-December 2008 ran from Feb 21 - 22, 2019
d1=`date --date="2019-02-21 14:58:00" +%j`
d2=`date +%j`
MTIME_PLUS=$((d2-d1+10))
# Presumably anything in ${SFAV2_DIR}/sfav2_20081[012]?? older than
# MTIME_PLUS is from early reanalysis efforts and we can discard it.
cd $SFAV2_DIR
for SUBDIR in sfav2_20081??? ; do
  mkdir -p ${CLEAROUT_DIR}/${SUBDIR}
  cd $SUBDIR
  find . -maxdepth 1 -type f -mtime +${MTIME_PLUS} -exec echo "mv {} ${CLEAROUT_DIR}/${SUBDIR}" \;
  cd ..
done
# The reanalysis for 2009 ran during Feb 22 - Mar 01, 2019
d1=`date --date="2019-02-22 18:58:00" +%j`
d2=`date +%j`
MTIME_PLUS=$((d2-d1+10))
# Presumably anything in ${SFAV2_DIR}/sfav2_20081[012]?? older than
# MTIME_PLUS is from early reanalysis efforts and we can discard it.
cd $SFAV2_DIR
for SUBDIR in sfav2_2009???? ; do
  mkdir -p ${CLEAROUT_DIR}/${SUBDIR}
  cd $SUBDIR
  find . -maxdepth 1 -type f -mtime +${MTIME_PLUS} -exec echo "mv {} ${CLEAROUT_DIR}/${SUBDIR}" \;
  cd ..
done
# The reanalysis for 2010 ran between Mar 01 - Mar 05
d1=`date --date="2019-03-01 00:32:00" +%j`
d2=`date +%j`
MTIME_PLUS=$((d2-d1+10))
