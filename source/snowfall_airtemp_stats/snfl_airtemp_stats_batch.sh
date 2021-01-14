#!/bin/sh
export START_YEAR=2006
export FINISH_YEAR=2019

# 21 days centered on 10/01
export START_DATE_MMDD=0921
export FINISH_DATE_MMDD=1011
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 10/11
export START_DATE_MMDD=1001
export FINISH_DATE_MMDD=1021
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 10/21
export START_DATE_MMDD=1011
export FINISH_DATE_MMDD=1031
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 10/31
export START_DATE_MMDD=1021
export FINISH_DATE_MMDD=1110
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 11/10
export START_DATE_MMDD=1031
export FINISH_DATE_MMDD=1120
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 11/20
export START_DATE_MMDD=1110
export FINISH_DATE_MMDD=1130
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 11/30
export START_DATE_MMDD=1120
export FINISH_DATE_MMDD=1210
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 12/10
export START_DATE_MMDD=1130
export FINISH_DATE_MMDD=1220
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 12/20
export START_DATE_MMDD=1210
export FINISH_DATE_MMDD=1230
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 12/30
export START_DATE_MMDD=1220
export FINISH_DATE_MMDD=0109
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 01/09
export START_DATE_MMDD=1230
export FINISH_DATE_MMDD=0119
idl -quiet -rt=snfl_airtemp_stats_batch.sav

export FINISH_YEAR=2020

# 21 days centered on 01/19
export START_DATE_MMDD=0109
export FINISH_DATE_MMDD=0129
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 01/29
export START_DATE_MMDD=0119
export FINISH_DATE_MMDD=0208
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 02/08
export START_DATE_MMDD=0129
export FINISH_DATE_MMDD=0218
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 02/18
export START_DATE_MMDD=0208
export FINISH_DATE_MMDD=0228
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21(ish) days centered on 2/28
export START_DATE_MMDD=0218
export FINISH_DATE_MMDD=0310
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21(ish) days centered on 03/10
export START_DATE_MMDD=0228
export FINISH_DATE_MMDD=0320
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 03/20
export START_DATE_MMDD=0310
export FINISH_DATE_MMDD=0330
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 03/30
export START_DATE_MMDD=0320
export FINISH_DATE_MMDD=0409
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 04/09
export START_DATE_MMDD=0330
export FINISH_DATE_MMDD=0419
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 04/19
export START_DATE_MMDD=0409
export FINISH_DATE_MMDD=0429
idl -quiet -rt=snfl_airtemp_stats_batch.sav

# 21 days centered on 04/29
export START_DATE_MMDD=0419
export FINISH_DATE_MMDD=0509
idl -quiet -rt=snfl_airtemp_stats_batch.sav



