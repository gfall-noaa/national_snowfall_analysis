#####################
# Clean logs/locks. #
#####################

# Clean/remove busy file. First make sure it is still there.

if [ ! -f "${BUSY_FILE_PATH}" ] ; then
  err_msg "Busy file ${BUSY_FILE_PATH} disappeared unexpectedly."
  err_out
fi

# Lock the BUSY_FILE.

UNIQUE_STR=${HOSTNAME}.$$.`date -u +%s`
BUSY_FILE_EDIT_PATH="${LOG_DIR}/logs/${UNIQUE_STR}"
if [ -f "${BUSY_FILE_EDIT_PATH}" ] ; then
  err_msg "Supposedly unique file ${BUSY_FILE_EDIT_PATH} exists."
  err_out
fi
touch "${BUSY_FILE_EDIT_PATH}"

if ! create_lock "${BUSY_FILE_PATH}" ; then
  notice_msg "Failed to get a lock on ${BUSY_FILE_PATH}."
  rm -f "${BUSY_FILE_EDIT_PATH}"
  usr_out
fi

# Remove this process from BUSY_FILE. and store the remainder in
# BUSY_FILE_EDIT_PATH.

grep -v "${THIS_BUSY_FILE_ENTRY}" "${BUSY_FILE_PATH}" > "${BUSY_FILE_EDIT_PATH}"
mv -f "${BUSY_FILE_EDIT_PATH}" "${BUSY_FILE_PATH}"

# An empty busy file means that this process was the only instance running.

if [ ! -s "${BUSY_FILE_PATH}" ] ; then
  rm -f "${BUSY_FILE_PATH}"
fi

rm -f "${BUSY_FILE_PATH}.lock"

# Make a copy of the LOG_FILE_PATH so we can call update_run_time_data (which
# deletes it) and still exit with usr_out.

#LOG_FILE_PATH_COPY=${LOG_FILE_PATH}.1
#if ( ! cp $LOG_FILE_PATH $LOG_FILE_PATH_COPY ) ; then
#  err_msg "Failed to create copy of ${LOG_FILE_PATH} for clean exit"
#  usr_out
#fi

#LOG_ENTRY=${LOG_FILE_PATH_COPY}

#. /home/opps_adm/crontab/update_run_time_data

rm -f "$LOG_FILE_PATH" # no emailing messages since we are quitting happily
usr_out
