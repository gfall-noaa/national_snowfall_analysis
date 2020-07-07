# Source profile.

#. /etc/bashrc

# Other environment setup.

#alias cp='cp --preserve=mode,timestamps'
#alias mv='mv'

# System information, needed for logging/locking and for deciding whether
# to copy data to web server.

HOSTNAME=`hostname -s`

if [ -z "$USER" ] ; then
  USER=`whoami`
fi

# Get script/directory information. Every automated script running on a cron
# in an operational setting can use this.

PWD_SE=`echo $PWD | sed "s/\\//\\\\\\\\\//g"`
COMMAND=`echo $0 | sed "s/^\.\//$PWD_SE\//g"`
COMMAND_SE=`echo $COMMAND | sed "s/\\//\\\\\\\\\//g"`
THIS_SCRIPT=`echo $COMMAND_SE | sed 's/^.*\///g'`
THIS_DIR=`echo $COMMAND | sed "s/\/$THIS_SCRIPT$//g"`
MODULE=$THIS_SCRIPT

# Establish the start time for this script.

DATE=`date -u "+%Y-%m-%d %H:%M:%S"`
DATE_EPOCH=`date -u --date="${DATE}" +%s`
DATE_YYYYMMDDHHMMSS=`date -u --date="${DATE}" +%Y%m%d%H%M%S`
START_TIME=$DATE_EPOCH # For compatibility with update_run_time_data
CUTOFF_TIME=5184000    # For compatibility with update_run_time_data

# Confirm directory to store logs/locks.

LOG_DIR=/operations/runlog
if [ ! -d "$LOG_DIR" ] || [ ! -w "$LOG_DIR" ] ; then
  err_msg "Bad log directory \"$LOG_DIR\""
  err_out
fi
if [ ! -d "${LOG_DIR}/logs" ] || [ ! -w "${LOG_DIR}/logs" ] ; then
  err_msg "Bad log directory \"${LOG_DIR}/logs\""
  err_out
fi

# We decide how to do messages based on the exit status of "tty -s". If it
# comes back with error status we use a log file and send messages via email.
# Otherwise we use stdout and stderr, via the info_msg, notice_msg,
# warning_msg, and err_msg functions.

if tty -s ; then
  TTY=TRUE
else
  TTY=FALSE
  if [ -z "$EMAIL" ] ; then
    err_msg "No TTY present and no EMAIL variable set for ${THIS_SCRIPT}."
            "Messages are likely to be lost."
  fi
fi

# Make files created group writable.

umask 002

# Settings relevant to outputs and log files.

LOG_FILE=${THIS_SCRIPT}.${HOSTNAME}.${DATE_YYYYMMDDHHMMSS}
LOG_FILE_PATH=${LOG_DIR}/${LOG_FILE}
LOG_ENTRY=$LOG_FILE_PATH # For compatibility with update_run_time_data
touch $LOG_FILE_PATH
if [ -z "$BUSY_FILE" ] ; then
    err_msg "No BUSY_FILE variable (required for process management)" \
            "is set for ${THIS_SCRIPT}."
    err_out
fi
BUSY_FILE_PATH="${LOG_DIR}/logs/${BUSY_FILE}"

# Check the limit onthe number of scripts.

if [ -z "$MAX_INSTANCES" ] ; then
    info_msg "No MAX_INSTANCES variable set for ${THIS_SCRIPT}." \
             "Defaulting to 1."
    MAX_INSTANCES=1
fi
SCRATCH=`echo "$MAX_INSTANCES" | sed 's/[0-9]//g'`
if [ -n "$SCRATCH" ] ; then
    err_msg "Invalid MAX_INSTANCES variable \"${MAX_INSTANCES}\"." \
            "Defaulting to 1."
    MAX_INSTANCES=1
fi

#####################
# Check logs/locks. #
#####################

THIS_BUSY_FILE_ENTRY="$HOSTNAME $USER $$ $DATE_EPOCH"
if [ -f "${BUSY_FILE_PATH}" ] ; then

# An existing BUSY_FILE means that another process may be in the way. Create
# a sure-to-be-unique (but not guaranteed unique) temporary file to edit the
# contents of the BUSY_FILE.

  UNIQUE_STR=${HOSTNAME}.$$.`date -u +%s`
  BUSY_FILE_EDIT_PATH="${LOG_DIR}/logs/${UNIQUE_STR}"
  if [ -f "${BUSY_FILE_EDIT_PATH}" ] ; then
    err_msg "FATAL: supposedly unique file ${BUSY_FILE_EDIT_PATH} exists."
    err_out
  fi
  touch "${BUSY_FILE_EDIT_PATH}"

# Clean the BUSY_FILE. Lock it, then confirm that each process ID listed
# within actually exists.

  if create_lock "${BUSY_FILE_PATH}" ; then
    while read BUSY_FILE_ENTRY ; do
      if [ `echo "${BUSY_FILE_ENTRY}" | wc -w` -ne 4 ] ; then
        warning_msg "Ignoring incorrectly-formatted entry" \
                    "${BUSY_FILE_ENTRY} from busy file ${BUSY_FILE_PATH}."
        continue
      fi
      BUSY_ENTRY_HOST=`echo "${BUSY_FILE_ENTRY}" | awk '{print $1}'`
      BUSY_ENTRY_USER=`echo "${BUSY_FILE_ENTRY}" | awk '{print $2}'`
      BUSY_ENTRY_PID=`echo "${BUSY_FILE_ENTRY}" | awk '{print $3}'`
      BUSY_ENTRY_START_EPOCH=`echo "${BUSY_FILE_ENTRY}" | awk '{print $4}'`
      if [ "$BUSY_ENTRY_USER" != "$USER" ] ; then
        # Process is owned by another user. This user cannot touch it.
        warning_msg "Busy file ${BUSY_FILE_PATH} has an entry owned by" \
                    "user ${BUSY_ENTRY_USER} on host ${BUSY_ENTRY_HOST}," \
                    "which user ${USER} cannot touch."
        echo "${BUSY_FILE_ENTRY}" >> "${BUSY_FILE_EDIT_PATH}"
        continue
      fi
      if [ "$BUSY_ENTRY_HOST" = "$HOSTNAME" ] ; then
        CMD="kill -0 $BUSY_ENTRY_PID"
      else
        # Verify that we can ssh to the BUSY_ENTRY_HOST
        if ( ! ssh -q ${USER}@${BUSY_ENTRY_HOST} /bin/true ) 2>/dev/null ; then
          warning_msg "Cannot connect to $BUSY_ENTRY_HOST to check busy" \
                      "file process ${BUSY_ENTRY_PID}."
          echo "${BUSY_FILE_ENTRY}" >> "${BUSY_FILE_EDIT_PATH}"
          continue
        fi
        CMD="ssh -q ${USER}@${BUSY_ENTRY_HOST} kill -0 $BUSY_ENTRY_PID"
      fi

      if ( $CMD ) 2>/dev/null ; then
        # Process is still going. If it has been running too long, kill it.
        PROCESS_WALL_TIME=$((DATE_EPOCH-BUSY_ENTRY_START_EPOCH))
        if [ $PROCESS_WALL_TIME -gt 3600 ] ; then
          notice_msg "Process id $BUSY_ENTRY_PID on host ${BUSY_ENTRY_HOST}" \
                     "has been running for $PROCESS_WALL_TIME seconds."
        fi
        echo "${BUSY_FILE_ENTRY}" >> "${BUSY_FILE_EDIT_PATH}"
      else
        notice_msg "Removing non-existent process id $BUSY_ENTRY_PID" \
                   "(which ran on host ${BUSY_ENTRY_HOST}) from busy file."
      fi
    done < "${BUSY_FILE_PATH}"
    mv -f "${BUSY_FILE_EDIT_PATH}" "${BUSY_FILE_PATH}"
  else
    notice_msg "Failed to get a lock on ${BUSY_FILE_PATH}."
    rm -f "${BUSY_FILE_EDIT_PATH}"
    #GF 2018-08-21 Changed usr_out to err_out so exit status can be checked.
    #usr_out
    err_out
  fi

  # Count the number of existing instances of this script.

  BUSY_FILE_STR=`cat ${BUSY_FILE_PATH} | tr '\n' ' ' \
    | sed s/^\ *//g | sed s/\ *$//g`
  NUM_GOING=`cat ${BUSY_FILE_PATH} \
             | wc -l | sed s/^\ *//g | sed s/\ *$//g`
  if [ $NUM_GOING -ge $MAX_INSTANCES ] ; then
      if [ $NUM_GOING -eq 1 ] ; then
          ISARE=is
          SCRIPTSCRIPTS=script
      else
          ISARE=are
          SCRIPTSCRIPTS=scripts
      fi
      notice_msg "There $ISARE $NUM_GOING $THIS_SCRIPT $SCRIPTSCRIPTS going" \
                 "already (${BUSY_FILE_STR})."
      rm -f "${BUSY_FILE_PATH}.lock"
      rm -f "$LOG_FILE_PATH" # no spamming 2017-03-13
      #GF 2018-08-21 Changed usr_out to err_out so exit status can be checked.
      #usr_out
      err_out
  else
      if [ $NUM_GOING -eq 1 ] ; then
          INSTANCEINSTANCES=instance
      else
          INSTANCEINSTANCES=instances
      fi
      if [ $NUM_GOING -gt 0 ] ; then
          info_msg "Okay to proceed; only $NUM_GOING $INSTANCEINSTANCES" \
                   "going (${BUSY_FILE_STR})."
    else
      info_msg "Okay to proceed. No existing instances."
    fi
  fi

fi

# This script has permission to run and owns the lock on the BUSY_FILE. Add
# the current process ID and unlock it.

echo "$THIS_BUSY_FILE_ENTRY" >> "${BUSY_FILE_PATH}"
#usr_msg "This is process ID $$"
rm -f "${BUSY_FILE_PATH}.lock"
