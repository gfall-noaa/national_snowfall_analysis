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

info_msg() {

  if [ -n "$TTY" ] &&         # TTY string defined
     [ "$TTY" = "FALSE" ] &&  # No terminal
     [ -n "$LOG_DIR" ] &&     # LOG_DIR string defined
     [ -d "$LOG_DIR" ] &&     # LOG_DIR is a directory
     [ -w "$LOG_DIR" ] &&     # LOG_DIR is writeable
     [ -n "$LOG_FILE_PATH" ]  # LOG_FILE_PATH string defined
    then
    echo "INFO $@" >> "$LOG_FILE_PATH"
  else
    COLUMNS=`tput cols`
    if [ $? -ne 0 ] ; then
      COLUMNS=80
    fi
    echo "INFO $@" | fold -w $COLUMNS -s
  fi

  return 0

}

notice_msg() {

  if [ -n "$TTY" ] &&          # TTY string defined
     [ "$TTY" = "FALSE" ] &&   # No terminal
     [ -n "$LOG_DIR" ] &&      # LOG_DIR string defined
     [ -d "$LOG_DIR" ] &&      # LOG_DIR is a directory
     [ -w "$LOG_DIR" ] &&      # LOG_DIR is writeable
     [ -n "$LOG_FILE_PATH" ]   # LOG_FILE_PATH string defined
    then
    echo "NOTICE $@" >> "$LOG_FILE_PATH"
  else
    COLUMNS=`tput cols`
    if [ $? -ne 0 ] ; then
      COLUMNS=80
    fi
    echo "NOTICE $@" | fold -w $COLUMNS -s
  fi

  return 0

}

warning_msg() {

  if [ -n "$TTY" ] &&          # TTY string defined
     [ "$TTY" = "FALSE" ] &&   # No terminal
     [ -n "$LOG_DIR" ] &&      # LOG_DIR string defined
     [ -d "$LOG_DIR" ] &&      # LOG_DIR is a directory
     [ -w "$LOG_DIR" ] &&      # LOG_DIR is writeable
     [ -n "$LOG_FILE_PATH" ]   # LOG_FILE_PATH string defined
    then
    echo "WARNING $@" >> "$LOG_FILE_PATH"
  else
    COLUMNS=`tput cols`
    if [ $? -ne 0 ] ; then
      COLUMNS=80
    fi
    echo "WARNING $@" | fold -w $COLUMNS -s 1>&2
  fi

  return 0

}

err_msg() {

  if [ -n "$TTY" ] &&          # TTY string defined
     [ "$TTY" = "FALSE" ] &&   # No terminal
     [ -n "$LOG_DIR" ] &&      # LOG_DIR string defined
     [ -d "$LOG_DIR" ] &&      # LOG_DIR is a directory
     [ -w "$LOG_DIR" ] &&      # LOG_DIR is writeable
     [ -n "$LOG_FILE_PATH" ]   # LOG_FILE_PATH string defined
    then
    echo "ERR $@" >> "$LOG_FILE_PATH"
  else
    COLUMNS=`tput cols`
    if [ $? -ne 0 ] ; then
      COLUMNS=80
    fi
    echo "ERR $@" | fold -w $COLUMNS -s 1>&2
  fi

  return 0

}

usr_msg() {

  if [ -n "$TTY" ] &&         # TTY string defined
     [ "$TTY" = "FALSE" ] &&  # No terminal
     [ -n "$LOG_DIR" ] &&     # LOG_DIR string defined
     [ -d "$LOG_DIR" ] &&     # LOG_DIR is a directory
     [ -w "$LOG_DIR" ] &&     # LOG_DIR is writeable
     [ -n "$LOG_FILE_PATH" ]  # LOG_FILE_PATH string defined
    then
    echo "INFO $@" >> "$LOG_FILE_PATH"
  else
    COLUMNS=`tput cols`
    if [ $? -ne 0 ] ; then
      COLUMNS=80
    fi
    echo "INFO $@" | fold -w $COLUMNS -s
  fi

  return 0

}

err_out() {

  # Can be given argument of exit status to deliver.

  if [ -n "$TTY" ] &&            # TTY string defined
     [ "$TTY" = "FALSE" ] &&     # No terminal
     [ -n "$LOG_FILE_PATH" ] &&  # LOG_FILE_PATH string defined
     [ -s "$LOG_FILE_PATH" ] &&  # LOG_FILE_PATH exists and is not empty
     [ -n "$EMAIL" ] &&          # EMAIL string defined
     [ -n "$MODULE" ]            # MODULE string defined
    then

    # Send error messages via email.

    if cat "$LOG_FILE_PATH" | \
      /bin/mail -s "Errors in $MODULE on $HOSTNAME" "$EMAIL" ;
      then
      rm -f "$LOG_FILE_PATH"
    fi

  fi

  if [ -n "$LOG_FILE_PATH" ] &&  # LOG_FILE_PATH string defined
     [ -e "$LOG_FILE_PATH" ]     # LOG_FILE_PATH exists
    then

    # Send error messages via stderr.

    if [ -s "$LOG_FILE_PATH" ] # LOG_FILE_PATH is not empty
      then
      cat "$LOG_FILE_PATH" 1>&2
    fi
    rm -f "$LOG_FILE_PATH"

  fi

  if [ -n "$I_OWN_LOCK_FILE" ] &&         # I_OWN_LOCK_FILE string defined
     [ "$I_OWN_LOCK_FILE" == "TRUE" ] &&  # I own LOCK_FILE_PATH
     [ -n "$LOCK_FILE_PATH" ] &&          # LOCK_FILE_PATH string defined
     [ -e "$LOCK_FILE_PATH" ]             # LOCK_FILE_PATH exists
    then

    # Delete the LOCK_FILE_PATH.

    rm -f "$LOCK_FILE_PATH"

  fi

  if [ $# -eq 1 ] ; then
    exit $1
  else
    exit 1
  fi

}

usr_out() {

  if [ -n "$TTY" ] &&            # TTY string defined
     [ "$TTY" = "FALSE" ] &&     # No terminal
     [ -n "$LOG_FILE_PATH" ] &&  # LOG_FILE_PATH string defined
     [ -s "$LOG_FILE_PATH" ] &&  # LOG_FILE_PATH exists and is not empty
     [ -n "$EMAIL" ] &&          # EMAIL string defined
     [ -n "$MODULE" ]            # MODULE string defined
    then

    # Send messages via email.

    if cat "$LOG_FILE_PATH" | \
      /bin/mail -s "$MODULE on $HOSTNAME completed" "$EMAIL" ;
      then
      rm -f "$LOG_FILE_PATH"
    fi

  fi

  if [ -n "$LOG_FILE_PATH" ] &&  # LOG_FILE_PATH string defined
     [ -e "$LOG_FILE_PATH" ]     # LOG_FILE_PATH exists
    then

    # Send messages via stdout.

    if [ -s "$LOG_FILE_PATH" ] # LOG_FILE_PATH is not empty
      then
      cat "$LOG_FILE_PATH"
    fi
    rm -f "$LOG_FILE_PATH"

  fi

  if [ -n "$I_OWN_LOCK_FILE" ] &&         # I_OWN_LOCK_FILE string defined
     [ "$I_OWN_LOCK_FILE" == "TRUE" ] &&  # I own LOCK_FILE_PATH
     [ -n "$LOCK_FILE_PATH" ] &&          # LOCK_FILE_PATH string defined
     [ -e "$LOCK_FILE_PATH" ]             # LOCK_FILE_PATH exists
    then

    # Delete the LOCK_FILE_PATH.

    rm -f "$LOCK_FILE_PATH"

  fi

  exit 0

}

create_lock() {

  HOSTNAME=`hostname -s`
  if [ -z "$USER" ] ; then
    USER=`whoami`
  fi
  LOCK_FILE_PATH="$1.lock"
  I_OWN_LOCK_FILE=FALSE

  DATE_EPOCH=`date -u +%s`

  LOCK_FILE_ENTRY="$HOSTNAME $USER $$ $DATE_EPOCH"

  if ( set -o noclobber ; echo "$LOCK_FILE_ENTRY" > ${LOCK_FILE_PATH} ) 2>/dev/null ; then
    # Success!
    I_OWN_LOCK_FILE=TRUE
    return 0
  fi

  # Lock creation failed.  Confirm that existing lock file has HOSTNAME
  # and PID columns
  if [ `cat "$LOCK_FILE_PATH" | wc -w` -ne 4 ] ; then
    # Lock file does not have three columns. Take it over.
    warning_msg "Removing incorrectly-formatted lock file" \
            "${LOCK_FILE_PATH}."
    echo "$LOCK_FILE_ENTRY" > $LOCK_FILE_PATH
    I_OWN_LOCK_FILE=TRUE
    return 0
  fi

  # Confirm that process owning lock file exists.
  LOCK_OWNER_HOST=`cat $LOCK_FILE_PATH | awk '{print $1}'`
  LOCK_OWNER_USER=`cat $LOCK_FILE_PATH | awk '{print $2}'`
  LOCK_OWNER_PID=`cat $LOCK_FILE_PATH | awk '{print $3}'`
  LOCK_OWNER_START_EPOCH=`cat $LOCK_FILE_PATH | awk '{print $4}'`
  if [ "$LOCK_OWNER_USER" != "$USER" ] ; then
    usr_msg "Lock file ${LOCK_FILE_PATH} is owned by user ${LOCK_OWNER_USER}."
    return 1
  fi
  if [ "$LOCK_OWNER_HOST" = "$HOSTNAME" ] ; then
    CMD="kill -0 $LOCK_OWNER_PID"
  else
    # Verify that we can ssh to the LOCK_OWNER_HOST
    if ( ! ssh -q ${USER}@${LOCK_OWNER_HOST} /bin/true ) 2>/dev/null ; then
      usr_msg "Cannot connect to $LOCK_OWNER_HOST to check lock file process" \
              "${LOCK_OWNER_PID}."
      return 1
    fi
    CMD="ssh -q ${USER}@${LOCK_OWNER_HOST} kill -0 $LOCK_OWNER_PID"
  fi
  if ( $CMD ) 2>/dev/null ; then
    # Process still going.  Return failure status.
    return 1
  fi

  # Process that owns lock file is dead.  Take it over.
  usr_msg "Warning: removing stale lock file ${LOCK_FILE_PATH} owned by" \
          "missing process ${LOCK_OWNER_PID} on host ${LOCK_OWNER_HOST}."
  echo "$LOCK_FILE_ENTRY" > $LOCK_FILE_PATH
  I_OWN_LOCK_FILE=TRUE
  return 0

}

cp_gw() {

# Copy files in such a way that existing files are first deleted. Needed for
# overwriting files with different owners but group write privileges (and
# where the two users are in the same group that the file belongs to).

  SRC=$1
  DEST=$2

  SRC_DIR=`echo "$SRC" | sed 's/\(.*\)\/.*/\1/g'`
  SRC_FILE=`echo "$SRC" | sed 's/.*\///g'`

  # If SRC is a file with no preceding directory, SRC_DIR and SRC_FILE
  # are the same.

  if [ -d $DEST ] ; then
    if [ -f ${DEST}/${SRC_FILE} ] ; then
      if ( ! rm -f ${DEST}/${SRC_FILE} ) ; then
        return $?
      fi
    fi
  else
    if [ -e $DEST ] ; then
      if ( ! rm -f ${DEST} ) ; then
        return $?
      fi
    fi
  fi
  cp $SRC $DEST
  return $?

}
