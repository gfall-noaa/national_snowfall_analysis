;****************************************************************************
;*
;* GETPID
;*
;* This idl function attempts to get the process id of the IDL instance
;* that calls it. It attempts to use the libc function "getpid".
;*
;* The program returns the pid if successful, -1, if not.
;*
;* Changes:
;* Anders Nilsson, VV,  12/11/02, Created
;* Greg Fall, UCAR/NOHRSC, 07 Jun 2006, Replaced FINDFILE with FILE_SEARCH.
;*
;****************************************************************************

FUNCTION GETPID

;   Find libc

    if (!Version.Memory_Bits eq 32) then $
      libc_dir = '/lib' $
    else $
      libc_dir = '/lib64'

    libc_file = FILE_SEARCH ( libc_dir + '/libc.so.*', COUNT=count)

    if ( count ne 0 ) then $
    begin

       if ( count gt 1 ) then print, 'More than 1 libc shared object found'
       result = LONG ( CALL_EXTERNAL ( libc_file[0], 'getpid' ) )
       libc_file = 0
       RETURN, result
       
    endif $
    else $
    begin

       print, libc_dir + '/libc shared object not found'
       RETURN, -1

    endelse

END

