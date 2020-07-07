PRO MKDIR_2775, directory

  COMMON info, message
  cmd = 'mkdir -m 2775 ' + directory
  SPAWN, cmd, EXIT_STATUS = status
  if (status ne 0) then $
    ERR_MSG, 'Command "mkdir -m 2775 ' + directory + $
             '" failed with status ' + $
    STRCRA(status)
  RETURN

end
