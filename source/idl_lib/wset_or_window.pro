PRO WSET_OR_WINDOW, windowIndex, $
                    XSIZE = xSize, $
                    YSIZE = ySize, $
                    ERASE = erase

; Determine if a window exists. If it does not, create it. If it does, use
; WSET to switch to it and only recreate it if its size does not match the
; XSIZE and YSIZE keywords (if present).

  CATCH, errorStatus

  if ( errorStatus ne 0 ) then begin

      
      if (!Error_State.msg ne $
          'WSET: Window is closed and unavailable.') then begin ; maybe check
                                                                ; code instead
          CATCH, /CANCEL
          MESSAGE, !Error_State.msg
      endif 

      case 1 of

          KEYWORD_SET ( xSize ) and KEYWORD_SET ( ySize ) : $
            WINDOW, windowIndex, XSIZE = xSize, YSIZE = ySize

          KEYWORD_SET ( xSize ) and NOT ( KEYWORD_SET ( ySize ) ) : $
            WINDOW, windowIndex, XSIZE = xSize

          NOT ( KEYWORD_SET ( xSize ) ) and KEYWORD_SET ( ySize ) : $
            WINDOW, windowIndex, YSIZE = ySize

          else : WINDOW, windowIndex

      endcase

      CATCH, /CANCEL

  endif

;  PRINT, 'WSET, ', windowIndex
  WSET, windowIndex ; If this fails, we try to create a window using WINDOW.
;  print, 'WINDOW SIZE: ', !d.x_size, ' x ', !d.y_size

  CATCH, /CANCEL

  if NOT ( KEYWORD_SET ( xSize ) ) then xSize = !d.x_size
  if NOT ( KEYWORD_SET ( ySize ) ) then ySize = !d.y_size

  if ( !d.x_size ne xSize ) or ( !d.y_size ne ySize ) then begin
;      PRINT, 'NEW WINDOW', WINDOWiNDEX
;      print, xSize, !d.x_size
;      print, ySize, !d.y_size
;      wait, 5
;      PRINT, 'WINDOW, ', windowIndex, ', XSIZE = ', xSize, ', YSIZE = ', ySize
      WINDOW, windowIndex, XSIZE = xSize, YSIZE = ySize
;      PRINT, 'RESULT: ', !d.x_size, !d.y_size
  endif

  if KEYWORD_SET ( erase ) then TV, BYTARR ( xSize, ySize )


;   WINDOW, windowIndex, $

  RETURN

end
