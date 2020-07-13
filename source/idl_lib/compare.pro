;+
; NAME:
;       COMPARE
;
; PURPOSE:
;
;   Compare two scalar numbers or arrays.  This function is necessary
;   because round-off errors cause IDL to sometimes screw up when
;   (num1 eq num2) is used.
;
; CALLING SEQUENCE:
;
;   Result = COMPARE(num1, num2)
;
; INPUTS:
;
;   num1 = an array or scalar variable
;   num2 = an array or scalar variable of the same type as num1
;
; OUTPUTS:
;
;   If successful, this function returns the equivalent of
;   (num1 eq num2).  If not successful, it returns the scalar value
;   -1.
;
; KEYWORD PARAMETERS:
;
;   None
;
; SIDE EFFECTS:
;
;   This function is probably much slower than (num1 eq num2)
;
; RESTRICTIONS:
;
;   None
;
; EXAMPLE:
;
;   ...
;
; MODIFICATION HISTORY:
;
;   Written by: Greg Fall (UCAR/NOHRSC) and 
;               10 January 2000
;
;-

FUNCTION compare, num1, num2

size1 = SIZE(num1)
size2 = SIZE(num2)

if (size1[0] ne size2[0]) then begin
    MESSAGE, 'Arguments must have the same dimension', /CONTINUE
    RETURN, -1
endif

if (size1[0] ge 1) then begin

    for sizeInd = 1, size1[0] do begin

        if (size1[sizeInd] ne size2[sizeInd]) then begin

            MESSAGE, 'Argument dimensions must be the same size', /CONTINUE
            RETURN, -1

        endif

    endfor

endif

if (size1[size1[0] + 1] ne size2[size2[0] + 1]) then begin
print, size1
print, size2
print, size1[size1[0] + 1], size2[size2[0] + 1]
    MESSAGE, 'Arguments must be of the same type', /CONTINUE
    RETURN, -1

endif

case size1[size1[0] + 1] of

    1: threshold = 1
    2: threshold = 1
    3: threshold = 1
    4: threshold = 1.0e-8
    5: threshold = 1.0e-10
    else: begin
        print, size1[0], size1[size1[0]+1]
        MESSAGE, 'Argument type not supported', /CONTINUE
        RETURN, -1
    end

endcase

RETURN, ABS(num1 - num2) le threshold

end
