FUNCTION SFAV2_KRIGING_OUTPUT_BIAS, $
    r2_, $ ; kriging input (points)
    r2_hat_, $ ; sampled kriging output at r2 points
    r2_hat_var_, $ ; sampled kriging output error variance at r2 points
    ndv, $ ; no data value for all of the above (and for use within function)
    OUTPUT_PNG_PATH = PNGImage, $ ; path for a file to plot results if desired
    TITLE = TITLE, $ ; text for plot title
    UTILITIES_DIR = utilsDir, $ ; recommended if OUTPUT_PNG_PATH is used
    VERBOSE = verbose

;+
; Estimate bias in kriging output for the National Snowfall Analysis.
; A linear fit is made between the sampled kriging output (r2_hat)
; vs. the input (r2), and the slope of this line is an estimate of the
; kriging bias.
;-

;+
; Confirm the arrays are matched.
;-
  numPoints = N_ELEMENTS(r2_)
  if ((N_ELEMENTS(r2_hat_) ne numPoints) or $
      (N_ELEMENTS(r2_hat_var_) ne numPoints)) then begin
      ERR_MSG, 'ERROR: size mismatch among input arrays.'
      
      RETURN, !NULL
  endif

;+
; Eliminate no-data values and values with zero variance, which can
; cause the fit to fail to converge (and are not believable).
;-
  ind = WHERE((r2_ ne ndv) and $
              (r2_hat_ ne ndv) and $
              (r2_hat_var_ ne ndv) and $
              (r2_hat_var_ gt 1.0e-6), count)
  if (count lt 100) then begin
      ERR_MSG, 'WARNING: too few points for a kriging bias estimate.'
      RETURN, !NULL
  endif

;+
; Eliminate no-data values.
;-
  r2 = r2_[ind]
  r2_hat = r2_hat_[ind]
  r2_hat_var = r2_hat_var_[ind]

;+
; Verify that none of the variances are negative.
;-
  if (MIN(r2_hat_var) lt 0.0) then begin
      ERR_MSG, 'ERROR: some input variances have negative values.'
      RETURN, !NULL
  endif

;+
; Eliminate correct negatives.
;-
  ind = WHERE((ABS(r2) gt 1.0e-3) or $
              (ABS(r2_hat gt 1.0e-3)), count)
  if (count lt 100) then begin
      ERR_MSG, 'WARNING: too few nonzero pairs for a kriging bias estimate.'
      RETURN, !NULL
  endif
  r2 = r2[ind]
  r2_hat = r2_hat[ind]
  r2_hat_var = r2_hat_var[ind]

;+
; The outsize influence of r2 values close to zero, which always
; dominate the inputs to this function, disrupts our ability to
; perform a linear fit of r2_hat vs. r2. To combat the problem we
; break the x axis into "zones" and work with averages in those
; zones. This levels the playing field and gives a much better fit.
;-

;+
; Divide the range of r2 into "zones" of uniform width. Try for 500
; zones, and reduce that number gradually until at least half the
; zones have something in them, but refuse to work with fewer than 100
; zones.
;-
  numZones = 550                ; starting number of zones + 50
  minNumZones = 100
  hMedian = 0.0
  while (hMedian lt 1.0) do begin
      numZones = numZones - 50
      zoneWidth = (MAX(r2) - MIN(r2)) / (numZones - 1)
      if (numZones eq minNumZones) then break
      h = HISTOGRAM(r2, $
                    MIN = MIN(r2) - 0.5 * zoneWidth, $
                    MAX = MAX(r2) + 0.5 * zoneWidth, $
                    BINSIZE = zoneWidth)
      h = h[0:numZones - 1]
      hMedian = MEDIAN(h, /EVEN)
  endwhile

;+
; Calculate the mean and variance of the r2_hat for each zone, as well
; as the mean of r2.
;-
  r2_hat_mean = MAKE_ARRAY(numZones, VALUE = ndv)
  r2_hat_mean_var = r2_hat_mean
  r2_mean = r2_hat_mean

  r2_low = MIN(r2) - 0.5 * zoneWidth
  r2_high = r2_low + zoneWidth
  for zc = 0, numZones - 1 do begin
      ind = WHERE((r2 ge r2_low) and $
                  (r2 lt r2_high), $
                  count)
      if (count eq 0) then begin
          r2_low = r2_high
          r2_high = r2_low + zoneWidth
          CONTINUE
      endif
      r2_hat_mean[zc] = MEAN(r2_hat[ind])
      r2_hat_mean_var[zc] = TOTAL(r2_hat_var[ind]) / count^2.0
      r2_mean[zc] = MEAN(r2[ind])
      r2_low = r2_high
      r2_high = r2_low + zoneWidth
  endfor

;+
; Eliminate no-data values.
;-
  ind = WHERE((r2_hat_mean ne ndv) and $
              (r2_hat_mean_var ne ndv) and $
              (r2_mean ne ndv), count)
  if (count lt 20) then begin
      ERR_MSG, 'WARNING: Too few "zone average" results for a ' + $
               'kriging bias estimate.'
      RETURN, !NULL
  endif
  r2_hat_mean = r2_hat_mean[ind]
  r2_hat_mean_var = r2_hat_mean_var[ind]
  r2_mean = r2_mean[ind]
  numZoneMeans = count

;+
; Perform an unconstrained linear fit for r2_hat_mean vs. r2_mean,
; using r2_hat_var, to estimate the slope to use for the final,
; constrained, fit. 
;-
  intSlp = LINFIT(r2_mean, r2_hat_mean, $
                  MEASURE_ERRORS = SQRT(r2_hat_mean_var))
  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'WARNING: Unhandled math error/s after calling LINFIT:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
  endif

;+
; Perform a least squares fit for r2_hat_mean vs. r2_mean, using
; r2_hat_mean_var and constraining the intercept to 0.0. Note that we
; are defaulting here to the LMFUNCT function, which is a quadratic,
; with the coefficients for the order zero and order 2 terms fixed at
; 0.0.
;-
  lmParams = [0.0, intSlp[1], 0.0]
  lmFitResult = LMFIT(r2_mean, r2_hat_mean, lmParams, $
                      FITA = [0, 1, 0], $
                      MEASURE_ERRORS = SQRT(r2_hat_mean_var), $
                      CHISQ = chiSquared)
  mathErrors = CHECK_MATH(MASK=32+64) ; Clear floating underflow and overflow.
  mathErrors = CHECK_MATH(MASK=147)   ; Other exceptions are unexpected.
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'WARNING: Unhandled math error/s after calling LMFIT:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
  endif

  chiSquaredCutoff = CHISQR_CVF(0.05, count - 1)
  mathErrors = CHECK_MATH()
  if (mathErrors ne 0) then begin
      errMsg = ['Integer divide by zero', $
                'Integer overflow', $
                'Unspecified error 2^2', $
                'Unspecified error 2^4', $
                'Floating divide by zero', $
                'Floating underflow', $
                'Floating overflow', $
                'Floating Illegal operand']
      ERR_MSG, 'WARNING: Unhandled math error/s after calling CHISQ_CVF:'
      for i = 0, 7 do if (ISHFT(mathErrors, -i) and 1) then $
          ERR_MSG, errMsg[i]
  endif

  if (lmParams[0] ne 0.0) then begin
      ERR_MSG, 'WARNING: LMFIT did not constrain zeroth order parameter.'
      RETURN, !NULL
  endif
  if (lmParams[2] ne 0.0) then begin
      ERR_MSG, 'WARNING: LMFIT did not constrain second order parameter.'
      RETURN, !NULL
  endif

;+
; Judge the fit using the coefficient of determination.
;-
  devFromFitSq = (r2_hat_mean - lmFitResult)^2.0
  r2MeanMean = MEAN(r2_mean)
  devFromMeanMeanSq = (r2_hat_mean - r2MeanMean)^2.0
  RSquared = 1.0 - TOTAL(devFromFitSq) / TOTAL(devFromMeanMeanSq)

  krigingBias = lmParams[1]

  goodFit = 1

  if (chiSquared gt chiSquaredCutoff) then begin
      ERR_MSG, 'WARNING: Kriging bias estimate cancelled due to ' + $
               'poor fit (chi squared = ' + $
               STRCRA(chiSquared) + $
               '; 95% cutoff = ' + $
               STRCRA(chiSquaredCutoff) + ').'
      goodFit = 0
  endif

  if (RSquared lt 0.25) then begin
      ERR_MSG, 'WARNING: Kriging bias estimate cancelled due to ' + $
               'poor fit (coefficient of determination = ' + $
               STRCRA(RSquared) + ').'
      goodFit = 0
  endif

  if KEYWORD_SET(verbose) then $
      USR_MSG, 'Kriging bias estimate = ' + $
               STRCRA(krigingBias) + $
               '; coefft. of determination = ' + $
               STRCRA(RSquared)

  if NOT(goodFit) then krigingBias = !NULL

  if KEYWORD_SET(PNGImage) then begin

;+
;     Scatter plot "zone" averages and standard deviation and show the
;     fit.
;-

;+
;     Remove .png or .PNG extension.
;-
      if ((STRMID(PNGImage, 3, /REVERSE_OFFSET) eq '.png') or $
          (STRMID(PNGImage, 3, /REVERSE_OFFSET) eq '.PNG')) then $
              PNGImage = STRMID(PNGImage, 0, STRLEN(PNGImage) - 4)

      if NOT(ARG_PRESENT(utilsDir)) then begin
;+
;         Define the directory for utility programs. This is a
;         copy/paste from sfav2.pro.
;-
          utilsDir = GETENV('SFAV2_UTILS_DIR')
          if (utilsDir eq '') then begin
              if LMGR(/RUNTIME) then $
                  utilsDir = NSAPrefix + '/gisrs/idl/snowfall_v2/utils' $
              else $
                  utilsDir = '/nwcdev/nsadev/snowfall_v2_devel' + $
                             '/national_snowfall_analysis/utils'
          endif
      endif

      if NOT(FILE_TEST(utilsDir, /DIR, /READ)) then begin
          if FILE_TEST(utilsDir, /DIR) then $
              ERR_MSG, 'ERROR: Utility directory ' + utilsDir + $
                       ' is not readable by this user.' $
          else $
              ERR_MSG, 'ERROR: Utility directory ' + utilsDir + ' not found.'
          RETURN, krigingBias
      endif

      oldDevice = !D.Name
      oldFont = !P.Font
      SET_PLOT, 'PS'
      DEVICE, FILE = PNGImage + '.ps'
      DEVICE, /COLOR, BITS = 8
      LOADCT, 0
      !P.Font = 1 ; TrueType
      DEVICE, SET_FONT = 'DejaVuSans', /TT_FONT

      if NOT(KEYWORD_SET(title)) then title = 'Kriging Bias'
                    
      bound = CEIL(MAX(ABS([r2_mean, r2_hat_mean])))
      PLOT, r2_mean, r2_hat_mean, /NODATA, $
            XRANGE = [-bound, bound], XSTYLE = 1, $
            YRANGE = [-bound, bound], YSTYLE=1, $
            TITLE = title, $
            XTITLE = 'Kriging Input (Local Mean, Inches)', $
            YTITLE = 'Sampled Kriging Output (Local Mean, Inches)', $
            POS = [0.1, 0.1, 0.95, 0.9]

      ;DRAW_ERROR_BARS, r2_mean, r2_hat_mean, $
      ;                 SQRT(r2_hat_mean_var), COLOR = 100
      theta = FINDGEN(33) / 32.0 * 2.0 * !Pi
      USERSYM, COS(theta), SIN(theta), /FILL
      OPLOT, r2_mean, r2_hat_mean, PSYM = 8, SYMSIZE = 0.5

;+
;     For each zone calculate the RMS difference between r2_hat and
;     r2, and the RMS difference between r2_hat / lmParams[1] and r2.
;     If the latter is a bigger difference (i.e., the cure is worse
;     than the disease), plot the r2_hat_mean value as an
;     "x". Otherwise plot the r2_hat_mean value as a diamond.

      ozc = 0 ; output zone counter
      r2_low = MIN(r2) - 0.5 * zoneWidth
      r2_high = r2_low + zoneWidth
      meanImproved = 0L
      meanWorsened = 0L
      pointsImproved = 0L
      pointsWorsened = 0L
      pointsImprovedMean = 0.0
      pointsWorsenedMean = 0.0
      ;PRINT, '# zones with mean values: ', numZoneMeans
      for zc = 0, numZones - 1 do begin
          ind = WHERE((r2 ge r2_low) and $
                      (r2 lt r2_high), $
                      count)
          if (count eq 0) then begin
              r2_low = r2_high
              r2_high = r2_low + zoneWidth
              CONTINUE
          endif
          if ((r2_mean[ozc] lt r2_low) or $
              (r2_mean[ozc] ge r2_high)) then STOP ; PROGRAMMING CHECK
          RMSDBefore = SQRT(MEAN((r2_hat[ind] - r2[ind])^2.0))
          RMSDAfter = SQRT(MEAN((r2_hat[ind] / lmParams[1] - r2[ind])^2.0))
          r2_hat_mean_after = r2_hat_mean[ozc] / lmParams[1]
          before = r2_hat_mean[ozc] - r2_mean[ozc]
          after = r2_hat_mean_after - r2_mean[ozc]
          if (ABS(before) ge ABS(after)) then begin
              meanImproved++
              ;meanString = 'improved ' + STRCRA(before) + $
              ;             ' to ' + STRCRA(after)
          endif else begin
              meanWorsened++
              ;meanString = 'worsened ' + STRCRA(before) + $
              ;             ' to ' + STRCRA(after)
          endelse
          if (RMSDAfter gt RMSDBefore) then begin
              pointsWorsened++
              pointsWorsenedMean = pointsWorsenedMean + (RMSDAfter - RMSDBefore)
              symbol = 7
              ;PRINT, r2_mean[ozc], ' WORSE ', RMSDBefore, RMSDAfter, ' average ' + meanString
          endif else begin
              pointsImproved++
              pointsImprovedMean = pointsImprovedMean + (RMSDBefore - RMSDAfter)
              symbol = 4
              ;PRINT, r2_mean[ozc], ' BETTER ', RMSDBefore, RMSDAfter, ' average ' + meanString
          endelse

          OPLOT, !X.CRange, !X.CRange, COLOR = 200
          OPLOT, !X.CRange, [0.0, 0.0], COLOR = 200
           if (ABS(r2_hat_mean_after) lt bound) then begin
             PLOTS, [r2_mean[ozc], r2_mean[ozc]], $
                     [MEAN(r2_hat[ind]), MEAN(r2_hat[ind] / lmParams[1])], $
                     LINESTYLE = 0, COLOR = 200
              PLOTS, r2_mean[ozc], MEAN(r2_hat[ind] / lmParams[1]), $
                     PSYM = symbol, COLOR = 200, SYMSIZE = 0.5
          endif else begin
              PLOTS, [r2_mean[ozc], r2_mean[ozc]], $
                     [MEAN(r2_hat[ind]), $
                      MEAN(r2_hat[ind]) / ABS(MEAN(r2_hat[ind])) * bound], $
                     LINESTYLE = 0, COLOR = 200
          endelse
          ;    PLOTS, r2_mean[ozc], r2_hat_mean_after, PSYM = symbol, $
          ;           COLOR = 200
          ;PLOTS, r2_mean[ozc], r2_hat_mean[ozc], PSYM = symbol, $
          ;       COLOR = 0
          ;PRINT, r2_mean[ozc], r2_hat_mean[ozc], r2_hat_mean_after
          r2_low = r2_high
          r2_high = r2_low + zoneWidth
          ozc++
      endfor
      ;PRINT, 'mean improved ', meanImproved, ' worsened ', meanWorsened
      ;PRINT, 'points improved ', pointsImproved, ' worsened ', pointsWorsened
      ;PRINT, 'points improved mean ', pointsImprovedMean / pointsImproved
      ;PRINT, 'points worsened mean ', pointsWorsenedMean / pointsWorsened

      ;OPLOT, r2_mean, r2_hat_mean, PSYM=4

      OPLOT, !X.CRange, lmParams[1] * !X.CRange
      x1 = !X.CRange[0] + 0.5 * (!X.CRange[1] - !X.CRange[0])
      y1 = !Y.CRange[0] + 0.2 * (!X.CRange[1] - !X.CRange[0])
      charSize = 1.0

      if NOT(goodFit) then begin
          y1 = y1 + 0.04 * (!Y.CRange[1] - !Y.CRange[0])
          XYOUTS, x1, y1, 'POOR FIT', CHARSIZE = charSize
          y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      endif
      XYOUTS, x1, y1, 'Slope (Bias): ' + $
              STRCRA(STRING(lmParams[1], FORMAT = '(F8.3)')), $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, 'Coefft. of Determination (R!U2!N): ' + $
              STRCRA(STRING(RSquared, FORMAT = '(F6.3)')), $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, 'Chi Squared Statistic: ' + $
              STRCRA(STRING(chiSquared, FORMAT = '(F8.3)')), $
              CHARSIZE = charSize
      y1 = y1 - 0.04 * (!Y.CRange[1] - !Y.CRange[0])
      XYOUTS, x1, y1, 'Chi Squared Cutoff (95%): ' + $
              STRCRA(STRING(chiSquaredCutoff, FORMAT = '(F8.3)')), $
              CHARSIZE = charSize
      DEVICE, /CLOSE
      SET_PLOT, oldDevice
      !P.FONT = oldFont
      cmd = utilsDir + $
            '/pstopng ' + PNGImage + '.ps'
      SPAWN, cmd, EXIT_STATUS = status
      if (status ne 0) then begin
          ERR_MSG, 'WARNING: Failed to convert ' + $
                   PNGImage + '.ps to PNG format.'
      endif else begin
          cmd = 'mogrify -trim -border 4% -bordercolor white ' + $
                PNGImage + '.png'
          SPAWN, cmd, EXIT_STATUS = status
          if (status ne 0) then $
              ERR_MSG, 'WARNING: Failed to fine-tune ' + $
                       PNGImage + '.png.'
          FILE_DELETE, PNGImage + '.ps'
      endelse

  endif

;  STOP

  RETURN, krigingBias

end

