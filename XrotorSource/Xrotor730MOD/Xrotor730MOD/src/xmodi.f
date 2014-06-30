
      SUBROUTINE MODI
      INCLUDE 'XROTOR.INC'
      CHARACTER*4 COMAND, CONSTR, ANS
      CHARACTER*132 COMARG, PARARG
      CHARACTER*12 CHPARM(11)
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR, OK
C
C----------------------------------------------------------
C     Modify rotor
C----------------------------------------------------------
      PLFACD = 0.6
      PLFAC1 = 0.7
      PLFAC2 = 0.85
      XORG  = 0.15
      YORG  = 0.10
C
      CFACA = 1.0
      CFACB = 0.0
      TDEG  = 0.0
C
      GREEK = .FALSE.
C
 900  CONTINUE
      CALL ASKC('.MODI^',COMAND,COMARG)
C
      DO I=1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
      ENDDO
      NINPUT = 0
      CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
      NINPUT = 0
      CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C
      IF(COMAND.EQ.'    ') RETURN
      IF(COMAND.EQ.'?   ') WRITE(*,1100)
      IF(COMAND.EQ.'?   ') GO TO 900
      IF(COMAND.EQ.'BLAD') GO TO 10
      IF(COMAND.EQ.'MODB') GO TO 30
      IF(COMAND.EQ.'MODC') GO TO 32
      IF(COMAND.EQ.'SCAL') GO TO 34
      IF(COMAND.EQ.'TLIN') GO TO 36
      IF(COMAND.EQ.'RTIP') GO TO 40
      IF(COMAND.EQ.'RHUB') GO TO 42
      IF(COMAND.EQ.'RWAK') GO TO 44 
      IF(COMAND.EQ.'RAKE') GO TO 46 
      IF(COMAND.EQ.'XPAX') GO TO 48
      IF(COMAND.EQ.'CLPI') GO TO 50
      IF(COMAND.EQ.'CLPO') GO TO 60
      IF(COMAND.EQ.'OPTI') GO TO 80
      IF(COMAND.EQ.'PLOT') GO TO 90
      IF(COMAND.EQ.'ANNO') GO TO 92
      IF(COMAND.EQ.'HARD') GO TO 94
      IF(COMAND.EQ.'SIZE') GO TO 96
      WRITE(*,1000) COMAND
      GO TO 900
C
C---------------------------------------------------------------------
 10   IF(NINPUT.GE.1) THEN
       NBLDS = IINPUT(1)
      ELSE
       CALL ASKI('Enter new number of blades^',NBLDS)
      ENDIF
      GO TO 900
C---------------------------------------------------------------------
 30   CONTINUE
      WRITE(*,*) 'Funzione disabilitata'
      GO TO 900
C---------------------------------------------------------------------
 32   CONTINUE
      WRITE(*,*) 'Funzione disabilitata'
      GO TO 900
C---------------------------------------------------------------------
 34   IF(NINPUT.GE.2) THEN
       CFACA = RINPUT(1)
       CFACB = RINPUT(2)
      ELSE
       WRITE(*,*) ' '
       WRITE(*,*) '(Chord)new = (Chord)old * [A + B(r/R)]'
       CALL ASKR('Enter constant chord scaling factor A^',CFACA)
       CALL ASKR('Enter  linear  chord scaling factor B^',CFACB)
      ENDIF
C
      DO I=1, II
        CH(I) = CH(I) * (CFACA + CFACB*XI(I))
      ENDDO
      GO TO 900
C
C---------------------------------------------------------------------
 36   IF(NINPUT.GE.1) THEN
       TDEG = RINPUT(1)
      ELSE
       CALL ASKR('Tip angle change (deg)^',TDEG)
      ENDIF
C
      TRAD = TDEG * PI/180.
      DO I=1, II
        BETA(I)  = BETA(I)  + TRAD * XI(I)/XITIP
        BETA0(I) = BETA0(I) + TRAD * XI(I)/XITIP
      ENDDO
      GO TO 900
C
C--------------------------------------------------------------
 40   IF(NINPUT.GE.1) THEN
       RADNEW = RINPUT(1)
      ELSE
       RADNEW = RAD
       WRITE(*,1040) RADNEW
 1040  FORMAT(/1X,'Current tip radius =', F9.4)
       CALL ASKR('Enter new tip radius (m)^',RADNEW)
      ENDIF
      XI0 = XI0 * RAD / RADNEW
      XW0 = XW0 * RAD / RADNEW
      RAD = RADNEW
      CALL SETX
      CALL XWINIT
      GO TO 900
C
C--------------------------------------------------------------
 42   IF(NINPUT.GE.1) THEN
       RHUB = RINPUT(1)
      ELSE
       RHUB = XI0*RAD
       WRITE(*,1042) RHUB
 1042  FORMAT(/1X,'Current hub radius =', F9.4)
       CALL ASKR('Enter new hub radius (m)^',RHUB)
      ENDIF
      XI0 = RHUB / RAD
      CALL SETX
      CALL XWINIT
      GO TO 900
C
C--------------------------------------------------------------
 44   IF(NINPUT.GE.1) THEN
       RWAK = RINPUT(1)
      ELSE
       RWAK = XW0*RAD
       WRITE(*,1044) RWAK
 1044  FORMAT(/1X,'Current hub wake displacement radius =', F9.4)
       CALL ASKR('Enter new hub wake displacement radius (m)^',RWAK)
      ENDIF
      XW0 = RWAK / RAD
      CALL XWINIT
      GO TO 900
C
C--------------------------------------------------------------
 46   IF(NINPUT.GE.1) THEN
       RAKD = RINPUT(1)
      ELSE
       RAKD = RAKE*180./PI
       WRITE(*,1046) RAKD
 1046  FORMAT(/1X,'Current blade rake angle =', F9.4)
       CALL ASKR('Enter new blade rake angle (deg)^',RAKD)
      ENDIF
      RAKE = RAKD * PI/180.
      GO TO 900
C
C---------------------------------------------------------------------
 48   IF(NINPUT.GE.1) THEN
       XPITCH = RINPUT(1)
      ELSE
       WRITE(*,1048) XPITCH
 1048  FORMAT(/1X,'Current pitch-axis x/c location^', F9.4)
       CALL ASKR('Enter new pitch-axis x/c location^',XPITCH)
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C---- clip inner rotor radius
 50   IF(NINPUT.GE.1) THEN
       RADNEW = RINPUT(1)
      ELSE
       RADINR = XI0*RAD
       WRITE(*,1060) RADINR
 1050  FORMAT(/1X,'Current hub radius =', F9.4)
       CALL ASKR('Enter new hub radius (m)^',RADINR)
      ENDIF
C
      DO I = 1, II
        W0(I) = XI(I)
        W1(I) = CH(I)
        W3(I) = BETA(I)
      ENDDO
      CALL SPLINE(W1,W2,W0,II)
      CALL SPLINE(W3,W4,W0,II)
C
      XI0 = RADINR / RAD
      CALL SETX
      CALL XWINIT
C
      DO I = 1, II
ccc        CHOLD = CH(I)
ccc        BETOLD = BETA(I)
        CH(I)   = SEVAL(XI(I),W1,W2,W0,II)
        BETA(I) = SEVAL(XI(I),W3,W4,W0,II)
ccc        write(*,*) 'clipinr i,ch,beta ',i,chold,ch(i),betold,beta(i)
      ENDDO
      GO TO 900
C
C---------------------------------------------------------------------
C---- clip outer rotor radius
 60   IF(NINPUT.GE.1) THEN
       RADNEW = RINPUT(1)
      ELSE
       WRITE(*,1060) RAD
 1060  FORMAT(/1X,'Current tip radius =', F9.4)
       CALL ASKR('Enter new tip radius (m)^',RADNEW)
      ENDIF
C
      DO I = 1, II
        W0(I) = XI(I)
        W1(I) = CH(I)
        W3(I) = BETA(I)
      ENDDO
      CALL SPLINE(W1,W2,W0,II)
      CALL SPLINE(W3,W4,W0,II)
C
      RRAT = RAD/RADNEW
C
      XI0 = XI0 * RRAT
      XW0 = XW0 * RRAT
      RAD = RADNEW
      CALL SETX
      CALL XWINIT
C
      DO I = 1, II
        CH(I)   = SEVAL(XI(I)/RRAT,W1,W2,W0,II) * RRAT
        BETA(I) = SEVAL(XI(I)/RRAT,W3,W4,W0,II)
      ENDDO
      GO TO 900
C
C---------------------------------------------------------------------
C---- optimize twist
 80   CONTINUE
      TDES = TTOT
      PDES = PTOT
      IF(DEST) WRITE(*,*) 'Thrust will be held fixed'
      IF(DESP) WRITE(*,*) 'Power  will be held fixed'
C
C---- save current twist distribution
      DO I=1, II
        W5(I) = BETA(I)
        W6(I) = BETA0(I)
      ENDDO
      CONV = .FALSE.
C
C---- initial guess for 1/(ideal efficiency)
      EFFINV = PWAK/TWAK
C
C---- initial guess for optimum Gamma(r) using Betz-Prandtl criterion
      BLDS = FLOAT(NBLDS)
      ZETA = 2.0*(EFFINV - 1.0)
      SFAC = SQRT(1.0 + 1.0/ADW**2)
      DO I=1, II
        YY = XW(I)/ADV
        ARG = MIN(20.0, 0.5*BLDS*(1.0 - XW(I)/XWTIP)*SFAC)
        EK = EXP(-ARG)
        FK = SQRT(1.0 - EK*EK)
        F = ATAN2(FK,EK)*2.0/PI
        GAM(I) = F*YY*YY/(1.0+YY*YY) * (2.0*PI*ADV/BLDS) * ZETA
      ENDDO
C
C---- Converge MIL rotor (fixed chord, varying twist, varying CL case)
      CALL DESMIL(3)
C
      IF(CONV) THEN
C
C----- display blade twist changes and solution
       WRITE(*,2500) (I,XI(I),(BETA(I)-W5(I))*180.0/PI, I=1, II)
C
       BRLX = 1.0
       WRITE(*,1500) BRLX
 1500  FORMAT(/' Enter relaxation factor for blade angle changes:',F8.3)
       CALL READR(1,BRLX,ERROR)
C
       IF(BRLX .NE. 0.0) THEN
         DO I=1, II
           BETA(I)  = W5(I) + BRLX*(BETA(I) -W5(I))
           BETA0(I) = W6(I) + BRLX*(BETA0(I)-W6(I))
         ENDDO
C
         IF(DEST) THEN
           TSPEC = TTOT * (RHO*VEL**2*RAD**2)
           CALL APER(1,2,.TRUE.)
         ENDIF
         IF(DESP) THEN
           PSPEC = PTOT * (RHO*VEL**3*RAD**2)
           CALL APER(3,2,.TRUE.)
         ENDIF
         IF(CONV) CALL OUTPUT(LUWRIT)
       ELSE
         WRITE(*,*)
         WRITE(*,*) 'Blade geometry not modified.'
       ENDIF
C
      ELSE
C
C----- restored clobbered twist distribution
       DO I=1, II
         BETA(I)  = W5(I)
         BETA0(I) = W6(I)
       ENDDO
       WRITE(*,*) 'Blade twist distribution restored'
C
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
 90   WRITE(*,*) 'Funzione disabilitata'
      GO TO 900
C
C---------------------------------------------------------------------
 92   WRITE(*,*) 'Funzione disabilitata'
      GO TO 900
C
C---------------------------------------------------------------------
 94   WRITE(*,*) 'Funzione disabilitata'
      GO TO 900
C
C---------------------------------------------------------------------
 96   IF(NINPUT.GE.1) THEN
       SIZE = RINPUT(1)
      ELSE
       WRITE(*,*) 'Current plot-object size =', SIZE
       CALL ASKR('Enter new plot-object size^',SIZE)
      ENDIF
      GO TO 900
C.....................................................................
C
 1000 FORMAT(1X,A4,' command not recognized.' //
     &             '  Type "?" for list, <Return> to exit menu.')
 1100 FORMAT(
     &  /'   BLAD i  Set new number of blades'
     &  /'   MODC    Modify chord distribution'
     &  /'   MODB    Modify blade twist angle distribution'
     &  /'   SCAL rr Scale current chords'
     &  /'   TLIN r  Add linear blade twist (proportional to r/R)'
     &  /'   RTIP r  Change tip radius (preserve chord/R)'
     &  /'   RHUB r  Change hub radius'
     &  /'   RWAK r  Change hub wake displacement body radius'
     &  /'   RAKE r  Change blade rake angle'
     &  /'   XPAX r  Change pitch-axis x/c'
     & //'   CLPI r  Clip current prop at inner radius (preserve chord)'
     &  /'   CLPO r  Clip current prop at outer radius (preserve chord)'
     & //'   OPTI    Optimize blade twist angles for current planform'
     & //'   PLOT i  Plot various rotor parameters'
     &  /'   ANNO    Annotate current plot'
     &  /'   HARD    Hardcopy current plot'
     &  /'   SIZE r  Change plot-object size')
C 
 2000 FORMAT(/'  0   CANCEL'
     &       /'  1   Geometry'
     &       /'  2   CL, Circulation, and Local Efficiency  vs  Radius')
 2500 FORMAT(/' Blade angle changes'
     &       /'   i    r/R   d(beta)', 100(/ 1X, I4, F7.3, F9.3) )
C               15  0.785  -10.345
      END ! MODI


