C
      SUBROUTINE CROTOR
      INCLUDE 'XROTOR.INC'
      CHARACTER*4 COMAND, ANS, CONAND
      CHARACTER*132 COMARG, ANSARG, CONARG
      CHARACTER*1 CHKEY
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR
c
C--------------------------------------------------------------
C     Counter-rotation extension for XROTOR
C     Philip Carter, Esotec Developments -  May 2008 
C     Esotec code is Free Software
C     Based on XOPER
C     Kudos to Mark Drela and Harold Youngren
C
C     Version 0.3
C--------------------------------------------------------------
C
      PLFAC1 = 0.7
      PLFAC2 = 0.8
      PLFACD = 0.6
      XORG = 0.15
      YORG = 0.10
C
      GREEK  = .FALSE.
      LFILER = .FALSE.
C
C--------------------------------------------------------------
C---  Reimport current rotor geometry (if still current)
C
      LCON = .FALSE.
      WRITE(*,*)
C
      IF(LCRIN .AND. LFWD) THEN
         IF(NAME .EQ. NAMECR(1)) THEN
            CALL STORCR(1) 
            WRITE(*,*) 'FWD rotor geometry updated'
            IF (FNAMEF.NE.FNAME) THEN
               WRITE(*,*) 'FWD rotor filename updated'
               FNAMEF = FNAME
            ENDIF
         ELSE
            WRITE(*,*) 'Current rotor has changed'
            FNAMEF = ' '
            CALL LOADF
            IF(LFILER) GO TO 900
         ENDIF
      ENDIF
C
      IF(LCRIN .AND. .NOT.LFWD) THEN
         IF(NAME .EQ. NAMECR(2)) THEN
            CALL STORCR(2)
            WRITE(*,*) 'AFT rotor geometry updated'
            IF (FNAMEA.NE.FNAME) THEN
               WRITE(*,*) 'AFT rotor filename updated'
               FNAMEA = FNAME
            ENDIF
         ELSE
            WRITE(*,*) 'Current rotor has changed'
            FNAMEA = ' '
            CALL LOADA
            IF(LFILER) GO TO 900
         ENDIF
      ENDIF
C
      VEL = VELCR
C
C--------------------------------------------------------------
C---  Display current CR configuration
C
      IF(LCRIN) THEN 
         CALL DISPIN(LUWRIT)
         IF(LFWD) THEN
           WRITE(*,*) 'FWD rotor loaded'
         ELSE
           WRITE(*,*) 'AFT rotor loaded'
         ENDIF
      ENDIF
C
C--------------------------------------------------------------
C
 900  CONTINUE
      LFILER = .FALSE.
C
      CALL ASKC('.CROT^',COMAND,COMARG)
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
C--- tidy up for exit
C
      IF(COMAND.EQ.'    ') THEN
         IF(LCRIN) THEN
            WRITE(*,*)
            IF(LFWD) THEN
              WRITE(*,4300) NAMECR(1)
              FNAME = FNAMEF
            ELSE
              WRITE(*,4400) NAMECR(2)
              FNAME = FNAMEA
            ENDIF
         ENDIF
         CALL CLRZOOM
         RETURN
      ENDIF
C
      IF(COMAND.EQ.'?   ') WRITE(*,1100)
      IF(COMAND.EQ.'?   ') GO TO 900
      IF(COMAND.EQ.'VRTX') GO TO 1
      IF(COMAND.EQ.'FORM') GO TO 2
      IF(COMAND.EQ.'WAKE') GO TO 3
      IF(COMAND.EQ.'TERS') GO TO 4
      IF(COMAND.EQ.'HARD') GO TO 6
      IF(COMAND.EQ.'SIZE') GO TO 8
      IF(COMAND.EQ.'ANNO') GO TO 9
      IF(COMAND.EQ.'DISP') GO TO 10
      IF(COMAND.EQ.'NAME') GO TO 15
      IF(COMAND.EQ.'WRIT') GO TO 20
      IF(COMAND.EQ.'DUCT') GO TO 22
      IF(COMAND.EQ.'VRAT') GO TO 24
      IF(COMAND.EQ.'PLOT') GO TO 30
      IF(COMAND.EQ.'ATMO') GO TO 35
      IF(COMAND.EQ.'VELO') GO TO 38
      IF(COMAND.EQ.'ANGL') GO TO 50
      IF(COMAND.EQ.'SAVE') GO TO 60
C
      IF(COMAND.EQ.'ITER') GO TO 75
      IF(COMAND.EQ.'INIT') GO TO 76
      IF(COMAND.EQ.'REIN') GO TO 78
c
      IF(COMAND.EQ.'INPU') GO TO 100
      IF(COMAND.EQ.'ROTF') GO TO 120
      IF(COMAND.EQ.'ROTA') GO TO 130
      IF(COMAND.EQ.'POWF') GO TO 140
      IF(COMAND.EQ.'POWA') GO TO 150
      IF(COMAND.EQ.'RPMF') GO TO 160
      IF(COMAND.EQ.'RPMA') GO TO 170
      IF(COMAND.EQ.'VELW') GO TO 180
      IF(COMAND.EQ.'PRPM') GO TO 190
C
      IF(COMAND.EQ.'DISI') GO TO 200
      IF(COMAND.EQ.'DISS') GO TO 205
      IF(COMAND.EQ.'DISV') GO TO 210
      IF(COMAND.EQ.'WRIV') GO TO 220
      IF(COMAND.EQ.'CRIT') GO TO 240
      IF(COMAND.EQ.'CCON') GO TO 250
      IF(COMAND.EQ.'PLIN') GO TO 260
      IF(COMAND.EQ.'VCLR') GO TO 270
      IF(COMAND.EQ.'SYNC') GO TO 275
      IF(COMAND.EQ.'CLCR') GO TO 280
      IF(COMAND.EQ.'BLEN') GO TO 285
C
      IF(COMAND.EQ.'FWD ') GO TO 300
      IF(COMAND.EQ.'AFT ') GO TO 320
      IF(COMAND.EQ.'DESC') GO TO 330
      IF(COMAND.EQ.'POWE') GO TO 400
      IF(COMAND.EQ.'RPM ') GO TO 400
c
c
      IF(COMAND.EQ.'Z   ') THEN
       CALL USETZOOM(.TRUE.,.TRUE.)
       CALL REPLOT(IDEV)
       GO TO 900
      ENDIF
      IF(COMAND.EQ.'U   ') THEN
       CALL CLRZOOM
       CALL REPLOT(IDEV)
       GO TO 900
      ENDIF
C
      WRITE(*,1050) COMAND
      GO TO 900
C
C
C---------------------------------------------------------------------
    1 VRTX = .NOT.VRTX
      IF(.NOT.VRTX) WRITE(*,*)'Discrete Vortex Formulation deselected'
      IF(VRTX)      WRITE(*,*)'Discrete Vortex Formulation selected'
      GO TO 900
C
C---------------------------------------------------------------------
    2 FAST = .NOT.FAST
      IF(FAST)      WRITE(*,*)'Graded Momentum Formulation selected'
      IF(.NOT.FAST) WRITE(*,*)'Potential Formulation selected'
      GO TO 900
C
C---------------------------------------------------------------------
    3 FREE = .NOT.FREE
      IF(FREE)      WRITE(*,*)'Self-deforming wake selected'
      IF(.NOT.FREE) WRITE(*,*)'Rigid wake selected'
      GO TO 900
C
C---------------------------------------------------------------------
    4 TERSE = .NOT.TERSE
      IF(TERSE)      WRITE(*,*)'Terse output selected'
      IF(.NOT.TERSE) WRITE(*,*)'Verbose output selected'
      GO TO 900
C
C---------------------------------------------------------------------
C--- Hardcopy current plot
    6 IF(LPLOT) THEN
       CALL PLEND
       CALL REPLOT(IDEVRP)
      ELSE
       WRITE(*,*) 'No current plot'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change plot size
    8 IF(NINPUT.GE.1) THEN
       SIZE = RINPUT(1)
      ELSE
       WRITE(*,*) 'Current plot size =', SIZE
       CALL ASKR('Enter new plot size^',SIZE)
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Annotate plot
    9 IF(LPLOT) THEN
       CALL ANNOT(1.2*CSIZE)
      ELSE
       WRITE(*,*) 'No current plot'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Display current prop operating point data 
C
   10 CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor'
      ELSE
         WRITE(*,*) 'AFT rotor'
      ENDIF
C
      IF(LCON) THEN
         WRITE(*,*) 'CR system converged'
      ELSE
         WRITE(*,*) 'CR system not converged'
      ENDIF
C
      CALL OUTPUT(LUWRIT)
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change case name
C
   15 CRNAME = COMARG
      IF(CRNAME(1:1).EQ.' ')
     &  CALL ASKS('Enter CR case name (32 characters max)^',CRNAME)
      GO TO 900
C
C---------------------------------------------------------------------
C--- Write current prop operating point data to file
C
   20 IF(COMARG(1:1).NE.' ') SAVFIL = COMARG
      CALL OPFILE(LUSAVE,SAVFIL)      
      CALL DISPIN(LUSAVE)
C
      IF(LFWD) THEN
         WRITE(LUSAVE,*) 'FWD rotor'
      ELSE
         WRITE(LUSAVE,*) 'AFT rotor'
      ENDIF
C
      CALL OUTPUT(LUSAVE)
      CLOSE(LUSAVE)
      GO TO 900
C
C--------------------------------------------------------------
 22   DUCT = .NOT.DUCT
      IF(DUCT) THEN
       WRITE(*,*) 'Duct option selected'
       IF(NINPUT.GE.1) THEN
        URDUCT = RINPUT(1)
       ELSE
        CALL ASKR('Enter Aexit/Aprop for duct^',URDUCT)
       ENDIF
      ELSE
       WRITE(*,*) 'Free-tip option selected'
       URDUCT = 1.0
      ENDIF
      GO TO 900
C
C--------------------------------------------------------------
 24   IF(DUCT) THEN
       IF(NINPUT.GE.1) THEN
        URDUCT = RINPUT(1)
       ELSE
        CALL ASKR('Enter Aexit/Aprop for duct^',URDUCT)
       ENDIF
      ELSE
       WRITE(*,*) '*** Select duct option first'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Plot stuff
   30 IF(NINPUT.GE.1) THEN
       NPLOT = IINPUT(1)
      ELSE
       WRITE(*,2000)
       NPLOT = 3
       CALL ASKI('select plot number^',NPLOT)
      ENDIF
C
      IF(NPLOT.EQ.0) THEN
       GO TO 900
C
      ELSE IF(NPLOT.EQ.1) THEN
       CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC1*SIZE,LPLOT,LLAND)
       CALL PLOT(XORG,YORG,-3)
       CALL GEOPLT('ALUE')
C
      ELSE IF(NPLOT.EQ.2) THEN
       CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC2*SIZE,LPLOT,LLAND)
       CALL PLOT(XORG,YORG,-3)
       CALL CLPLT
C
      ELSEIF(NPLOT.EQ.3) THEN
       CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFACD*SIZE,LPLOT,.NOT.LLAND)
       CALL PLOT(0.175,0.075,-3)
       CALL GEOPLT('AL')
       CALL PLOTABS(0.0,0.0,-3)
       CALL PLOT(0.175,0.875,-3)
       CALL CLPLT
C
      ELSE IF(NPLOT.EQ.4) THEN
       CALL ACLPLT
C
      ELSE IF(NPLOT.EQ.5) THEN
       CALL CASPLT
C
      ELSE IF(NPLOT.EQ.6) THEN
       CALL UVIPLT
C
      ELSE IF(NPLOT.EQ.7) THEN
       CALL TRIPLT
C
      ELSE IF(NPLOT.EQ.8) THEN
       IF(NADD.LT.2) THEN
        WRITE(*,*) 'No slipstream profiles present'
        GO TO 900
       ENDIF
       CALL VELPLT
C
      ELSE IF(NPLOT.EQ.9) THEN
       FNAME = ' '
       CALL REFPLT(FNAME, XYOFF(1),XYOFF(2),XYFAC(1),XYFAC(2),
     &             0.5*CSIZE, 1)
C
      ELSE IF(NPLOT.EQ.10) THEN
        CALL PLOT_DATA(NAME)
C
      ELSE IF(NPLOT.EQ.11) THEN
        CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFACD*SIZE,LPLOT,.NOT.LLAND)
        CALL PLOT(0.175,0.175,-3)
        CALL PRPPLT
C
      ELSE IF(NPLOT.EQ.12) THEN
        IF(LCON) THEN
          CALL CRPLIN
        ELSE
           WRITE(*,*) 'CR System is not converged'
        ENDIF
C
      ELSE
       NINPUT = 0
       GO TO 30
C
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change altitude
   35 IF(NINPUT.GE.1) THEN
       ALT = RINPUT(1)
      ELSE
       CALL ASKR('flight altitude (km)^',ALT)
      ENDIF
      CALL ATMO(ALT,VSO,RHO,RMU)
      CALL FLOSHO(LUWRIT, VSO, RHO, RMU)
      CONV = .FALSE.
      LCON = .FALSE.
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change flight velocity
   38 VELOLD = VEL
      IF(NINPUT.GE.1) THEN
       VEL = RINPUT(1)
      ELSE
       CALL ASKR('flight speed (m/s)^',VEL)
      ENDIF
C--- Change CT,CQ,CP to give same thrust,torque,power
      THR  = TTOT *(RHO*VELOLD**2*RAD**2)
      TTOT = THR / (RHO*VEL**2*RAD**2)
      TRQ  = QTOT *(RHO*VELOLD**2*RAD**3)
      QTOT = TRQ / (RHO*VEL**2*RAD**3)
      PWR  = PTOT *(RHO*VELOLD**3*RAD**2)
      PTOT = PWR / (RHO*VEL**3*RAD**2)
C
      VELCR = VEL
      CONV  = .FALSE.
      LCON  = .FALSE.
      GO TO 900
C
C
C---------------------------------------------------------------------
C--- Change blade angle - current rotor
C
 50   IF(NINPUT.GE.1) THEN
       DELB = RINPUT(1)
      ELSE
         IF(LFWD) THEN
           CALL ASKR('Enter FWD rotor angle change (deg)^',DELB)
         ELSE
           CALL ASKR('Enter AFT rotor angle change (deg)^',DELB)
         ENDIF
      ENDIF
C
      DO I=1, II
        BETA(I)  = BETA(I)  + DELB*PI/180.
        BETA0(I) = BETA0(I) + DELB*PI/180.
      ENDDO
C
      CONV = .FALSE.
      LCON = .FALSE.
      GO TO 900      
CC---------------------------------------------------------------------
C--- Save rotor from within CROTOR
 60   WRITE(*,*)
C
      IF(LFWD) THEN
         WRITE(*,*) 'Saving FWD Rotor to disk'
         WRITE(*,4600) NAMECR(1)
         CALL SAVE(COMARG)
         FNAMEF = FNAME
      ELSE
         WRITE(*,*) 'Saving AFT Rotor to disk'
         WRITE(*,4600) NAMECR(2)
         CALL SAVE(COMARG)
         FNAMEA = FNAME
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- Set max number or iterations for nonlinear solution
 75   IF(NINPUT.GE.1) THEN
       NITERA = IINPUT(1)
      ELSE
       CALL ASKI('Max number of iterations^',NITERA)
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Toggle initialization flag 
 76   LOPRINI = .NOT.LOPRINI
      IF(LOPRINI) THEN
       WRITE(*,*) 'Analysis case will be initialized'
      ELSE
       WRITE(*,*) 'Analysis case will not be initialized'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Reinitialize operating point
 78   CALL REINIT
      GO TO 900
C
C---------------------------------------------------------------------
C
C---------------------------------------------------------------------
C--- INPU - input CR specifications
C
 100  LCRIN = .FALSE.  ! default input has not been entered
C 
C--- Specify propellers
C     
      FNAMEF = ' '
      CALL LOADF
      IF(LFILER) GO TO 900
      WRITE(*,*)
      WRITE(*,*) 'FWD rotor loaded'
C
      FNAMEA = ' '
      CALL LOADA 
      IF(LFILER)  GO TO 900
      WRITE(*,*)
      WRITE(*,*) 'AFT rotor loaded'
C
C---  Specify Power
C
      CALL ASKR('Enter power (W) - FWD rotor^',POWERF)
      CALL ASKR('Enter power (W) - AFT rotor^',POWERA)
C
C---  Specify RPM
C
      IF(LSRPM) THEN
         IF(RRAT.EQ.1.0) THEN
           CALL ASKR('Enter rpm - both rotors^',RPMF)
         ELSE
           CALL ASKR('Enter rpm - FWD rotor^',RPMF)
         ENDIF
         RPMA = RPMF * RRAT
      ELSE
         CALL ASKR('Enter rpm - FWD rotor^',RPMF)
         CALL ASKR('Enter rpm - AFT rotor^',RPMA)
      ENDIF
C
C--- Specifiy flight speed
C
      VELOLD = VEL
      CALL ASKR('Enter flight speed (m/s)^',VELCR)
      VEL = VELCR
C
C--- Change CT,CQ,CP to give same thrust,torque,power
      THR  = TTOT *(RHO*VELOLD**2*RAD**2)
      TTOT = THR / (RHO*VEL**2*RAD**2)
      TRQ  = QTOT *(RHO*VELOLD**2*RAD**3)
      QTOT = TRQ / (RHO*VEL**2*RAD**3)
      PWR  = PTOT *(RHO*VELOLD**3*RAD**2)
      PTOT = PWR / (RHO*VEL**3*RAD**2)
C
C---  End input
C
      LCON = .FALSE.
      LCRIN = .TRUE.
C
      CALL DISPIN(LUWRIT)
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- ROTF - Change forward rotor
C
 120  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      FNAMEF = COMARG
      CALL LOADF
      IF(LFILER) GO TO 900
      CALL DISPIN(LUWRIT)
      WRITE(*,*) 'FWD rotor loaded'
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- ROTA - Change aft propeller
C
 130  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      FNAMEA = COMARG
      CALL LOADA
      IF(LFILER) GO TO 900
      CALL DISPIN(LUWRIT)
      WRITE(*,*) 'AFT rotor loaded'
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- POWF - change power, forward propeller
C
 140  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        POWERF = RINPUT(1)
      ELSE
        CALL ASKR('Enter power (W) - FWD rotor^',POWERF)
      ENDIF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- POWA - change power, aft propeller
C
 150  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        POWERA = RINPUT(1)
      ELSE
        CALL ASKR('Enter power (W) - AFT rotor^',POWERA)
      ENDIF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- RPMF - change rpm, forward propeller
C
 160  IF(.NOT.LCRIN) THEN
        WRITE(*,*) 'Use INPUT command first'
        GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        RPMF = RINPUT(1)
      ELSE
        CALL ASKR('Enter rpm - FWD rotor^',RPMF)
      ENDIF
C
      IF(LSRPM) RPMA = RPMF*RRAT
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C---  RPMA - change rpm, aft propeller
C
 170  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        RPMA = RINPUT(1)
      ELSE
        CALL ASKR('Enter rpm - AFT rotor^',RPMA)
      ENDIF
C
      IF(LSRPM) RPMF = RPMA/RRAT
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- VELW - specify velocity weights
C
 180  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C     
      CALL ASKR('Enter FWD axial velocity weight (0 ===> 1)^',UWTF)
      CALL ASKR('Enter AFT axial velocity weight (0 ===> 1)^',UWTA)
      CALL ASKR('Enter FWD tang. velocity weight (0 , +/-1)^',VWTF)
      CALL ASKR('Enter AFT tang. velocity weight (0 , +/-1)^',VWTA)
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      GO TO 900
C
C
C---------------------------------------------------------------------
C--- PRPM - fixed pitch/rpm toggle
C
 190  LPITCH = .NOT.LPITCH
C
      IF(LPITCH) THEN
         WRITE(*,*) 'Blade pitch is fixed, rpm is variable'
      ELSE
         WRITE(*,*) 'Rpm is fixed, pitch is variable'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- DISI - display CR specifications
C
 200  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- DISS - display external slipstreams
C
 205  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C     
      IF(NADDF.EQ.0 .AND. NADDA.EQ.0) THEN
         WRITE(*,*) 'There are no external slipstreams to display'
         GO TO 900
      ENDIF
C
      IF(RADCR(1).EQ.RADCR(2) .AND. XI0CR(1).EQ.XI0CR(2) .AND.
     &    NADDF.EQ.NADDA) THEN
C
        WRITE(*,3900)
        DO I=1,NADDF
          WRITE(*,4000) RADDF(I),UADDF(I),VADDF(I),UADDA(I),VADDA(I)
        ENDDO
        WRITE(*,4100)
      ELSE
        WRITE(*,3600)
        WRITE(*,3800) (RADDF(I),UADDF(I),VADDF(I), I=1, NADDF)
        WRITE(*,3700)
        WRITE(*,3800) (RADDA(I),UADDA(I),VADDA(I), I=1, NADDA)
        WRITE(*,4200)
      ENDIF
C
      IF(LCON) THEN
         WRITE(*,*) 'Slipstreams are converged'
      ELSE
         WRITE(*,*) 'Slipstreams are not converged'
      ENDIF
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF   
CC      
      GO TO 900
C
C---------------------------------------------------------------------
C--- DISV - display induced velocity profiles
C
 210  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C     
      IF(LCON) THEN
        CALL OUTVEL(LUWRIT)
        WRITE(*,*) 'Slipstream is converged'  
      ELSE
        WRITE(*,*) 'Slipstream is not converged'
        GO TO 900
      ENDIF
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF   
C
      GO TO 900
C
CC---------------------------------------------------------------------
C--- WRIV - write velocity profiles to disk
C
 220  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      IF(.NOT.LCON) THEN
         WRITE(*,*) 'Slipstream is not converged'
         GO TO 900
      ENDIF
C
      IF(COMARG(1:1).NE.' ') SAVFIL = COMARG
C
      CALL OPFILE(LUSAVE,SAVFIL)      
      CALL DISPIN(LUSAVE)
      CALL OUTVEL(LUSAVE)
C
      CLOSE(LUSAVE)
      GO TO 900
C
C---------------------------------------------------------------------
C--- CRIT - set CR iteration limit
C
 240  IF(NINPUT.GE.1) THEN
        ICRITL = IINPUT(1)
      ELSE
        CALL ASKI('Enter max number of CR iterations^',ICRITL)
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- CCON - set CR convergence delta
C
 250  IF(NINPUT.GE.1) THEN
        CRCON = RINPUT(1)
      ELSE
        CALL ASKR('Enter new CR convergence delta^',CRCON)
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- PLIN - plot combined CR induced velocities
C
 260  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      IF(.NOT.LCON) WRITE(*,*) 'Slipstream is not converged'
C
      CALL CRPLIN
      GO TO 900
C
C---------------------------------------------------------------------
C--- VCLR - initialize slipstream velocities
C
 270  NADD  = 0
      NADDF = 0
      NADDA = 0
C
      WRITE(*,*) 'Current and stored slipstreams zeroed'
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- SYNC - synchronize rpms toggle
C
 275  LSRPM = .NOT.LSRPM
C
      WRITE(*,*)
      IF(LSRPM) THEN
         WRITE(*,*) 'Rotor speeds will be synchronized'
         CALL ASKR('Enter speed ratio (Rpm_AFT / Rpm_FWD)^',RRAT)         
         RPMA = RPMF * RRAT
      ELSE
         WRITE(*,*) 'Rotor speeds will not be synchronized'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- CLCR - change MIL design lift coefficients
C
 280  IF(NINPUT.GE.1) THEN
        CRCLR = RINPUT(1)
        CRCLT = CRCLR
      ELSE
        WRITE(*,*)
        WRITE(*,4500) CRCLR,CRCLT
        CALL ASKR('Enter design CL at blade root^',CRCLR)
        CALL ASKR('Enter design CL at blade tip^',CRCLT)
      ENDIF
C
      GO TO 900
C
C
C---------------------------------------------------------------------
C--- BLEN - blend between loaded rotors
C
 285  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      WRITE(*,*)
      IF(LFWD) THEN
        WRITE(*,*) 'Replacing FWD rotor: ', NAMECR(1)
      ELSE
        WRITE(*,*) 'Replacing AFT rotor: ', NAMECR(2)
      ENDIF
C
      WRITE(*,*)'<a> to abort  <return> to leave name unchanged'
C
      CALL ASKS('Enter new rotor name^',BNAME)
C
      IF(BNAME.EQ.'A' .OR. BNAME.EQ.'a') GO TO 900
      IF(BNAME(1:1) .NE. ' ') NAME = BNAME
C
      WRITE(*,*)
      WRITE(*,*)   '       FWD   0 <------> 1   AFT'
C
      CALL ASKR('Enter rotor interpolation factor^',BLENDF)
C
      LBA = .FALSE.
      LBC = .FALSE.
C
 288  CALL ASKS('Blend angles, chords, or both? (a,c,b)^',CHKEY)
C
      IF (CHKEY.EQ.' ') THEN
         WRITE(*,*) 'Interpolation aborted - geometry unchanged'
         GO TO 900
      ENDIF
C         
      IF    (CHKEY.EQ.'A' .OR. CHKEY.EQ.'a') THEN
         LBA = .TRUE.
      ELSEIF(CHKEY.EQ.'C' .OR. CHKEY.EQ.'c') THEN
         LBC = .TRUE.
      ELSEIF(CHKEY.EQ.'B' .OR. CHKEY.EQ.'b') THEN
         LBA = .TRUE.
         LBC = .TRUE.
      ELSE
         GO TO 288
      ENDIF
C                  
      CALL CRBLEND
C
      GO TO 900
C
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C--- FWD - Run forward propeller in current slipstream
C
 300  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      RPMINF = RPMF
      IF(NINPUT.GE.1) RPMINF = RINPUT(1)
C
      IF(.NOT.LFWD) CALL LOADCR(1)
C
      RPM = RPMINF
      ADV = VEL / (RAD*RPM*PI/30.)
      CONV = .FALSE.
      CALL APER(4,2,LOPRINI)
C
      WRITE(*,3400) CRNAME
      CALL OUTPUT(LUWRIT)
      WRITE(*,*)
C
      IF(CONV) THEN
         IF(LCON) THEN
            IF(RPMINF.EQ.RCONF) THEN
               WRITE(*,*) 'FWD rotor at converged CR state'
            ELSE
               WRITE(*,*) 'FWD rotor off converged CR state'
            ENDIF
         ELSE
            WRITE(*,*) 'FWD rotor - CR system not converged'
         ENDIF
      ELSE
         WRITE(*,*) 'FWD rotor not converged'
      ENDIF
C
      IF(RPMINF.NE.RPMF) WRITE(*,2310) RPMINF
C
C      CALL DISPIN(LUWRIT)
C
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC2*SIZE,LPLOT,LLAND)
      CALL PLOT(XORG,YORG,-3)
      CALL CLPLT
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- AFT - Run aft  propeller in current slipstream
C
 320  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF   
C
      RPMINA = RPMA
      IF(NINPUT.GE.1) RPMINA = RINPUT(1)
C
      IF(LFWD) CALL LOADCR(2)
C
      RPM = RPMINA
      ADV = VEL / (RAD*RPM*PI/30.)
      CONV = .FALSE.
      CALL APER(4,2,LOPRINI)
C
      WRITE(*,3420) CRNAME
      CALL OUTPUT(LUWRIT)
      WRITE(*,*)
C
      IF(CONV) THEN
         IF(LCON) THEN
            IF(RPMINA.EQ.RCONA) THEN
               WRITE(*,*) 'AFT rotor at converged CR state'
            ELSE
               WRITE(*,*) 'AFT rotor off converged CR state'
            ENDIF
         ELSE
            WRITE(*,*) 'AFT rotor - CR system not converged'
         ENDIF
      ELSE
         WRITE(*,*) 'AFT rotor not converged'
      ENDIF
C
      IF(RPMINA.NE.RPMA) WRITE(*,2320) RPMINA
C
C      CALL DISPIN(LUWRIT)
C
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC2*SIZE,LPLOT,LLAND)
      CALL PLOT(XORG,YORG,-3)
      CALL CLPLT
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- DESC - design MIL prop
C
 330  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      IF(.NOT.LCON) THEN
         WRITE(*,*) 'WARNING: slipstream is not converged'
      ENDIF
C
      CALL DESCR
C
      GO TO 900
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C--- Initialize CR Iteration loop
C
 400  IF(.NOT.LCRIN) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      LCONF  = .FALSE.
      LCONA  = .FALSE.
      LCON   = .FALSE.
      LNCON  = .FALSE.
      LSWIT  = .FALSE.
      TTOTF  = 0.0
      TTOTA  = 0.0
      ICRITR = 0
C
C---- process input
C
      POWINF = POWERF
      POWINA = POWERA
      POWINT = POWERF+POWERA
      POWIN  = 0.0
      RPMINF = RPMF
      RPMINA = RPMA
      RPMIN  = 0.0
C
      IF(NINPUT.GE.1) THEN
        IF(COMAND.EQ.'RPM ') THEN
          RPMIN  = RINPUT(1)
          RPMINF = RPMIN
          IF(LSRPM) THEN
            RPMINA = RPMINF*RRAT
          ELSE
            RPMINA = RPMINF
          ENDIF
        ELSE
          POWIN  = RINPUT(1)
          POWINF = POWIN/2.0
          POWINA = POWIN/2.0
        ENDIF
      ENDIF
C
      IF(LFWD) CALL LOADCR(2)
C
C------------------------------------------------------------------------
C---- Iteration loop
C
 410  ICRITR = ICRITR + 1
      LFWD = .NOT.LFWD
      LAUTO  = .FALSE.
C
      IF(LFWD) THEN
         CALL LOADCR(1)
      ELSE 
         CALL LOADCR(2)
      ENDIF
C
      IF(COMAND.EQ.'RPM ') GO TO 440
C
C-------------------------------------------------------------------------
C--- POWE -  specified power
C
 430  IF(LFWD) THEN
         PSPEC = POWINF
         RPM   = RPMINF
      ELSE
         PSPEC = POWINA
         RPM   = RPMINA
      ENDIF
C
      CONV = .FALSE.
      IF(.NOT.LPITCH) THEN
         ADV = VEL/(RAD*RPM*PI/30.0)
         CALL APER(3,1,LOPRINI)
      ELSE
         IF(.NOT.LSRPM) THEN
           CALL APER(3,2,LOPRINI)
C
           IF(LFWD) THEN
             RPMFWD = VEL/(RAD*ADV*PI/30.)
           ELSE
             RPMAFT = VEL/(RAD*ADV*PI/30.)
           ENDIF
C
         ELSE
           IF((LCONA.OR.LCONF) .AND. .NOT.LSWIT) THEN
             RPMINF = (RPMFWD+RPMAFT)/(1.+RRAT)
             RPMINA = RPMINF*RRAT
             LSWIT = .TRUE.
             LCONF = .FALSE.
             LCONA = .FALSE.
             LCON  = .FALSE.
           ENDIF
C
           IF(LSWIT) THEN
             IF(LFWD)THEN
               RPM = RPMINF
             ELSE
               RPM = RPMINA
             ENDIF
             ADV = VEL / (RAD*RPM*PI/30.)
             CONV = .FALSE.
             CALL APER(4,2,LOPRINI)
C
             IF(LFWD) THEN
               POWFWD = PTOT*RHO*VEL**3*RAD**2
             ELSE
               POWAFT = PTOT*RHO*VEL**3*RAD**2
             ENDIF
           ELSE
             CALL APER(3,2,LOPRINI)
C
             IF(LFWD) THEN
               RPMFWD = VEL/(RAD*ADV*PI/30.)
             ELSE
               RPMAFT = VEL/(RAD*ADV*PI/30.)
             ENDIF
C
           ENDIF
        ENDIF
      ENDIF
C
C----- converged: check for invalid pitch change and neg. thrust
C  
      IF(CONV) THEN
        IF(.NOT.LPITCH) THEN
          IF(ABS(DBETA).GT.PI/15.0 .AND. TTOT.LT.0.0) THEN
            WRITE(*,*)
            WRITE(*,*) 'Neg. thrust and excessive blade angle change'
            GO TO 590
          ENDIF
        ENDIF
        GO TO 445
      ENDIF
C
C----- auto loop or not converged
C
      IF(LAUTO) GO TO 620      ! not converged, auto loop
      GO TO 590                ! not converged, first time
C
C
C---------------------------------------------------------------------
C--- RPM - constant pitch and rpm
C
 440  IF(LFWD)THEN
         RPM = RPMINF
      ELSE
         RPM = RPMINA
      ENDIF
C
      ADV = VEL / (RAD*RPM*PI/30.)
      CONV = .FALSE.
      CALL APER(4,2,LOPRINI)
C
      IF(LFWD) THEN
         POWFWD = PTOT*RHO*VEL**3*RAD**2
      ELSE
         POWAFT = PTOT*RHO*VEL**3*RAD**2
      ENDIF
C
      IF(.NOT.CONV) GO TO 590
C
C---------------------------------------------------------------------
C--- Converged CR iteration exists
C--- Process slipstream velocities
C
 445  IF(LFWD) THEN
         UWT = UWTA
         VWT = VWTA
         CALL PUTVEL
         NADDA = NADD
         DO I=1,NADD
            RADDA(I) = RADD(I)
            UADDA(I) = UADD(I)
            VADDA(I) = VADD(I)
            UADDRA(I)= UADDR(I)
            VADDRA(I)= VADDR(I)
         ENDDO
      ELSE
         UWT = UWTF
         VWT = VWTF
         CALL PUTVEL
         NADDF = NADD
         DO I=1,NADD
            RADDF(I) = RADD(I)
            UADDF(I) = UADD(I)
            VADDF(I) = VADD(I)
            UADDRF(I)= UADDR(I)
            VADDRF(I)= VADDR(I)
         ENDDO
      ENDIF
C
C--- Check slipstream convergence--------------------------------------
C
 460  IF(LFWD) THEN
         TDIFF = ABS((TTOT-TTOTF)/TTOT)
         TTOTF = TTOT
      ELSE
         TDIFF = ABS((TTOT-TTOTA)/TTOT)
         TTOTA = TTOT
      END IF
C 
      IF(TDIFF .LE. CRCON) THEN
         IF (LFWD) THEN
            LCONF = .TRUE.
         ELSE
            LCONA = .TRUE.
         END IF
      END IF
C
      IF(LCONF .AND. LCONA) THEN
        IF(LPITCH.AND.LSRPM.AND..NOT.LSWIT.AND.(COMAND.EQ.'POWE'))THEN
          GO TO 410
        ELSE
          GO TO 500
        ENDIF
      ENDIF
C
      IF(ICRITR .GE. ICRITL) THEN
         WRITE(*,*) 'CR iteration limit exceeded.'
         GO TO 510
      END IF
C
      GO TO 410
C
C--- System is converged----------------------------------------
C
 500  IF (.NOT.LFWD .AND. .NOT.LCON)  GO TO 410
      LCON  = .TRUE.
C
      GO TO 520
C
C--- System is not converged------------------------------------
C      
 510  IF (.NOT.LFWD .AND. .NOT.LNCON) GO TO 410
      LNCON = .TRUE.
C
C--- Output-----------------------------------------------------
C
 520  IF(LFWD) THEN
         IF(LCON) THEN
            WRITE(*,2600) CRNAME
         ELSE
            WRITE(*,2800) CRNAME
         ENDIF
      ELSE
         IF(LCON) THEN
            WRITE(*,3000) CRNAME
         ELSE
            WRITE(*,3200) CRNAME
         ENDIF
      ENDIF
C
      CALL OUTPUT(LUWRIT)
C
      IF(LFWD) THEN
        WRITE(*,3500)
        CALL CRDATA(1)
        GO TO 410
      ENDIF
C
      CALL CRDATA(2)
      TCRTOT = TCR(1) + TCR(2)
      PCRTOT = PCR(1) + PCR(2)
      ECRTOT = (ECR(1)*PCR(1)+ECR(2)*PCR(2))/PCRTOT
      TCRLB  = TCRTOT/4.4482216
      PCRHP  = PCRTOT/745.69987
      VKNOT  = VEL*1.943844
C
      WRITE(*,2500) TCRTOT, PCRTOT, ECRTOT, TCRLB, PCRHP, VKNOT
C
C      CALL DISPIN(LUWRIT)
C
C---- update default input
C
      IF(LSWIT .OR. (COMAND.EQ.'RPM ')) THEN
        POWINF = POWFWD
        POWINA = POWAFT
      ENDIF
C
      IF(LPITCH .AND..NOT.LSRPM.AND.(COMAND.EQ.'POWE'))THEN
        RPMINF = RPMFWD
        RPMINA = RPMAFT
      ENDIF
C
      POWERF = POWINF
      POWERA = POWINA
      RPMF   = RPMINF
      RPMA   = RPMINA
C
C---  store converged slipstream input values
C
      RCONF = RPMINF
      RCONA = RPMINA
      PCONF = POWINF
      PCONA = POWINA
C
C--- reporting
C
      IF(COMAND.EQ.'RPM ') THEN
        WRITE(*,*) 'Specified rpm, fixed pitch'
        IF(RPMIN.NE.0.0) THEN
           IF(.NOT.LSRPM .OR. RRAT.EQ.1.0) THEN
              WRITE(*,2300) RPMINF
           ELSE
              WRITE(*,2310) RPMINF
           ENDIF
        ENDIF
      ELSE 
        IF(LPITCH) THEN
          IF(LSRPM) THEN
           WRITE(*,*)'(Near) specified power, fixed pitch, synched rpm'
          ELSE
           WRITE(*,*) 'Specified power, fixed pitch'
          ENDIF
        ELSE
          WRITE(*,*) 'Specified power and rpm, variable pitch'
        ENDIF
C
        IF(POWIN.NE.0.0) WRITE(*,2400) POWIN
        IF(POWIN.EQ.0.0 .AND. LSWIT) WRITE(*,2400) POWINT
C
      ENDIF
C
      IF(LCON) THEN
        WRITE(*,2100) ICRITR
      ELSE
        WRITE(*,2200) ICRITR
      ENDIF
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
C---- restore slipstream at current rotor (not by...)
C
      NADD = NADDA
      DO I=1,NADD
         RADD(I)  = RADDA(I)
         UADD(I)  = UADDA(I)
         VADD(I)  = VADDA(I)
         UADDR(I) = UADDRA(I)
         VADDR(I) = VADDRA(I)
      ENDDO
C
      CALL CRPLIN
C
      GO TO 900
C
C
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C----- convergence failed: return blade angles and display output
C
 590  IF(COMAND .EQ. 'POWE') THEN
        DO I=1, II
          BETA(I)  = BETA(I)  - DBETA
          BETA0(I) = BETA0(I) - DBETA
        ENDDO
      ENDIF
C
      CALL OUTPUT(LUWRIT)
C
      IF(COMAND .EQ. 'POWE') THEN
        WRITE(*,*) 'Initial blade angles restored'
      ENDIF
C
      WRITE(*,*)
      IF(LFWD) THEN
        WRITE(*,4800) ICRITR
      ELSE
        WRITE(*,4900) ICRITR
      ENDIF
C
C---- Convergence prompt-----------------------------------------------
C
 600  CALL ASKC('..CONV^',CONAND,CONARG)
C
      DO I=1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
      ENDDO
      NINPUT = 0
      CALL GETINT(CONARG,IINPUT,NINPUT,ERROR)
      NINPUT = 0
      CALL GETFLT(CONARG,RINPUT,NINPUT,ERROR)
C
C---- convergence commands
C
      IF(CONAND.EQ.'    ') THEN
         IF(COMAND.EQ.'POWE') GO TO 430
         IF(COMAND.EQ.'RPM ') GO TO 440
      ENDIF
C
      IF(CONAND.EQ.'?   ') THEN
        WRITE(*,5000)
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'STOP') THEN
         WRITE(*,*) ' CR iteration aborted'
         LCON = .FALSE.
         GO TO 900
      END IF
C
      IF(CONAND.EQ.'AUTO' .OR. CONAND.EQ.'A   ') THEN
         IF(LPITCH .OR. COMAND.EQ.'RPM ') THEN
            WRITE(*,*) 'AUTO command not available for fixed pitch'
            GO TO 600
         ENDIF
         LAUTO = .TRUE.
         IAUTO = 0
         GO TO 620
      ENDIF
C
      IF(CONAND.EQ.'ASET') THEN
        CALL ASKI('Max number of convergence attempts^',NAUTO)
        CALL ASKR('Max deviation from velocity vector (deg)^',CRANGE)
 610    WRITE(*,*)
        WRITE(*,*) 'root 0 <---------> 1 tip'
        CALL ASKR( 'Sample blade location^',CLOC)
        IF(CLOC.LT.0.0 .OR. CLOC .GT. 1.0) GO TO 610
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'VRTX') THEN
        VRTX = .NOT.VRTX
        IF(.NOT.VRTX) WRITE(*,*)'Discrete Vortex Formulation deselected'
        IF(VRTX)      WRITE(*,*)'Discrete Vortex Formulation selected'
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'FORM') THEN
        FAST = .NOT.FAST
        IF(FAST)      WRITE(*,*)'Graded Momentum Formulation selected'
        IF(.NOT.FAST) WRITE(*,*)'Potential Formulation selected'
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'WAKE') THEN
        FREE = .NOT.FREE
        IF(FREE)      WRITE(*,*)'Self-deforming wake selected'
        IF(.NOT.FREE) WRITE(*,*)'Rigid wake selected'
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'INIT') THEN
        LOPRINI = .NOT.LOPRINI
        IF(LOPRINI) THEN
          WRITE(*,*) 'Analysis case will be initialized'
        ELSE
          WRITE(*,*) 'Analysis case will not be initialized'
        ENDIF
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'REIN') THEN
        CALL REINIT
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'ANGL') THEN
        IF(NINPUT.GE.1) THEN
          DELB = RINPUT(1)
        ELSE      
          CALL ASKR('Enter blade angle change (deg)^',DELB)
        ENDIF
C
        DO I=1, II
           BETA(I)  = BETA(I)  + DELB*PI/180.
           BETA0(I) = BETA0(I) + DELB*PI/180.
        ENDDO
C
        GO TO 600
      ENDIF
C
      WRITE(*,1050) CONAND  !  command not recognized
      GO TO 600
C
C
C---- Auto-converge loop-------------------------------------------
C
  620 IAUTO = IAUTO + 1
C
      IF(IAUTO .GT. NAUTO) THEN
         LAUTO = .FALSE.
         WRITE(*,*) 'AUTO command failed'
         GO TO 600
      ENDIF
C
      CALL VVECT(IAUTO)
      GO TO 430
C
C
C---------------------------------------------------------------------
c---------------------------------------------------------------------
c
c
 1100 FORMAT(  
     &  /'   INPU    Input default CR specifications'
C
     & //'   ROTF f  Change forward rotor'
     &  /'   ROTA f  Change aft rotor'
     &  /'   POWF r  Change power - forward rotor'
     &  /'   POWA r  Change power - aft rotor'
     &  /'   RPMF r  Change rpm   - forward rotor'
     &  /'   RPMA r  Change rpm   - aft rotor'
     &  /'   VELO r  Change flight speed'
     &  /'   VELW    Change slipstream velocity weights'  
C    
     & //'   POWE r  Run CR iteration (spec. power, fixed pitch/rpm)'
     &  /'   RPM  r  Run CR iteration (specified rpm, fixed pitch)'
     &  /'   FWD  r  Run forward rotor in current slipstream'
     &  /'   AFT  r  Run aft rotor in current slipstream'
     &  /'   DESC    Design MIL rotor to current input and slipstream'
     &  /'   BLEN r  Interpolate between loaded rotor geometries'

     & //'   ANGL r  Change blade angles - current rotor'   
     &  /'   PRPM    Fixed pitch/rpm toggle'   
     &  /'   SYNC    Synchronize rpms toggle'   
     &  /'   NAME s  Set or change CR case name'
     &  /'   CLCR r  Change lift coefficients for MIL design'   
     &  /'   ATMO r  Set fluid properties from standard atmosphere'
c
     & //'   CRIT i  Change CR iteration limit'
     &  /'   CCON r  Change CR convergence delta (del_thrust/thrust)'
     &  /'   FORM    Toggle between Graded Mom.and Potential Form'
     &  /'   VRTX    Toggle between Graded Mom.and Vortex Form'
     &  /'   WAKE    Toggle between rigid and self-deforming wake'
c
     & //'   DISI    Display current CR Default Input and status'
     &  /'   DISS    Display current CR external slipstreams'
     &  /'   DISV    Display current CR average induced velocities'
     &  /'   DISP    Display current rotor operating state'
     &  /'   WRIT f  Write current rotor operating state to disk file'
     &  /'   WRIV f  Write current induced velocities to disk file'
C
     & //'   INIT    Initialize next analysis case'
     &  /'   VCLR    Initialize slipstream velocity profiles'
     &  /'   REIN    Re-initialize rotor to known operating state'
     &  /'   TERS    Toggle between terse and verbose output'
c
     & //'   PLOT i  Plot various rotor parameters'
     &  /'   ANNO    Annotate plot'
     &  /'   HARD    Hardcopy current plot'
     &  /'   SIZE r  Change plot-object size')
c
 1000 FORMAT(A)
 1050 FORMAT(1X,A4,' command not recognized.' //
     &             '  Type "?" for list, <Return> to exit menu.')
C
 2000 FORMAT(/'  0   CANCEL'
     &       /'  1   Geometry'
     &       /'  2   Radial distributions for current case'
     &       /'  3   Radial distributions plus geometry'
     &       /'  4   Radial distributions for all cases'
     &       /'  5   Case sequence parameters'
     &       /'  6   Induced velocities  vs  r/R'
     &       /'  7   Velocity triangles'
     &       /'  8   External slipstream velocity profiles'
     &       /'  9   Reference x,y data'
     &       /' 10   Plot blade data vs r/R'
     &       /' 11   Rotor axial view'
     &       /' 12   Plot CR System induced velocities')
C
 2100 FORMAT(1X, 'Converged after',I3,' iterations')
C
 2200 FORMAT(1X, 'Not converged after',I3,' iterations')
C
 2300 FORMAT(1X, 'Specified rpm - both rotors: ',F6.0)
C
 2310 FORMAT(1X, 'Specified rpm - forward rotor: ',F6.0)
C
 2320 FORMAT(1X, 'Specified rpm - aft rotor: ',F6.0)
C
 2400 FORMAT(1X, 'Specified total power (W): ',F7.0)
C
 2500 FORMAT(/1X,75('-')
     & /' System Thrust N: ',G9.3, 6X,'Power input W: ',G9.3,
     & 4X,'  Effy: ',F6.4,
     & /'             lbf:  ',F8.2,6X,'           hp:  ',F8.2,
     & 4X,' V(kn): ',F6.2,
     & /1X,75('-'))
C
C
 2600 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor at converged CR state: ',A32)
C
 2700 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor off converged CR state: ',A32)
CC
 2800 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor - CR system not converged: ',A32)
C
 2900 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor not converged: ',A32)
C
C
 3000 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor at converged CR state: ',A32)
C
 3100 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor off converged CR state: ',A32)
C
 3200 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor - CR system not converged: ',A32)

 3300 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor not converged: ',A32)
C
 3400 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor: ',A32)
C
 3420 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor: ',A32)
C
 3500 FORMAT(/1X,75('-'))
C
C
 3600 FORMAT(/'        Slipstream Velocity Profiles (m/s)'
     &       /' -------------------------------------------------'
     &       /'                 At FWD Rotor'
     &       /'      r (m)          Vaxi            Vrot')
C
 3700 FORMAT(/' -------------------------------------------------'
     &       /'                 At AFT Rotor'
     &       /'      r (m)          Vaxi            Vrot')
C
 3800 FORMAT( F12.4,4X,F12.4,4X,F12.4)
C
 3900 FORMAT(/'                Slipstream Velocity Profiles (m/s)'
     &       /1X,63('-'),
     &       /'                     At FWD Rotor',
     &        '               At AFT Rotor'
     &       /'   r(m)            Vaxi        Vrot',
     &        '           Vaxi        Vrot')

 4000 FORMAT(1X, F8.4,7X,F8.4,4x,F8.4,7X,F8.4,4X,F8.4)
C
 4100 FORMAT(1X, 63('-'))
C
 4200 FORMAT(1X, 48('-'))
C
 4300 FORMAT(1X, 'FWD Rotor loaded: ',A32)
C
 4400 FORMAT(1X, 'AFT Rotor loaded: ',A32)
C
 4500 FORMAT(/1X,'Current design CLs:',F5.2,' (root) -',
     &           F5.2,' (tip)')
C
 4600 FORMAT(1X, 'Name: ',A32)
C
 4800 FORMAT(/ ' FWD Rotor, CR Iteration # ', I2
     &       / ' Subroutine APER convergence failed')    
C   
 4900 FORMAT(/ ' AFT Rotor, CR Iteration # ', I2
     &       / ' Subroutine APER convergence failed')    
C
 5000 FORMAT(
     &  /' <Return>  Continue'
     &  /'   AUTO<a> Auto-initialize blade angles and continue'
     &  /'   ANGL r  Change blade angles manually'   
     &  /'   FORM    Toggle between Graded Mom.and Potential Form'
     &  /'   VRTX    Toggle between Graded Mom.and Vortex Form'
     &  /'   WAKE    Toggle between rigid and self-deforming wake'
     &  /'   INIT    Initialize next analysis case'
     &  /'   REIN    Re-initialize rotor to known operating state'
     &  /'   ASET    Change AUTO settings'
     &  /'   STOP    Abort and return to CROTOR prompt')
C
      END ! CROTOR
C
C------------------------------------------------------------------------
C------------------------------------------------------------------------


      SUBROUTINE PUTVEL
      INCLUDE 'XROTOR.INC'
C--------------------------------------------------
C     Automates SAVVEL and GETVEL
C     Applies weights to slipstream velocities
C--------------------------------------------------
C
      NADD=II
C
      BLDS = FLOAT(NBLDS)
      DO 10 I=1, II
C------ use circumferentially averaged induced velocity 
        VT = BLDS*GAM(I)/(4.0*PI*XI(I))
        VA = VT*XI(I)/ADW
C
C------ include duct effect on freestream and induced axial velocity
        UDUCT     = 0.0
        VADUCT_VA = 1.0
        IF(DUCT) THEN
          UDUCT = URDUCT-1.0
          VADUCT_VA = 2.0*URDUCT
        ENDIF
C
        VA = VA * VADUCT_VA
        UTOT = 1.0 + UDUCT + UBODY(I)
        CALL UVADD(XI(I),WA,WT)
C
        CI = XI(I)/ADV - WT  -  VT
        SI = UTOT      + WA  +  VA
C
        RDIM = XI(I)*RAD
        UDIM =  2.0*(SI - (UTOT      + WA))*VEL
        VDIM = -2.0*(CI - (XI(I)/ADV - WT))*VEL
C
c        WRITE(LU,*) RDIM, UDIM, VDIM
C
       W1(I)=RDIM
       W2(I)=UDIM
       W3(I)=VDIM
C
 10   CONTINUE
C
C
C
      DO 20 I=1, NADD
        RADD(I) = W1(I)
        UADD(I) = W2(I)
        VADD(I) = W3(I)
 20   CONTINUE
C
C
      DO 30 I=1, NADD
        UADD(I) = UWT*UADD(I)
        VADD(I) = VWT*VADD(I)
 30   CONTINUE
C
      CALL SPLINE(UADD,UADDR,RADD,NADD)
      CALL SPLINE(VADD,VADDR,RADD,NADD)
C
C      WRITE(*,1200)
C      WRITE(*,1250) (RADD(I), UADD(I), VADD(I), I=1, NADD)
c
      RETURN
      END
      
C---------------------------------------------------------------------


      SUBROUTINE DISPIN(LU)
      INCLUDE 'XROTOR.INC'
C
      WRITE(LU,1200) CRNAME,NAMECR(1),NAMECR(2),FNAMEF,FNAMEA,
     &               POWERF,POWERA
C
      IF(LSRPM) THEN
        WRITE(LU,1220) RRAT,RPMF,RPMA
      ELSE
        WRITE(LU,1210) RPMF,RPMA
      ENDIF
C
      WRITE(LU,1250) UWTF,UWTA,VWTF,VWTA
C
      IF(LCON) THEN
         WRITE(LU,1300) ALT, VEL
      ELSE
         WRITE(LU,1400) ALT, VEL
      ENDIF
C
      RETURN
C
 1200 FORMAT(
     & /' ------------------------------------------------------------'
     & /'                   CR SYSTEM DEFAULT INPUT'
     & /1X,A25,               'Fwd Rotor           Aft Rotor'
     & /' ------------------------------------------------------------'
     & /' Rotor names              ',A20,A20
     & /' Filenames                ',A20,A20
     & /' Power(W)                 ',F8.1,12X,F8.1)
C
 1210 FORMAT(
     &  ' Rpm (no ratio)             ',F6.1,14X,F6.1)
C
 1220 FORMAT(
     &  ' Rpm (ratio',F6.3,')          ',F6.1,14X,F6.1)
C
 1250 FORMAT(
     &  ' Axial Vel. Wt.               ',F4.1,16X,F4.1
     & /' Tang. Vel. Wt.               ',F4.1,16X,F4.1
     & /' ------------------------------------------------------------')
C
 1300 FORMAT(
     &   ' Slipstream converged         Alt(km):',F5.2,
     &   '    V(m/s): ',F6.2)
C
 1400 FORMAT(
     &   ' Slipstream not converged     Alt(km):',F5.2,
     &   '    V(m/s): ',F6.2)
C
      END  ! DISPIN

C---------------------------------------------------------------------

      SUBROUTINE CRINIT
      INCLUDE 'XROTOR.INC'
C----------------------------------------------------------
C     CROTOR Initialization + ESPARA
C----------------------------------------------------------
C
      LCRIN  = .FALSE.  ! default input has not been entred
      LPITCH = .FALSE.  ! pitch is not fixed
      LSRPM  = .TRUE.   ! rpms are synchronized 
C
      FNAMEF = 'none'
      FNAMEA = 'none'
      CRNAME = 'Unnamed CR System'
C
C     Standard CR velocity weights
      UWTF = 0.5
      UWTA = 0.5
      VWTF = 0.0
      VWTA = -1.0
C
      ICRITL = 20     ! CR iteration limit
      CRCON  = 0.001  ! convergence flag delta (thrust)
C
      CRCLR  = 0.6     ! design CL - root
      CRCLT  = 0.6     ! design CL - tip
      BLENDF = 0.5     ! rotor interpolation factor
      RRAT   = 1.0     ! rpm ratio (rpms synchronized)
C
      NAUTO  = 5       ! auto converge - max attempts
      CRANGE = 10.0    ! auto converge - max deviation (deg)
      CLOC   = 0.7     ! auto converge - VV blade location
C
      PLOC   = 0.5     ! ESPARA beta data blade location
C
      NADDF = 0        ! no slipstream at front rotor
      NADDA = 0        ! no slipstream at aft rotor
C
      RETURN
      END   ! CRINIT




C---------------------------------------------------------------------

      SUBROUTINE OUTVEL(LU)
      INCLUDE 'XROTOR.INC'
C---------------------------------------------------
C     Output induced velocities
C---------------------------------------------------
      REAL Z1(II),Z2(II),Z3(II),Z4(II),Z5(II),Z6(II)
C
C---- extract velocity data
C
      DO I=1,II
        Z1(I) =  CVEL3(1,I)   !  FWD rotor axial
        Z2(I) =  CVEL4(1,I)   !  FWD rotor tangential
        Z3(I) =  CVEL3(2,I)   !  AFT rotor axial
        Z4(I) = -CVEL4(2,I)   !  AFT rotor tangential
        Z5(I) =  Z1(I)+Z3(I)  !  Total axial
        Z6(I) =  Z2(I)+Z4(I)  !  Total tangential
      ENDDO
C
      IF(RADCR(1).EQ.RADCR(2) .AND. XI0CR(1).EQ.XI0CR(2) .AND.
     &         IICR(1).EQ.IICR(2)) THEN
C
        WRITE(LU,1500)
        DO I=1,II
           RVEL = RAD*XI(I)
           WRITE(LU,1600) RVEL, Z1(I), Z2(I), Z3(I), Z4(I),
     &                          Z5(I), Z6(I)
        ENDDO
        WRITE(*,*)
C        WRITE(LU,1800)
      ELSE
         WRITE(LU,1000)
         J = IICR(1)
         DO I=1, J
            RVEL = RADCR(1)*XICR(1,I)
            WRITE(LU,1400) RVEL, Z1(I), Z2(I)
         ENDDO
C
         WRITE(LU,1200)
         J = IICR(2)
         DO I=1, J
            RVEL = RADCR(2)*XICR(2,I)
            WRITE(LU,1400) RVEL, Z3(I), Z4(I)
         ENDDO
         WRITE(*,*)
C         WRITE(LU,1700)
      ENDIF
C    
      RETURN
C
C
C
 1000 FORMAT(/'          Induced Velocity Profiles (m/s)'
     &       /' ------------------------------------------------'
     &       /'                  At FWD Rotor'
     &       /'      r (m)           Vaxi          Vrot')
C
 1200 FORMAT(/' ------------------------------------------------'
     &       /'                  At AFT Rotor'
     &       /'      r (m)           Vaxi          Vrot')

 1400 FORMAT(F12.4,4X,F12.4,4X,F12.4)
C
C
 1500 FORMAT(/ 19X, 'Average Induced Velocity Profiles (m/s)'
     &       /1X,74('-'),
     &       /'                From FWD Rotor       From AFT Rotor',
     &        '            Totals'
     &       /'   r(m)         Vaxi      Vrot        Vaxi',
     &        '      Vrot        Vaxi      Vrot')

 1600 FORMAT(1X,F8.4,4X,F8.4,2x,F8.4,4X,F8.4,2X,F8.4,4X,F8.4,2X,F8.4)
C
 1700 FORMAT(1X, 48('-'))
C
 1800 FORMAT(1X, 74('-'))
C
C
      END   !  OUTVEL
C


C---------------------------------------------------------------------

      SUBROUTINE LOADF
      INCLUDE 'XROTOR.INC'
C---------------------------------------------------
C     Load forward rotor
C---------------------------------------------------
C
      IF(FNAMEF(1:1).EQ.' ')
     &   CALL ASKS('Enter FWD rotor filename^',FNAMEF)
      IF (FNAMEF.EQ.'S' .OR. FNAMEF.EQ.'s') FNAMEF = FNAMEA
C
      CALL LOAD(FNAMEF)
      VEL = VELCR
C
      IF(LFILER) THEN
         WRITE(*,*) 'FWD rotor not loaded'
         RETURN
      ENDIF
C
      NADDF = NADD
      DO 100 I=1,NADD
         RADDF(I)  = RADD(I)
         UADDF(I)  = UADD(I)
         VADDF(I)  = VADD(I)
         UADDRF(I) = UADDR(I)
         VADDRF(I) = VADDR(I)
 100  CONTINUE
C
      LFWD  = .TRUE.
      CALL STORCR(1)
      ZBETA(1) = BETA(1)
      LCON = .FALSE.
C
      RETURN
      END
C
C---------------------------------------------------------------------

      SUBROUTINE LOADA
      INCLUDE 'XROTOR.INC'
C---------------------------------------------------
C     Load aft rotor
C---------------------------------------------------
C
      IF(FNAMEA(1:1).EQ.' ')
     &   CALL ASKS('Enter AFT rotor filename^',FNAMEA)
      IF (FNAMEA.EQ.'S' .OR. FNAMEA.EQ.'s') FNAMEA = FNAMEF
C
      CALL LOAD(FNAMEA)
      VEL = VELCR
C
      IF(LFILER) THEN
         WRITE(*,*) 'AFT rotor not loaded'
         RETURN
      ENDIF
C
      NADDA = NADD
      DO 100 I=1,NADD
         RADDA(I)  = RADD(I)
         UADDA(I)  = UADD(I)
         VADDA(I)  = VADD(I)
         UADDRA(I) = UADDR(I)
         VADDRA(I) = VADDR(I)
 100  CONTINUE
C
      LFWD  = .FALSE.
      CALL STORCR(2)
      ZBETA(2) = BETA(1)
      LCON = .FALSE.
C
      RETURN
      END
C
C-------------------------------------------------------------------------


      SUBROUTINE STORCR(ICR)
      INCLUDE 'XROTOR.INC'
C--------------------------------------------------
C     Stores blade geometry for future CR iteration
C--------------------------------------------------
C
      J = ICR
C
      RADCR(J)  = RAD
      RAKECR(J) = RAKE
      XI0CR(J)  = XI0
      XW0CR(J)  = XW0
      IICR(J)   = II
      NBLDCR(J) = NBLDS
      NAMECR(J) = NAME
C
      DO I=1, II
        XICR(J,I)    = XI(I)
        CHCR(J,I)    = CH(I)
        BETACR(J,I)  = BETA(I)
        BETA0CR(J,I) = BETA0(I)
        UBODYCR(J,I) = UBODY(I)
      END DO
C
      RETURN
      END
C
C-------------------------------------------------------------------------



      SUBROUTINE LOADCR(ICR)
      INCLUDE 'XROTOR.INC'
C---------------------------------------------------
C     Loads geometry and slipstream for CR iteration
C---------------------------------------------------
C
      J = ICR
C
      IF(J .EQ. 1) THEN
         K = 2
      ELSE
         K = 1
      ENDIF
C
C--- Store blade angle of outgoing rotor
C
      ZBETA(K) = BETA(1)
C
C--- Read in new geometry
C      
      RAD   = RADCR(J)
      RAKE  = RAKECR(J)
      XI0   = XI0CR(J)
      XW0   = XW0CR(J)
      II    = IICR(J)
      NBLDS = NBLDCR(J)
      NAME  = NAMECR(J)
C
      DO I=1, II
        XI(I)    = XICR(J,I)
        CH(I)    = CHCR(J,I)
        BETA(I)  = BETACR(J,I)
        BETA0(I) = BETA0CR(J,I)
        UBODY(I) = UBODYCR(J,I)
      END DO
C
C--- Restore blade angle
C
      DELBET = ZBETA(J) - BETA(1)
C
      DO I=1,II
         BETA(I)  = BETA(I)  + DELBET
         BETA0(I) = BETA0(I) + DELBET
      ENDDO
C
      IF(J .EQ. 1) THEN
         LFWD = .TRUE.
         NADD = NADDF
         DO I=1,NADD
           RADD(I)  = RADDF(I)
           UADD(I)  = UADDF(I)
           VADD(I)  = VADDF(I)
           UADDR(I) = UADDRF(I)
           VADDR(I) = VADDRF(I)
         ENDDO
      ELSE
         LFWD = .FALSE.
         NADD = NADDA
         DO I=1,NADD
           RADD(I)  = RADDA(I)
           UADD(I)  = UADDA(I)
           VADD(I)  = VADDA(I)
           UADDR(I) = UADDRA(I)
           VADDR(I) = VADDRA(I)
         ENDDO
      ENDIF
C
C      CALL XWINIT
C
      IINF = II + II/2
C
      CALL SETIAERO

      RETURN    
      END
C
C------------------------------------------------------------------------


      SUBROUTINE CRDATA(ICR)
      INCLUDE 'XROTOR.INC'
C----------------------------------------------------
C     Stores output data from converged system
C----------------------------------------------------
C
      J = ICR
C
C----Calculate and store induced velocities
      BLDS = FLOAT(NBLDS)
C
      DO I = 1, II
C---- induced velocity on blade 
        VT = VIND(3,I)
        VA = VIND(1,I)
C------ include duct effect on freestream and induced axial velocity
        UDUCT     = 0.0
        VADUCT_VA = 1.0
        IF(DUCT) THEN
          UDUCT = URDUCT-1.0
          VADUCT_VA = 2.0*URDUCT
        ENDIF
        VA = VA * VADUCT_VA
        W1(I) = VA
        W2(I) = VT
C---- velocity from circulation
        VT = 0.5*BLDS*GAM(I) / (2.0*PI*XI(I))
        VA = 0.5*BLDS*GAM(I) / (2.0*PI*ADW  )
C------ include duct effect on axial induced velocity
        VA = VA * VADUCT_VA
        W3(I) = VA
        W4(I) = VT
C---- dimensional induced velocities
        W1(I) = W1(I)*VEL
        W2(I) = W2(I)*VEL
        W3(I) = W3(I)*VEL
        W4(I) = W4(I)*VEL
C
        CVEL1(J,I) = W1(I)
        CVEL2(J,I) = W2(I)
        CVEL3(J,I) = W3(I)
        CVEL4(J,I) = W4(I)
C
      ENDDO
C
C---- thrust, power and efficiency
C
      TCR(J) = TTOT*RHO*VEL**2*RAD**2
      ECR(J) = TTOT/PTOT
      PCR(J) = PTOT*RHO*VEL**3*RAD**2
C
      RETURN
      END
C
C----------------------------------------------------------------


      SUBROUTINE CRPLIN
      INCLUDE 'XROTOR.INC'
C--------------------------------------
C     Plots CR induced velocities vs r/R
C     Based on UVIPLT
C--------------------------------------
      REAL Z1(II),Z2(II),Z3(II),Z4(II),Z5(II),Z6(II)
C
      EXTERNAL PLCHAR,PLMATH
      DATA LMASK1, LMASK2, LMASK3 / -32640, -30584, -21846 /
C
C---- limitations
C
      IF(RADCR(1).NE.RADCR(2) .OR. XI0CR(1).NE.XI0CR(2) .OR.
     &         IICR(1).NE.IICR(2)) THEN
         WRITE(*,*) 
     &  'Plotting requires identical radii and stations'
         RETURN
      ENDIF
C
C---- plot aspect ratio
      PLPAR = 1.36
C
C---- character size for axis numbers, labels
      CS  = CSIZE
      CSL = CSIZE*1.4
C
      CSCALE = 1.8*CS   !  axial/tang. text
      CSLAB  = 1.1*CS   !  Label text
C
C---- find radial stations closest to 1/2 and 3/4 radius for labels
C
      XIB = 0.75
      XIA = 0.50
      XIBX = 1.0
      XIAX = 1.0
C
      DO I = 1, II
        IF(ABS(XI(I)-XIA).LT.XIAX) THEN
          XIAX = ABS(XI(I)-XIA)
          IA = I
        ENDIF
        IF(ABS(XI(I)-XIB).LT.XIBX) THEN
          XIBX = ABS(XI(I)-XIB)
          IB = I
        ENDIF
      ENDDO
C
C---- extract velocity data
C
      DO I=1,II
        Z1(I) =  CVEL3(1,I)   !  FWD rotor axial
        Z2(I) =  CVEL4(1,I)   !  FWD rotor tangential
        Z3(I) =  CVEL3(2,I)   !  AFT rotor axial
        Z4(I) = -CVEL4(2,I)   !  AFT rotor tangential
        Z5(I) =  Z1(I)+Z3(I)  !  Total axial
        Z6(I) =  Z2(I)+Z4(I)  !  Total tangential
      ENDDO
C
C
      WMIN = 0.
      WMAX = 0.
        DO I = 3, II-II/5
          WMIN =MIN(WMIN,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I))
          WMAX =MAX(WMAX,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I))
        ENDDO
C
      CALL SCALIT(1,WMIN,0.0,WMNFAC)
      CALL SCALIT(1,WMAX,0.0,WMXFAC)
C
      WFAC = MIN( WMNFAC , WMXFAC )
      WDEL = 0.5 / (5.0*WFAC)
C
      IF(WMIN .LT. 0.0) WMIN = -WDEL * AINT( -WMIN/WDEL + 0.99 )
      IF(WMAX .GT. 0.0) WMAX =  WDEL * AINT(  WMAX/WDEL + 0.99 )
C
      WFAC = PLPAR / (WMAX - WMIN)
C
C
      CALL PLTINI(SCRNFR,IPSLU,IDEV,7.0,LPLOT,.FALSE.)
      CALL PLOTABS(0.9,0.7,-3)
C
      CALL GETCOLOR(ICOL0)
C
      CALL PLOT(0.0,-WFAC*WMIN,-3)
C
C---- case title
C
      CALL NEWPEN(4)
      XT = 0.0
      YT = WFAC*WMAX + 2.0*CSL
      CALL PLCHAR(XT,YT,CSL,CRNAME,0.0,-1)
C
      CALL NEWPEN(1)
      CALL PLOT(0.0,0.0,3)
      CALL PLOT(1.0,0.0,2)
C
C---- label axes
C
      CALL NEWPEN(2)
      CALL XAXIS(0.0,WFAC*WMIN, 1.0,0.2    , 0.0,0.2, CS,1)
      CALL NEWPEN(3)
      CALL PLCHAR(0.5-1.5*CSL,WFAC*WMIN-3.0*CSL,CSL,'r/R',0.0,3)
C
      IF(LGRID) THEN
       CALL NEWPEN(1)
       NXG = 5
       NYG = INT( (WMAX-WMIN)/WDEL + 0.0001 )
       CALL PLGRID(0.0,WFAC*WMIN, NXG,0.2, NYG,WFAC*WDEL, LMASK2 )
      ENDIF
C
      CALL NEWPEN(2)
      CALL YAXIS(0.0,WFAC*WMIN,WFAC*(WMAX-WMIN),WFAC*WDEL,
     &            WMIN,WDEL, CS,-2)
      CALL NEWPEN(3)
C
      XL = -5.0*CSL
      YL = WFAC*(WMAX-3.5*WDEL) - 0.5*CSL
      CALL PLCHAR(XL,YL,CSL,'Vind'  ,0.0,4)
      CALL PLCHAR(XL+1.5*CSL,YL-1.2*CSL,0.8*CSL,'m/s'  ,0.0,3)
C
C---- axial/tangential
C
      XL = XI(IA) + 1.5*CS
      YL  = WFAC*(MAX(Z1(IA),Z3(IA)) + Z5(IA))/2.0 - 0.5*CS
C
      IF(Z5(IA) .LT. 0.35*Z5(II)) THEN
         YL = WFAC*(Z5(IA)+0.35*(Z5(II)-Z5(IA)))
      ENDIF
C
      IF(WIND) YL = -YL
C
      CALL PLCHAR(XL,YL+0.6*CS,CSCALE,'v'  ,0.0,1)
      CALL PLSUBS(XL,YL+0.6*CS,CSCALE,'axi',0.0,3,PLCHAR)
C
      XL = XI(IB) + 1.5*CS
      IF(WIND) THEN
        YL = -WFAC*Z2(IB) + 3.0*CS
      ELSE
        YL =  WFAC*Z2(IB) + 3.0*CS
      ENDIF
      CALL PLCHAR(XL,YL+0.6*CS,CSCALE,'v'  ,0.0,1)
      CALL PLSUBS(XL,YL+0.6*CS,CSCALE,'tan',0.0,3,PLCHAR)
C     
C---- Forward Rotor
C
      CALL NEWCOLORNAME('cyan')
C
      CALL NEWPEN(4)
      IF(WIND) THEN
        CALL XYLINE(II,XI,Z1,0.0,1.0,0.0,-WFAC,1)
        CALL XYLINE(II,XI,Z2,0.0,1.0,0.0,-WFAC,1)
      ELSE
        CALL XYLINE(II,XI,Z1,0.0,1.0,0.0, WFAC,1)
        CALL XYLINE(II,XI,Z2,0.0,1.0,0.0, WFAC,1)
      ENDIF
C
C---- Aft Rotor
C
      CALL NEWCOLORNAME('yellow')
C     
      CALL NEWPEN(4)
      IF(WIND) THEN
       CALL XYLINE(II,XI,Z3,0.0,1.0,0.0,-WFAC,1)
       CALL XYLINE(II,XI,Z4,0.0,1.0,0.0,-WFAC,1)
      ELSE
       CALL XYLINE(II,XI,Z3,0.0,1.0,0.0, WFAC,1)
       CALL XYLINE(II,XI,Z4,0.0,1.0,0.0, WFAC,1)
      ENDIF
C
C---- Totals
C
      CALL NEWCOLORNAME('red')
C     
      CALL NEWPEN(4)
      IF(WIND) THEN
       CALL XYLINE(II,XI,Z5,0.0,1.0,0.0,-WFAC,1)
       CALL XYLINE(II,XI,Z6,0.0,1.0,0.0,-WFAC,1)
      ELSE
       CALL XYLINE(II,XI,Z5,0.0,1.0,0.0, WFAC,1)
       CALL XYLINE(II,XI,Z6,0.0,1.0,0.0, WFAC,1)
      ENDIF
C
C ---- legend
C
      XYOFF(1) = 0.
      XYOFF(2) = 0.
      XYFAC(1) = 1.0
      IF(WIND) THEN
       XYFAC(2) = -WFAC
      ELSE
       XYFAC(2) =  WFAC
      ENDIF     
C
      W8(1) =  0.15    
      W8(2) =  W8(1)  + 2.5*CSL
      W9(1) =  0.5*CSL
      W9(2) =  0.5*CSL
C
      XL = 0.04       
      YL = WFAC*WMAX - 2.8*CSL
C
      IF(WFAC*Z5(1).GT.0.78) THEN
         YL=WFAC*(Z5(1)+(MAX(Z1(1),Z3(1))))/2 + 1.5*CSL
      ENDIF
C
      CALL NEWCOLORNAME('cyan')
      CALL NEWPEN(4)
      CALL XYLINE(2,W8,W9,-XL,1.0,-YL,1.0,1)
      CALL NEWCOLOR(ICOL0)
      CALL NEWPEN(3)
      CALL PLCHAR(XL,YL,CSLAB,'FWD Rotor',0.0,9)
C
      YL = YL - 2.0*CSL
      CALL NEWCOLORNAME('yellow')
      CALL NEWPEN(4)
      CALL XYLINE(2,W8,W9,-XL,1.0,-YL,1.0,1)
      CALL NEWCOLOR(ICOL0)
      CALL NEWPEN(3)
      CALL PLCHAR(XL,YL,CSLAB,'AFT Rotor',0.0,9)
C
      YL = YL - 2.0*CSL
      CALL NEWCOLORNAME('red')
      CALL NEWPEN(4)
      CALL XYLINE(2,W8,W9,-XL,1.0,-YL,1.0,1)
      CALL NEWCOLOR(ICOL0)
      CALL NEWPEN(3)
      CALL PLCHAR(XL,YL,CSLAB,'CR System',0.0,9)
C
      CALL PLFLUSH
C
      RETURN
      END !   CRPLIN
C
C----------------------------------------------------------------



      SUBROUTINE DESCR
      INCLUDE 'XROTOR.INC'
C--------------------------------------
C---- Design MIL prop within CROTOR
C--------------------------------------
C
      CHARACTER*32 DNAME
      CHARACTER*1  CRANS
C
      PLFACD = 0.6
      PLFAC1 = 0.7
      PLFAC2 = 0.85
      XORG = 0.15
      YORG = 0.10
C
      WRITE(*,*)
      WRITE(*,1000) CRCLR,CRCLT
      WRITE(*,*)
      IF(LFWD) THEN
        WRITE(*,*)'Redesigning FWD rotor: ', NAMECR(1)
      ELSE
        WRITE(*,*)'Redesigning AFT rotor: ', NAMECR(2)
      ENDIF
C
      WRITE(*,*)'<a> to abort  <return> to leave name unchanged'
C
      CALL ASKS('Enter new rotor name^',DNAME)
C
      IF(DNAME.EQ.'A' .OR. DNAME.EQ.'a') RETURN
      IF(DNAME(1:1) .NE. ' ') NAME = DNAME
C
      ADVDES = 0.0
      TDDES  = 0.0
C
      RADDES = RAD
      R0DES  = XI0*RAD
      RWDES  = XW0*RAD
      VELDES = VEL
C
      IF(LFWD) THEN
        RPMDES = RPMF
        PDDES  = POWERF
      ELSE
        RPMDES = RPMA
        PDDES  = POWERA
      ENDIF
C
      CLROOT = CRCLR
      CLTIP  = CRCLT
      CALL SETCLD(CLROOT,CLTIP)
      CLDES0 = 0.5*(CLROOT+CLTIP)
C
      LROTOR = .FALSE.
C
      CALL DESGEN
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFACD*SIZE,LPLOT,.NOT.LLAND)
      CALL PLOT(0.175,0.075,-3)
      CALL GEOPLT('AL')
      CALL PLOTABS(0.0,0.0,-3)
      CALL PLOT(0.175,0.875,-3)
      CALL CLPLT
C
      IF(LFWD) THEN
        CALL STORCR(1)
        ZBETA(1) = BETA(1)
        FNAMEF = 'none'
      ELSE
        CALL STORCR(2)
        ZBETA(2) = BETA(1)
        FNAMEA = 'none'
      ENDIF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
        WRITE(*,*) 'Redesigned FWD rotor loaded'
      ELSE
        WRITE(*,*) 'Redesigned AFT rotor loaded'
      ENDIF
C
      RETURN
C
C
 1000 FORMAT(1X,'Design CLs (root - tip) = ',F4.2,' - ',F4.2)
C
      END   ! DESCR
C
C----------------------------------------------------------------



      SUBROUTINE CRBLEND
      INCLUDE 'XROTOR.INC'
C--------------------------------------
C     Blends between loaded rotors
C--------------------------------------
C
      BF = BLENDF
C
C---- limitations
C
      IF(RADCR(1).NE.RADCR(2) .OR. XI0CR(1).NE.XI0CR(2) .OR.
     &         IICR(1).NE.IICR(2)) THEN
         WRITE(*,*) 
     &  'Blending requires identical radii and stations'
         RETURN
      ENDIF
C
      DO I=1,II
       IF(LBC)CH(I)   =CHCR(1,I)   +BF*(CHCR(2,I)   -CHCR(1,I))
       IF(LBA)BETA(I) =BETACR(1,I) +BF*(BETACR(2,I) -BETACR(1,I))
       IF(LBA)BETA0(I)=BETA0CR(1,I)+BF*(BETA0CR(2,I)-BETA0CR(1,I))
      ENDDO
C
      IF(LFWD) THEN
         CALL STORCR(1)
         ZBETA(1) = BETA(1)
         FNAMEF = 'none'
      ELSE
         CALL STORCR(2)
         ZBETA(2) = BETA(1)
         FNAMEA = 'none'
      END IF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
        WRITE(*,*) 'Blended FWD rotor loaded'
      ELSE
        WRITE(*,*) 'Blended AFT rotor loaded'
      ENDIF
C
      RETURN
      END
C
C----------------------------------------------------------------



      SUBROUTINE VVECT(IAUTO)
      INCLUDE 'XROTOR.INC'
C-------------------------------------------------------------
C---- calculates velocity vector at sample blade station and
C---- adjusts blade angle incrementally around velocity vector
C-------------------------------------------------------------
C
C---  Find sample blade station
C
      DO I = 1,II
        BLOC = (XI(I) - XI0)/(1 - XI0)   
        IF(BLOC.GE.CLOC) GO TO 10
      ENDDO 
C
 10   IF(I.GT.II) I=II
C
C---  increments
C
      RAUTO = FLOAT(NAUTO)
      BINC = 2. * CRANGE/(RAUTO - 1.)
C
C---  deviation from velocity vector
C         
      BSIGN = (-1.0) ** IAUTO
C
      IDEL = IAUTO/2
      RDEL = FLOAT(IDEL)
C      
      BDEL = (BSIGN * BINC * RDEL) * PI/180.
C
C---  calculate velocity vector
C
      VT = VIND(3,I)
      VA = VIND(1,I)
C
      UDUCT     = 0.0
      VADUCT_VA = 1.0
      IF(DUCT) THEN
        UDUCT = URDUCT-1.0
        VADUCT_VA = 2.0*URDUCT
      ENDIF
      VA = VA * VADUCT_VA
      UTOT = 1.0 + UDUCT + UBODY(I)
      CALL UVADD(XI(I),WA,WT)
C
      VTANG  = XI(I)/ADV - WT  -  VT
      VAXIAL = UTOT      + WA  +  VA
C
      VV = ATAN(VAXIAL/VTANG)
C
C---  move blades
C
      BMOVE  = VV + BDEL - BETA(I)
C
      DO J=1, II
        BETA(J)  = BETA(J)  + BMOVE
        BETA0(J) = BETA0(J) + BMOVE
      ENDDO
C
      IF(IAUTO.EQ.1) WRITE(*,1000)
C
      BDELDG = BDEL * 180./PI
      VVDG = VV * 180./PI
      BETADG = BETA(I) * 180./PI
C
      WRITE(*,1100) IAUTO, I, BINC, BDELDG, VVDG, BETADG
C
      RETURN
C
 1000 FORMAT(
     &  / ' Iter   Stn     Incr      Dev       VV       Beta')
C
 1100 FORMAT(2X, I2, 4X, I2,4X,F6.2,4X,F6.2,4X,F6.2,4X,F6.2)
C
C
      END
C
C
C-----------------------------------------------------------------


