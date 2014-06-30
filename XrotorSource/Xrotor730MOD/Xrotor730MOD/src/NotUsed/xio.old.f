
      SUBROUTINE SAVE(FNAME1)
      INCLUDE 'XROTOR.INC'
      CHARACTER*(*) FNAME1
C-------------------------------------
C     Save rotor and operating state
C-------------------------------------
      LOGICAL LVDUCT
C
      CHARACTER*1 ANS
C
      GREEK = .FALSE.
      IF(.NOT.CONV) THEN
       WRITE(*,1050)
       RETURN
      ENDIF
C
      LVDUCT = ABS(ADW-ADV*URDUCT) .GE. 5.E-5
C
C
      LU = LUTEMP
C
      FNAME = FNAME1
      IF(FNAME(1:1) .EQ. ' ') CALL ASKS('Enter filename^',FNAME)
C
      OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=5)
      WRITE(*,*)
      WRITE(*,*) 'Output file exists.  Overwrite?  Y'
      READ (*,1000) ANS
      IF(INDEX('Nn',ANS).EQ.0) GO TO 6
C
      CLOSE(LU)
      WRITE(*,*) 'Current rotor not saved.'
      RETURN
C
 5    OPEN(LU,FILE=FNAME,STATUS='NEW',ERR=90)
 6    REWIND(LU)
C
      WRITE(LU,1100) NAME
      WRITE(LU,*) II,NBLDS
      WRITE(LU,*) RHO,VSO,RMU
      WRITE(LU,*) RAD,VEL,ADV
      WRITE(LU,*) XI0,XW0
C
C--- Save only #1 section to stay compatible with old XROTOR format
      N = 1
      CALL GETAERO(N,XISECT,A0,CLMAX,CLMIN,
     &             DCLDA,DCLDA_STALL,DCL_STALL,
     &             CDMIN,CLDMIN,DCDCL2,CMCON,REREF,REXP)
      WRITE(LU,*) A0,DCLDA,CLMAX,CLMIN
      WRITE(LU,*) CDMIN,CLDMIN,DCDCL2
      WRITE(LU,*) REREF,REXP
C
      WRITE(LU,*) LVDUCT, DUCT, WIND
      DO I=1, II
        WRITE(LU,*) XI(I),CH(I),BETA0(I),UBODY(I)
      END DO
C
      WRITE(LU,*) URDUCT
C
      IF(NADD.GT.1) THEN
       DO I=1, NADD
         WRITE(LU,*) RADD(I), UADD(I), VADD(I)
       END DO
       WRITE(*,*) 'External slipstream included in save file'
      ENDIF
C
      CLOSE(LU)
      RETURN
C
 90   WRITE(*,*) 'Bad filename.'
      WRITE(*,*) 'Current rotor not saved.'
      RETURN
C
C...................................................................
 1000 FORMAT(A)
 1050 FORMAT(/' *** Converged operating solution does not exist ***')
 1100 FORMAT(A)
      END ! SAVE



      SUBROUTINE LOAD(FNAME1)
      INCLUDE 'XROTOR.INC'
      CHARACTER*(*) FNAME1
      CHARACTER*128 LINE
C
C--------------------------------------
C     Reads in previously saved rotor
C--------------------------------------
      GREEK = .FALSE.
C
      LU = LUTEMP
C
      FNAME = FNAME1
      IF(FNAME(1:1) .EQ. ' ') CALL ASKS('Enter filename^',FNAME)
C
      OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=200)
      READ(LU,1000,ERR=210) NAME
      READ(LU,*,ERR=210) IIX,NBLDS
      READ(LU,*,ERR=210) RHO,VSO,RMU
      READ(LU,*,ERR=210) RAD,VEL,ADV
      READ(LU,*,ERR=210) XI0,XW0
      READ(LU,*,ERR=210) A0,DCLDA,CLMAX,CLMIN
      READ(LU,*,ERR=210) CDMIN,CLDMIN,DCDCL2
      READ(LU,*,ERR=210) REREF,REXP
      READ(LU,*,ERR=210) FREE, DUCT, WIND
      WRITE(*,*)
      IF(     FREE) WRITE(*,*) 'Self-deforming wake option set'
      IF(.NOT.FREE) WRITE(*,*) 'Rigid wake option set'  
      IF(     DUCT) WRITE(*,*) 'Duct option set'
      IF(.NOT.DUCT) WRITE(*,*) 'Free-tip option set'
      IF(     WIND) WRITE(*,*) 'Windmill plotting mode set'
      IF(.NOT.WIND) WRITE(*,*) 'Propeller plotting mode set'
      WRITE(*,*) ' '
      DO I=1, IIX
        READ(LU,*,ERR=210) XI(I),CH(I),BETA(I),UBODY(I)
        BETA0(I) = BETA(I)
cc        write(*,*) 'load i,ch,beta ',i,ch(i),beta(i)
      END DO
C
      URDUCT = 1.0
      READ(LU,*,END=19) URDUCT
C
C---- try to read slipstream data
 19   DO I=1, IX
        READ(LU,*,END=21) RADD(I), UADD(I), VADD(I)
      END DO
      I = IX+1
 21   CONTINUE
      IF(I.GT.2) THEN
       NADD = I-1
       WRITE(*,*)
       WRITE(*,*) 'Slipstream profiles read in'
      ENDIF
      CLOSE(LU)
C
      ALT = 999.0
      CONV = .FALSE.
C
C--- Check for number of analysis stations to use
      IF(IIX.NE.II) THEN
 22     WRITE(*,23) IIX,II,II
        READ(*,24) LINE
        IF(LINE.NE.' ') THEN
          READ(LINE,*,ERR=22) II
        ENDIF
 23     FORMAT(/'Read  # input stations = ',I3,
     &         /'Using # blade stations = ',I3,
     &         /'Enter # stations or <cr> for ',I3,' ',$)
 24     FORMAT(A)
      ENDIF
C
C---- spline blade geometry to "old" radial locations
      DO I = 1, IIX
        W1(I) = XI(I)
        W2(I) = CH(I)
        W4(I) = BETA(I)
        W6(I) = UBODY(I)
      ENDDO
      CALL SPLINE(W2,W3,W1,IIX)
      CALL SPLINE(W4,W5,W1,IIX)
      CALL SPLINE(W6,W7,W1,IIX)
C
C---- set radial stations for built-in distribution scheme
      CALL SETX
      CALL XWINIT
C
C---- interpolate read-in geometry to generated radial stations
      DO I = 1, II
        CH(I)    = SEVAL(XI(I),W2,W3,W1,IIX)
        BETA(I)  = SEVAL(XI(I),W4,W5,W1,IIX)
        UBODY(I) = SEVAL(XI(I),W6,W7,W1,IIX)
        BETA0(I) = BETA(I)
cc        write(*,*) 'load trp i,ch,beta ',i,ch(i),beta(i)
      ENDDO
      IINF = II + II/2
C
C--- Install defined aero properties for section(s), 
C    supply defaults for those not stored in XROTOR dataset
      DCL_STALL   =  0.1 ! CL increment from incipient to total stall
      DCLDA_STALL =  0.1 ! stalled lift curve slope    /radian
      CMCON       = -0.1 ! section Cm  (for pitch-axis moments)
      NAERO = 1
      XISECT = 0.0
      CALL PUTAERO(NAERO,XISECT,A0,CLMAX,CLMIN,
     &             DCLDA,DCLDA_STALL,DCL_STALL,
     &             CDMIN,CLDMIN,DCDCL2,CMCON,REREF,REXP)
      CALL SETIAERO
C
C---- calculate current operating point
      CALL APER(4,2,.TRUE.)
      IF(CONV) CALL OUTPUT(LUWRIT)
C
C---- define design quantities for design of MIL prop with same parameters
      RADDES = RAD
      VELDES = VEL
      ADVDES = 0.
      RPMDES = VEL/(RAD*ADV) * 30.0/PI
      R0DES = XI0*RAD
      RWDES = XW0*RAD
      TDDES = TTOT*RHO*VEL**2*RAD**2
      PDDES = PTOT*RHO*VEL**3*RAD**2
      DEST = .FALSE.
      DESP = .TRUE.
      DO I=1, II
        CLDES(I) = CL(I)
      ENDDO
      CLDES0 = 0.
C
C---- rotor now exists
      LROTOR = .TRUE.
      CALL SETIAERO
C
      RETURN
C
  200 WRITE(*,1010) FNAME(1:32)
      RETURN
C
  210 WRITE(*,1020) FNAME(1:32)
      CLOSE(LU)
      CONV = .FALSE.
      RETURN
C..............................
 1000 FORMAT(A)
 1010 FORMAT(' File  ',A,' not found'/)
 1020 FORMAT(' File  ',A,' has incompatible format'/
     &       ' Loading not completed'/)
      END ! LOAD

