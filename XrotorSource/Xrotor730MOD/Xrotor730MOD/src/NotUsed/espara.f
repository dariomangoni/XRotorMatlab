C-------------------------------------------------------------------------
C     Parametric Analysis in XROTOR
C     Philip Carter, Esotec Developments - June 2001, May 2002, May 2008
C     Esotec code is Free Software
C     Acknowledgements to Mark Drela and Harold Youngren
C
C     Version 0.7
C-------------------------------------------------------------------------

      SUBROUTINE ESPARA
      INCLUDE 'XROTOR.INC'
C----------------------------------
C     NPROPA = Propeller array size
C     NALTA  = Altitude array size
C     NPWRA  = Power array size
C     NRPMA  = RPM array size
C     NVELA  = Velocity array size
C----------------------------------
      PARAMETER (NPROPA=12,NALTA=5,NPWRA=12,NRPMA=10,NVELA=15)
      PARAMETER (NVALUE=20) ! Maximum no of parameter values (for ESPVAL)
C                            (No smaller than NALTA,NPWRA,NRPMA,or NVELA!)
C
      LOGICAL PAREAD,FILYES,SAVASK
C
      CHARACTER*32 EPROP(NPROPA)
      CHARACTER*32 EFILET,EFILE,NAMRD
      CHARACTER*4 COMAND,CONAND
      CHARACTER*132 COMARG,ANSARG,PROMPT,CONARG,LINE
      CHARACTER*1 CHKEY, ANS
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR
C
      INTEGER EALT(NALTA),EPWR(NPWRA),ERPM(NRPMA),EVEL(NVELA)
      INTEGER ESUNT,BETLOC,CONSTN,DEFSTN
C
      REAL EANG  (NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EADV  (NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EMACH (NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EEFFID(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EEFFIN(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EEFFNT(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL ETHRST(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL ESTALL(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
C
      INTEGER NPANAL(NPROPA),NPNOTC(NPROPA),BLDNUM(NPROPA)
      REAL BLDRAD(NPROPA),BETRAD(NPROPA)
C
C----Initialize Arrays
C
      DO I=1,NPROPA
         EPROP(I) = 'undefined'
         NPANAL(I)= 0
         NPNOTC(I)= 0
         BLDNUM(I)= 0
         BLDRAD(I)= 0.0 
         BETRAD(I)= 0.0
      ENDDO
C
      DO I=1,NALTA
         EALT(I)=0
      ENDDO
C
      DO I=1,NPWRA
         EPWR(I)=0
      ENDDO
C
      DO I=1,NRPMA
         ERPM(I)=0
      ENDDO
C
      DO I=1,NVELA
         EVEL(I)=0
      ENDDO
C
      DO 2 IPROP=1,NPROPA
         DO 2 IALT=1,NALTA
            DO 2 IPWR=1,NPWRA
               DO 2 IRPM=1,NRPMA
                  DO 2 IVEL=1,NVELA
          EANG  (IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EADV  (IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EMACH (IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EEFFID(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          ETHRST(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
 2    CONTINUE
C
C----Initialise defaults
C
      ESUNT=71               ! Database File IO Unit
      EFILE='file not saved' ! Initial ESPROP filename
C
      GREEK = .FALSE.
      CONV  = .FALSE.
C
      PAREAD=.FALSE.  ! True when file parameters have been read in
      FILYES=.FALSE.  ! True when file is available for writing to
      SAVASK=.FALSE.  ! True if data or parameters have not been saved
C
C----Conversion Factors
C
      ALTCON= 1./3280.84    ! Feet to Kilometers
      PWRCON= 746.0         ! HP to Watts
      VELCON= 0.51444       ! Knots to m/s
C
C----Initialize Accumulated Totals
C
      NPANAC=0    ! Accumulated Analysed Points
      NPNCAC=0    ! Accumulated Unconverged Points
      NPROP =0    ! No propellers initially loaded
C
C----Default analysis bounds
C
      LVELL=1
      LPWRL=1
      LRPML=1
      LALTL=1
      LVELU=1
      LPWRU=1
      LRPMU=1
      LALTU=1
      LPROP=0
C
C----Reference blade station for beta output
C
      DO I = 1,II
        BLOC = (XI(I) - XI0)/(1 - XI0)   
        IF(BLOC .GE. PLOC) THEN    ! PLOC set in CRINIT
           BETLOC = I
           GO TO 5
        ENDIF
      ENDDO
C
      BETLOC = II
 5    CONTINUE
C
C
C----PARA Menu----------------------------------------------------------
C
      WRITE(*,6100)
C
 1000 CONTINUE
      CALL ASKC('.PARA^',COMAND,COMARG)
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
      IF(COMAND.EQ.'    ') GO TO 10
      IF(COMAND.EQ.'?   ') WRITE(*,6100)
      IF(COMAND.EQ.'?   ') GO TO 1000
      IF(COMAND.EQ.'OPEN') GO TO 100
      IF(COMAND.EQ.'SAVE') GO TO 800
      IF(COMAND.EQ.'SAVA') GO TO 700
      IF(COMAND.EQ.'STAT') GO TO 200
C
      IF(COMAND.EQ.'DBAS') GO TO 185
      IF(COMAND.EQ.'PENT') GO TO 160 
      IF(COMAND.EQ.'PVEL') GO TO 20 
      IF(COMAND.EQ.'PRPM') GO TO 20  
      IF(COMAND.EQ.'PPOW') GO TO 20     
      IF(COMAND.EQ.'PALT') GO TO 20
C
      IF(COMAND.EQ.'ADDP') GO TO 130
      IF(COMAND.EQ.'REMP') GO TO 130
      IF(COMAND.EQ.'NAMP') GO TO 130  
C
      IF(COMAND.EQ.'DISB') GO TO 350
      IF(COMAND.EQ.'BVEL') GO TO 50
      IF(COMAND.EQ.'BRPM') GO TO 50
      IF(COMAND.EQ.'BPOW') GO TO 50 
      IF(COMAND.EQ.'BALT') GO TO 50
C
      IF(COMAND.EQ.'IPRO') GO TO 50  
      IF(COMAND.EQ.'BLOC') GO TO 60
C
      IF(COMAND.EQ.'RUN ') GO TO 400           
C
      WRITE(*,1050) COMAND
      GO TO 1000
C
C----SAVE warning before quitting
C
 10   IF(SAVASK) THEN
         WRITE(*,7045)
         READ(*,7000) ANS
         IF(INDEX('Nn',ANS).NE.0) GO TO 1000
      END IF
      RETURN
C
C----To Modify Database Parameters------------------------
C
 20   CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
      DO I=1,NPROP
         IF(NPANAL(I) .NE. 0) THEN
            WRITE(*,*)'Database contains analysis data' 
            WRITE(*,*)'Cannot change Database Parameters'
            GO TO 1000
         END IF
      END DO
C
      IF(COMAND.EQ.'PVEL') GO TO 165 
      IF(COMAND.EQ.'PRPM') GO TO 170  
      IF(COMAND.EQ.'PPOW') GO TO 175     
      IF(COMAND.EQ.'PALT') GO TO 180
C
C---Modify Analysis Bounds--------------------------------
C
 50   IF(.NOT.LROTOR) THEN
         WRITE(*,*) 'No rotor'
         WRITE(*,*) 'Load rotor at top level'
         GO TO 1000
      ENDIF
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(COMAND .EQ. 'IPRO') THEN
         IF(NINPUT.GE.1) THEN
           LPROP = IINPUT(1)
         ELSE
           CALL ASKI(' Enter propeller index ^',LPROP)
         ENDIF
C
         IF(LPROP .LT. 1 .OR. LPROP .GT. NPROP) THEN
           WRITE(*,*)'Index outside parameter range'
           NINPUT = 0
           GO TO 50
         END IF      
C
         IF(NPANAL(LPROP).EQ.0) THEN
           EPROP(LPROP)= NAME
           BLDNUM(LPROP)=NBLDS
           BLDRAD(LPROP)=RAD
           BETRAD(LPROP)=XI(BETLOC)*RAD
         ELSE
           WRITE(*,*) 'Caution: propeller index contains data'
         ENDIF
C
         GO TO 200
      END IF
C
      IF(COMAND .EQ. 'BVEL') THEN
         PROMPT=' Enter Velocity analysis bounds <0 for full sweep>^'
         CALL PARAM(PROMPT,NVEL,LVELU,LVELL)
         GO TO 350
      END IF
C
      IF(COMAND .EQ. 'BRPM') THEN
         PROMPT=' Enter RPM analysis bounds <0 for full sweep>^'
         CALL PARAM(PROMPT,NRPM,LRPMU,LRPML)
         GO TO 350
      END IF
C
      IF(COMAND .EQ. 'BPOW') THEN
         PROMPT=' Enter Power analysis bounds <0 for full sweep>^'
         CALL PARAM(PROMPT,NPWR,LPWRU,LPWRL)
         GO TO 350
      END IF
C
      IF(COMAND .EQ. 'BALT') THEN
         PROMPT=' Enter Altitude analysis bounds <0 for full sweep>^'
         CALL PARAM(PROMPT,NALT,LALTU,LALTL)
         GO TO 350
      END IF
C
C----------------------------------------------------------------------
C----Change blade station for Beta output
C
 60   IF(.NOT.LROTOR) THEN
         WRITE(*,*) 'No rotor'
         WRITE(*,*) 'Load rotor at top level'
         GO TO 1000
      ENDIF
C
      IF(NINPUT.GE.1) THEN
        PLOC = RINPUT(1)
      ELSE
        WRITE(*,*)
        WRITE(*,*) 'root 0 <---------> 1 tip'
        CALL ASKR( 'Enter beta output blade location^',PLOC)
      ENDIF
C
      IF(PLOC.LT.0.0 .OR. PLOC .GT. 1.0) THEN
        NINPUT = 0
        GO TO 60
      ENDIF
C
      DO I = 1,II
        BLOC = (XI(I) - XI0)/(1 - XI0)   
        IF(BLOC .GE. PLOC) THEN
           BETLOC = I
           GO TO 65
        ENDIF
      ENDDO  
C
      BETLOC = II
 65   BRAD   = XI(BETLOC)*RAD   ! Beta Stn radius
C
      WRITE(*,7050)BETLOC, XI(BETLOC), BRAD, BLOC
      GO TO 1000
C
C-----------------------------------------------------------------------
C----Open EXISTING ESPROP File
C
 100  IF(FILYES) THEN
         WRITE(*,*) 'One database can be loaded per session'
         WRITE(*,*) 'Exit .PARA to initialize'
         GO TO 1000
      END IF
C
      IF(PAREAD) THEN
         DO I=1,NPROP
            IF(NPANAL(I) .NE. 0) THEN
               WRITE(*,*) 'Existing data will be overwritten' 
               WRITE(*,*) 'SAVE and exit .PARA to initialize'
               GO TO 1000
            END IF
         END DO
      END IF
C
      EFILET = COMARG
      IF(EFILET(1:1) .EQ. ' ')
     $CALL ASKS(' Enter ESPROP filename <A to abort>^',EFILET)
C
      IF (EFILET .EQ. 'A' .OR. EFILET .EQ. 'a') GO TO 1000
C
C----Open File
C
      EFILE = EFILET
      OPEN(UNIT=ESUNT,FILE=EFILE,STATUS='OLD',IOSTAT=KODE)
C
          IF(KODE.NE.0) THEN
             WRITE(*,*)'Cannot open file ',EFILE
             GO TO 1000
          END IF
C
C----Read data into arrays
C
      CALL RDLINE2(ESUNT,LINE,ICNT)
      IF(LINE.EQ.'END' .OR. LINE.EQ.'ERR') GO TO 1000
C
      IF(LINE(1:6).NE.'ESPROP') THEN
        WRITE(*,*) 'Not an ESPROP database file'
        CLOSE(ESUNT)
        GO TO 1000
      ENDIF
C
      READ(UNIT=ESUNT,FMT=7010,ERR=900)NPROP,NALT,NPWR,NRPM,NVEL
C
      IF(NPROP .GT. NPROPA .OR. NALT .GT. NALTA .OR. NPWR .GT. NPWRA
     $   .OR. NRPM .GT. NRPMA .OR. NVEL .GT. NVELA) THEN
         WRITE(UNIT=*,FMT=*)'Too many operating points -enlarge arrays'
         WRITE(UNIT=*,FMT=*)'File read aborted'
         CLOSE(UNIT=ESUNT)
         GO TO 1000
      END IF
C
      DO I=1,NPROP
         READ(UNIT=ESUNT,FMT=7020,ERR=900) EPROP(I)
      ENDDO
C
      DO I=1,NALT
         READ(UNIT=ESUNT,FMT=7030,ERR=900) EALT(I)
      ENDDO
C
      DO I=1,NPWR
         READ(UNIT=ESUNT,FMT=7030,ERR=900) EPWR(I)
      ENDDO
C
      DO I=1,NRPM
         READ(UNIT=ESUNT,FMT=7030,ERR=900) ERPM(I)
      ENDDO
C
      DO I=1,NVEL
         READ(UNIT=ESUNT,FMT=7030,ERR=900) EVEL(I)
      ENDDO
C 
      DO I=1,NPROP
         READ(UNIT=ESUNT,FMT=7035,ERR=900)
     $   BLDNUM(I),BLDRAD(I),BETRAD(I),NPANAL(I),NPNOTC(I)
      ENDDO
C
      DO 111 IPROP=1,NPROP
         DO 111 IALT=1,NALT
            DO 111 IPWR=1,NPWR
               DO 111 IRPM=1,NRPM
                  DO 111 IVEL=1,NVEL
      READ(UNIT=ESUNT,FMT=7040,ERR=900)
     $                       EANG  (IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EEFFID(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       ETHRST(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EADV  (IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EMACH (IPROP,IALT,IPWR,IRPM,IVEL),
     $                       ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)
C
 111  CONTINUE
C
      CLOSE(UNIT=ESUNT)
C
C----Initialize analysis bounds for full sweep
C
      LVELL=1
      LPWRL=1
      LRPML=1
      LALTL=1
      LVELU=NVEL
      LPWRU=NPWR
      LRPMU=NRPM
      LALTU=NALT
C
      PAREAD = .TRUE.
      FILYES = .TRUE.
C
      WRITE(*,*)'ESPROP Database Loaded: ',EFILE
      GO TO 190
C
C-------------------------------------------------------------------------
C----Modify propeller dimension and names
C
 130  CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(COMAND.EQ.'ADDP') GO TO 135
      IF(COMAND.EQ.'REMP') GO TO 140
      IF(COMAND.EQ.'NAMP') GO TO 150 
C
C----Add propellers to list-------------------------------
C
 135  IF(NINPUT.GE.1) THEN
         NPRADD = IINPUT(1)
         GO TO 139
      END IF
C
 137  NPRADD=1
      CALL ASKI(' Enter no. of propellers to add^',NPRADD)
C
 139  IF(NPRADD .LT. 0) THEN
         WRITE(*,*)'Must be a positive integer'
         GO TO 137
      END IF
C
      IF(NPRADD+NPROP .GT. NPROPA) THEN
         WRITE(*,*)'Exceeds array limit of ',NPROPA
         GO TO 137
      END IF
C
      NPROP=NPROP+NPRADD
      IF(NPRADD.EQ.0)THEN
         WRITE(*,*)'Propeller list unchanged'
      ELSE
         WRITE(*,*)'Propeller list increased to ',NPROP
         SAVASK=.TRUE.
      END IF
C
      GO TO 1000
C
C----Delete propellers from list-----------------------------
C
 140  IF(NINPUT.GE.1) THEN
         NPREM = IINPUT(1)
         GO TO 144
      END IF
C
 142  NPREM=1
      CALL ASKI(' Enter no. of propellers to remove^',NPREM)
      
 144  IF(NPREM .LT.0) THEN
         WRITE(*,*)'Must be a positive integer'
         GO TO 142
      END IF
C
      IF(NPREM .GE. NPROP) THEN
         WRITE(*,*)'At least one propeller is required'
         GO TO 142
      END IF
C
      NPREMA=0
      DO I=NPROP,NPROP-NPREM+1,-1
         IF(NPANAL(I) .NE. 0) THEN
            WRITE(*,7170) I
            READ(*,7000) ANS
            IF(INDEX('Nn',ANS).NE.0) GO TO 145
         END IF
         NPREMA=NPREMA+1
      END DO
C
 145  NPROP=NPROP-NPREMA
      IF(NPREMA.EQ.0) THEN
         WRITE(*,*)'Propeller list unchanged'
      ELSE
         WRITE(*,*)'Propeller list reduced to ', NPROP
         SAVASK=.TRUE.
      END IF
C
      GO TO 1000
C
C----Enter/Change Propeller Names---------------------------
C
 150  IF(NINPUT.GE.1) THEN
        NUMBLD = IINPUT(1)
      ELSE 
        NUMBLD=1
        CALL ASKI(' Enter index of propeller being named^',NUMBLD)
      ENDIF
C
      WRITE(*,7175) NUMBLD, EPROP(NUMBLD)
      CALL ASKS(' Enter propeller name <ret takes default>^', NAMRD)
      IF (NAMRD(1:1) .EQ. ' ') GO TO 1000
C
      EPROP(NUMBLD) = NAMRD
      SAVASK=.TRUE.
      GO TO 200
C
C-------------------------------------------------------------------------
C----Enter Database Parameters
C
 160  CONTINUE
C
C----Check if analysis data exists
C
      DO I=1,NPROP
         IF(NPANAL(I) .NE. 0) THEN
            WRITE(*,*)'Database contains analysis data' 
            WRITE(*,*)'Cannot enter new Database Parameters'
            GO TO 1000
         END IF
      END DO
C
      PAREAD = .FALSE.
C
C----Propeller Parameter Dimension
C
      WRITE(*,7160) NPROPA,NVELA,NPWRA,NRPMA,NALTA
C
 162  NPROPT=NPROP
      CALL ASKI (' Enter Number of Propellers^', NPROPT)
C
      IF(NPROPT .GT. NPROPA) THEN
         WRITE(*,*)'Exceeds array limit of ',NPROPA
         GO TO 162
      END IF
C
      IF(NPROPT .LT. 1) THEN
         WRITE(*,*)'Minimum 1 Propeller required'
         GO TO 162
      END IF
C
      NPROP = NPROPT
C
      DO I=1,NPROPA
         EPROP(I)='undefined'
      ENDDO
C
C----Velocity Parameters
C
 165  WRITE(*,*)
      PROMPT= ' Enter Velocity parameters (knots)^'
      CALL ESPVAL(PROMPT,NVALUE,NVEL,NVELA,EVEL)
      LVELL=1
      LVELU=NVEL
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----RPM Parameters
C
 170  WRITE(*,*)
      PROMPT= ' Enter RPM parameters^'
      CALL ESPVAL(PROMPT,NVALUE,NRPM,NRPMA,ERPM)
      LRPML=1
      LRPMU=NRPM
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----Power Parameters
C
 175  WRITE(*,*)
      PROMPT= ' Enter Power parameters (hp)^'
      CALL ESPVAL(PROMPT,NVALUE,NPWR,NPWRA,EPWR)
      LPWRL=1
      LPWRU=NPWR
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----Altitude Parameters
C
 180  WRITE(*,*)
      PROMPT= ' Enter Altitude parameters (ft)^'
      CALL ESPVAL(PROMPT,NVALUE,NALT,NALTA,EALT)
      LALTL=1
      LALTU=NALT
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----Parameters read in - write to terminal
C
      PAREAD = .TRUE.
      GO TO 185
C
C--------------------------------------------------------------------------
C----Write Parameter List to Terminal
C
 185  CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
 190  CONTINUE
C
C----Accumulated totals
C
      NPANAC=0
      NPNCAC=0
      DO I=1,NPROP
         NPANAC=NPANAC+NPANAL(I)
         NPNCAC=NPNCAC+NPNOTC(I)
      ENDDO
C
      NPPRO =NALT*NPWR*NRPM*NVEL
      NPNTOT=NPROP*NPPRO
      NPCONV=NPANAC-NPNCAC
C
C----Write database parameters
C
      WRITE(*,7100)EFILE,NPROP,NVEL,NRPM,NPWR,NALT,
     $NPPRO,NPNTOT,NPANAC,NPCONV,NPNCAC
C
      WRITE(*,7110)
      DO I=1,NPROP
         WRITE(*,7140)I,EPROP(I)
      ENDDO
C
      WRITE(*,7130)
      DO I=1,NVEL
         WRITE(*,7150)I,EVEL(I)
      ENDDO
C
      WRITE(*,7125)
      DO I=1,NRPM
         WRITE(*,7150)I,ERPM(I)
      ENDDO
C
      WRITE(*,7120)
      DO I=1,NPWR
         WRITE(*,7150)I,EPWR(I)
      ENDDO
C
      WRITE(*,7115)
      DO I=1,NALT
         WRITE(*,7150)I,EALT(I)
      ENDDO
C
      WRITE(*,7135)
C
      GO TO 1000
C
C------------------------------------------------------------------------
C----Display Analysis Status
C
 200  CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
      WRITE(*,7400) EFILE
      DO I=1,NPROP
         WRITE(*,7420)I,EPROP(I),BLDNUM(I),BLDRAD(I),BETRAD(I),
     $                NPANAL(I),NPNOTC(I)
      ENDDO
      WRITE(*,7430)
C
      IF(LPROP.EQ.0) THEN
        WRITE(*,*) 'Propeller index not set'
      ELSE
        WRITE(*,7440) LPROP
      ENDIF
C
      GO TO 1000
C
C----------------------------------------------------------------------
C----Display analysis parameter bounds
C
 350  IF(.NOT.LROTOR) THEN
         WRITE(*,*) 'No rotor'
         WRITE(*,*) 'Load rotor at top level'
         GO TO 1000
      ENDIF
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(LPROP .EQ. 0) THEN
         WRITE(*,*)'Propeller has not been specified'
 355     CALL ASKI(' Enter Propeller index ^',LPROP)
         IF(LPROP .LT. 1 .OR. LPROP .GT. NPROP) THEN
            WRITE(*,*)'Index outside parameter range'
            GO TO 355
         ENDIF
C
         IF(NPANAL(LPROP).EQ.0) THEN
           EPROP(LPROP)= NAME
           BLDNUM(LPROP)=NBLDS
           BLDRAD(LPROP)=RAD
           BETRAD(LPROP)=XI(BETLOC)*RAD
         ELSE
           WRITE(*,*) 'Caution: propeller index contains data'
         ENDIF
C
      ENDIF
C
      NOVEL=LVELU-LVELL+1
      NORPM=LRPMU-LRPML+1
      NOPWR=LPWRU-LPWRL+1
      NOALT=LALTU-LALTL+1
C
      NAOUT = NOVEL*NOPWR*NORPM*NOALT
C
      WRITE(*,7300) LPROP,EPROP(LPROP),
     $   LVELL,LVELU,EVEL(LVELL),EVEL(LVELU),NOVEL,
     $   LRPML,LRPMU,ERPM(LRPML),ERPM(LRPMU),NORPM,
     $   LPWRL,LPWRU,EPWR(LPWRL),EPWR(LPWRU),NOPWR,
     $   LALTL,LALTU,EALT(LALTL),EALT(LALTU),NOALT,
     $   NAOUT
C
      GO TO 1000
C
C-----------------------------------------------------------------------
C----Analysis Routines
C-----------------------------------------------------------------------
C
 400  CONTINUE
C
      IF(.NOT.LROTOR) THEN
         WRITE(*,*) 'No rotor'
         WRITE(*,*) 'Load rotor at top level'
         GO TO 1000
      END IF
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
C----Prompt for propeller index if undefined
C
      IF(LPROP .EQ. 0) THEN
 410     CALL ASKI(' Enter Propeller index <ret to abort>^',LPROP)
         IF(LPROP.EQ.0) GO TO 1000
         IF(LPROP .LT. 1 .OR. LPROP .GT. NPROP) THEN
            WRITE(*,*)'Index outside parameter range'
            GO TO 410
         END IF
      END IF
C
C----Set propeller index
C
      IPROP=LPROP
C
C----Check if propeller index holds data
C
      IF(NPANAL(IPROP).NE.0) THEN
         WRITE(*,7180) IPROP
         READ(*,7000) ANS
         IF(INDEX('Nn',ANS).NE.0) GO TO 1000
C
 415     WRITE(*,*)'Overwrite or Accumulate data? <o/a>'
         READ(*,7000) ANS
C
         IF(INDEX('Oo',ANS).NE.0) THEN ! IF Overwrite
            NPANAL(IPROP)=0            ! Initialize totals
            NPNOTC(IPROP)=0
            EPROP (IPROP)=NAME         ! Record rotor name
C
            DO 418 IVEL=1,NVEL         ! Initialize output data
               DO 418 IRPM=1,NRPM
                  DO 418 IPWR=1,NPWR
                     DO 418 IALT=1,NALT
                       EANG  (IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
                       EADV  (IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
                       EMACH (IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
                       EEFFID(IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
                       EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
                       EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
                       ETHRST(IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
                       ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) = 0.0
 418        CONTINUE
C
            WRITE(*,7185)IPROP
         ELSE IF(INDEX('Aa',ANS).NE.0) THEN
            GO TO 420
         ELSE
            WRITE(*,*)'<O> to overwrite, <A> to accumulate'
            GO TO 415
         END IF
C
      ENDIF
C
C----Record blade data
C
 420  SAVASK=.TRUE.   
      BLDNUM(IPROP)=NBLDS
      BLDRAD(IPROP)=RAD
      BETRAD(IPROP)=XI(BETLOC)*RAD
C
C----Record Name if currently undefined
C
      IF(EPROP(IPROP).EQ.'undefined') EPROP(IPROP)=NAME
C
C----Loop through operating points
C
      DO 699 IVEL=LVELL,LVELU       
         VEL = EVEL(IVEL)*VELCON
C
        DO 699 IRPM=LRPML,LRPMU
           RPM = ERPM(IRPM)
           ADV = VEL / (RAD*RPM*PI/30.) 
C
          DO 699 IPWR=LPWRL,LPWRU
             PSPEC = EPWR(IPWR)*PWRCON  
C
            DO 699 IALT = LALTL, LALTU
              ALT = EALT(IALT)*ALTCON
              CALL ATMO(ALT,VSO,RHO,RMU)
C
              NPANAL(IPROP)=NPANAL(IPROP)+1  ! Increment counter   
              LAUTO = .FALSE.                
C
C----Initialize blade angle
C
              IF(IPWR.EQ.LPWRL.AND.IALT.EQ.LALTL) CALL VVECT(1)
C
C----Analyse operating point
C
 450          CONV = .FALSE.
              CALL APER(3,1,LOPRINI)
C
C----If converged, trap negative convergence
C
              IF(CONV) THEN
C
                IF(TTOT .LT. 0.) THEN
                  WRITE(*,7520)
                  GO TO 480
                END IF
C
C----Write to arrays and to terminal and continue
C
                CALL ESOPUT (LUWRIT,TDIM,EFFTOT,EFFIND,EIDEAL,
     $                       REFDEG,MACH,STALPC,BETLOC)
C
                TDIMKG=TDIM/9.81
                ADVJ=ADV*PI
C
                EANG  (IPROP,IALT,IPWR,IRPM,IVEL) = REFDEG
                EADV  (IPROP,IALT,IPWR,IRPM,IVEL) = ADVJ
                EMACH (IPROP,IALT,IPWR,IRPM,IVEL) = MACH
                EEFFID(IPROP,IALT,IPWR,IRPM,IVEL) = EIDEAL
                EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL) = EFFIND
                EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL) = EFFTOT
                ETHRST(IPROP,IALT,IPWR,IRPM,IVEL) = TDIMKG
                ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) = STALPC
C
                WRITE(UNIT=*,FMT=7500) NPANAL(IPROP),NAOUT,
     $          IPROP,EVEL(IVEL),ERPM(IRPM),EPWR(IPWR),EALT(IALT),
     $          REFDEG,ADV,MACH,TDIMKG,STALPC,EIDEAL,EFFIND,EFFTOT
C
                GO TO 699
C
              END IF
C
C----------------------------------------------------------------------
C----Convergence Failed or Negative 
C----Write unconverged output and failed parameters to terminal
C
C----Restore blade angles
C
 480        IF(LAUTO) GO TO 620
C
            DO I=1, II
              BETA(I)  = BETA(I)  - DBETA
              BETA0(I) = BETA0(I) - DBETA
            ENDDO
C
            CALL OUTPUT(LUWRIT)
            WRITE(*,*) 'Initial blade angles restored'
C
            ESMETV=EVEL(IVEL)*VELCON
            ESMETA=EALT(IALT)*ALTCON
            ESMETP=EPWR(IPWR)*PWRCON
C
            WRITE(UNIT=*,FMT=7550) NPANAL(IPROP),NAOUT,IPROP,
     $      EVEL(IVEL),ERPM(IRPM),EPWR(IPWR),EALT(IALT),ESMETV,ESMETP 
C
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
      IF(CONAND.EQ.'    ') GO TO 450
C---------------------------------------------
      IF(CONAND.EQ.'?   ') THEN
        WRITE(*,5000)
        GO TO 600
      ENDIF
C---------------------------------------------
      IF(CONAND.EQ.'STOP') THEN
        WRITE(*,7580)
        WRITE(*,7300) LPROP,EPROP(LPROP),
     $  LVELL,LVELU,EVEL(LVELL),EVEL(LVELU),NOVEL,
     $  LRPML,LRPMU,ERPM(LRPML),ERPM(LRPMU),NORPM,
     $  LPWRL,LPWRU,EPWR(LPWRL),EPWR(LPWRU),NOPWR,
     $  LALTL,LALTU,EALT(LALTL),EALT(LALTU),NOALT,
     $  NAOUT
C
        NPNOTC(IPROP)=NPNOTC(IPROP)+1
C
        WRITE(*,7600)IVEL,IRPM,IPWR,IALT,NPANAL(IPROP),
     &               NPNOTC(IPROP)
        GO TO 1000 
      END IF
C--------------------------------------------
      IF(CONAND.EQ.'AUTO' .OR. CONAND.EQ.'A   ') THEN
         LAUTO = .TRUE.
         IAUTO = 0
         GO TO 620
      ENDIF
C-------------------------------------------
      IF(CONAND.EQ.'ASET') THEN
        CALL ASKI('Max number of convergence attempts^',NAUTO)
        CALL ASKR('Max deviation from velocity vector (deg)^',CRANGE)
 610    WRITE(*,*)
        WRITE(*,*) 'root 0 <---------> 1 tip'
        CALL ASKR( 'Sample blade location^',CLOC)
        IF(CLOC.LT.0.0 .OR. CLOC .GT. 1.0) GO TO 610
        GO TO 600
      ENDIF
C-----------------------------------------
      IF(CONAND.EQ.'VRTX') THEN
        VRTX = .NOT.VRTX
        IF(.NOT.VRTX) WRITE(*,*)'Discrete Vortex Formulation deselected'
        IF(VRTX)      WRITE(*,*)'Discrete Vortex Formulation selected'
        GO TO 600
      ENDIF
C------------------------------------------
      IF(CONAND.EQ.'FORM') THEN
        FAST = .NOT.FAST
        IF(FAST)      WRITE(*,*)'Graded Momentum Formulation selected'
        IF(.NOT.FAST) WRITE(*,*)'Potential Formulation selected'
        GO TO 600
      ENDIF
C----------------------------------------
      IF(CONAND.EQ.'WAKE') THEN
        FREE = .NOT.FREE
        IF(FREE)      WRITE(*,*)'Self-deforming wake selected'
        IF(.NOT.FREE) WRITE(*,*)'Rigid wake selected'
        GO TO 600
      ENDIF
C---------------------------------------
      IF(CONAND.EQ.'INIT') THEN
        LOPRINI = .NOT.LOPRINI
        IF(LOPRINI) THEN
          WRITE(*,*) 'Analysis case will be initialized'
        ELSE
          WRITE(*,*) 'Analysis case will not be initialized'
        ENDIF
        GO TO 600
      ENDIF
C-------------------------------------
      IF(CONAND.EQ.'REIN') THEN
        CALL REINIT
        GO TO 600
      ENDIF
C--------------------------------------
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
C-------------------------------------
      IF(CONAND.EQ.'RPM ') THEN
        CONV = .FALSE.
        CALL APER(4,2,LOPRINI)
        CALL OUTPUT(LUWRIT)
C
        IF(CONV .AND. TTOT .GE. 0.) THEN
           GO TO 600
        ELSE IF(CONV .AND. TTOT .LT. 0.) THEN
           WRITE(*,7520)
        ELSE IF(TTOT .LT. 0.) THEN
           WRITE(*,7530)
        ENDIF
C
        WRITE(*,7550) NPANAL(IPROP),NAOUT,IPROP,EVEL(IVEL),
     $  ERPM(IRPM),EPWR(IPWR),EALT(IALT),ESMETV,ESMETP 
C
        GO TO 600
      ENDIF
C-----------------------------------------
      IF(CONAND.EQ.'SKIP')THEN
        ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)= -99.9 ! Non-conv. flag
        NPNOTC(IPROP)=NPNOTC(IPROP)+1            ! Non-conv. counter
        GO TO 699
      ENDIF
C-----------------------------------------
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
      GO TO 450
C
C---------------------------------------------------------------------
C----End Convergence Controls
C---------------------------------------------------------------------
C
 699  CONTINUE
C
C----ALL DONE! Display totals and return to PARA menu
C
      WRITE(UNIT=*,FMT=7570) NPANAL(IPROP),NPNOTC(IPROP)
      GO TO 1000 
C
C
C-----------------------------------------------------------------------
C----Open NEW ESPROP file
C
 700  CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Use PENT or load ESPROP file'
         GO TO 1000
      END IF
C
      EFILET = COMARG
      IF(EFILET(1:1) .EQ. ' ')
     $CALL ASKS(' Enter new database filename <A to abort>^',EFILET)
C
      IF(EFILET .EQ. 'A' .OR. EFILET .EQ. 'a') GO TO 1000
C
      EFILE = EFILET
      OPEN(UNIT=ESUNT,FILE=EFILE,STATUS='NEW',IOSTAT=KODE)
C
          IF(KODE.NE.0) THEN
             WRITE(*,*)'Error opening file ',EFILE
             GO TO 1000
          END IF
C
      FILYES = .TRUE.
      GO TO 805
C
C-----------------------------------------------------------------------
C----Write Arrays to ESPROP File
C
 800  CONTINUE
C
      IF(.NOT.FILYES) GO TO 700
C
      OPEN(UNIT=ESUNT,FILE=EFILE,STATUS='OLD',IOSTAT=KODE)
C
         IF(KODE.NE.0) THEN
             WRITE(UNIT=*,FMT=*)'Error Opening File ',EFILE
             GO TO 1000
         END IF
C
 805  REWIND(UNIT=ESUNT)
C
      WRITE(UNIT=ESUNT,FMT=7005,ERR=910)
      WRITE(UNIT=ESUNT,FMT=7010,ERR=910)NPROP,NALT,NPWR,NRPM,NVEL
C
      DO I=1,NPROP
         WRITE(UNIT=ESUNT,FMT=7020,ERR=910)EPROP(I)
      ENDDO
C
      DO I=1,NALT
         WRITE(UNIT=ESUNT,FMT=7030,ERR=910)EALT(I)
      ENDDO
C
      DO I=1,NPWR
         WRITE(UNIT=ESUNT,FMT=7030,ERR=910)EPWR(I)
      ENDDO
C
      DO I=1,NRPM
         WRITE(UNIT=ESUNT,FMT=7030,ERR=910)ERPM(I)
      ENDDO
C
      DO I=1,NVEL
         WRITE(UNIT=ESUNT,FMT=7030,ERR=910)EVEL(I)        
      ENDDO
C
      DO I=1,NPROP
         WRITE(UNIT=ESUNT,FMT=7035,ERR=910)
     $   BLDNUM(I),BLDRAD(I),BETRAD(I),NPANAL(I),NPNOTC(I)
      ENDDO
C
      DO 811 IPROP=1,NPROP
         DO 811 IALT=1,NALT
            DO 811 IPWR=1,NPWR
               DO 811 IRPM=1,NRPM
                  DO 811 IVEL=1,NVEL
      WRITE(UNIT=ESUNT,FMT=7040,ERR=910)
     $                       EANG  (IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EEFFID(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       ETHRST(IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EADV  (IPROP,IALT,IPWR,IRPM,IVEL),
     $                       EMACH (IPROP,IALT,IPWR,IRPM,IVEL),
     $                       ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)
C
 811  CONTINUE
C
      CLOSE(UNIT=ESUNT)
      WRITE(*,*)'Data written to file: ',EFILE
      SAVASK = .FALSE.
C
      GO TO 1000     
C
C----End of File Print Routine------------------------------------------
C
C----End Game
C
 900  WRITE(*,*)' ****Database File Read Error. Aborted****'
      CLOSE(UNIT=ESUNT)
      GO TO 1000
C
 910  WRITE(*,*)' ****Disk File Write Error. Aborted****'
      CLOSE (UNIT=ESUNT)
      GO TO 1000
C
C
C----Format Statements----------------------------------------------------
C
C
 1050 FORMAT(1X,A4,' command not recognized.' //
     &             '  Type "?" for list, <Return> to exit menu.')
C
 5000 FORMAT(
     &  /' <Return>  Continue'
     &  /'   AUTO<a> Auto-initialize blade angles and continue'
     &  /'   ANGL r  Change blade angles manually'  
     &  /'   RPM     Analyse at current blade angle' 
     &  /'   FORM    Toggle between Graded Mom.and Potential Form'
     &  /'   VRTX    Toggle between Graded Mom.and Vortex Form'
     &  /'   WAKE    Toggle between rigid and self-deforming wake'
     &  /'   INIT    Initialize next analysis case'
     &  /'   REIN    Re-initialize rotor to known operating state'
     &  /'   ASET    Change AUTO settings'
     &  /'   SKIP    Give up trying to converge this point'
     &  /'   STOP    Abort this run and return to .PARA prompt')
C
C
 6100 FORMAT(
     $  /'   OPEN f  Load ESPROP database from disk'
     $  /'   SAVE    Save current data to Esprop file'
     $  /'   SAVA f  Save current data to NEW Esprop file (Save As)'
     $  /'   STAT    Display analysis status'
C
     $ //'   DBAS    Display database parameters'
     $  /'   PENT    Enter   database parameters (all)'
     $  /'   PVEL    Change  Velocity     "'
     $  /'   PRPM    Change  RPM          "'
     $  /'   PPOW    Change  Power        "'
     $  /'   PALT    Change  Altitude     "'
C
     $ //'   ADDP i  Add propeller/s'
     $  /'   REMP i  Remove propeller/s'
     $  /'   NAMP i  Change propeller name'
     $  /'   BLOC r  Change beta output blade location'
C
     $ //'   DISB    Display analysis bounds'
     $  /'   BVEL    Change  Velocity   "'
     $  /'   BRPM    Change  RPM        "'
     $  /'   BPOW    Change  Power      "'
     $  /'   BALT    Change  Altitude   "'
C
     $ //'   IPRO i  Specify propeller index'
     $  /'   RUN     Run parametric analysis'
     $  /'   <ret>   Return to top level (unsaved data is lost)')
C
 7000 FORMAT(A)
C
 7005 FORMAT('ESPROP Parametric Database')
 7010 FORMAT(1X,I4,I4,I4,I4,I4)
 7020 FORMAT(1X,A30)
 7030 FORMAT(1X,I6)
 7035 FORMAT(1X,I4,F9.5,F9.5,I6,I6)
 7040 FORMAT(1X,F7.3,F7.4,F7.4,F7.4,F7.2,F7.3,F7.3,F7.2)
C
 7045 FORMAT(' Unsaved data will be lost',/,
     $       ' Exit PARA anyway? <y/n>')
C
 7050 FORMAT(' Beta output blade station: ',I2,/,
     $       ' Blade station r/R:        ',F6.3,/,
     $       ' Station radius (m):       ',F6.3,/,
     $       ' Blade location (0 - 1):   ',F6.3)
C
 7100 FORMAT(/,6X,'ESPROP Database Parameters',/,
     $4X, 30('='),/,
     $6X,'Filename: ',A30,/,
     $4X, 30('-'),/,
     $7X,I2,' Propellers',/,
     $7X,I2,' Velocity Points',/,
     $7X,I2,' RPM Points',/,
     $7X,I2,' Power Points',/,
     $7X,I2,' Altitude Points',/,
     $4X, 30('-'),/,
     $5X,' Points per rotor    ',I6,/,
     $5X,' Total Points        ',I6,/,
     $5X,' Analysed Points     ',I6,/,
     $5X,' Converged Solutions ',I6,/,
     $5X,' Failed Points       ',I6)
C
 7110 FORMAT(4X,30('-'),/,6X,' Propeller')
 7115 FORMAT(4X,30('-'),/,6X,' Altitude       (ft)')
 7120 FORMAT(4X,30('-'),/,6X,'   Power         (hp)')
 7125 FORMAT(4X,30('-'),/,6X,'    RPM        (r/min)')
 7130 FORMAT(4X,30('-'),/,6X,' Velocity      (knots)')
 7135 FORMAT(4X,30('='),/)
C
 7140 FORMAT(10X,I2,5X,A30)
 7150 FORMAT(10X,I2,8X,I6)
C
 7160 FORMAT(/,' Current Array Limits',/,
     $         ' --------------------',/,
     $        3X,I2,'  Propellers',/,
     $        3X,I2,'  Velocities',/,
     $        3X,I2,'  Powers',/,
     $        3X,I2,'  RPMs',/,
     $        3X,I2,'  Altitudes',/,
     $         ' --------------------')
C
 7170 FORMAT(' WARNING: Propeller ',I2,' contains data',/,
     $       ' Remove propeller? <y/n>')
C
 7175 FORMAT(/,' Propeller ',I2,': current name is ',A32)
C
 7180 FORMAT(/,' WARNING: Propeller ',I2,' contains data',/,
     $       ' Continue with analysis? <y/n>')
C
 7185 FORMAT(/,' Propeller ',I2,' data initialized to zero')
C
 7300 FORMAT(/,
     $'                 Analysis Bounds',/,
     $'                 ---------------',/,
     $'  Propeller ',I2,'            Name: ',A16,/,
     $2X, 46('-'),/,
     $'  Velocity ',I2,' to ',I2,3X,I5,' to ',I5,' kts  ',I2,' pts'/,
     $'  RPM      ',I2,' to ',I2,3X,I5,' to ',I5,' rpm  ',I2,' pts'/,
     $'  Power    ',I2,' to ',I2,3X,I5,' to ',I5,' hp   ',I2,' pts'/,
     $'  Altitude ',I2,' to ',I2,3X,I5,' to ',I5,' ft   ',I2,' pts'/,
     $2X, 46('-'),/,
     $'  Total Analysis Points = ',I5,/)
C
 7400 FORMAT(/,
     $'                 Parametric Analysis Status',/,
     $'                 --------------------------',/,
     $22X,A32,/,
     $' ------------------------------------------------------------',/,
     $' Prop    Prop        Blade  Radius  BetaRad  Analysed  Failed',/,
     $'  No.    Name        Number   (m)     (m)     Points   Points',/,
     $' ------------------------------------------------------------')
C
 7420 FORMAT(2X,I2,2X,A15,2X,I1,4X,F5.3,4X,F5.3,5X,I4,5X,I3)
 7430 FORMAT(1X,60('-'))
C
 7440 FORMAT(1X,'Current propeller index: ',I2)
C
 7500 FORMAT(/,1X,'Analysis Point ',I4,' of ',I4,/,
     $1X,'Prop ',I2,8X,'Vel:',I4,' kts',3X,'RPM:',I5,5X,
     $'Pwr:',I4,' hp',4X,'Alt:',I6,' ft',/,
     $1X,'BAng=',F4.1,' deg',2X,'AdvR=',F5.3,5X,'Mach=',F4.2,5X,
     $'Thrst=',F5.1,' kg',/,
     $1X,'Stall%=',F6.1,2X,'EffIdl=',F5.3,3X,'EffInd=',F5.3,2X,
     $'Effy=',F5.3,/)
C
 7520 FORMAT(/22X,'*** NEGATIVE CONVERGENCE ***')
 7530 FORMAT(/24X,'*** NEGATIVE THRUST ***')
C
 7550 FORMAT(/,22X,'UNCONVERGED OPERATING POINT',/,
     $22X,'Analysis Point',I5,' of',I5,/,
     $1X,'Prop: ',I2,3X,'Velocity: ',I3,' kts',3X,'RPM: ',I4,4X,
     $'Pwr: ',I4,' hp',4X,'Alt: ',I5,' ft',/,
     $20X,F5.1,' m/s',17X,F8.1,' W ',/)
C
 7570 FORMAT(/,
     $6X,'==== Parametric Analysis Run Complete ====',/,
     $13X, I4,' points analysed',/,
     $13X, I4,' points unconverged',/,
     $13X, 'SAVE to write data to disk')
C
 7580 FORMAT(/,10X,'*** Analysis Run Abandoned ***')
C
 7600 FORMAT(
     $9X,'       Run Abandoned at:',/,
     $9X,'         Velocity = ',I2,/,
     $9X,'         RPM      = ',I2,/,
     $9X,'         Power    = ',I2,/,  
     $9X,'         Altitude = ',I2,//,
     $13X, I4,' points analysed',/,
     $13X, I4,' unconverged points',/)
C
C
      END    !ESPARA
C
C-------------------------------------------------------------------------
C----ESOPUT - Based on OUTPUT, by Drela
C    
      SUBROUTINE ESOPUT
     $         (LU,TDIM,EFFTOT,EFFIND,EIDEAL,REFDEG,MACH,STALPC,BETLOC)
C
      INCLUDE 'XROTOR.INC'
      INTEGER BETLOC
C
      TDIM = TTOT*RHO*VEL**2*RAD**2
      QDIM = QTOT*RHO*VEL**2*RAD**3
      PDIM = PTOT *RHO*VEL**3*RAD**2
      EFFTOT = TTOT/PTOT
      RPM = VEL/(RAD*ADV*PI/30.)
      DIA = 2.0*RAD
C
      TNACEL = (TWAK-TINV)*RHO*VEL**2*RAD**2
C
C---- standard coefficients based on forward speed
      TC = TDIM/(0.5*RHO*VEL**2 * PI*RAD**2)
      PC = PDIM/(0.5*RHO*VEL**3 * PI*RAD**2)
C
C---- induced efficiency (including nacelle thrust effect)
      EFFIND = TWAK/PWAK
C
C---- ideal (actuator disk) efficiency
      TCLIM = MAX( -1.0 , TC )
      EIDEAL = 2.0 / (1.0 + SQRT(TCLIM + 1.0))
C
C---- standard coefficients based on rotational speed
      EN = RPM/60.0
      CT = TDIM/(RHO*EN**2*DIA**4)
      CP = PDIM/(RHO*EN**3*DIA**5)
C
C----Reference blade angle
C
      REFDEG = BETA(BETLOC)*180./PI
C
C----Tip Mach Calculation
C
      CALL UVADD(XI(II),WA,WT)
C
        VT = VIND(3,II)
        VA = VIND(1,II)
        UTOT = URDUCT + UBODY(II)
C
        CI = XI(II)/ADV - WT  -  VT
        SI = UTOT      + WA  +  VA
        MACH = SQRT(SI*SI + CI*CI) * VEL/VSO
C
C---- find stalled region-percentage of blade
C
      DO I=1, II
         IF(STALL(I)) THEN
           ISTIN=I
           GO TO 12
         END IF
      ENDDO
C
      STALPC=0.0
      GO TO 30
C
 12   DO I=ISTIN+1, II
         IF(.NOT.STALL(I)) THEN
            ISTOUT = I-1
            GO TO 15
         END IF
      ENDDO
C
      STALPC=100.*(1.- XI(ISTIN))/(1.- XI(1))
      GO TO 20
C
 15   STALPC=100.*(((XI(ISTOUT)+XI(ISTOUT+1))/2.)-XI(ISTIN))/
     $ (1.- XI(1))
C
 20   IF(CL(ISTIN) .LE. 0.) THEN
         STALPC = -1.0 * STALPC
      END IF
C
 30   RETURN
      END  !ESOPUT
C---------------------------------------------------------------------------


C-----------------------------------------
C     Subroutine ESIMP
C     Reads in ESIMP format blade geometry file
C     Based on ARBI by Drela
C-----------------------------------------

      SUBROUTINE ESIMP
      INCLUDE 'XROTOR.INC'
C
      LOGICAL LCOR
      CHARACTER ESNAME*32
      REAL RADMET(30),CDMET(30)
      INTEGER ESUNIT
C
      ESUNIT=72     ! ESOTEC File IO unit
C
      GREEK = .FALSE.
      CONV = .FALSE.
C
 100  CALL ASKS(' Enter ESIMP geometry filename (A to abort)^',ESNAME)
C
      IF(ESNAME .EQ. 'A' .OR. ESNAME .EQ. 'a') GO TO 900
C
      OPEN(UNIT=ESUNIT,FILE=ESNAME,STATUS='OLD',IOSTAT=KODE)
C
         IF(KODE.NE.0) THEN
            WRITE(*,*)'File cannot be opened'
            GO TO 100
         END IF
C
C----Read in geometry data
C
      READ(UNIT=ESUNIT,FMT=2200,ERR=800) NAME
      READ(UNIT=ESUNIT,FMT=2240,ERR=800) RAD,ROOT,NST
C
      IF(NST.LT.3) THEN
         WRITE(*,*) 'Must have 3 or more stations. File not read'
         CLOSE(UNIT=ESUNIT)
         GO TO 100
      END IF
C
      IF(RAD.LE.ROOT) THEN
         WRITE(*,*) 'Tip radius smaller than root. File not read'
         CLOSE(UNIT=ESUNIT)
         GO TO 100
      END IF
C
      XI0 = ROOT/RAD
      XW0 = ROOT/RAD
      CALL SETX
      CALL XWINIT
C
      DO 20 N=1, NST 
C  
        READ (UNIT=ESUNIT,FMT=2260,ERR=800) RADMET(N),CDMET(N),W3(N)
C
        W1(N)=RADMET(N)/RAD
        W2(N)=CDMET(N)/RAD
C
        IF(W1(N).LT.XI0 .OR. W1(N).GT.XITIP) THEN
         WRITE(*,*) 'Radii must be between ',XI0,' and  1.0. Not read'
         GO TO 100
        ENDIF
C
        IF(N.GT.1 .AND. W1(N).LE.W1(N-1)) THEN
         WRITE(*,*) 'Radii must monotonically increase. File not read'
         GO TO 100
        ENDIF
C
   20 CONTINUE
C
      CLOSE(UNIT=ESUNIT)
C
      CALL ASKI(' Enter number of blades   ^',NBLDS)
      CALL ASKR(' Enter flight speed (m/s) ^',VEL)
C
C----End of data input
C
   60 DO 70 N=1, NST
        W3(N) = W3(N)*PI/180.
        W1(N) = TINVRT(W1(N))
   70 CONTINUE
C
      CALL SPLINE(W2,W4,W1,NST)
      CALL SPLINE(W3,W5,W1,NST)
C
      DO 90 I=1, II
        CH(I)   = SEVAL(T(I),W2,W4,W1,NST)
        BETA(I) = SEVAL(T(I),W3,W5,W1,NST)
        BETA0(I) = BETA(I)
        CH(I) = ABS(CH(I))                   ! negative chords are a no-no
        UBODY(I) = 0.
   90 CONTINUE
C
C---- estimate reasonable advance ratio to start iterative routines
C
      IS = II/2 + 1
CHHY A0 undefined, use 0.0 for now
      A0 = 0.0
      ANG = BETA(IS) - A0
      ADV = XI(IS)*SIN(ANG)/COS(ANG)
      ADV = MAX(0.1,ADV)
      ADW = ADV * URDUCT
C
      WRITE(*,2500) NAME
      GO TO 900
C
 800  WRITE(*,*)'File read error. Aborted'
      CLOSE(UNIT=ESUNIT)
C
 900  RETURN
C
C----Format Statements-----------------------------------------------
C
 2200 FORMAT(1X,A30)
 2240 FORMAT(1X,F12.6,F12.6,I4)
 2260 FORMAT(1X,F12.6,F12.6,F12.6)
C
 2500 FORMAT(/,
     $1X,43('='),/,
     $' New rotor geometry created from ESIMP file.',/,
     $' Current rotor name is ',A30,/,
     $1X,43('='))
C
      END ! ESIMP



C------------------------------------------------------------------------
C    SUBROUTINE PARAM
C    Prompt for parameter bounds
C    Read string from keyboard and convert to integers
C    Confirm integers are bounded by 1 and NPAR
C    Confirm second integer .GE. first integer
C    Single integer sets both bounds
C    0 returns integers 1  and NPAR
C    Returns lower bound in IPARL
C    Returns upper bound in IPARU
C------------------------------------------------------------------------
C
      SUBROUTINE PARAM(PROMPT,NPAR,IPARU,IPARL)
C
      CHARACTER PROMPT*132
      CHARACTER ASKARG*132
      LOGICAL ERROR
      DIMENSION IINPUT(20)

 210  CALL ASKS(PROMPT,ASKARG)
C
      DO I=1, 20
        IINPUT(I) = 0
      ENDDO
      NINPUT = 2
      CALL GETINT(ASKARG,IINPUT,NINPUT,ERROR)
C
      IF(ERROR .OR. NINPUT .EQ. 0) THEN
         WRITE(*,*)'Data input error'
         GO TO 210
      END IF
C
      IF(IINPUT(1) .EQ. 0) THEN
         IPARL=1
         IPARU=NPAR
         GO TO 220
      END IF
C
      IF(NINPUT .EQ. 1) THEN
         IPARL=IINPUT(1)
         IPARU=IINPUT(1)
         GO TO 215
      ELSE
         IPARL=IINPUT(1)
         IPARU=IINPUT(2)
         GO TO 215
      END IF
C
 215  IF(IPARL .LT. 1 .OR. IPARU .GT. NPAR) THEN
         WRITE(*,*)'Range outside database parameters'
         GO TO 210
      END IF
C
      IF(IPARU .LT. IPARL) THEN
         WRITE(*,*)'Upper bound smaller than lower bound'
         GO TO 210
      END IF
C
 220  RETURN
      END   ! PARAM



C----------------------------------------------------------------------
C----Subroutine ESPVAL
C----Prompt for and read in parameter values
C----------------------------------------------------------------------
      SUBROUTINE ESPVAL(PROMPT,NVALUE,NDIM,NARRAY,IVALUE)
C
      CHARACTER PROMPT*132
      CHARACTER ASKARG*132
      LOGICAL ERROR
      DIMENSION IVALIN(NVALUE)
      DIMENSION IVALUE(NARRAY)
C
 210  CALL ASKS(PROMPT,ASKARG)
C
      DO I=1, NVALUE
        IVALIN(I) = 0
      ENDDO
      NINPUT = NVALUE
C
      CALL GETINT(ASKARG,IVALIN,NINPUT,ERROR)
C
      IF(ERROR) THEN
         WRITE(*,*)'Data input error'
         GO TO 210
      END IF
C
      IF(NINPUT .GT. NARRAY) THEN
         WRITE(*,*)'Too many values: max = ',NARRAY
         GO TO 210
      END IF
C
      DO I=2,NINPUT
         IF(IVALIN(I) .LE. IVALIN(I-1)) THEN
           WRITE(*,*)'Values must increase monotonically'
           GO TO 210
         END IF
      ENDDO
C
      NDIM=NINPUT
C
      DO I=1,NDIM
         IVALUE(I)=IVALIN(I)
      ENDDO
C
      RETURN
      END    !ESPVAL
C
C----------------------------------------------------------------------


      SUBROUTINE RDLINE2(LUN,LINE,ICNT)
C...Purpose  Read a non-comment line from the input file 
C
C...Input    Data read from unit LUN
C...Output   LINE  Character string with input line
C                  LINE is set to 'END' for end or errors
C
C   Comment lines are assumed to start with ! or #
C
      CHARACTER*(*) LINE
      LOGICAL LECHO
      DATA LECHO / .FALSE. /
C
 1000 FORMAT(A)
 1010 FORMAT(I4,1X,A)
C
   20 ICNT = ICNT + 1
      READ (LUN,1000,END=80,ERR=90) LINE
      IF(LECHO) WRITE(*,1010) ICNT,LINE(1:60)
C
C---- skip comment line
      IF(INDEX('!#',LINE(1:1)) .NE. 0) GO TO 20
C
C---- skip blank line
      IF(LINE.EQ.' ') GO TO 20
C
C---- normal return after significant line
      RETURN
C
   80 LINE = 'END '
      RETURN
C
   90 LINE = 'ERR '
      RETURN
      END

