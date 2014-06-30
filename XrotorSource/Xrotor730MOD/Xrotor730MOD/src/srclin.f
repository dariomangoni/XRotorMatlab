
      SUBROUTINE SRCLIN(NB,ZB,RB, SRC)
      DIMENSION ZB(*), RB(*), SRC(*)
C------------------------------------------------------------
C     Calculates point-source strengths required to represent 
C     an axisymmetric body shape ZB(i),RB(i), i = 1..NB,
C     immersed in a unity freestream flow.
C
C     The NB-1 sources are placed at the midpoints of the 
C     ZB intervals.  The body is assumed to extend upstream
C     with radius RB(1), and downstream with radius RB(NB).
C------------------------------------------------------------
      PARAMETER (NMAX=200)
      DIMENSION A(NMAX,NMAX)
C
      IF(NB.GT.NMAX) STOP 'SRCLIN: Local array overflow. Increase NMAX.'
C
      PI = 4.0*ATAN(1.0)
C
      N = NB - 1
C
      DO I=1, N
        RC = (RB(I+1) + RB(I))*0.5
        ZC = (ZB(I+1) + ZB(I))*0.5
        DR =  RB(I+1) - RB(I)
        DZ =  ZB(I+1) - ZB(I)
C
        DO J=1, N
          RS = (RB(J+1) + RB(J))*0.5
          ZS = (ZB(J+1) + ZB(J))*0.5
C
          RCS = SQRT((ZC-ZS)**2 + RC**2)
C
          U = (ZC-ZS) / RCS**3
          V =  RC     / RCS**3
          A(I,J) = U*DR - V*DZ
        ENDDO
C
        SRC(I) = -4.0*PI * DR
C
      ENDDO
C
      CALL GAUSS(NMAX,N,A,SRC,1)
C
      RETURN
      END



      SUBROUTINE SRCVEL(NB,ZB,SRC, Z,R,U,V)
      DIMENSION ZB(*), SRC(*)
C------------------------------------------------------------
C     Determines the z,r velocity components U,V 
C     at some location Z,R, due to points sources 
C     of strength SRC along the z axis.  
C
C     The NB-1 sources are placed at the midpoints 
C     of the ZB intervals. 
C
C------------------------------------------------------------
      PI = 4.0*ATAN(1.0)
C
      N = NB - 1
C
      USUM = 0.0
      VSUM = 0.0
      DO I=1, N
        ZS = (ZB(I+1) + ZB(I))*0.5
C
        RS = SQRT((Z-ZS)**2 + R**2)
C
        USUM = USUM + SRC(I) * (Z-ZS) / RS**3
        VSUM = VSUM + SRC(I) *  R     / RS**3
      ENDDO
C
      U = USUM / (4.0*PI)
      V = VSUM / (4.0*PI)
C
      RETURN
      END



