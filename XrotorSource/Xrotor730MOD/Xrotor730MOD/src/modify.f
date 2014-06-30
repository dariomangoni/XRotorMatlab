
  


      SUBROUTINE SORT(NW,S,W)
      IMPLICIT REAL (A-H,O-Z)
      DIMENSION S(NW), W(NW)
      LOGICAL DONE
C
C---- sort arrays
      DO 10 IPASS=1, 500
        DONE = .TRUE.
        DO 101 N=1, NW-1
          NP = N+1
          IF(S(NP).GE.S(N)) GO TO 101
           TEMP = S(NP)
           S(NP) = S(N)
           S(N) = TEMP
           TEMP = W(NP)
           W(NP) = W(N)
           W(N) = TEMP
           DONE = .FALSE.
  101   CONTINUE
        IF(DONE) GO TO 11
   10 CONTINUE
      STOP 'SORT failed'
C
C---- search for duplicate pairs and eliminate each one
   11 NWS = NW
      DO 20 N=1, NWS
        IF(N.GE.NW) RETURN
        IF(S(N).NE.S(N+1)) GO TO 20
C------- eliminate pair
         NW = NW-2
         DO 201 NT=N, NW
           S(NT) = S(NT+2)
           W(NT) = W(NT+2)
  201    CONTINUE
   20 CONTINUE
C
      RETURN
      END ! SORT
