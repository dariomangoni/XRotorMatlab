C***********************************************************************
C    Module:  modify.f
C 
C    Copyright (C) 2011 Mark Drela 
C 
C    This program is free software; you can redistribute it and/or modify
C    it under the terms of the GNU General Public License as published by
C    the Free Software Foundation; either version 2 of the License, or
C    (at your option) any later version.
C
C    This program is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU General Public License for more details.
C
C    You should have received a copy of the GNU General Public License
C    along with this program; if not, write to the Free Software
C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C***********************************************************************

      SUBROUTINE CRSMOD(N,X,Y,YP,
     &                  XOFF,XSF,YOFF,YSF, SSIZ, NSPLT,
     &                  LSLOPE, IMOD1,IMOD2 )
      DIMENSION X(N),Y(N),YP(N)
      LOGICAL LSLOPE
C--------------------------------------------------
C     Modifies current y(x) from cursor input.
C
C     The extent of the modification is returned 
C     in the two indices:    IMOD1 < i < IMOD2
C
C--------------------------------------------------
      LOGICAL LABORT, LZOOM, LUNZOOM
      CHARACTER*1 KCHAR
C
      LOGICAL LSLOP1, LSLOP2
C
      PARAMETER (NWX=100)
      DIMENSION W1(NWX), W2(NWX), W3(NWX)
C
      WRITE(*,*) 'Funzione disattivata TOTALMENTE'
      RETURN
      END



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
