C        Generated by TAPENADE     (INRIA, Tropics team)
C  Tapenade 3.9 (r5096) - 24 Feb 2014 16:53
C
C  Differentiation of obj in reverse (adjoint) mode:
C   gradient     of useful results: y
C   with respect to varying inputs: xold
C
      SUBROUTINE OBJ_B(xold, xoldb, y, yb)
      IMPLICIT NONE
      DOUBLE PRECISION xold(4), y
      DOUBLE PRECISION xoldb(4), yb
      INTRINSIC DLOG
      INTRINSIC EXP
      INTEGER ii1
      DO ii1=1,4
        xoldb(ii1) = 0.D0
      ENDDO
      xoldb(1) = xoldb(1) + 3*xold(1)**2*yb
      xoldb(2) = xoldb(2) + 2*yb/xold(2)
      xoldb(3) = xoldb(3) + EXP(xold(3))*yb
      xoldb(4) = xoldb(4) + 4*yb
      END
