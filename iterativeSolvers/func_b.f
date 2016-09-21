C        Generated by TAPENADE     (INRIA, Tropics team)
C  Tapenade 3.9 (r5096) - 24 Feb 2014 16:53
C
C  Differentiation of func in reverse (adjoint) mode:
C   gradient     of useful results: y
C   with respect to varying inputs: y b
C   RW status of diff variables: y:in-zero b:out
C
      SUBROUTINE FUNC_B(a, b, bb, xold, y, yb)
      IMPLICIT NONE
      DOUBLE PRECISION a(4, 4), b(4), xold(4), y, xnew(4), temp, ep
      DOUBLE PRECISION bb(4), xoldb(4), yb, xnewb(4), tempb
      INTEGER it, i, j, n, flag, z
      INTRINSIC ABS
      INTEGER branch
      INTEGER ad_count
      INTEGER i0
      DOUBLE PRECISION tempb0
      DOUBLE PRECISION abs0
      INTEGER ii1
      ep = 1e-15
C	  A = transpose(reshape((A),shape(A)))
      temp = 0
      flag = 1
      ad_count = 0
      DO WHILE (flag .EQ. 1)
C	    print *,z
C            z=z+1
        DO i=1,4
          DO j=1,4
            IF (i .NE. j) THEN
              temp = temp + a(i, j)*xold(j)
              CALL PUSHCONTROL1B(1)
            ELSE
              CALL PUSHCONTROL1B(0)
            END IF
          ENDDO
          xnew(i) = 1.0/a(i, i)*(b(i)-temp)
          temp = 0
        ENDDO
C            print *,xNew(i)
C		If the difference between the all values of xNew(i) and xOld(i) is less than or equal to ep then flag is set to 0.		
        DO j=1,4
          IF (xnew(j) - xold(j) .GE. 0.) THEN
            abs0 = xnew(j) - xold(j)
          ELSE
            abs0 = -(xnew(j)-xold(j))
          END IF
          IF (abs0 .LE. ep) THEN
            flag = 0
          ELSE
            GOTO 100
          END IF
        ENDDO
        GOTO 110
 100    flag = 1
 110    DO j=1,4
          xold(j) = xnew(j)
        ENDDO
        ad_count = ad_count + 1
      ENDDO
      CALL PUSHINTEGER4(ad_count)
      CALL OBJ_B(xold, xoldb, y, yb)
      DO ii1=1,4
        bb(ii1) = 0.D0
      ENDDO
      DO ii1=1,4
        xnewb(ii1) = 0.D0
      ENDDO
      CALL POPINTEGER4(ad_count)
      DO i0=1,ad_count
        DO j=4,1,-1
          xnewb(j) = xnewb(j) + xoldb(j)
          xoldb(j) = 0.D0
        ENDDO
        DO i=4,1,-1
          tempb0 = xnewb(i)/a(i, i)
          bb(i) = bb(i) + tempb0
          tempb = -tempb0
          xnewb(i) = 0.D0
          DO j=4,1,-1
            CALL POPCONTROL1B(branch)
            IF (branch .NE. 0) xoldb(j) = xoldb(j) + a(i, j)*tempb
          ENDDO
        ENDDO
      ENDDO
      yb = 0.D0
      END
