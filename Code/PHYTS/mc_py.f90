


FUNCTION PI()
IMPLICIT NONE
integer :: count, n, i
real :: r, x, y, PI
count = 0
n = 1000000000
CALL RANDOM_SEED

DO i = 1, n
   CALL RANDOM_NUMBER(x)
   CALL RANDOM_NUMBER(y)
   IF (x*x + y*y <1.0) count = count + 1

   IF (MODULO(i, 100) .EQ. 0) THEN
      r = 4 * REAL(count)/i
   END IF
END DO
PI = r
END FUNCTION
