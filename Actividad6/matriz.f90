SUBROUTINE Matriz(A, aw, h, g, l, M)
IMPLICIT NONE

REAL,INTENT(in):: A,aw,h,g,l
REAL,DIMENSION(2),intent(out):: M
REAL:: ap, w, a2, w2
REAL,DIMENSION(2):: p1
REAL,DIMENSION(2):: p2
!Aclarando y definiendo variables dandole valores 
 ap = A
 W = aw
 a2 = h*w
 w2 = -h * g / l *a
 p1 = (/a, w/)
 p2 = (/a2, w2/)

 M = p1 + p2

end subroutine Matriz
