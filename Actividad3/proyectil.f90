PROGRAM Proyectil
IMPLICIT NONE
!**********************************************************************
!El programa calculara el tiempo total y la velocidad inicial
!del proyectil
!***********************************************************************
  ! definimos constantes
  REAL, PARAMETER :: g = 9.8
  REAL, PARAMETER :: pi = 3.1415927
  REAL,PARAMETER::dt=0.1

  ! definimos las variables
  REAL ::t, u, x, y
  REAL,DIMENSION(6)::a

 !Variables integer
 INTEGER::i,j,m,n,npasos

a=(/15,30,45,60,75,90/)


  !Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  PRINT*,'El tiempo y la rapidez inicial'
  READ*, t, u
 
  !convirtiendo ángulo a radianes
  a = a * pi / 180.0

 !Numero de puntos que vas a querer en la grafica
 PRINT*,"Dame el numero de pasos que quieres dar en la parabola"
 READ*,npasos

  OPEN(UNIT=11,FILE="PUNTOS",STATUS="UNKNOWN")
!(1,FILE="PUNTOS",ACCESS="APPEND")
  DO j=1,6
  !ang=float(m)*dtang
  DO i=0,npasos
   t=float(i)*dt
   !las ecuaciones de la posición en x y y
   x = u * cos(a(j)) * t
   y = u * sin(a(j)) * t - 0.5 * g * t * t
   PRINT*, x,y
  IF(y<0.0)EXIT
 WRITE(11,*)x,y
 END DO
WRITE(11,*)" "
WRITE(11,*)"#"
WRITE(11,*)" "
END DO
STOP
CLOSE(11) 

 END PROGRAM Proyectil
