PROGRAM pade
IMPLICIT NONE

REAL(kind=8),external::Sna

REAL(kind=8)::Senox,y,x,b,ErrorR
INTEGER::j

b=0

OPEN (1,FILE="pade.dat")
 DO j=-31415926,31415926,1000
  x=j*0.0000001
  Senox=Sin(x)
  WRITE(1,*) x,Senox,b
END DO

WRITE(1,*) ' '
b=1

DO j=-31415926,31415926,1000
 	   
x=j*0.0000001
y=Sna(x)
WRITE(1,*) x,y,b
END DO
CLOSE (1)
 
OPEN (2,FILE="Error.dat")
DO j=0,31415926,1000

 x=j*0.0000001
 Senox=Sin(x)
 y=Sna(x)
 
ErrorR=Senox-(y/Senox)
 PRINT*, x, ErrorR
 WRITE(2,*) x,ErrorR
END DO
 
CLOSE (2)
	
END PROGRAM pade
	
FUNCTION Sna(x)

IMPLICIT NONE
!Declaro los valores que van de entrada y de salida
REAL(kind=8),intent(in)::x
REAL(kind=8)::Sna,m,n

!Descompongo el valor estimado del seno para que sea mas facil leelo para el program
m=x-(x**3)*(2363.0/18183.0)+(x**5)*(12671.0/4363920.0)

n=1+(x**2)*(445.0/12122.0)+(x**4)*(601.0/872784.0)+(x**6)*(121.0/16662240.0)

Sna=m/n

END FUNCTION Sna
