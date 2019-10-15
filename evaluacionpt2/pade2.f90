PROGRAM pade2
IMPLICIT NONE

REAL(kind=8),external::ExpPx,ExpPz,ExpPc !cambie j por n ahora por c
REAL(kind=8)::ExpPaX,y,x,Error,z,c!cambiar j por n?
INTEGER::i

 OPEN (1,FILE="padeexp2.dat")
  DO i=-31415926,31415926,1000
    x=i*0.0000001
     ExpPaX=Exp(x)
     y=ExpPx(x)
      Error=ExpPaX-(y/ExpPaX)
	Print*, x, Error
       WRITE(1,*) x,Error
  END DO
 CLOSE (1)

 OPEN (2,FILE="padeexp11.dat")
  DO i=-31415926,31415926,1000
    z=i*0.0000001
     ExpPaX=Exp(z)
     y=ExpPz(z)
      Error=ExpPaX-(y/ExpPaX)
	
       WRITE(2,*) z,Error
  END DO
 CLOSE (2)

 OPEN (3,FILE="padeexp20.dat")
  DO i=-31415926,31415926,1000
    z=i*0.0000001
     ExpPaX=Exp(c)
     y=ExpPc(c)
      Error=ExpPaX-(y/ExpPaX)
       
 WRITE(3,*) c,Error
  END DO
 CLOSE (3)

 
END PROGRAM pade2
!------------------------------------------------
FUNCTION ExpPx(x)
IMPLICIT NONE

REAL(kind=8),intent(in)::x
REAL(kind=8)::ExpPx,m,n

m=1.0

n=1-x+(x**2)*(1.0/2.0)

ExpPx=m/n

END FUNCTION ExpPx
!-----------------------------------------------------

FUNCTION ExpPz(z)

IMPLICIT NONE

REAL(kind=8),intent(in)::z
REAL(kind=8)::ExpPz,m,n

m=1+z*(1.0/2.0)

n=1-z*(1.0/2.0)

ExpPz=m/n

END FUNCTION ExpPz
!------------------------------------------------------

FUNCTION ExpPc(c)
 IMPLICIT NONE

REAL(kind=8),intent(in)::c
REAL(kind=8)::ExpPc,m,n

m=1+c+(c**2)*(1.0/2.0)

n=1.0

ExpPc=m/n

END FUNCTION ExpPc
