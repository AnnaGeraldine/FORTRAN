PROGRAM resistencia
IMPLICIT NONE
!****************************************************************
!Programa que calcule el lanzamiento de proyectiles pero con
!resistencia al aire.
!****************************************************************

!Declaramos constantes
REAL,PARAMETER::g=-9.8				
REAL,PARAMETER::pi=3.14159265359

!Definimos variables
  REAL ::t,Sx,Sy,m,Vox,Voy,dt,x,y
  REAL::Vt,Vx,Vy,Vo,k,a,d,ymax
 
!Variables integer
 INTEGER::n,npasos

  x=0
  y=0
  t=0
  dt=0.1

  !Pedir las variables de la pelota
  PRINT*,"Dame la masa de la pelota"
  READ*,m
  
  PRINT*,"Dame la velocidad inicial de la pelota"
  READ*, Vo
  
  PRINT*,"Dame la velocidad final de la pelota" 
  READ*,Vt
  
  PRINT*,"Dame el angulo de la pelota"
  READ*,a
 !numero depasos que quieres dar
  PRINT*,"Dame el numero de pasos que quieres dar en la parabola"
  READ*,npasos
  
  k=(m*g)/Vt
	
 OPEN(unit=11,FILE="CORDENADAS.dat",STATUS="unknown")
  !convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
   Vox=Vo*Cos(a)
   Voy=Vo*Sin(a)
 
DO n=0,npasos
   Sx=(m/k)*Vox*(1.0-exp((-k/m)*t))

   Sy=((-m*g)/k)*t-(m/k)*(Voy+(m*g/k))*(1.0-exp(-k/m)*t)

   Vx=Vox*exp((-k/m)*t)

   Vy=((-m*g)/k)+(Voy+(m*g/k))*exp(-k/m)*t

   t=t+dt
 
     IF(Vy>Vt)THEN
      Vy=Vt
    END IF
    IF(Vx>Vt)THEN
     Vx=Vt
    ENDIF

  PRINT*,"x",Sx, "y",Sy
  WRITE(11,*)Sx,Sy
  
END DO
 WRITE(11,*)"#"

 ymax=(Sy/2.0)
PRINT*,"La altura maxima en y es de",ymax
 
 d=(Vo**2.0/g)*Sin(2.0*a)
 PRINT*,"La altura maxima es de:",d

 
  CLOSE(11)


END PROGRAM resistencia
