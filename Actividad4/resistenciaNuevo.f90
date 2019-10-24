PROGRAM resistencia
IMPLICIT NONE
!****************************************************************
!Programa que calcule el lanzamiento de proyectiles pero con
!resistencia al aire.
!****************************************************************

!Declaramos constantes
REAL,PARAMETER::g=9.81				
REAL,PARAMETER::pi=3.14159265359

!Definimos variables
  REAL ::t,Sx,Sy,m,Vox,Voy,dt,x,y
  REAL::Vt,Vx,Vy,Vo,k,a,d,ymax
 
!Variables integer
 INTEGER::n,npasos

  !Pedir las variables de la pelota
  PRINT*,"Dame la masa de la pelota"
  READ*,m
  
  PRINT*,"Dame la velocidad inicial de la pelota"
  READ*, Vo
  
  PRINT*,"Dame la velocidad terminal de la pelota" 
  READ*,Vt
  
  PRINT*,"Dame el angulo de la pelota"
  READ*,a
 !numero depasos que quieres dar
  PRINT*,"Dame el numero de pasos que quieres dar en la parabola"
  READ*,npasos
  
  k=m*g/Vt
  write(*,*) 'k= ', k
	
 OPEN(unit=11,FILE="CORDENADAS.dat",STATUS="unknown")
  !convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
   Vox=Vo*cos(a)
   Voy=Vo*sin(a)
!write(*,*) 'Vox, Voy: ', Vox, Voy

!  x=0
!  y=0
 
  t=0.0
  dt=0.05


! Lanzado desde el orígen 
   Sx=0.0
   Sy=0.0

DO n=0,npasos

   Sx=(m/k)*Vox*(1.0-exp(-k*t/m))
   Sy=(-m*g/k)*t+(m/k)*(Voy+m*g/k)*(1.0-exp(-k*t/m))

   Vx=Vox*exp(-k*t/m)
   Vy=-(m*g/k)+(Voy+m*g/k)*exp(-k*t/m)

   t=t+dt
 

! La altura máxima sera cuando la velocidad Vy llegue casi a cero   
    If (abs(Vy) < 0.5) then
       ymax = Sy
       write(*,*) 'Altura maxima ymax: ', ymax
    endif

  PRINT*,t, Sx, Sy, Vx, Vy
  WRITE(11,*)Sx,Sy

! Si la pelota llega al suelo, detener el DO LOOP
  if (Sy < 0 ) then
     exit 
    endif

END DO
 WRITE(11,*)"#"

 
! d=(Vo**2.0/g)*Sin(2.0*a)
! PRINT*,"La altura maxima es de:",d

 
  CLOSE(11)


END PROGRAM resistencia
