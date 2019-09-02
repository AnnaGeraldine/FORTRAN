PROGRAM tiempo
  IMPLICIT NONE
  !Definimos constantes
  REAL, PARAMETER:: g=9.8,pi=3.14159265359

  !Definimos variables
  REAL::Vo,a,t
  !vo=Velocidad inicial
  !a=angulo
  !t=Tiempo

  PRINT*,"Dame el angulo"
  READ*,a
  PRINT*,"Dame la velociodad inicial"
  READ*,Vo
  
  a=a*pi/180.0
  
  t=(2.0*Vo)*(sin(a))/g

  PRINT*,t
  END PROGRAM

  
