PROGRAM Distancia
  IMPLICIT NONE

  !********************************************
  !Con este programa calcularemos la distancia
  !que alcanza un proyectil con una trayectoria
  !parabolica
  !*********************************************
  !Declaro las constantes
  REAL,PARAMETER::g=9.8, pi=3.14159265359

  !Declaro las variables
  REAL::Vo,d,a,h,t
  !d=distancia
  !v=velocidad inicial
  !a=angulo
  !t=tiempo de vuelo
  !altura

  !Le pediremos al usuario la velocidad,tiempo yangulo
  PRINT*,"Dame el valor de la velocidad inicial"
  READ*,Vo
  PRINT*,"Dame el valor de el tiempo total"
  READ*,t
  PRINT*,"Dame el valor de el angulo"
  READ*,a

  !Convertimos el angulo a radianes 
  a=a*pi/180.0
  
  !Calculamos la altura
  d=Vo*t*Cos(a)
  PRINT*,"La distancia maxima es",d

END PROGRAM Distancia
