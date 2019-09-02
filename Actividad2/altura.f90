PROGRAM altura_max
  !****************************************
  !Con el programa calcularemos la altura maxima
  !que logra alcanzar un proyectil 
  !********************************************


 !Definimos las constantes
  REAL,PARAMETER::g=9.8,pi=3.14159265359
  !g=gravedad
  !pi

  !Definimos variables
  REAL::h,Vo,a
  !h=altura
  !Vo=Velocidad inocial
  !a=angulo
  
!Pedimos el valor de las variables
  PRINT*,"Hola usuario"

  PRINT*,"Dame el valor de la velocidada inicial"
  READ*,Vo

  PRINT*,"Dame el valor de la angulo del proyectil"
  READ*,a

  !Convertimos "a" a radianes
  a=a*pi/180.0

  !Calculamos la altura
  h=(Vo**2.0)*(sin(a)**2.0)/(2.0*g)

  PRINT*,"la altura maxima del proyectil es",h
  
END PROGRAM altura_max

  
