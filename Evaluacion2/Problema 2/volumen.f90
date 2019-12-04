PROGRAM Triangle
 IMPLICIT NONE
  REAL :: a, b, c, Area,Volumen                                     
  PRINT *, 'Ingresa el valor de los lados del triangulo'
  READ *, a,b,c
  PRINT *, "Area del triangulo", Area(a,b,c)
  PRINT*,"Volumen del paralelepipedo" ,Volumen(a,b,c)
END PROGRAM Triangle
!-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
FUNCTION Area(x,y,z)
 IMPLICIT NONE
  
   REAL :: Area            
   REAL, INTENT( IN ) :: x,y,z
   REAL :: theta, height
   theta = ACOS(((x**2+y**2-z**2)/(2.0*x*y)))
   height = x*SIN(theta); Area = 0.5*y*height
END FUNCTION Area
!---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
FUNCTION Volumen(x,y,z)
 IMPLICIT NONE

  REAL::Volumen 
  REAL,INTENT(IN)::x,y,z

  Volumen=x*y*z
END FUNCTION Volumen 
!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
