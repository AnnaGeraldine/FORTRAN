FUNCTION Area(a,b,c)
IMPLICIT NONE

REAL:: Area ! function type
REAL, INTENT( IN ) :: a,b,c
!Variable local
REAL::s 

s=(a+b+c)/2.0
Area=sqrt(s*(s-a)*(s-b)*(s-c))   
    
END FUNCTION Area
