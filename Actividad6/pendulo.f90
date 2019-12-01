PROGRAM pendulo
  implicit none
!----------------------------------------------------------------------------------------------------------------
!Con el programa analizaremos un fenomeno fisico de un pendulo por lo cual necesitaermos resolver su ecuacion.
!Lo resolveremos por el metodo de Euler
!----------------------------------------------------------------------------------------------------------------
  
!Variables que usaremos en el programa
REAL:: A, h, aw, t, y, B
INTEGER:: j
REAL,DIMENSION(2):: M
REAL,PARAMETER:: l = 9.81
REAL,PARAMETER:: g = 9.81
  
!calculamos(es partede la formula)
   aw = sqrt(g/l)	
 
!Le pedimos al usuario que introduza angulo y pasas a dar por el pendulo,asi lo podra hacer para cada pendulo que quiera
PRINT*," Favor de introducir el angulo y tamaño de pasos"
READ(*,*) A, h
 
!Calculamos el movimiento del pendulo dada la funcion 
OPEN(1, FILE="Pendulo.dat", STATUS="unknown")
DO j=0,7000
    t=float(j)* h
 IF(t>6.3) EXIT
    y=A*cos(aw*t)       
     PRINT*, t, y
     WRITE(1,*) t,y,1
   END DO

  WRITE(1,*) " "
  B = A 
   DO j=0,9000
    t=float(j)*h
    IF(t>6.3) EXIT
   CALL Matriz(A, aw, h, l, g, M)
     WRITE(1,*) t, M(1), 2
    A = M(1)
    aw = M(2)
   END DO

  CLOSE(1)

!Calculamos el error relativo
PRINT*, "Error", abs((B-A)/B)
 END PROGRAM pendulo
