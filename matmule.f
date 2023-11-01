      PROGRAM MATRIXMUL
      IMPLICIT NONE

      INTEGER I, J, K, L
      REAL A(4,3), B(3,5), C(5,8), TEMP(4,5), D(4,8)

      ! Initializing matrices A, B, and C with some values.
      ! For simplicity, just using indices as values.
      DO I = 1, 4
         DO J = 1, 3
            A(I,J) = I + J
         ENDDO
      ENDDO

      DO I = 1, 3
         DO J = 1, 5
            B(I,J) = I * J
         ENDDO
      ENDDO

      DO I = 1, 5
         DO J = 1, 8
            C(I,J) = I - J
         ENDDO
      ENDDO

      ! Computing matrix TEMP = A * B
      DO I = 1, 4
         DO J = 1, 5
            TEMP(I,J) = 0.0
            DO K = 1, 3
               TEMP(I,J) = TEMP(I,J) + A(I,K) * B(K,J)
            ENDDO
         ENDDO
      ENDDO

      ! Computing matrix D = TEMP * C
      DO I = 1, 4
         DO J = 1, 8
            D(I,J) = 0.0
            DO K = 1, 5
               D(I,J) = D(I,J) + TEMP(I,K) * C(K,J)
            ENDDO
         ENDDO
      ENDDO

      ! Printing matrices A, B, C, and D
      WRITE(*,*) 'Matrix A:'
      DO I = 1, 4
         WRITE(*,*) (A(I,J), J = 1, 3)
      ENDDO
      WRITE(*,*)

      WRITE(*,*) 'Matrix B:'
      DO I = 1, 3
         WRITE(*,*) (B(I,J), J = 1, 5)
      ENDDO
      WRITE(*,*)

      WRITE(*,*) 'Matrix C:'
      DO I = 1, 5
         WRITE(*,*) (C(I,J), J = 1, 8)
      ENDDO
      WRITE(*,*)

      WRITE(*,*) 'Matrix D:'
      DO I = 1, 4
         WRITE(*,*) (D(I,J), J = 1, 8)
      ENDDO

      END PROGRAM MATRIXMUL
