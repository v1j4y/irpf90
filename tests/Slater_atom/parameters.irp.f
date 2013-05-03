! -*- F90 -*-

BEGIN_PROVIDER [real*8, xnqeff, (7) ]
BEGIN_DOC
! the effective n in the Slater model
END_DOC
      implicit none
      integer :: i
      XNQEFF(1)=1.D0
      XNQEFF(2)=2.D0
      XNQEFF(3)=3.D0
      XNQEFF(4)=3.7D0
      XNQEFF(5)=4.0D0
      XNQEFF(6)=4.2D0
      XNQEFF(7)=4.3D0

      WRITE(6,*)
END_PROVIDER

 BEGIN_PROVIDER [real*8, z30]
&BEGIN_PROVIDER [real*8, z35]
&BEGIN_PROVIDER [real*8, z85]
BEGIN_DOC
! the three parameters for the screening constants in the model
END_DOC
! parametrization
      Z30=0.31D0
      Z35=0.35D0
      Z85=0.85D0
      WRITE(6,*) ' the Slater model is used with the following constants '
      WRITE(6,*)
      WRITE(6,9901) Z30,Z35,Z85
 9901  format('    1s - 1s: ',F6.2,'  nl - nl: ',F6.2,'  nl - (n-1)l'': ',F6.2,/)
      WRITE(6,*) '   effective n: '
      WRITE(6,9821) (XNQEFF(I),I=1,7)
9821  FORMAT('     n =      1     2     3     4     5     6     7',/ &
           ,'   n_eff= ',7F6.2)
      WRITE(6,*)
END_PROVIDER

BEGIN_PROVIDER [real*8, screen, (17,17)]
BEGIN_DOC
! the explicit table of the screening constants
!
!     1s, 2s, 2p, 3s, 3p, 3d, 4s, 4p, 4d, 4f, 5s, 5p, 5d, 5f, 6s, 6p, 7s
!
! 1s .31
!
! 2s .85 .35
!
! 2p .85 .35 .35
!
! 3s  1  .85 .85 .35
!
! 3p  1  .85 .85 .35 .35
!
! 3d  1   1   1   1   1  .35
!
! 4s  1   1   1  .85 .85 .85 .35
!
! 4p  1   1   1  .85 .85 .85 .35 .35
!
! 4d  1   1   1   1   1   1   1   1  .35
!
! 4f  1   1   1   1   1   1   1   1   1  .35
!
! 5s  1   1   1   1   1   1  .85 .85 .85 .85 .35
!
! 5p  1   1   1   1   1   1  .85 .85 .85 .85 .35 .35
!
! 5d  1   1   1   1   1   1   1   1   1   1   1   1   .35
!
! 5f  1   1   1   1   1   1   1   1   1   1   1   1   1  .35
!
! 6s  1   1   1   1   1   1   1   1   1   1  .85 .85 .85 .35 .35
!
! 6p  1   1   1   1   1   1   1   1   1   1  .85 .85 .85 .85 .35 .35
!
! 7s  1   1   1   1   1   1   1   1   1   1   1   1   1   1  .85 .85 .35
!
!
!
END_DOC
     implicit none
     integer :: i,j
      DO I=1,17
       DO J=I+1,17
        SCREEN(I,J)=0.D0
       END DO
       DO J=1,I
        SCREEN(I,J)=1.D0
       END DO
      END DO
!
! 1s - 1s
      SCREEN (1,1)=Z30
! the remainder of the diagonal
      DO I=2,17
       SCREEN(I,I)=Z35
      END DO
! 2s,26 - 1s
      SCREEN (2,1)=Z85
      SCREEN (3,1)=Z85
! 2p,3s,3p - 2s
      SCREEN (3,2)=Z35
      SCREEN (4,2)=Z85
      SCREEN (5,2)=Z85
! 3s,3p - 2p
      SCREEN (4,3)=Z85
      SCREEN (5,3)=Z85
! 3p,4s,4p - 3s
      SCREEN (5,4) =Z35
      SCREEN (7,4) =Z85
      SCREEN (8,4) =Z85
! 4s,4p - 3p
      SCREEN (7,5) =Z85
      SCREEN (8,5) =Z85
! 4s,4p - 3d
      SCREEN (7,6) =Z85
      SCREEN (8,6) =Z85
! 4p,5s,5p - 4s
      SCREEN (8,7) =Z35
      SCREEN (11,7)=Z85
      SCREEN (12,7)=Z85
! 5s,5p - 4p
      SCREEN (11,8)=Z85
      SCREEN (12,8)=Z85
! 5s,5p - 4d
      SCREEN (11,9)=Z85
      SCREEN (12,9)=Z85
! 5s,5p - 4f
      SCREEN (11,10)=Z85
      SCREEN (12,10)=Z85
! 5p,6s,6p - 5s
      SCREEN (12,11)=Z35
      SCREEN (15,11)=Z85
      SCREEN (16,11)=Z85
! 6s,6p - 5p
      SCREEN (15,12)=Z85
      SCREEN (16,12)=Z85
! 6s,6p - 5d
      SCREEN (15,13)=Z85
      SCREEN (16,13)=Z85
! 6s,6p - 5f
      SCREEN (15,14)=Z85
      SCREEN (16,14)=Z85
! 6p,7s - 6s
      SCREEN (16,15)=Z35
      SCREEN (17,15)=Z85
! 7s - 6p
      SCREEN (17,16)=Z85
!
      DO I=1,17
       DO J=I+1,17
        SCREEN(I,J)=SCREEN(J,I)
       END DO
      END DO
END_PROVIDER

