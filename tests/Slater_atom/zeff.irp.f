! -*- F90 -*-
BEGIN_PROVIDER [real*8, zeff, (17) ]
BEGIN_DOC
! the effective nuclear charges
! are calculated from the input data
END_DOC
     implicit none
     integer :: i,j
     real*8 :: z
!
! calculate the Zeff
!
     DO I=1,17
      ZEFF(I)=0.D0
      IF (IOCC(I).NE.0) THEN
       Z=DBLE(NZ)
       DO J=1,I
        IF (I.NE.J.AND.IOCC(J).NE.0) THEN
         Z=Z-DBLE(IOCC(J))*SCREEN(I,J)
        END IF
        IF (I.EQ.J) THEN
         Z=Z-DBLE(IOCC(J)-1)*SCREEN(I,I)
        END IF
       END DO
       ZEFF(I)=Z
      END IF
     END DO
!
! correct for sp shells:
! if p, then Zeff(s)=Zeff(p)
! if no p, then no correction
!
! this concerns shells 2/3 4/5 7/8 11/12 15/16
      IF (IOCC(3) .NE.0) ZEFF(2) =ZEFF(3)
      IF (IOCC(5) .NE.0) ZEFF(4) =ZEFF(5)
      IF (IOCC(8) .NE.0) ZEFF(7) =ZEFF(8)
      IF (IOCC(12).NE.0) ZEFF(11)=ZEFF(12)
      IF (IOCC(16).NE.0) ZEFF(15)=ZEFF(16)

END_PROVIDER
