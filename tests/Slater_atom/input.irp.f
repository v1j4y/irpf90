! -*- F90 -*-
BEGIN_PROVIDER [integer, NZ]
BEGIN_DOC
! the nuclear charge
END_DOC
      implicit none
      integer :: i
      WRITE(6,*)
      WRITE(6,*)

      WRITE(6,*) ' PLEASE GIVE THE ATOMIC NUMBER OF THE ELEMENT'
      READ(5,*) NZ
      IF (NZ.GT.92) THEN
       WRITE(6,*) ' WE CAN TREAT ELEMENTS UP TO 92'
       STOP ' Z TOO HIGH '
      ELSE IF (NZ.LT.0) THEN
       WRITE(6,*) ' ATOMIC NUMBERS SHOULD BE POSITIVE '
       STOP ' Z NEGATIVE '
      END IF
      WRITE(6,*)
      WRITE(6,*) ' ATOMIC SYMBOL OF THE ELEMENT: ',SYMBAT(NZ)
!
! which next rare gas?
      I=1
 100  CONTINUE
      IF (NZ.LT.NGAS(I)) GO TO 200
      I=I+1
      GOTO 100
 
 200  CONTINUE
      I=I-1
      IF (NGAS(I).EQ.NZ) THEN
       WRITE(6,*) ' the atom is a rare gas '
      ELSE
       WRITE(6,*) ' the core corresponds to the rare gas ' &
           ,SYMBAT(NGAS(I))
      END IF
      WRITE(6,*)
END_PROVIDER

BEGIN_PROVIDER [integer, iocc, (17)]
BEGIN_DOC
! the orbital occupation numbers
END_DOC
     implicit none
     integer :: i,ielec,idum,iact,inew,ioccm(17)
     character*1 :: c1
!
! electronic configuration
!
      WRITE(6,*) ' Electronic Configuration according', &
      ' to the Aufbau Principle: '
      IDUM=NZ
      DO I=1,17
       IOCCM(IMAP(I))=MIN(NSUBS(IMAP(I)),IDUM)
       IDUM=MAX(IDUM-NSUBS(IMAP(I)),0)
       IOCC(IMAP(I))=IOCCM(IMAP(I))
      END DO
!
      DO I=1,17
       WRITE(6,9901) I,SHSYM(I),IOCCM(I)
      END DO
!
 9901 FORMAT(5X,' Shell ',I2,' (',A2,'):  ',I2,' electrons')
      WRITE(6,*)
!
! this is the occupation according to the Aufbau principle
! now we reorder according to the main quantum number n
!
 115  CONTINUE
      WRITE(6,*) ' Do you want to modify the configuration? (y/n)'
      READ(5,'(A1)') C1
      IF (C1.EQ.'Y'.OR.C1.EQ.'y') THEN
 117   CONTINUE
       WRITE(6,*) ' Which shell occupation should be modified? '
       READ(5,*) IACT
 118   CONTINUE
       WRITE(6,*) ' How many electrons should be there? '
       READ(5,*) INEW
       IF (INEW.GT.NSUBS(IACT)) THEN
        WRITE(6,*) ' THIS SHELL CAN ONLY HOLD ',NSUBS(IACT) &
            ,' ELECTRONS! '
        GO TO 118
       END IF
       IOCC(IACT)=INEW
       WRITE(6,*) ' Is that all? (y/n)'
       READ(5,'(A1)') C1
       IF (C1.EQ.'N'.OR.C1.EQ.'n') GO TO 117
! print actual configuration
       WRITE(6,*)
       WRITE(6,*) ' Chosen electronic configuration:'
       IELEC=0
       DO I=1,17
        IF (IOCC(I).NE.0) THEN
         IELEC=IELEC+IOCC(I)
         WRITE(6,9901) I,SHSYM(I),IOCC(I)
        END IF
       END DO
       WRITE(6,*)
       WRITE(6,*) ' total charge: ',NZ-IELEC
       WRITE(6,*)
       goto 115
      END IF
!
      WRITE(6,*)
      WRITE(6,*) ' chosen electronic configuration:'
      IELEC=0
      DO I=1,17
       IF (IOCC(I).NE.0) THEN
        WRITE(6,9901) I,SHSYM(I),IOCC(I)
        IELEC=IELEC+IOCC(I)
       END IF
      END DO
      WRITE(6,*)
      WRITE(6,*) ' total charge: ',NZ-IELEC
      WRITE(6,*)
END_PROVIDER
