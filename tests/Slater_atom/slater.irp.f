! -*- F90 -*-
!
! Calculates total energies of atoms after the data given by
! J.C.Slater in his Phys.Rev. paper 1930:
!
! J.C.Slater: "Atomic Shielding Constants", Phys.Rev. 36 (1930) 57
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! P.Reinhardt, Paris, 1/2000
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      PROGRAM SLATER
        IMPLICIT none
        real*8 :: x,cptime
        
        X=CPTIME(3)
        CALL DATING('SLATER   ',1)
        
        call header
        
        call print_result
        X=CPTIME(4)
        CALL DATING('SLATER   ',2)
      end PROGRAM SLATER
       
      subroutine print_result
        implicit none
        integer :: i,iouter
        real*8 :: eev,eau,ecm,ekj

        WRITE(6,9903)
        DO I=1,17
         IF (IOCC(I).NE.0) WRITE(6,9902) SHSYM(I),IOCC(I),ZEFF(I) &
              ,XNEFF(I),BOHR*XNEFF(I)*XNEFF(I)/ZEFF(I), &
              -0.5d0*ZEFF(I)*ZEFF(I)/XNEFF(I)/XNEFF(I), &
              -0.5D0*HARTREE*ZEFF(I)*ZEFF(I)/XNEFF(I)/XNEFF(I)
        END DO
9902    FORMAT(4X,A2,8X,I2,6X,F12.6,F12.2,6X,F12.6,6X,F12.6,F14.2)
9903    FORMAT(2X,'Shell',4X,'Occupation',3X,'effective Z',5X &
             ,'effective n',4X,'radius (A)',4X,'energy (a.u.)' &
             ,4X,' (eV)')
 
      EEV=-ETOTAL*0.5d0*hartree
      EAU=-ETOTAL*0.5D0
      ECM=-ETOTAL*rydberg
      EKJ=-ETOTAL*kjmol
      WRITE(6,*)
      WRITE(6,*) ' TOTAL ENERGY (in 1/2 a.u.) : ',-ETOTAL
      WRITE(6,*)
      WRITE(6,*) ' TOTAL ENERGY (in eV):     ',EEV
      WRITE(6,*) ' TOTAL ENERGY (in a.u.):   ',EAU
      WRITE(6,*) ' TOTAL ENERGY (in cm-1):   ',ECM
      WRITE(6,*) ' TOTAL ENERGY (in kJ/mol): ',EKJ
      WRITE(6,9928) RADIUS
 9928 FORMAT(/,'  RADIUS OF THE ATOM (in Angstrom): ',F12.8,/)
 
 
      END

 BEGIN_PROVIDER [real*8, hartree ]
&BEGIN_PROVIDER [real*8, Bohr]
&BEGIN_PROVIDER [real*8, Rydberg]
&BEGIN_PROVIDER [real*8, kJmol]
BEGIN_DOC
! some unit conversion factors 
END_DOC
      BOHR=0.529177249D0
      HARTREE=27.21D0
      rydberg= 109700.D0
      kjmol=1311.04D0
END_PROVIDER

BEGIN_PROVIDER [real*8, radius]
BEGIN_DOC
! the radius of the atom
END_DOC
      implicit none
      integer :: iouter
!
! calculate the atomic radius
! which is the outermost shell?
      IOUTER=17
 107  CONTINUE
      IF (IOCC(IOUTER).EQ.0) THEN
       IOUTER=IOUTER-1
       GO TO 107
      END IF
!
      RADIUS=BOHR*XNEFF(IOUTER)*XNEFF(IOUTER)/ZEFF(IOUTER)
END_PROVIDER

      subroutine header
        implicit none
        WRITE(6,*) ' --------------------------------------------'
        WRITE(6,*)
        WRITE(6,*) '             S L A T E R  '
        WRITE(6,*) '     - calculation of atomic data'
        WRITE(6,*) ' after J.C.Slater Phys.Rev. 36 (1930) 57 '
        WRITE(6,*) '        P. Reinhardt (Paris) 2001 '
        WRITE(6,*) '      implementation using IRP (2012)'
        WRITE(6,*)
        WRITE(6,*) ' --------------------------------------------'
        WRITE(6,*)
      end subroutine header

BEGIN_PROVIDER [real*8, etotal]
BEGIN_DOC
! calculation of the total energy
END_DOC
     implicit none
     integer :: i
!
! calculate the total energy of a configuration
!
     ETOTAL=0.D0
     DO I=1,17
      IF (IOCC(I).NE.0) ETOTAL=ETOTAL+DBLE(IOCC(I))*(ZEFF(I)/XNEFF(I)) &
           *(ZEFF(I)/XNEFF(I))
     END DO
END_PROVIDER
