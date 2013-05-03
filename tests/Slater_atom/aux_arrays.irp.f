! -*- F90 -*-
BEGIN_PROVIDER [integer, imap, (17)]
BEGIN_DOC
! mapping array for the Klechkowski rule (Aufbau principle)
END_DOC
      IMAP( 1)=1
      IMAP( 2)=2
      IMAP( 3)=3
      IMAP( 4)=4
      IMAP( 5)=5
      IMAP( 6)=7
      IMAP( 7)=6
      IMAP( 8)=8
      IMAP( 9)=11
      IMAP(10)=9
      IMAP(11)=12
      IMAP(12)=15
      IMAP(13)=10
      IMAP(14)=13
      IMAP(15)=16
      IMAP(16)=17
      IMAP(17)=14
END_PROVIDER

BEGIN_PROVIDER [integer, nquant, (17)]
BEGIN_DOC
! main quantum number of the shells 
END_DOC
      NQUANT( 1)=1
      NQUANT( 2)=2
      NQUANT( 3)=2
      NQUANT( 4)=3
      NQUANT( 5)=3
      NQUANT( 6)=3
      NQUANT( 7)=4
      NQUANT( 8)=4
      NQUANT( 9)=4
      NQUANT(10)=4
      NQUANT(11)=5
      NQUANT(12)=5
      NQUANT(13)=5
      NQUANT(14)=5
      NQUANT(15)=6
      NQUANT(16)=6
      NQUANT(17)=7
END_PROVIDER

BEGIN_PROVIDER [integer, nsubs, (17)]
BEGIN_DOC
! max occupation of a shell
END_DOC
      NSUBS( 1)=2
      NSUBS( 2)=2
      NSUBS( 3)=6
      NSUBS( 4)=2
      NSUBS( 5)=6
      NSUBS( 6)=10
      NSUBS( 7)=2
      NSUBS( 8)=6
      NSUBS( 9)=10
      NSUBS(10)=14
      NSUBS(11)=2
      NSUBS(12)=6
      NSUBS(13)=10
      NSUBS(14)=14
      NSUBS(15)=2
      NSUBS(16)=6
      NSUBS(17)=10
END_PROVIDER

BEGIN_PROVIDER  [integer, ngas, (7)]
BEGIN_DOC
! next rare gas
END_DOC
      NGAS(1)=2
      NGAS(2)=10
      NGAS(3)=18
      NGAS(4)=36
      NGAS(5)=54
      NGAS(6)=86
      NGAS(7)=200
END_PROVIDER

BEGIN_PROVIDER[ real*8, xneff, (17)]
BEGIN_DOC
! attribution of the effective n for the shells 
END_DOC
      implicit none
      integer :: i
      do i=1,17
       XNEFF(I)=XNQEFF(NQUANT(I))
      end do
END_PROVIDER

