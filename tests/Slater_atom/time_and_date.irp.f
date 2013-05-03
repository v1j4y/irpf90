! -*- F90 -*-
      SUBROUTINE TIMING(NOMZ)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        CHARACTER*4 NOMZ
        CALL SECNND(FT)
        WRITE(6,100)NOMZ,FT
100     FORMAT(1X,53('T'),1X,A4,' CPU',F11.3)
        CALL FLUSH(6)
      END SUBROUTINE TIMING
!
      SUBROUTINE SECNND(X)
        REAL*8 X,CPTIME
        X=CPTIME(1)
      END subroutine secnnd
!
      REAL*8 FUNCTION CPTIME(I)
        IMPLICIT REAL*8 (A-H,O-Z)
        INTEGER I
!
        IF (I.EQ.1) THEN
         CALL CPUTIM(X)
         CPTIME=X
        ELSEIF (I.EQ.4.OR.I.EQ.3) THEN
         CALL CPUTIM(X)
         IX1=INT(X/3600.)
         IX2=INT((X-REAL(IX1*3600))/60.)
         X2=X-REAL(IX1*3600 + IX2*60)
         IF (I.EQ.4) THEN
          CALL TIMING(' ALL ')
          WRITE(6,*)
          WRITE (6,9991) IX1,IX2,X2
         END IF
9991     FORMAT(' TOTAL CPUTIME: ',I5,' hrs ',I3, &
              ' mins and ',f7.4,' secs')
         CPTIME=X
        END IF
      END FUNCTION CPTIME
!
 
      SUBROUTINE CPUTIM(TIM)
        IMPLICIT REAL*8 (A-H,O-Z)
!
!     UNIX SYSTEM
!
        REAL T(2),SEC1,ETIME
        SEC1=ETIME(T)
        TIM=DBLE(T(1)+T(2))
      END SUBROUTINE CPUTIM
!
      SUBROUTINE DATIM(A,B)
        implicit none
        CHARACTER*8 A,B
        CHARACTER*24 DATE
        CHARACTER*3 MON(12)
        CHARACTER*2 M(13)
        integer :: i,imon
        DATA MON /'Jan','Feb','Mar','Apr','May','Jun','Jul', &
             'Aug','Sep','Oct','Nov','Dec'/
        DATA M /'01','02','03','04','05','06','07','08', &
             '09','10','11','12','**'/
        DO I=1,24
         DATE(I:I)='-'
        end do
        CALL FDATE(DATE)
        imon=13
        DO I=1,12
         IF (DATE(5:7).EQ.MON(I)) then
          imon=i
         end IF
        end DO
        I=imon
        A(1:2)=DATE(9:10)
        A(3:3)='.'
        A(4:5)=M(I)
        A(6:6)='.'
        A(7:8)=DATE(23:24)
        B(1:8)=DATE(12:19)
      END SUBROUTINE DATIM

      SUBROUTINE DATING(PNAME,IACT)
        CHARACTER*8 PNAME,A,B
        CALL DATIM(A,B)
! remove leading or trailing blanks from pname
        IFST=1
100     CONTINUE
        IF (PNAME(IFST:IFST).EQ.' ') THEN
         IFST=IFST+1
         IF (IFST.LT.8) GO TO 100
        END IF
        ILST=8
        IF (IFST.EQ.8) GO TO 300
200     CONTINUE
        IF (PNAME(ILST:ILST).EQ.' ') THEN
         ILST=ILST-1
         IF (ILST.GT.1) GO TO 200
        END IF
300     CONTINUE
        
        IF (IACT.EQ.1) THEN
         WRITE(6,9901) PNAME(IFST:ILST),A,B
        ELSE IF (IACT.EQ.2) THEN
         WRITE(6,9902) PNAME(IFST:ILST),A,B
        ELSE
         WRITE(6,9903) PNAME(IFST:ILST),A,B
        END IF
9901    FORMAT(//,' Started ',A,';  date: ',A8,' time: ',A8,//)
9902    FORMAT(//,' Finished ',A,';  date: ',A8,' time: ',A8,//)
9903    FORMAT(//,1X,A,';  date: ',A8,' time: ',A8,//)
      END SUBROUTINE DATING
      



