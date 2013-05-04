BEGIN_PROVIDER [double precision, T]
    BEGIN_DOC
! Calculate kinetic energy
    END_DOC
    implicit none
    integer :: i
    T=0d0
    do i=1,natoms
        T+=mass(i) * velocity2(i)
    enddo
    T*=0.5d0
END_PROVIDER

BEGIN_PROVIDER [double precision, velocity2, (natoms)]
    BEGIN_DOC
! Calculate velocity of atoms
    END_DOC
    implicit none
    integer :: i,k
    do i=1,natoms
        velocity2(i)=0d0
        do k=1,natoms
            velocity2(i)+=velocity(k,i)*velocity(k,i)
        enddo
    enddo
END_PROVIDER

BEGIN_PROVIDER [double precision, velocity, (3,natoms)]
    BEGIN_DOC
! Calculate components of velocity
    END_DOC
    implicit none
    integer :: i,k
    do i=1,natoms
        do k=1,3
            velocity(k,i)=0d0
        enddo
    enddo
END_PROVIDER
