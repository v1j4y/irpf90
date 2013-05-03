BEGIN_PROVIDER [double precision, V]
    implicit none
    BEGIN_DOC
! potential energy
    END_DOC
    V = V_lj
END_PROVIDER
BEGIN_PROVIDER [double precision, V_lj]
    implicit none
    BEGIN_DOC
! Lennard Jones potential energy.
    END_DOC
    double precision :: sigma_over_r
    integer :: i,j
    V_lj=0d0
    do i=1,natoms
        do j=i+1,natoms
            ASSERT(distance(j,i) >0.)
            sigma_over_r = sigma_lj/distance(j,i)
            V_lj += ( sigma_over_r**12 - sigma_over_r**6)
        enddo
    enddo
            V_lj *= 4.d0 * epsilon_lj
END_PROVIDER

BEGIN_PROVIDER [double precision, epsilon_lj]
&BEGIN_PROVIDER [double precision, sigma_lj]
    implicit none
    BEGIN_DOC
! Parameteres of the Lennard-Jones potential
    END_DOC
    print *, 'Epsilon?'
    read(*,*) epsilon_lj
    ASSERT (epsilon_lj > 0.)
    print *, 'Sigma?'
    read(*,*) sigma_lj
    ASSERT (sigma_lj > 0.)
END_PROVIDER

BEGIN_PROVIDER [double precision, interratomic_distance]
    implicit none
    BEGIN_DOC
! Distance between the atoms
    END_DOC
    print *,'inter-atomic distance?'
    read(*,*)interratomic_distance
    ASSERT (interratomic_distance > 0.)
END_PROVIDER

BEGIN_PROVIDER [double precision, acceleration, (3,natoms)]
    implicit none
    BEGIN_DOC
! Calculate accelaration = -grad(V)/m
    END_DOC
    integer :: i,k
    do i=1,natoms
        do k=1,3
            acceleration(k,i)=-V_grad(k,i)/mass(i)
        enddo
    enddo
END_PROVIDER

BEGIN_PROVIDER [double precision, V_grad, (3,natoms)]
    implicit none
    BEGIN_DOC
! Calculate gradiant
    END_DOC
    integer :: i,k
    do i=1,natoms
        do k=1,3
            V_grad(k,i)= V_grad_numeric(k,i)
        enddo
    enddo
END_PROVIDER

BEGIN_PROVIDER [double precision, V_grad_numeric, (3,natoms)]
    implicit none
    BEGIN_DOC
! Calculate numeric gradient of potential
    END_DOC
    integer :: i,k
    do i=1,natoms
        do k=1,3
            coord(k,i)+=dstep
            TOUCH coord
            V_grad_numeric(k,i)=V
            coord(k,i)-=2.d0*dstep
            TOUCH coord
            V_grad_numeric(k,i)-=V
            V_grad_numeric(k,i)*=.5d0/dstep
            coord(k,i)+=dstep
        enddo
    enddo
    SOFT_TOUCH coord
END_PROVIDER

BEGIN_PROVIDER [double precision, dstep]
    implicit none
    BEGIN_DOC
! Finite difference step
    END_DOC
    dstep=1.d-4
END_PROVIDER
