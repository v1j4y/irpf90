! Fichier verlet.irp.f
BEGIN_PROVIDER [integer, nsteps]
    implicit none
    BEGIN_DOC
! number of steps for the dynamics
    END_DOC
    print *,'nsteps?'
    read(*,*)nsteps
    ASSERT(nsteps > 0.)
END_PROVIDER

BEGIN_PROVIDER [double precision, tstep]
&BEGIN_PROVIDER [double precision, tstep2]
    implicit none
    BEGIN_DOC
! Time step for the dynamics
    END_DOC
    print *, 'time step?'
    read(*,*)tstep
    ASSERT(tstep > 0.)
    tstep2=tstep*tstep
END_PROVIDER

subroutine verlet
    implicit none
    integer :: is,i,k
    do is=1,nsteps
        do i=1,natoms
            do k=1,3
                coord(k,i)+=tstep*velocity(k,i) + .5d0*tstep2*acceleration(k,i)
                velocity(k,i)+=.5d0*tstep*acceleration(k,i)
            enddo
        enddo
        TOUCH coord velocity
        do i=1,natoms
            do k=1,3
                velocity(k,i)+=0.5d0*tstep*acceleration(k,i)
            enddo
        enddo
        TOUCH velocity
        call print_data(is)
    enddo
end subroutine
