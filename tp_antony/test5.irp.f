! Fichier test5.irp.f
program test5
    implicit none
    integer :: i
    call verlet
    do i=1,natoms
        print *, coord(:,i)
    enddo
end program

