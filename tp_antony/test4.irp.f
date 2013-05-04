! Fichier test4.irp.f
program test4
    implicit none
    integer :: i
    do i=1,natoms
        print *, acceleration(:,i)
    enddo
end program
