! Fichier test2.irp.f
program test2
    integer :: i
    do i=1,natoms
        print *, distance(:,i)
    enddo
end program
