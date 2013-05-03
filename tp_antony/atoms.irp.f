! Fichier atoms.irp.f
BEGIN_PROVIDER [integer, natoms]
    BEGIN_DOC
! Number of atoms
    END_DOC
    implicit none
    print *, 'number of atoms?'
    read(*,*)natoms
    ASSERT (natoms > 0)
END_PROVIDER

BEGIN_PROVIDER [ double precision, coord, (3,natoms)]
&BEGIN_PROVIDER [double precision, mass, (natoms)]
    implicit none
    BEGIN_DOC
! Atomic data, input in atomic units.
    END_DOC
    print *, 'for each atom: x,y,z,mass?'
    integer :: i,j
    do i=1,natoms
        read(*,*) (coord(j,i),j=1,3),mass(i)
        ASSERT (mass(i) > 0.)
    enddo
END_PROVIDER

BEGIN_PROVIDER [double precision, distance, (natoms,natoms)]
    implicit none
    BEGIN_DOC
! distance : distance matrix of atoms
    END_DOC
    integer :: i,j,k
    do i=1,natoms
        do j=1,natoms
            distance(j,i)=0
            do k=1,3
                distance(j,i)+=(coord(k,j)-coord(k,i))**2
            enddo
            distance(j,i)=SQRT(distance(j,i))
        enddo
    enddo
END_PROVIDER

