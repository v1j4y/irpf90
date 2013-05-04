! fichier files.irp.f
integer function getUnitAndOpen(f,mode)
! trouve un numero d'unite et ouvre le fichier
    implicit none
    character*(*)   :: f
    character*(128) :: new_f
    integer         :: iunit
    logical         :: is_open, exists
    character       :: mode

    is_open=.True.
    iunit=10
    new_f=f
    do while (is_open)
        inquire(unit=iunit,opened=is_open)
        if (.not.is_open)then
            getUnitAndOpen=iunit
        endif
        iunit=iunit+1
    enddo
    if (mode .eq.'r')then
        inquire(file=f,exist=exists)
        if (.not.exists)then
            open(unit=getUnitAndOpen,file=f,status='NEW',action='WRITE')
            close(unit=getUnitAndOpen)
        endif
        open(unit=getUnitAndOpen,file=f,status='OLD',action='READ')
    else if (mode.eq.'w') then
        open(unit=getUnitAndOpen,file=new_f,status='UNKNOWN',action='WRITE')
    else if (mode .eq. 'a') then
        open(unit=getUnitAndOpen,file=new_f,status='UNKNOWN', &
        action='WRITE',position='APPEND')
    else if (mode .eq. 'x') then
        open(unit=getUnitAndOpen,file=new_f)
    endif
end function getUnitAndOpen

BEGIN_PROVIDER [integer, output]
    BEGIN_DOC
! file unit corresponding to output
    END_DOC
    integer :: getUnitAndOpen
    output=getUnitAndOpen('output','w')
END_PROVIDER

subroutine print_data(is)
    implicit none
    integer , intent (in) :: is
    write(output,*)natoms
    write(output,'(I8, 3(2X, E15.8))')  is, V, T, Etot
    integer :: i
    do i=1,natoms
        write(output,'(A,3(2X,F15.8))')'H', coord(:,i)
    enddo
end subroutine print_data

