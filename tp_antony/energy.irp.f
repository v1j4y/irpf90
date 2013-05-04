BEGIN_PROVIDER [double precision, Etot]
    BEGIN_DOC
! Caltulate the total energy
    END_DOC
    implicit none
    Etot = T + V
END_PROVIDER

