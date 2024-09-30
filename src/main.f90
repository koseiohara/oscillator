program main

    use namelist  , only : read_nml
    use oscillator, only : simulate

    implicit none

    real(4) :: time_begin
    real(4) :: time_end

    call cpu_time(time_begin)

    call read_nml()

    call simulate()

    call cpu_time(time_end)

    write(*,'(A,I0,A)') 'PROCCESS TIME : ', int(time_end-time_begin), 's'

end program main

