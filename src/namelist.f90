module namelist

    use params, only : kp                                                                    , &
                     & number_of_particles, mass_of_particles, spring_constant, spring_length, &
                     & number_of_time_steps, time_delta, output_delta                        , &
                     & output_filename

    implicit none

    private
    public :: read_nml

    real(kp) :: simulation_seconds

    contains


    subroutine read_nml()
        integer, parameter :: iunit=5

        namelist / time / simulation_seconds, time_delta, output_delta
        namelist / constant / number_of_particles, mass_of_particles, spring_constant, spring_length
        namelist / file / output_filename

        simulation_seconds = -999
        time_delta         = 0._kp
        output_delta       = 0._kp
        
        number_of_particles = -999
        mass_of_particles   = -999.E20_kp
        spring_constant     = -999.E20_kp
        spring_length       = -999.E20_kp

        output_filename = ''

        read(iunit,nml=time    )
        read(iunit,nml=constant)
        read(iunit,nml=file    )

        call checker()

        write(*,*)
        write(*,'(A)')        'SETTING ----------------------------------------------'
        write(*,'(A)')        '  OUTPUT FILE NAME        : ' // output_filename
        write(*,'(A,I0)')     '  NUMBER OF PARTICLES     : ', number_of_particles
        write(*,'(A,ES11.3)') '  MASS OF PARTICLES       :', mass_of_particles
        write(*,'(A,ES11.3)') '  SPRING_CONSTANT         :', spring_constant
        write(*,'(A,ES11.3)') '  SPRING_LENGTH           :', spring_length
        write(*,'(A,ES11.3)') '  TIME DELTA FOR INTEGRAL :', time_delta
        write(*,'(A,ES11.3)') '  TIME DELTA FOR OUTPUT   :', output_delta
        write(*,'(A,I0)')     '  NUMBER OF TIME STEPS    : ', number_of_time_steps
        write(*,'(A)')        '------------------------------------------------------'
        write(*,*)

        number_of_time_steps = int(simulation_seconds/time_delta)

    end subroutine read_nml


    subroutine checker()

        if (simulation_seconds <= 0._kp) then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A,ES11.3)') 'Invalid simulation_seconds : ', simulation_seconds
            write(*,'(A)') 'Invalid number (<1) was substituted, or the value was not specified'
            ERROR STOP
        endif

        if (time_delta <= 0._kp) then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A,ES11.3)') 'Invalid time_delta :', time_delta
            write(*,'(A)') 'Invalid number (<=0) was substituted, or the value was not specified'
            ERROR STOP
        endif

        if (output_delta <= 0._kp) then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A,ES11.3)') 'Invalid output_delta :', output_delta
            write(*,'(A)') 'Invalid number (<=0) was substituted, or the value was not specified'
            ERROR STOP
        endif

        if (number_of_particles <= 0) then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A,I0)') 'Invalid number_of_particles : ', number_of_particles
            write(*,'(A)') 'Invalid number (<1) was substituted, or the value was not specified'
            ERROR STOP
        endif

        if (mass_of_particles <= 0._kp) then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A,ES11.3)') 'Invalid mass_of_particles :', mass_of_particles
            write(*,'(A)') 'Invalid number (<=0) was substituted, or the value was not specified'
            ERROR STOP
        endif

        if (spring_constant <= 0._kp) then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A,ES11.3)') 'Invalid spring_constant :', spring_constant
            write(*,'(A)') 'Invalid number (<=0) was substituted, or the value was not specified'
            ERROR STOP
        endif

        if (spring_length <= 0._kp) then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A,ES11.3)') 'Invalid spring_length :', spring_length
            write(*,'(A)') 'Invalid number (<=0) was substituted, or the value was not specified'
            ERROR STOP
        endif

        if (output_filename == '') then
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A)') 'output_filename was not specified'
            ERROR STOP
        endif

    end subroutine checker


end module namelist

