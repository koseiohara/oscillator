module oscillator

    use BinIO , only : finfo, fopen, fclose, fwrite
    use params, only : kp                                             , &
                     & nparts=>number_of_particles, mass_of_particles , &
                     & k=>spring_constant, spring_length              , &
                     & nt=>number_of_time_steps, dt=>time_delta       , &
                     & out_dt=>output_delta                           , &
                     & ofile=>output_filename

    implicit none

    private
    public :: simulate

    contains


    subroutine init(mass, posit, vel)
        real(kp), intent(out) :: mass(nparts)
        real(kp), intent(out) :: posit(nparts)
        real(kp), intent(out) :: vel(nparts)

        integer :: i

        call random_seed()
        
        mass(1:nparts) = mass_of_particles
        vel(1:nparts)  = 0._kp
        !call random_number(vel(1:nparts))  !! OUT
        !vel(1:nparts) = vel(1:nparts) * spring_length

        ! Random Generate
        !call random_number(posit(1:nparts))  !! OUT
        !posit(1:nparts)  = (posit(1:nparts)-0.5_kp)*spring_length + [(real(i, kind=kp) * spring_length, i = 1, nparts)]

        posit(1:nparts) = [(real(i, kind=kp) * spring_length, i = 1, nparts)]
        posit(nparts) = posit(nparts) + 0.8_kp*spring_length

    end subroutine init


    subroutine simulate()
        real(kp) :: mass(nparts)
        real(kp) :: posit(nparts)
        real(kp) :: vel(nparts)

        type(finfo) :: output_file

        integer :: output_recstep
        integer :: tt
        integer :: rr

        call fopen(output_file     , &  !! OUT
                 & file   =ofile   , &  !! IN
                 & action ='WRITE' , &  !! IN
                 & record =1       , &  !! IN
                 & recl   =4*nparts, &  !! IN
                 & recstep=1         )  !! IN

        ! Initialize the status
        call init(mass(1:nparts) , &  !! OUT
                & posit(1:nparts), &  !! OUT
                & vel(1:nparts)    )  !! OUT

        call fwrite(output_file    , &  !! INOUT
                  & posit(1:nparts)  )  !! IN

        output_recstep = int(out_dt/dt)

        do rr = 1, nt, output_recstep
            do tt = 1, output_recstep

                call rungeKutta(mass(1:nparts) , &  !! IN
                              & posit(1:nparts), &  !! INOUT
                              & vel(1:nparts)    )  !! INOUT

            enddo

            call fwrite(output_file    , &  !! INOUT
                      & posit(1:nparts)  )  !! IN
        
        enddo

        call fclose(output_file)  !! INOUT

    end subroutine simulate


    !
    ! Time Integration by the Runge Kutta Method
    !
    subroutine rungeKutta(mass, posit, vel)
        real(kp), intent(in)    :: mass(nparts)
        real(kp), intent(inout) :: posit(nparts)
        real(kp), intent(inout) :: vel(nparts)

        real(kp) :: vel1(nparts)
        real(kp) :: vel2(nparts)
        real(kp) :: vel3(nparts)
        real(kp) :: vel4(nparts)

        real(kp) :: accel1(nparts)
        real(kp) :: accel2(nparts)
        real(kp) :: accel3(nparts)
        real(kp) :: accel4(nparts)

        real(kp) :: dt_half

        dt_half = dt * 0.5_kp

        accel1(1:nparts) = get_accel(mass(1:nparts), posit(1:nparts))
        vel1(1:nparts)   = vel(1:nparts)

        accel2(1:nparts) = get_accel(mass(1:nparts), posit(1:nparts) + vel1(1:nparts)*dt_half)
        vel2(1:nparts)   = vel(1:nparts) + accel1(1:nparts)*dt_half

        accel3(1:nparts) = get_accel(mass(1:nparts), posit(1:nparts) + vel2(1:nparts)*dt_half)
        vel3(1:nparts)   = vel(1:nparts) + accel2(1:nparts)*dt_half

        accel4(1:nparts) = get_accel(mass(1:nparts), posit(1:nparts) + vel3(1:nparts)*dt)
        vel4(1:nparts)   = vel(1:nparts) + accel3(1:nparts)*dt

        posit(1:nparts) = posit(1:nparts) + (  vel1(1:nparts)                  &
                                           & + vel2(1:nparts) + vel2(1:nparts) &
                                           & + vel3(1:nparts) + vel3(1:nparts) &
                                           & + vel4(1:nparts)                  ) * dt * 0.166666666666666666666666666666666_kp

        vel(1:nparts) = vel(1:nparts) + (  accel1(1:nparts)                    &
                                     &   + accel2(1:nparts) + accel2(1:nparts) &
                                     &   + accel3(1:nparts) + accel3(1:nparts) &
                                     &   + accel4(1:nparts)                    ) * dt * 0.166666666666666666666666666666666_kp

    end subroutine rungeKutta


    !
    ! Acceleration
    ! This is NOT the forces. divide argument "accel" by the masses
    !
    function get_accel(mass, posit) result(accel)
        real(kp), intent(in)  :: mass(nparts)
        real(kp), intent(in)  :: posit(nparts)

        real(kp) :: accel(nparts)        !!! OUTPUT PARAMETER

        call spring_force_accel(mass(1:nparts) , &  !! IN
                              & posit(1:nparts), &  !! IN
                              & accel(1:nparts)  )  !! OUT

    end function get_accel


    subroutine spring_force_accel(mass, posit, accel)
        real(kp), intent(in)  :: mass(nparts)
        real(kp), intent(in)  :: posit(nparts)
        real(kp), intent(out) :: accel(nparts)

        real(kp) :: posit_expand(0:nparts+1)

        posit_expand(0)        = 0._kp
        posit_expand(1:nparts) = posit(1:nparts)
        posit_expand(nparts+1) = spring_length*real(nparts+1, kind=kp)

        accel(1:nparts) = (posit_expand(0:nparts-1) - (posit_expand(1:nparts)+posit_expand(1:nparts)) + posit_expand(2:nparts+1)) &
                       & * k / mass(1:nparts)

    end subroutine spring_force_accel


end module oscillator

