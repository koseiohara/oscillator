module params

    implicit none

    integer, parameter :: kp = 8

    integer :: number_of_particles
    integer :: number_of_time_steps

    real(kp) :: mass_of_particles
    real(kp) :: spring_constant
    real(kp) :: spring_length
    real(kp) :: time_delta
    real(kp) :: output_delta

    character(128) :: output_filename

end module params

