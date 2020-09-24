program main
  use assertions_interface, only: assert
  use kind_parameters, only: rkind
  use state_interface, only: state_t
  use universal_constants, only: PI

  implicit none

  type(state_t) :: state_1
  type(state_t) :: state_2
  type(state_t) :: total

  real(rkind) :: time_1
  real(rkind) :: time_2
  real(rkind) :: mass_1
  real(rkind) :: mass_2
  real(rkind) :: energy_1
  real(rkind) :: energy_2
  real(rkind) :: burn_depth_1
  real(rkind) :: burn_depth_2

  real(rkind), allocatable :: values(:,:)

  call random_number(time_1)
  call random_number(time_2)
  call random_number(mass_1)
  call random_number(mass_2)
  call random_number(energy_1)
  call random_number(energy_2)
  call random_number(burn_depth_1)
  call random_number(burn_depth_2)

  state_1 = state_t(time = time_1, mass = mass_1, energy = energy_1, burn_depth = burn_depth_1)
  state_2 = state_t(time = time_2, mass = mass_2, energy = energy_2, burn_depth = burn_depth_2)

  total = state_1 + state_2

  call assert(&
      abs(state_1%time() + state_2%time() - total%time()) < 1.0e-6_rkind &
      .and. abs(state_1%mass() + state_2%mass() - total%mass()) < 1.0e-6_rkind &
      .and. abs(state_1%energy() + state_2%energy() - total%energy()) < 1.0e-6_rkind &
      .and. abs(state_1%burn_depth() + state_2%burn_depth() - total%burn_depth()) < 1.0e-6_rkind, &
      "adding 2 states adds their components")

  values = state_1%row_vector()

  call assert( &
      abs(state_1%time() - values(1,1)) < 1.0e-6_rkind &
      .and. abs(state_1%mass() - values(1,2)) < 1.0e-6_rkind &
      .and. abs(state_1%energy() - values(1,3)) < 1.0e-6_rkind &
      .and. abs(state_1%burn_depth() - values(1,4)) < 1.0e-6_rkind, &
      "returns a vector of its values in the order time, mass, energy, burn_depth")

  print *," Test passed."

end program main
