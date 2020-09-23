program main
  use assertions_interface, only: assert
  use kind_parameters, only: rkind
  use nozzle_interface, only: nozzle_t
  use universal_constants, only: PI

  implicit none

  type(nozzle_t) :: nozzle

  real(rkind) :: diameter
  real(rkind) :: C_f
  real(rkind) :: pressure_1
  real(rkind) :: pressure_2
  real(rkind) :: thrust_1
  real(rkind) :: thrust_2

  call random_number(diameter)
  call random_number(C_f)
  call random_number(pressure_1)
  call random_number(pressure_2)

  nozzle = nozzle_t(diameter = diameter, C_f = C_f)

  call assert( &
      abs(nozzle%diameter() - diameter) < 1.0e-6_rkind, &
      "the given diameter is preserved")
  call assert( &
      abs(nozzle%area() - diameter**2 / 4.0_rkind * PI) < 1.0e-6_rkind, &
      "the area is calculated in accordance with the given diameter")
  thrust_1 = nozzle%thrust(pressure_1)
  thrust_2 = nozzle%thrust(pressure_2)
  call assert(&
      abs(thrust_1/pressure_1 - thrust_2/pressure_2) < 1.0e-6_rkind, &
      "thrust is proportional to the pressure")

  print *," Test passed."

end program main
