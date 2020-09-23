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

  do
    call random_number(diameter)
    if (diameter > 0.0_rkind) exit
  end do
  do
    call random_number(C_f)
    if (C_f > 0.0_rkind) exit
  end do
  do
    call random_number(pressure_1)
    if (pressure_1 > 0.0_rkind) exit
  end do
  do
    call random_number(pressure_2)
    if (pressure_2 > pressure_1) exit
  end do

  nozzle = nozzle_t(diameter = diameter, C_f = C_f)

  thrust_1 = nozzle%thrust(pressure_1)
  thrust_2 = nozzle%thrust(pressure_2)

  call assert(&
      thrust_1 < thrust_2, &
      "thrust is greater with greater pressure")

  call assert( &
      abs(nozzle%thrust(0._rkind)) < 1.0e-6_rkind, &
      "thrust is zero with zero pressure")

  print *," Test passed."

end program main
