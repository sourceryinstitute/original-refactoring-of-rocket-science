module refurbished_rocket_module
  implicit none

  private
  public :: refurbished_rocket

contains

function refurbished_rocket(input_file)
  !! This driver function simulates a single stage rocket motor flowing out of a nozzle,
  !! assuming a thrust coefficient and ignoring the complexities of what happens to thrust
  !! low pressures, i.e. shock in the nozzle.  The result is a results_t object that
  !! encapsulates the time history of values that can be printed via user-defined
  !! derived-type output.

use assertions_interface, only : assert, max_errmsg_len
use results_interface, only : results_t
use burn_state_interface, only : burn_state_t
use geometry_interface, only : geometry_t
use generation_rate_interface, only : generation_rate_t
use flow_rate_t_interface, only : flow_rate_t
use chamber_gas_interface, only : chamber_gas_t
use nozzle_t_interface, only : nozzle_t
use constants, only : dp, zero, one
implicit none

character(len=*), intent(in) :: input_file
type(results_t) refurbished_rocket

character(len=max_errmsg_len) error_message
integer io_status, file_unit
integer, parameter :: success = 0

type(burn_state_t) burn_state
type(geometry_t) geometry
type(chamber_gas_t) chamber_gas
type(nozzle_t) nozzle

real(dp), parameter :: initial_volume = one
real(dp) time, dt, tmax, rhos, Tflame
real(dp), allocatable :: output(:,:)

integer nsteps, i

real(dp) dt_, t_max_
real(dp) c_p_, MW_
real(dp) temperature_, pressure_
real(dp) T_flame_, r_ref_, n_
real(dp) id_, od_, length_, rho_solid_
real(dp) dia_, C_f_

namelist/numerics_list/ dt_, t_max_
namelist/gas_list/ c_p_, MW_
namelist/state_list/  temperature_, pressure_
namelist/combustion_list/ T_flame_, r_ref_, n_
namelist/grain_list/ id_, od_, length_, rho_solid_
namelist/nozzle_list/ dia_, C_f_

open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
call assert(io_status == success, "legacy_rocket: io_status == success", error_message)

read(file_unit, nml=numerics_list)
dt   = dt_
tmax = t_max_

read(file_unit, nml=gas_list)
read(file_unit, nml=state_list)
chamber_gas = chamber_gas_t(MW = MW_, c_p = c_p_, T = temperature_, p = pressure_, V = initial_volume)

read(file_unit, nml=combustion_list)
Tflame = T_flame_

read(file_unit, nml=grain_list)
geometry = geometry_t(vol = initial_volume, id = id_, od = od_, length = length_)
rhos   = rho_solid_

read(file_unit, nml=nozzle_list)
nozzle = nozzle_t(dia=dia_, C_f=C_f_)

close(file_unit)

burn_state = burn_state_t(reference_burn_rate = r_ref_, pressure = pressure_, exponent_ = n_)


nsteps=nint(tmax/dt) ! number of time steps

allocate(output(0:nsteps,6)) ! preallocate an output array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

  time = zero
  associate(mdotos => zero, thrust => zero, volume => geometry%vol())
    output(0,:)=[time, chamber_gas%p(volume), chamber_gas%T(), mdotos, thrust, volume]
  end associate

  associate(R_gas => chamber_gas%R_gas(), c_v => chamber_gas%c_v(), g => chamber_gas%g(), c_p => chamber_gas%c_p())

    do i=1,nsteps

      associate(p => chamber_gas%p(geometry%vol()))

        burn_state = burn_state_t(burn_state, r_ref_, p, n_, dt)

        associate(db => burn_state%db(), r => burn_state%r())
          associate(surf => geometry%surf(db))

            geometry = geometry_t(geometry,  volume_increment = merge(zero, r*surf*dt, geometry%burnout(db)) )

            associate( &
              flow_rate => flow_rate_t(chamber_gas%T(), g, R_gas, p, c_p, nozzle%area()), &
              generation_rate => generation_rate_t(rhos, r, surf, c_p, Tflame) &
            )
              associate(mdotos => flow_rate%mdotos())
                chamber_gas = chamber_gas_t( chamber_gas,  &
                  mass_increment   = (generation_rate%mdotgen() - mdotos)*dt, &
                  energy_increment = (generation_rate%edotgen() - flow_rate%edotos())*dt  &
                )
                associate(volume => geometry%vol())
                  associate(p => chamber_gas%p(volume))
                    time = time + dt
                    output(i,:)=[time, p, chamber_gas%T(), mdotos, nozzle%thrust(p), volume]
                  end associate
                end associate
              end associate
            end associate
          end associate
        end associate
      end associate
    end do
  end associate

  block
    character(len=*), parameter :: header(*) = [ character(len=len("temperatureRefurbished)")) :: &
      "timeRefurbished", "pressureRefurbished", "temperatureRefurbished", "mdotosRefurbished", "thrustRefurbished", &
      "volumeRefurbished"]
    refurbished_rocket = results_t(header, output)
  end block

end function refurbished_rocket
end module refurbished_rocket_module