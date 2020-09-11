module state_rate_interface
  !! Encapsulate the time rate of change of the state variables that are defined as
  !! state_t components.  These rate variables are used in the main program,
  !! where a state_rate_t object gets multiplied by the time step:
  !! motor%d_dt(state)*dt
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: state_rate_t
    private
    real(rkind) mass_rate_       !! chamber mass increase per unit time
    real(rkind) energy_rate_     !! chamber internal energy increase per unit time
    real(rkind) burn_depth_rate_ !! surface-normal burn depth increase per unit time
    real(rkind) time_rate_       !! time increase per unit time (always 1)
  contains
    procedure, private :: add
    procedure, private :: post_multiply
    procedure, private, pass(this) :: pre_multiply
    generic :: operator(*) => post_multiply, pre_multiply
    generic :: operator(+) => add
  end type

  interface state_rate_t
    module procedure new_rate
  end interface

  interface

    pure module function new_rate(time_rate, mass_rate, energy_rate, burn_depth_rate)
      !! result is a newly constructured state_rate_t object
      implicit none
      real(rkind), intent(in) :: time_rate, mass_rate, energy_rate, burn_depth_rate
      type(state_rate_t) new_rate
    end function

    pure module function post_multiply(this, rhs) result(this_x_rhs)
      !! result has components computed from multiply each rhs component by dt
      use state_interface, only : state_t
      implicit none
      class(state_rate_t), intent(in) :: this
      real(rkind), intent(in) :: rhs
      type(state_t) this_x_rhs
    end function

    pure module function pre_multiply(lhs, this) result(lhs_x_this)
      !! result has components computed from multiply each rhs component by dt
      use state_interface, only : state_t
      implicit none
      class(state_rate_t), intent(in) :: this
      integer, intent(in) :: lhs
      type(state_rate_t) lhs_x_this
    end function

    pure module function add(this, rhs) result(total)
      !! result has components computed from summing each pair of rhs & lhs components
      use state_interface, only : state_t
      implicit none
      class(state_rate_t), intent(in) :: this
      type(state_rate_t), intent(in) :: rhs
      type(state_rate_t) total
    end function

  end interface

end module state_rate_interface
