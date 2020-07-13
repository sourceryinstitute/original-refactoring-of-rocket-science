module gas_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP

  implicit none
  real(DP), parameter :: Ru=8314_DP
  private

  public :: gas_t
  public :: define
  public :: get_c_p
  public :: get_T
  public :: get_MW
  public :: get_m
  public :: R_gas
  public :: c_v
  public :: g

  type gas_t
    private
    real(DP) :: c_p, T, MW, m
  end type

  interface define
    module procedure define_gas
  end interface

contains

  subroutine define_gas(this, file_name)
    type(gas_t), intent(out) :: this
    character(len=*), intent(in) :: file_name
    character(len=max_errmsg_len) error_message
    real(DP) :: c_p, T, MW, m
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/gas/ c_p, MW, T, m

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "gas%define: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=gas)
    close(file_unit)

    this%c_p = c_p
    this%MW = MW
    this%T = T
    this%m = m
  end subroutine

  function get_c_p(this) result(this_c_p)
    type(gas_t), intent(in) :: this
    real(DP) :: this_c_p
    this_c_p = this%c_p
  end function

  function get_T(this) result(this_T)
    type(gas_t), intent(in) :: this
    real(DP) :: this_T

    this_T = this%T
  end function

  function get_MW(this) result(this_MW)
    type(gas_t), intent(in) :: this
    real(DP) :: this_MW

    this_MW = this%MW
  end function

  function get_m(this) result(this_m)
    type(gas_t), intent(in) :: this
    real(DP) :: this_m

    this_m = this%m
  end function

  function R_gas(this) result(Rgas)
    type(gas_t), intent(in) :: this
    real(DP) Rgas
    real(DP), parameter :: R_universal = 8.31446261815324_DP

    Rgas = R_universal/this%MW
  end function

  function c_v(this) result(this_c_v)
    type(gas_t), intent(in) :: this
    real(DP) this_c_v

    this_c_v = R_gas(this) - this%c_p
  end function

  function g(this) result(gamma)
    type(gas_t), intent(in) :: this
    real(DP) gamma

    gamma = this%c_p/c_v(this)
  end function

end module gas_module
