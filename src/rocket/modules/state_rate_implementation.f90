submodule(state_rate_interface) state_rate_implementation
  use kind_parameters, only : rkind
  implicit none

contains

  module procedure new_rate
    new_rate%time_rate_ = time_rate
    new_rate%mass_rate_ = mass_rate
    new_rate%energy_rate_ = energy_rate
    new_rate%burn_depth_rate_ = burn_depth_rate
  end procedure

  module procedure post_multiply
    use state_interface, only : state_t

    this_x_rhs = state_t( &
      mass       = this%mass_rate_*rhs, &
      energy     = this%energy_rate_*rhs, &
      burn_depth = this%burn_depth_rate_*rhs, &
      time       = this%time_rate_*rhs &
    )
  end procedure

  module procedure pre_multiply
    use state_interface, only : state_t

    lhs_x_this = state_rate_t( &
      mass_rate       = lhs*this%mass_rate_, &
      energy_rate     = lhs*this%energy_rate_, &
      burn_depth_rate = lhs*this%burn_depth_rate_, &
      time_rate       = lhs*this%time_rate_ &
    )
  end procedure

  module procedure add
    total%time_rate_       = this%time_rate_       + rhs%time_rate_
    total%mass_rate_       = this%mass_rate_       + rhs%mass_rate_
    total%energy_rate_     = this%energy_rate_     + rhs%energy_rate_
    total%burn_depth_rate_ = this%burn_depth_rate_ + rhs%burn_depth_rate_
  end procedure

end submodule state_rate_implementation
