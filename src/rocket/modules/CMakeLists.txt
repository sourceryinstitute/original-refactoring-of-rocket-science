add_library(rocket_parts
  module_variables.f90
  subroutines.f90
  rocket_function.f90
)
target_link_libraries(rocket_parts utilities)
