add_executable( test-nozzle-class
  test-nozzle-class.f90
)
target_link_libraries( test-nozzle-class
  PRIVATE utilities
  PRIVATE rocket
)
