add_executable( test-state-class
  test-state-class.f90
)
target_link_libraries( test-state-class
  PRIVATE utilities
  PRIVATE rocket
)
