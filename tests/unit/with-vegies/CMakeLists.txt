add_executable( test-with-vegies
  main.f90
  nozzle_class_test.f90
  state_class_test.f90
  Vegetables_m.f90
  ISO_VARYING_STRING.f90
  strff.f90
)
target_link_libraries( test-with-vegies
  PRIVATE utilities
  PRIVATE rocket
)
