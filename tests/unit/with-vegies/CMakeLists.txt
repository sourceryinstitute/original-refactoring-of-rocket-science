add_executable( test-with-vegies
  main.f90
  nozzle_class_test.f90
  state_class_test.f90
)
target_link_libraries( test-with-vegies
  PRIVATE utilities
  PRIVATE rocket
  PRIVATE vegetables
)
