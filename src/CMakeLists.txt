set(directories_to_build tests)
foreach(directory ${directories_to_build})
  add_subdirectory(${directory})
endforeach()
