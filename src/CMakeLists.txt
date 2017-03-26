set(directories_to_build tests utilities)
foreach(directory ${directories_to_build})
  add_subdirectory(${directory})
endforeach()
