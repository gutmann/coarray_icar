executable := test-initialization
#compile := caf 
compile := ifort
link := $(compile)
link_flags := -coarray=shared
opt := -O3 -Wall -fcheck=all
opt := -standard-semantics -coarray=shared -check all -traceback
libs := 

objects_dir=../objects
utilities=../utilities
tests_dir=.

coarray_icar_root := ../..
lib_dirs := -L $(homework_root)/lib
mod_dirs := -I $(homework_root)/mod
install_dir := $(homework_root)/bin

# Locate required source files
objects_source := $(shell ls $(objects_dir)/*.f90)
bare_objects_source := $(subst $(objects_dir)/,,$(objects_source)) # Eliminate paths 

utilities_source := $(shell ls $(utilities)/*.f90) 
bare_utilities_source := $(subst $(utilities)/,,$(utilities_source)) # Eliminate paths 

tests_source := $(shell ls $(tests_dir)/*.f90) 
bare_tests_source := $(subst $(tests_dir)/,,$(tests_source)) # Eliminate paths 

source_files := $(bare_objects_source) $(bare_utilities_source) $(bare_tests_source)

# Swap .f90 file extensions for .o extensions 
object_files := $(subst .f90,.o,$(source_files))

# Executable depends on all object files and on the sourcery library
$(executable): $(object_files) #$(lib_dirs)/libx.a
	$(link) $(object_files) -o $@ $(link_flags) #$(lib_dirs) $(libs)

install: $(executable)
	mkdir -p $(install_dir)
	cp $(executable) $(install_dir)

# All dependencies are interfaces or libraries (no implementation depends on another implementation)
test-initialization.o: domain_interface.o assertions_interface.o

assertions_implementation.o: assertions_interface.o

domain_interface.o: configuration_interface.o exchangeable_interface.o

domain_implementation.o: domain_interface.o assertions_interface.o 

# Prefer files in the present working directory (picks the local main over another main)
%.o: %.f90
	$(compile) $(opt) -c $<  #$(mod_dirs) $(lib_dirs) $(libs)

# Compile dependencies from utilities directory
%.o: $(utilities)/%.f90
	$(compile) $(opt) -c $< #$(mod_dirs) $(lib_dirs) $(libs)

# Compile dependencies from objects
%.o: $(objects_dir)/%.f90
	$(compile) $(opt) -c $<  #$(mod_dirs) $(lib_dirs) $(libs)

.PHONY : clean uninstall

clean :
	-rm -f  *.o *.mod *.smod $(executable)

uninstall :
	-rm -f  $(install_dir)/$(executable)
