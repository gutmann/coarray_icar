# Usage:
#
#    make USE_ASSERTIONS=.false. # Defaults to .true. if not provided

# Example of printing a variable named "var" delimited by square braces
# $(info $$var is [${var}])

ifeq ($(USE_ASSERTIONS),)
  USE_ASSERTIONS:=.false.
endif

executable := test-initialization
# Options: gnu, intel
compiler := gnu
# compiler := intel

# Specify the compiler invocation name and flags
ifeq ($(compiler),gnu)
  compile := caf
  opt := -fcheck=all -Wall -fbacktrace -g -cpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS)
  link_flags := -fcheck=all -Wall -fbacktrace -g
  opt := -Ofast -cpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS)
  link_flags := -Ofast
  lib :=
else
  ifeq ($(compiler),intel)
    compile := mpif90
    # opt := -standard-semantics -coarray=shared -check all -traceback -g -fpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS)
	opt := -standard-semantics -coarray -check all -traceback -g -fpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS)
    link_flags := -standard-semantics -coarray -check all -traceback -g
    lib :=
  endif
endif
link := $(compile)

objects_dir=../objects
utilities=../utilities
physics=../physics
tests_dir=.

coarray_icar_root := ../..
root := $(HOME)
lib_dirs := -L $(root)/lib
mod_dirs := -I $(root)/mod
install_dir := $(root)/bin


# Locate required source files
objects_source := $(shell ls $(objects_dir)/*.f90)
bare_objects_source := $(subst $(objects_dir)/,,$(objects_source)) # Eliminate paths

utilities_source := $(shell ls $(utilities)/*.f90)
bare_utilities_source := $(subst $(utilities)/,,$(utilities_source)) # Eliminate paths

physics_source := $(shell ls $(physics)/*.f90)
bare_physics_source := $(subst $(physics)/,,$(physics_source)) # Eliminate paths

tests_source := $(shell ls $(tests_dir)/*.f90)
bare_tests_source := $(subst $(tests_dir)/,,$(tests_source)) # Eliminate paths

source_files := $(bare_objects_source) $(bare_utilities_source) $(bare_tests_source) $(bare_physics_source)

# Swap .f90 file extensions for .o extensions
object_files := $(subst .f90,.o,$(source_files))

# Executable depends on all object files
$(executable): $(object_files)
	$(link) $(object_files) -o $@ $(link_flags)

test-timer: test-timer.o timer_interface.o timer_implementation.o
	$(link) $^ -o $@ $(link_flags)


install: $(executable)
	mkdir -p $(install_dir)
	cp $(executable) $(install_dir)

# All dependencies are interfaces or libraries (no implementation should depend on another implementation)
test-initialization.o: domain_interface.o assertions_interface.o mp_driver.o

test-timer.o: timer_interface.o

timer_implementation.o: timer_interface.o

assertions_implementation.o: assertions_interface.o

domain_interface.o: configuration_interface.o exchangeable_interface.o

exchangeable_implementation.o: exchangeable_interface.o
configuration_implementation.o: configuration_interface.o

domain_implementation.o: domain_interface.o assertions_interface.o

mp_driver.o: mp_thompson.o domain_interface.o



# Prefer files in the present working directory (picks the local main over another main)
%.o: %.f90
	$(compile) $(opt) -c $<

# Compile dependencies from utilities directory
%.o: $(utilities)/%.f90
	$(compile) $(opt) -c $<

# Compile dependencies from physics directory
%.o: $(physics)/%.f90
	$(compile) $(opt) -c $<

# Compile dependencies from objects
%.o: $(objects_dir)/%.f90
	$(compile) $(opt) -c $<

.PHONY : clean uninstall

clean :
	-rm -f  *.o *.mod *.smod $(executable)

uninstall :
	-rm -f  $(install_dir)/$(executable)
