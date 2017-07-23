# Usage:
#
#    make USE_ASSERTIONS=.false. # Defaults to .true. if not provided

# Example of printing a variable named "var" delimited by square braces
# $(info $$var is [${var}])

ifeq ($(USE_ASSERTIONS),)
  USE_ASSERTIONS:=.false.
endif

BUILD=../build
objects_dir=../objects
utilities=../utilities
physics=../physics
tests_dir=.

coarray_icar_root := ../..
root := $(HOME)
lib_dirs := -L $(root)/lib
mod_dirs := -I $(root)/mod
install_dir := $(root)/bin


executable := test-ideal test-initialization test-timer
# Options: gnu, intel
compiler := gnu
# compiler := intel

# Specify the compiler invocation name and flags
ifeq ($(compiler),gnu)
  compile := caf
  opt := -fcheck=all -Wall -fbacktrace -g -cpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS) -J $(BUILD)/
  link_flags := -fcheck=all -Wall -fbacktrace -g
  # opt := -Ofast -cpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS) -J $(BUILD)/
  # link_flags := -Ofast
  lib :=
else
  ifeq ($(compiler),intel)
    compile := mpif90
    # opt := -standard-semantics -coarray=shared -check all -traceback -g -fpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS)
	opt := -standard-semantics -coarray -check all -traceback -g -fpp -DUSE_ASSERTIONS=$(USE_ASSERTIONS) -module $(BUILD)/
    link_flags := -standard-semantics -coarray -check all -traceback -g
    lib :=
  endif
endif
link := $(compile)


# Locate required source files
# objects_source := $(shell ls $(objects_dir)/*.f90)
# bare_objects_source := $(subst $(objects_dir)/,,$(objects_source)) # Eliminate paths
#
# # Swap .f90 file extensions for .o extensions
# object_files := $(subst .f90,.o,$(build_source_files))

# Executable depends on object files
test-ideal: \
			$(BUILD)/domain_implementation.o		\
			$(BUILD)/domain_interface.o				\
			$(BUILD)/exchangeable_implementation.o	\
			$(BUILD)/exchangeable_interface.o		\
			$(BUILD)/timer_implementation.o			\
			$(BUILD)/timer_interface.o				\
			$(BUILD)/assertions_implementation.o	\
			$(BUILD)/assertions_interface.o			\
			$(BUILD)/configuration_implementation.o	\
			$(BUILD)/configuration_interface.o		\
			$(BUILD)/test-ideal.o					\
			$(BUILD)/grid_interface.o				\
			$(BUILD)/grid_implementation.o			\
			$(BUILD)/mp_driver.o					\
			$(BUILD)/mp_thompson.o
	$(link) $^ -o $@ $(link_flags)

test-initialization: \
					$(BUILD)/domain_implementation.o 		\
					 $(BUILD)/domain_interface.o 			\
					 $(BUILD)/exchangeable_implementation.o	\
					 $(BUILD)/exchangeable_interface.o		\
					 $(BUILD)/assertions_implementation.o	\
					 $(BUILD)/assertions_interface.o		\
					 $(BUILD)/configuration_implementation.o\
					 $(BUILD)/configuration_interface.o		\
					 $(BUILD)/test-initialization.o			\
					 $(BUILD)/grid_interface.o				\
					 $(BUILD)/grid_implementation.o			\
					 $(BUILD)/mp_driver.o					\
					 $(BUILD)/mp_thompson.o
	$(link) $^ -o $@ $(link_flags)

test-timer: \
			$(BUILD)/timer_interface.o	\
			$(BUILD)/test-timer.o		\
			$(BUILD)/timer_implementation.o
	$(link) $^ -o $@ $(link_flags)


install: $(executable)
	mkdir -p $(install_dir)
	cp $(executable) $(install_dir)

# All dependencies are interfaces or libraries (no implementation should depend on another implementation)
$(BUILD)/test-initialization.o: test-initialization.f90 		\
								$(BUILD)/domain_interface.o     \
								$(BUILD)/assertions_interface.o \
								$(BUILD)/mp_driver.o			\
								$(BUILD)/grid_interface.o		\
								$(BUILD)/timer_interface.o
	$(compile) $(opt) -c $< -o $@

$(BUILD)/test-ideal.o: 	test-ideal.f90					\
						$(BUILD)/domain_interface.o 	\
						$(BUILD)/assertions_interface.o \
						$(BUILD)/mp_driver.o			\
						$(BUILD)/grid_interface.o		\
						$(BUILD)/timer_interface.o
	$(compile) $(opt) -c $< -o $@

$(BUILD)/test-timer.o: test-timer.f90 $(BUILD)/timer_interface.o
	$(compile) $(opt) -c $< -o $@


$(BUILD)/mp_driver.o: 					$(BUILD)/mp_thompson.o 				\
										$(BUILD)/domain_interface.o

$(BUILD)/grid_implementation.o: 		$(BUILD)/grid_interface.o

$(BUILD)/timer_implementation.o: 		$(BUILD)/timer_interface.o

$(BUILD)/assertions_implementation.o: 	$(BUILD)/assertions_interface.o

$(BUILD)/exchangeable_interface.o: 		$(BUILD)/grid_interface.o

$(BUILD)/exchangeable_implementation.o: $(BUILD)/exchangeable_interface.o 	\
										$(BUILD)/grid_interface.o

$(BUILD)/configuration_implementation.o: $(BUILD)/configuration_interface.o

$(BUILD)/domain_interface.o: 			$(BUILD)/configuration_interface.o 	\
										$(BUILD)/exchangeable_interface.o	\
										$(BUILD)/grid_interface.o

$(BUILD)/domain_implementation.o: 		$(BUILD)/domain_interface.o 		\
										$(BUILD)/assertions_interface.o		\
										$(BUILD)/grid_interface.o


$(info $$BUILD        = ${BUILD})
$(info $$tests_dir    = ${tests_dir})
$(info $$objects_dir  = ${objects_dir})
$(info $$utilities    = ${utilities})
$(info $$physics      = ${physics})

# Prefer files in the present working directory (picks the local main over another main)
# %.o: %.f90
# 	$(compile) $(opt) -c $< -o $@



%.o: $(tests_dir)/%.f90
	$(compile) $(opt) -c $< -o $@

# Compile dependencies from utilities directory
%.o: $(utilities)/%.f90
	$(compile) $(opt) -c $< -o $@

# Compile dependencies from physics directory
%.o: $(physics)/%.f90
	$(compile) $(opt) -c $< -o $@

# Compile dependencies from objects
%.o: $(objects_dir)/%.f90
	$(compile) $(opt) -c $< -o $@

.PHONY : clean uninstall

clean :
	-rm -f  $(BUILD)/*.o $(BUILD)/*.mod $(BUILD)/*.smod $(executable)

uninstall :
	-rm -f  $(install_dir)/$(executable)
