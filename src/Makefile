compile := ifort
opt := -coarray=shared -standard-semantics 
#compile := caf 
#opt := -O3 -Wall -fcheck=all
#libs := -lsourcery
link := $(compile)
executable := test-skeleton
coarray_icar_root := ..
src_dir := $(coarray_icar_root)/src
install_dir := $(coarray_icar_root)/bin
object_files_dir := $(coarray_icar_root)/obj
mod_dir := $(coarray_icar_root)/mod

#lib_dirs := -L $(homework_root)/lib
#mod_dirs := -I $(homework_root)/mod

# Locate required source files
source_files := $(shell ls *.f90)
#source_files := $(subst src/,,$(source_files)) # Eliminate paths 

# Swap .f90 file extensions for .o extensions 
object_files := $(subst .f90,.o,$(source_files))

# Executable depends on all object files
$(executable): $(object_files)
	$(link) $(opt) $(object_files) -o $@ $(lib_dirs) $(libs)

install: $(executable)
	mkdir -p $(install_dir)
	cp $(executable) $(install_dir)

# All dependencies are interfaces or libraries (no implementation depends on another implementation)
main.o: domain_interface.o assertions_interface.o

domain_interface.o: configuration_interface.o

domain_implementation.o: domain_interface.o assertions_interface.o 

configuration_implementation.o: configuration_interface.o

assertions_implementation.o: assertions_interface.o

# Compile dependencies from procedural fireworks homework assignment
%.o: %.f90
	$(compile) $(opt) -c $< $(lib_dirs) $(libs) $(mod_dirs)

.PHONY : clean uninstall

clean :
	-rm -f  *.o *.mod *.smod $(executable)

uninstall:
	-rm -f  $(install_dir)/$(executable) 
