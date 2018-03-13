<a name="top"> </a>

[This document is formatted with GitHub-Flavored Markdown.     ]:#
[For better viewing, including hyperlinks, read it online at   ]:#
[https://github.com/gutmann/coarray_icar/blob/master/README.md ]:#

Coarray ICAR
============

[![National Center for Atmospheric Research][ncar logo]](https://ncar.ucar.edu)

This document provides information about the Coarray ICAR program.

CONTENTS
--------

* [Installation and testing](#installation-and-testing)
* [System requirements](#system-requirements)
* [Generating documentation](#generating-documentation)


Installation and testing
------------------------

The first execution creates three *.dat files and takes about 30 minutes usin 4 cores inside 
a Lubuntu Linux virtualmachine.  Subsequent executions will read the *.dat files and executive 
run much faster than the first run.

### Installation and execution via Make and GCC in a bash shell:
```bash
$ cd coarray_icar/src/tests 
$ export COMPILER=gnu
$ make USE_ASSERTIONS=.true.
$ cafrun -n 4 ./test-ideal
```
which will run the `test-ideal` executable program in 4 images.
### Installation via CMake and execution via CTest:

Compile and test coarray in a bash shell on a Linux, macOS, or the Windows Subsystem for Linux:
```bash
  cd <path-to-coarray-icar>  # Change directory to the coarray_icar source directory
  mkdir build            # create the build directory
  cd build
  FC=<compiler-command> cmake .. -DCMAKE_INSTALL_PREFIX=<coarray-icar-install-path>
  make -j <number-of-processes>
  ctest
```
where 
 * \<path-to-coarray-icar\> is the location of of the downloaded coarray ICAR source archive
 * \<number-of-processes\> is a count of the parallel processes that `make` should use, and
 * \<compiler-command\> is the command you use to invoke the compiler of your choice.
 * \<coarray-icar-install-path\> is the desired location for installing coarray ICAR 

For example, you might specify, `~/coarray_icar`, `4`, `caf`, and `~/bin`, respectively, if the Coarray 
ICAR source is in your home directoy, you want to accelerate the build by using 4 parallel processes, and 
you use the command `caf` to invoke the GNU Fortran compiler via the [OpenCoarrays] wrapper. 
Alternatively, specify `ifort` as the compiler command for to invoke the Intel Fortran compiler.

TODO: The CMake files need adjusting for building with non-GNU compilers.

### Build options
Append `-DNO_ASSERTIONS=ON` to the above `cmake` command to turn off runtime checking of assertions.
Including assertions enhances the runtime error checking at a significant cost (factor of 2 in runtime).

System requirements
-------------------

The following operating systems are supported: 
* Windows 10 Ubuntu Subsystem for Linux beta
* Linux
* macOS

The following compilers are supported: 
- Intel Fortran Compiler Version 16.0.0 or later
- [GNU Fortran Compiler] Version 6.3.0 (a regression prevents the use of later versions)
- Cray Fortran Compiler (untested)

The following build software is supported: 
- [CMake] 3.7 or later (required for Fortran submodule support)
OR
- GNU Makefile 

Generating documentation
------------------------
Install the [FORD] Fortran documentation generator and type
```bash
  ford doc-generator.md
```
which will create `doc/` subdirectory containing HTML files. Open
`doc/index.html` for the Coarray ICAR documentation. On Linux, you
might exeute the following at the command prompt: 
```bash
  firefox doc/index.html &
```

[Internal hyperlinks]:#

[Installation and testing]: #installation-and-testing
[System requirements]: #system-requirements

[External hyperlinks]:#
[FORD]: https://github.com/cmacmackin/ford

[GNU Fortran Compiler]: https://gcc.gnu.org/fortran/ 
[CMake]: https://www.cmake.org/
[ncar logo]: http://www.innovationews.com/content/uploads/2013/02/ncar-logo.jpg 
