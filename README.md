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

### Installation for Linux/OSX Operating Systems

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

For example, you might specify, `~/coarray_icar`, `4`, and `caf`, respectively, if Coarray 
ICAR your home directoy, you want to accelerate the build by using 4 parallel processes, and 
you use the command `caf` to invoke the [OpenCoarrays] wrapper for the GNU Fortran compiler. 
Alternatively specify, `ifort` as the compiler command for to invoke the Intel Fortran compiler.

TODO: The CMake files need adjusting for building with non-GNU compilers. 

### Build options
Append `-DNO_ASSERTIONS=ON` to the above `cmake` command to turn off runtime checking of assertions.

System requirements
-------------------

The following operating systems are supported: 
* Windows 10 Ubuntu Subsystem for Linux beta
* Linux
* macOS

The following compilers are supported: 
- Intel Fortran Compiler Version 16.0.0 or later
OR 
- [GNU Fortran Compiler] Version 6.1.0 or later
- GNU Makefile
- [CMake] 3.7 or later (required for Fortran submodule support)
OR 
- Cray Fortran Compiler (untested)

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
