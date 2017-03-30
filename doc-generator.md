---
project: Coarray ICAR Prototype
summary: An exploration of implementing a core ICAR algorithm, MPDATA, in coarray Fortran
src_dir: src/objects
src_dir: src/tests
src_dir: src/utilities
output_dir: doc
preprocess: true
display: public
         protected
         private
source: true
graph: true
sort: alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_download: https://github.com/sourceryinstitute/cme-214-homework-<username>/releases/latest
license: bsd
author: Ethan Gutmann 
email: gutmann@ucar.edu
author_description: Projet Scientist II
author_pic: http://xenon.colorado.edu/portal/images/people/Ethan_Gutmann.jpg 
project_github: https://github.com/gutmann/coarray_icar
project_download: https://github.com/gutmann/coarray_icar/releases/latest
license: bsd 
---

[source: display source code corresponding to item being documented]:#
[graph: generate call graphs, module dependency graphs, derive type composition/inheritance graphs ]:#
[sort: different sorting schemes for the modules or procedures or programs or derived types (alpha = alphabetical see wiki).]:#
[extra_mods: documentation for intrinsic modules]:#

[This document is a FORD project file, formatted with Pythonic Markdown                                      ]:#
[See https://github.com/cmacmackin/ford/wiki/Project-File-Options for more info on writing FORD project files]:#

--------------------


Brief description
-----------------

@warning
This archive uses Fortran 2015.  The GNU, Intel, and Cray compilers support all features at the time of this writing in March 2017. However, we will also evaluate Fortran 2015 features that only the GNU compiler supports such as asynchronous events (`event_type`, `event_query`, `event post`, and `event wait`).  These include event_type.

### ToDo

FORD generates HTML documentation for the following directories: 
 - [ ] [tests](./test/pargen)
 - [ ] [objects](./src/objects)
 - [X] [utilities](./utilities)
where an X marks directories in which we have also completed developer-written documentation that FORD parses and incorporates into the HTML files. In the remaining directories FORD generates documentation automatically based on the source code without a complete set of additional developer comments in the format expected by FORD.

Compilers
---------

This archive has been tested with the GNU 6.3.0 and Intel 17 Fortran compilers.
