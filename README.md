# PMDK Fortran Interface
This repository implements a Fortran interface to the `libpmem` library from the PMDK set of libraries.

The aim is to allow persistent memory to be programmed directly from Fortran, rather than requiring every Fortran code developer to create their own interface or C/C++ code to exploit this functionality.

## Interface

The interface source code is in the `/src` directory. This can be built as standalone object files and then included in your build as required. It will also build a library file you can use.

## Test

There is a separate test directory that includes a small test code that uses the interface. This expects the persistent memory to be mounted in a location of form:
```
/mnt/pmem_fsdax0
```
Where the number at the end represents the processor socket/numa domain of the memory. For instance, this was tested in a system with 2 sockets, that had 2 sets of persist memory mounted at:
```
/mnt/pmem_fsdax0
/mnt/pmem_fsdax1
```
If your configuration is different you will need to modify the code that constructs the directory/filename path for the PMDK mount to be able to run this test.

## Example

This directory includes an example implementation of the STREAM benchmark that has been adapted to use persistent memory. It has the same caveats as the Test code, i.e. you may need to modify the location of the persistent memory in the example code if it does not match the system we developed this for.

## Compilation

All the directories have `Makefile`s that should build the code. You may need to modify them to change the compiler used, and you will need to update the following variables:
```
PMDK_LIBRARY=
PMDK_INCLUDE=
```
To specify the location of the PMDK `libpmem` library and include directories.

## Author

Adrian Jackson <a.jackson@epcc.ed.ac.uk>
