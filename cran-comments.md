## Resubmission 
1. Added value tags to rd files
2. removed usage of ::: from examples
3. used message() instead of cat() in non print functions

## Test environments
* local ubuntu 22.04 install, R 4.1.2, 4.3.0
* local Windows 11   install, R 4.3.0
* ubuntu latest (on GitHub actions), R 4.0.0
* Windows Server 2022, R-devel, 64 bit (RHub)
* Fedora Linux, R-devel, clang, gfortran (RHub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (RHub)
* macOS 13.3.1 (22E261),Mac mini,Apple M1,R 4.3.0,clang-1403.0.22.14.1,GNU Fortran (GCC) 12.2.0 (mac-r-project) 

## Notes
There are no references to works/articles in my package that discuss the methods.  
This is because this package is built to convert legacy 'LaTeX` article to 
'Rmarkdown' based 'RJ-web-article' format.

Also there is a general requirement of pandoc >= v2.17. In case its lower than v2.17
the package will not execute the function, informing the user to upgrade pandoc.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
