## Submission
This is a submission for an updated version of this package.

## Test environments
- Windows Server 2022, R-devel, 64 bit
- Fedora Linux, R-devel, clang, gfortran
- Ubuntu Linux 20.04.1 LTS, R-release, GCC
- macOS 10.13.6 High Sierra, R-release, brew

## R CMD check results

There were no ERRORS or WARNINGS

There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit):

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
As mentioned in the [R-hub issue #503](https://github.com/r-hub/rhub/issues/503),
this warning could occur due to a bug in MiKTeX and can likely be ignored.

There is one NOTE that is only found on (Fedora Linux, R-devel, clang, gfortran)

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

As described [here](https://groups.google.com/g/r-sig-mac/c/7u_ivEj4zhM),
our understanding is that this is an old bug/issue which is located at the
testing environment.

## Local check - with devtools::check()
-- R CMD check results  concstats 0.1.6 ----
Duration: 2m 45.4s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

* This is a new release.
