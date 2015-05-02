# SciencesPo  [![Build Status](https://travis-ci.org/danielmarcelino/SciencesPo.svg)](https://travis-ci.org/danielmarcelino/SciencesPo)  [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

_SciencesPo_ is a facility package for the political science crowd. It provides a variety of functions for analyzing random and nonrandom data.

The _SciencesPo_  package lives on the R Foundation repository [(CRAN)](http://cran.r-project.org/web/packages/SciencesPo/index.html) and is also hosted on [Github](http://github.com/danielmarcelino/SciencesPo). To install it, you can use the following methods.

1 - From the CRAN repository:

  ```
  install.packages('SciencesPo',repos='http://cran.r-project.org')
  require(SciencesPo)
  ```

2 -  You can always download the latest development version using the nifty function from devtools package.


  ```
  require(devtools)
  install_github("danielmarcelino/SciencesPo")
  ```
  
3 - Or download the [sources in a zip](https://github.com/danielmarcelino/SciencesPo/zipball/master) file and build manually. To do so, please unzip the file to an empty dir and run the following commands there:


```
R CMD build SciencesPo
R CMD INSTALL SciencesPo_*.tar.gz
```

If you're running R on Windows, you need to install [Rtools](http://cran.stat.ucla.edu/bin/windows/Rtools/ ). Once you have installed `Rtools`, issue following command in command prompt:

```
R CMD build --binary <path to .tar.gz file>
R CMD INSTALL <path to .zip file>
```

## Usage

For a brief introcution to _SciencesPo_ functionality, run:

```
demo(SciencesPo)
```



