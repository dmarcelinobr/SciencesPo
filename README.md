<img src="/inst/doc/SciencesPo_logo.png" alt="SciencesPo" />

[![CRAN Version](http://www.r-pkg.org/badges/version/SciencesPo)](http://cran.r-project.org/package=SciencesPo)
[![Build Status](https://travis-ci.org/danielmarcelino/SciencesPo.svg)](https://travis-ci.org/danielmarcelino/SciencesPo)  [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
 ![](http://cranlogs.r-pkg.org/badges/grand-total/SciencesPo) 
 [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11474.svg)](http://dx.doi.org/10.5281/zenodo.11474)

_SciencesPo_ is a facility package for the political science crowd. It provides a variety of functions for analyzing random and nonrandom data. The package lives on the R Foundation repository [(CRAN)](http://cran.r-project.org/package=SciencesPo/) and is also hosted on [Github](http://github.com/danielmarcelino/SciencesPo). To install it, you can use the following methods.

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

Please note that the package contains some C code and thus you need a development environment to build the package. If you're running R on Windows, you need to install [Rtools](http://cran.stat.ucla.edu/bin/windows/Rtools/ ). Once you have installed `Rtools`, issue following command in command prompt:

```
R CMD build --binary <path to .tar.gz file>
R CMD INSTALL <path to .zip file>
```

## Usage

For a brief introcution to _SciencesPo_ functionality, run:

```
demo(SciencesPo)
```


To see what functions are implemented in _SciencesPo_, run:

```
help(package=SciencesPo)
```

## Visualization Instances
To quickly get our feet wet with creating charming charts, we start with a minimal example.



### Voronoi diagram

```voronoi(p=2, n=20, dim=1000)```

<img src="/inst/doc/voronoi.png" alt="Voronoi Diagram" />


### Sample Power 

```sample.power(mu0=68, mu1=69, sigma=3.1, n=100)```

<img src="/inst/doc/sample.power.png" alt="Sample Power Graph" />


### Plots for count data



