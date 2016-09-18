<img width="900" src="/inst/doc/SciencesPo_logo.jpg" alt="SciencesPo" />




| [License](http://www.gnu.org/licenses/gpl-2.0.html) | [Version](http://www.r-pkg.org/badges/version/SciencesPo) | [Build Status](https://travis-ci.org) | [Rate](https://cran.r-project.org/package=SciencesPo) | [Zenodo](https://zenodo.org/) |
| :---- | :----: | :----: | :----: | :----: | 
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)  | [![Version](http://www.r-pkg.org/badges/version/SciencesPo)](http://cran.r-project.org/package=SciencesPo) | [![Build Status](https://travis-ci.org/danielmarcelino/SciencesPo.svg)](https://travis-ci.org/danielmarcelino/SciencesPo) |  [![Downloads](http://cranlogs.r-pkg.org/badges/SciencesPo)]( https://cran.r-project.org/package=SciencesPo) |  [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.54876.svg)](http://dx.doi.org/10.5281/zenodo.54876) | 
| *License* | *Latest CRAN version* | *Build status*  | *CRAN Downloads* | *Independently-hosted Archive* |




**SciencesPo** is a facility package for the political science crowd. It provides a variety of functions for analyzing elections and political behavior data, including measures of political fragmentation, seat apportionment, and  small data visualization graphs. The package lives on the R Foundation repository [(CRAN)](https://cran.r-project.org/package=SciencesPo), and its development version hosted on [Github](http://github.com/danielmarcelino/SciencesPo). To install it, you can use the following methods.

1 - From the CRAN repository (stable version):

  ```
  install.packages('SciencesPo', dep=TRUE)

  library(SciencesPo)
  ```

2 -  You can install the latest development version using the nifty function from devtools package.

```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("danielmarcelino/SciencesPo")
```

3 - Or download the [sources in a zip](https://github.com/danielmarcelino/SciencesPo/zipball/master) file and build manually. To do so, please unzip the file to an empty dir and run the following commands there:


```
R CMD build SciencesPo
R CMD INSTALL SciencesPo_*.tar.gz
```

Please note that the package contains some C code and thus you need a development environment to build the package. If you're running R on Windows, you need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/ ). Once you have installed `Rtools`, issue following command in command prompt:

```
R CMD build --binary <path to .tar.gz file>
R CMD INSTALL <path to .zip file>
```

## Helping Out
**SciencesPo** is intended to be a useful project for the social sciences community. Contributions are welcome.

If you're familar with GitHub and R packages, feel free to submit a [pull request](https://github.com/danielmarcelino/SciencesPo/pulls). If you'd like to report a bug or make a suggestion, please create a GitHub [issue](https://github.com/danielmarcelino/SciencesPo/issues); issues are a usually a good place to ask public questions too.

## Usage

For a brief introduction to **SciencesPo** functionality, run:

```
demo(SciencesPo)
```


To see what functions are implemented in **SciencesPo**, run:

```
help(package=SciencesPo)
```


## Vignettes
* [Concepts and Basics of SciencesPo](https://cran.r-project.org/web/packages/SciencesPo/vignettes/SciencesPo.html)
* [Descriptive Stats]
* [Cross-Tabulation]
* [Are My Data Normal?]
* [Measures of Political Behavior](https://cran.r-project.org/web/packages/SciencesPo/vignettes/Indices.html)
* [The Anatomy of a Plot](https://cran.r-project.org/web/packages/SciencesPo/vignettes/Viz.html)
