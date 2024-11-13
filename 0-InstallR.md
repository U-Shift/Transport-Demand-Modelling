How to install R and RStudio
================

## Installation

You should install R before installing RStudio.

### Install R

1.  Go to world wide CRAN mirror: <https://cloud.r-project.org/>
2.  Select you OS
3.  Select “base” (*if you want to install R for the first time*)
4.  Download and run

> **If you use Windows: You will also need to install** [Rtools](https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html), which is a collection of tools necessary to build R packages in Windows.

After this installation, you don't need to open R base. Please proceed to install RStudio.

### Install RStudio

This is a wrapper for a friendly usage of R.

1.  Go to RStudio download website:
    <https://rstudio.com/products/rstudio/download/>
2.  Select the Free RStudio Desktop version
3.  Download and run

## Install basic packages

Open RStudio (**not R** !).

If you type `sessionInfo()` at the console, it will show you the
packages you have with your installation.

You can install packages by the menu on the **Packages** tab  
![image](https://user-images.githubusercontent.com/39107166/98879689-08f9cd00-247e-11eb-814c-cc29534d7e21.png)

Try to install `readxl`(to deal with MS Excel files). It will install
also some dependent packages.

You may also type in the console the packages or group of packages you’d
like to install. Try to install these three packages with the console:

-   `tidyverse`- a group of basic packages to deal with data trames and
    graphics,
-   `RColorBrewer`- a package to use beautiful color palettes, and
-   `devtools`- a package to deal with the development versions of
    packages

Type

``` r
install.packages(c("tidyverse","devtools","RColorBrewer"))
```

> Even if you want only one package, the command is in plural :)

## Load packages

Simply by calling them;

``` r
library(tidyverse)
librar(readxl)
...
```

When you restart R (`Ctrl + Shift + F10`) you’ll need to load packages
again.

## The R dashboard

### R script

You can click here at the upper left icon, and select R Script  
![image](https://user-images.githubusercontent.com/39107166/98965420-edd0a100-2501-11eb-8e22-974053312fd4.png)  
Or `Ctrl + Shift + F`  
Or in the console, type `file.edit("startup.R)`

Start by typing the aim of that script, commented with a cardinall
(**\#**)  
You can comment/uncomment any line by selecting it and press
`Ctrl + Shift + C`

Save the script.

### Working directory

Step by step:

1.  click on “Session”;
2.  Click on “Set working directory”;
3.  Click on “Choose directory” and select the folder that contains the
    dataset.

Or you could just run

``` r
setwd("D:/TDM/Transport-Demand-Modelling") #for instance
```

#### Run a script

### Environment

And .RData

### Flavors

`Tools > Global Options > Appearence > Editor Theme > ...`

Try to change to “Cobalt”, or check others you may like.  
![](RmdFiles/0-InstallR/Cobalt.PNG)

You can also change font, font-size, and even the panels layout.

<!-- > See [r.rosafelix.bike](http://r.rosafelix.bike/) for some R tips to use in Transport Demand Modeling ! (_em português_) -->
