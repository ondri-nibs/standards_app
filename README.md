README
================

Jedid Ahn, Roberto Lentini, Logan Lim, Jeremy Tanuan, & Derek Beaton,
2021JAN25

# Standards (Shiny) App <img src='etc/logo.png' align="right" height="139"/>

The Standards app is a heavy-duty app meant to cover multiple bases for
the ONDRI project and beyond. The current standards app will perform
standards checks in accordance with:

  - The general data structure we use

  - OND01

  - BEAM

  - OND06/REMINDD

  - and custom projects (with user input)

Additional information, documentation, and guides coming soon.

<hr>

  - Install [R](https://cran.r-project.org/) first and then
    [RStudio](https://rstudio.com/products/rstudio/download/). Please
    choose the correct installer carefully as it will depend on your
    computer’s operating system.

<br>

  - Install the `rowr` package (which is no longer available through
    CRAN) with the following lines of code:

<!-- end list -->

``` 
  if (!require("rowr")){
      utils::install.packages("https://cran.r-project.org/src/contrib/Archive/rowr/rowr_1.1.3.tar.gz",
                              repos = NULL, type = "source")
  }
```

<br>

  - Download and install the shiny app directly with the following lines
    of code:

<!-- end list -->

``` 
  if (!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github(repo = "ondri-nibs/standards_app")
```

If you get the following message in your RStudio console, please type 3.
<br><br> <img src='etc/package-update.png'>

<br>

  - Type `ONDRIStandardsApp::installPackages()` to install any missing
    packages and/or dependencies. Please type 3 again if you get the
    message above.

<br>

  - When installation is complete, type `ONDRIStandardsApp::runApp()` to
    open the app.
