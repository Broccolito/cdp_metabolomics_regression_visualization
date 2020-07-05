# CDP Metabolomics Visualization
This visualization uses R shiny and shinydashboard to visualize regressions between phenotypes and metabolites of CDP subjects.



### Usage

- Install R

  [R for Windows]https://cran.r-project.org/bin/windows/base/old/3.5.3/

  [R for Mac]https://cran.r-project.org/bin/macosx/

  In Ubuntu:

  ```bash
  sudo apt-get install r-base
  ```

- Install Shiny and required packages in R

  In R, if shiny is not installed, type in:

  ```R
  install.packages("shiny")
  library("shiny")
  ```

- Then, run Github instance by typing in:

  ```R
  runGitHub(repo = "cdp_metabolomics_regression_visualization", username = "Broccolito")
  ```


### License

Any unauthorized use of this program or its deidentified database is currently not allowed. This is currently an internal work with all rights reserved by the Simonson Lab at UCSD School of Medicine.



### Contributor

| Name      | Email           |
| --------- | --------------- |
| Wanjun Gu | wag001@ucsd.edu |