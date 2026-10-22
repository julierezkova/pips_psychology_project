# pips_psychology_project

This repository contains the code to simulate realistic clustered bivariate data sets, in both R and a user-friendly Shiny app. These data sets have a myriad of applications in psychological research including to evaluate methods, simulate phenomena, create test data, or even just to play around and explore some of the parameters that effect how data clusters.

This project was completed as part of the Programming in Psychological Science course (Fall 2026) of the Research Master Psychology program at University of Amsterdam.

A tutorial of how to generate clustered data in both R and through our Shiny app can be found in `main.qmd`. This script also includes an illustrative example of how clustered data generated using our code can be applied to specializations, such as clinical psychology.

## Setup and use

Clone the repository and confirm a sufficiently up-to-date R variant. The R variant 4.6.1 or newer is recommended.

```{bash}
git clone https://github.com/julierezkova/pips_psychology_project.git
cd pips_psychology_project
```

```{r}
install.packages(c("knitr", "rmarkdown", "quarto"))
```

Open `main.qmd` in RStudio and run its code chunks from top to bottom. The `setup` chunk will check if any required packages have not been installed, and will install them if necessary. All required packages will be loaded. Note that the code chunk containing the command to launch the Shiny app does not evaluate automatically, and will need to be run separately.

## Repository structure

*Only scripts potentially of use to the user are included here*

``` text
pips_psychology_project/
├── src/                        # R scripts
│   ├── app.R                      # Shiny app
│   ├── plot_clusters.R            # Cluster visualization
│   └── simulate.R                 # Clustered data generation
└── main.qmd                    # R main deliverable
```

## Dependencies

Repository set up requires `knitr`, `rmarkdown`, and `quarto` packages. Data generation and visualization in R requires `stats`, `utils` , and `ggplot2` packages. Loading of the Shiny required the additional packages `bslib` and `shiny`.

Individual scripts will install any packages not already installed, and load all dependencies.

## Group Contributions

| Description                            | R Scripts       | Contributer   |
| ---                                    | ---             |  ---          |
| Data simulation and visualization      | `simulate.R` <br>  `plot_clusters.R` <br> `test_simulate.R` | \@elakovac    |
| Shiny app and Github                   | `main.qmd` <br> `README.md` <br> `app.R` | @claireakelly |
| Data analysis and illustrative example | `analysis.R`  <br>  `main.qmd`  | @julierezkova |
