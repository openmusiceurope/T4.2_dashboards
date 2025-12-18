[![DOI](https://zenodo.org/badge/1024804070.svg)](https://doi.org/10.5281/zenodo.16360364)
# OpenMusE project

This GitHub repository is part of the [Open Music Europe](https://www.openmuse.eu) (OpenMusE) project. 
The project aims to make the European music industry more competitive, fair, sustainable and transparent.

## Downloading the repository

There are few ways to download the content of the repository. First one is to clone the repository to 
your computer. This can be done by running the following code in the terminal.

```
git clone https://github.com/openmusiceurope/OpenMusE_WP4_UTU.git
```

Another option is to download the content of the repository as a ZIP. This can be done by the green 
code button and selecting Download ZIP from the available options.

After downloading the contents of the repository you can open the quarto files in your local R 
environment to run the codes yourself.

## Directory structure

```
├── README.md                     # project overview
├── input/                        # contains data for the project
│   ├── data_raw/                 # raw input data
│      ├── venue_data             # unprocessed venue survey data
│      └── census_data            # unprocessed live music census data
│   ├── data_processed/           # processed input data
|      └── translated_questions   # translated open questions from the survey datasets
│   └── other/                    # contains other data files
├── code/                         # contains code files
│   ├── Census_dashboard/         # contains R file and data for running the live music census dashboard
|   ├── Cap_dashboard/            # conatins R file and data for running the cultural access and participation dashboard
│   └── quarto_files/             # contains qmd files
└── output/                       # contains pdf and html files 
```

## Data

There were three different surveys conducted as part of European Live Music Census 2024. One survey was
for people who attended live music event during October Friday 11th in Helsinki, Vilnius, Mannheim,
Lviv or Heidelberg. There were also two surveys done for music venues in these cities. The raw data files 
can be found in directory `input/data_raw/`. Directory `input/data_processed/` contains the processed 
data files. File `combined_census_data.sav` contains the combined audience survey data and files 
`pre_snapshot_clean.xlsx` and `post_snapshot_clean.xlsx` contains the processed data from the
two venue surveys. The file `input/other/codebook.xlsx` contains data used for mapping and harmonizing different dataset 
columns and variable values.

Another survey collected for the project is the cultural access and participation survey. The data for 
this survey can also be found in the `input/data_processed/` directory. The file is called `cultural_access_parcipation.sav`. 

All datasets included here are interim versions. The official releases, complete with standardized metadata and documentation, will be published at a later date.

## Census dashboard

The census dashboard offers visualizations of the different questions in the audience census data from all the cities.
The census dashboard can be found in the directory `code/Census_dashboard`. The directory contains the
Shiny R file, `app.R`, for the dashboard and the data needed for running the dashboard. The census
dashboard can be run with the following command: 

```
shiny::runApp(appDir = "code/Census_dashboard/")
```

The code above expects that your working directory is `openmuse/`. If it is not, you have to change 
the path in the code to the directory that has the `app.R` file in it. You will also need to install 
some R-packages to run the app. The list of packages can be found below.

## Cultural access and participation dashboard

There is also a dashboard connected to the cultural access and participation survey. It offers similar 
visualizations to the other dashboard. The code can run with the following command:

```
shiny::runApp(appDir = "code/Cap_dashoard/")
```

## Probabilistic analysis tutorials

There are also tutorials for how to conduct probabilistic analysis of ordinal survey data. These tutorials can 
be found in the following files:

- `prob-example.qmd` document goes over the basics of how to perform probabilistic analysis of ordinal survey data
- `advanced_prob_examples.qmd` document introduces some ways how the probabilistic framework can be used to fit 
more advanced models.

To run these models you need to install `brms` and `rstan`. For more information about `brms` you can 
check the packages [GitHub page](https://github.com/paul-buerkner/brms?tab=readme-ov-file). For more information 
on how to install `rstan` you can check the following [site](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

## Eurostat tutorials

There are also two tutorials on how to use `eurostat` package to retrieve music and culture related, and 
demographic data. These tutorials can be found in the following files:

- `eurostat_tutorial.qmd` document showcases how the package can be used to retrieve music and culture related data
- `demographic_data.qmd` document showcases how the package can be used to retrieve demographic data for different countries

## Data visualizations and wrangling

The quarto documents use a lot of different packages. You need to download the packages to run the code in 
the document. You can install the packages as you need them or you can run the following code to install 
all the packages used in the documents.

```
# Installing tidyverse will install all the tidyverse packages 
package_list <- c("quarto", "dplyr", "tidyverse", "haven", "labelled", "retroharmonize", "crosswalkr", 
"kableExtra", "ggrepel", "readxl", "janitor", "openxlsx", "deeplr", "brms", "tidybayes", "broom.mixed",
"eurostat")

for (p in package_list) {
  if (!p in installed.packages()) {
    install.packages(p)
  }
}

# Packages needed for running the dashboard
package_list_dash <- c("shiny", "bslib", "leaflet")

for (p in package_list_dash) {
  if (!p in installed.packages()) {
    install.packages(p)
  }
}
```


Directory `output/` contains pdf files for data visualizations and wrangling.

- File `data_wrangling.pdf` shows how the different census data files can be combined into a one file.
- File `census_analysis.pdf` shows how the different census questions can be visualized.
- File `census_analysis_tables.pdf` contains tables for census questions.
- File `venue_analysis.pdf` shows plots based on the two music venue surveys.
- File `venue_analysis_tables.pdf` contains tables for the two venue surveys.
- File `demographic_data.pdf` first shows how to use `eurostat` R-package to download demographic data
- File `city_specific_data.pdf` has tables from the audience survey's and venue surveys' questions.
The tables are for data from Helsinki, but can be changed by modifying the corresponding quarto file.
demographic data for the different census cities. After this it compares the demographic
breakdown in the census to the population. 
- File `missing.pdf` has answer percentage per question for the different cities and file `missing2.pdf` 
has the number of answers per question.
- File `prob_example.pdf` showcases how probabilistic models can be used to analyse ordinal variables.
- File `advanced_prob_examples.pdf` showcases more complex probabilistic ordinal regression models.
- File `open_question_translation.pdf` shows how the open questions can be translated using
machine translation tools. 

The quarto files used for generating these documents can be found in `code/quarto_files/` directory. 
Any of the documents can be rendered by running the following commands:

```
# Rendering data_wrangling.pdf
quarto::quarto_render(input = "code/quarto_files/data_wrangling.qmd")
# Rendering census_analysis.pdf
quarto::quarto_render(input = "code/quarto_files/census_analysis.qmd")
# Rendering census_analysis_tables.html
quarto::quarto_render(input = "code/quarto_files/census_analysis_tables.qmd")
# Rendering venue_analysis.pdf
quarto::quarto_render(input = "code/quarto_files/venue_analysis.qmd")
# Rendering venue_analysis_tables.html
quarto::quarto_render(input = "code/quarto_files/venue_analysis_tables.qmd")
# Rendering demographic_data.pdf
quarto::quarto_render(input = "code/quarto_files/demographic.qmd")
# Rendering city_specific_tables.pdf 
quarto::quarto_render(input = "code/quarto_files/city_specific_tables.qmd")

```
The `input` specifies the location of the quarto file you want to render. Running these command generate 
the corresponding pdf/html in the `output/` directory. Rendering the `data_wrangling.pdf` will also 
generate and save the `combined_census_data.sav` file to the `input/data_processed/`. For the
`city_specific_tables.qmd` file, you can change the city/cities the data is made for by changing the value of
the `city` vector at the beginning of the document.

