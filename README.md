# US Traffic Accidents Analysis

Multivariate and spatial analysis of US traffic accidents in Massachusetts, New York and New Jersey using MCA, FAMD, clustering, geostatistics and topic modelling in R.

## Overview

This project explores the factors behind traffic accidents across three US states (MA, NY, NJ) using a rich dataset of ~7,800 preprocessed and imputed records. The goal is to identify spatial, temporal and contextual patterns that influence accident severity.

## Project Structure

The analysis is divided into 6 parts:

| Part | Analysis |
|------|----------|
| I | Problem framing & dataset description |
| II | Multiple Correspondence Analysis (MCA) |
| III | FAMD, Clustering & Profiling |
| IV | Descriptive spatial statistics |
| V | Geostatistics & Point Process |
| VI | Textual Analysis (CA + LDA) |

## Dataset

The file `dataset_imputado_final.csv` contains **7,793 accident records** (preprocessed and imputed) with the following features:

- **Temporal** — date, time, time of day, daylight indicator
- **Meteorological** — temperature, humidity, pressure, wind, precipitation, visibility, weather condition
- **Geospatial** — latitude, longitude, state, city, county, street
- **Infrastructure** — amenity, crossing, junction, railway, station, traffic signal, nearby hospital, school zone, police, commercial area
- **Accident** — severity category, distance, duration

## Methods

- **MCA** — dimensionality reduction for categorical variables
- **FAMD** — mixed data (quantitative + qualitative) factorial analysis
- **Clustering** — unsupervised grouping with profiling
- **TLP / aTLP** — latent profile analysis
- **Spatial statistics** — point pattern analysis, kernel density estimation across states and cities
- **Geostatistics** — spatiotemporal accident study
- **Text mining** — Correspondence Analysis on descriptions + Latent Dirichlet Allocation (LDA) for topic modelling

## Requirements

R with the following packages:

```r
install.packages(c(
  "tm", "tidytext", "FactoMineR", "factoextra", "corrplot",
  "ggplot2", "sf", "dplyr", "maps", "leaflet", "tmap",
  "tidycensus", "tigris", "units", "SnowballC", "slam",
  "Matrix", "xlsx", "topicmodels", "RColorBrewer", "wordcloud",
  "lda", "raster", "spatstat", "geosphere", "viridis",
  "gganimate", "lubridate", "gridExtra"
))
```

## Usage

1. Clone the repository
2. Set your local path in `D4_Script_Unificat.R`:
   ```r
   path <- "your/local/path/"
   ```
3. Run the script section by section in RStudio

## Authors

Sílvia Borràs, Sam Brumwell, Mariona Casasnovas, Anna Galstyan, Martina Hernández, Núria López  
Grau en Intel·ligència Artificial — Universitat Politècnica de Catalunya (UPC)  
Course: Preprocessament i Models Avançats d'Anàlisi de Dades, Spring 2025
