# Ruta Carmelita Bird Monitoring Dashboard

An interactive **R Shiny + Leaflet** dashboard for exploring point-count sites and the bird species observed at each site through time.

## What the app does

- Displays one clickable marker per point-count site on a satellite basemap.
- Colors markers by the combination of survey years represented at each site.
- Uses marker shape to distinguish habitat:
  - circles = mature forest
  - diamonds = restored pasture
- Shows all species recorded at a selected site, grouped by survey year.
- Filters mapped sites by:
  - one or more species
  - survey year
  - habitat
  - transect
- Supports either **any** or **all** selected species when filtering.
- Lets you search for a site by `pc_id`.
- Exports the currently filtered observation records as a CSV.
- Uses the coordinates from the most recent surveyed year for each site.

## Files

```text
ruta_carmelita_bird_dashboard/
├── app.R
├── README.md
├── data/
│   └── species_list_by_sites_forchat.csv
└── www/
    └── styles.css
```

## Required CSV columns

The app expects a long-format CSV with these exact columns:

```text
transect_id
pc_id
lat
lon
year
species_code
scientific_name
common_name
hab_type
```

Each row should represent one species observed at one point-count site in one survey year. Duplicate `pc_id`–`year`–`species_code` rows are automatically collapsed.

`hab_type` should contain values recognizable as either `forest` or `pasture`. Values containing `restor` are also treated as pasture.

## Install packages

Run this once in R:

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "dplyr", "readr", "tidyr",
  "stringr", "htmltools"
))
```

## Run the app

Open the project folder in RStudio, then run:

```r
shiny::runApp()
```

Or click **Run App** while `app.R` is open.

## Update the data

### Permanent update

Replace:

```text
data/species_list_by_sites_forchat.csv
```

with a newer CSV that uses the same required column names, then restart the app.

### Temporary update during a session

Use the file-upload control in the dashboard sidebar. This changes the data only for the current app session; it does not overwrite the bundled CSV.

## Basemap

The default background is Esri World Imagery, with a light Carto basemap available from the layer control. Internet access is required for basemap tiles.

## Notes on filtering

- The year filter determines the years considered when testing whether a site contains the selected species.
- The site-detail panel always shows the complete species history for the selected site, across every year in the loaded CSV.
- Selected species are highlighted in the site-detail list.
- When several species are selected:
  - **Any selected species** retains a site if at least one was recorded there.
  - **All selected species** retains a site only if every selected species was recorded there within the selected year context.

## Optional deployment

The app can be deployed to shinyapps.io, Posit Connect, or a Shiny Server. Before deployment, confirm that your organization is comfortable hosting the underlying monitoring data and exact site coordinates.
