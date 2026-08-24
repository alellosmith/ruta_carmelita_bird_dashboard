# Ruta Carmelita bird monitoring dashboard
#
# Replace data/species_list_by_sites_forchat.csv with an updated CSV that has
# the same column names, then restart the app. A replacement CSV can also be
# uploaded temporarily through the sidebar while the app is running.

required_packages <- c(
  "shiny", "bslib", "leaflet", "dplyr", "readr", "tidyr",
  "stringr", "htmltools"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the required packages before running the app:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

default_data_path <- file.path("data", "species_list_by_sites_forchat.csv")

required_columns <- c(
  "transect_id", "pc_id", "lat", "lon", "year", "species_code",
  "scientific_name", "common_name", "hab_type"
)

known_year_colors <- c(
  "2019" = "#E66101",
  "2019, 2024, 2025, 2026" = "#00A6D6",
  "2019, 2024, 2026" = "#F2C500",
  "2024, 2025" = "#7A4EAB",
  "2024, 2025, 2026" = "#D81B60",
  "2024, 2026" = "#0072B2"
)

# -----------------------------------------------------------------------------
# Data helpers
# -----------------------------------------------------------------------------

read_bird_data <- function(path) {
  dat <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
    na = c("", "NA", "NaN"))

  missing_cols <- setdiff(required_columns, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      "The CSV is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  dat <- dat %>%
    transmute(
      transect_id = stringr::str_squish(as.character(transect_id)),
      pc_id = stringr::str_squish(as.character(pc_id)),
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon)),
      year = suppressWarnings(as.integer(year)),
      species_code = stringr::str_squish(as.character(species_code)),
      scientific_name = stringr::str_squish(as.character(scientific_name)),
      common_name = stringr::str_squish(as.character(common_name)),
      hab_type = stringr::str_to_lower(
        stringr::str_squish(as.character(hab_type))
      )
    ) %>%
    mutate(
      hab_type = case_when(
        stringr::str_detect(hab_type, "forest") ~ "forest",
        stringr::str_detect(hab_type, "pasture|restor") ~ "pasture",
        TRUE ~ "other"
      )
    ) %>%
    filter(
      !is.na(pc_id), pc_id != "",
      !is.na(lat), !is.na(lon),
      dplyr::between(lat, -90, 90),
      dplyr::between(lon, -180, 180),
      !is.na(year),
      !is.na(species_code), species_code != "",
      !is.na(common_name), common_name != ""
    ) %>%
    distinct(pc_id, year, species_code, .keep_all = TRUE)

  if (nrow(dat) == 0) {
    stop("No valid observation rows remained after data validation.")
  }

  dat
}

build_site_table <- function(dat) {
  # Use coordinates and habitat from the most recent surveyed year. If there
  # are several coordinate records in that year, retain the first one.
  latest <- dat %>%
    distinct(pc_id, transect_id, lat, lon, year, hab_type) %>%
    arrange(pc_id, desc(year)) %>%
    group_by(pc_id) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(pc_id, transect_id, lat, lon, hab_type, coordinate_year = year)

  survey_history <- dat %>%
    distinct(pc_id, year) %>%
    arrange(pc_id, year) %>%
    group_by(pc_id) %>%
    summarise(
      years_surveyed = paste(year, collapse = ", "),
      n_years = n(),
      .groups = "drop"
    )

  richness <- dat %>%
    distinct(pc_id, species_code) %>%
    count(pc_id, name = "total_species")

  site_records <- dat %>%
    count(pc_id, name = "species_year_records")

  latest %>%
    left_join(survey_history, by = "pc_id") %>%
    left_join(richness, by = "pc_id") %>%
    left_join(site_records, by = "pc_id")
}

make_year_palette <- function(year_combos) {
  combos <- sort(unique(year_combos))
  palette <- known_year_colors[intersect(names(known_year_colors), combos)]
  missing_combos <- setdiff(combos, names(palette))

  if (length(missing_combos) > 0) {
    fallback <- grDevices::hcl.colors(length(missing_combos), "Dark 3")
    names(fallback) <- missing_combos
    palette <- c(palette, fallback)
  }

  palette[combos]
}

safe_id <- function(x) {
  stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

make_svg_marker <- function(color, habitat) {
  if (habitat == "forest") {
    shape <- sprintf(
      paste0(
        '<circle cx="16" cy="16" r="10" fill="%s" ',
        'stroke="#FFFFFF" stroke-width="3"/>',
        '<circle cx="16" cy="16" r="11.5" fill="none" ',
        'stroke="#1C1C1C" stroke-width="1"/>'
      ),
      color
    )
  } else if (habitat == "pasture") {
    shape <- sprintf(
      paste0(
        '<polygon points="16,4.5 27.5,16 16,27.5 4.5,16" ',
        'fill="%s" stroke="#FFFFFF" stroke-width="3"/>',
        '<polygon points="16,3 29,16 16,29 3,16" fill="none" ',
        'stroke="#1C1C1C" stroke-width="1"/>'
      ),
      color
    )
  } else {
    shape <- sprintf(
      paste0(
        '<rect x="6" y="6" width="20" height="20" rx="3" fill="%s" ',
        'stroke="#FFFFFF" stroke-width="3"/>',
        '<rect x="4.5" y="4.5" width="23" height="23" rx="4" ',
        'fill="none" stroke="#1C1C1C" stroke-width="1"/>'
      ),
      color
    )
  }

  svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" ',
    'viewBox="0 0 32 32">',
    '<filter id="shadow" x="-40%" y="-40%" width="180%" height="180%">',
    '<feDropShadow dx="0" dy="1" stdDeviation="1" flood-opacity="0.45"/>',
    '</filter><g filter="url(#shadow)">', shape, '</g></svg>'
  )

  icon_url <- paste0(
    "data:image/svg+xml;utf8,",
    utils::URLencode(svg, reserved = TRUE)
  )

  leaflet::makeIcon(
    iconUrl = icon_url,
    iconWidth = 32, iconHeight = 32,
    iconAnchorX = 16, iconAnchorY = 16
  )
}

make_icon_set <- function(site_dat, palette) {
  key_table <- tidyr::expand_grid(
    years_surveyed = unique(site_dat$years_surveyed),
    hab_type = unique(site_dat$hab_type)
  ) %>%
    mutate(icon_key = paste(hab_type, safe_id(years_surveyed), sep = "__"))

  icons <- lapply(seq_len(nrow(key_table)), function(i) {
    make_svg_marker(
      color = unname(palette[key_table$years_surveyed[i]]),
      habitat = key_table$hab_type[i]
    )
  })
  names(icons) <- key_table$icon_key

  do.call(leaflet::iconList, icons)
}

make_map_legend <- function(site_dat, palette) {
  combo_counts <- site_dat %>%
    count(years_surveyed, name = "n") %>%
    arrange(years_surveyed)

  color_rows <- paste0(
    '<div class="legend-row"><span class="legend-color" style="background:',
    palette[combo_counts$years_surveyed],
    '"></span><span>',
    htmltools::htmlEscape(combo_counts$years_surveyed),
    ' <span class="legend-count">(n=', combo_counts$n, ')</span></span></div>',
    collapse = ""
  )

  paste0(
    '<div class="map-legend">',
    '<div class="legend-title">Years surveyed</div>',
    color_rows,
    '<div class="legend-divider"></div>',
    '<div class="legend-title">Habitat</div>',
    '<div class="legend-row"><span class="legend-shape circle-shape"></span>',
    '<span>Mature forest</span></div>',
    '<div class="legend-row"><span class="legend-shape diamond-shape"></span>',
    '<span>Restored pasture</span></div>',
    '</div>'
  )
}

format_species_list <- function(dat, highlighted_codes = character()) {
  if (nrow(dat) == 0) {
    return(tags$div(class = "empty-note", "No species records for this year."))
  }

  items <- lapply(seq_len(nrow(dat)), function(i) {
    is_highlighted <- dat$species_code[i] %in% highlighted_codes
    item_class <- if (is_highlighted) "species-item species-highlight" else "species-item"

    tags$li(
      class = item_class,
      tags$span(class = "species-common", dat$common_name[i]),
      tags$span(class = "species-scientific",
        paste0(" — ", dat$scientific_name[i]))
    )
  })

  tags$ul(class = "species-list", items)
}

# -----------------------------------------------------------------------------
# User interface
# -----------------------------------------------------------------------------

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#176B5B",
  secondary = "#5C6F68",
  bg = "#F4F7F5",
  fg = "#1D2925"
)

ui <- bslib::page_sidebar(
  title = tags$div(
    class = "app-title-wrap",
    tags$span(class = "app-title", "Ruta Carmelita Bird Monitoring"),
    tags$span(class = "app-subtitle",
      "Interactive point-count species dashboard")
  ),
  window_title = "Ruta Carmelita Bird Monitoring Dashboard",
  theme = theme,
  fillable = TRUE,
  fillable_mobile = TRUE,
  sidebar = bslib::sidebar(
    width = 340,
    open = "desktop",
    title = "Filters and data",
    fileInput(
      "data_file",
      "Temporarily load a replacement CSV",
      accept = c(".csv", "text/csv")
    ),
    tags$p(
      class = "input-help",
      "To update the dashboard permanently, replace the CSV in the data folder and restart the app."
    ),
    tags$hr(),
    selectizeInput(
      "species_filter",
      "Filter sites by species",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Type a common or scientific name...",
        plugins = list("remove_button")
      )
    ),
    radioButtons(
      "species_match",
      "When several species are selected",
      choices = c("Any selected species" = "any", "All selected species" = "all"),
      selected = "any"
    ),
    checkboxGroupInput(
      "year_filter",
      "Survey years",
      choices = NULL
    ),
    checkboxGroupInput(
      "habitat_filter",
      "Habitat",
      choices = c("Mature forest" = "forest", "Restored pasture" = "pasture"),
      selected = c("forest", "pasture")
    ),
    selectInput(
      "transect_filter",
      "Transect",
      choices = "All transects"
    ),
    selectizeInput(
      "site_search",
      "Jump to a point-count site",
      choices = NULL,
      options = list(placeholder = "Search by site ID...")
    ),
    bslib::layout_columns(
      actionButton("reset_filters", "Reset filters", class = "btn-outline-secondary"),
      actionButton("zoom_results", "Zoom to results", class = "btn-outline-primary"),
      col_widths = c(6, 6)
    ),
    downloadButton("download_filtered", "Download filtered records",
      class = "btn-primary w-100"),
    tags$div(class = "sidebar-note",
      "Map colors show the combination of years surveyed. Circles indicate mature forest; diamonds indicate restored pasture."
    )
  ),
  bslib::layout_columns(
    bslib::card(
      full_screen = TRUE,
      class = "map-card",
      bslib::card_header(
        tags$div(
          class = "card-header-flex",
          tags$span("Point-count sites"),
          uiOutput("summary_strip")
        )
      ),
      bslib::card_body(
        class = "p-0",
        min_height = "660px",
        leafletOutput("site_map", height = "100%")
      ),
      bslib::card_footer(uiOutput("map_footer"))
    ),
    bslib::card(
      class = "details-card",
      bslib::card_header(uiOutput("detail_title")),
      bslib::card_body(
        class = "site-details-body",
        min_height = "660px",
        uiOutput("site_details")
      )
    ),
    col_widths = c(8, 4),
    row_heights = "1fr"
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  initial_data <- tryCatch(
    read_bird_data(default_data_path),
    error = function(e) stop(
      "Could not read the bundled data file at ", default_data_path,
      ": ", conditionMessage(e)
    )
  )

  observations <- reactiveVal(initial_data)
  selected_site <- reactiveVal(NULL)

  site_table <- reactive({
    build_site_table(observations())
  })

  species_table <- reactive({
    observations() %>%
      distinct(species_code, common_name, scientific_name) %>%
      arrange(common_name)
  })

  observeEvent(observations(), {
    dat <- observations()
    species <- species_table()
    sites <- site_table()

    species_choices <- stats::setNames(
      species$species_code,
      paste0(species$common_name, " — ", species$scientific_name)
    )

    updateSelectizeInput(session, "species_filter",
      choices = species_choices, selected = character(), server = TRUE)
    updateCheckboxGroupInput(session, "year_filter",
      choices = sort(unique(dat$year)), selected = sort(unique(dat$year)))
    updateSelectInput(session, "transect_filter",
      choices = c("All transects", sort(unique(dat$transect_id))),
      selected = "All transects")
    updateSelectizeInput(session, "site_search",
      choices = c("Select a site..." = "", stats::setNames(sites$pc_id, sites$pc_id)),
      selected = "", server = TRUE)

    selected_site(NULL)
  }, ignoreInit = FALSE)

  observeEvent(input$data_file, {
    req(input$data_file$datapath)

    tryCatch({
      replacement <- read_bird_data(input$data_file$datapath)
      observations(replacement)
      showNotification(
        paste0(
          "Loaded ", format(nrow(replacement), big.mark = ","),
          " validated observation records."
        ),
        type = "message", duration = 5
      )
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })

  year_context_data <- reactive({
    dat <- observations()
    selected_years <- suppressWarnings(as.integer(input$year_filter))

    if (length(selected_years) > 0) {
      dat <- dat %>% filter(year %in% selected_years)
    }

    dat
  })

  filtered_sites <- reactive({
    dat <- year_context_data()
    sites <- site_table()

    # Keep sites surveyed in the selected year context.
    sites <- sites %>% semi_join(dat %>% distinct(pc_id), by = "pc_id")

    if (length(input$habitat_filter) > 0) {
      sites <- sites %>% filter(hab_type %in% input$habitat_filter)
    }

    if (!is.null(input$transect_filter) && input$transect_filter != "All transects") {
      sites <- sites %>% filter(transect_id == input$transect_filter)
    }

    if (length(input$species_filter) > 0) {
      matched <- dat %>%
        filter(species_code %in% input$species_filter) %>%
        distinct(pc_id, species_code) %>%
        count(pc_id, name = "n_selected_species")

      if (identical(input$species_match, "all")) {
        matched <- matched %>%
          filter(n_selected_species == length(input$species_filter))
      }

      sites <- sites %>% semi_join(matched, by = "pc_id")
    }

    sites %>% arrange(transect_id, pc_id)
  })

  filtered_records <- reactive({
    dat <- year_context_data() %>%
      semi_join(filtered_sites() %>% select(pc_id), by = "pc_id")

    if (length(input$species_filter) > 0) {
      dat <- dat %>% filter(species_code %in% input$species_filter)
    }

    dat %>% arrange(pc_id, year, common_name)
  })

  output$site_map <- renderLeaflet({
    sites <- site_table()
    palette <- make_year_palette(sites$years_surveyed)

    map <- leaflet::leaflet(
      options = leaflet::leafletOptions(preferCanvas = TRUE, zoomControl = TRUE)
    ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = "Satellite imagery"
      ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        group = "Light map"
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c("Satellite imagery", "Light map"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      leaflet::addScaleBar(
        position = "bottomleft",
        options = leaflet::scaleBarOptions(metric = TRUE, imperial = FALSE)
      ) %>%
      leaflet::addControl(
        html = htmltools::HTML(make_map_legend(sites, palette)),
        position = "bottomright",
        className = "dashboard-map-legend"
      )

    if (nrow(sites) > 0) {
      map <- map %>%
        leaflet::fitBounds(
          lng1 = min(sites$lon), lat1 = min(sites$lat),
          lng2 = max(sites$lon), lat2 = max(sites$lat)
        )
    }

    map
  })

  observe({
    sites <- filtered_sites()
    all_sites <- site_table()
    palette <- make_year_palette(all_sites$years_surveyed)
    icon_set <- make_icon_set(all_sites, palette)

    proxy <- leaflet::leafletProxy("site_map") %>%
      leaflet::clearGroup("sites") %>%
      leaflet::clearGroup("selected")

    if (nrow(sites) == 0) {
      return()
    }

    sites <- sites %>%
      mutate(
        icon_key = paste(hab_type, safe_id(years_surveyed), sep = "__"),
        marker_label = paste0(
          "<strong>", htmltools::htmlEscape(pc_id), "</strong><br>",
          htmltools::htmlEscape(transect_id), " · ",
          if_else(hab_type == "forest", "Mature forest", "Restored pasture"),
          "<br>Surveyed: ", htmltools::htmlEscape(years_surveyed),
          "<br>", total_species, " species across all years"
        )
      )

    proxy %>%
      leaflet::addMarkers(
        data = sites,
        lng = ~lon, lat = ~lat,
        layerId = ~pc_id,
        group = "sites",
        icon = ~icon_set[icon_key],
        label = lapply(sites$marker_label, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          direction = "auto", opacity = 0.95,
          style = list("font-size" = "13px")
        ),
        options = leaflet::markerOptions(riseOnHover = TRUE)
      )

    current <- selected_site()
    if (!is.null(current) && current %in% all_sites$pc_id) {
      selected_row <- all_sites %>% filter(pc_id == current)
      leaflet::leafletProxy("site_map") %>%
        leaflet::clearGroup("selected") %>%
        leaflet::addCircleMarkers(
          data = selected_row,
          lng = ~lon, lat = ~lat,
          group = "selected",
          radius = 14,
          color = "#FFFFFF", weight = 4,
          fill = FALSE, opacity = 1,
          options = leaflet::pathOptions(interactive = FALSE)
        )
    }
  })

  observeEvent(input$site_map_marker_click, {
    click <- input$site_map_marker_click
    req(click$id)
    selected_site(click$id)
    updateSelectizeInput(session, "site_search", selected = click$id)
  })

  observeEvent(input$site_search, {
    req(input$site_search, input$site_search != "")
    selected_site(input$site_search)

    site <- site_table() %>% filter(pc_id == input$site_search)
    if (nrow(site) == 1) {
      leaflet::leafletProxy("site_map") %>%
        leaflet::setView(lng = site$lon, lat = site$lat, zoom = 15)
    }
  })

  observeEvent(selected_site(), {
    site_id <- selected_site()
    if (is.null(site_id)) {
      return()
    }

    site <- site_table() %>% filter(pc_id == site_id)
    if (nrow(site) == 1) {
      leaflet::leafletProxy("site_map") %>%
        leaflet::clearGroup("selected") %>%
        leaflet::addCircleMarkers(
          data = site,
          lng = ~lon, lat = ~lat,
          group = "selected",
          radius = 14,
          color = "#FFFFFF", weight = 4,
          fill = FALSE, opacity = 1,
          options = leaflet::pathOptions(interactive = FALSE)
        )
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$zoom_results, {
    sites <- filtered_sites()

    if (nrow(sites) == 0) {
      showNotification("No sites match the current filters.", type = "warning")
    } else if (nrow(sites) == 1) {
      leaflet::leafletProxy("site_map") %>%
        leaflet::setView(lng = sites$lon, lat = sites$lat, zoom = 15)
    } else {
      leaflet::leafletProxy("site_map") %>%
        leaflet::fitBounds(
          lng1 = min(sites$lon), lat1 = min(sites$lat),
          lng2 = max(sites$lon), lat2 = max(sites$lat)
        )
    }
  })

  observeEvent(input$reset_filters, {
    dat <- observations()
    updateSelectizeInput(session, "species_filter", selected = character())
    updateRadioButtons(session, "species_match", selected = "any")
    updateCheckboxGroupInput(session, "year_filter",
      selected = sort(unique(dat$year)))
    updateCheckboxGroupInput(session, "habitat_filter",
      selected = c("forest", "pasture"))
    updateSelectInput(session, "transect_filter", selected = "All transects")
    updateSelectizeInput(session, "site_search", selected = "")
    selected_site(NULL)

    sites <- site_table()
    leaflet::leafletProxy("site_map") %>%
      leaflet::fitBounds(
        lng1 = min(sites$lon), lat1 = min(sites$lat),
        lng2 = max(sites$lon), lat2 = max(sites$lat)
      )
  })

  output$summary_strip <- renderUI({
    sites <- filtered_sites()
    dat <- filtered_records()

    tags$div(
      class = "summary-strip",
      tags$span(class = "summary-pill",
        tags$strong(format(nrow(sites), big.mark = ",")), " sites"),
      tags$span(class = "summary-pill",
        tags$strong(format(n_distinct(dat$species_code), big.mark = ",")),
        " species"),
      tags$span(class = "summary-pill",
        tags$strong(format(n_distinct(sites$transect_id), big.mark = ",")),
        " transects")
    )
  })

  output$map_footer <- renderUI({
    n_sites <- nrow(filtered_sites())
    if (n_sites == 0) {
      tags$span(class = "text-danger",
        "No sites match the current filters. Adjust the filters or reset them.")
    } else {
      tags$span(
        "Click a marker to view all species recorded at that site, grouped by year."
      )
    }
  })

  output$detail_title <- renderUI({
    site_id <- selected_site()
    if (is.null(site_id)) {
      return(tags$span("Site details"))
    }

    tags$div(
      class = "detail-heading",
      tags$span(site_id),
      actionButton("clear_site", "Clear", class = "btn-sm btn-outline-secondary")
    )
  })

  observeEvent(input$clear_site, {
    selected_site(NULL)
    updateSelectizeInput(session, "site_search", selected = "")
    leaflet::leafletProxy("site_map") %>% leaflet::clearGroup("selected")
  })

  output$site_details <- renderUI({
    site_id <- selected_site()

    if (is.null(site_id)) {
      return(tags$div(
        class = "site-placeholder",
        tags$div(class = "placeholder-symbol", "●"),
        tags$h4("Select a point-count site"),
        tags$p(
          "Click a marker on the map or search for a site in the sidebar. " ,
          "The species list will appear here, grouped by survey year."
        )
      ))
    }

    site <- site_table() %>% filter(pc_id == site_id)
    records <- observations() %>%
      filter(pc_id == site_id) %>%
      distinct(year, species_code, common_name, scientific_name) %>%
      arrange(year, common_name)

    if (nrow(site) == 0) {
      return(tags$div(class = "empty-note", "Site not found in the current data."))
    }

    highlighted_codes <- if (is.null(input$species_filter)) character() else input$species_filter

    year_sections <- lapply(sort(unique(records$year)), function(this_year) {
      year_species <- records %>%
        filter(year == this_year) %>%
        arrange(common_name)

      tags$details(
        class = "year-section",
        open = TRUE,
        tags$summary(
          tags$span(class = "year-label", this_year),
          tags$span(class = "year-richness",
            paste0(nrow(year_species), " species"))
        ),
        format_species_list(year_species, highlighted_codes)
      )
    })

    habitat_label <- case_when(
      site$hab_type == "forest" ~ "Mature forest",
      site$hab_type == "pasture" ~ "Restored pasture",
      TRUE ~ "Other habitat"
    )

    tags$div(
      tags$div(
        class = "site-meta-grid",
        tags$div(class = "site-meta-item",
          tags$span(class = "meta-label", "Transect"),
          tags$span(class = "meta-value", site$transect_id)),
        tags$div(class = "site-meta-item",
          tags$span(class = "meta-label", "Habitat"),
          tags$span(class = "meta-value", habitat_label)),
        tags$div(class = "site-meta-item",
          tags$span(class = "meta-label", "Years surveyed"),
          tags$span(class = "meta-value", site$years_surveyed)),
        tags$div(class = "site-meta-item",
          tags$span(class = "meta-label", "Total species"),
          tags$span(class = "meta-value", site$total_species))
      ),
      tags$div(
        class = "coordinate-note",
        sprintf(
          "Coordinates: %.6f, %.6f (from the most recent surveyed year: %s)",
          site$lat, site$lon, site$coordinate_year
        )
      ),
      tags$hr(),
      year_sections
    )
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("ruta_carmelita_filtered_records_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_records(), file)
    }
  )
}

shinyApp(ui, server)
