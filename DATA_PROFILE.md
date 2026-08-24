# Bundled data profile

The supplied CSV was inspected before building the dashboard.

- Raw rows: **47,835**
- Unique point-count sites: **411**
- Unique transects: **62**
- Unique species: **304**
- Survey years: **2019, 2024, 2025, 2026**
- Habitat classes: **159 forest sites; 252 pasture sites**

## Survey-year combinations represented

| Years surveyed | Sites |
|---|---:|
| 2019 | 124 |
| 2019, 2024, 2025, 2026 | 191 |
| 2019, 2024, 2026 | 2 |
| 2024, 2025 | 1 |
| 2024, 2025, 2026 | 64 |
| 2024, 2026 | 29 |

## Cleaning decisions built into the app

- The app removes exact duplicate `pc_id`–`year`–`species_code` records before display. The supplied file contains **597** such duplicates.
- Coordinates vary slightly among years for **245 sites**. The map uses the coordinate record from the most recent surveyed year for each `pc_id`, matching the project’s stated preference.
- Site-detail lists retain one occurrence per species per site-year and sort species alphabetically by common name.
