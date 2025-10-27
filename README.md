# Cambodia Food Customs — Analysis & Models

Quantitative analysis of fish consumption customs in Cambodia (2019–2022), focusing on:
- **Participation** (eat vs not eat),
- **Frequency** (how often people eat), and
- **Willingness to Pay (WTP)** for three dish types (*chopped fish — Henicorhynchus siamensis*, *sliced fish — Labeo sp.*, and *pufferfish*).

The workflow is implemented in R, orchestrated via a Quarto driver, with Bayesian models fit using **brms** + **cmdstanr**.

> **Note on PCA signs:** Principal component scores are unique up to a sign flip; interpret via loadings, not absolute signs.

---

## Repository structure

```
analysis/
├─ 01_driver_analysis.qmd        # Reproducible driver (renders the full pipeline)
├─ R/
│  ├─ 10_load_clean.R            # Load/clean survey data
│  ├─ 20_pca_scores.R            # Neophobia PCA + long-format build
│  ├─ 30_model_participation.R   # Participation (main effects)
│  ├─ 31_model_participation.R   # Participation (interactions)
│  ├─ 40_model_frequency.R       # Frequency models
│  ├─ 41_model_frequency.R       # Frequency w/ season & occasion, interactions
│  ├─ 50_model_wtp.R             # WTP (main effects)
│  ├─ 51_model_wtp.R             # WTP (interaction variants)
│  └─ utils_*.R                  # Small helpers (if any)
├─ data/
│  └─ raw/                       # (not tracked) raw Excel & shapefiles
└─ outputs/                      # Model fits, tables, diagnostics (created at run time)
```

- **Raw data**: `analysis/data/raw/rawfish_eat_Cambodia_2020_2022.xlsx` (not included).
- **Optional** shapefile: `analysis/data/raw/KHM_adm/KHM_adm4.shp`.

---

## Quick start

### 1) System requirements
- R ≥ 4.3
- CmdStan via **cmdstanr** (CmdStan ≥ 2.34 recommended)
- C++ toolchain (Rtools on Windows, Xcode CLI on macOS, build-essentials on Linux)

### 2) Install R packages
```r
install.packages(c(
  "tidyverse","readxl","janitor","checkmate","forcats",
  "brms","cmdstanr","broom.mixed","GGally","sf","here","gt","webshot2"
))
library(cmdstanr)
# one-time install:
install_cmdstan()
```

### 3) Place data
```
analysis/data/raw/rawfish_eat_Cambodia_2020_2022.xlsx
# (optional) shapefile at analysis/data/raw/KHM_adm/KHM_adm4.shp
```

### 4) Run end-to-end
```r
here::i_am("analysis/01_driver_analysis.qmd")
quarto::render("analysis/01_driver_analysis.qmd")
```
Outputs (CSV/RDS/tables) will be written to `analysis/outputs/`.

---

## What the pipeline does

1. **Load & clean**  
   Drops incomplete cases for required demographics + neophobia block.  
   Saves `clean_dat.rds/csv`.

2. **PCA on neophobia block**  
   Computes 3 PCs; binds to analysis table and constructs long format.  
   Saves `pca_rotation.csv`, `pca_importance.csv`, and `analysis_long.rds/csv`.

3. **Participation (Bernoulli–logit)**  
   - Main effects: `gender + age_z + novelty_z + experience_z + tradition_z + mo(q022_ord) + dish_type + (1|district)`.  
   - Interaction model (optional): adds gender×(novelty,tradition) and dish×(novelty,experience,tradition), with random slopes `(1 + dish_type | district)`.  
   - Saves `fit_participation*.rds`, `estimates_participation_OR.csv`.

4. **Frequency (NegBin, log link)**  
   - Main effects: demographics + PCs + dish + `season` + `occasion` + `(1|district)`.  
   - Interaction variants available in `41_model_frequency.R`.  
   - Saves `fit_frequency*.rds`, `estimates_frequency_RR.csv`.

5. **WTP (Lognormal on price per 100 g)**  
   - Main effects: `gender + dish_type + season + youandme + (1|district)`.  
   - Interaction variant: `dish_type × season`.  
   - Saves `fit_wtp*.rds`, `estimates_wtp_lognormal.csv`, `predictions_wtp_lognormal.csv`.

---

## Key results (high level)

- **Participation:** higher for males; lower with higher neophobia; strong dish effects (*pufferfish* much less likely). Economic change (`q022`) shows a weak, monotonic trend.  
- **Frequency:** higher in **rainy** season and at **parties**; decreases with higher **tradition**; strong dish effects.  
- **WTP:** higher in **dry** season and for **sliced (large) fish**; lower for males; little difference for pufferfish vs baseline.

(See `analysis/outputs/` for complete tables and intervals.)

---

## Reproducibility notes

- `set.seed(2025)` is used in the driver.  
- Models are fit via **brms** with backend **cmdstanr**; iterations and `adapt_delta` are specified in scripts.  
- Some PPC plots are disabled to avoid environment-specific plotting errors.

---

## Troubleshooting

- **cmdstanr / compiler errors**: run `cmdstanr::check_cmdstan_toolchain()` and (re)install with `install_cmdstan()`.  
- **Quarto cannot find project root**: ensure `here::i_am("analysis/01_driver_analysis.qmd")` or start the R session at repo root.  
- **PCA sign confusion**: component signs are arbitrary; interpret via loadings (see `pca_rotation.csv`).  
- **Missing `season` / `occasion`**: ensure they are created in `20_pca_scores.R` or remove those terms from models.

---

## How to cite

<!---
If you use this code or derivative outputs:

> **Utsunomiya Y., et al.** (2025). *Cambodia Food Customs: Bayesian analysis of participation, frequency, and WTP for fish dishes*. GitHub: yuzuruu/Cambodia_food_custom.

(Replace with the final article citation once published.)
--->
---

## License

Code is released under the **MIT License** (see `LICENSE`).  
Data may be restricted; contact the authors for access and terms.

---

## Acknowledgements

We thank all survey participants and field staff. Modeling uses **brms**, **cmdstanr**, and the **tidyverse** ecosystem.
