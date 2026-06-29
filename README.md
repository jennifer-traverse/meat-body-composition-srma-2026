# meat-body-composition-srma-2026

**Systematic Review and Meta-Analysis: Effects of Meat Consumption on Body Size, Composition, and Skeletal Muscle Outcomes in Adults**

Peregoy JA, Fleming RA, Leidy HJ, Fleming SA | Traverse Science | 2026

This repository contains the R scripts and dataset supporting the systematic review and meta-analysis evaluating the effects of meat consumption on body size, composition, and skeletal muscle outcomes in adults.

---

## Repository Structure

```
meat-body-composition-srma-2026/
├── data/
│   ├── Meat_Extraction_Database.xlsx   # Raw extracted data from included studies
│   └── Meat_Analytic_Dataset.xlsx      # Cleaned, analysis-ready dataset
├── scripts/
│   ├── Creation_of_Meat_Analytic_Dataset.R   # Data cleaning and preparation
│   └── Analysis_Meat_SRMA.R                  # All meta-analyses, subgroup analyses,
│                                             # sensitivity analyses, and figures
├── .zenodo.json          # Zenodo metadata for DOI minting
├── .gitignore
├── LICENSE
└── README.md
```

> Note: The `output/` folder is excluded from version control. Running `Analysis_Meat_SRMA.R` regenerates all figures and tables locally under `output/results/<timestamp>/`.

---

## Requirements

- R (>= 4.2.0)
- Key packages: `meta`, `metafor`, `dplyr`, `ggplot2`, `readxl`, `openxlsx`

Install all dependencies with:

```r
install.packages(c("meta", "metafor", "dplyr", "ggplot2", "readxl", "openxlsx"))
```

---

## Reproducing the Analysis

1. Clone the repository:
   ```bash
   git clone https://github.com/jennifer-traverse/meat-body-composition-srma-2026.git
   cd meat-body-composition-srma-2026
   ```

2. Open `scripts/Creation_of_Meat_Analytic_Dataset.R` in R or RStudio and run it to prepare the analytic dataset.

3. Open `scripts/Analysis_Meat_SRMA.R` and run it to reproduce all pooled meta-analyses, subgroup analyses, sensitivity analyses, publication bias tests, and figures. Output is written to `output/results/<timestamp>/`.

---

## Citation

If you use this code or data, please cite:

> Peregoy JA, Fleming RA, Leidy HJ, Fleming SA. (2026). *meat-body-composition-srma-2026: R scripts and dataset for systematic review and meta-analysis of meat consumption and body composition outcomes in adults* (v1.0.0). Zenodo. https://doi.org/[YOUR_DOI_HERE]

A BibTeX entry:

```bibtex
@software{meat_body_composition_srma_2026,
  author    = {Peregoy, Jennifer A. and Fleming, R. A. and Leidy, H. J. and Fleming, S. A.},
  title     = {meat-body-composition-srma-2026},
  year      = {2026},
  publisher = {Zenodo},
  doi       = {[YOUR_DOI_HERE]},
  url       = {https://doi.org/[YOUR_DOI_HERE]}
}
```

> Update `[YOUR_DOI_HERE]` with the DOI assigned by Zenodo after your first release.

---

## License

This work is licensed under a [Creative Commons Attribution 4.0 International License](LICENSE) (CC BY 4.0).

You are free to share and adapt this material for any purpose, provided appropriate credit is given.

---

## Contact

For questions, open an issue or contact the corresponding author.
