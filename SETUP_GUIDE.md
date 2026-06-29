# GitHub + Zenodo Setup Guide
### meat-body-composition-srma-2026

This guide walks you through creating the GitHub repository, pushing your files, linking Zenodo, and minting a persistent DOI.

---

## Part 1: Create the GitHub Repository

1. Go to [https://github.com/new](https://github.com/new)

2. Fill in:
   - **Repository name:** `meat-body-composition-srma-2026`
   - **Description:** R scripts and dataset for systematic review and meta-analysis evaluating the effects of meat consumption on body size, composition, and skeletal muscle outcomes in adults
   - **Visibility:** Public (required for Zenodo to index it)
   - **Do NOT** initialize with a README — you already have one

3. Click **Create repository**

---

## Part 2: Push Your Files to GitHub

Open a terminal and run the following commands from your project folder:

```bash
cd /path/to/your/project

# Initialize git (skip if already done)
git init

# Add all files
git add .

# First commit
git commit -m "Initial commit: R scripts, data, and metadata"

# Link to your GitHub repo (replace YOUR_USERNAME)
git remote add origin https://github.com/YOUR_USERNAME/meat-body-composition-srma-2026.git

# Push
git branch -M main
git push -u origin main
```

> **Files to include before pushing:**
> - `README.md`
> - `LICENSE`
> - `.zenodo.json`
> - `.gitignore`
> - Your `data/`, `scripts/`, and `output/` folders

---

## Part 3: Link Zenodo to GitHub

1. Go to [https://zenodo.org](https://zenodo.org) and log in

2. Click your name (top right) → **GitHub**

3. You'll see a list of your GitHub repositories. Find `meat-body-composition-srma-2026` and **toggle it ON**

   > If the repo doesn't appear, click **Sync** at the top to refresh the list.

4. Zenodo will now watch this repository for new releases.

---

## Part 4: Edit Zenodo Metadata (before your first release)

Before creating a release, update `.zenodo.json` in your repository with your real details:

- `"name"`: change `"Last, First"` to your actual name (format: `"Surname, Given Name"`)
- `"affiliation"`: your institution
- `"orcid"`: your ORCID iD (get one free at [orcid.org](https://orcid.org) if you don't have one)
- Remove or update the `"related_identifiers"` block if there's no linked paper yet

Commit and push the updated `.zenodo.json` before proceeding.

---

## Part 5: Create a GitHub Release (this mints the DOI)

1. On your GitHub repo page, click **Releases** (right sidebar) → **Create a new release**

2. Fill in:
   - **Tag:** `v1.0.0`
   - **Release title:** `v1.0.0 - Initial release`
   - **Description:** Brief summary of what's included

3. Click **Publish release**

4. Zenodo detects the release automatically (within a minute or two) and mints a DOI.

---

## Part 6: Get Your DOI from Zenodo

1. Go back to [https://zenodo.org/account/settings/github/](https://zenodo.org/account/settings/github/)

2. Find `meat-body-composition-srma-2026` — you'll see a DOI badge next to it

3. Click the DOI to open the Zenodo record

4. Copy the DOI (format: `10.5281/zenodo.XXXXXXX`)

---

## Part 7: Update README with the Real DOI

Replace the placeholder `[YOUR_DOI_HERE]` in `README.md` with the actual DOI from Zenodo, then commit:

```bash
git add README.md
git commit -m "Add Zenodo DOI to README"
git push
```

---

## Part 8: Add a DOI Badge to Your README (optional but recommended)

Add this line near the top of your `README.md` (replace with your real DOI):

```markdown
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)
```

This renders as a clickable badge on GitHub.

---

## Summary Checklist

- [ ] GitHub repo created (public)
- [ ] Files pushed (including `.zenodo.json`, `LICENSE`, `README.md`)
- [ ] `.zenodo.json` updated with real author name, affiliation, ORCID
- [ ] Zenodo linked to GitHub repo (toggle ON)
- [ ] GitHub release created (`v1.0.0`)
- [ ] DOI minted on Zenodo
- [ ] README updated with real DOI
- [ ] DOI badge added to README

---

## Tips

- Every new GitHub release you tag will generate a **new version DOI** on Zenodo, while the **concept DOI** (the top-level one) always resolves to the latest version. Use the concept DOI in citations.
- You can edit the Zenodo record manually after minting if you need to add co-authors or fix metadata — go to your Zenodo dashboard, find the record, and click **Edit**.
- If your dataset is large (>50 MB), consider uploading data files directly to Zenodo rather than GitHub. Link the Zenodo record to the GitHub repo using `related_identifiers` in `.zenodo.json`.
