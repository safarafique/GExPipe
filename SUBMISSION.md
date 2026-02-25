# Bioconductor submission: what to submit and what’s missing

## What is “ready to submit”

You do **not** submit a `.tar.gz` file. Bioconductor expects a **GitHub repository**.

- **Ready:** Your **package source** (this directory) is in good shape:
  - `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `vignettes/`, `inst/`, `LICENSE`, `NEWS.md` are present and used for local checks.
  - Local checks: build with `R CMD build . --no-build-vignettes`, then run `R CMD check` and `BiocCheck` on the resulting tarball (e.g. `OmniVerse_0.1.0.tar.gz`).

So the thing that must be “ready” is: **a GitHub repo whose default branch contains exactly this package source** (no tarball upload).

---

## What you need to do before opening the submission issue

### 1. Put the package on GitHub (required)

- Create a GitHub repository (e.g. `https://github.com/yourusername/OmniVerse`).
- Push this package **source** (the contents of this folder, excluding `OmniVerse.Rcheck`, `OmniVerse.BiocCheck`, `*.tar.gz`) to the **default branch** (e.g. `main` or `master`).
- The default branch must contain **only package code** (no GitHub Actions / devtools config in that branch if they would confuse the build; or keep them in a separate branch).
- Add your **SSH public key(s)** to your GitHub account (needed later for Bioconductor git).

### 2. Register on the Bioconductor support site (required for BiocCheck)

- Register at **https://support.bioconductor.org** using the **same email** as in `DESCRIPTION`: `safa.res.sbb@pu.edu.pk`.
- Until this is done, BiocCheck will report an ERROR about your email not being found on the support site.

### 3. Optional but recommended

- Subscribe to the **bioc-devel** mailing list: https://stat.ethz.ch/mailman/listinfo/bioc-devel.
- Add **OmniVerse** to your “Watched tags” on the support site after registration.

---

## How to submit (after 1 and 2 are done)

1. Open a **new issue** here:  
   **https://github.com/Bioconductor/Contributions/issues/new**
2. **Title:** use the package name, e.g. `OmniVerse`.
3. **Body:** add the **link to your GitHub repository** (e.g. `https://github.com/yourusername/OmniVerse`) and confirm that you have read the review process, package guidelines, and maintainer responsibilities (as in the issue template).
4. The submitter should be the **maintainer** listed in `DESCRIPTION` so BiocCredentials can be verified.

After submission, Bioconductor will clone your repo, build and check the package, and run BiocCheck. All further changes are made by pushing to the **git.bioconductor.org** repository they create for you (you will get instructions).

---

## Summary: which “file” is ready, and what’s missing

| Item | Status |
|------|--------|
| Package source (DESCRIPTION, R/, man/, vignettes/, inst/, LICENSE, NEWS.md) | Ready |
| Local build/check/BiocCheck | Ready (run locally; tarball e.g. `OmniVerse_0.1.0.tar.gz` is for this only) |
| GitHub repository (default branch = this package) | **You need to create and push** |
| Support site registration (same email as DESCRIPTION) | **You need to register** |
| New issue at Bioconductor/Contributions with repo link | **You do this after the two steps above** |

Nothing is “missing” from the R package itself for submission; the missing pieces are the GitHub repo and support site registration, then opening the issue with your repo link.
