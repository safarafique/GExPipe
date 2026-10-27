# How to release a new GExPipe version

Step-by-step guide for publishing a new version of GExPipe to **GitHub**, **Bioconductor** and **Zenodo**.
Anyone who maintains the package can follow it. Commands are for the Windows Command Prompt (`cmd`).

**Order matters:** GitHub first → Bioconductor → Zenodo (Zenodo is updated automatically by the GitHub release).

| Where | What it is | How it is updated |
|---|---|---|
| GitHub `safarafique/GExPipe` (branch `main`) | Main source code | `git push origin main` |
| Bioconductor `git.bioconductor.org/packages/GExPipe` (branch `devel`) | Package users install with `BiocManager` | Push from the `bioc-sync` branch |
| Zenodo (all-versions DOI `10.5281/zenodo.23401720`) | Permanent archive with a DOI for citation | Automatic, from a GitHub **release** |

---

## 0. One-time setup (new computer or new maintainer)

1. Clone the repository and add the Bioconductor remote:

   ```bash
   git clone https://github.com/safarafique/GExPipe.git D:\GExPipe
   cd /d D:\GExPipe
   git remote add upstream git@git.bioconductor.org:packages/GExPipe.git
   git fetch upstream
   ```

2. Create the Bioconductor sync folder (a second working folder for the `bioc-sync` branch):

   ```bash
   git worktree add -b bioc-sync D:\GExPipe_bioc_sync upstream/devel
   ```

   If the branch `bioc-sync` already exists, use `git worktree add D:\GExPipe_bioc_sync bioc-sync` instead.

3. **Bioconductor SSH key:** pushing to Bioconductor needs an SSH key registered at
   https://git.bioconductor.org/BiocCredentials/ . Only the maintainer (or someone given access by Bioconductor) can push.

4. **Zenodo ↔ GitHub link:** log in to Zenodo → https://zenodo.org/account/settings/github/ →
   **Sync now** → make sure **safarafique/GExPipe** (not `GExPipe-`) is switched **ON**.

5. *(Optional)* GitHub CLI login, only needed for creating releases from the command line:

   ```bash
   gh auth login
   ```

   Answer: GitHub.com → HTTPS → Yes → Login with a web browser.

---

## 1. Update the version number

Bioconductor only builds a push whose version is **higher** than the last one.
Increase the last number by 1 (e.g. `0.99.117` → `0.99.118`). Change it in **all** of these files:

| File | Line to change |
|---|---|
| `DESCRIPTION` | `Version: 0.99.118` |
| `NEWS.md` | Add a new section at the top: `# GExPipe 0.99.118` with a list of changes |
| `.zenodo.json` | `"version": "0.99.118"` (this is the version Zenodo shows) |
| `CITATION.cff` | `version: 0.99.118` and `date-released: "YYYY-MM-DD"` (today) |
| `README.md` | The example file names `GExPipe_0.99.118.tar.gz` (3 places) |

`inst/CITATION` reads the version from `DESCRIPTION` automatically; no change needed.

Check that nothing still has the old number:

```bash
git grep -n "0.99.117"
```

(Only `NEWS.md` should still mention the old version.)

---

## 2. Check the package (recommended before every release)

In R:

```r
devtools::document()
devtools::check()
BiocCheck::BiocCheck(".")
```

Fix every **ERROR** and **WARNING** before going on.

---

## 3. GitHub: commit and push

```bash
cd /d D:\GExPipe
git status
git add -A
git commit -m "GExPipe 0.99.118: short summary of the changes"
git push origin main
```

Before `git add -A`, look at `git status` and make sure no private files are included
(unpublished results, manuscript drafts, patient data, large data files).

---

## 4. Bioconductor: sync and push

The `bioc-sync` branch receives a copy of `main` as one commit per version.

```bash
cd /d D:\GExPipe_bioc_sync
git fetch upstream
git status
```

`git status` must say `Your branch is up to date with 'upstream/devel'`. Then:

```bash
git restore --source=main --staged --worktree -- .
git commit -m "Sync with GitHub main: GExPipe 0.99.118"
git push upstream bioc-sync:devel
```

Notes:
- The warning `connection is not using a post-quantum key exchange algorithm` is harmless.
- If the commit says `nothing to commit`, step 3 was not done (or not committed) — go back.
- After 1–2 days check the build report: https://bioconductor.org/checkResults/devel/bioc-LATEST/GExPipe/
  and the package page: https://bioconductor.org/packages/devel/bioc/html/GExPipe.html

---

## 5. Zenodo: create a GitHub release

Zenodo copies every published GitHub release automatically and adds it as a new version of the same record.

**On the website:**

1. Open https://github.com/safarafique/GExPipe/releases/new
2. Click **Tag: Select tag** → type `v0.99.118` → click **+ Create new tag: v0.99.118 on publish**
   (without this you get the error "tag name can't be blank").
3. **Target:** `main`
4. **Release title:** `GExPipe v0.99.118`
5. **Description:** paste the new section from `NEWS.md`.
6. Leave **Set as a pre-release** unticked (Zenodo ignores pre-releases).
7. Click **Publish release**.

**Or from the command line** (needs `gh auth login` once):

```bash
gh release create v0.99.118 --repo safarafique/GExPipe --target main --title "GExPipe v0.99.118" --notes "See NEWS.md for changes in 0.99.118"
```

After 5–10 minutes, open the Zenodo record: **Versions** should list `0.99.118` with a new DOI and the file
`safarafique/GExPipe-v0.99.118.zip`.

**Do not** also click **New version** on Zenodo for the same release — that makes two copies.
If there is an unfinished **New version draft** on Zenodo, click **Discard version** first, otherwise the
automatic import fails.

---

## 6. Final check

| Check | Where |
|---|---|
| GitHub release is marked **Latest** | https://github.com/safarafique/GExPipe/releases |
| Zenodo shows the new version | https://doi.org/10.5281/zenodo.23401720 |
| Bioconductor build is OK (1–2 days) | https://bioconductor.org/checkResults/devel/bioc-LATEST/GExPipe/ |
| Installed version in R | `packageVersion("GExPipe")` |

---

## Adding a new author or contributor

Add the person in **all four** places so GitHub, Bioconductor and Zenodo show the same list:

1. `DESCRIPTION` → `Authors@R`, e.g.

   ```r
   person(given = "First", family = "Last", email = "name@example.org",
          role = "aut", comment = c(ORCID = "0000-0000-0000-0000"))
   ```

   Use `role = "ctb"` for a contributor who is not an author. Only one person has `"cre"` (the maintainer).
2. `.zenodo.json` → `"creators"`: `{"name": "Last, First", "orcid": "0000-0000-0000-0000"}`
3. `CITATION.cff` → `authors:` list.
4. `inst/CITATION` if it lists authors by name.

Then release a new version (steps 1–6). Changing the maintainer (`cre`) also needs to be reported to
Bioconductor; see https://contributions.bioconductor.org/ .

---

## Common problems

| Message | Meaning / fix |
|---|---|
| `tag name can't be blank` (GitHub) | Set the tag in step 5.2 |
| `gh: To get started with GitHub CLI, please run: gh auth login` | Run `gh auth login`, or use the website |
| `package(s) not installed when version(s) same as or greater than current` (R) | You already have this version; add `force = TRUE` to reinstall |
| `Some listed binary packages have no source` (R) | Harmless repository message |
| Bioconductor push rejected | Version not increased, or SSH key not registered |
| Zenodo shows the wrong version number | `.zenodo.json` was not updated before the release |
| New Zenodo version did not appear | Check the switch at https://zenodo.org/account/settings/github/ ; discard any open Zenodo draft; the release must not be a pre-release or draft |
