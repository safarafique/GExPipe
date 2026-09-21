"""Update GExPipe.docx with current features (Parallel DE, 16-step, multi-GSE)."""
from __future__ import annotations

import shutil
import sys
from pathlib import Path

try:
    from docx import Document
except ImportError:
    print("python-docx required", file=sys.stderr)
    sys.exit(1)

DOCX = Path(r"d:\GExPipe-123\GExPipe_Manuscript\GExPipe.docx")
BACKUP = DOCX.with_suffix(".docx.bak")


def replace_in_paragraph(p, old: str, new: str) -> bool:
    if old not in p.text:
        return False
    if p.runs:
        full = p.text.replace(old, new)
        for r in p.runs[1:]:
            r.text = ""
        p.runs[0].text = full
    else:
        p.text = p.text.replace(old, new)
    return True


def replace_everywhere(doc: Document, replacements: list[tuple[str, str]]) -> int:
    n = 0
    for p in doc.paragraphs:
        for old, new in replacements:
            if replace_in_paragraph(p, old, new):
                n += 1
    for table in doc.tables:
        for row in table.rows:
            for cell in row.cells:
                for p in cell.paragraphs:
                    for old, new in replacements:
                        if replace_in_paragraph(p, old, new):
                            n += 1
    return n


def insert_after_paragraph_containing(doc: Document, needle: str, new_paragraphs: list[str]) -> bool:
    for p in doc.paragraphs:
        if needle in p.text:
            ref = p._element
            for text in reversed(new_paragraphs):
                new_p = doc.add_paragraph(text)
                ref.addnext(new_p._element)
            return True
    return False


def main() -> None:
    if not BACKUP.is_file():
        shutil.copy2(DOCX, BACKUP)

    doc = Document(str(BACKUP))

    global_replacements = [
        (
            "within a single gated, 15-step environment",
            "within a single gated, 16-step environment supporting four analysis types "
            "(RNA-seq only, microarray only, Merged (Both), and Parallel DE then merge)",
        ),
        ("gated, 15-step Shiny workflow", "gated, 16-step Shiny workflow"),
        ("gated, 15-step workflow panel", "gated, 16-step workflow panel"),
        ("gated 15-step analysis bar", "gated 16-step analysis bar"),
        ("15-step GExPipe workflow", "16-step GExPipe workflow"),
        ("R ≥ 4.5.0 (Bioconductor 3.24, devel branch)", "R ≥ 4.6.0 (Bioconductor 3.22)"),
        ("GExPipe (version 0.99.51)", "GExPipe (version 0.99.105)"),
        (
            "multi-engine differential expression, WGCNA network construction, "
            "eight-algorithm ensemble machine learning and in-pipeline validation",
            "multi-engine differential expression (limma, DESeq2, edgeR, limma-voom), "
            "Parallel DE with platform-matched engines, WGCNA network construction, "
            "eight-algorithm ensemble machine learning, GSEA, and in-pipeline validation",
        ),
        (
            "Figure 1. The GExPipe interface showing the gated 15-step analysis bar.",
            "Figure 1. The GExPipe interface showing the gated 16-step analysis bar "
            "and four analysis types at Step 1.",
        ),
        (
            "Figure 2. Schematic of the 15-step GExPipe workflow from GEO data retrieval to validation and export.",
            "Figure 2. Schematic of the 16-step GExPipe workflow from GEO data retrieval to validation and export, "
            "including optional Step 7 (RNA-seq ∩ microarray) for Parallel DE.",
        ),
        (
            "(C) identification of common genes among intersections",
            "(C) identification of common genes among DEG and WGCNA module intersections "
            "(Step 9; Step 7 for Parallel DE)",
        ),
    ]
    replace_everywhere(doc, global_replacements)

    intro_add = (
        "GExPipe implements both joint cross-platform modelling (Merged (Both)) and a Parallel DE "
        "path that runs platform-appropriate normalisation, batch correction and DE separately "
        "before Step 7 (RNA-seq ∩ microarray) retains same-direction genes significant on both platforms."
    )
    for p in doc.paragraphs:
        if "joint modelling of harmonised matrices" in p.text and "Parallel DE" not in p.text:
            t = p.text.rstrip()
            if not t.endswith("."):
                t += "."
            t += " " + intro_add
            if p.runs:
                for r in p.runs[1:]:
                    r.text = ""
                p.runs[0].text = t
            else:
                p.text = t
            break

    parallel_section = [
        "2.1.1 Analysis types and Parallel DE workflow",
        (
            "Step 1 selects one of four analysis types. RNA-seq only and microarray only restrict "
            "download to a single platform but accept multiple GEO series (GSE) in the same run. "
            "Merged (Both) downloads all typed GSE IDs on each side, maps probes to HGNC symbols "
            "(Step 2b when needed), normalises each study, intersects common genes, optionally applies "
            "global quantile alignment, performs one condition-protected batch correction, and runs a "
            "single joint limma differential expression model on the merged matrix. Parallel DE then "
            "merge keeps platforms separate from download through Step 6: gene lists are not intersected "
            "early; each platform has its own normalisation (Auto follows the Step 1 RNA-seq DE choice—"
            "raw counts for DESeq2, edgeR or limma-voom; limma on TMM when limma is selected), "
            "QC visualisation, group assignment, and batch correction (separate methods per platform, "
            "not joint ComBat). Step 6 runs two DE engines with one action—microarray limma and "
            "RNA-seq DESeq2, edgeR, limma-voom or limma—with independent log2 fold-change, adjusted P "
            "and heatmap gene cutoffs. Step 7 (RNA-seq ∩ microarray) is available only for Parallel DE; "
            "Auto retains same-direction dual-platform significant genes for Step 9 (DEG ∩ WGCNA modules), "
            "while Step 8 WGCNA builds one network on the platform with more samples (or user choice), "
            "using top variable genes on RNA-seq VST or batch-corrected microarray values rather than "
            "consensus DEGs alone. Steps 10–16 (STRING PPI, eight-algorithm ML, validation, ROC, "
            "nomogram, GSEA, PDF summary) are shared with context-aware labels for the active DEG source."
        ),
        (
            "Parallel mode uses a two-column UI (RNA-seq left, microarray right) from normalisation "
            "through DE, with separate run logs and QC gene-symbol overlap plots (Venn/UpSet). "
            "Multi-GSE entry in Merged and Parallel allows several array and RNA-seq cohorts in one session."
        ),
    ]
    insert_after_paragraph_containing(
        doc, "Workspace state can be saved and restored", parallel_section
    )

    merged_parallel_note = (
        "For Merged (Both), per-platform normalisation precedes common-gene intersection and optional "
        "global quantile alignment before one joint batch step and one limma DE model. For Parallel DE, "
        "platforms are normalised and batch-corrected independently without early gene intersection or "
        "global quantile; differential expression remains platform-specific until Step 7 consensus."
    )
    insert_after_paragraph_containing(
        doc, "2.2 Normalisation, batch correction and differential expression", [merged_parallel_note]
    )

    # Feature checklist paragraph (16 steps overview)
    steps_overview = (
        "The 16 sidebar steps are grouped as data preparation (1–5: download, per-study normalisation, "
        "QC and outlier handling, group selection, batch correction), gene discovery (6–9: differential "
        "expression, optional RNA-seq ∩ microarray consensus, WGCNA, DEG ∩ module overlap with GO/KEGG), "
        "candidate refinement (10–13: PPI, ML ensemble, validation, ROC with optional immune deconvolution), "
        "and clinical translation (14–16: nomogram, GSEA, automated PDF summary). Probe-to-symbol mapping, "
        "PVCA variance partitioning, workspace save/load, and 300 dpi figure export are integrated throughout."
    )
    insert_after_paragraph_containing(doc, "2.3 Implementation and performance", [steps_overview])

    doc.save(str(DOCX))
    print(f"Saved {DOCX} from clean backup")


if __name__ == "__main__":
    main()
