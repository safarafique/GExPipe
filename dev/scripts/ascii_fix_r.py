"""Replace non-ASCII punctuation in R sources for portable R CMD check."""
from pathlib import Path

REPLACEMENTS = {
    "\u2014": "-",  # em dash
    "\u2013": "-",  # en dash
    "\u2192": "->",
    "\u2265": ">=",
    "\u03b2": "beta",
    "\u2229": "intersect",
    "\u2713": "OK",
    "\u2717": "X",
    # mojibake from UTF-8 misread as Latin-1
    "\u00d4\u00c7\u00d6": "-",  # ÔÇö
    "\u00d4\u00e5\u00d2": "->",  # ÔåÆ
    "\u00d4\u00f6\u00c7": "-",  # ÔöÇ (box drawing)
}

r_dir = Path(__file__).resolve().parents[2] / "R"
for path in sorted(r_dir.glob("*.R")):
    text = path.read_text(encoding="utf-8")
    orig = text
    for old, new in REPLACEMENTS.items():
        text = text.replace(old, new)
    if text != orig:
        path.write_text(text, encoding="utf-8", newline="\n")
        print(f"fixed: {path.name}")
