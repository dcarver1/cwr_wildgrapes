# Copy one taxon's summary document from a run to the Vitis website repository.
#
# The live page links m/<taxon>_Summary_fnaFilter.html. Since the October 2026
# taxonomy update each live document carries (a) a banner after <body> that
# names the update and links the published version kept under m_published2025,
# and (b) the citation footer added by html_injectFooter.R in the website repo.
# This script reproduces that for a single taxon after a re-run.
#
#   Rscript -e 'taxon <- "Vitis tiliifolia"; runVersion <- "run10222026_1k"; source("utilities/publishTaxonSummary.R")'
#
# It copies the file, inserts the banner when it is missing and runs the footer
# script (idempotent over the whole m/ folder). It does not commit or push.

if (!exists("taxon")) stop("preset `taxon`")
if (!exists("runVersion")) stop("preset `runVersion`")
if (!exists("websiteRepo")) websiteRepo <- path.expand("~/trueNAS/work/vitis")

src <- file.path("data/Vitis/speciesSummaryHTML", runVersion, paste0(taxon, "_Summary_", runVersion, ".html"))
dst <- file.path(websiteRepo, "m", paste0(taxon, "_Summary_fnaFilter.html"))
if (!file.exists(src)) stop("summary document not found: ", src)
if (!file.exists(dst)) stop("no live document to replace (new taxa need a card in gapAnalysis.md as well): ", dst)

html <- paste(readLines(src, warn = FALSE), collapse = "\n")
banner <- paste0(
  '<div class="taxonomy-update-banner" style="background:#fff8e1;border-bottom:1px solid #e0c200;padding:8px 16px;font-family:sans-serif;font-size:0.9rem">',
  'Updated October 2026 with the corrected taxonomy. <a href="../m_published2025/', utils::URLencode(taxon), '_Summary_fnaFilter.html">',
  'Open the version as published (August 2025 run)</a>.</div>')
if (!grepl("taxonomy-update-banner", html, fixed = TRUE)) {
  html <- sub("<body>", paste0("<body>", banner), html, fixed = TRUE)
}
writeLines(html, dst)
message("written: ", dst)

# citation footer (website repo script, skips files that already carry it)
source(file.path(websiteRepo, "html_injectFooter.R"))
