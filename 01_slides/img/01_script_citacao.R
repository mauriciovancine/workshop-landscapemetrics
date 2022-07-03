# package
library(namedropR)

# bib
bib_path <- "01_slides/img/01_citation.bib"

# example
drop_name(bib_path,
          cite_key = "hesselbarth_landscapemetrics_2019",
          output_dir = "01_slides/img",
          export_as = "png")
