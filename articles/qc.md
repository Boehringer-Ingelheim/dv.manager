# Quality Control

### ✅ dv.manager 3.0.0

Date: 2026-Mar-05 14:05:08

This document provides the Quality Control (QC) report for the R package
to confirm that it fulfills the criteria required for a “released”
status. The QC report contains the following information:

- **Specifications (specs):** These can be attached to every test that
  the user adds.
- **Traceability matrix:** Contains test cases with passed, failed, or
  skipped expectations.
- **Uncovered or undeclared specs**
- **Session Info and System Configuration**

------------------------------------------------------------------------

## Traceability matrix

In this traceability matrix only those tests that point to an
specification are included.

Test cases can contain several expectations a test is considered:

- **passed** if all expectations in the test pass.

- **failed** if at least one expectation in the test fails.

- **skipped** if at least one expectation in the test is skipped.

A test can be both **failed** and **skipped**.

### Summary

### Passed tests

### Failed tests

### Skipped tests

### Uncovered specifications

### Undeclared specifications

This should always be empty, as non existant specs are controlled during
test execution.

## Session Info and System Configuration

    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.5.2 (2025-10-31)
    #>  os       Ubuntu 22.04.5 LTS
    #>  system   x86_64, linux-gnu
    #>  ui       X11
    #>  language en
    #>  collate  en_US.UTF-8
    #>  ctype    en_US.UTF-8
    #>  tz       Etc/UTC
    #>  date     2026-03-05
    #>  pandoc   3.9 @ /usr/bin/ (via rmarkdown)
    #>  quarto   1.8.27 @ /usr/local/bin/quarto
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package     * version date (UTC) lib source
    #>  bslib         0.9.0   2025-01-30 [2] RSPM
    #>  cachem        1.1.0   2024-05-16 [2] RSPM
    #>  cli           3.6.5   2025-04-23 [2] RSPM
    #>  crosstalk     1.2.2   2025-08-26 [2] RSPM
    #>  desc          1.4.3   2023-12-10 [2] RSPM
    #>  devtools      2.4.6   2025-10-03 [2] RSPM
    #>  digest        0.6.39  2025-11-19 [2] RSPM
    #>  DT            0.34.0  2025-09-02 [2] RSPM
    #>  ellipsis      0.3.2   2021-04-29 [2] RSPM
    #>  evaluate      1.0.5   2025-08-27 [2] RSPM
    #>  fastmap       1.2.0   2024-05-15 [2] RSPM
    #>  fs            1.6.6   2025-04-12 [2] RSPM
    #>  glue          1.8.0   2024-09-30 [2] RSPM
    #>  htmltools     0.5.8.1 2024-04-04 [2] RSPM
    #>  htmlwidgets   1.6.4   2023-12-06 [2] RSPM
    #>  jquerylib     0.1.4   2021-04-26 [2] RSPM
    #>  jsonlite      2.0.0   2025-03-27 [2] RSPM
    #>  knitr         1.50    2025-03-16 [2] RSPM
    #>  lifecycle     1.0.4   2023-11-07 [2] RSPM
    #>  magrittr      2.0.4   2025-09-12 [2] RSPM
    #>  memoise       2.0.1   2021-11-26 [2] RSPM
    #>  pkgbuild      1.4.8   2025-05-26 [2] RSPM
    #>  pkgdown       2.2.0   2025-11-06 [2] RSPM
    #>  pkgload       1.4.1   2025-09-23 [2] RSPM
    #>  purrr         1.2.0   2025-11-04 [2] RSPM
    #>  R6            2.6.1   2025-02-15 [2] RSPM
    #>  ragg          1.5.0   2025-09-02 [2] RSPM
    #>  remotes       2.5.0   2024-03-17 [2] RSPM
    #>  rlang         1.1.6   2025-04-11 [2] RSPM
    #>  rmarkdown     2.30    2025-09-28 [2] RSPM
    #>  sass          0.4.10  2025-04-11 [2] RSPM
    #>  sessioninfo   1.2.3   2025-02-05 [2] RSPM
    #>  systemfonts   1.3.1   2025-10-01 [2] RSPM
    #>  textshaping   1.0.4   2025-10-10 [2] RSPM
    #>  usethis       3.2.1   2025-09-06 [2] RSPM
    #>  vctrs         0.6.5   2023-12-01 [2] RSPM
    #>  xfun          0.54    2025-10-30 [2] RSPM
    #>  yaml          2.3.11  2025-11-28 [2] RSPM
    #> 
    #>  [1] /tmp/RtmpJLN93q/temp_libpath1ac5534887e
    #>  [2] /usr/local/lib/R/site-library
    #>  [3] /usr/local/lib/R/library
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────

## List of specifications
