# nolint start

TT <- local({
  SUBJECT_LEVEL_FILTER <-
    "Apply a filter to the dataset and use the resulting subject IDs (default) to consistently filter the rest of datasets."

  DATASET_FILTER <-
    "Apply a filter to a specific dataset. Does not impact the rest of datasets. Only datasets that are used by the currently selected module are shown in this dataset."

  poc(
    SUBJECT_LEVEL_FILTER = SUBJECT_LEVEL_FILTER,
    DATASET_FILTER = DATASET_FILTER
  )
})

# nolint end

LAYOUT <- poc(
  # nolint
  ATTRIBUTE = "layout",
  TAB_GROUP = "tab_group"
)

ID <- poc(
  NAV_HEADER = "nav_header",
  FILTER = "filter",
  SUBGROUP = "subgroup",
  FILTER_STATE_JSON_INPUT = "filter_state_json_input",
  FILTER_LOG_INPUT = "filter_log_input",
  SAVED_FILTER_STATE_JSON_MSG_INPUT = "saved_filter_state_json_msg_input",
  FILTER_MODE_INPUT = "filter_mode",
  EXPORT_FILTER_CODE_INPUT = "export_filter_code_button_input",
  BLOCKLY = poc(
    CONTAINER = "blockly_container",
    GEN_CODE = "gen_code_button",
    INNER_CONTAINER = "blockly_inner_filter_container"
  ),
  EXPORT_CODE = "export_code",
  EXPORT_CODE_MENU = "export_code_menu"
)

REPORT <- poc(
  ATTR = "report_element_type",
  ELEMENT_KIND = poc(
    ERROR = "error",
    TABLE = "table",
    DEFAULT = "default"
  ),
  OUTPUT_FORMAT = poc(
    HTML = "html",
    PDF = "pdf"
  )
)

REPORT[["TEMPLATES"]] <- list()

REPORT[["TEMPLATES"]][["HEADER"]] <- local({
  templates <- character(0)
  templates[[REPORT$OUTPUT_FORMAT$PDF]] <- r"--(
---
title: "A report"
author: "A user"
date: "`r format(Sys.Date(), '%B %d, %Y')`"
output:
  pdf_document:
    keep_tex: true
    toc: true
    toc_depth: 3
    number_sections: false
    df_print: kable
    latex_engine: xelatex
header-includes:
  - \usepackage{atbegshi}
  - \usepackage{graphicx}
  - \usepackage{xcolor}
  - \AtBeginShipout{\AtBeginShipoutUpperLeft{\put(25,-420){\rotatebox{90}{\color{red}\fontsize{20pt}{24pt}\selectfont UNVALIDATED CONTENT}}\put(570,-420){\rotatebox{90}{\color{red}\fontsize{20pt}{24pt}\selectfont UNVALIDATED CONTENT}}}}
  - \newcommand{\alertwarning}[1]{\par\vspace{4pt}\noindent\fcolorbox{yellow!70!black}{yellow!20}{\parbox{\dimexpr\linewidth-2\fboxsep-2\fboxrule}{\color{yellow!60!black}\detokenize{#1}}}\par\vspace{4pt}}

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, tidy = TRUE, tidy.opts = list(width.cutoff = 60))
```

)--"

  templates[[REPORT$OUTPUT_FORMAT$HTML]] <- r"--(
---
title: "A report"
author: "A user"
date: "`r format(Sys.Date(), '%B %d, %Y')`"
output:
  html_document:
    code_folding: "hide"
    code_download: true
    toc: true
    toc_float: true
    toc_depth: 3
    number_sections: false
    df_print: paged    
    self_contained: true
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  out.width = "100%", 
  tidy = TRUE
)
```
        
```{css, echo=FALSE}
body::before {
  content: "UNVALIDATED CONTENT";
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) rotate(-45deg);
  font-size: 8rem;
  font-weight: 700;
  color: rgba(0, 0, 0, 0.08);
  white-space: nowrap;
  pointer-events: none;   /* clicks/selection pass through */
  z-index: 9999;
  user-select: none;
}
        
@media print {
  body::before { position: fixed; }  /* most browsers repeat fixed bg per page */
}
```

)--"

  templates
})

REPORT[["TEMPLATES"]][["FOOTER"]] <- local({
  templates <- character(0)
  templates[[REPORT$OUTPUT_FORMAT$PDF]] <- ""

  # Currently code is attached to the PDF file
  #   r"--(
  # # Annex: Code

  # ```{r show-code, ref.label = setdiff(knitr::all_labels(), c("setup", "show-code")), echo=TRUE, eval=FALSE}
  # ```
  # )--"

  templates[[REPORT$OUTPUT_FORMAT$HTML]] <- ""

  templates
})
