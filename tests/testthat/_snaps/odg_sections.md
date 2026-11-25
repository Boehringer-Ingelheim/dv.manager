# build_hardcoded_hash_section lists a content hash per dataset

    Code
      cat(build_hardcoded_hash_section(hash_dataset_list(dataset_list)))
    Output
      ## Hardcoded Data hash:
      
       **name**: `a` **hash**: 4c5be99243505147
      
       **name**: `b` **hash**: e3d711106256114f
      
      These hashes are calculated in-app, they correspond to the data loaded in the app that created this output documentation.

# build_hardcoded_hash_section handles an empty dataset list

    Code
      cat(build_hardcoded_hash_section(hash_dataset_list(list())))
    Output
      ## Hardcoded Data hash:
      
      These hashes are calculated in-app, they correspond to the data loaded in the app that created this output documentation.

# build_dynamic_hash_section emits an asis loop checking the dataset-list variable against the in-app hashes

    Code
      cat(build_dynamic_hash_section_for(in_app_list, ODG$OUTPUT_FORMAT$HTML))
    Output
      ## Dynamic Data hash:
      
      ```{r odg-dynamic_data_hash, echo = FALSE, results='asis'}
      local({
          in_app_hash <- c(a = "4c5be99243505147", b = "e3d711106256114f")
          not_found <- "Dataset not found"
          for (name in union(names(selected_dataset_list), names(in_app_hash))) {
              hash <- if (name %in% names(selected_dataset_list)) {
                  digest::digest(selected_dataset_list[[name]], algo = "xxhash64")
              }
              else {
                  not_found
              }
              expected_hash <- if (name %in% names(in_app_hash)) 
                  in_app_hash[[name]]
              else not_found
              line <- sprintf("\n\n **name**: `%s` **hash**: %s", name, hash)
              if (!identical(hash, expected_hash)) {
                  line <- paste0("\n\n<div class=\"alert alert-danger\" role=\"alert\">", "\n\n **Hash mismatch**: the data used to generate this document is not the data loaded in the app.", line, sprintf(" **in-app hash**: %s", expected_hash), "\n\n</div>")
              }
              cat(line)
          }
      })
      ```
      
      These hashes are calculated while generating the output documentation and checked against those in the **Hardcoded Data hash** section. Any mismatch is highlighted.

---

    Code
      cat(build_dynamic_hash_section_for(in_app_list, ODG$OUTPUT_FORMAT$PDF))
    Output
      ## Dynamic Data hash:
      
      ```{r odg-dynamic_data_hash, echo = FALSE, results='asis'}
      local({
          in_app_hash <- c(a = "4c5be99243505147", b = "e3d711106256114f")
          not_found <- "Dataset not found"
          for (name in union(names(selected_dataset_list), names(in_app_hash))) {
              hash <- if (name %in% names(selected_dataset_list)) {
                  digest::digest(selected_dataset_list[[name]], algo = "xxhash64")
              }
              else {
                  not_found
              }
              expected_hash <- if (name %in% names(in_app_hash)) 
                  in_app_hash[[name]]
              else not_found
              line <- sprintf("\n\n **name**: `%s` **hash**: %s", name, hash)
              if (!identical(hash, expected_hash)) {
                  line <- paste0("\n\n```{=latex}\n\\begin{alertdanger}\n```", "\n\n **Hash mismatch**: the data used to generate this document is not the data loaded in the app.", line, sprintf(" **in-app hash**: %s", expected_hash), "\n\n```{=latex}\n\\end{alertdanger}\n```")
              }
              cat(line)
          }
      })
      ```
      
      These hashes are calculated while generating the output documentation and checked against those in the **Hardcoded Data hash** section. Any mismatch is highlighted.

# build_dynamic_hash_section handles zero datasets

    Code
      cat(section)
    Output
      ## Dynamic Data hash:
      
      ```{r odg-dynamic_data_hash, echo = FALSE, results='asis'}
      local({
          in_app_hash <- character(0)
          not_found <- "Dataset not found"
          for (name in union(names(selected_dataset_list), names(in_app_hash))) {
              hash <- if (name %in% names(selected_dataset_list)) {
                  digest::digest(selected_dataset_list[[name]], algo = "xxhash64")
              }
              else {
                  not_found
              }
              expected_hash <- if (name %in% names(in_app_hash)) 
                  in_app_hash[[name]]
              else not_found
              line <- sprintf("\n\n **name**: `%s` **hash**: %s", name, hash)
              if (!identical(hash, expected_hash)) {
                  line <- paste0("\n\n<div class=\"alert alert-danger\" role=\"alert\">", "\n\n **Hash mismatch**: the data used to generate this document is not the data loaded in the app.", line, sprintf(" **in-app hash**: %s", expected_hash), "\n\n</div>")
              }
              cat(line)
          }
      })
      ```
      
      These hashes are calculated while generating the output documentation and checked against those in the **Hardcoded Data hash** section. Any mismatch is highlighted.

# the dynamic hash chunk highlights changed datasets and those only one side has

    Code
      cat(html)
    Output
      
      
      <div class="alert alert-danger" role="alert">
      
       **Hash mismatch**: the data used to generate this document is not the data loaded in the app.
      
       **name**: `a` **hash**: 4c5be99243505147 **in-app hash**: d2f4e68889aec86a
      
      </div>
      
       **name**: `b` **hash**: e3d711106256114f
      
      <div class="alert alert-danger" role="alert">
      
       **Hash mismatch**: the data used to generate this document is not the data loaded in the app.
      
       **name**: `d` **hash**: 6beddb0fa38b45cf **in-app hash**: Dataset not found
      
      </div>
      
      <div class="alert alert-danger" role="alert">
      
       **Hash mismatch**: the data used to generate this document is not the data loaded in the app.
      
       **name**: `c` **hash**: Dataset not found **in-app hash**: 194a9469fc6f7b82
      
      </div>

---

    Code
      cat(pdf)
    Output
      
      
      ```{=latex}
      \begin{alertdanger}
      ```
      
       **Hash mismatch**: the data used to generate this document is not the data loaded in the app.
      
       **name**: `a` **hash**: 4c5be99243505147 **in-app hash**: d2f4e68889aec86a
      
      ```{=latex}
      \end{alertdanger}
      ```
      
       **name**: `b` **hash**: e3d711106256114f
      
      ```{=latex}
      \begin{alertdanger}
      ```
      
       **Hash mismatch**: the data used to generate this document is not the data loaded in the app.
      
       **name**: `d` **hash**: 6beddb0fa38b45cf **in-app hash**: Dataset not found
      
      ```{=latex}
      \end{alertdanger}
      ```
      
      ```{=latex}
      \begin{alertdanger}
      ```
      
       **Hash mismatch**: the data used to generate this document is not the data loaded in the app.
      
       **name**: `c` **hash**: Dataset not found **in-app hash**: 194a9469fc6f7b82
      
      ```{=latex}
      \end{alertdanger}
      ```

# build_date_section embeds the date range and a per-dataset modification-time loop

    Code
      cat(build_date_section(sdl, date_range_mr, gcic))
    Output
      ## Data Modification Dates:
      
       **Date range**:
      
      `r date_range <- as.POSIXct(c("2026-01-01", "2026-01-02"), tz = "UTC")
      date_range`
      
      ```{r odg-data_modification_dates, echo = FALSE, results='asis'}
      local(for (idx in seq_along(selected_dataset_list)) {
          mtime <- attr(selected_dataset_list[[idx]], "meta")[["mtime"]]
          mtime <- if (is.null(mtime)) 
              "Time not available"
          else format(mtime)
          cat(sprintf("\n\n **name**: `%s` **modification time**: %s", names(selected_dataset_list)[[idx]], mtime))
      })
      ```
      
      These dates are calculated while generating the output documentation.

# build_date_section shows the modification time as not available when meta is missing

    Code
      cat(section)
    Output
      ## Data Modification Dates:
      
       **Date range**:
      
      `r date_range <- as.POSIXct(character(0))
      date_range`
      
      ```{r odg-data_modification_dates, echo = FALSE, results='asis'}
      local(for (idx in seq_along(selected_dataset_list)) {
          mtime <- attr(selected_dataset_list[[idx]], "meta")[["mtime"]]
          mtime <- if (is.null(mtime)) 
              "Time not available"
          else format(mtime)
          cat(sprintf("\n\n **name**: `%s` **modification time**: %s", names(selected_dataset_list)[[idx]], mtime))
      })
      ```
      
      These dates are calculated while generating the output documentation.

# build_date_section handles zero datasets

    Code
      cat(build_date_section(sdl, date_range_mr, gcic))
    Output
      ## Data Modification Dates:
      
       **Date range**:
      
      `r date_range <- as.POSIXct(character(0))
      date_range`
      
      ```{r odg-data_modification_dates, echo = FALSE, results='asis'}
      local(for (idx in seq_along(selected_dataset_list)) {
          mtime <- attr(selected_dataset_list[[idx]], "meta")[["mtime"]]
          mtime <- if (is.null(mtime)) 
              "Time not available"
          else format(mtime)
          cat(sprintf("\n\n **name**: `%s` **modification time**: %s", names(selected_dataset_list)[[idx]], mtime))
      })
      ```
      
      These dates are calculated while generating the output documentation.

# build_filter_txt_section wraps the filter code in format-appropriate verbatim tags

    Code
      cat(build_filter_section(build_filter_txt_section, ODG$OUTPUT_FORMAT$HTML))
    Output
      ## Filters:
      
      <pre>
      
      ```{r odg-filter_odg_txt, echo = FALSE, results='asis'}
      
      local(cat(filter_odg[["txt"]]))
      
      ```
      
      </pre>
      
      An explicit call to the filter and parameters used can be found in the code that accompanies this output documentation. Range bounds marked (rounded) are printed with 15 significant digits, the filter applies their exact value. Check the code in the Rmd to see the exact value applied.

---

    Code
      cat(build_filter_section(build_filter_txt_section, ODG$OUTPUT_FORMAT$PDF))
    Output
      ## Filters:
      
      \begin{verbatim}
      
      ```{r odg-filter_odg_txt, echo = FALSE, results='asis'}
      
      local(cat(filter_odg[["txt"]]))
      
      ```
      
      \end{verbatim}
      
      An explicit call to the filter and parameters used can be found in the code that accompanies this output documentation. Range bounds marked (rounded) are printed with 15 significant digits, the filter applies their exact value. Check the code in the Rmd to see the exact value applied.

# build_filter_reference_section wraps the filter reference code so pandoc does not read it as markdown

    Code
      cat(build_filter_section(build_filter_reference_section, ODG$OUTPUT_FORMAT$HTML))
    Output
      # Filter references:
      
      <pre>
      
      ```{r odg-filter_odg_reference_list, echo = FALSE, results='asis'}
      
      local({
          references <- filter_odg[["references"]]
          if (length(references) == 0) {
              cat("No references found")
          }
          else {
              cat(paste0(paste0("Reference (", seq_along(references), "): \n"), lapply(references, function(x) paste0("\"", x, "\"", collapse = "\t")), "\n", collapse = "\n"))
          }
      })
      
      ```
      
      </pre>

---

    Code
      cat(build_filter_section(build_filter_reference_section, ODG$OUTPUT_FORMAT$PDF))
    Output
      # Filter references:
      
      ```{=latex}
      
      ```{r odg-filter_odg_reference_list, echo = FALSE, results='asis'}
      
      local({
          references <- filter_odg[["references"]]
          if (length(references) == 0) {
              cat("No references found")
          }
          else {
              entries <- character(length(references))
              for (idx in seq_along(references)) {
                  values <- paste0("\\texttt{\"", gt::escape_latex(as.character(references[[idx]])), "\"}", collapse = " ")
                  entries[[idx]] <- sprintf("\\textbf{Reference (%d):}\\\\\n%s", idx, values)
              }
              cat(sprintf("\\begin{flushleft}\n%s\n\\end{flushleft}", paste(entries, collapse = "\n\n")))
          }
      })
      
      ```
      
      ```

