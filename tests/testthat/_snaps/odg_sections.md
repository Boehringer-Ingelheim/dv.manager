# build_hardcoded_hash_section lists a content hash per dataset

    Code
      cat(build_hardcoded_hash_section(dataset_list))
    Output
      ## Hardcoded Data hash:
      
       **name**: `a` **hash**: 55a120a9c16e53051c6fb3c477eca7dc
      
       **name**: `b` **hash**: 3283d8f999f408731e81590e84265eec
      
      These hashes are calculated in-app, they correspond to the data loaded in the app that created this output documentation.

# build_hardcoded_hash_section handles an empty dataset list

    Code
      cat(build_hardcoded_hash_section(list()))
    Output
      ## Hardcoded Data hash:
      
      These hashes are calculated in-app, they correspond to the data loaded in the app that created this output documentation.

# build_dynamic_hash_section emits an asis loop referencing the shared dataset-list variable

    Code
      cat(build_dynamic_hash_section(sdl, gcic))
    Output
      ## Dynamic Data hash:
      
      ```{r dynamic_data_hash, echo = FALSE, results='asis'}
      for (idx in seq_along(selected_dataset_list)) {
          cat(sprintf("\n\n **name**: `%s` **hash**: %s", names(selected_dataset_list)[[idx]], digest::digest(selected_dataset_list[[idx]], algo = "md5")))
      }
      ```
      
      These hashes are calculated while generating the output documentation, and should match those in the **Hardcoded Data hash** section.

# build_dynamic_hash_section handles zero datasets

    Code
      cat(build_dynamic_hash_section(sdl, gcic))
    Output
      ## Dynamic Data hash:
      
      ```{r dynamic_data_hash, echo = FALSE, results='asis'}
      for (idx in seq_along(selected_dataset_list)) {
          cat(sprintf("\n\n **name**: `%s` **hash**: %s", names(selected_dataset_list)[[idx]], digest::digest(selected_dataset_list[[idx]], algo = "md5")))
      }
      ```
      
      These hashes are calculated while generating the output documentation, and should match those in the **Hardcoded Data hash** section.

# build_date_section embeds the date range and a per-dataset modification-time loop

    Code
      cat(build_date_section(sdl, date_range_mr, gcic))
    Output
      ## Data Modification Dates:
      
       **Date range**:
      
      `r date_range <- as.POSIXct(c("2026-01-01", "2026-01-02"), tz = "UTC")
      date_range`
      
      ```{r data_modification_dates, echo = FALSE, results='asis'}
      for (idx in seq_along(selected_dataset_list)) {
          mtime <- attr(selected_dataset_list[[idx]], "meta")[["mtime"]]
          mtime <- if (is.null(mtime)) 
              "Time not available"
          else format(mtime)
          cat(sprintf("\n\n **name**: `%s` **modification time**: %s", names(selected_dataset_list)[[idx]], mtime))
      }
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
      
      ```{r data_modification_dates, echo = FALSE, results='asis'}
      for (idx in seq_along(selected_dataset_list)) {
          mtime <- attr(selected_dataset_list[[idx]], "meta")[["mtime"]]
          mtime <- if (is.null(mtime)) 
              "Time not available"
          else format(mtime)
          cat(sprintf("\n\n **name**: `%s` **modification time**: %s", names(selected_dataset_list)[[idx]], mtime))
      }
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
      
      ```{r data_modification_dates, echo = FALSE, results='asis'}
      for (idx in seq_along(selected_dataset_list)) {
          mtime <- attr(selected_dataset_list[[idx]], "meta")[["mtime"]]
          mtime <- if (is.null(mtime)) 
              "Time not available"
          else format(mtime)
          cat(sprintf("\n\n **name**: `%s` **modification time**: %s", names(selected_dataset_list)[[idx]], mtime))
      }
      ```
      
      These dates are calculated while generating the output documentation.

# build_filter_txt_section wraps the filter code in format-appropriate verbatim tags

    Code
      cat(build_filter_section(build_filter_txt_section, ODG$OUTPUT_FORMAT$HTML))
    Output
      ## Filters:
      
      <pre>
      
      ```{r filter_odg_txt, echo = FALSE, results='asis'}
      
      local(cat(filter_odg[["txt"]]))
      
      ```
      
      </pre>
      
      An explicit call to the filter and parameters used can be found in the code that accompanies this output documentation.

---

    Code
      cat(build_filter_section(build_filter_txt_section, ODG$OUTPUT_FORMAT$PDF))
    Output
      ## Filters:
      
      \begin{verbatim}
      
      ```{r filter_odg_txt, echo = FALSE, results='asis'}
      
      local(cat(filter_odg[["txt"]]))
      
      ```
      
      \end{verbatim}
      
      An explicit call to the filter and parameters used can be found in the code that accompanies this output documentation.

# build_filter_reference_section wraps the filter reference code so pandoc does not read it as markdown

    Code
      cat(build_filter_section(build_filter_reference_section, ODG$OUTPUT_FORMAT$HTML))
    Output
      # Filter references:
      
      <pre>
      
      ```{r filter_odg_reference_list, echo = FALSE, results='asis'}
      
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
      
      ```{r filter_odg_reference_list, echo = FALSE, results='asis'}
      
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

