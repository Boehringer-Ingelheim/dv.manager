# export_element_formatters formats PDF default elements, with a section header only for the first element

    Code
      cat(first)
    Output
      
      \section{Module 1}
      
      \subsection{Element 1}
      
      
      
      ```{r mod1-el1}
      1 + 1
      ```
      

---

    Code
      cat(later)
    Output
      
      \subsection{Element 1}
      
      
      
      ```{r mod1-el1}
      1 + 1
      ```
      

# export_element_formatters wraps PDF tables in a wide page sized to char_width

    Code
      cat(out)
    Output
      
      
      \beginwidepage{42}
      
      \begingroup
      
      \wptabfont
      
      
      \section{Module 1}
      
      \subsection{Element 1}
      
      
      
      ```{r mod1-el1}
      1 + 1
      ```
      
      
      
      \endgroup
      
      \stopwidepage
      

# export_element_formatters flags PDF errors with alertwarning

    Code
      cat(out)
    Output
      
      \section{Module 1}
      
      \subsection{Element 1}
      
      
      
      \alertwarning{boom}
      

# export_element_formatters formats HTML elements, and table delegates to default

    Code
      cat(default_out)
    Output
      
      # Module 1
      
      ## Element 1
      
      
      
      ```{r mod1-el1}
      1 + 1
      ```
      

---

    Code
      cat(table_out)
    Output
      
      # Module 1
      
      ## Element 1
      
      
      
      ```{r mod1-el1}
      1 + 1
      ```
      

# export_element_formatters flags HTML errors with an alert div

    Code
      cat(out)
    Output
      
      # Module 1
      
      ## Element 1
      
      
      
      <div class = "alert alert-warning" role = "alert">boom</div>
      

# build_export_rmd assembles an HTML document from all pieces, in order

    Code
      cat(rmd)
    Output
      FIXTURE_HEADER_TEMPLATE
      
      ```{r data_source}
      df <- load_data()
      ```
      
      
      
      # Module 1
      
      ## Element 1
      
      
      
      ```{r mod1-el1}
      1 + 1
      ```
      
      
      
      
      ## Element 2
      
      
      
      <div class = "alert alert-warning" role = "alert">boom</div>
      
      
      
      
      # Data source
      
      FIXTURE_DATE_SECTION
      
      FIXTURE_HARDCODED_HASH_SECTION
      
      FIXTURE_DYNAMIC_HASH_SECTION
      
      FIXTURE_FILTER_TXT_SECTION
      
      FIXTURE_FILTER_REFERENCE_SECTION
      
      FIXTURE_SESSION_INFO_TEMPLATE
      
      FIXTURE_FOOTER_TEMPLATE
      

# build_export_rmd wraps a PDF table element in a wide page

    Code
      cat(rmd)
    Output
      FIXTURE_HEADER_TEMPLATE
      
      ```{r data_source}
      df <- load_data()
      ```
      
      
      
      
      \beginwidepage{10}
      
      \begingroup
      
      \wptabfont
      
      
      \section{Module 1}
      
      \subsection{Table 1}
      
      
      
      ```{r mod1-el1}
      1 + 1
      ```
      
      
      
      \endgroup
      
      \stopwidepage
      
      
      
      
      # Data source
      
      FIXTURE_DATE_SECTION
      
      FIXTURE_HARDCODED_HASH_SECTION
      
      FIXTURE_DYNAMIC_HASH_SECTION
      
      FIXTURE_FILTER_TXT_SECTION
      
      FIXTURE_FILTER_REFERENCE_SECTION
      
      FIXTURE_SESSION_INFO_TEMPLATE
      
      FIXTURE_FOOTER_TEMPLATE
      

