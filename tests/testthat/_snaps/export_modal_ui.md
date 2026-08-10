# build_export_modal_ui shows every element and selects all of them by default

    Code
      cat(as.character(res[["modal_dialog"]]))
    Output
      <div class="modal fade" id="shiny-modal" tabindex="-1">
        <div class="modal-dialog">
          <div class="modal-content">
            <div class="modal-body">
              <div class="d-flex flex-column vh-25" style="max-height: 90vh">
                <div class="overflow-auto flex-grow-1 p-3 min-h-0">
                  <h3>Export menu</h3>
                  <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-require-bs-caller="card()" data-require-bs-version="5">
                    <div class="card-header bslib-gap-spacing">Module 1</div>
                    <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
                      <div class="form-check form-switch">
                        <label class="form-check-label" title="Info m1-a">
                          <input class="form-check-input" type="checkbox" role="switch" checked onchange="Shiny.setInputValue(&#39;test-export_menu_selection&#39;, {value: this.checked, id: &#39;m1-a&#39;});"/>
                          Label m1-a
                        </label>
                      </div>
                      <div class="form-check form-switch">
                        <label class="form-check-label" title="Info m1-b">
                          <input class="form-check-input" type="checkbox" role="switch" checked onchange="Shiny.setInputValue(&#39;test-export_menu_selection&#39;, {value: this.checked, id: &#39;m1-b&#39;});"/>
                          Label m1-b
                        </label>
                      </div>
                    </div>
                    <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
                  </div>
                  <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-require-bs-caller="card()" data-require-bs-version="5">
                    <div class="card-header bslib-gap-spacing">Module 2</div>
                    <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
                      <div class="form-check form-switch">
                        <label class="form-check-label" title="Info m2-a">
                          <input class="form-check-input" type="checkbox" role="switch" checked onchange="Shiny.setInputValue(&#39;test-export_menu_selection&#39;, {value: this.checked, id: &#39;m2-a&#39;});"/>
                          Label m2-a
                        </label>
                      </div>
                    </div>
                    <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
                  </div>
                  <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-require-bs-caller="card()" data-require-bs-version="5">
                    <div class="card-header bslib-gap-spacing">Output format</div>
                    <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
                      <div id="test-output_format" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="test-output_format-label">
                        <label class="control-label shiny-label-null" for="test-output_format" id="test-output_format-label"></label>
                        <div class="shiny-options-group">
                          <div class="radio">
                            <label>
                              <input type="radio" name="test-output_format" value="html" checked="checked"/>
                              <span>HTML</span>
                            </label>
                          </div>
                          <div class="radio">
                            <label>
                              <input type="radio" name="test-output_format" value="pdf"/>
                              <span>PDF</span>
                            </label>
                          </div>
                        </div>
                      </div>
                    </div>
                    <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
                  </div>
                </div>
                <a id="test-export_code" class="btn btn-default shiny-download-link disabled" href="" target="_blank" download aria-disabled="true" tabindex="-1">
                  <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
                  Export
                </a>
              </div>
            </div>
          </div>
        </div>
        <script>if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\./)) {
               var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));
               modal.show();
            } else {
               $('#shiny-modal').modal().focus();
            }</script>
      </div>

