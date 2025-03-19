/**
 * 
 * Based on the number slider field by kozbial@google.com
 * 
 * https://www.npmjs.com/package/@blockly/field-slider
 * 
 */

import * as Blockly from 'blockly/core';
/**
 * Slider field.
 */
export class multiPickerField extends Blockly.FieldTextInput {
    /**
     * Class for a multi selector field.
     *
     * @param value The initial value of the field. Defaults to all.
     * @param validator A function that is called to validate
     *    changes to the field's value. Takes in a number & returns a validated
     *    number, or null to abort the change.
     * @param config A map of options used to configure the field.
     *    See the [field creation documentation]{@link
     * https://developers.google.com/blockly/guides/create-custom-blocks/fields/built-in-fields/number#creation}
     *    for a list of properties this parameter supports.
     */

    CURSOR = 'default';

    constructor(menuGenerator, validator, config) {

      let default_options = [];
      for(let idx = 0; idx < menuGenerator.length; idx++) {
        default_options.push(menuGenerator[idx][0])
      }

      default_options = JSON.stringify(default_options);

        super(default_options, validator, config);
        /**
         * Array holding info needed to unbind events.
         * Used for disposing.
         * Ex: [[node, name, func], [node, name, func]].
         */
        this.boundEvents = [];        
        this.multi_input = null;
        this.menu_options = menuGenerator;        
    }
    /**
     * Constructs a FieldSlider from a JSON arg object.
     *
     * @param options A JSON object with options
     *     (value, min, max, precision).
     * @returns The new field instance.
     * @package
     * @nocollapse
     */
    static fromJson(options) {
        // `this` might be a subclass of FieldSlider if that class doesn't override
        // the static fromJson method.
        return new this(options.value, undefined, undefined, undefined, undefined, options);
    }

    
    doClassValidation_(newValue) {
      if(this.menu_options) {
        let parsed = JSON.parse(newValue);
        let available_options = [];        
        for (let idx = 0; idx < this.menu_options.length; ++idx) {
          available_options.push(this.menu_options[idx][0]);
        }
        let found = parsed.filter((x) => available_options.includes(x));
        let removed = parsed.filter((x) => !available_options.includes(x));
        if(removed.length>0) {
          // We should never reach this point, because filter_state should be cleaned before reaching here
          // and there is no manual way of setting this options
          // This is set in case we pass a newValue that is not contained in the options                 
          alert("Removed values: " + removed.join() + " from " + parsed.join())
        }
        newValue = JSON.stringify(found);     
      }      
      return(newValue);
    }

    widgetCreate_() {
        const block = this.getSourceBlock();
    if (!block) {
      throw new UnattachedFieldError();
    }
    Blockly.Events.setGroup(true);
    const div = Blockly.WidgetDiv.getDiv();

    const clickTarget = this.getClickTarget_();
    if (!clickTarget) throw new Error('A click target has not been set.');
    clickTarget.classList.add('editing');

    const htmlInput = document.createElement('select');
    htmlInput.className = 'blocklyHtmlInput';
    // AnyDuringMigration because:  Argument of type 'boolean' is not assignable
    // to parameter of type 'string'.
    htmlInput.setAttribute(
      'spellcheck',
      this.spellcheck_,
    );
    const scale = this.workspace_.getScale();
    const fontSize = this.getConstants().FIELD_TEXT_FONTSIZE * scale + 'pt';
    div.style.fontSize = fontSize;
    htmlInput.style.fontSize = fontSize;
    let borderRadius = 4 * scale + 'px'; // MAGIC NUMBER FROM https://github.com/google/blockly/blob/d016801089b2c580416674ab947d2bd0ddf3f854/core/field_input.ts#L59

    if (this.isFullBlockField()) {
      const bBox = this.getScaledBBox();

      // Override border radius.
      borderRadius = (bBox.bottom - bBox.top) / 2 + 'px';
      // Pull stroke colour from the existing shadow block
      const strokeColour = block.getParent()
        ? (block.getParent()).getColourTertiary()
        : (this.sourceBlock_).getColourTertiary();
      htmlInput.style.border = 1 * scale + 'px solid ' + strokeColour;
      div.style.borderRadius = borderRadius;
      div.style.transition = 'box-shadow 0.25s ease 0s';
      if (this.getConstants().FIELD_TEXTINPUT_BOX_SHADOW) {
        div.style.boxShadow =
          'rgba(255, 255, 255, 0.3) 0 0 0 ' + 4 * scale + 'px';
      }
    }
    htmlInput.style.borderRadius = borderRadius;

    div.appendChild(htmlInput);

    htmlInput.value = htmlInput.defaultValue = this.getEditorText_(this.value_);
    htmlInput.setAttribute('data-untyped-default-value', String(this.value_));

    this.resizeEditor_();

    this.bindInputEvents_(htmlInput);

    return htmlInput;
      }

    /* eslint-disable @typescript-eslint/naming-convention */
    /**
     * Show the inline free-text editor on top of the text along with the slider
     * editor.
     *
     * @param e Optional mouse event that triggered the field to
     *     open, or undefined if triggered programmatically.
     * @param quietInput Quiet input (prevent focusing on the editor).
     */

    showEditor_(e, quietInput) {        
        super.showEditor_(e, true);
        this.initMultiPicker_()       
    }

    widgetDispose_() {
      this.onPickerSave_();
      Blockly.WidgetDiv.hide(); // Remove focus so the selector can be used immediately after closing without manually chaging focus
      $(this.htmlInput_).selectpicker("destroy")
      $(this.htmlInput_).remove();
      super.widgetDispose_();
    }

    initMultiPicker_() {
        if (!this.htmlInput_) return;

        let parsed_value = JSON.parse(this.value_);
        
      // Include options in the menu
      for (let idx = 0; idx < this.menu_options.length; idx++) {
        let opt = this.menu_options[idx];
        let option = document.createElement("option");
        option.value = opt[0];
        option.textContent = opt[1];        
        this.htmlInput_.appendChild(option)
      }
        
        this.htmlInput_.setAttribute("multiple", "");
        this.htmlInput_.setAttribute("data-live-search", "true");
        this.htmlInput_.setAttribute("data-actions-box", "true");

        $(this.htmlInput_).selectpicker();
        $(this.htmlInput_).selectpicker("val", parsed_value);
        $(this.htmlInput_).selectpicker("hide");

        setTimeout(() => {
            // Wait until we are out of this stack to add listeners and hide elements. This "ensures" that the element is present in the DOM
            // We imitate the behavior of the selector by hiding the button
            $(this.htmlInput_).next("button").css({display:"none"}); // Hide the button selector so only the selection menu is visible
            $(this.htmlInput_).selectpicker("show"); // Start with it open
            $(this.htmlInput_).selectpicker("toggle"); // Start with it open
            // Add listener to dispose the element so we destroy it on menu hide and store the value
            $(this.htmlInput_).on("hide.bs.select", ()=> {
              this.widgetDispose_(); // Otherwise the div stays in editing mode and directly reclicking on the select does not work.
            });
        }, 0);



    }

    /**
     * Sets the text to match the slider's position.
     */
    onPickerSave_() {
        /* Refine by guessing which field is being updated */
        this.value_ = JSON.stringify($(this.htmlInput_).selectpicker("val"));
    }
    
    getText_() {
      if(this.value_ === '[]') return("0 items selected");
      return(String(JSON.parse(this.value_)));
    }
}
Blockly.fieldRegistry.register('multi_picker', multiPickerField);

//# sourceMappingURL=field_slider.js.map 
