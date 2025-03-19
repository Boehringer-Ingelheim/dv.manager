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
export class rangeSliderField extends Blockly.FieldNumber {
    /**
     * Class for an number slider field.
     *
     * @param value The initial value of the field. Should
     *    cast to a number. Defaults to 0.
     * @param min Minimum value.
     * @param max Maximum value.
     * @param precision Precision for value.
     * @param validator A function that is called to validate
     *    changes to the field's value. Takes in a number & returns a validated
     *    number, or null to abort the change.
     * @param config A map of options used to configure the field.
     *    See the [field creation documentation]{@link
     * https://developers.google.com/blockly/guides/create-custom-blocks/fields/built-in-fields/number#creation}
     *    for a list of properties this parameter supports.
     */
    constructor(value, min, max, precision, validator, config) {
        super(value, min, max, precision, validator, config);
        /**
         * Array holding info needed to unbind events.
         * Used for disposing.
         * Ex: [[node, name, func], [node, name, func]].
         */
        this.boundEvents = [];
        /**
         * The HTML range input element.
         */
        this.sliderInput = null;
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
        // Always quiet the input for the super constructor, as we don't want to
        // focus on the text field, and we don't want to display the modal
        // editor on mobile devices.
        super.showEditor_(e, true);
        // Build the DOM.
        const editor = this.dropdownCreate_();
        Blockly.DropDownDiv.getContentDiv().appendChild(editor);
        
        const sourceBlock = this.getSourceBlock();
        if (sourceBlock instanceof Blockly.BlockSvg) {
            const primary = "white";
            const tertiary = "gray";
            Blockly.DropDownDiv.setColour(primary, tertiary);
        }
        Blockly.DropDownDiv.showPositionedByBlock(this, sourceBlock, this.dropdownDispose_.bind(this));    

        // Wait until we are outside of the stack for initialize
        setTimeout(() => {
          this.initIonSlider_();
        }, 0);
        
        // Focus on the slider field, unless quietInput is passed.
        if (!quietInput) {
            editor.firstChild.focus({
                preventScroll: true,
            });
        }
    }
    
    /**
     * Creates the slider editor and add event listeners.
     *
     * @returns The newly created slider.
     */
    dropdownCreate_() {
        const wrapper = document.createElement('div');
        wrapper.className = 'fieldSliderContainer';
        wrapper.style.height = "auto";
        wrapper.style.width = "400px";
        const internal = document.createElement('div');
        internal.style.backgroundColor = "white";
        const sliderInput = document.createElement('input');        
        wrapper.appendChild(internal);
        internal.appendChild(sliderInput);
        this.sliderInput = sliderInput;        
        return wrapper;
    }

    initIonSlider_() {
      if (!this.sliderInput) return;

      $(this.sliderInput).ionRangeSlider({
          min: this.min_,          
          max: this.max_,
          type: "double",
          step: this.precision_,
          from: this.getSourceBlock().getFieldValue("min"),
          to: this.getSourceBlock().getFieldValue("max"),
          skin: "shiny",
          grid: "true",
          onChange: (data) => this.onSliderChange_(data),          
      });

      this.ionSliderInstance = $(this.sliderInput).data("ionRangeSlider");
  }

    /**
     * Disposes of events belonging to the slider editor.
     */
    dropdownDispose_() {
        for (const event of this.boundEvents) {
            Blockly.browserEvents.unbind(event);
        }
        this.boundEvents.length = 0;
        this.ionSliderInstance.destroy();
        this.ionSliderInstance = null;
        this.sliderInput = null;
    }
    /**
     * Sets the text to match the slider's position.
     */
    onSliderChange_(data) {
      /* Refine by guessing which field is being updated */
      this.getSourceBlock().getField("min").setEditorValue_(Number(data.from), false);
      this.getSourceBlock().getField("max").setEditorValue_(Number(data.to), false);
      this.getSourceBlock().setFieldValue(Number(data.from), "min");
      this.getSourceBlock().setFieldValue(Number(data.to), "max");
      this.resizeEditor_();
    }
    /**
     * Updates the slider when the field rerenders.
     */
    updateSlider_() {
        if (!this.sliderInput) {
            return;
        }
        this.sliderInput.setAttribute('value', `${this.getValue()}`);
    }
}
Blockly.fieldRegistry.register('field_slider', rangeSliderField);
 
//# sourceMappingURL=field_slider.js.map