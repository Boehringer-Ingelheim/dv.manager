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
export class datePickerField extends Blockly.FieldTextInput {
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

    CURSOR = 'default';

    constructor(value, min, max, validator, config) {
        super(value, validator, config);
        /**
         * Array holding info needed to unbind events.
         * Used for disposing.
         * Ex: [[node, name, func], [node, name, func]].
         */
        this.boundEvents = [];
        /**
         * The HTML range input element.
         */
        this.min_ = min;
        this.max_ = max;
        this.fromInput = null;
        this.toInput = null;
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
        super.showEditor_(e, true);
        this.initDatePicker_();
    }
    
    initDatePicker_() {
        if (!this.htmlInput_) return;

        $(this.htmlInput_).bsDatepicker({
            format: 'yyyy-mm-dd',  // Date format
            autoclose: true,       // Auto close after selection
            startDate: this.min_, // Minimum date
            endDate: this.max_,   // Maximum date
        });
        $(this.htmlInput_).bsDatepicker("setDate", this.value_)        
        
        setTimeout(() => {   
            $(this.htmlInput_).css({visibility:"hidden"});                     
            $(this.htmlInput_).bsDatepicker("show"); // Start with it open            
            $(this.htmlInput_).on("changeDate", () => {            
                this.onDatePickerSave_();
            });
            $(this.htmlInput_).on("hide", () => {            
                this.widgetDispose_();
            });
        }, 0)

        /* We force cursor to disappear from the text box, otherwise it admits editing when the picker is hidden.
         This editing is not properly captured. User is forced to reselect and therefore picker is shown */
        $(this.fromInput).on("hide", () => {
            $(this.fromInput).blur();
        });
    }

    widgetDispose_() {        
        Blockly.WidgetDiv.hide(); // Remove focus so the selector can be used immediately after closing without manually chaging focus
        $(this.htmlInput_).bsDatepicker("destroy");
        $(this.htmlInput_).remove();
        super.widgetDispose_();
      }

    /**
     * Sets the text to match the slider's position.
     */
    onDatePickerSave_(data) {        
        this.value_ =  $(this.htmlInput_).val();
        this.widgetDispose_();
    }
}
Blockly.fieldRegistry.register('date_picker', datePickerField);

//# sourceMappingURL=field_slider.js.map
