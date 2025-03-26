/**
 * 
 * Based on the number slider field by kozbial@google.com
 * 
 * https://www.npmjs.com/package/@blockly/field-slider
 * 
 */

import * as Blockly from 'blockly/core';

export class datePickerField extends Blockly.FieldTextInput {
    /**
     * Class for an date picker field using bs-datepicker.
     *
     * @param value The initial value of the field.
     * @param min Minimum value.
     * @param max Maximum value.     
    */

    CURSOR = 'default';

    constructor(value, min, max) {
        super(value);
        this.min_ = min;
        this.max_ = max;
        this.fromInput = null;
        this.toInput = null;
    }

    /**
     * Show the inline date-picker
     *
     * @param e Optional mouse event that triggered the field to
     *     open, or undefined if triggered programmatically.     
     */
    showEditor_(e, _) {
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
            $(this.htmlInput_).css({ visibility: "hidden" });
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
     * Sets the text to match the date picker selection.
     */
    onDatePickerSave_() {
        this.value_ = $(this.htmlInput_).val();
        this.widgetDispose_();
    }
}
Blockly.fieldRegistry.register('date_picker', datePickerField);
