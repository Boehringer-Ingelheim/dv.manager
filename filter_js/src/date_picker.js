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
        // Always quiet the input for the super constructor, as we don't want to
        // focus on the text field, and we don't want to display the modal
        // editor on mobile devices.
        // super.showEditor_(e, true); // We can remove the editor as we only have a single place to edit
        // Build the DOM.
        const editor = this.dropdownCreate_();
        Blockly.DropDownDiv.getContentDiv().appendChild(editor);

        const sourceBlock = this.getSourceBlock();
        if (sourceBlock instanceof Blockly.BlockSvg) {
            const primary = sourceBlock.getColour() || '';
            const tertiary = sourceBlock.getColourTertiary() || '';
            Blockly.DropDownDiv.setColour(primary, tertiary);
        }
        Blockly.DropDownDiv.showPositionedByField(this, this.dropdownDispose_.bind(this));

        // Wait until we are outside of the stack for initialize
        setTimeout(() => {
            this.initDatePicker_();
        }, 0);

        // Focus on the slider field, unless quietInput is passed.
        if (!quietInput) {
            editor.firstChild.focus({
                preventScroll: true,
            });
        }
    }
    /**
     * Updates the slider when the field rerenders.
     */
    render_() {
        super.render_();
        this.updateSlider_();
    }
    /**
     * Creates the slider editor and add event listeners.
     *
     * @returns The newly created slider.
     */
    dropdownCreate_() {
        const wrapper = document.createElement('div');
        wrapper.classList.add('datePickerFieldContainer');
        // wrapper.style.height = "100px";        
        wrapper.style.display = "flex";
        wrapper.style["align-items"] = "baseline";
        const internal = document.createElement('div');
        internal.classList.add("input-daterange", "input-group", "input-group-sm");
        const from_input = document.createElement("input");
        const to_input = document.createElement("input");

        const separator = document.createElement("span");
        separator.classList.add("input-group-addon", "input-group-prepend", "input-group-append");
        const internal_sep = document.createElement("span");
        internal_sep.classList.add("input-group-text");
        internal_sep.innerText = "to";
        separator.appendChild(internal_sep)

        internal.append(from_input, separator, to_input);
        
        wrapper.appendChild(internal);

        this.fromInput = from_input;
        this.toInput = to_input;
        return wrapper;

    }

    initDatePicker_() {
        if (!this.fromInput || !this.toInput) return;

        $(this.fromInput).bsDatepicker({
            format: 'yyyy-mm-dd',  // Date format
            autoclose: true,       // Auto close after selection
            startDate: this.min_, // Minimum date
            endDate: this.max_,   // Maximum date
        });
        $(this.fromInput).bsDatepicker("setDate", this.getSourceBlock().getFieldValue("min"))        
        $(this.fromInput).on("changeDate", () => {
            let data = ({ from: $(this.fromInput).val(), to: $(this.toInput).val() });
            this.onPickerChange_(data);
        });
        /* We force cursor to disappear from the text box, otherwise it admits editing when the picker is hidden.
         This editing is not properly captured. User is forced to reselect and therefore picker is shown */
        $(this.fromInput).on("hide", () => {
            $(this.fromInput).blur();
        });
        


        $(this.toInput).bsDatepicker({
            format: 'yyyy-mm-dd',  // Date format
            autoclose: true,       // Auto close after selection
            startDate: this.min_, // Minimum date
            endDate: this.max_,   // Maximum date
        });
        $(this.toInput).bsDatepicker("setDate", this.getSourceBlock().getFieldValue("max"));
        $(this.toInput).on("changeDate", () => {
            let data = ({ from: $(this.fromInput).val(), to: $(this.toInput).val() });
            this.onPickerChange_(data);
        });

        /* We force cursor to disappear from the text box, otherwise it admits editing when the picker is hidden.
         This editing is not properly captured. User is forced to reselect and therefore picker is shown */
         $(this.toInput).on("hide", () => {
            $(this.toInput).blur();
        });

    }

    /**
     * Disposes of events belonging to the slider editor.
     */
    dropdownDispose_() {
        for (const event of this.boundEvents) {
            Blockly.browserEvents.unbind(event);
        }
        this.boundEvents.length = 0;
        $(this.fromInput).bsDatepicker("destroy"); // Good practice and better and otherwise it is left floating
        $(this.toInput).bsDatepicker("destroy"); // Good practice and better and otherwise it is left floating
        this.fromInput = null;
        this.toInput = null;
    }
    /**
     * Sets the text to match the slider's position.
     */
    onPickerChange_(data) {
        /* Refine by guessing which field is being updated */
        this.getSourceBlock().getField("min").setEditorValue_(data.from, false);
        this.getSourceBlock().getField("max").setEditorValue_(data.to, false);
        this.getSourceBlock().setFieldValue(data.from, "min");
        this.getSourceBlock().setFieldValue(data.to, "max");
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
Blockly.fieldRegistry.register('date_picker', datePickerField);

//# sourceMappingURL=field_slider.js.map
