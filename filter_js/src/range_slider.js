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
     * @param value The initial value of the field.
     * @param min Minimum value.
     * @param max Maximum value.
     * @param precision Precision for value.     
     */
    constructor(value, min, max, precision) {
        super(value, min, max, precision);
        /**
         * The HTML range input element.
         */
        this.sliderInput = null;
    }

    /* eslint-disable @typescript-eslint/naming-convention */
    /**
     * Creates dropdown
     *
     * @param e Optional mouse event that triggered the field to
     *     open, or undefined if triggered programmatically.     
     */
    showEditor_(e, quietInput) {
        super.showEditor_(e, true);
        this.old_unclicked_value = this.name === "max" ? this.getSourceBlock().getFieldValue("min") : this.getSourceBlock().getFieldValue("max");        

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
        this.ionSliderInstance.destroy();
        this.ionSliderInstance = null;
        this.sliderInput = null;

        // Send extra event now
        // This block only sends an event automatically when the value of the clicked input changes
        // But it does not do it for the unclicked element. Therefore, we do it manually.

        const name_unclicked = this.name === "max" ? "min" : "max";
        const current_unclicked_value = this.getSourceBlock().getFieldValue(name_unclicked);
        if(this.old_unclicked_value !== current_unclicked_value) {
            Blockly.Events.fire(new Blockly.Events.BlockChange(
                this.getSourceBlock(), 'field', this.name_unclicked, this.old_unclicked_value, current_unclicked_value
              ));                        
        }
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