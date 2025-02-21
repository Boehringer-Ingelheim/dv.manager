/**
 * @license
 * Copyright Copyright 2024 Boehringer-Ingelheim Pharma GmbH & Co.KG
 * SPDX-License-Identifier: Apache-2.0
 * 
 * See repository root for full license
 * 
 */


import * as Blockly from 'blockly';
import {rangeSliderField} from './range_slider.js';
import {datePickerField} from './date_picker.js';  

/* TODO: Add validation rules so bounds of the ranges cannot be inverted, min_range > max_range. At the moment we only 
do not confuse with them being within the bounds of the column this is already controlled. */

/* TODO: 

- Multiselector is a lower priority as it can be achieved with some and or not operations. Unlikely we will see
a 200 participant selection, if it happens it will be due to a filter state being loaded.

*/

/*TODO: Save and load of the state should be done on the code structure.
filter_json -> Add missing properties to imitate blockly's -> load through blockly machinery

Advantage a single structure is used. This would also help when we use different frontends to blockly
*/

/*TODO: Consider statements inside of puzzle pieces
*/

/*TODO: Include Subject filter and Dataset filter as singlenton non deletebale pieces
*/

/*TODO: Split in two workspaces one for the subject filters and another for the table filters
*/

/*TODO: Include buttons to save filter states and recover them later with names, etc.
*/

/*TODO: Let the user know when the filter is a non-finished state when pressing the apply filter button.
*/

/*TODO: Create an alternative UI that matches the current dv.filter while using the same backend
*/

/*TODO: Terms dataset and table are used interchangeably in the code that would bring some headaches in the future
Coheren usage of the terms is recommended. Table will be the recommended word*/

//DONE

/*DONE: Check pieces from a table are only used under the correc table
1. ~~Do this in the get code section -> not required as we control it remotely in the server and we do not allow the connection
2. When a piece is moved under a table piece, done
Disconnect piece immdiately
*/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const filterBlockly = (() => {

  // let logger = function(x) {console.log(x)};
  let logger = function (x) { return; }

  const C = {
    TYPE: {
      FILTER_COMB_OPERATION: 'c_filter_comb_operation',
      FILTER_NOT_OPERATION: 'c_filter_not_operation',
      DATASETS_FILTER: 'datasets_filter',
      SUBJECT_FILTER: 'subject_filter'
    }
  }
  /*
    Generates the code for the datasets_filters piece
  */
  const dataset_filter_generator = function (block, generator) {
    const children_code = generator.valueToCode(block, "children", 0);
    const code = '{"name" : "' + block.table_name + '", "kind": "dataset", "children": [' + children_code + ']}';
    return (code);
  }

  /*
    Generates the code for the subject_filters piece
  */
  const subject_filter_generator = function (block, generator) {
    const code = generator.valueToCode(block, "content", 0);
    return ('{"subject_filter": {"children": [' + code + '] }}');
  }

  const filter_operation_generator = function (block, generator) {
    let children_code = "";
    let current_inputs = block.inputList.map(x => x.name);
    for (input of current_inputs) {
      const code = generator.valueToCode(block, input, 0);
      if (code !== '') {
        children_code = children_code + code + ", ";
      }
    }
    children_code = children_code.slice(0, -2);

    const kind = 'filter_operation';
    const operation = block.getFieldValue('operation');
    const code = '{' +
      '"kind": "' + kind + '"' +
      ', "operation": "' + operation + '"' +
      ', "children": [' + children_code + ']}';
    return ([code, null])
  }

  const filter_generator_range = function (block, generator) {
    const dataset = block.table_name;
    const field = block.field_name;
    const kind = 'filter';
    const operation = 'select_range';
    const min = block.getFieldValue('min');
    const max = block.getFieldValue('max');
    const include_NA = block.getFieldValue('include_NA')==="FALSE"?false:true;

    const code = '{' +
      '"kind": "' + kind + '"' +
      ', "dataset": "' + dataset + '"' +
      ', "operation": "' + operation + '"' +
      ', "field": "' + field + '"' +
      ', "min": ' + min +
      ', "max": ' + max +
      ', "include_NA": ' + include_NA +
      '}';

    return ([code, null]);
  }

  const filter_generator_date_range = function (block, generator) {
    const dataset = block.table_name;
    const field = block.field_name;
    const kind = 'filter';
    const operation = 'select_date';
    const min = block.getFieldValue('min');
    const max = block.getFieldValue('max');
    const include_NA = block.getFieldValue('include_NA')==="FALSE"?false:true;

    const code = '{' +
      '"kind": "' + kind + '"' +
      ', "dataset": "' + dataset + '"' +
      ', "operation": "' + operation + '"' +
      ', "field": "' + field + '"' +
      ', "min": "' + min + '"' +
      ', "max": "' + max + '"' +
      ', "include_NA": ' + include_NA +
      '}';

    return ([code, null]);
  }

  const filter_generator_subset = function (block, generator) {
    const dataset = block.table_name;
    const field = block.field_name;
    const kind = 'filter';
    const operation = 'select_subset';
    const values = Array.isArray(block.getFieldValue('value')) ? block.getFieldValue('value') : [block.getFieldValue('value')];
    const include_NA = block.getFieldValue('include_NA')==="FALSE"?false:true;

    const joined_values = "[" + values.map(function (x) { return ('"' + x + '", ') })
      .reduce(function (x, y) { return (x + y) })
      .slice(0, -2) +
      "]";

    const code = '{' +
      '"kind": "' + kind + '"' +
      ', "dataset": "' + dataset + '"' +
      ', "operation": "' + operation + '"' +
      ', "field": "' + field + '"' +
      ', "values":' + joined_values +
      ', "include_NA": ' + include_NA +
      '}';

    return ([code, null]);
  }

  const get_code = function (ws, gen) {

    const start = new Date();

    let blockly_state = Blockly.serialization.workspaces.save(ws);
    let blocks = blockly_state["blocks"]["blocks"];
    let topBlocks = blocks.slice(); // shallow copy
    blockly_state["blocks"]["blocks"].length = 0;

    var headless = new Blockly.Workspace();

    let filters = {
      datasets_filter: { children: [] },
      subject_filter: { children: [] }
    };

    for (let i = 0; i < topBlocks.length; i++) {
      let current_block = topBlocks[i];
      blockly_state["blocks"]["blocks"].push(current_block);
      Blockly.serialization.workspaces.load(blockly_state, headless);
      const current_filter = JSON.parse(gen.workspaceToCode(headless));
      if (current_filter.kind === "dataset") {
        filters.datasets_filter.children.push(current_filter);
      } else {
        filters.subject_filter = (current_filter.subject_filter);
      }
      blockly_state["blocks"]["blocks"].length = 0;
    }

    blockly_state["blocks"]["blocks"] = topBlocks; // Restore blocks for saving

    const res_state = {
      state: blockly_state,
      filters: filters
    }

    let stringified_res = JSON.stringify(res_state)

    const end = new Date();
    logger("Get code: " + (end.getTime() - start.getTime()) + " ms");

    return (stringified_res)
  }

  let json_generator = new Blockly.Generator("JSON");

  json_generator.scrub_ = function (block, code, thisOnly) {
    const nextBlock =
      block.nextConnection && block.nextConnection.targetBlock();
    if (nextBlock && !thisOnly) {
      return code + ",\n" + json_generator.blockToCode(nextBlock);
    }
    return code;
  };

  json_generator.forBlock[C.TYPE.SUBJECT_FILTER] = subject_filter_generator;

  let toolbox = {
    kind: 'categoryToolbox',
    contents: [
      {
        kind: 'category',
        name: 'Filter Types',
        contents: [
          {
            kind: 'block',
            type: C.TYPE.SUBJECT_FILTER
          },
        ]
      },
      {
        kind: 'category',
        name: 'Operations',
        contents: [
          {
            kind: 'block',
            type: C.TYPE.FILTER_COMB_OPERATION
          },
          {
            kind: 'block',
            type: C.TYPE.FILTER_NOT_OPERATION
          }
        ]
      },
    ]
  };

  json_generator.forBlock[C.TYPE.FILTER_COMB_OPERATION] = filter_operation_generator;
  json_generator.forBlock[C.TYPE.FILTER_NOT_OPERATION] = filter_operation_generator;

  const init = function (id) {
    const container_div = document.getElementById(id);
    const script_tag = container_div.querySelector("script[type='application/json']");

    let json_data;

    try {
      json_data = JSON.parse(script_tag.textContent.trim());
    } catch (error) {
      console.error('Error parsing JSON:', error);
    }

    let filter_state = json_data.state;
    let filter_data = json_data.data;

    let selected_dataset_name = "D1";

    let selected_tables;

    for (let dataset of filter_data["datasets"]) {
      let name = dataset["name"];
      if (name === selected_dataset_name) {
        selected_tables = dataset["tables"];
        break;
      }
    }

    if (selected_tables === null) {
      throw new Error('Selected dataset not found');
    }

    let populate_inputs = function (block, input_names) {
      // Secondary effects on block
      for (let idx = 0; idx < input_names.length; idx++) {
        block.appendValueInput(input_names[idx])
      }
    };

    let append_value_input = function (block) {
      let input_name = "contents_" + Blockly.utils.idGenerator.genUid();
      block.appendValueInput(input_name);
    }

    let remove_value_inputs = function (block, input_names_for_removal) {
      // Secondary effects on block      
      input_names_for_removal.forEach((element) => {
        if (block.inputList.map(x => x.name).includes(element)) {
          block.removeInput(element)
        } else {
          logger("Skipping removal of " + element + "not found");
        }
      });
    }

    let remove_empty_inputs = function (block) {
      // Secondary effects on block
      let empty_input_names = block.inputList
        .filter(function (item) { return item.name.startsWith("contents_") && item.connection.targetConnection === null })
        .map(x => x.name);
      remove_value_inputs(block, empty_input_names)
    };

    Blockly.Blocks[C.TYPE.FILTER_COMB_OPERATION] = {
      init: function () {
        this.appendDummyInput("header")
          .appendField(new Blockly.FieldDropdown(
            [['and', 'and'], ['or', 'or'], ['not', 'not']]
          ), "operation");
        this.setOutput(true);
        this.setColour(255);
      },// This is called during serialization
      saveExtraState: function () {
        let saved_inputs = this.inputList.map(x => x.name).filter(x => x.startsWith("contents_"))
        logger(saved_inputs);
        return { data: saved_inputs };
      },

      loadExtraState: function (state) {
        if (state && state.data !== undefined && state.data.length > 0) {
          logger("Loading with state")
          populate_inputs(this, state.data);
          logger(this.inputList.map(x => x.name));
        } else {
          logger("Loading with no state");
          append_value_input(this);
        }
      }
    };

    Blockly.Blocks[C.TYPE.FILTER_NOT_OPERATION] = {
      init: function () {
        this.appendDummyInput("header")
          .appendField(new Blockly.FieldDropdown(
            [['not', 'not']]
          ), "operation")
        this.appendValueInput("contents_fix");
        this.setOutput(true);
        this.setColour(255);
      }
    };

    Blockly.Blocks[C.TYPE.SUBJECT_FILTER] = {
      init: function () {
        this.appendDummyInput('label')
          .appendField('Subject Filter');
        this.appendValueInput("content");
        this.setColour(160);
      }
    };

    let current_color = 0;

    for (let table of selected_tables) {
      current_color = current_color + 30;
      const table_color = current_color;
      const table_name = table["name"];
      const table_type = "t_" + table_name;
      const table_category = {
        kind: 'category',
        name: table_name,
        contents: []
      };

      Blockly.Blocks[table_type] = {
        init: function () {
          this.appendDummyInput()
            .appendField("Dataset Filter: ")
            .appendField(table_name, "table_name");
          this.appendValueInput("children");
          this.setColour(table_color);
          this.table_name = table_name;
          this.is_top_table = true;
        }
      };

      toolbox.contents[0].contents.push(
        {
          kind: 'block',
          type: table_type
        }
      );

      json_generator.forBlock[table_type] = dataset_filter_generator;

      logger(toolbox);


      for (let field of table["fields"]) {
        const field_name = field["name"];
        const field_type = "t_" + table_name + "_f_" + field_name;
        const field_label = `(${table_name}) ${field_name}`;
        const kind = field["kind"];
        const block_color = table_color; // Otherwise it takes the value of table_color from the outer closure
        const field_na_label = "NA("+field["NA_count"]+"):"

        if (kind === "categorical") {
          const values = field["values_count"];
          let dd_options = [];
          for (let v of values) {
            dd_options.push([v.value, v.value])
          }

          if (dd_options.length == 0) dd_options = [['_EMPTY_VEC_', '_EMPTY_VEC_']]

          Blockly.Blocks[field_type] = {
            init: function () {
              this.appendEndRowInput()
                .appendField(field_label)
                .appendField(new Blockly.FieldDropdown(dd_options),"value")
                .appendField(field_na_label)
                .appendField(new Blockly.FieldCheckbox(false), 'include_NA');       
              this.setOutput(true);
              this.setColour(block_color);
              this.field_name = field_name;
              this.table_name = table_name;
            }
          }

          json_generator.forBlock[field_type] = filter_generator_subset;

        } else if (kind === "numerical") {
          const min = field["min"];
          const max = field["max"];

          Blockly.Blocks[field_type] = {
            init: function () {
              this.appendDummyInput()
                .appendField(field_label)
                .appendField("Min")
                .appendField(new rangeSliderField(min, min, max), "min")
                .appendField("Max")
                .appendField(new rangeSliderField(max, min, max), "max")
                .appendField(new Blockly.FieldLabel(field_na_label, "na_label"))
                .appendField(new Blockly.FieldCheckbox(false), 'include_NA');
                                    
              this.setOutput(true);
              this.setColour(block_color);
              this.field_name = field_name;
              this.table_name = table_name;
            }
          }
          json_generator.forBlock[field_type] = filter_generator_range;
        } else if (kind === "date") {
          const min = field["min"];
          const max = field["max"];

          Blockly.Blocks[field_type] = {
            init: function () {
              this.appendEndRowInput()
                .appendField(field_label)
                .appendField("Start: ")
                .appendField(new datePickerField(min, min, max), "min")
                .appendField("End: ")
                .appendField(new datePickerField(max, min, max), "max")
                .appendField(field_na_label)
                .appendField(new Blockly.FieldCheckbox(false), 'include_NA');
              this.setOutput(true);
              this.setColour(block_color);
              this.field_name = field_name;
              this.table_name = table_name;
            }
          }
          json_generator.forBlock[field_type] = filter_generator_date_range;
        } else {
          console.error("Unknown field kind: " + kind)
          continue;
          // throw new Error("Unknown field kind: " + kind);
        }

        let field_block = {
          kind: 'block',
          type: field_type
        };
        table_category.contents.push(field_block);
      }
      toolbox.contents.push(table_category);
    }

    function onChange(event) {
      if (event.type !== Blockly.Events.BLOCK_MOVE) return;
      let current_workspace = Blockly.Workspace.getById(event.workspaceId);
      let new_parent_block = current_workspace.getBlockById(event.newParentId);
      let old_parent_block = current_workspace.getBlockById(event.oldParentId);
      let current_block = current_workspace.getBlockById(event.blockId);

      // Code replacement is broken in set and filter_operations by the code below
      // In replacement first the piece is disconnected and then reconnected
      // When disconnected the input in the piece is removed, then an attempt to connect is done
      // But the attempt is not possible because the input is no longer there
      // An attempt to fix it by placing back the input was done, but it was not possible
      // if(!new_parent_block.inputList.map(x=>x.name).includes(input_for_append)) new_parent_block.appendInput(input_for_append);
      // As `Uncaught TypeError TypeError: d.isVisible is not a function` appeared. The origin is unclear

      if (event.reason.includes("connect")) {

        // Check the connection is legal

        let root_block = current_block.getParent();

        while (root_block.getParent() !== null) {
          root_block = root_block.getParent();
        }

        // Only check if the top parent is a table

        if (root_block.is_top_table) {
          const table_name = root_block.table_name;
          let stack = [];
          stack.push(root_block);
          while (stack.length) {
            let b = stack.pop();
            if (b.table_name && b.table_name !== table_name) {
              logger("Incorrect piece in stack");
              current_block.unplug();
              return;
            }
            stack.push(...b.getChildren());
          }
        }

        if (new_parent_block && new_parent_block.type === C.TYPE.FILTER_COMB_OPERATION) {
          remove_empty_inputs(new_parent_block);
          append_value_input(new_parent_block);
        }
      } else if (event.reason.includes("disconnect")) {
        if (old_parent_block && old_parent_block.type === C.TYPE.FILTER_COMB_OPERATION) {
          // Remove the input that has been disconnected
          const input_for_removal = event.oldInputName;
          remove_value_inputs(old_parent_block, [input_for_removal]);
        }

      } else {
        return;
      }
    }


    let options = {}
    options.toolbox = toolbox;
    let ws = Blockly.inject(container_div, options);

    ws.MAX_UNDO = 0; //Disconnect undo because of listeners
    // When removing elements using JS the undo is messed up
    // For example when removing an element from an operation the id of the input changes
    // and redo cannot happen because we cannot reconnect as the previous input id is
    // stored in the undo stack. No control is provided over the undo stack.

    ws.addChangeListener(onChange);

    if (filter_state) {
      Blockly.serialization.workspaces.load(filter_state.state, ws)
    }

    let res = {
      workspace: ws,
      generator: json_generator
    }
    return (res)
  }

  // Return public API
  return ({
    init: init,
    get_code: get_code,
    chaff: Blockly.hideChaff
  });

})();

const init = filterBlockly.init;
const get_code = filterBlockly.get_code;
const chaff = filterBlockly.chaff;

export {init, get_code, chaff}

