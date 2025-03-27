/**
 * @license
 * Copyright Copyright 2024 Boehringer-Ingelheim Pharma GmbH & Co.KG
 * SPDX-License-Identifier: Apache-2.0
 * 
 * See repository root for full license
 * 
 */


/* Func specs: 

- When a column in the preset filter is not present in the current dataset
  - Send error message, remove the whole filter
- When a value in a multiselector is not present
  - Remove the value from the selection
  - Ranges are covered by blockly validators    
- When we switch datasets
  - Remove the filters, apply preset one always
- Bookmarked only applied once
- Client should strive to send only applicable filter states. But server must always check that those are correct.


*/

import * as Blockly from 'blockly';
import { rangeSliderField } from './range_slider.js'; ``
import { datePickerField } from './date_picker.js';
import { multiPickerField } from './multi_picker.js';
import './toolbox-search'


/* TODO: Filter data structure generation is slow, it is generated everytime the UI starts, it could be moved to a 
step previous to the UI, to avoid running it everytime we start the application. Memoize the operation?

/* TODO: Control for incorrect filter definitions that should never be sent to the server. e.g. ands with not children

Several dataset or subjet filters blocks

*/

/* TODO: Define behavior of unplugged blocks */

/* TODO: Should a warning appear when a preset filters uses an out of range value. They are corrected by blockly but there is no warning*/

/* TODO: name consitency */

/* TODO: Clean, clean, clean!!!!

/* TODO: # When attaching the dependencies on my own an error occurs when using multiple
    # When including shinyWidget picker_input itself the error disappears, this should be explored
    */

/* TODO: Add validation rules so bounds of the ranges cannot be inverted, min_range > max_range. At the moment we only 
do not confuse with them being within the bounds of the column this is already controlled. */

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

  const get_block_filter_type = function (dataset_name, variable_name) {
    return ("d_" + dataset_name + "_v_" + variable_name);
  }

  const get_block_dataset_type = function (dataset_name, variable_name) {
    return ("d_" + dataset_name);
  }

  const get_random_input_id = function () {
    return ("contents_" + Blockly.utils.idGenerator.genUid());
  }

  const filter_to_state = function (filter, dataset_list) {
    
    let state = {
      blocks: {
        languageVersion: 0,
        blocks: []
      }
    };

    let log = [];

    if (filter){

    let process_filter = function (filter) {

      let res = {};
      let stack = [];
      stack.push([res, filter]);

      while (stack.length) {

        let current_block, current_filter;
        [current_block, current_filter] = stack.pop();

        logger("Processing");
        logger(current_block);
        logger(current_filter);

        if (current_filter.kind === "filter") {
          current_block.type = get_block_filter_type(current_filter.dataset, current_filter.variable);
          current_block.id = Blockly.utils.idGenerator.genUid();

          /* Check if filter is applicable
            Checks only if variables and datasets are available in a given dataset_list
          */

          let applicable = true;
          let dataset_names = dataset_list.map((x)=>x.name);
          if(dataset_names.includes(current_filter.dataset)) {
            let dataset_idx = dataset_list.map((x)=>x.name).indexOf(current_filter.dataset);
            let variable_names = dataset_list[dataset_idx].variables.map((x)=>x.name);            
            if(!variable_names.includes(current_filter.variable)) {
              applicable = false
            }
          } else {
            applicable = false;
          }

          if(!applicable) {
            res = null;
            log = "Preselected/bookmarked filter is not applicable to the current dataset";
            break;
          }

          if (current_filter.operation === "select_subset") {
            logger("as subset");
            logger(current_filter);
            let dataset_idx = dataset_list.map((x)=>x.name).indexOf(current_filter.dataset);
            let variable_names = dataset_list[dataset_idx].variables.map((x)=>x.name);
            let variable_idx = variable_names.indexOf(current_filter.variable);
            let variable_values = dataset_list[dataset_idx].variables[variable_idx].values_count.map((x) => x.value);
            let found = current_filter.values.filter((x) => variable_values.includes(x));
            let removed = current_filter.values.filter((x) => !variable_values.includes(x));

            if (removed.length>0) {
              log.push("Removed values: " + removed.join() + " from " + current_filter.dataset + " - " + current_filter.variable)              
            }
            current_block.fields = {
              value: JSON.stringify(found),
              include_NA: current_filter.include_NA
            };
          } else if (current_filter.operation === "select_range") {
            logger("as range");
            logger(current_filter);
            current_block.fields = {
              min: current_filter.min,
              max: current_filter.max,
              include_NA: current_filter.include_NA
            };
          } else if (current_filter.operation === "select_date") {
            logger("as date");
            logger(current_filter);
            current_block.fields = {
              min: current_filter.min,
              max: current_filter.max,
              include_NA: current_filter.include_NA
            };
          } else {
            throw new Error("Unkown operation: " + current_filter.operation)
          }

        } else if (current_filter.kind === "filter_operation") {

          if (current_filter.operation === "and" || current_filter.operation === "or") {
            current_block.type = C.TYPE.FILTER_COMB_OPERATION;
            current_block.id = Blockly.utils.idGenerator.genUid();
            current_block.extraState = {
              data: []
            };
            current_block.fields = { operation: current_filter.operation };
            current_block.inputs = {};
            for (let idx = 0; idx < current_filter.children.length; idx++) {
              let input_id = get_random_input_id();
              current_block.extraState.data.push(input_id);
              current_block.inputs[input_id] = { block: {} };
              stack.push([current_block.inputs[input_id].block, current_filter.children[idx]]);
            }
            current_block.extraState.data.push(get_random_input_id()); // One extra because we want a free input
          } else if (current_filter.operation === "not") {
            current_block.type = C.TYPE.FILTER_NOT_OPERATION;
            current_block.id = Blockly.utils.idGenerator.genUid();
            current_block.fields = { operation: current_filter.operation };
            current_block.inputs = {
              contents_fix: { block: {} }
            };
            stack.push([current_block.inputs.contents_fix.block, current_filter.children[0]]);
          } else {
            throw new Error("Unkown operation: " + current_filter.operation)
          }

        } else {
          throw new Error("Unknown kind: " + current_filter.kind)
        }

      }      
      return (res);
    }

    if (filter[C.TYPE.SUBJECT_FILTER] && filter[C.TYPE.SUBJECT_FILTER].children.length > 0) {
      let processed_filter = process_filter(filter[C.TYPE.SUBJECT_FILTER].children[0]);
      if (processed_filter !== null) {
        let subject_filter = {
          type: C.TYPE.SUBJECT_FILTER,
          id: Blockly.utils.idGenerator.genUid(),
          x: 0,
          y: 0,
          inputs: { content: { block: process_filter(filter[C.TYPE.SUBJECT_FILTER].children[0]) } }
        };
        state.blocks.blocks.push(subject_filter);        
      }      
    }

    if (filter.datasets_filter && filter.datasets_filter.children.length > 0) {
      for (let idx = 0; idx < filter.datasets_filter.children.length; idx++) {
        let curr_dataset_filter = filter.datasets_filter.children[idx];
        let processed_filter = process_filter(curr_dataset_filter.children[0]);
        if (processed_filter !== null) {
          let dataset_filter = {
            type: get_block_dataset_type(curr_dataset_filter.name),
            id: Blockly.utils.idGenerator.genUid(),
            x: 0,
            y: 0,
            inputs: { children: { block: process_filter(curr_dataset_filter.children[0]) } }
          };
          state.blocks.blocks.push(dataset_filter);
        }
      }
    }
  }
   
    return ([state, log]);
  }

  /*
    Generates the code for the datasets_filters piece
  */
  const dataset_filter_generator = function (block, generator) {
    const children_code = generator.valueToCode(block, "children", 0);
    const code = '{"name" : "' + block.dataset_name + '", "kind": "dataset", "children": [' + children_code + ']}';
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
    const dataset_name = block.dataset_name;
    const variable = block.variable_name;
    const kind = 'filter';
    const operation = 'select_range';
    const min = block.getFieldValue('min');
    const max = block.getFieldValue('max');
    const include_NA = block.getFieldValue('include_NA') === "FALSE" ? false : true;

    const code = '{' +
      '"kind": "' + kind + '"' +
      ', "dataset": "' + dataset_name + '"' +
      ', "operation": "' + operation + '"' +
      ', "variable": "' + variable + '"' +
      ', "min": ' + min +
      ', "max": ' + max +
      ', "include_NA": ' + include_NA +
      '}';

    return ([code, null]);
  }

  const filter_generator_date_range = function (block, generator) {
    const dataset_name = block.dataset_name;
    const variable = block.variable_name;
    const kind = 'filter';
    const operation = 'select_date';
    const min = block.getFieldValue('min');
    const max = block.getFieldValue('max');
    const include_NA = block.getFieldValue('include_NA') === "FALSE" ? false : true;

    const code = '{' +
      '"kind": "' + kind + '"' +
      ', "dataset": "' + dataset_name + '"' +
      ', "operation": "' + operation + '"' +
      ', "variable": "' + variable + '"' +
      ', "min": "' + min + '"' +
      ', "max": "' + max + '"' +
      ', "include_NA": ' + include_NA +
      '}';

    return ([code, null]);
  }

  const filter_generator_subset = function (block, generator) {
    const dataset_name = block.dataset_name;
    const variable = block.variable_name;
    const kind = 'filter';
    const operation = 'select_subset';
    const values = block.getFieldValue('value');
    const include_NA = block.getFieldValue('include_NA') === "FALSE" ? false : true;

    const code = '{' +
      '"kind": "' + kind + '"' +
      ', "dataset": "' + dataset_name + '"' +
      ', "operation": "' + operation + '"' +
      ', "variable": "' + variable + '"' +
      ', "values":' + values +
      ', "include_NA": ' + include_NA +
      '}';

    return ([code, null]);
  }

  const get_code = function ({workspace, generator, dataset_name}) {

    const start = new Date();

    let filters = {
      datasets_filter: { children: [] },
      subject_filter: { children: [] }
    };

    let blockly_state = Blockly.serialization.workspaces.save(workspace);

    if(blockly_state["blocks"] !== undefined){
      
      let blocks = blockly_state["blocks"]["blocks"];
      let topBlocks = blocks.slice(); // shallow copy
      blockly_state["blocks"]["blocks"].length = 0;
  
      let hl = new Blockly.Workspace();
  
      for (let i = 0; i < topBlocks.length; i++) {
        let current_block = topBlocks[i];
        blockly_state["blocks"]["blocks"].push(current_block);
        Blockly.serialization.workspaces.load(blockly_state, hl);
        const current_filter = JSON.parse(generator.workspaceToCode(hl));
        if (current_filter.kind === "dataset") {
          filters.datasets_filter.children.push(current_filter);
        } else {
          filters.subject_filter = (current_filter.subject_filter);
        }
        blockly_state["blocks"]["blocks"].length = 0;
      }
  
      blockly_state["blocks"]["blocks"] = topBlocks; // Restore blocks for saving

    }

    const res_state = {
      filters: filters,
      dataset_list_name: dataset_name
    };

    let stringified_res = JSON.stringify(res_state)

    const end = new Date();
    logger("Get code: " + (end.getTime() - start.getTime()) + " ms");

    return (stringified_res)
  }

  const init = function (id, dataset_name) {
    {
      /* Clear previous block definitions
      Otherwise block definitions are kept from one dataset to the other
      This can be a problem when:
      - App has two datasets A and B
      - The preset filter is using a column present in dataset B but not A
      - When the app starts selected dataset is A, an error indicating that restoring the state is not possible
        - At this stage Blockly.Blocks has type declarations for all the columns in dataset A
      - When we switch to dataset B restores occurs with no problem
        - At this stage Blockly.Blocks has type declarations for all the columns in dataset A AND for dataset B
      - When we switch back to dataset A, although the blocks from B are not in the toolbox, they can be instantiated
      programatically, when that piece is instantiated and when the code is generated an error occurs because the generator
      does not know how to treat that piece, as it was not defined for dataset A.

      The problem described above is not applicable anymore as we clean the state before attempting to restart, nonetheless
      we leave the description as the side effects of Blockly.Blocks must be taken into account.

      */
      const block_names = Object.keys(Blockly.Blocks);
      for (let idx = 0; idx < block_names.length; ++idx) {
        Blockly.Blocks[block_names[idx]] = null;        
      }

    }

    const container_div = document.getElementById(id);
    const script_tag = container_div.querySelector("script[type='application/json']");

    // Preface start


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
          'kind': 'search',
          'name': 'Search',
          'contents': [],
        },
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

    // Preface End

    let json_data;

    try {
      json_data = JSON.parse(script_tag.textContent.trim());
    } catch (error) {
      console.error('Error parsing JSON:', error);
    }

    let filter_data = json_data.data;

    let selected_dataset_name = dataset_name;

    let selected_datasets;

    for (let dataset of filter_data["dataset_lists"]) {
      let name = dataset["name"];
      if (name === selected_dataset_name) {
        selected_datasets = dataset["dataset_list"];
        break;
      }
    }

    if (selected_datasets === null) {
      throw new Error('Selected dataset not found');
    }

    let populate_inputs = function (block, input_names) {
      // Secondary effects on block
      for (let idx = 0; idx < input_names.length; idx++) {
        block.appendValueInput(input_names[idx])
      }
    };

    let append_value_input = function (block) {
      block.appendValueInput(get_random_input_id());
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
            [['and', 'and'], ['or', 'or']]
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

    for (let dataset of selected_datasets) {
      current_color = current_color + 30;
      const dataset_color = current_color;
      const dataset_name = dataset["name"];
      const dataset_type = get_block_dataset_type(dataset_name);
      const dataset_category = {
        kind: 'category',
        name: dataset_name,
        contents: [],
      };

      Blockly.Blocks[dataset_type] = {
        init: function () {
          this.appendDummyInput()
            .appendField("Dataset Filter: ")
            .appendField(dataset_name, "dataset_name");
          this.appendValueInput("children");
          this.setColour(dataset_color);
          this.dataset_name = dataset_name;
          this.is_top_dataset = true;
        }
      };

      toolbox.contents[1].contents.push(
        {
          kind: 'block',
          type: dataset_type
        }
      );

      json_generator.forBlock[dataset_type] = dataset_filter_generator;

      logger(toolbox);

      for (let variable of dataset["variables"]) {
        const variable_name = variable["name"];
        const variable_type = get_block_filter_type(dataset_name, variable_name);
        const variable_label = typeof (variable["label"]) === "string" ? variable["label"] : "";
        const kind = variable["kind"];
        const block_color = dataset_color; // Otherwise it takes the value of dataset_color from the outer closure
        const variable_na_label = "NA(" + variable["NA_count"] + "):"

        if (kind === "categorical") {
          const values = variable["values_count"];
          let dd_options = [];
          for (let v of values) {
            dd_options.push([v.value, v.value])
          }

          if (dd_options.length == 0) dd_options = [['_EMPTY_VEC_', '_EMPTY_VEC_']]

          Blockly.Blocks[variable_type] = {
            init: function () {
              this.appendEndRowInput()
                .appendField(`[(${dataset_name}) - ${variable_name}]`)
                .appendField(new Blockly.FieldLabel(variable_label, "blockly_filter_bold"))
                .appendField(new multiPickerField(dd_options), "value")
                .appendField(variable_na_label)
                .appendField(new Blockly.FieldCheckbox(false), 'include_NA');
              this.setOutput(true);
              this.setColour(block_color);
              this.variable_name = variable_name;
              this.dataset_name = dataset_name;
            }
          }

          json_generator.forBlock[variable_type] = filter_generator_subset;

        } else if (kind === "numerical") {
          const min = variable["min"];
          const max = variable["max"];

          Blockly.Blocks[variable_type] = {
            init: function () {
              this.appendDummyInput()
                .appendField(`[(${dataset_name}) - ${variable_name}]`)
                .appendField(new Blockly.FieldLabel(variable_label, "blockly_filter_bold"))
                .appendField("Min:")
                .appendField(new rangeSliderField(min, min, max), "min")
                .appendField("Max:")
                .appendField(new rangeSliderField(max, min, max), "max")
                .appendField(new Blockly.FieldLabel(variable_na_label, "na_label"))
                .appendField(new Blockly.FieldCheckbox(false), 'include_NA');

              this.setOutput(true);
              this.setColour(block_color);
              this.variable_name = variable_name;
              this.dataset_name = dataset_name;
            }
          }
          json_generator.forBlock[variable_type] = filter_generator_range;
        } else if (kind === "date") {
          const min = variable["min"];
          const max = variable["max"];

          Blockly.Blocks[variable_type] = {
            init: function () {
              this.appendEndRowInput()
                .appendField(`[(${dataset_name}) - ${variable_name}]`)
                .appendField(new Blockly.FieldLabel(variable_label, "blockly_filter_bold"))
                .appendField("From:")
                .appendField(new datePickerField(min, min, max), "min")
                .appendField("to:")
                .appendField(new datePickerField(max, min, max), "max")
                .appendField(variable_na_label)
                .appendField(new Blockly.FieldCheckbox(false), 'include_NA');
              this.setOutput(true);
              this.setColour(block_color);
              this.variable_name = variable_name;
              this.dataset_name = dataset_name;
            }
          }
          json_generator.forBlock[variable_type] = filter_generator_date_range;
        } else {
          console.error("Unknown variable kind: " + kind)
          continue;
          // throw new Error("Unknown field kind: " + kind);
        }

        let variable_block = {
          kind: 'block',
          type: variable_type
        };
        dataset_category.contents.push(variable_block);
      }
      toolbox.contents.push(dataset_category);
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

        let root_block = current_block;

        while (root_block.getParent() !== null) {
          root_block = root_block.getParent();
        }

        // Only check if the top parent is a dataset

        if (root_block.is_top_dataset) {
          const dataset_name = root_block.dataset_name;
          let stack = [];
          stack.push(root_block);
          while (stack.length) {
            let b = stack.pop();
            if (b.dataset_name && b.dataset_name !== dataset_name) {
              logger("Incorrect piece in stack");
              current_block.unplug();
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


    let options = {};

    options.toolbox = toolbox;
    let ws = Blockly.inject(container_div, options);

    ws.MAX_UNDO = 0; //Disconnect undo because of listeners
    // When removing elements using JS the undo is messed up
    // For example when removing an element from an operation the id of the input changes
    // and redo cannot happen because we cannot reconnect as the previous input id is
    // stored in the undo stack. No control is provided over the undo stack.

    ws.addChangeListener(onChange);

    let state_for_restore;

    if(script_tag.hasAttribute("bookmark")) {
      state_for_restore = json_data.bookmark.filters;
      script_tag.toggleAttribute("bookmark");
    } else {
      state_for_restore = json_data.state;
    }

    let filter_state, filter_state_log;
    [filter_state, filter_state_log] = filter_to_state(state_for_restore, selected_datasets);

    if (filter_state) {      
      try {        
        Blockly.serialization.workspaces.load(filter_state, ws);
        ws.cleanUp(); // If overlap reorganize
      } catch (error) {
        alert("Error restoring preset/bookmarked state");
      }

    }

    let res = {
      workspace: ws,
      generator: json_generator,
      dataset_name: dataset_name,
      log: filter_state_log
    }
    return (res)
  }

  let chaff = function(){
    if(Blockly.getMainWorkspace() !== undefined){
      Blockly.hideChaff; 
    }    
  }

  // Return public API
  return ({
    init: init,
    get_code: get_code,
    chaff: chaff
  });
 
})();

const init = filterBlockly.init;
const get_code = filterBlockly.get_code;
const chaff = filterBlockly.chaff;

let send_code = null;

let init_blockly_handler = function (msg) {

  // // let logger = console.log;
  let logger = function(x){}

  const container_id = msg[["container_id"]];
  const dataset = msg[["dataset"]];
  const button_id = msg[["gen_code_button_id"]];
  const json_input_id = msg[["json_input_id"]];
  const log_input_id = msg[["log_input_id"]];

  // Remove first if present
  if ($("#filter-filter_container").data("filter") !== undefined) {
    logger("Disposing");
    const filter = $("#filter-filter_container").data("filter");
    filter.workspace.dispose();
    $("#filter-filter_container").data("filter", undefined);
  }

  if(send_code !== null) {
    document.getElementById(button_id).removeEventListener('click', send_code);
  }

  // Initialize
  logger("Initializing: " + dataset);
  $('#' + container_id).data('filter', blockly_filter.init(container_id, dataset));
  send_code = function () {
    const filter = $('#' + container_id).data('filter');
    const code = blockly_filter.get_code(filter);
    logger("sending to " + json_input_id);
    logger(code);
    Shiny.setInputValue(json_input_id, code, { priority: 'event' });
  };  

  send_log = function () {
    const filter = $('#' + container_id).data('filter');
    const log = filter.log;
    logger("sending to " + log_input_id);
    logger(log);
    Shiny.setInputValue(log_input_id, log, { priority: 'event' });
  };

  send_log();
  send_code(); // Send code on init in case there is a preloaded state
  document.getElementById(button_id).addEventListener('click', send_code);
}

Shiny.addCustomMessageHandler("init_blockly_filter", init_blockly_handler);


export { init, get_code, chaff }


