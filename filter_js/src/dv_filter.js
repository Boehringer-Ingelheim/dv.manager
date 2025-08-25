/**
 * @license
 * Copyright Copyright 2024 Boehringer-Ingelheim Pharma GmbH & Co.KG
 * SPDX-License-Identifier: Apache-2.0
 * 
 * See repository root for full license and individual files for specific licenses
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
import './toolbox-search/index.js'


/* TODO: Filter data structure generation is slow, it is generated everytime the UI starts, it could be moved to a 
step previous to the UI, to avoid running it everytime we start the application. Memoize the operation?

/* TODO: Should a warning appear when a preset filters uses an out of range value. They are corrected by blockly but there is no warning*/

/* TODO: name consitency */

/* TODO: # When attaching the dependencies on my own an error occurs when using multiple
    # When including shinyWidget picker_input itself the error disappears, this should be explored
  */


/*TODO: Split in two workspaces one for the subject filters and another for the table filters
*/

/*TODO: Include buttons to save filter states and recover them later with names, etc.
*/

/*TODO: Let the user know when the filter is a non-finished state when pressing the apply filter button.
*/

/*TODO: Create an alternative UI that matches the current dv.filter while using the same backend
*/

/* TODO: Think about having, at least, an initial state. Ids of elements, etc.

/* TODO: Use data for attributes with the ids of the filter inside to avoid passing ids all the time and just trust
in a given internal structure.
*/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let logger = function (x) { console.log(x) }

let debounce = function (func, delay = 1000) {
  let timeoutId;
  return function (...args) {
    const context = this;
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => {
      func.apply(context, args);
    }, delay);
  };
}


//#region BLOCKLY FILTER

const BC = {
  TYPE: {
    ROW_COMB_OPERATION: 'c_row_comb_operation',
    ROW_NOT_OPERATION: 'c_row_not_operation',
    SET_COMB_OPERATION: 'c_set_comb_operation',
    SET_COMPLEMENT_OPERATION: 'c_set_complement_operation',
    DATASETS_FILTER: 'datasets_filter',
    SUBJECT_FILTER: 'subject_filter'
  },
  ATTRIBUTE: {
    INNER_FILTER: "data-inner-filter",
    MODAL: "data-blockly-modal"
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

const filter_state_to_blockly_state = function (previous_filter, dataset_list) {

  let filter = undefined;
  if (previous_filter) {
    filter = previous_filter.filters
  }

  let state = {
    blocks: {
      languageVersion: 0,
      blocks: []
    }
  };

  let log = [];

  if (filter) {

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
          let dataset_names = dataset_list.map((x) => x.name);
          if (dataset_names.includes(current_filter.dataset)) {
            let dataset_idx = dataset_list.map((x) => x.name).indexOf(current_filter.dataset);
            let variable_names = dataset_list[dataset_idx].variables.map((x) => x.name);
            if (!variable_names.includes(current_filter.variable)) {
              applicable = false
            }
          } else {
            applicable = false;
          }

          if (!applicable) {
            res = null;
            log = "Preselected/bookmarked filter is not applicable to the current dataset";
            break;
          }

          if (current_filter.operation === "select_subset") {
            logger("as subset");
            logger(current_filter);
            let dataset_idx = dataset_list.map((x) => x.name).indexOf(current_filter.dataset);
            let variable_names = dataset_list[dataset_idx].variables.map((x) => x.name);
            let variable_idx = variable_names.indexOf(current_filter.variable);
            let variable_values = dataset_list[dataset_idx].variables[variable_idx].values_count.map((x) => x.value);
            let found = current_filter.values.filter((x) => variable_values.includes(x));
            let removed = current_filter.values.filter((x) => !variable_values.includes(x));

            if (removed.length > 0) {
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

        } else if (current_filter.kind === "set_operation") {

          if (current_filter.operation === "intersect" || current_filter.operation === "union") {
            current_block.type = BC.TYPE.SET_COMB_OPERATION;
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
          } else if (current_filter.operation === "complement") {
            current_block.type = BC.TYPE.SET_COMPLEMENT_OPERATION;
            current_block.id = Blockly.utils.idGenerator.genUid();
            current_block.fields = { operation: current_filter.operation };
            current_block.inputs = {
              contents_fix: { block: {} }
            };
            stack.push([current_block.inputs.contents_fix.block, current_filter.children[0]]);
          } else {
            throw new Error("Unkown operation: " + current_filter.operation)
          }

        } else if (current_filter.kind === "row_operation") {

          if (current_filter.operation === "and" || current_filter.operation === "or") {
            current_block.type = BC.TYPE.ROW_COMB_OPERATION;
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
            current_block.type = BC.TYPE.ROW_NOT_OPERATION;
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

    if (filter[BC.TYPE.SUBJECT_FILTER] && filter[BC.TYPE.SUBJECT_FILTER].children.length > 0) {
      let processed_filter = process_filter(filter[BC.TYPE.SUBJECT_FILTER].children[0]);
      if (processed_filter !== null) {
        let subject_filter = {
          type: BC.TYPE.SUBJECT_FILTER,
          id: Blockly.utils.idGenerator.genUid(),
          x: 0,
          y: 0,
          inputs: { content: { block: process_filter(filter[BC.TYPE.SUBJECT_FILTER].children[0]) } }
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

const dataset_filter_generator = function (block, generator) {
  const children_code = generator.valueToCode(block, "children", 0);
  const code = '{"name" : "' + block.dataset_name + '", "kind": "dataset", "children": [' + children_code + ']}';
  return (code);
}

const subject_filter_generator = function (block, generator) {
  const code = generator.valueToCode(block, "content", 0);
  return ('{"subject_filter": {"children": [' + code + '] }}');
}

const set_operation_generator = function (block, generator) {
  let children_code = "";
  let current_inputs = block.inputList.map(x => x.name);
  for (input of current_inputs) {
    const code = generator.valueToCode(block, input, 0);
    if (code !== '') {
      children_code = children_code + code + ", ";
    }
  }
  children_code = children_code.slice(0, -2);

  const kind = 'set_operation';
  const operation = block.getFieldValue('operation');
  const code = '{' +
    '"kind": "' + kind + '"' +
    ', "operation": "' + operation + '"' +
    ', "children": [' + children_code + ']}';
  return ([code, null])
}

const row_operation_generator = function (block, generator) {
  let children_code = "";
  let current_inputs = block.inputList.map(x => x.name);
  for (input of current_inputs) {
    const code = generator.valueToCode(block, input, 0);
    if (code !== '') {
      children_code = children_code + code + ", ";
    }
  }
  children_code = children_code.slice(0, -2);

  const kind = 'row_operation';
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

const get_blockly_code = function ({ workspace, generator, dataset_name }) {

  const start = new Date();

  let filters = {
    datasets_filter: { children: [] },
    subject_filter: { children: [] }
  };

  let blockly_state = Blockly.serialization.workspaces.save(workspace);

  if (blockly_state["blocks"] !== undefined) {

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

  const end = new Date();
  logger("Get code: " + (end.getTime() - start.getTime()) + " ms");

  return (res_state)
}

const init_blockly = function (el, dataset_name, filter_data, init_state) {
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

  const container_div = el;


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

  json_generator.forBlock[BC.TYPE.SUBJECT_FILTER] = subject_filter_generator;

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
            type: BC.TYPE.SUBJECT_FILTER
          },
        ]
      },
      {
        kind: 'category',
        name: 'Operations',
        contents: [
          {
            kind: 'block',
            type: BC.TYPE.ROW_COMB_OPERATION
          },
          {
            kind: 'block',
            type: BC.TYPE.ROW_NOT_OPERATION
          },
          {
            kind: 'block',
            type: BC.TYPE.SET_COMB_OPERATION
          },
          {
            kind: 'block',
            type: BC.TYPE.SET_COMPLEMENT_OPERATION
          }
        ]
      },
    ]
  };

  json_generator.forBlock[BC.TYPE.ROW_COMB_OPERATION] = row_operation_generator;
  json_generator.forBlock[BC.TYPE.ROW_NOT_OPERATION] = row_operation_generator;
  json_generator.forBlock[BC.TYPE.SET_COMB_OPERATION] = set_operation_generator;
  json_generator.forBlock[BC.TYPE.SET_COMPLEMENT_OPERATION] = set_operation_generator;

  // Preface End

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

  let append_value_input_row_comb = function (block) {
    block.appendValueInput(get_random_input_id()).setCheck(["filter", "row"]);
  }

  let append_value_input_set_comb = function (block) {
    block.appendValueInput(get_random_input_id()).setCheck(["set", "filter", "row"]);
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

  Blockly.Blocks[BC.TYPE.ROW_COMB_OPERATION] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['and', 'and'], ['or', 'or']]
        ), "operation");
      this.setOutput(true, "row");
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
        append_value_input_row_comb(this);
      }
    }
  };

  Blockly.Blocks[BC.TYPE.ROW_NOT_OPERATION] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['not', 'not']]
        ), "operation")
      this.appendValueInput("contents_fix")
        .setCheck(["filter", "row"]);
      this.setOutput(true, "row");
      this.setColour(255);
    }
  };

  Blockly.Blocks[BC.TYPE.SET_COMB_OPERATION] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['intersect', 'intersect'], ['union', 'union']]
        ), "operation");
      this.setOutput(true, "set");
      this.setColour(160);
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
        append_value_input_set_comb(this);
      }
    }
  };

  Blockly.Blocks[BC.TYPE.SET_COMPLEMENT_OPERATION] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['complement', 'complement']]
        ), "operation")
      this.appendValueInput("contents_fix")
        .setCheck(["set", "filter", "row"]);
      this.setOutput(true, "set");
      this.setColour(160);
    }
  };

  Blockly.Blocks[BC.TYPE.SUBJECT_FILTER] = {
    init: function () {
      this.appendDummyInput('label')
        .appendField('Subject Filter');
      this.appendValueInput("content");
      this.setColour(160);
      this.is_top_subject = true;
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
        this.appendValueInput("children")
          .setCheck(["filter", "row"]);
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
            this.setOutput(true, "filter");
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

            this.setOutput(true, "filter");
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
            this.setOutput(true, "filter");
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

    // Code replacement is broken in set and row_operations by the code below
    // In replacement first the piece is disconnected and then reconnected
    // When disconnected the input in the piece is removed, then an attempt to connect is done
    // But the attempt is not possible because the input is no longer there
    // An attempt to fix it by placing back the input was done, but it was not possible
    // if(!new_parent_block.inputList.map(x=>x.name).includes(input_for_append)) new_parent_block.appendInput(input_for_append);
    // As `Uncaught TypeError TypeError: d.isVisible is not a function` appeared. The origin is unclear

    if (event.reason.includes("connect")) {

      // Check the connection is legal

      let root_block = current_block;

      // Look for the highest block that is a row operation or a dataset filter

      while (root_block.getParent() !== null &&
        root_block.getParent().type !== BC.TYPE.SET_COMB_OPERATION &&
        root_block.getParent().type !== BC.TYPE.SET_COMPLEMENT_OPERATION &&
        root_block.getParent().type !== BC.TYPE.SUBJECT_FILTER
      ) {
        root_block = root_block.getParent();
      }

      // Only check if the top parent is a dataset

      if (
        root_block.is_top_dataset ||
        root_block.type === BC.TYPE.ROW_COMB_OPERATION ||
        root_block.type === BC.TYPE.ROW_NOT_OPERATION
      ) {

        // If a dataset filter is on the top we take set the target dataset as that one
        // Otherwise we set it inside the loop. It is set to the first block that we find that has
        // a dataset_name property.
        // We travel the whole tree this is can be optimized but as trees should not be too deep it is left as is.

        let dataset_name = root_block.is_top_dataset ? root_block.dataset_name : null;
        logger("Before loop: " + dataset_name)
        let stack = [];
        stack.push(root_block);
        while (stack.length) {
          let b = stack.pop();
          if (b.dataset_name && dataset_name === null) {
            dataset_name = b.dataset_name
            logger("Set in loop: " + dataset_name)
          }
          logger("b.dataset_name: " + b.dataset_name)
          if (b.dataset_name && b.dataset_name !== dataset_name) {
            logger("Incorrect piece in stack");
            current_block.unplug();
            break;
          }
          stack.push(...b.getChildren());
        }
      }

      if (new_parent_block && new_parent_block.type === BC.TYPE.ROW_COMB_OPERATION) {
        remove_empty_inputs(new_parent_block);
        append_value_input_row_comb(new_parent_block);
      }

      if (new_parent_block && new_parent_block.type === BC.TYPE.SET_COMB_OPERATION) {
        remove_empty_inputs(new_parent_block);
        append_value_input_set_comb(new_parent_block);
      }
    } else if (event.reason.includes("disconnect")) {
      if (old_parent_block && (old_parent_block.type === BC.TYPE.ROW_COMB_OPERATION || old_parent_block.type === BC.TYPE.SET_COMB_OPERATION)) {
        // Remove the input that has been disconnected
        const input_for_removal = event.oldInputName;
        remove_value_inputs(old_parent_block, [input_for_removal]);
      }

    } else {
      return;
    }
  }


  let options = {};

  options.maxInstances = {};
  const idx_singleton_cat = toolbox.contents.findIndex(x => x.name === "Filter Types");
  toolbox.contents[idx_singleton_cat].contents.map((x) => options.maxInstances[x.type] = 1)

  options.toolbox = toolbox;
  let ws = Blockly.inject(container_div, options);

  ws.MAX_UNDO = 0; //Disconnect undo because of listeners
  // When removing elements using JS the undo is messed up
  // For example when removing an element from an operation the id of the input changes
  // and redo cannot happen because we cannot reconnect as the previous input id is
  // stored in the undo stack. No control is provided over the undo stack.

  ws.addChangeListener(onChange);

  let filter_state, filter_state_log;
  [filter_state, filter_state_log] = filter_state_to_blockly_state(init_state, selected_datasets);

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

let blockly_disposal = function(){};

let blockly_static_init = function(blockly_root_el) {

  let show_label = document.createElement('label');
  show_label.textContent = "Show filter";
  show_label.setAttribute("for", "blockly-filter-checkbox");
  show_label.className = "btn btn-primary";

  let show_checkbox = document.createElement("input");
  show_checkbox.type = "checkbox";
  show_checkbox.id = "blockly-filter-checkbox";
  show_checkbox.style.display = "none";

  blockly_root_el.appendChild(show_label);
  blockly_root_el.appendChild(show_checkbox);

  let modal_overlay = document.createElement('div');
  modal_overlay.className = "blockly_overlay";

  let modal = document.createElement('div');
  modal.className = "blockly_modal";
  modal.setAttribute(BC.ATTRIBUTE.MODAL, '');

  let title = document.createElement("h4");
  title.textContent = "Filter";

  let outer_filter_el = document.createElement("div");
  outer_filter_el.className = "blockly_filter outer";  

  let inner_filter_el = document.createElement("div");
  inner_filter_el.className = "blockly_filter inner";
  inner_filter_el.setAttribute(BC.ATTRIBUTE.INNER_FILTER, '');
  
  let gen_code_button = document.createElement("button");
  gen_code_button.type = "button";
  gen_code_button.className = "btn btn-primary btn-lg";
  gen_code_button.textContent = "Apply Filter";

  let export_code_button = document.createElement("button");
  export_code_button.type = "button";
  export_code_button.className = "btn btn-primary btn-lg";
  export_code_button.textContent = "Export Filter";

  outer_filter_el.appendChild(inner_filter_el);
  outer_filter_el.appendChild(gen_code_button);
  outer_filter_el.appendChild(export_code_button);

  let hide_label = document.createElement('label');
  hide_label.textContent = "close filter";
  hide_label.setAttribute("for", "blockly-filter-checkbox");
  hide_label.className = "btn btn-primary";

  modal.appendChild(title);
  modal.appendChild(outer_filter_el);
  modal.appendChild(hide_label);

  modal_overlay.append(modal);
  blockly_root_el.appendChild(modal_overlay);
  
  $(show_checkbox).change(function () {
    window.dispatchEvent(new Event('resize')); //Otherwise blockly is wrongly sized
    chaff();
  });

  modal_overlay.addEventListener('click', function(event){
    let $target = $(event.target);
      if(!$target.closest(`[${BC.ATTRIBUTE.MODAL}]`).length) {
        show_checkbox.checked = false;
        $(show_checkbox).trigger('change');
      }
  });
  
  let send_code = function () {
    const filter = $(inner_filter_el).data('filter');
    const code = get_blockly_code(filter);
    const event = new CustomEvent(FC.EVENT.UPDATED_FILTER, {
      detail: {filter: code, mode: FC.MODE.BLOCKLY},
      bubbles: true,
      cancelable: true
    });
    blockly_root_el.dispatchEvent(event);
  };

  blockly_disposal = function(){
    const filter = $(inner_filter_el).data("filter");
    filter.workspace.dispose();
    $(inner_filter_el).data("filter", undefined);        
  }
  gen_code_button.addEventListener('click', send_code);


  let res = {
    send_code: send_code
  };

  return(res);
};

let chaff = function () {
  if (Blockly.getMainWorkspace() !== undefined) {
    Blockly.hideChaff();
  }
}

//#endregion BLOCKLY FILTER

//#region Simple filter

const SC = {
  EVENTS: {
    CHANGED_FILTER: 'dv_filter:changed'
  },
  ATTRIBUTE: {
    DATASET: "data-dataset",
    SUBJECT_FILTER: 'data-subject-filter',
    VARIABLE: "data-variable",
    KIND: "data-kind",
    FILTER_CONTROL: "data-filter-control"
  },
  VARIABLE: {
    NUMERICAL: "numerical",
    CATEGORICAL: "categorical",
    DATE: "date"
  }
}

let get_simple_root_el = function(el){
  return(get_root_el(el).querySelector(`[${FC.ATTRIBUTE.FILTER_MODE}="${FC.MODE.SIMPLE}"]`));
}

// Returns a simplified filter state or null if the filter_state is not compatible with the simple filter
// This can happen because the simple filter can express a subset of the concepts available in the blockly one
let check_state_compatibility = function(state, subject_dataset_name) {

  let check_single_dataset = function (state) {
    let dataset_name = state.name;
    let compatible = true;
    let simple_state = [];
    compatible = compatible && state.children.length <= 1;
    
    if(!(state.children.length <= 1)) {
      console.error("First child length > 1");
    }

    if (state.children.length === 1) {
      compatible = compatible && (state.children[0].kind === "filter" || (state.children[0].kind === "row_operation" && state.children[0].operation === "and"));
      if (state.children[0].kind === "row_operation" && state.children[0].operation === "and") {
        let ok_all_child = state.children[0].children.reduce(function (acc, obj) { return (acc && obj.kind === "filter" && obj.dataset === dataset_name) }, true);
        if (!ok_all_child) {
          console.error("At least one child is not of kind filter or does not belong to the correct dataset")
        }
        compatible = compatible && ok_all_child;
        simple_state = state.children[0].children;
      } else {
        let filter_and_correct_dataset = state.children[0].kind === "filter" && state.children[0].dataset === dataset_name;
        compatible = compatible && filter_and_correct_dataset;
        if (!filter_and_correct_dataset) {
          console.error("First child is not and or filter with correct dataset")
        }
        simple_state = [state.children[0]];
      }
    }

    let res = {
      compatible: compatible,
      state: compatible ? simple_state : null
    };    

    return (res);
  }
  
  let compatible = true;
  let states = {};

  if (state !==null) {

    let dataset_filters_to_be_checked = structuredClone(state.filters.datasets_filter.children);

    let sbj_filter = {
      name: subject_dataset_name,
      children: structuredClone(state.filters.subject_filter.children)
    };

    dataset_filters_to_be_checked.push(sbj_filter);

    for(let i = 0; i < dataset_filters_to_be_checked.length; ++i) {
      let current_check = check_single_dataset(dataset_filters_to_be_checked[i]);
      compatible = compatible && current_check.compatible;
      states[dataset_filters_to_be_checked[i].name] = current_check.state;
    }

  }

  return({state:states, compatible: compatible})
};

// Creates the variable selector for each of the datasets
// Container_el is the parent container in which the container for all the selectors will be created. We look for the container itself
// Only called on from the simple dynamic init
let update_dataset_filter = function(simple_root_el, dataset, dataset_filter_state, is_subject_filter) {
  let selected_variables = [];
  for(let i = 0; i < dataset_filter_state.length; ++i) {
    selected_variables.push(dataset_filter_state[i].variable)
  }
  
  logger("Updating UI for " + dataset.name);  

  let prev_div = simple_root_el.querySelector(`[${SC.ATTRIBUTE.DATASET} = '${dataset.name}']`);
  if(prev_div) {prev_div.remove();};
  
  let dataset_filter_container = document.createElement('div');
  dataset_filter_container.className = "panel panel-primary";
  dataset_filter_container.setAttribute(SC.ATTRIBUTE.DATASET, dataset.name);
  dataset_filter_container.setAttribute(SC.ATTRIBUTE.SUBJECT_FILTER, is_subject_filter);
  
  let title = document.createElement("div");
  title.className = "panel-heading";
  title.textContent = dataset.name;
  dataset_filter_container.appendChild(title);
  
  let control_container = document.createElement('div');
  control_container.className = 'panel-body';
  control_container.setAttribute(SC.ATTRIBUTE.FILTER_CONTROL, '');

  let select = document.createElement('select');
  select.className = 'selectpicker';
  select.setAttribute('multiple', '');
  select.setAttribute('title', 'Choose an option');
  select.setAttribute('data-live-search', 'true');  

  for(let i = 0; i < dataset.variables.length; ++i) {
    let option = document.createElement('option');
    option.value = dataset.variables[i].name;
    option.textContent = `${dataset.variables[i].name} - ${dataset.variables[i].label}`;
    if(selected_variables.includes(option.value)) {
      option.setAttribute("selected", "");
    }
    select.appendChild(option);
  }
 
  dataset_filter_container.appendChild(select);
  dataset_filter_container.appendChild(control_container);
  simple_root_el.appendChild(dataset_filter_container);
  $(select).selectpicker();
  update_filter_controls(control_container, dataset, selected_variables,  dataset_filter_state);
};

// Creates each of the selected variable filters
// Container_el is the container itself where each of the container, it is the container itself
let update_filter_controls = function(control_container_el, dataset, selected_variables, dataset_filter_state) {
  // Redraw on filter changes? Redraw on show?
  // Redraw smartly and only remove or include specific divs
  // Clean UI and listeners
  let ion_range_slider_to_destroy = control_container_el.querySelectorAll(`[${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.NUMERICAL}'] input`);
  logger("Destroying: " + ion_range_slider_to_destroy.length + " ion.range.sliders");
  for(let i = 0; i < ion_range_slider_to_destroy.length; ++i) {
    $(ion_range_slider_to_destroy[i]).data("ionRangeSlider").destroy();
  }
  control_container_el.innerHTML = ''; // Last step to remove actual DOM elements

  for(let i = 0; i < selected_variables.length; ++i) {      
    let current_variable = dataset.variables.find((obj)=> obj.name===selected_variables[i]);
    let current_state = dataset_filter_state.find((obj)=> obj.variable===current_variable.name);

    // #region common header
    let variable_div = document.createElement("div");
    variable_div.setAttribute(SC.ATTRIBUTE.VARIABLE, current_variable.name);
    variable_div.setAttribute(SC.ATTRIBUTE.KIND, current_variable.kind);
    variable_div.style = "margin-bottom: 20px";

    let variable_header = document.createElement("div");    
    variable_header.style.display = "flex";
    variable_header.style.justifyContent = "space-between";
    variable_header.style.alignItems = "center";

    let variable_label = document.createElement("span");
    variable_label.className = "label label-default";
    variable_label.textContent = current_variable.name;

    let variable_close_button = document.createElement("button");
    variable_close_button.type = "button";
    variable_close_button.className = "btn btn-danger close";
    variable_close_button.setAttribute("data-action", "remove");

    let variable_close_icon = document.createElement("span");      
    variable_close_icon.innerHTML = "&times;"    

    variable_close_button.appendChild(variable_close_icon);

    variable_header.appendChild(variable_label);
    variable_header.appendChild(variable_close_button);
    variable_div.appendChild(variable_header);

    //#endregion

    if(current_variable.kind === SC.VARIABLE.CATEGORICAL) {
      let variable_select = document.createElement('select');
      variable_select.className = 'selectpicker';
      variable_select.setAttribute('multiple', '');        
      variable_select.setAttribute('data-live-search', 'true');
      variable_select.setAttribute('data-actions-box', 'true');

      for(let i = 0; i < current_variable.values_count.length; ++i) {
        let option = document.createElement('option');
        option.value = current_variable.values_count[i].value;
        option.textContent = current_variable.values_count[i].value;
        if(current_state) {
          if(current_state.values.includes(option.value)) {
            option.setAttribute("selected", '');         
          }
        } else {
          option.setAttribute("selected", '');          
        }
        variable_select.appendChild(option);
      }

      variable_div.appendChild(variable_select);
      control_container_el.appendChild(variable_div);
      $(variable_select).selectpicker();
            
    } else if (current_variable.kind === SC.VARIABLE.DATE) {
      variable_select = document.createElement("p");
      variable_select.textContent = `${current_variable.name} - ${current_variable.kind}`;
      variable_div.appendChild(variable_select);
      control_container_el.appendChild(variable_div);
    } else if (current_variable.kind === SC.VARIABLE.NUMERICAL) {
      let variable_input = document.createElement("input");
      variable_div.appendChild(variable_input);
      control_container_el.appendChild(variable_div);

      let from;
      let to;
      if(current_state) {
        from = Math.max(current_state.min, current_variable.min);
        to = Math.min(current_state.max, current_variable.max);
      } else {
        from = current_variable.min;
        to = current_variable.max;
      }

      $(variable_input).ionRangeSlider({
          min: current_variable.min,
          max: current_variable.max,
          type: "double",
          from: from,
          to: to,
          skin: "shiny",
          grid: "true",
          onFinish: function () {$(variable_input).trigger("finished.ion.range.slider");}
      });

    } else {
      variable_select = document.createElement("p");
      variable_select.textContent = `${current_variable.name} - ${current_variable.kind}`;
      variable_div.appendChild(variable_select);
      control_container_el.appendChild(variable_div);
    }  
  };
};

// Gets the state of an specific dataset
let get_single_dataset_filter_state = function (dataset_container_el) {
  let filter_state = [];
  let dataset_name = dataset_container_el.getAttribute(SC.ATTRIBUTE.DATASET);

  let variable_selectors = dataset_container_el.querySelectorAll(`[${SC.ATTRIBUTE.VARIABLE}]`);

  let include_NA = false; // TODO: cover NA cases

  for (let i = 0; i < variable_selectors.length; ++i) {
    let current_variable = variable_selectors[i];
    let kind = current_variable.getAttribute(SC.ATTRIBUTE.KIND);
    let variable_name = current_variable.getAttribute(SC.ATTRIBUTE.VARIABLE);

    let dataset_parent = current_variable.closest(`[${SC.ATTRIBUTE.DATASET}]`);
    if (!dataset_parent) {
      throw new Error(`No data-dataset parent found for element: ${current_variable.outerHTML}`);
    }
    let dataset_name = dataset_parent.getAttribute(SC.ATTRIBUTE.DATASET);
    let curr_filter;

    // Find the select inside that parent
    if (kind === SC.VARIABLE.CATEGORICAL) {

      let select = current_variable.querySelector("select");
      if (!select) {
        throw new Error(`No select element found inside: ${current_variable.outerHTML}`);
      }

      let values = $(select).val();

      curr_filter = {
        kind: "filter",
        dataset: dataset_name,
        operation: "select_subset",
        variable: variable_name,
        values: values,
        include_NA: include_NA
      }

    } else if (kind === SC.VARIABLE.DATE) {

    } else if (kind === SC.VARIABLE.NUMERICAL) {

      let input = current_variable.querySelector("input");
      if (!input) {
        throw new Error(`No input element found inside: ${current_variable.outerHTML}`);
      }

      let from = $(input).data("ionRangeSlider").result.from;
      let to = $(input).data("ionRangeSlider").result.to;

      curr_filter = {
        kind: "filter",
        dataset: dataset_name,
        operation: "select_range",
        variable: variable_name,
        min: from,
        max: to,
        include_NA: include_NA
      }

    } else {
      throw new Error(`Unknown kind '${kind}' in ${dataset_name} - ${variable_name}`);
    }
    filter_state.push(curr_filter);
  }


  if(filter_state.length > 0) {
    filter_state = [{
      kind: "row_operation",
      operation: "and",
      children: filter_state
    }]
  }

  let res = {
    state: filter_state,
    dataset_name: dataset_name
  }

  return(res);
};

/**
 * Returns the filter state for the simple filter
 * 
 * @param simple_root_el Top div of simple filter
 * 
 * @param dataset_list_name name of the current data_list
 * 
*/
// Gets the state of the whole simple filter
let get_filter_state = function (simple_root_el, dataset_list_name) {

  let subject_div = simple_root_el.querySelector(`[${SC.ATTRIBUTE.SUBJECT_FILTER}=true]`);
  let other_div = simple_root_el.querySelectorAll(`[${SC.ATTRIBUTE.SUBJECT_FILTER}=false]`);

  let subject_filter = get_single_dataset_filter_state(subject_div);
  let dataset_filters = [];

  for (let i = 0; i < other_div.length; ++i) {
    let current_div = other_div[i];
    let current_state = get_single_dataset_filter_state(current_div);
    if (current_state.state.length > 0) {
      let res = {
        name: current_state.dataset_name,
        kind: "dataset",
        children: current_state.state
      }
      dataset_filters.push(res);
    }    
  }

  let state = {
    filters: {
      datasets_filter: {
        children: dataset_filters
      },
      subject_filter: {
        children: subject_filter.state
      }
    },
    dataset_list_name: dataset_list_name
  };

  logger(state);
  return (state);
};

// Handles actions buttons. For not it is only one, maybe it is an unrequired generalization
let handle_action = function() {
    
  let handlers = {
    remove: function(el) {
      const variable_to_be_removed = el.closest(`[${SC.ATTRIBUTE.VARIABLE}]`).getAttribute(SC.ATTRIBUTE.VARIABLE); 
      const select = el.closest(`[${SC.ATTRIBUTE.DATASET}]`).querySelector("select");
      let current_selection = $(select).val();
      let new_selection = current_selection.filter(item => item != variable_to_be_removed);
      $(select).selectpicker('val', new_selection);      
    }
  };
  handlers[this.getAttribute('data-action')](this);
};

let dispatch_simple_filter_changed = function(event) {  
  get_simple_root_el(event.target).dispatchEvent(new CustomEvent(SC.EVENTS.CHANGED_FILTER));
};

// Initialize all listeners, no listener should happen outside here
let simple_static_init = function(simple_root_el) {  
  if(!simple_root_el) {
    throw new Error(`simple_root_e found not found`);
  }
  
  let send_code = function() {
    logger("Simple sending code");
    let dataset_list_name = get_filter_property(simple_root_el, FC.PROPERTY.DATASET_LIST_NAME);
    let code = get_filter_state(simple_root_el, dataset_list_name);
    const new_event = new CustomEvent(FC.EVENT.UPDATED_FILTER, {
      detail: {filter: code, mode: FC.MODE.SIMPLE},
      bubbles: true,
      cancelable: true
    });
    simple_root_el.dispatchEvent(new_event);
  }

  
  
  $(simple_root_el).on('changed.bs.select', `div[${SC.ATTRIBUTE.DATASET}] > div.dropdown > select`, function(event) { //FIXME: This selector is ugly it can be done better
    let dataset_div = event.target.closest(`div[${SC.ATTRIBUTE.DATASET}]`);
    let dataset_name = dataset_div.getAttribute(SC.ATTRIBUTE.DATASET);
    let dataset_list_name = get_filter_property(simple_root_el, FC.PROPERTY.DATASET_LIST_NAME);

    let current_dataset_list = get_filter_property(simple_root_el, FC.PROPERTY.DATA).dataset_lists.find(obj=>obj.name === dataset_list_name);
    let dataset = current_dataset_list.dataset_list.find(obj=>obj.name === dataset_name);

    let selected_variables = $(event.target).val();

    let dataset_control_div = dataset_div.querySelector(`[${SC.ATTRIBUTE.FILTER_CONTROL}]`);

    let dataset_filter_state = check_state_compatibility(get_filter_state(get_simple_root_el(dataset_div), dataset_list_name), get_filter_property(simple_root_el, FC.PROPERTY.SUBJECT_DATASET_NAME)).state[dataset_name];

    update_filter_controls(dataset_control_div, dataset, selected_variables, dataset_filter_state);
    dispatch_simple_filter_changed(event);
  });

  $(simple_root_el).on("changed.bs.select", `div[${SC.ATTRIBUTE.VARIABLE}][${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.CATEGORICAL}'] select`, dispatch_simple_filter_changed);
  $(simple_root_el).on("finished.ion.range.slider", `div[${SC.ATTRIBUTE.VARIABLE}][${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.NUMERICAL}'] input`, dispatch_simple_filter_changed);

  //TODO: Consider debounce
  simple_root_el.addEventListener(SC.EVENTS.CHANGED_FILTER, send_code);  
  $(simple_root_el).on('click', "[data-action]", handle_action);

  let res = {
    send_code: send_code
  }

  return(res);
}

// Handles the changes of datasets
let simple_dynamic_init = function(simple_root_el, filter_data, subject_dataset_name, filter_state) {

  // Subject filter

  let simple_filter_state = check_state_compatibility(filter_state, subject_dataset_name);

  if(!simple_filter_state.compatible) {
    console.error("State not compatible")    
  } else {
    logger("State compatible")
  }

  let subject_dataset = filter_data.dataset_list.find(obj=>obj.name === subject_dataset_name);
  let other_datasets = filter_data.dataset_list.filter(obj=>obj.name !== subject_dataset_name);
  
  update_dataset_filter(
    simple_root_el,
    subject_dataset,
    simple_filter_state.state[subject_dataset_name],
    true
  );

  for(let i = 0; i < other_datasets.length; ++i) {    
    update_dataset_filter(
      simple_root_el,
      other_datasets[i],
      simple_filter_state.state[other_datasets[i].name],
      false
    )
  }
}


//#endregion


//#region General init

let get_blockly_root_el = function(el){
  return(get_root_el(el).querySelector(`[${FC.ATTRIBUTE.FILTER_MODE}="${FC.MODE.BLOCKLY}"]`));
}

let init_filter_handler = function (msg, root_el, initial_send_code) {

  let dataset_list_name = msg.dataset_list_name;
  set_filter_property(root_el, FC.PROPERTY.DATASET_LIST_NAME, dataset_list_name);  

  let filter_data = get_filter_property(root_el, FC.PROPERTY.DATA);
  let subject_filter_dataset_name = get_filter_property(root_el, FC.PROPERTY.SUBJECT_DATASET_NAME);
  let filter_state = get_filter_property(root_el, FC.PROPERTY.STATE);

  let dataset_list = filter_data.dataset_lists.find(obj=>obj.name === dataset_list_name);

  simple_dynamic_init(
    get_simple_root_el(root_el),
    dataset_list,
    subject_filter_dataset_name,
    filter_state
  );

  let blockly_el = get_blockly_root_el(root_el);  
  blockly_dynamic_init(
    blockly_el,    
    dataset_list_name,    
    filter_data, filter_state
  );
  
  initial_send_code();
}

let blockly_dynamic_init = function(blockly_root_el, dataset_list_name, filter_data, filter_state) {
  let inner_filter_el = blockly_root_el.querySelector(`[${BC.ATTRIBUTE.INNER_FILTER}]`);
  const filter = $(inner_filter_el).data("filter");
  if (filter) {
    filter.workspace.dispose();
    $(inner_filter_el).data("filter", undefined);
  }
  $(inner_filter_el).data('filter', init_blockly(inner_filter_el, dataset_list_name, filter_data, filter_state));
}

let FC = {
  MODE: {
    SIMPLE: "simple",
    DATASETS: "datasets",
    BLOCKLY: "blockly"
  },
  EVENT: {
    UPDATED_FILTER: "updated_filter"
  },
  ATTRIBUTE: {
    ROOT: "data-root",
    FILTER_MODE: "data-filter-mode"
  },
  PROPERTY: {
    DATA: "filter_data",
    STATE: "filter_state",
    DATASET_LIST_NAME: "dataset_list_name",
    SUBJECT_DATASET_NAME: "subject_dataset_name"
  }
}

let get_root_el = function(el) {
  let root_el;
  if(el.hasAttribute(FC.ATTRIBUTE.ROOT)) {
    root_el = el;
  } else {
    root_el = el.closest(`[${FC.ATTRIBUTE.ROOT}]`)    
  }
  
  if(!root_el) {
    throw new Error("no root found from" + el);
  }
  return(root_el);
}

let get_filter_property = function(el, property) {  
  return(structuredClone(get_root_el(el)[property]));
}

let set_filter_property = function(el, property, val) {
  return(get_root_el(el)[property] = val);
}

let get_selected_filter_mode = function(el) {
  return(get_root_el(el).querySelector("select").value);
}

const init = function(root_id, filter_data, filter_state, subject_dataset_name, filter_json_input_id, filter_log_input_id) {
  logger("Filter root id: " + root_id);

  let root_el = document.getElementById(root_id);
  root_el.setAttribute(FC.ATTRIBUTE.ROOT, '');  
  root_el[FC.PROPERTY.DATA] = filter_data;
  root_el[FC.PROPERTY.STATE] = filter_state;
  root_el[FC.PROPERTY.SUBJECT_DATASET_NAME] = subject_dataset_name;
  let select = document.createElement('select');

  root_el.appendChild(select);

  // Simple

  let simple_option = document.createElement('option');
  simple_option.value = FC.MODE.SIMPLE;
  simple_option.textContent = FC.MODE.SIMPLE;
  select.appendChild(simple_option);
  
  let simple_div = document.createElement("div");
  simple_div.setAttribute(FC.ATTRIBUTE.FILTER_MODE, FC.MODE.SIMPLE);
  root_el.appendChild(simple_div);

  let simple_init_ret = simple_static_init(simple_div);

  // Blockly

  let blockly_option = document.createElement('option');
  blockly_option.value = FC.MODE.BLOCKLY;
  blockly_option.textContent = FC.MODE.BLOCKLY;
  select.appendChild(blockly_option);
  
  let blockly_div = document.createElement("div");
  blockly_div.setAttribute(FC.ATTRIBUTE.FILTER_MODE, FC.MODE.BLOCKLY);
  root_el.appendChild(blockly_div);

  let blockly_init_ret = blockly_static_init(blockly_div);
  
  let change_filter_mode = function(event) {    
    let filter_divs = root_el.querySelectorAll(`[${FC.ATTRIBUTE.FILTER_MODE}]`);    
    let new_selection = select.value;
    logger(`Changing to: ${new_selection}`);

    for(let i = 0; i < filter_divs.length; ++i) {
      let current_div_filter = filter_divs[i];
      let current_mode = current_div_filter.getAttribute(FC.ATTRIBUTE.FILTER_MODE);

      if (new_selection === current_mode) {
        current_div_filter.style.display = 'block';
      } else {
        current_div_filter.style.display = 'none';
      }            
    }

    let initial_send_code;
    if(new_selection === FC.MODE.SIMPLE) {
      initial_send_code = simple_init_ret.send_code;
    } else if (new_selection === FC.MODE.BLOCKLY) {
      initial_send_code = blockly_init_ret.send_code;
    } else {
      throw new Error("Unknown mode: " + new_selection);
    }
    if(event) { //FIXME: Terrible we should not be distinguising by event
      init_filter_handler({dataset_list_name: get_filter_property(root_el, FC.PROPERTY.DATASET_LIST_NAME)}, root_el, initial_send_code);
    }    
  };

  select.value = FC.MODE.SIMPLE;

  select.addEventListener('change', change_filter_mode);
  change_filter_mode();

  root_el.addEventListener(FC.EVENT.UPDATED_FILTER, function(event){
    logger("Sending to Shiny " + filter_json_input_id);
    set_filter_property(event.target, FC.PROPERTY.STATE, event.detail.filter);
    Shiny.setInputValue(filter_json_input_id, JSON.stringify(event.detail.filter), { priority: 'event' });
  });

  // TODO: WHAT DO WE DO IN THE INITIAL PASS? THERE SHOULD BE AT LEAST ONE FILTER READY

  let initial_send_code;

  if(select.value === FC.MODE.SIMPLE) {
    initial_send_code = simple_init_ret.send_code;
  } else if (select.value === FC.MODE.BLOCKLY) {
    initial_send_code = blockly_init_ret.send_code;
  } else {
    throw new Error("Unknown mode: " + select.value);
  }

  let baked_init_filter_handler = function(msg) {
    init_filter_handler(msg, root_el, initial_send_code);
  };
  
  Shiny.addCustomMessageHandler("init_filter", baked_init_filter_handler);
}

//#endregion

export {init}
