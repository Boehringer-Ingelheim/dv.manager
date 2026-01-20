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
import {deserialize_b64_filter_data} from './js_deserializer/deserializer.mjs';
import './toolbox-search/index.js'

const __DEV_MODE = false;
const __LOGGER = false;
const __TIMER = false;

let __logger = function(x){};
let ___logger = function(x){
  console.warn("single logger called")
  console.log(x)
};
let __assert = function(condition, message){};
let __time_function_start = function(caller_name) {};
let __time_function_end = function(caller_name) {};
let __get_caller_name = function() {};
  
if(__DEV_MODE) {

  let timed_functions = [
    "simple_dynamic_init"
    // ,"create_variable_filter_controls"
    // ,"get_single_dataset_filter_state"
    // ,"get_filter_state"
    // ,"create_dataset_filter"
    // ,"simple_static_init"  
    // ,"update_filter_controls"
    // ,"update_dataset_filter" 
  ];

  if(__LOGGER) {
    __logger = function(x) {console.log(x)};
  }
  
  __assert = function (condition, message) {
    if (__DEV_MODE && !condition()) {
      throw new Error(message || condition.toString());
    }
  };

  if(__TIMER) {
    __get_caller_name = function() {
      // console.time("__get_caller_name");
      const stack = new Error().stack;
      if (!stack) return undefined;
    
      const lines = stack.split("\n");
      // lines[0] = "Error"
      // lines[1] = "at getCallerName ..."
      // lines[2] = "at <CALLER_NAME> ..."
      const line = lines[3]?.trim();
      if (!line) return undefined;
  
      // Remove the "at " prefix and everything after the first space or (
      // Example: "at myFunction (file.js:10:5)" → "myFunction"
      const match = line.match(/at\s+([^\s(]+)/);    
      // console.timeEnd("__get_caller_name");
      return match ? match[1] : undefined; 
    };
  
    // Times are poisoned by get_caller_name in the order of 0.017 to 0.03 ms locally.
    __time_function_start = function(caller_name){
      if(!caller_name) {
        caller_name = __get_caller_name();        
      };
      
      if(timed_functions.includes(caller_name)) {
        // console.log("starting " + caller_name);
        console.time(caller_name);
      }
    };
    __time_function_end = function(caller_name){
      if(!caller_name) {
        caller_name = __get_caller_name();        
      };    
      if(timed_functions.includes(caller_name)) {
        // console.log("ending " + caller_name);
        console.timeEnd(caller_name)
      }
    };
  }  
}

let is_html_element = function(obj) {
  return obj instanceof HTMLElement && !!obj.tagName;
}

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

let max_str_date = function(date1, date2) {
  let num_date1 = new Date(date1).getTime();
  let num_date2 = new Date(date2).getTime();

  let res = date1;
  if (num_date2>num_date1) {
    res = date2;  
  }
  return(res);
}

let is_numeric_finite = function (value) {
  return typeof value === "number" && Number.isFinite(value);
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
  COLOR: {
    ROW_OPERATION: 0,
    SUBJECT_FILTER: 30,
    SET_OPERATION: 30,
    DATASET_RANGE_MIN:90,
    DATASET_RANGE_MAX:300,
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

const filter_state_to_blockly_state = function (previous_filter, dataset_list, ns) {

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

        __logger("Processing");
        __logger(current_block);
        __logger(current_filter);

        if (current_filter.kind === "filter") {
          current_block.type = ns(get_block_filter_type(current_filter.dataset, current_filter.variable));
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
            __logger("as subset");
            __logger(current_filter);
            let dataset_idx = dataset_list.map((x) => x.name).indexOf(current_filter.dataset);
            let variable_names = dataset_list[dataset_idx].variables.map((x) => x.name);
            let variable_idx = variable_names.indexOf(current_filter.variable);
            let variable_values = dataset_list[dataset_idx].variables[variable_idx].value;
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
            __logger("as range");
            __logger(current_filter);
            current_block.fields = {
              min: current_filter.min,
              max: current_filter.max,
              include_NA: current_filter.include_NA
            };
          } else if (current_filter.operation === "select_date") {
            __logger("as date");
            __logger(current_filter);
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
            current_block.type = ns(BC.TYPE.SET_COMB_OPERATION);
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
            current_block.type = ns(BC.TYPE.SET_COMPLEMENT_OPERATION);
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
            current_block.type = ns(BC.TYPE.ROW_COMB_OPERATION);
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
            current_block.type = ns(BC.TYPE.ROW_NOT_OPERATION);
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
          type: ns(BC.TYPE.SUBJECT_FILTER),
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
            type: ns(get_block_dataset_type(curr_dataset_filter.name)),
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
  __logger("Get code: " + (end.getTime() - start.getTime()) + " ms");

  return (res_state)
}

const init_blockly = function (el, dataset_name, filter_data, init_state, skip_dataset_filters) {
  let id = get_root_el(el).id;
  let namespace = id + "-" + dataset_name;
  let ns = function(x) {return(namespace + "-" + x)};

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
      let current_name = block_names[idx];
      if(current_name.startsWith(namespace)){
        __logger("Removing: " + block_names[idx]);
        Blockly.Blocks[block_names[idx]] = null;
      } else {
        __logger("Keeping: " + block_names[idx]);
      }
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

  json_generator.forBlock[ns(BC.TYPE.SUBJECT_FILTER)] = subject_filter_generator;

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
            type: ns(BC.TYPE.SUBJECT_FILTER)
          },
        ]
      },
      {
        kind: 'category',
        name: 'Operations',
        contents: [
          {
            kind: 'block',
            type: ns(BC.TYPE.ROW_COMB_OPERATION)
          },
          {
            kind: 'block',
            type: ns(BC.TYPE.ROW_NOT_OPERATION)
          },
          {
            kind: 'block',
            type: ns(BC.TYPE.SET_COMB_OPERATION)
          },
          {
            kind: 'block',
            type: ns(BC.TYPE.SET_COMPLEMENT_OPERATION)
          }
        ]
      },
    ]
  };

  json_generator.forBlock[ns(BC.TYPE.ROW_COMB_OPERATION)] = row_operation_generator;
  json_generator.forBlock[ns(BC.TYPE.ROW_NOT_OPERATION)] = row_operation_generator;
  json_generator.forBlock[ns(BC.TYPE.SET_COMB_OPERATION)] = set_operation_generator;
  json_generator.forBlock[ns(BC.TYPE.SET_COMPLEMENT_OPERATION)] = set_operation_generator;

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
        __logger("Skipping removal of " + element + "not found");
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

  Blockly.Blocks[ns(BC.TYPE.ROW_COMB_OPERATION)] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['and', 'and'], ['or', 'or']]
        ), "operation");
      this.setOutput(true, "row");
      this.setColour(BC.COLOR.ROW_OPERATION);
    },// This is called during serialization
    saveExtraState: function () {
      let saved_inputs = this.inputList.map(x => x.name).filter(x => x.startsWith("contents_"))
      __logger(saved_inputs);
      return { data: saved_inputs };
    },

    loadExtraState: function (state) {
      if (state && state.data !== undefined && state.data.length > 0) {
        __logger("Loading with state")
        populate_inputs(this, state.data);
        __logger(this.inputList.map(x => x.name));
      } else {
        __logger("Loading with no state");
        append_value_input_row_comb(this);
      }
    }
  };

  Blockly.Blocks[ns(BC.TYPE.ROW_NOT_OPERATION)] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['not', 'not']]
        ), "operation")
      this.appendValueInput("contents_fix")
        .setCheck(["filter", "row"]);
      this.setOutput(true, "row");
      this.setColour(BC.COLOR.ROW_OPERATION);
    }
  };

  Blockly.Blocks[ns(BC.TYPE.SET_COMB_OPERATION)] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['intersect', 'intersect'], ['union', 'union']]
        ), "operation");
      this.setOutput(true, "set");
      this.setColour(BC.COLOR.SET_OPERATION);
    },// This is called during serialization
    saveExtraState: function () {
      let saved_inputs = this.inputList.map(x => x.name).filter(x => x.startsWith("contents_"))
      __logger(saved_inputs);
      return { data: saved_inputs };
    },

    loadExtraState: function (state) {
      if (state && state.data !== undefined && state.data.length > 0) {
        __logger("Loading with state")
        populate_inputs(this, state.data);
        __logger(this.inputList.map(x => x.name));
      } else {
        __logger("Loading with no state");
        append_value_input_set_comb(this);
      }
    }
  };

  Blockly.Blocks[ns(BC.TYPE.SET_COMPLEMENT_OPERATION)] = {
    init: function () {
      this.appendDummyInput("header")
        .appendField(new Blockly.FieldDropdown(
          [['complement', 'complement']]
        ), "operation")
      this.appendValueInput("contents_fix")
        .setCheck(["set", "filter", "row"]);
      this.setOutput(true, "set");
      this.setColour(BC.COLOR.SET_OPERATION);
    }
  };

  Blockly.Blocks[ns(BC.TYPE.SUBJECT_FILTER)] = {
    init: function () {
      this.appendDummyInput('label')
        .appendField('Subject Filter');
      this.appendValueInput("content");
      this.setColour(BC.COLOR.SUBJECT_FILTER);
      this.is_top_subject = true;
    }
  };

  let color_step = Math.floor((BC.COLOR.DATASET_RANGE_MAX - BC.COLOR.DATASET_RANGE_MIN) / selected_datasets.length);
  let current_color = (BC.COLOR.DATASET_RANGE_MIN - color_step);

  for (let dataset of selected_datasets) {
    current_color = current_color + color_step;
    const dataset_color = current_color;
    const dataset_name = dataset["name"];
    const dataset_type = get_block_dataset_type(dataset_name);
    const dataset_category = {
      kind: 'category',
      name: dataset_name,
      contents: [],
    };

    if (!skip_dataset_filters) {
      Blockly.Blocks[ns(dataset_type)] = {
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
          type: ns(dataset_type)
        }
      );

      json_generator.forBlock[ns(dataset_type)] = dataset_filter_generator;
    }
    

    __logger(toolbox);

    for (let variable of dataset["variables"]) {
      const variable_name = variable["name"];
      const nsed_variable_type = ns(get_block_filter_type(dataset_name, variable_name));
      const variable_label = typeof (variable["label"]) === "string" ? variable["label"] : "";
      const kind = variable["kind"];
      const block_color = dataset_color; // Otherwise it takes the value of dataset_color from the outer closure
      const variable_na_label = "NA(" + variable["NA_count"] + "):";

      if (kind === "categorical") {
        const values = variable.value;
        let dd_options = [];
        for (let i = 0; i < values.length; ++i) {
          dd_options.push([values[i], values[i]])
        }

        if (dd_options.length == 0) dd_options = [['_EMPTY_VEC_', '_EMPTY_VEC_']]

        Blockly.Blocks[nsed_variable_type] = {
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

        json_generator.forBlock[nsed_variable_type] = filter_generator_subset;

      } else if (kind === "numerical") {
        const min = variable["min"];
        const max = variable["max"];

        Blockly.Blocks[nsed_variable_type] = {
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
        json_generator.forBlock[nsed_variable_type] = filter_generator_range;
      } else if (kind === "date") {        
        const min = variable["min"];
        const max = variable["max"];

        Blockly.Blocks[nsed_variable_type] = {
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
        json_generator.forBlock[nsed_variable_type] = filter_generator_date_range;
      } else {        
        // console.warn("Unknown kind variable: " + variable_name);
        continue; 
      }

      let variable_block = {
        kind: 'block',
        type: nsed_variable_type
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
        root_block.getParent().type !== ns(BC.TYPE.SET_COMB_OPERATION) &&
        root_block.getParent().type !== ns(BC.TYPE.SET_COMPLEMENT_OPERATION) &&
        root_block.getParent().type !== ns(BC.TYPE.SUBJECT_FILTER)
      ) {
        root_block = root_block.getParent();
      }

      // Only check if the top parent is a dataset

      if (
        root_block.is_top_dataset ||
        root_block.type === ns(BC.TYPE.ROW_COMB_OPERATION) ||
        root_block.type === ns(BC.TYPE.ROW_NOT_OPERATION)
      ) {

        // If a dataset filter is on the top we take set the target dataset as that one
        // Otherwise we set it inside the loop. It is set to the first block that we find that has
        // a dataset_name property.
        // We travel the whole tree this is can be optimized but as trees should not be too deep it is left as is.

        let dataset_name = root_block.is_top_dataset ? root_block.dataset_name : null;
        __logger("Before loop: " + dataset_name)
        let stack = [];
        stack.push(root_block);
        while (stack.length) {
          let b = stack.pop();
          if (b.dataset_name && dataset_name === null) {
            dataset_name = b.dataset_name
            __logger("Set in loop: " + dataset_name)
          }
          __logger("b.dataset_name: " + b.dataset_name)
          if (b.dataset_name && b.dataset_name !== dataset_name) {
            __logger("Incorrect piece in stack");
            current_block.unplug();
            break;
          }
          stack.push(...b.getChildren());
        }
      }

      if (new_parent_block && new_parent_block.type === ns(BC.TYPE.ROW_COMB_OPERATION)) {
        remove_empty_inputs(new_parent_block);
        append_value_input_row_comb(new_parent_block);
      }

      if (new_parent_block && new_parent_block.type === ns(BC.TYPE.SET_COMB_OPERATION)) {
        remove_empty_inputs(new_parent_block);
        append_value_input_set_comb(new_parent_block);
      }
    } else if (event.reason.includes("disconnect")) {
      if (old_parent_block && (old_parent_block.type === ns(BC.TYPE.ROW_COMB_OPERATION) || old_parent_block.type === ns(BC.TYPE.SET_COMB_OPERATION))) {
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

  if(blockly_disposal[id]){
    blockly_disposal[id];
    blockly_disposal[id] = undefined;
  }  
  let ws = Blockly.inject(container_div, options);
  blockly_disposal[id] = function(){ws.disposal();}

  ws.MAX_UNDO = 0; //Disconnect undo because of listeners
  // When removing elements using JS the undo is messed up
  // For example when removing an element from an operation the id of the input changes
  // and redo cannot happen because we cannot reconnect as the previous input id is
  // stored in the undo stack. No control is provided over the undo stack.

  ws.addChangeListener(onChange);

  let filter_state, filter_state_log;
  [filter_state, filter_state_log] = filter_state_to_blockly_state(init_state, selected_datasets, ns);

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

let blockly_disposal = {}; // FIXME: This is a very ugly way of disposing the workspace but less resistance route currently

let blockly_static_init = function(blockly_root_el, id) {

  let show_button_id = id + "-blockly-filter-checkbox"; // Namespaced button

  let show_label = document.createElement('label');
  show_label.textContent = "Show filter";
  show_label.setAttribute("for", show_button_id);
  show_label.className = "btn btn-primary";

  let show_checkbox = document.createElement("input");
  show_checkbox.type = "checkbox";
  show_checkbox.id = show_button_id;
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
  
  let button_container = document.createElement("div");
  button_container.classList= "d-inline-flex justify-content-center"  ;

  let gen_code_button = document.createElement("button");
  gen_code_button.type = "button";
  gen_code_button.className = "btn btn-primary btn-lg m-2";
  gen_code_button.textContent = "Apply Filter";

  let hide_label = document.createElement('label');
  hide_label.textContent = "Close Filter";
  hide_label.setAttribute("for", show_button_id);
  hide_label.className = "btn btn-primary btn-lg m-2";

  button_container.appendChild(gen_code_button);
  button_container.appendChild(hide_label);

  outer_filter_el.appendChild(inner_filter_el);
  outer_filter_el.appendChild(button_container);


  modal.appendChild(title);
  modal.appendChild(outer_filter_el);

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
    const event = new CustomEvent(FC.EVENT.NEW_FILTER_VALUE, {
      detail: {filter: code, mode: FC.MODE.BLOCKLY},
      bubbles: true,
      cancelable: true
    });
    blockly_root_el.dispatchEvent(event);
  };

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
  TAG: {
    DATASET_FILTER : "dv-filter-dataset-filter",
    VARIABLE_FILTER_CONTAINER : "dv-filter-variable-filter-container",
    VARIABLE_FILTER: "dv-filter-variable-filter",
    FILTER_COUNT_TAG: "dv-filter-count-tag",
    ROW_COUNT_TAG: "dv-filter-row-count-tag",
    INCOMPATIBLE_WARNING_ELEMENT: "dv-incompatible-warning"
  },
  EVENTS: {
    CHANGED_FILTER: 'dv_filter:changed'
  },
  ATTRIBUTE: {
    DATASET_NAME: "data-dataset-name",
    SUBJECT_FILTER: 'data-subject-filter',
    VARIABLE: "data-variable",
    KIND: "data-kind",
    FILTER_CONTROL_CONTAINER: "data-filter-control",
    VARIABLE_SELECTOR: "variable_selector",
    NA_CONTROL: "na_control",
    FILTER_VALUE: "filter_value"
  },
  VARIABLE: {
    NUMERICAL: "numerical",
    CATEGORICAL: "categorical",
    DATE: "date"
  },
  CLASS_ICON: {    
    "numeric": "sort-by-order",
    "integer": "sort-by-order",
    "Date": "calendar",
    "POSIXct": "calendar",
    "POSIXlt": "calendar",
    "ordered": "sort-by-attributes",
    "factor": "tags",
    "character": "font",
    "logical": "ok-circle",
    "unknown": "question-sign"    
  },
  PROPERTY: {
    STATE_OVERRIDE: "state_override"
  }
}

let get_simple_root_el = function(el){
  __assert(()=>is_html_element(el))
  return(get_root_el(el).querySelector(`${FC.TAG.FILTER}[${FC.ATTRIBUTE.FILTER_MODE}="${FC.MODE.SIMPLE}"]`));
}

// Returns a simplified filter state or null if the filter_state is not compatible with the simple filter
// This can happen because the simple filter can express a subset of the concepts available in the blockly one
let simplify_filter_state = function(state, subject_dataset_name) {

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
          __logger("At least one child is not of kind filter or does not belong to the correct dataset")
        }
        compatible = compatible && ok_all_child;
        simple_state = state.children[0].children;
      } else {
        let filter_and_correct_dataset = state.children[0].kind === "filter" && state.children[0].dataset === dataset_name;
        compatible = compatible && filter_and_correct_dataset;
        if (!filter_and_correct_dataset) {
          __logger("First child is not an `and` operation or a single filter with correct dataset")
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

let create_dataset_filter = function(simple_root_el, dataset, dataset_filter_state, is_subject_filter) {
  __time_function_start() 
  __assert(()=>is_html_element(simple_root_el));
  __assert(() => Array.isArray(dataset_filter_state));
  __assert(()=> !simple_root_el.querySelector(`${SC.TAG.DATASET_FILTER}[${SC.ATTRIBUTE.DATASET_NAME} = '${dataset.name}']`));
  
  let selected_variables = [];  
  for(let i = 0; i < dataset_filter_state.length; ++i) {
    selected_variables.push(dataset_filter_state[i].variable)
  }  
  
  __logger("Creating UI for " + dataset.name);  
  
  let dataset_filter_container = document.createElement(SC.TAG.DATASET_FILTER);
  dataset_filter_container.className = "card border-primary mb-2";
  dataset_filter_container.setAttribute(SC.ATTRIBUTE.DATASET_NAME, dataset.name);
  dataset_filter_container.setAttribute(SC.ATTRIBUTE.SUBJECT_FILTER, is_subject_filter);

  let card_heading = document.createElement("div");
  card_heading.className = "card-header bg-primary text-white dv-data-filter-header"; 
  
  let title_tag_container = document.createElement("h6");  
  title_tag_container.className = "card-title mb-0 dv-title-tag ";  

  let card_collapse_link = document.createElement("a");
  card_collapse_link.textContent = dataset.name;
  card_collapse_link.className = "text-white text-decoration-none";
  card_collapse_link.setAttribute("data-bs-toggle", "collapse");  
  card_collapse_link.setAttribute("data-bs-target", `${SC.TAG.DATASET_FILTER}[${SC.ATTRIBUTE.DATASET_NAME}=${dataset.name}] .card-body`);
  card_collapse_link.href = "#"; // Recommended to make it keyboard-accessible

  title_tag_container.appendChild(card_collapse_link);  

  let filter_count_tag = document.createElement(SC.TAG.FILTER_COUNT_TAG);
  title_tag_container.appendChild(filter_count_tag);

  let row_count_tag = document.createElement(SC.TAG.ROW_COUNT_TAG);
  row_count_tag.className = "badge bg-light";
  title_tag_container.appendChild(row_count_tag);

  card_heading.appendChild(title_tag_container);

  if (is_subject_filter) {
    let icon = document.createElement("span");
    icon.className = "glyphicon glyphicon-user";  
    card_heading.appendChild(document.createTextNode(" "));
    card_heading.appendChild(icon);
  }

  let add_button = document.createElement("button");
  add_button.className = "btn btn-outline-light btn-sm";
  add_button.className = "btn btn-outline-light btn-sm";

  let add_icon = document.createElement("i");
  add_icon.className = "glyphicon glyphicon-plus";
  add_button.appendChild(add_icon);

  card_heading.appendChild(add_button);

  
  dataset_filter_container.appendChild(card_heading);

  let card_body = document.createElement("div");
  card_body.className = 'card-body collapse show p-1';

  let select = document.createElement('select');
  select.className = 'selectpicker dv-hide';
  select.setAttribute('multiple', '');
  select.setAttribute('title', 'Add / Remove Filters');
  select.setAttribute('data-live-search', 'true');
  select.setAttribute('data-width', '100%');
  select.setAttribute('data-style', 'btn');
  select.setAttribute('data-selected-text-format', 'static');
  let data_container;
  if(document.querySelector(".dv_main_panel")) {
    data_container = 'body .dv_main_panel'     
  } else {
    data_container = 'body'
  }
  select.setAttribute('data-container', data_container);
  select.setAttribute(SC.ATTRIBUTE.VARIABLE_SELECTOR, '');

  for(let i = 0; i < dataset.variables.length; ++i) {
    let option = document.createElement('option');
    option.value = dataset.variables[i].name;
    let disabled;
    if (dataset.variables[i].kind === "unknown") {
      option.setAttribute("disabled", "");            
    }

    option.setAttribute('data-content', `
      <span class="glyphicon glyphicon-${SC.CLASS_ICON[dataset.variables[i].class]}"></span>
      ${dataset.variables[i].name}
      <code style="color:darkblue;">${dataset.variables[i].class}</code>
      ${dataset.variables[i].NA_count>0?`<small style="color:darkred;">(${dataset.variables[i].NA_count} missing)</small>` : ""}      
      </br>
      <small class="text-muted">${dataset.variables[i].label}</small>
    `);
    option.setAttribute('data-subtext', `Description for`);    
    if(selected_variables.includes(option.value)) {
      option.setAttribute("selected", "");
    }
    select.appendChild(option);
  }
 
  
  card_body.appendChild(select);
  
  let variable_filter_control_container = document.createElement(SC.TAG.VARIABLE_FILTER_CONTAINER);
  card_body.appendChild(variable_filter_control_container);
  
  dataset_filter_container.appendChild(card_body);
  simple_root_el.appendChild(dataset_filter_container);
    
  $(select).selectpicker();

  create_variable_filter_controls(variable_filter_control_container, dataset, selected_variables,  dataset_filter_state);
  __time_function_end() 
}

let destroy_dataset_filter = function(dataset_el) {
  __time_function_start() 
  __assert(()=>is_html_element(dataset_el))
  __assert(()=> dataset_el.hasAttribute(SC.ATTRIBUTE.DATASET_NAME));

  let variable_filter_control_container = dataset_el.querySelector(SC.TAG.VARIABLE_FILTER_CONTAINER);
  destroy_variable_filter_controls(variable_filter_control_container);

  let select = dataset_el.querySelector(`select[${SC.ATTRIBUTE.VARIABLE_SELECTOR}]`);
  if(!$(select).data('selectpicker')) {
    throw new Error("Attempt to destroy non selectpicker element");
  }
  $(select).selectpicker("destroy");

  dataset_el.remove();
  __time_function_end() 
}

let destroy_variable_filter_controls = function(variable_filter_control_container_el) {
  __time_function_start()  
  __assert(()=> variable_filter_control_container_el.tagName.toLowerCase() === SC.TAG.VARIABLE_FILTER_CONTAINER);

  let select_pickers_to_destroy = variable_filter_control_container_el.querySelectorAll(`${SC.TAG.VARIABLE_FILTER}[${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.CATEGORICAL}'] [${SC.ATTRIBUTE.FILTER_VALUE}] select`);
  __logger("Destroying: " + select_pickers_to_destroy.length + " selectpickers");
  for(let i = 0; i < select_pickers_to_destroy.length; ++i) {
    if(!$(select_pickers_to_destroy[i]).data('selectpicker')) {
      throw new Error("Attempt to destroy non selectpicker element");
    }
    $(select_pickers_to_destroy[i]).selectpicker("destroy");
  }

  let ion_range_slider_to_destroy = variable_filter_control_container_el.querySelectorAll(`${SC.TAG.VARIABLE_FILTER}[${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.NUMERICAL}'] [${SC.ATTRIBUTE.FILTER_VALUE}] input`);
  __logger("Destroying: " + ion_range_slider_to_destroy.length + " ion.range.sliders");
  for(let i = 0; i < ion_range_slider_to_destroy.length; ++i) {
    if(!$(ion_range_slider_to_destroy[i]).data('ionRangeSlider')) {
      throw new Error("Attempt to destroy non ionRangeSlider element");
    }
    $(ion_range_slider_to_destroy[i]).data("ionRangeSlider").destroy();
  }

  let date_range_to_destroy = variable_filter_control_container_el.querySelectorAll(`${SC.TAG.VARIABLE_FILTER}[${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.DATE}'] [${SC.ATTRIBUTE.FILTER_VALUE}] input`);
  __logger("Destroying: " + date_range_to_destroy.length + " date pickers");
  for(let i = 0; i < date_range_to_destroy.length; ++i) {
    if(!$(date_range_to_destroy[i]).data('datepicker')) {
      throw new Error("Attempt to destroy non datepicker element");
    }
    $(date_range_to_destroy[i]).bsDatepicker("destroy");
  }

  variable_filter_control_container_el.innerHTML='';
  __time_function_end()
}

let create_variable_filter_controls = function(variable_filter_control_container_el, dataset, selected_variables, dataset_filter_state) {

  __time_function_start();
  __assert(() => Array.isArray(dataset_filter_state));
  __assert(()=>is_html_element(variable_filter_control_container_el));
  __assert(()=> variable_filter_control_container_el.tagName.toLowerCase() === SC.TAG.VARIABLE_FILTER_CONTAINER);

  let count_tag = variable_filter_control_container_el.closest(SC.TAG.DATASET_FILTER).querySelector(SC.TAG.FILTER_COUNT_TAG);

  if(selected_variables.length > 0) {
    count_tag.textContent = selected_variables.length;
    count_tag.className = "badge bg-light";
  } else {
    count_tag.textContent = "";
    count_tag.className = "dv-hide";
  }

  for(let i = 0; i < selected_variables.length; ++i) {      
    let current_variable = dataset.variables.find((obj)=> obj.name===selected_variables[i]);
    let current_state = dataset_filter_state.find((obj)=> obj.variable===current_variable.name);

    // #region common header
    let container = document.createElement(SC.TAG.VARIABLE_FILTER);
    container.setAttribute(SC.ATTRIBUTE.VARIABLE, current_variable.name);
    container.setAttribute(SC.ATTRIBUTE.KIND, current_variable.kind);    

    let header = document.createElement("div");
    header.className = "variable-header";
    
    let name_label = document.createElement("span");
    name_label.className = "badge text-bg-primary"; // styled like a pill
    name_label.textContent = current_variable.label !== "" ? current_variable.label : current_variable.name;
    
    let na_group = document.createElement("div");
    na_group.className = `na-control ${current_variable.NA_count > 0 ? "na_warning" : ""}`;
    na_group.setAttribute(SC.ATTRIBUTE.NA_CONTROL, "");
    
    let na_label = document.createElement("span");
    na_label.className = "na-label";
    na_label.textContent = `NA: ${current_variable.NA_count}`;
    na_group.appendChild(na_label);

    let na_checkbox_addon = document.createElement("span");
    na_checkbox_addon.className = `na-checkbox ${current_variable.NA_count > 0 ? "" : "dv-hide"}`;
    
    let na_checkbox = document.createElement("input");
    na_checkbox.type = "checkbox";
    
    if (current_state) {
      na_checkbox.checked = current_state.include_NA;
    } else {
      na_checkbox.checked = true;
    }

    na_checkbox_addon.appendChild(na_checkbox);
    na_group.appendChild(na_checkbox_addon);

    if(current_variable.NA_count > 0) {
     
    }
    
    let close_button = document.createElement("button");
    close_button.type = "button";
    close_button.className = "close-btn";
    close_button.setAttribute("data-action", "remove");
    close_button.innerHTML = "&times;";
    
    // Assemble header
    header.appendChild(name_label);
    header.appendChild(na_group);
    header.appendChild(close_button);

    header.appendChild(name_label);
    header.appendChild(na_group);
    header.appendChild(close_button);
    container.appendChild(header);

    //#endregion

    if(current_variable.kind === SC.VARIABLE.CATEGORICAL) {
      let categorical_select = document.createElement('select');
      categorical_select.className = 'selectpicker';
      categorical_select.setAttribute('multiple', '');        
      categorical_select.setAttribute('data-live-search', 'true');
      categorical_select.setAttribute('data-actions-box', 'true');
      categorical_select.setAttribute('data-container', 'body .dv_main_panel');
      categorical_select.setAttribute('data-width', '100%');
      categorical_select.setAttribute(SC.ATTRIBUTE.FILTER_VALUE, '');

      let value = current_variable.value;
      let count = current_variable.value;
      __assert(()=>count.every((v, i, a) => i === 0 || a[i-1] >= v)) // Check is sorted
      __assert(()=>value.length === count.length) // Check is sorted

      for(let i = 0; i < value.length; ++i) {
        let option = document.createElement('option');
        option.value = value[i];
        option.textContent = value[i];
        option.setAttribute("data-subtext", `${count[i]} / ${dataset.nrow}`);
        if(current_state) {
          if(current_state.values.includes(option.value)) {
            option.setAttribute("selected", '');         
          }
        } else {
          option.setAttribute("selected", '');          
        }
        categorical_select.appendChild(option);
      }

      container.appendChild(categorical_select);
      variable_filter_control_container_el.appendChild(container);
      $(categorical_select).selectpicker();
                  
    } else if (current_variable.kind === SC.VARIABLE.DATE) {

      let from;
      let to;      

      if(current_state) {
        from = max_str_date(current_state.min, current_variable.min);
        to = max_str_date(current_state.max, current_variable.max);
      } else {
        from = current_variable.min;
        to = current_variable.max;
      }

      let date_group = document.createElement("div");
      date_group.className = "input-daterange input-group";
      date_group.setAttribute(SC.ATTRIBUTE.FILTER_VALUE, '');

      // from input
      let from_control = document.createElement("input");
      from_control.type = "text";
      from_control.value = from;
      from_control.className = "form-control";

      // separator
      let to_separator = document.createElement("span");
      to_separator.className = "input-group-addon";
      to_separator.textContent = "to";

      // to input
      let to_control = document.createElement("input");
      to_control.type = "text";
      to_control.value = to;
      to_control.className = "form-control";

      // assemble
      date_group.appendChild(from_control);
      date_group.appendChild(to_separator);
      date_group.appendChild(to_control);

      container.appendChild(date_group);
      variable_filter_control_container_el.appendChild(container);

      $(date_group).bsDatepicker({
        format: "yyyy-mm-dd",
        autoclose: true,
        startDate: current_variable.min,
        endDate: current_variable.max
      });      
    } else if (current_variable.kind === SC.VARIABLE.NUMERICAL) {

      let numeric_finite_max_and_min = is_numeric_finite(current_variable.min) && is_numeric_finite(current_variable.max);
      
      if(numeric_finite_max_and_min) {
        const MAGIC_NEGATIVE_MARGIN = -25;  // This is the distance between of the ion.range.slider top and the slider line
      const histogram_container = document.createElement("div");
      histogram_container.className = "histogram";
      histogram_container.style = `display:flex; align-items:flex-end; margin-bottom: ${MAGIC_NEGATIVE_MARGIN}px;`
      const density = current_variable.density;
      // Find max density to scale heights
      const max_density = Math.max(...density);
      // Draw bars
      density.forEach(d => {
        const bar = document.createElement("div");
        bar.style.flex = "1";               // equal width
        bar.style.marginRight = "2px";      // spacing between bars
        bar.style.backgroundColor = "DodgerBlue";        
        bar.style.height = (d / max_density) * 25 + "px"; // scale height
        histogram_container.appendChild(bar);
      });
      container.appendChild(histogram_container);      

      let numerical_input = document.createElement("input");
      numerical_input.setAttribute(SC.ATTRIBUTE.FILTER_VALUE, '');      
      container.appendChild(numerical_input);

      variable_filter_control_container_el.appendChild(container);

      let from;
      let to;
      if(current_state) {
        from = Math.max(current_state.min, current_variable.min);
        to = Math.min(current_state.max, current_variable.max);
      } else {
        from = current_variable.min;
        to = current_variable.max;
      }

      $(numerical_input).ionRangeSlider({
          min: current_variable.min,
          max: current_variable.max,
          type: "double",
          from: from,
          to: to,
          skin: "shiny",
          grid: "true",
          onFinish: function () {$(numerical_input).trigger("finished.ion.range.slider");}
      });
      } else {
        let numerical_input = document.createElement("input");
        numerical_input.setAttribute(SC.ATTRIBUTE.FILTER_VALUE, '');      
        container.appendChild(numerical_input);  
        variable_filter_control_container_el.appendChild(container);
  
        $(numerical_input).ionRangeSlider({
            min: 0,
            max: 0,
            type: "double",
            from: 0,
            to: 0,
            skin: "shiny",
            grid: "true",            
            disable: true
        });

      }
    } else {
      let fallback_content = document.createElement("p");
      fallback_content.textContent = `${current_variable.name} - ${current_variable.kind}`;
      container.appendChild(fallback_content);
      variable_filter_control_container_el.appendChild(container);
    }  
  };
  __time_function_end();
}

// Creates the variable selector for each of the datasets
// Container_el is the parent container in which the container for all the selectors will be created. We look for the container itself
// Only called on from the simple dynamic init
let update_dataset_filter = function(simple_root_el, dataset, dataset_filter_state, is_subject_filter) {
  __time_function_start();
  __assert(()=>is_html_element(simple_root_el))
  __assert(() => Array.isArray(dataset_filter_state));

  let prev_dataset_filter_el = simple_root_el.querySelector(`${SC.TAG.DATASET_FILTER}[${SC.ATTRIBUTE.DATASET_NAME} = '${dataset.name}']`);

  if(prev_dataset_filter_el) {
    destroy_dataset_filter(prev_dataset_filter_el);    
  }
  create_dataset_filter(simple_root_el, dataset, dataset_filter_state, is_subject_filter);
  __time_function_end();
};

// Creates each of the selected variable filters
// Container_el is the container itself where each of the container, it is the container itself
let update_filter_controls = function(variable_filter_control_container_el, dataset, selected_variables, dataset_filter_state) {
  __time_function_start();
  destroy_variable_filter_controls(variable_filter_control_container_el);
  create_variable_filter_controls(variable_filter_control_container_el, dataset, selected_variables, dataset_filter_state);
  __time_function_end();
};

// Gets the state of an specific dataset
let get_single_dataset_filter_state = function (dataset_container_el) {
  __time_function_start();
  let filter_state = [];
  let dataset_name = dataset_container_el.getAttribute(SC.ATTRIBUTE.DATASET_NAME);
  let variable_selector_els = dataset_container_el.querySelectorAll(`${SC.TAG.VARIABLE_FILTER}`);

  for (let i = 0; i < variable_selector_els.length; ++i) {
    let current_variable_el = variable_selector_els[i];
    let kind = current_variable_el.getAttribute(SC.ATTRIBUTE.KIND);
    let variable_name = current_variable_el.getAttribute(SC.ATTRIBUTE.VARIABLE);

    let curr_filter;

    let include_NA = current_variable_el.querySelector(`[${SC.ATTRIBUTE.NA_CONTROL}] input`).checked;

    // Find the select inside that parent
    if (kind === SC.VARIABLE.CATEGORICAL) {

      let select = current_variable_el.querySelector(`[${SC.ATTRIBUTE.FILTER_VALUE}]`);
      if (!select) {
        throw new Error(`No select element found inside: ${current_variable_el.outerHTML}`);
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

      let input = current_variable_el.querySelectorAll(`[${SC.ATTRIBUTE.FILTER_VALUE}] input`);
      if (!input || input.length !=2) {
        throw new Error(`No 2 input elements found inside: ${current_variable_el.outerHTML}`);
      }

      let from = $(input[0]).val();
      let to = $(input[1]).val();

      curr_filter = {
        kind: "filter",
        dataset: dataset_name,
        operation: "select_date",
        variable: variable_name,
        min: from,
        max: to,
        include_NA: include_NA
      }

    } else if (kind === SC.VARIABLE.NUMERICAL) {

      let input = current_variable_el.querySelector(`[${SC.ATTRIBUTE.FILTER_VALUE}]`);
      if (!input) {
        throw new Error(`No input element found inside: ${current_variable_el.outerHTML}`);
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
  __time_function_end() 
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
  __assert(()=>is_html_element(simple_root_el))
  __time_function_start()

  if(simple_root_el[SC.PROPERTY.STATE_OVERRIDE]!== undefined) {
    return(simple_root_el[SC.PROPERTY.STATE_OVERRIDE]);
  }

  let subject_div = simple_root_el.querySelector(`${SC.TAG.DATASET_FILTER}[${SC.ATTRIBUTE.SUBJECT_FILTER}=true]`);
  let other_div = simple_root_el.querySelectorAll(`${SC.TAG.DATASET_FILTER}[${SC.ATTRIBUTE.SUBJECT_FILTER}=false]`);

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

  __logger(state);
  __time_function_end() 
  return (state);
};

// Handles actions buttons. For not it is only one, maybe it is an unrequired generalization
let handle_action = function() {
    
  let handlers = {
    remove: function(el) {
      __assert(()=>is_html_element(el))
      const variable_to_be_removed = el.closest(SC.TAG.VARIABLE_FILTER).getAttribute(SC.ATTRIBUTE.VARIABLE); 
      const select = el.closest(SC.TAG.DATASET_FILTER).querySelector("select");
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

// Initialize all listeners, no listener should happen outside here, or in shiny handlers
let simple_static_init = function(simple_root_el) {
  __time_function_start();
  __assert(()=>is_html_element(simple_root_el))

  let card_collapse_link = document.createElement("a");
  card_collapse_link.textContent = "Show/Hide all";  
  card_collapse_link.href = "#"; // Recommended to make it keyboard-accessible

  simple_root_el.appendChild(card_collapse_link);
  
  let send_code = function() {
    __logger("Simple sending code");
    let dataset_list_name = get_filter_property(simple_root_el, FC.PROPERTY.DATASET_LIST_NAME);
    let code = get_filter_state(simple_root_el, dataset_list_name);
    const new_event = new CustomEvent(FC.EVENT.NEW_FILTER_VALUE, {
      detail: {filter: code, mode: FC.MODE.SIMPLE},
      bubbles: true,
      cancelable: true
    });
    simple_root_el.dispatchEvent(new_event);
  }

  card_collapse_link.addEventListener("click", function() {
    let dataset_filter_cards_body = simple_root_el.querySelectorAll(".card-body.collapse");
    let opened_dataset_filter_bodies = simple_root_el.querySelectorAll(".card-body.collapse.show");

    let show_or_hide_method;
    if((opened_dataset_filter_bodies.length>0 && opened_dataset_filter_bodies.length < dataset_filter_cards_body.length) || opened_dataset_filter_bodies.length == 0) {
      show_or_hide_method = "show";
    } else {
      show_or_hide_method = "hide";
    }

    for(let i = 0; i < dataset_filter_cards_body.length; ++i){    
      bootstrap.Collapse.getOrCreateInstance(dataset_filter_cards_body[i])[show_or_hide_method]();
    }
  }
);

  $(simple_root_el).on('click', `${SC.TAG.DATASET_FILTER} .dv-data-filter-header button`, function(event) {
    let select = event.target.closest(SC.TAG.DATASET_FILTER).querySelector("select");
    let filter_body_el = event.target.closest(`${SC.TAG.DATASET_FILTER}`).querySelector(".card-body");
    let filter_body_collapse_instance = bootstrap.Collapse.getInstance(filter_body_el);
    if (!filter_body_collapse_instance) {
      filter_body_collapse_instance = new bootstrap.Collapse(filter_body_el, { toggle: false });
    }
    filter_body_collapse_instance.show();
    $(select).selectpicker('toggle');
  });

  $(simple_root_el).on('changed.bs.select', `${SC.TAG.DATASET_FILTER} select[${SC.ATTRIBUTE.VARIABLE_SELECTOR}]`, function(event) {
    let dataset_div = event.target.closest(`${SC.TAG.DATASET_FILTER}`);
    let dataset_name = dataset_div.getAttribute(SC.ATTRIBUTE.DATASET_NAME);
    let dataset_list_name = get_filter_property(simple_root_el, FC.PROPERTY.DATASET_LIST_NAME);

    let current_dataset_list = get_filter_property(simple_root_el, FC.PROPERTY.DATA).dataset_lists.find(obj=>obj.name === dataset_list_name);
    let dataset = current_dataset_list.dataset_list.find(obj=>obj.name === dataset_name);

    let selected_variables = $(event.target).val();

    let dataset_control_div = dataset_div.querySelector(SC.TAG.VARIABLE_FILTER_CONTAINER);
    let dataset_filter_state = simplify_filter_state(get_filter_state(get_simple_root_el(dataset_div), dataset_list_name), get_filter_property(simple_root_el, FC.PROPERTY.SUBJECT_DATASET_NAME)).state[dataset_name]  ?? []; //FIXME: loiuhb who is reponsible for this is not well defined

    update_filter_controls(dataset_control_div, dataset, selected_variables, dataset_filter_state);
    dispatch_simple_filter_changed(event);
  });

  $(simple_root_el).on("changed.bs.select", `${SC.TAG.VARIABLE_FILTER}[${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.CATEGORICAL}'] select`, dispatch_simple_filter_changed);
  $(simple_root_el).on("finished.ion.range.slider", `${SC.TAG.VARIABLE_FILTER}[${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.NUMERICAL}'] input`, dispatch_simple_filter_changed);
  $(simple_root_el).on("changeDate", `${SC.TAG.VARIABLE_FILTER}[${SC.ATTRIBUTE.KIND}='${SC.VARIABLE.DATE}'] input`, dispatch_simple_filter_changed);
  $(simple_root_el).on("change", `${SC.TAG.VARIABLE_FILTER} [${SC.ATTRIBUTE.NA_CONTROL}] input`, dispatch_simple_filter_changed);

  //TODO: Consider debounce
  simple_root_el.addEventListener(SC.EVENTS.CHANGED_FILTER, send_code);  
  $(simple_root_el).on('click', "[data-action]", handle_action);

  let res = {
    send_code: send_code
  }
  __time_function_end();
  return(res);
}

// Handles the changes of datasets
let simple_dynamic_init = function(simple_root_el, filter_data, subject_dataset_name, filter_state) {
  __assert(()=>is_html_element(simple_root_el))
  __time_function_start();
  // Subject filter

  let simple_filter_state = simplify_filter_state(filter_state, subject_dataset_name);
  let subject_dataset = filter_data.dataset_list.find(obj=>obj.name === subject_dataset_name);
  let other_datasets = filter_data.dataset_list.filter(obj=>obj.name !== subject_dataset_name);  

  if(!simple_filter_state.compatible) {
    __logger("State not compatible");
    simple_root_el.classList.add("dv-disabled-controls");
    simple_root_el[SC.PROPERTY.STATE_OVERRIDE] = filter_state;
    simple_filter_state.state = {};

    let warning_element = document.createElement(SC.TAG.INCOMPATIBLE_WARNING_ELEMENT);
    warning_element.innerHTML = "&#9888; Filter state cannot be represented";
    warning_element.classList = "alert alert-warning mb-3 border-1 rounded";
    simple_root_el.prepend(warning_element);
  } else {
    __logger("State compatible");    
    simple_root_el.classList.remove("dv-disabled-controls");    
    simple_root_el[SC.PROPERTY.STATE_OVERRIDE] = undefined;   
    simple_root_el.querySelector(SC.TAG.INCOMPATIBLE_WARNING_ELEMENT)?.remove();
  }

  update_dataset_filter(
      simple_root_el,
      subject_dataset,
      simple_filter_state.state[subject_dataset_name] ?? [], //FIXME: loiuhb who is reponsible for this is not well defined
      true
    );

  for (let i = 0; i < other_datasets.length; ++i) {
    update_dataset_filter(
      simple_root_el,
      other_datasets[i],
      simple_filter_state.state[other_datasets[i].name] ?? [], //FIXME: loiuhb who is reponsible for this is not well defined
      false
    )
  }
  
  __time_function_end();
}


//#endregion


//#region General init

let get_blockly_root_el = function(el){  
  __assert(()=>is_html_element(el))
  return(get_root_el(el).querySelector(`${FC.TAG.FILTER}[${FC.ATTRIBUTE.FILTER_MODE}="${FC.MODE.BLOCKLY}"]`));
}

let init_filter_handler = function (root_el, dataset_list_data, dataset_list_name, subject_filter_dataset_name, filter_state, static_init_ret, selected_mode, skip_dataset_filters) {
  __logger(`init_filter_handler: ${root_el}`);
  __logger(root_el);
  __logger(dataset_list_data);
  __assert(()=>is_html_element(root_el));
  
  let dataset_list = dataset_list_data.dataset_lists.find(obj=>obj.name === dataset_list_name);
  
  if(selected_mode === FC.MODE.SIMPLE) {
    get_simple_root_el(root_el).style.display = 'block';
    get_blockly_root_el(root_el).style.display = 'none';   
    simple_dynamic_init(
      get_simple_root_el(root_el),
      dataset_list,
      subject_filter_dataset_name,
      filter_state
    );
    static_init_ret[FC.MODE.SIMPLE].send_code();
  } else if (selected_mode === FC.MODE.BLOCKLY) {
    get_simple_root_el(root_el).style.display = 'none';
    get_blockly_root_el(root_el).style.display = 'flex';
    get_blockly_root_el(root_el).style.justifyContent = 'space-around';
    blockly_dynamic_init(
      get_blockly_root_el(root_el),   
      dataset_list_name,    
      dataset_list_data, filter_state,
      skip_dataset_filters
    );
    static_init_ret[FC.MODE.BLOCKLY].send_code();
  } else {
    throw new Error("Unknown mode: " + selected_mode);
  }
}

let update_filter_result_handler = function(msg, root_el){

  let parsed_msg = JSON.parse(msg.json)
  console.log(parsed_msg);

  let dataset_list_name = get_filter_property(root_el, FC.PROPERTY.DATASET_LIST_NAME);
  let current_dataset_list = get_filter_property(root_el, FC.PROPERTY.DATA).dataset_lists.find(obj=>obj.name === dataset_list_name);

  
  let row_count = parsed_msg.row_count; 
  for(let idx = 0; idx < row_count.length; ++idx) {
    let name = row_count[idx].name;
    let current_nrow = row_count[idx].count;
    let total_nrow = current_dataset_list.dataset_list.find(obj=>obj.name === name).nrow;

    root_el.querySelector(`${SC.TAG.DATASET_FILTER}[${SC.ATTRIBUTE.DATASET_NAME}=${name}] ${SC.TAG.ROW_COUNT_TAG}`).textContent = `${current_nrow} / ${total_nrow}`;
  }
}

let show_hide_dataset_filters_handler =  function(msg, root_el){
  let dataset_filters = root_el.querySelectorAll(`${SC.TAG.DATASET_FILTER}`);

  for(let i = 0; i < dataset_filters.length; ++i) {
    let current_dataset_name = dataset_filters[i].getAttribute(SC.ATTRIBUTE.DATASET_NAME);
    if (msg.hidden.includes(current_dataset_name)) {
      dataset_filters[i].classList.add("dv-hide");
    } else {
      dataset_filters[i].classList.remove("dv-hide");
    }
  }
}

let blockly_dynamic_init = function(blockly_root_el, dataset_list_name, filter_data, filter_state, skip_dataset_filters) {
  __assert(()=>is_html_element(blockly_root_el))

  let inner_filter_el = blockly_root_el.querySelector(`[${BC.ATTRIBUTE.INNER_FILTER}]`);
  const filter = $(inner_filter_el).data("filter");
  if (filter) {
    filter.workspace.dispose();
    $(inner_filter_el).data("filter", undefined);
  }
  $(inner_filter_el).data('filter', init_blockly(inner_filter_el, dataset_list_name, filter_data, filter_state, skip_dataset_filters));
}

let FC = {
  TAG:{
    ROOT: "dv-filter-root",
    FILTER: "dv-filter-filter",
    CLEAR_ALL_BUTTON: "dv-filter-clear-all-button",
    SAVE_BUTTON: "dv-filter-save-button",
    SAVED_STATES_LIST: "dv-filter-saved-states-list",
    SAVED_STATES_CONTAINER: "dv-filter-saved-states-container",
    SAVED_STATE_BUTTON: "dv-filter-saved-state-button",
    REMOVED_SAVED_STATE_BUTTON: "dv-filter-removed-saved-state-button",
  },
  MODE: {
    SIMPLE: "simple",
    DATASETS: "datasets",
    BLOCKLY: "blockly"
  },
  EVENT: {
    NEW_FILTER_VALUE: "new_filter_value",
    REQUESTED_REDRAW: "requested_redraw"
  },
  ATTRIBUTE: {
    ROOT: "data-root",
    FILTER_MODE: "data-filter-mode",
    STATE_NAME: "state_name",
    SAVED_FILTER_STATE_NAME: "data-saved-filter-name"
  },
  PROPERTY: {
    FIELD: "dv_filter_properties", // Container of all properties
    DATA: "filter_data",
    STATE: "filter_state",
    SAVED_STATES: "saved_states",
    DATASET_LIST_NAME: "dataset_list_name",
    SUBJECT_DATASET_NAME: "subject_dataset_name",
    STATIC_RET: "static_ret",
    FILTER_MODE: "filter_mode",
    SKIP_DATASET_FILTERS: "skip_dataset_filters"
  },
  VAL: {
    EMPTY_FILTER: {
        filters: {
          datasets_filter: {children : [] },
          subject_filter: {children : [] },
          dataset_list_name: ""
        }
      }
  }
}

let get_root_el = function(el) {
  __assert(()=>is_html_element(el))
  let root_el;
  if(el && el.tagName && el.tagName.toLowerCase() === FC.TAG.ROOT) {
    root_el = el;
  } else {
    root_el = el.closest(FC.TAG.ROOT)    
  }
  
  if(!root_el) {
    throw new Error("no root found from" + el);
  }
  return(root_el);
}

let get_root_el_by_id = function(id) {
  return(document.getElementById(id));  
}

// Should be called only inside listeners/message handlers to get the current state of the filter
let get_filter_property = function(el, property, clone = true) {  
  __assert(()=>is_html_element(el));
  __logger("Getting property: " + property);  
  if(clone) {
    return(structuredClone(get_root_el(el)[FC.PROPERTY.FIELD][property]));  
  } else {
    return(get_root_el(el)[FC.PROPERTY.FIELD][property]);  
  }
  
}

// Should be called only inside listeners/message handlers or during main init to set the state of the filter
let set_filter_property = function(el, property, val) {
  __assert(()=>is_html_element(el))
  return(get_root_el(el)[FC.PROPERTY.FIELD][property] = val);
}

let init_filter_property_field = function(el) {  
  __assert(()=>is_html_element(el));
  get_root_el(el)[FC.PROPERTY.FIELD] = {};
}

const init = function(root_id, filter_state_json, saved_filter_states_json, subject_dataset_name, filter_state_json_input_id, saved_filter_state_json_msg_input_id, export_button_id, filter_log_input_id) {  
  let filter_state = JSON.parse(filter_state_json);
  let saved_filter_states = JSON.parse(saved_filter_states_json);

  __logger("Filter shiny id: " + root_id);
  __logger(`Initial filter state:`);
  __logger(filter_state_json);
  __logger(`Initial saved states:`);
  __logger(saved_filter_states);

  let root_el = document.getElementById(root_id);
  __logger("root el for " + root_id);
  __logger(root_el);
  init_filter_property_field(root_el);
  set_filter_property(root_el, FC.PROPERTY.STATE, filter_state);
  set_filter_property(root_el, FC.PROPERTY.SAVED_STATES, !saved_filter_states ? [] : saved_filter_states);
  set_filter_property(root_el, FC.PROPERTY.SUBJECT_DATASET_NAME, subject_dataset_name);
  
  let top_control_container = document.createElement("dv-filter-top-control-container");
  top_control_container.className = "p-3 m-3 bg-light border rounded";

  root_el.appendChild(top_control_container);

  let export_button = document.createElement("a");
  export_button.id = export_button_id;
  export_button.className = "btn btn-primary btn-sm shiny-download-link disabled";
  export_button.setAttribute("href", "");
  export_button.setAttribute("target", "_blank");
  export_button.setAttribute("download", "");
  export_button.setAttribute("tabindex", "-1");
  export_button.setAttribute("title", "Export filter");

  let export_icon = document.createElement("span");
  export_icon.className = "glyphicon glyphicon-export";
  export_button.appendChild(export_icon);


  let clear_all_button = document.createElement(FC.TAG.CLEAR_ALL_BUTTON);
  clear_all_button.className = "btn btn-primary btn-sm";  
  clear_all_button.setAttribute("title", "Clear all filters");

  let clear_all_icon = document.createElement("span");
  clear_all_icon.className = "glyphicon glyphicon-trash";
  clear_all_button.appendChild(clear_all_icon);


  let select = document.createElement('select');
  select.className = "form-select form-select-sm w-auto d-inline-block";

  
  let saved_states_container = document.createElement(FC.TAG.SAVED_STATES_CONTAINER);

  let save_controls = document.createElement("div");
  save_controls.className = "input-group";

  let save_input = document.createElement("input");
  save_input.setAttribute("type", "text");
  save_input.setAttribute("class", "form-control");
  save_input.setAttribute(FC.ATTRIBUTE.STATE_NAME, "");
  save_input.setAttribute("placeholder", "Enter filter name");
  save_controls.appendChild(save_input);

  let save_button = document.createElement(FC.TAG.SAVE_BUTTON);
  save_button.className = "btn btn-primary btn-sm";  
  save_button.setAttribute("title", "Save current filter");

  let save_icon = document.createElement("span");
  save_icon.className = "glyphicon glyphicon-floppy-disk";
  save_button.appendChild(save_icon);
  save_controls.appendChild(save_button);

  let saved_states_list = document.createElement(FC.TAG.SAVED_STATES_LIST);
  saved_states_container.appendChild(saved_states_list);

  top_control_container.appendChild(select);
  top_control_container.appendChild(export_button);
  top_control_container.appendChild(clear_all_button);
  top_control_container.appendChild(save_controls);
  top_control_container.appendChild(saved_states_container);

  let bottom_container = document.createElement("div");
  bottom_container.className = "mb-3 p-1 border bg-light";

  
  static_ret = {};

  // Simple
  let simple_option = document.createElement('option');
  simple_option.value = FC.MODE.SIMPLE;
  simple_option.textContent = FC.MODE.SIMPLE;
  select.appendChild(simple_option);
  
  let simple_div = document.createElement(FC.TAG.FILTER);
  simple_div.setAttribute(FC.ATTRIBUTE.FILTER_MODE, FC.MODE.SIMPLE);
  bottom_container.appendChild(simple_div);

  static_ret[FC.MODE.SIMPLE] = simple_static_init(simple_div);

  set_filter_property(root_el, FC.PROPERTY.SUBJECT_DATASET_NAME, subject_dataset_name);

  // Blockly

  let blockly_option = document.createElement('option');
  blockly_option.value = FC.MODE.BLOCKLY;
  blockly_option.textContent = FC.MODE.BLOCKLY;
  select.appendChild(blockly_option);
  
  let blockly_div = document.createElement(FC.TAG.FILTER);
  blockly_div.setAttribute(FC.ATTRIBUTE.FILTER_MODE, FC.MODE.BLOCKLY);
  bottom_container.appendChild(blockly_div);
  root_el.appendChild(bottom_container);

  static_ret[FC.MODE.BLOCKLY] = blockly_static_init(blockly_div, root_id);
  set_filter_property(root_el, FC.PROPERTY.STATIC_RET, static_ret);
  
  select.value = FC.MODE.SIMPLE;
  set_filter_property(root_el, FC.PROPERTY.FILTER_MODE, select.value);

  let dev_current_filter_div;
  if(__DEV_MODE) {
    dev_current_filter_div = document.createElement("div");
    root_el.appendChild(dev_current_filter_div);
  }

  root_el.addEventListener(FC.EVENT.REQUESTED_REDRAW, function(){    
    let dataset_list_data = get_filter_property(root_el, FC.PROPERTY.DATA);    
    let dataset_list_name = get_filter_property(root_el, FC.PROPERTY.DATASET_LIST_NAME);
    let subject_filter_dataset_name = get_filter_property(root_el, FC.PROPERTY.SUBJECT_DATASET_NAME);
    let filter_state = get_filter_property(root_el, FC.PROPERTY.STATE);
    let static_init_ret = get_filter_property(root_el, FC.PROPERTY.STATIC_RET, false);
    let filter_mode = get_filter_property(root_el, FC.PROPERTY.FILTER_MODE);
    let skip_dataset_filters = get_filter_property(root_el, FC.PROPERTY.SKIP_DATASET_FILTERS);
    
    init_filter_handler( 
      root_el,      
      dataset_list_data,
      dataset_list_name,
      subject_filter_dataset_name,
      filter_state,
      static_init_ret,
      filter_mode,
      skip_dataset_filters      
    );
  })

  select.addEventListener('change', function(){
    set_filter_property(root_el, FC.PROPERTY.FILTER_MODE, select.value);
    root_el.dispatchEvent(new Event(FC.EVENT.REQUESTED_REDRAW, { bubbles: true }));    
  });

  root_el.addEventListener(FC.EVENT.NEW_FILTER_VALUE, function(event){
    __logger("Sending to Shiny " + filter_state_json_input_id);
    set_filter_property(root_el, FC.PROPERTY.STATE, event.detail.filter);
    Shiny.setInputValue(filter_state_json_input_id, JSON.stringify(event.detail.filter), { priority: 'event' });
    if(__DEV_MODE) {
      dev_current_filter_div.textContent = JSON.stringify(event.detail.filter, null, 2);
    }
  });

  clear_all_button.addEventListener("click", function(){
    set_filter_property(root_el, FC.PROPERTY.STATE, FC.VAL.EMPTY_FILTER);
    root_el.dispatchEvent(new Event(FC.EVENT.REQUESTED_REDRAW, { bubbles: true })); // Trigger filter redraw after cleaning filters
  });

  let render_saved_states = function (saved_states) {
    saved_states_list.innerHTML = "";
    for (let i = 0; i < saved_states.length; ++i) {
      let group = document.createElement("div");
      group.className = "input-group input-group-sm w-auto";

      // the "main action" button
      let button = document.createElement(FC.TAG.SAVED_STATE_BUTTON);
      button.className = "btn btn-primary";
      button.textContent = saved_states[i].name;
      button.setAttribute(
        FC.ATTRIBUTE.SAVED_FILTER_STATE_NAME,
        saved_states[i].name
      );

      // the remove button as an input-group-append
      let remove_button = document.createElement(FC.TAG.REMOVED_SAVED_STATE_BUTTON);
      remove_button.type = "button";
      remove_button.className = "btn btn-primary";
      remove_button.innerHTML = "&times;";
      remove_button.setAttribute(
        FC.ATTRIBUTE.SAVED_FILTER_STATE_NAME,
        saved_states[i].name
      );

      // assemble
      group.appendChild(button);
      group.appendChild(remove_button);
      saved_states_list.appendChild(group);
    }
  };

  let send_saved_states = function(saved_states) {    
    __logger(`Sending saved states: ${JSON.stringify(saved_states)} to ${saved_filter_state_json_msg_input_id}`);
    Shiny.setInputValue(saved_filter_state_json_msg_input_id, JSON.stringify(saved_states), { priority: 'event' });
  };

  save_button.addEventListener("click",  function(){    
    let state_name = save_input.value;
    if(!state_name || state_name === "") {
      return;
    }
    save_input.value = "";
    let saved_states = get_filter_property(root_el, FC.PROPERTY.SAVED_STATES);
    let to_be_saved_state = {
      name: state_name,
      state: get_filter_property(root_el, FC.PROPERTY.STATE)
    };
    saved_states.push(to_be_saved_state);
    set_filter_property(root_el, FC.PROPERTY.SAVED_STATES, saved_states);

    render_saved_states(saved_states);
    send_saved_states(saved_states);
  });

  saved_states_list.addEventListener("click", function(event) {
    if(event.target.tagName.toLowerCase() === FC.TAG.SAVED_STATE_BUTTON) {
      __logger("Loading filter");
      let saved_states = get_filter_property(root_el, FC.PROPERTY.SAVED_STATES);
      let state_name = event.target.getAttribute(FC.ATTRIBUTE.SAVED_FILTER_STATE_NAME);
      let new_state = saved_states.find((obj)=> obj.name===state_name);
      if(!new_state) {
        throw new Error(`Could not find saved state ${state_name}`);
      }
      set_filter_property(root_el, FC.PROPERTY.STATE, new_state.state);
      root_el.dispatchEvent(new Event(FC.EVENT.REQUESTED_REDRAW, { bubbles: true })); // Trigger filter redraw after cleaning filters
    };

    
    if(event.target.tagName.toLowerCase() === FC.TAG.REMOVED_SAVED_STATE_BUTTON) {
      __logger("Removing filter");
      let saved_states = get_filter_property(root_el, FC.PROPERTY.SAVED_STATES);      
      let to_be_removed_state_name = event.target.getAttribute(FC.ATTRIBUTE.SAVED_FILTER_STATE_NAME);
      saved_states = saved_states.filter(obj => obj.name !== to_be_removed_state_name);
      set_filter_property(root_el, FC.PROPERTY.SAVED_STATES, saved_states);
      render_saved_states(saved_states);
      send_saved_states(saved_states);
    };
  });

  // First call with no event on init.
  render_saved_states(get_filter_property(root_el, FC.PROPERTY.SAVED_STATES));

  Shiny.initializedPromise.then(() => { // Otherwise Shiny is not ready to send input values
    send_saved_states(get_filter_property(root_el, FC.PROPERTY.SAVED_STATES));
  });
  
};

let baked_update_filter_result_handler= function(msg) {
  let root_el = get_root_el_by_id(msg.id)
  if(!root_el) console.error("Root el: " + msg.id + "not found");  
  update_filter_result_handler(msg, root_el);  
};
Shiny.addCustomMessageHandler("update_filter_result", baked_update_filter_result_handler);

let update_data = function(msg) {
  let root_el = get_root_el_by_id(msg.id)
  if(!root_el) console.error("Root el: " + msg.id + "not found");  
  set_filter_property(root_el, FC.PROPERTY.DATA, JSON.parse(msg.data));
  root_el.dispatchEvent(new Event(FC.EVENT.REQUESTED_REDRAW, { bubbles: true }));
  //FIXME: select reference and event cannot happen here
};
Shiny.addCustomMessageHandler("update_data", update_data);

let request_dataset_filter_state = function(msg) {
  let root_el = get_root_el_by_id(msg.id)
  if(!root_el) console.error("Root el: " + msg.id + "not found");
  set_filter_property(root_el, FC.PROPERTY.STATE, JSON.parse(msg.state));
  root_el.dispatchEvent(new Event(FC.EVENT.REQUESTED_REDRAW, { bubbles: true }));  
};
Shiny.addCustomMessageHandler("request_dataset_filter_state", request_dataset_filter_state);

let baked_show_hide_dataset_filters_handlers = function(msg) {
  let root_el = get_root_el_by_id(msg.id)
  if(!root_el) console.error("Root el: " + msg.id + "not found");  
  show_hide_dataset_filters_handler(msg, root_el);
};
Shiny.addCustomMessageHandler("show_hide_dataset_filters", baked_show_hide_dataset_filters_handlers);

let baked_init_filter_handler = function(msg) {            
    let root_el = get_root_el_by_id(msg.id)
    if(!root_el) console.error("Root el: " + msg.id + "not found");
    let dataset_lists_filter_data = deserialize_b64_filter_data(msg.dataset_lists_filter_data);
    set_filter_property(root_el, FC.PROPERTY.DATA, dataset_lists_filter_data);
    set_filter_property(root_el, FC.PROPERTY.DATASET_LIST_NAME, msg.dataset_list_name);
    set_filter_property(root_el, FC.PROPERTY.SKIP_DATASET_FILTERS, msg.skip_dataset_filters);
    
    let dataset_list_data = dataset_lists_filter_data;    
    let dataset_list_name = msg.dataset_list_name;
    let subject_filter_dataset_name = get_filter_property(root_el, FC.PROPERTY.SUBJECT_DATASET_NAME);
    let filter_state = get_filter_property(root_el, FC.PROPERTY.STATE);
    let static_init_ret = get_filter_property(root_el, FC.PROPERTY.STATIC_RET, false);
    let filter_mode = get_filter_property(root_el, FC.PROPERTY.FILTER_MODE);
    let skip_dataset_filters = msg.skip_dataset_filters;
    
    init_filter_handler( 
      root_el,      
      dataset_list_data,
      dataset_list_name,
      subject_filter_dataset_name,
      filter_state,
      static_init_ret,
      filter_mode,
      skip_dataset_filters  
    );
};
Shiny.addCustomMessageHandler("init_filter", baked_init_filter_handler);

//#endregion

export {init}

// FIXME: move the read and set properties to top level handlers

// A wall will be hit regarding who is responsible of the state managing things are getting complicated, maybe full state
// should be passed back and forth, otherwise state gets divided.

// TODO: Check requested filter states, they may contain variables that are not present and this brings errors
// Define behavior:
// Bookmarked and loaded filters must remain unaltered, so in the future when data matches they will work
// Datasets must show error/warnings for the non applicable filter/variable
// This can be done in the client and in the server
// Maybe sanitize before loading?
// TODO: Add transition to filter add and removal
// TODO: Add support to filter state update from the server (Send filter from server);
// TODO: Filter creator helpers
// TODO: Pretty print filters

/* TODO: Consider pairing creation and destruction

let destroy_picker = function() {
        categorical_select.destroy()
      }

      deferred_destroyers.push(destroy_picker);

*/

/*FIXME: loiuhb

Problem is when I simplify the dataset if the dataset is not present in the filter state we have an undefined.
This is not bad per se but functions downstream expect an empty dataset as they use .length and such.

Who is responsible for this is unclear:
- Should simplify_filter_state be responsible? It cannot directly be but:
  - a method can be included in the object
  - We can pass a list with all dataset names and those not included in the state will have an empty array.
- Should a helper be included? It receives the object and a possible dataset.
- Should functions downstream shield themselves? (an assertion is defensively included to catch errors)
- As it is implemented now we do it adhoc, simplest but probably not the best (or yes if this system is not supposed to grow)

*/

/* TODO: Should a warning appear when a preset filters uses an out of range value. They are corrected by blockly but there is no warning*/


/* TODO: # When attaching the dependencies on my own an error occurs when using multiple
    # When including shinyWidget picker_input itself the error disappears, this should be explored
  */

/*TODO: Split in two workspaces one for the subject filters and another for the table filters
*/

/*TODO: Let the user know when the filter is a non-finished state when pressing the apply filter button.
*/

/*TODO: Unapplied changes in blockly are easy to miss. */


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



