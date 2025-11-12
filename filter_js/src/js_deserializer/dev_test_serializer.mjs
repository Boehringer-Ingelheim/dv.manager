import {deserializeb64_filter_data} from './deserializer.mjs';

let b64_example = "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"

let deserialized_example = {
 "dataset_lists": [
  {
   "name": "dataset_list_1",
   "dataset_list": [
    {
     "name": "dataset_1",
     "nrow": 6,
     "variables": [
      {
       "name": "range_var",
       "label": "range_var",
       "class": "integer",
       "kind": "numerical",
       "NA_count": 1,
       "min": 1,
       "max": 5,
       "density": [
        0.4,
        0.2,
        0.2,
        0.2
       ]
      },
      {
       "name": "date_var",
       "label": "date_var",
       "class": "Date",
       "kind": "date",
       "NA_count": 1,
       "min": "2024-01-01",
       "max": "2024-01-05"
      },
      {
       "name": "posix_var",
       "label": "posix_var",
       "class": "POSIXct",
       "kind": "date",
       "NA_count": 1,
       "min": "2024-01-01",
       "max": "2024-01-05"
      },
      {
       "name": "subset_var",
       "label": "subset_var",
       "class": "factor",
       "kind": "categorical",
       "NA_count": 1,
       "value": [
        "a",
        "b",
        "c",
        "d",
        "e"
       ],
       "count": [
        1,
        1,
        1,
        1,
        1
       ]
      },
      {
       "name": "logical_var",
       "label": "logical_var",
       "class": "logical",
       "kind": "categorical",
       "NA_count": 1,
       "value": [
        "FALSE",
        "TRUE"
       ],
       "count": [
        3,
        2
       ]
      },
      {
       "name": "sbj_var",
       "label": "sbj_var",
       "class": "factor",
       "kind": "categorical",
       "NA_count": 0,
       "value": [
        "SBJ-1",
        "SBJ-2",
        "SBJ-3",
        "SBJ-4",
        "SBJ-5",
        "SBJ-6"
       ],
       "count": [
        1,
        1,
        1,
        1,
        1,
        1
       ]
      }
     ]
    },
    {
     "name": "dataset_2",
     "nrow": 6,
     "variables": [
      {
       "name": "sbj_var",
       "label": "sbj_var",
       "class": "factor",
       "kind": "categorical",
       "NA_count": 0,
       "value": [
        "SBJ-1",
        "SBJ-2",
        "SBJ-3",
        "SBJ-4",
        "SBJ-5",
        "SBJ-6"
       ],
       "count": [
        1,
        1,
        1,
        1,
        1,
        1
       ]
      }
     ]
    }
   ]
  }
 ]
};

let buf_read_int32 = function(b_struct){
  let size_of_int32 = 4;
  let int = b_struct.data.readInt32LE(b_struct.offset);

  b_struct.offset += size_of_int32;
  // console.log("ReadI: ", int);
  // console.log("offset: ", b_struct.offset);
  return(int);
}

let buf_read_double = function(b_struct){
  let size_of_double = 8;
  let double = b_struct.data.readDoubleLE(b_struct.offset);
  b_struct.offset += size_of_double;
  // console.log("ReadD: ", double);
  // console.log("offset: ", b_struct.offset);
  return(double);
}

let buf_read_str = function(b_struct){
  let size_of_str = buf_read_int32(b_struct);
  let str = b_struct.data.toString("utf8", b_struct.offset, b_struct.offset + size_of_str - 1); //Remove null terminator
  b_struct.offset += size_of_str;
  // console.log("ReadS: ", str);
  // console.log("offset: ", b_struct.offset);
  return(str);
}

let deserialize_binary_filter_data = function(buf) {

  let b_struct = {
    offset: 0,
    data: buf
  }

  const magic = b_struct.data.toString("utf8", b_struct.offset, b_struct.offset + 8);
  b_struct.offset += 9;
  console.log("offset: ", b_struct.offset);
  console.log("Magicnum:", magic);

  if (magic.trim() !== "FILTDATA") throw new Error(`Wrong magic number ${magic}`);

  const version = buf_read_int32(b_struct);
  
  console.log("Version:", version);
  if (version !== 1) throw new Error(`Wrong version ${version}`);

  let dataset_lists_len = buf_read_int32(b_struct);
  console.log("DSLS_LEN", dataset_lists_len);

  let deser = {
    dataset_lists: []
  };

  for (let dataset_list_idx = 0; dataset_list_idx < dataset_lists_len; dataset_list_idx++) {        
    let dataset_list_name = buf_read_str(b_struct);
    console.log("DSLN_name:", dataset_list_name);
    let dataset_list_len = buf_read_int32(b_struct);
    console.log("DSLN_LENGTH:", dataset_list_len);
    
    let dataset_list = {
      name: dataset_list_name,
      dataset_list: []
    };

    for (let dataset_idx = 0; dataset_idx < dataset_list_len; dataset_idx++) {
      let dataset_name = buf_read_str(b_struct);
      console.log("DS_NAME:", dataset_name);

      let dataset_nrow = buf_read_int32(b_struct);
      console.log("DS_NROW:", dataset_name);

      let dataset_nvar = buf_read_int32(b_struct);
      console.log("DS_NVAR:", dataset_nvar);

      let dataset = {
        name: dataset_name,
        nrow: dataset_nrow,
        variables: []
      }

      for(let variable_idx = 0; variable_idx < dataset_nvar; variable_idx++){        
        let variable_name = buf_read_str(b_struct);
        console.log("VN:", variable_name);

        let variable_label = buf_read_str(b_struct);
        console.log("VL:", variable_label);

        let variable_class = buf_read_str(b_struct);
        console.log("VC:", variable_class);

        let variable_kind = buf_read_str(b_struct);
        console.log("VK:", variable_kind);

        let NA_count = buf_read_int32(b_struct);

        let variable = {
          name: variable_name,
          label: variable_label,
          class: variable_class,
          kind: variable_kind,
          NA_count: NA_count
        };

        if(variable_kind === "categorical") {
          let value_len = buf_read_int32(b_struct);
          variable.value = [];
          variable.count = [];
          for(let value_idx = 0; value_idx < value_len; value_idx++) {
            variable.value.push(buf_read_str(b_struct));
          }

          for(let count_idx = 0; count_idx < value_len; count_idx++) {
            variable.count.push(buf_read_int32(b_struct));
          }          
        } else if (variable_kind === "numerical") {
          variable.min = buf_read_double(b_struct);
          variable.max = buf_read_double(b_struct);
          variable.density = [];
          let density_len = buf_read_int32(b_struct);
          for(let density_idx = 0; density_idx < density_len; density_idx++) {
            variable.density.push(buf_read_double(b_struct));
          }
        } else if (variable_kind === "date") {
          variable.min = buf_read_double(b_struct);
          variable.max = buf_read_double(b_struct);
        } else {
          throw new Error(`Unknown kind ${variable_kind}`);          
        }
        console.log("Pushing variable:", variable_name);
        dataset.variables.push(variable);
        console.log("Pushed");
      }
      console.log("Pushing dataset:", dataset_name);
      dataset_list.dataset_list.push(dataset);
      console.log("Pushed");
    }
    console.log("Pushing dataset_list:", dataset_list_name);
    deser.dataset_lists.push(dataset_list);
    console.log("Pushed");
  }

  console.log("Returning");

  return(deser);

}

let buf = Buffer.from(b64_example, "base64");
let deserialized = deserialize_binary_filter_data(buf);

console.log("")

let deser_json = JSON.stringify(deserialized, null, 1);
let deser_json_example = JSON.stringify(deserialized_example, null, 1);

import { writeFileSync } from "fs";

writeFileSync('deser.json', deser_json, 'utf8');
writeFileSync('deser_example.json', deser_json_example, 'utf8');

console.log('File saved successfully!');

console.log("Equal: ", JSON.stringify(deserialized) === JSON.stringify(deserialized_example));

