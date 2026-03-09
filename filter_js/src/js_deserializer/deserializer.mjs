// node filter_js/src/js_deserializer/deserializer.js

const decoder = new TextDecoder("utf-8");

let buf_read_int32 = function(b_struct) {
  const int = b_struct.data.getInt32(b_struct.offset, true); // true = little-endian
  b_struct.offset += 4; // size of int32

  return int;
};

let buf_read_double = function(b_struct) {  
  const double = b_struct.data.getFloat64(b_struct.offset, true); // true = little-endian
  b_struct.offset += 8; //size of float64

  return double;
};

let buf_read_str = function(b_struct) {
  const size_of_str = buf_read_int32(b_struct);
  const str = decoder.decode(b_struct.bytes.subarray(b_struct.offset, b_struct.offset + size_of_str - 1));
  b_struct.offset += size_of_str;
  return str;
};

let deserialize_b64_filter_data = function(base64){
  console.time("deserializeb64");
  let res = deserialize_binary_filter_data(base64_to_Uint8Array(base64));
  console.timeEnd("deserializeb64");
  return(res);
}

let deserialize_binary_filter_data = function(buf) {  

  // let log = console.log;
  let log = function(...args) {};

  let b_struct = {
    offset: 0,
    data: new DataView(buf.buffer, buf.byteOffset, buf.byteLength),
    bytes: new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength)
  }

  const size_of_magic_num = 8;
  const bytes = new Uint8Array(b_struct.data.buffer, b_struct.data.byteOffset + b_struct.offset, size_of_magic_num); // remove null terminator
  const magic = decoder.decode(bytes);
  b_struct.offset += size_of_magic_num + 1;
  
  log("offset: ", b_struct.offset);
  log("Magicnum:", magic);

  if (magic.trim() !== "FILTDATA") throw new Error(`Wrong magic number ${magic}`);

  const version = buf_read_int32(b_struct);
  
  log("Version:", version);
  if (version !== 1) throw new Error(`Wrong version ${version}`);

  let dataset_lists_len = buf_read_int32(b_struct);
  log("DSLS_LEN", dataset_lists_len);

  let deser = {
    dataset_lists: new Array(dataset_lists_len)
  };

  for (let dataset_list_idx = 0; dataset_list_idx < dataset_lists_len; dataset_list_idx++) {
    let dataset_list_name = buf_read_str(b_struct);
    log("DSLN_name:", dataset_list_name);
    let dataset_list_len = buf_read_int32(b_struct);
    log("DSLN_LENGTH:", dataset_list_len);
    
    let dataset_list = {
      name: dataset_list_name,
      dataset_list: new Array(dataset_list_len)
    };

    for (let dataset_idx = 0; dataset_idx < dataset_list_len; dataset_idx++) {
      let dataset_name = buf_read_str(b_struct);
      log("DS_NAME:", dataset_name);

      let dataset_label = buf_read_str(b_struct);
      log("DS_LABEL:", dataset_label);

      let dataset_nrow = buf_read_int32(b_struct);
      log("DS_NROW:", dataset_name);

      let dataset_nvar = buf_read_int32(b_struct);
      log("DS_NVAR:", dataset_nvar);

      let dataset = {
        name: dataset_name,
        label: dataset_label,
        nrow: dataset_nrow,
        variables: new Array(dataset_nvar)
      }

      for(let variable_idx = 0; variable_idx < dataset_nvar; variable_idx++){        
        let variable_name = buf_read_str(b_struct);
        // log("VN:", variable_name);

        let variable_label = buf_read_str(b_struct);
        // log("VL:", variable_label);

        let variable_class = buf_read_str(b_struct);
        // log("VC:", variable_class);

        let variable_kind = buf_read_str(b_struct);
        // log("VK:", variable_kind);

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
          variable.value = new Array(value_len);
          variable.count = new Array(value_len);
          for(let value_idx = 0; value_idx < value_len; value_idx++) {
            variable.value[value_idx] = buf_read_str(b_struct);
          }

          for(let count_idx = 0; count_idx < value_len; count_idx++) { // TODO: Bulk read could be possible if elements were aligned, not currently
            variable.count[count_idx] = buf_read_int32(b_struct);
          }          
        } else if (variable_kind === "numerical") {
          variable.min = buf_read_double(b_struct);
          variable.max = buf_read_double(b_struct);
          let density_len = buf_read_int32(b_struct);
          variable.density = new Array(density_len);
          
          for (let density_idx = 0; density_idx < density_len; density_idx++) {  // TODO: Bulk read could be possible if elements were aligned, not currently
            variable.density[density_idx] = buf_read_double(b_struct);
          }
        } else if (variable_kind === "date") {
          variable.min = buf_read_double(b_struct);
          variable.max = buf_read_double(b_struct);

          // Format to string as it is how is used downstream
          // Infs are treated as epoch origin for min and today + 1 for max
          variable.min = format_date_yyyy_mm_dd(R_numeric_date_JS_Date(variable.min, new Date(0)));
          variable.max = format_date_yyyy_mm_dd(R_numeric_date_JS_Date(variable.max, new Date(Date.now() + 86400000))); // Today + 1 day
        } else {
          console.warn("Unknown kind variable: " + variable_name);         
        }
        // log("Pushing variable:", variable_name);
        dataset.variables[variable_idx] = variable;
        // log("Pushed");
      }
      log("Pushing dataset:", dataset_name);
      dataset_list.dataset_list[dataset_idx] = dataset;
      log("Pushed");
    }
    log("Pushing dataset_list:", dataset_list_name);
    deser.dataset_lists[dataset_list_idx] = dataset_list;
    log("Pushed");
  }

  log("Returning");

  return(deser);

}

let R_numeric_date_JS_Date = function(days_since_epoch, default_if_nan) {

  let MILISECONDS_PER_DAY = 86400000;
  const date = new Date(days_since_epoch * MILISECONDS_PER_DAY);  

  return isNaN(date.getTime()) ? default_if_nan : date 
}

let format_date_yyyy_mm_dd = function(date){
  const day = String(date.getDate()).padStart(2, '0');
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const year = date.getFullYear();
  const formatted = `${year}-${month}-${day}`;
  return(formatted);
}

let base64_to_Uint8Array = function (base64) {
  if (typeof Uint8Array.fromBase64 === "function") {
    return(Uint8Array.fromBase64(base64));
  } else {    
    return (base64_to_Uint8Array_chromium_less_than_140(base64));
  }
}

let base64_to_Uint8Array_chromium_less_than_140 = function (base64) {
  const binary = atob(base64);
  const len = binary.length;

  const bytes = new Uint8Array(len);

  for (let i = 0; i < len; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}



export {deserialize_binary_filter_data, deserialize_b64_filter_data, R_numeric_date_JS_Date, format_date_yyyy_mm_dd as format_date_dd_mm_yyyy};