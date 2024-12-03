const filterBlockly = (() => {

  const logger = console.log;

  // Block definition 

  const table_generator = function (block, generator) {
    const dataset = block.type.split('_')[1];
    const target = block.type.split('_').slice(2).join('_');
    const statements = generator.statementToCode(block, 'contents');
    const code = `{"dataset_name": "${dataset}", "filters": [{"target": "${target}", "filter":${statements}}]}`
    logger(code);
    return (code)
  }

  const subject_generator = function (block, generator) {
    const statements = "[" + generator.statementToCode(block, 'contents') + "]";
    const parsed_contents = JSON.parse(statements);

    let res = {};
    let ds_name = parsed_contents[0].dataset_name;
    let filters = [];

    for (idx in parsed_contents) {
      if (ds_name !== parsed_contents[idx]["dataset_name"]) {
        throw new Error("Dataset differ");
      }
      if(parsed_contents[idx].filters) {filters.push(...parsed_contents[idx].filters)};
      if(parsed_contents[idx].filter_list) {filters.push(...parsed_contents[idx].filter_list)};      
    }
    res[ds_name] = { subject: filters }

    const code = JSON.stringify(res)
    logger(code);
    return (code)
  }

  const dataset_generator = function (block, generator) {
    const statements = "[" + generator.statementToCode(block, 'contents') + "]";
    const parsed_contents = JSON.parse(statements);

    let res = {};
    let ds_name = parsed_contents[0].dataset_name;
    let filters = [];

    for (idx in parsed_contents) {
      if (ds_name !== parsed_contents[idx]["dataset_name"]) {
        throw new Error("Dataset differ");
      }
      
      if(parsed_contents[idx].filters) {
        filters.push(...parsed_contents[idx].filters);
      }
      if(parsed_contents[idx].filter_list) {
        debugger;
        filters.push(...parsed_contents[idx].filter_list);
      }
    }
    res[ds_name] = { dataset: filters }

    const code = JSON.stringify(res)
    logger(code);
    return (code)
  }

  const connector_generator = function (block, generator) {
    const type = block.getFieldValue("operation");
    const statements = generator.statementToCode(block, 'contents');
    let code;
    if (type === "and" || type === "or") {
      code = `{"type": "${type}", "filter_list":[${statements}]}`
    } else if (type === "not") {
      code = `{"type": "${type}", "filter":${statements}}`
    } else {
      throw new Error("Operation unknown");
    }

    logger(code);
    return (code)
  }

  const set_generator = function (block, generator) {
    const type = block.getFieldValue("operation");
    const statements = generator.statementToCode(block, 'contents');
    let code;
    if (type === "union" || type === "intersect") {
      code = `{"type": "${type}", "filter_list":[${statements}]}`
    } else if (type === "diff") {
      code = `{"type": "${type}", "filter":${statements}}`
    } else {
      throw new Error("Operation unknown");
    }

    logger(code);
    return (code)
  }

  const filter_generator_numeric = function (block, generator) {
    const column = block.type.split('_').slice(3).join('_');
    const def = {
      type: "double",
      column: column,
      value: { min: block.getFieldValue("min"), max: block.getFieldValue("max") },
      NAs: "FALSE"
    }
    return (JSON.stringify(def))
  }

  const filter_generator_factor = function (block, generator) {

    const column = block.type.split('_').slice(3).join('_');
    const def = {
      type: "category",
      column: column,
      value: [block.getFieldValue("value")],
      NAs: "FALSE"
    }
    return (JSON.stringify(def))
  }

  const table_factory = function (type, name, color) {
    let def = {
      type: type,
      tooltip: "",
      helpUrl: "",
      message0: name + " %1",
      args0: [
        {
          type: "input_statement",
          name: "contents"
        }
      ],
      previousStatement: null,
      nextStatement: null,
      colour: color
    }
    return (def)
  }

  const factor_factory = function (type, name, value, color) {
    let dd_options = [];
    for (v of value) {
      dd_options.push([v, v])
    }

    if (dd_options.length == 0) dd_options = [["_EMPTY_VEC_", "_EMPTY_VEC_"]]

    let def =
    {
      "type": type,
      "tooltip": "",
      "helpUrl": "",
      "message0": name + "%1 %2",
      "args0": [
        {
          "type": "field_dropdown",
          "name": "value",
          "options": dd_options,
        },
        {
          "type": "input_dummy",
          "name": "NAME"
        }
      ],
      previousStatement: null,
      nextStatement: null,
      "colour": color
    };

    return (def)
  }

  const numerical_factory = function (type, name, value, color) {
    let def = {
      type: type,
      tooltip: "",
      helpUrl: "",
      message0: name + " Min %1 Max %2 %3",
      args0: [
        {
          type: "field_number",
          name: "min",
          value: value.min,
          min: value.min,
          max: value.max,
        },
        {
          type: "field_number",
          name: "max",
          value: value.max,
          min: value.min,
          max: value.max,
        },
        {
          type: "input_dummy",
          name: "NAME"
        }
      ],
      previousStatement: null,
      nextStatement: null,
      colour: color
    }

    return (def)
  }

  const get_code = function (ws, gen) {

    var json = Blockly.serialization.workspaces.save(ws);
    var blocks = json['blocks']['blocks'];
    var topBlocks = blocks.slice();  // Create shallow copy.
    blocks.length = 0;

    var allCode = [];
    var headless = new Blockly.Workspace();
    for (var i = 0; i < topBlocks.length; i++) {
      var block = topBlocks[i];
      blocks.push(block);
      Blockly.serialization.workspaces.load(json, headless);
      allCode.push(JSON.parse(gen.workspaceToCode(headless)));
      blocks.length = 0;
    }

    const ds_name = Object.keys(allCode[0])[0];
    let res = {};
    res[ds_name] = {};

    for (idx in allCode) {
      if (ds_name !== Object.keys(allCode[idx])[0]) {
        throw new Error("Dataset differ");
      }
      Object.assign(res[ds_name], allCode[idx][ds_name]); // Merge the properties
    }

    const res_state = {
      state: Blockly.serialization.workspaces.save(ws),
      filters: res
    }

    let stringified_res = JSON.stringify(res_state)

    return (stringified_res)
  }

  let json_generator = new Blockly.Generator('JSON');

  json_generator.scrub_ = function (block, code, thisOnly) {
    const nextBlock =
      block.nextConnection && block.nextConnection.targetBlock();
    if (nextBlock && !thisOnly) {
      return code + ',\n' + json_generator.blockToCode(nextBlock);
    }
    return code;
  };

  json_generator.forBlock['dataset_filters'] = dataset_generator;
  json_generator.forBlock['subject_filters'] = subject_generator;
  json_generator.forBlock['c_dataset_connector'] = connector_generator;
  json_generator.forBlock['c_subject_connector'] = set_generator;

  const init = function (id) {
    const container_div = document.getElementById(id);
    const script_tag = container_div.querySelector('script[type="application/json"]');

    let json_data;

    try {
      json_data = JSON.parse(script_tag.textContent.trim());
      // logger(json_data);
    } catch (error) {
      console.error("Error parsing JSON:", error);
    }

    let filter_state = json_data.state;
    let filter_data = json_data.data;

    let block_definition = [
      {
        "type": "c_dataset_connector",
        "tooltip": "",
        "helpUrl": "",
        "message0": "%1 %2",
        "args0": [
          {
            "type": "field_dropdown",
            "name": "operation",
            "options": [
              [
                "and",
                "and"
              ],
              [
                "or",
                "or"
              ],
              [
                "not",
                "not"
              ]
            ]
          },
          {
            "type": "input_statement",
            "name": "contents"
          }
        ],
        previousStatement: null,
        nextStatement: null,
        "colour": 225
      },
      {
        "type": "c_subject_connector",
        "tooltip": "",
        "helpUrl": "",
        "message0": "%1 %2",
        "args0": [
          {
            "type": "field_dropdown",
            "name": "operation",
            "options": [
              [
                "union",
                "union"
              ],
              [
                "intersect",
                "intersect"
              ],
              [
                "diff",
                "diff"
              ]
            ]
          },
          {
            "type": "input_statement",
            "name": "contents"
          }
        ],
        previousStatement: null,
        nextStatement: null,
        "colour": 225
      },
      {
        "type": "dataset_filters",
        "tooltip": "",
        "helpUrl": "",
        "message0": "Dataset Filters %1",
        "args0": [
          {
            "type": "input_statement",
            "name": "contents"
          }
        ],
        "colour": 285
      },
      {
        "type": "subject_filters",
        "tooltip": "",
        "helpUrl": "",
        "message0": "Subject Filters %1",
        "args0": [
          {
            "type": "input_statement",
            "name": "contents"
          }
        ],
        "colour": 285
      }
    ];
    let toolbox = {
      kind: "categoryToolbox",
      contents: [
        {
          kind: "category",
          name: "tables",
          contents: [
            {
              kind: "block",
              type: "dataset_filters"
            },
            {
              kind: "block",
              type: "subject_filters"
            },      
          ]
        },          
        {
          kind: "category",
          name: "connectors",
          contents: [
            {
              kind: "block",
              type: "c_dataset_connector"
            },{
              kind: "block",
              type: "c_subject_connector"
            }
          ]
        },
      ]
    };

    for (const dataset in filter_data) {
      logger(dataset)
      let color = 0;
      let dataset_category = {
        kind: "category",
        name: dataset,
        contents: []
      };
      this_dataset = filter_data[dataset];

      for (const table in this_dataset) {
        const table_name = "t_" + dataset + "_" + table;
        let table_category = {
          kind: "category",
          name: table,
          contents: []
        };
        let table_block = {
          kind: "block",
          type: table_name
        };
        this_table = this_dataset[table];
        block_definition.push(table_factory(table_name, table, color))
        json_generator.forBlock[table_name] = table_generator
        dataset_category.contents.push(table_block)

        for (const col in this_table) {
          const this_col = this_table[col];
          logger(this_col["name"]);

          const block_ns_type = "f_" + dataset + "_" + table + "_" + this_col.name;

          let block_tb = {
            kind: "block",
            type: block_ns_type,
          };

          let block_def;
          // logger(this_col)
          if (this_col.type === "double" || this_col.type === "integer") {
            block_def = numerical_factory(block_ns_type, this_col.name, this_col.value, color);
            json_generator.forBlock[block_ns_type] = filter_generator_numeric
          } else if (this_col.type === "character" || this_col.type === "factor") {
            block_def = factor_factory(block_ns_type, this_col.name, this_col.value, color);
            json_generator.forBlock[block_ns_type] = filter_generator_factor
          } else {
            block_def = null;
          }
          if (block_def) table_category.contents.push(block_tb);
          if (block_def) block_definition.push(block_def);
        }
        dataset_category.contents.push(table_category)
        color = color + 30;
      }
      toolbox.contents.push(dataset_category)
    }

    Blockly.defineBlocksWithJsonArray(block_definition)

    let options = {}
    options.toolbox = toolbox;
    let ws = Blockly.inject(container_div, options);

    if(filter_state) {
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
  });

})();
