const path = require('path');

module.exports = {
  entry: ['./src/blockly_filter.js'],
  output: {
    filename: 'blockly_filter_minified.js',
    path: path.resolve(__dirname, '../inst/filter/'),
    library: 'blockly_filter',
    libraryTarget: 'umd', // Makes it work with different module systems
    iife: true,
  },
  experiments: {
    outputModule: true,
  },
};

