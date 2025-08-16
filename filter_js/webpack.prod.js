const path = require('path');

module.exports = {
  entry: ['./src/dv_filter.js'],
  output: {
    filename: 'dv_filter_minified.js',
    path: path.resolve(__dirname, '../inst/filter/'),
    library: 'dv_filter',
    libraryTarget: 'umd', // Makes it work with different module systems
    iife: true,
  },
  experiments: {
    outputModule: true,
  },
};

