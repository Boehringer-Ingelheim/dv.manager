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
  mode: 'production',
  devtool: 'eval-source-map',
  optimization: {
    minimize: false,
  },
  resolve: {
    extensions: ['.js', '.ts'], // 1️⃣ Allow importing files without specifying extensions
    modules: [path.resolve(__dirname, 'src'), 'node_modules'], // 2️⃣ Where Webpack should look for modules
  }, 
  
};

