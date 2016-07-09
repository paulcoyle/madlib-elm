const webpack = require('webpack')
const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const nib = require('nib')

module.exports = {
  entry: path.join(__dirname, 'src', 'index.js'),

  output: {
    path: 'dist',
    filename: 'js/app.[chunkhash].js',
    publicPath: '/'
  },

  resolve: {
    extensions: [ '', '.js', '.elm' ]
  },

  module: {
    noParse: /\.elm$/,
    loaders: [
      {
        test: /\.elm$/,
        loader: 'elm-webpack',
        exclude: [/elm-stuff/, /node_modules/]
      }, {
        test: /\.styl$/,
        loader: 'style-loader!css-loader!stylus-loader',
        exclude: [/elm-stuff/, /node_modules/]
      }
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      filename: 'index.html',
      inject: 'body'
    })
  ],

  stylus: {
    use: [require('nib')()],
    import: ['~nib/index.styl']
  }
}
