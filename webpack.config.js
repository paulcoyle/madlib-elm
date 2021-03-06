const webpack = require('webpack')
const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const nib = require('nib')

module.exports = {
  entry: path.join(__dirname, 'src', 'index.js'),

  output: {
    path: 'dist',
    publicPath: '/',
    filename: 'js/app.[chunkhash].js',
    chunkFilename: 'js/[id].[chunkhash].js'
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
        test: /\.js$/,
        loader: 'babel',
        exclude: [/elm-stuff/, /node_modules/],
        query: {
          presets: ['es2015']
        }
      }, {
        test: /\.styl$/,
        loader: 'style-loader!css-loader!stylus-loader',
        exclude: [/elm-stuff/, /node_modules/]
      }, {
        test: /\.json$/,
        loader: 'json-loader',
        exclide: [/elm-stuff/, /node_modules/]
      }, {
        test: /\.svg$/,
        loader: 'svg-sprite?' + JSON.stringify({
          name: 'icon-[name]',
          prefixize: false
        }),
        exclude: [/elm-stuff/, /node_modules/],
      }
    ]
  },

  plugins: getPlugins(),

  stylus: {
    use: [require('nib')()],
    import: ['~nib/index.styl']
  }
}

function getPlugins() {
  let plugins = [
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      filename: 'index.html',
      inject: 'body'
    })
  ]

  if (process.env.NODE_ENV === 'production') {
    plugins.push(
      new webpack.optimize.UglifyJsPlugin({
        minimize: true,
        output: { comments: false },
        warnings: false
      })
    )
  }

  return plugins
}
