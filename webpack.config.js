const path = require('path');
const webpack = require('webpack');

module.exports = {
    "entry": [
      'react-hot-loader/patch',
      path.resolve(__dirname, "./lib/es6/src/main.js"),
    ],
    "output": {
      "filename": "bundle.js",
      "path": "/public/",
      "publicPath": "/public/"
    },
    "module": {
      "rules": [
        {
          "test": /\\.(js(x)+)$/,
          "use": ['react-hot-loader/webpack']
        }
      ]
    },
    plugins: [
      new webpack.HotModuleReplacementPlugin()
    ],
    "resolve": {
      "extensions": [".ml", ".js"]
    },
    devServer: {
      proxy: {
        '/problems/**': {
          target: `http://localhost:${process.env.SERVER_PORT || 3000}`,
          secure: false,
        }
      },
      port: (process.env.WEBPACK_PORT || 8080),
      hot: true,
      inline: true,
      historyApiFallback: true
    }
};
