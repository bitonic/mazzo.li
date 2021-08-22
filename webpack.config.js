const path = require('path');

const isDevelopment = process.env.NODE_ENV !== 'production';

console.log({
  isDevelopment,
  mode: process.env.NODE_ENV,
});

module.exports = {
  mode: process.env.NODE_ENV,
  entry: './ts/scripts.ts',
  resolve: {
    extensions: ['.tsx', '.ts', '.js', '.scss', '.css'],
    modules: [
      'ts',
      'scss',
      'node_modules',
    ],
  },
  module: {
    rules: [
      {
        test: /\.[jt]sx?$/,
        exclude: /node_modules/,
        use: [ { loader: "ts-loader" } ],
      },
      {
        test: /\.scss$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" },
          { loader: "sass-loader" }
        ]
      },
      {
        test: /\.css$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" }
        ]
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          { loader: 'file-loader' }
        ]
      }
    ],
  },
  output: {
    filename: 'scripts.js',
    path: path.resolve(__dirname, 'assets/webpack'),
    publicPath: '/assets/webpack',
  },
  devtool: isDevelopment ? 'inline-source-map' : undefined,
  devServer: {
    static: path.join(__dirname, "_site"),
  },
};
