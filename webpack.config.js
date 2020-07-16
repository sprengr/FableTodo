const path = require("path");

module.exports = (env, argv) => {
  const mode = argv.mode;

  return {
    mode: mode,
    entry: "./src/App.fsproj",
    devServer: {
      contentBase: path.join(__dirname, "./dist"),
    },
    module: {
      rules: [
        {
          test: /\.fs(x|proj)?$/,
          use: "fable-loader",
        },
        {
            test: /\.(css|sass|scss)$/i,
            use: [
                "style-loader",
                "css-loader",
                "sass-loader",
                "postcss-loader"
            ]
        }
      ],
    },
  };
};
