//@ts-check
const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const WriteFilePlugin = require('write-file-webpack-plugin');

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}

var CONFIG = {
    fsharpEntry: './client.fsproj',
    babel: {
        presets: [
            ['@babel/preset-env', {
                modules: false,
                // This adds polyfills when needed. Requires core-js dependency.
                // See https://babeljs.io/docs/en/babel-preset-env#usebuiltins
                useBuiltIns: 'usage',
                corejs: 3
            }]
        ],
    }
};

module.exports = (env, argv) => {
    var isProduction = argv.mode === "production";
    var mode = isProduction ? "production" : "development";
    var port = process.env.SUAVE_FABLE_PORT || "8083";
    console.log("Bundling for " + mode + "...");

    const plugins = [
        new WriteFilePlugin(),
        new CleanWebpackPlugin(),
        new HtmlWebpackPlugin({
            template: "public/index.html"
        }),
        new CopyPlugin([
            {
                from: 'public',
                to: '.',
                ignore: ['public/index.html']
            },
          ]),
    ];

    if (isProduction) {
        plugins.push(new webpack.HotModuleReplacementPlugin());
        plugins.push(new webpack.NamedModulesPlugin());
    }

    return {
        mode,
        devtool: "source-map",
        entry: resolve(CONFIG.fsharpEntry),
        output: {
            filename: 'bundle.js',
        },
        devServer: {
            proxy: [
                {
                    context: ['/api/socket'],
                    target: 'ws://localhost:' + port,
                    ws: true
                },
                {
                    context: ['/api', '/', '/logon', '/logoff', '/logonfast'],
                    target: 'http://localhost:' + port,
                    changeOrigin: true
                }],
            hot: true,
            inline: true
        },
        module: {
            rules: [
                {
                    test: /\.fs(x|proj)?$/,
                    use: {
                        loader: "fable-loader",
                        options: {
                            babel: CONFIG.babel,
                            define: isProduction ? [] : ["DEBUG"]
                        }
                    }
                },
                {
                    test: /\.js$/,
                    exclude: /node_modules/,
                    use: {
                        loader: 'babel-loader',
                        options: CONFIG.babel
                    },
                },
                {
                    test: /\.scss$/,
                    use: [
                        "style-loader",
                        "css-loader",
                        {
                            loader: 'sass-loader',
                            options: { implementation: require('sass') }
                        }
                    ]
                },
                {
                    test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/,
                    use: ['file-loader']
                }
            ]
        },
        plugins
    };
};