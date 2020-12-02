const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

module.exports = {
    entry: {
        top: path.resolve(__dirname, 'src/index.tsx')
    },
    output: {
        filename: '[name].[contenthash].bundle.js',
        path: path.resolve(__dirname, 'dist')
    },
    module: {
        rules: [
            {
                test: /(\.tsx|\.ts)$/,
                include: path.resolve(__dirname, 'src'),
                exclude: /node_modules/,
                loader: 'ts-loader'
            },
            {
                test: /\.scss$/,
                include: path.resolve(__dirname, 'src'),
                use: [
                    'style-loader',
                    {
                        loader: 'css-loader',
                        options: {
                            modules: true,
                            importLoaders: 1
                        }
                    },
                    'sass-loader'
                ]
            }
        ]
    },
    resolve: {
        extensions: [ '.tsx', '.ts', '.js', '.scss', '.css' ],
        modules: [ path.resolve(__dirname, 'src'), 'node_modules' ]
    },
    plugins: [
        new CleanWebpackPlugin({
            cleanOnceBeforeBuildPatterns: ['**/*', '!.gitignore', '!.gitkeep']
        }),
        new HtmlWebpackPlugin({
            chunks: ['top'],
            filename: 'index.html',
            template: 'src/index.html'
        })
    ],
    optimization: {
        splitChunks: {
            chunks: 'async',
            cacheGroups: {
                libs: {
                    test: /src[\/\\]Lib/,
                    name: 'libs',
                    chunks: 'all',
                    enforce: true
                },
                vendors: {
                    test: /node_modules/,
                    name: 'vendors',
                    chunks: 'all',
                    enforce: true
                }
            }
        }
    }
};
