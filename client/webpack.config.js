const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

module.exports = {
    entry: {
        top: './src/index.tsx',
        csv: './src/Csv/index.tsx'
    },
    output: {
        filename: '[name].[contenthash].js',
        path: path.resolve(__dirname, 'dist')
    },
    module: {
        rules: [
            {
                test: /(\.tsx|\.ts)$/,
                exclude: /node_modules/,
                loader: 'ts-loader'
            }
        ]
    },
    resolve: {
        extensions: [ '.tsx', '.ts', '.js' ],
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
    ]
};
