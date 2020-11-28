const path = require('path');

module.exports = {
    entry: {
        top: './src/index.tsx',
        csv: './src/Csv/index.tsx'
    },
    output: {
        filename: '[name].js',
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
    }
};
