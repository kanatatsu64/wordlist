const path = require('path')
const { merge } = require('webpack-merge')
const common = require('./webpack.config')

module.exports = merge(common, {
    mode: 'development',
    devtool: 'source-map',
    cache: {
        type: 'filesystem',
        cacheDirectory: path.resolve(__dirname, 'cache'),
        buildDependencies: {
            config: [__filename]
        }
    }
})
