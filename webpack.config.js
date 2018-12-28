const path = require('path')

module.exports = {
    entry : './src/ParserC.bs.js',
    output: {
        path: path.resolve(__dirname, 'src'),
        filename: 'bundle.js'
    },
    mode: 'development'
}