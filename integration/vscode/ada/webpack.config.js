'use strict';
// eslint-disable-next-line @typescript-eslint/no-require-imports
const path = require('path'); // This require is mandatory

module.exports = {
    target: 'webworker',
    entry: './out/src/visualizing/App.js',
    mode: 'development',
    output: {
        filename: 'src/visualizing/AppMain.js',
        path: path.resolve(__dirname, 'out'),
    },
    resolve: {
        mainFields: ['module', 'main'],
        extensions: ['.ts', '.js'],
        alias: {},
        fallback: {},
        modules: ['node_modules'],
    },
    watchOptions: { ignored: /node_modules/ },
    module: {
        rules: [
            {
                test: /\.css$/i,
                use: [
                    { loader: 'style-loader' },
                    { loader: 'css-loader', options: { url: false } },
                ],
            },
        ],
    },
};
