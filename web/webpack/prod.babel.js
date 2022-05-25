// production config
import commonConfig from './common.babel';
import {merge} from 'webpack-merge';
import {resolve} from 'path';

export default merge(commonConfig, {
    mode: 'production',
    entry: './index.tsx',
    output: {
        filename: 'bundle.[contenthash:8].min.js',
        path: resolve(__dirname, '../dist'),
        publicPath: '/turplanering/',
    },
    devtool: 'source-map',
});

