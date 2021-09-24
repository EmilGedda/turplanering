// development config
import commonConfig from './common.babel';
import {merge} from 'webpack-merge';
import {resolve} from 'path';

export default merge(commonConfig, {
    mode: 'development',
    entry: [
        'react-hot-loader/patch', // activate HMR for React
        'webpack-dev-server/client?http://localhost:8080',// bundle the client for webpack-dev-server and connect to the provided endpoint
        'webpack/hot/only-dev-server', // bundle the client for hot reloading, only- means to only hot reload for successful updates
        './index.tsx' // the entry point of our app
    ],
    output: {
        path: resolve(__dirname, '../dist'),
        publicPath: '/',
    },
    devServer: {
        hot: true, // enable HMR on the server
        compress: true,
        historyApiFallback:  {
            disableDotRule: true
        },
        static: {
            publicPath: '/',
            directory: resolve(__dirname, '../dist'),
        },
    },
    devtool: 'eval-source-map',
    resolve: {
        alias: {
            'react-dom': '@hot-loader/react-dom',
        },
    }
});
