// development config
import commonConfig from './common.babel';
import {merge} from 'webpack-merge';
import webpack from 'webpack';

export default merge(commonConfig, {
    mode: 'development',
    entry: [
        'react-hot-loader/patch', // activate HMR for React
        'webpack-dev-server/client?http://localhost:8080',// bundle the client for webpack-dev-server and connect to the provided endpoint
        'webpack/hot/only-dev-server', // bundle the client for hot reloading, only- means to only hot reload for successful updates
        './index.tsx' // the entry point of our app
    ],
    devServer: {
        hot: true, // enable HMR on the server
    },
    devtool: 'eval-source-map',
    plugins: [
        new webpack.HotModuleReplacementPlugin(), // enable HMR globally
    ],
});
