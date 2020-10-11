// shared config (dev and prod)
import {resolve} from 'path';
import ForkTsCheckerWebpackPlugin from 'fork-ts-checker-webpack-plugin';
import HtmlWebpackPlugin from 'html-webpack-plugin';
import webpack from 'webpack'

export default {
    resolve: {
        extensions: ['.ts', '.tsx', '.js', '.jsx', '.json'],
    },
    context: resolve(__dirname, '../src'),
    module: {
        rules: [
            {
                test: /\.js$/,
                use: ['babel-loader', 'source-map-loader'],
                exclude: /node_modules/
            },
            {
                test: /\.tsx?$/,
                exclude: /node_modules/,
                use: {
                    loader: 'ts-loader',
                    options: {
                        'transpileOnly': true
                    }
                }
            },
            {
                test: /\.(jpe?g|png|gif|svg)$/i,
                use: {
                    loader: 'file-loader',
                    options: {
                        hash: 'sha256',
                        digest: 'hex',
                        name: 'img/[contenthash:8].[ext]'
                    }
                }
            }
        ]
    },
    plugins: [
        new ForkTsCheckerWebpackPlugin({typescript: {configFile: '../tsconfig.json'}}),
        new HtmlWebpackPlugin({
            template: 'index.html',
            favicon: '../assets/favicon.svg'
        }),
        new webpack.BannerPlugin({
            banner:
                `Turplanering
Copyright (C) 2020 Emil Gedda

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
`
        })
    ],
    externals: {
        'react': 'React',
        'react-dom': 'ReactDOM',
        'leaflet': 'L',
    },
};

