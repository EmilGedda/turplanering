///<reference types="webpack-env" />
/** @license
 *
 *  Turplanering
 *  Copyright (C) 2020 Emil Gedda
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import { render } from 'react-dom';
import ScopedCssBaseline from '@material-ui/core/ScopedCssBaseline';
import { Smhi } from './forecast';
import { App } from './components/App';

const env = (() => {
  const browser = {
    hasTouch: 'ontouchstart' in window || navigator.maxTouchPoints > 0,
  };
  if (process.env.NODE_ENV == 'production') {
    return {
      apiURL: 'localhost:8080',
      environment: 'production',
      browser,
    };
  }
  return {
    apiURL: 'localhost:8080',
    environment: 'development',
    browser,
  };
})();

// Opt-in to Webpack hot module replacement
if (module.hot) module.hot.accept();

render(
  <React.StrictMode>
    <ScopedCssBaseline>
      <App env={env} forecastAPI={Smhi} />
    </ScopedCssBaseline>
  </React.StrictMode>,
  document.getElementById('root') as HTMLElement
);
