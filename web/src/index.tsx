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
import ScopedCssBaseline from '@mui/material/ScopedCssBaseline';
import { StyledEngineProvider, ThemeProvider } from '@mui/material/styles';
import { App } from './components/App';
import EnvContext, {
  Environment,
  developmentDefault
} from './contexts/EnvContext';
import proj4 from 'proj4';
import * as ol from 'ol/proj/proj4';
import { theme } from './Theme';

const env: Environment = (() => {
  const browser = {
    hasTouch: 'ontouchstart' in window || navigator.maxTouchPoints > 0
  };
  if (process.env.NODE_ENV == 'production') {
    return {
      ...developmentDefault,
      environment: 'production',
      tileURL: '',
      base: window.location.pathname,
      browser
    };
  }
  return {
    ...developmentDefault,
    browser
  };
})();

// Opt-in to Webpack hot module replacement
if (module.hot) module.hot.accept();

proj4.defs(
  'EPSG:3006',
  '+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
);

ol.register(proj4);
// TODO: load theme settings from localStorage or account preferences

render(
  <React.StrictMode>
    <StyledEngineProvider injectFirst>
      <EnvContext.Provider value={env}>
        <ThemeProvider theme={theme.light}>
          <ScopedCssBaseline>
            <App />
          </ScopedCssBaseline>
        </ThemeProvider>
      </EnvContext.Provider>
    </StyledEngineProvider>
  </React.StrictMode>,
  document.getElementById('root') as HTMLElement
);
