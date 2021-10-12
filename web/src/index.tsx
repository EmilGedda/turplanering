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
import {
  alpha,
  createTheme,
  StyledEngineProvider,
  Theme,
  ThemeProvider
} from '@mui/material/styles';
import { orange, common, grey } from '@mui/material/colors';
import { Smhi } from './Forecast';
import { App } from './components/App';
import EnvContext, {
  Environment,
  developmentDefault
} from './contexts/EnvContext';
import proj4 from 'proj4';
import * as ol from 'ol/proj/proj4';

declare module '@mui/material/styles' {
  // eslint-disable-next-line
  interface DefaultTheme extends Theme{}
}

const env: Environment = (() => {
  const browser = {
    hasTouch: 'ontouchstart' in window || navigator.maxTouchPoints > 0
  };
  if (process.env.NODE_ENV == 'production') {
    return {
      ...developmentDefault,
      apiURL: 'localhost:8080',
      environment: 'production',
      tileURL: '',
      browser
    };
  }
  return {
    ...developmentDefault,
    browser
  };
})();

const theme = {
  light: createTheme({
    palette: {
      mode: 'light'
    }
  }),
  dark: createTheme({
    palette: {
      mode: 'dark'
    },
    components: {
      MuiSlider: {
        styleOverrides: {
          // drag color and opacity
          track: { backgroundColor: grey[500] },
          rail: { backgroundColor: grey[600] },
          thumb: {
            backgroundColor: 'white',
            '&.Mui-focusVisible,&:hover': {
              boxShadow: `0px 0px 0px 8px ${alpha(common.white, 0.16)}`,
              '@media (hover: none)': {
                boxShadow: 'none'
              }
            },
            '&.Mui-active': {
              boxShadow: `0px 0px 0px 14px ${alpha(common.white, 0.16)}`
            }
          }
        }
      },
      MuiIconButton: {
        styleOverrides: {
          colorPrimary: {
            color: orange[500]
          }
        }
      }
    }
  })
};

// Opt-in to Webpack hot module replacement
if (module.hot) module.hot.accept();

proj4.defs(
  'EPSG:3006',
  '+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
);
proj4.defs('SWEREF99', proj4.defs('EPSG:3006'));
proj4.defs('urn:ogc:def:crs:EPSG:6.3:3006', proj4.defs('EPSG:3006'));
proj4.defs('urn:ogc:def:crs:EPSG:6.3:3857', proj4.defs('EPSG:3857'));
ol.register(proj4);

//export const MAP_EXTENT = [-1200000, 4700000, 2600000, 8500000];
// export const MAP_RESOLUTIONS = [4096.0, 2048.0, 1024.0, 512.0, 256.0, 128.0, 64.0, 32.0, 16.0, 8.0, 4.0, 2.0, 1.0, 0.5, 0.25, 0.125];
// export const MAP_WMTS_RESOLUTIONS = [4096.0, 2048.0, 1024.0, 512.0, 256.0, 128.0, 64.0, 32.0, 16.0, 8.0, 4.0, 2.0, 1.0, 0.5];

// TODO: load theme settings from localStorage or account preferences

render(
  <React.StrictMode>
    <StyledEngineProvider injectFirst>
      <EnvContext.Provider value={env}>
        <ThemeProvider theme={theme.light}>
          <ScopedCssBaseline>
            <App env={env} forecastAPI={Smhi} />
          </ScopedCssBaseline>
        </ThemeProvider>
      </EnvContext.Provider>
    </StyledEngineProvider>
  </React.StrictMode>,
  document.getElementById('root') as HTMLElement
);
