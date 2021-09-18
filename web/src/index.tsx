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
  createTheme,
  ThemeProvider,
  Theme,
  StyledEngineProvider,
  alpha,
  adaptV4Theme
} from '@mui/material/styles';
import { orange, common, grey } from '@mui/material/colors';
import { Smhi } from './forecast';
import { App } from './components/App';

declare module '@mui/styles/defaultTheme' {
  // eslint-disable-next-line @typescript-eslint/no-empty-interface
  interface DefaultTheme extends Theme {}
}

const env = (() => {
  const browser = {
    hasTouch: 'ontouchstart' in window || navigator.maxTouchPoints > 0
  };
  if (process.env.NODE_ENV == 'production') {
    return {
      apiURL: 'localhost:8080',
      environment: 'production',
      browser
    };
  }
  return {
    apiURL: 'localhost:8080',
    environment: 'development',
    browser
  };
})();

const theme = {
  light: createTheme(
    adaptV4Theme({
      palette: {
        mode: 'light'
      }
    })
  ),
  dark: createTheme(
    adaptV4Theme({
      palette: {
        mode: 'dark'
      },
      overrides: {
        MuiSlider: {
          // drag color and opacity
          track: { backgroundColor: grey[500] },
          rail: { backgroundColor: grey[600] },
          thumb: {
            backgroundColor: 'white',
            '&$focusVisible,&:hover': {
              boxShadow: `0px 0px 0px 8px ${alpha(common.white, 0.16)}`,
              '@media (hover: none)': {
                boxShadow: 'none'
              }
            },
            '&$active': {
              boxShadow: `0px 0px 0px 14px ${alpha(common.white, 0.16)}`
            }
          }
        },
        MuiIconButton: {
          colorPrimary: {
            color: orange[500]
          }
        }
      }
    })
  )
};

// Opt-in to Webpack hot module replacement
if (module.hot) module.hot.accept();

// TODO: load theme settings from localStorage or account preferences

render(
  <React.StrictMode>
    <StyledEngineProvider injectFirst>
      <ThemeProvider theme={theme.light}>
        <ScopedCssBaseline>
          <App env={env} forecastAPI={Smhi} />
        </ScopedCssBaseline>
      </ThemeProvider>
    </StyledEngineProvider>
  </React.StrictMode>,
  document.getElementById('root') as HTMLElement
);
