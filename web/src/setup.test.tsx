/* eslint-disable */
import React from 'react';
import 'whatwg-fetch';
import { StyledEngineProvider, Theme, ThemeProvider } from '@mui/material';
import { server } from './Mocks';
import {render, RenderOptions} from '@testing-library/react'
import EnvContext, { developmentDefault, Environment } from './contexts/EnvContext';
import { theme as appTheme } from './Theme';
import { Smhi } from './Forecast';


// @ts-ignore
jsdom.reconfigure({
  pretendToBeVisual: true
});

// Establish API mocking before all tests.
beforeAll(() => server.listen());

// Reset any request handlers that we may add during the tests,
// so they don't affect other tests.
afterEach(() => server.resetHandlers());

// Clean up after the tests are finished.
afterAll(() => server.close());

export const testEnv: Environment = {
  ...developmentDefault,
  forecastAPI: new Smhi({ baseURL: '' })
}

export type RenderArgs = {
  env: Environment,
  theme: Theme
}

type RenderFunction = (args?: Partial<RenderArgs> | undefined) => 
    (ui: React.ReactElement, options?: Omit<RenderOptions, "wrapper"> | undefined) => void;

export const makeRender: RenderFunction = (args?: Partial<RenderArgs>) => {
  const def = {
    env: testEnv,
    theme: appTheme.light
  }

  Object.assign(def, args);

  const wrapper = ({children}: any) => (
    <StyledEngineProvider injectFirst>
      <EnvContext.Provider value={def.env}>
        <ThemeProvider theme={def.theme}>
          {children}
        </ThemeProvider>
      </EnvContext.Provider>
    </StyledEngineProvider>
  )

  return (ui: React.ReactElement, options?: Omit<RenderOptions, 'wrapper'>) => {
    render(ui, {wrapper, ...options})
  }
}


const customRender = makeRender();

export * from '@testing-library/react'
export {customRender as render}
