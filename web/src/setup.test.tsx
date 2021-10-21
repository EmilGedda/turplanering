/* eslint-disable */
import React, { FC } from 'react';
import 'whatwg-fetch';
import { StyledEngineProvider, ThemeProvider } from '@mui/material';
import { server } from './Mocks';
import {render, RenderOptions} from '@testing-library/react'
import EnvContext, { developmentDefault } from './contexts/EnvContext';
import { theme } from './Theme';
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

const env = {
  ...developmentDefault,
  forecastAPI: new Smhi({ baseURL: '' })
}

const ctxProviders: FC = ({children}) => {
  return (
    <StyledEngineProvider injectFirst>
      <EnvContext.Provider value={env}>
        <ThemeProvider theme={theme.light}>
            {children}
        </ThemeProvider>
      </EnvContext.Provider>
    </StyledEngineProvider>
  );
}

const customRender = (
  ui: React.ReactElement,
  options?: Omit<RenderOptions, 'wrapper'>,
) => render(ui, {wrapper: ctxProviders, ...options})

export * from '@testing-library/react'
export {customRender as render}
