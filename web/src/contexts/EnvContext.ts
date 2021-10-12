import React from 'react';
import { ForecastAPI, Smhi } from '../Forecast';

export type Environment = {
  apiURL: string;
  environment: string;
  tileURL: string;
  forecastAPI: ForecastAPI;
  browser: {
    hasTouch: boolean;
  };
};

export const developmentDefault = {
  apiURL: 'localhost:8080',
  environment: 'development',
  tileURL:
    'http://localhost:8081/maps/trails/{z}/{x}/{y}.vector.pbf?debug=true',
  forecastAPI: Smhi,
  browser: {
    hasTouch: false
  }
};

export default React.createContext<Environment>(developmentDefault);
