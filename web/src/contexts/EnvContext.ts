import React from "react";

export type Environment = {
  apiURL: string,
  environment: string,
  tileURL: string,
  browser: {
    hasTouch: boolean
  }
}

export const developmentDefault = {
  apiURL: 'localhost:8080',
  environment: 'development',
  tileURL: 'http://localhost:8081/maps/trails/{z}/{x}/{y}.vector.pbf?debug=true',
  browser: {
    hasTouch: false
  }
}

export default React.createContext<Environment>(developmentDefault);
