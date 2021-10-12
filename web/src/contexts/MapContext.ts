import React from 'react';
import * as ol from 'ol';

export type MapContext = {
  map: ol.Map | undefined;
};

export default React.createContext<MapContext>({
  map: undefined
});
