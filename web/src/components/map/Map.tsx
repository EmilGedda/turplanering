import React, { useRef, useState, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import { Coordinate, format } from 'ol/coordinate';
import { Global } from '@emotion/react';
import { toLonLat } from 'ol/proj';
import {
  defaults as defaultControls,
  MousePosition,
  ScaleLine
} from 'ol/control';
import * as ol from 'ol';

export type Props = {
  zoom: number;
  className?: string;
  center: Coordinate;
  children?: React.ReactNode;
};

const mapControls = defaultControls().extend([
  new MousePosition({
    projection: 'EPSG:3857',
    className: 'ol-coord-pos',
    coordinateFormat: (coord) => {
      if (!coord) return '? ?';
      return format(toLonLat(coord), '{y}, {x}', 6);
    }
  }),
  new ScaleLine()
]);

const Map: React.FC<Props> = ({ children, zoom, center, className }) => {
  const mapRef = useRef<HTMLDivElement>(null);
  const [map, setMap] = useState<ol.Map>();

  // on component mount
  useEffect(() => {
    console.log('new map');
    const options = {
      view: new ol.View({
        zoom,
        center
      }),
      layers: [],
      controls: mapControls,
      overlays: []
    };
    const mapObject = new ol.Map(options);
    if (mapRef.current) {
      mapObject.setTarget(mapRef.current);
    }
    setMap(mapObject);
    return () => mapObject.setTarget(undefined);
  }, []);

  return (
    <MapContext.Provider value={{ map }}>
      <Global
        styles={{
          '.ol-coord-pos': {
            bottom: '8px',
            right: '8px',
            position: 'absolute'
          }
        }}
      />
      <div ref={mapRef} className={className}>
        {children}
      </div>
    </MapContext.Provider>
  );
};

export default Map;
