import React, { useRef, useState, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import { Coordinate } from 'ol/coordinate';
import * as ol from 'ol';

export type Props = {
  zoom: number;
  className?: string;
  center: Coordinate;
  children?: React.ReactNode;
};

const Map: React.FC<Props> = ({ children, zoom, center, className }) => {
  const mapRef = useRef<HTMLDivElement>(null);
  const [map, setMap] = useState<ol.Map>();

  // on component mount
  useEffect(() => {
    console.log('new map');
    const options = {
      view: new ol.View({ zoom, center }),
      layers: [],
      controls: [],
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
      <div ref={mapRef} className={className}>
        {children}
      </div>
    </MapContext.Provider>
  );
};

export default Map;
