import React, { useRef, useState, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import { Coordinate, format } from 'ol/coordinate';
import { Global } from '@emotion/react';
import { toLonLat } from 'ol/proj';
import type MapEvent from 'ol/MapEvent';
import { updateURL, CoordURL } from '../../URL';
import {
  defaults as defaultControls,
  MousePosition,
  ScaleLine
} from 'ol/control';
import * as ol from 'ol';

export type Props = {
  view: {
    zoom: number;
    center: Coordinate;
  };
  className?: string;
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

const onMoveEnd = (event: MapEvent) => {
  const view = event.map.getView();
  const center = view.getCenter();
  const zoom = view.getZoom();
  if (center) {
    const [lon, lat] = toLonLat(center);
    updateURL(new CoordURL(lat, lon, zoom));
  }
};

const Map: React.FC<Props> = ({ children, view, className }) => {
  const mapRef = useRef<HTMLDivElement>(null);
  const [map, setMap] = useState<ol.Map>();

  // on component mount
  useEffect(() => {
    const { zoom, center } = view;
    const options = {
      view: new ol.View({
        zoom,
        center
      }),
      layers: [],
      controls: mapControls,
      overlays: [],
      keyboardEventTarget: document
    };

    const mapObject = new ol.Map(options);
    if (mapRef.current) {
      mapObject.setTarget(mapRef.current);
    }

    setMap(mapObject);
    mapObject.on('moveend', onMoveEnd);

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
