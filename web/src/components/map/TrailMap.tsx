import React, { FC } from 'react';
import {
  Map,
  MapProps,
  AttributionControl,
  Children,
  ScaleControl,
  Circle,
  CircleMarker
} from 'react-leaflet';
import { LatLngTuple, CRS } from 'leaflet';
import { WMSLayer, BufferedWMSLayerProps } from './WMSTileLayer';

type GPSMarkerProps = { pos: Coordinates };

export const GPSMarker: React.FC<GPSMarkerProps> = ({ pos }) => {
  const center: LatLngTuple = [pos.latitude, pos.longitude];
  return (
    <>
      <Circle radius={pos.accuracy} center={center} />
      <CircleMarker
        radius={5}
        center={center}
        fillOpacity={1.0}
        color='#0c31eb'
      />
    </>
  );
};

type OverlayProps = Omit<BufferedWMSLayerProps, 'url'> & {
  referenceTime: Date;
  displayTime: Date;
};

type SmhiLayerProps = OverlayProps & { layer: string };

const shortISO = (d: Date): string => d.toISOString().slice(0, -5) + 'Z';

const SmhiLayer: FC<SmhiLayerProps> = (props) => {
  const { displayTime, referenceTime, layer, ...baseProps } = props;
  const now = shortISO(referenceTime);

  return (
    <WMSLayer
      {...baseProps}
      url='https://wts{s}.smhi.se/tile/'
      subdomains='1234'
      transparent={true}
      tileSize={512}
      crs={CRS.EPSG900913}
      format='image/png'
      dim_reftime={now}
      time={shortISO(displayTime)}
      layers={`pmpfrekvent:${layer}::${now}`}
    />
  );
};

export const WeatherLayer: FC<Omit<OverlayProps, 'layer'>> = (props) => {
  return (
    <SmhiLayer transparent={true} {...props} layer='wpt-overview_n-europe__' />
  );
};

export const TemperatureLayer: FC<Omit<OverlayProps, 'layer'>> = (props) => {
  return (
    <SmhiLayer
      layer='temperature-2m_n-europe_rainbow_'
      opacity={0.5}
      {...props}
    />
  );
};

type Props = Omit<
  MapProps,
  'children' | 'useFlyTo' | 'attributionControl' | 'zoomSnap'
> & {
  children?: Children;
  showTemperature?: boolean;
  showWind?: boolean;
};

export const TrailMap = React.forwardRef<Map, Props>((props, ref) => {
  const { children, ...baseProps } = props;
  return (
    <Map
      {...baseProps}
      useFlyTo={true}
      attributionControl={false}
      zoomSnap={0}
      zoomControl={false}
      ref={ref}
    >
      {children}
      <ScaleControl position='bottomleft' />
      <AttributionControl position='bottomright' prefix={false} />
    </Map>
  );
});

TrailMap.displayName = 'TrailMap';
