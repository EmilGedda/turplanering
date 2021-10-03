import React from 'react';
import { TileWMS } from 'ol/source';
import TileLayer from './TileLayer';
import TileGrid from 'ol/tilegrid/TileGrid';
import { get as getProjection } from 'ol/proj';
import { getWidth } from 'ol/extent';
import type { TileLayerProps } from './TileLayer';

type OverlayProps = Omit<TileLayerProps, 'source'> & {
  referenceTime: Date;
  displayTime: Date;
};

type SmhiLayerProps = OverlayProps & { layer: string };

const shortISO = (d: Date): string => d.toISOString().slice(0, -5) + 'Z';

const projExtent = getProjection('EPSG:3857').getExtent();
const startResolution = getWidth(projExtent) / 256;
const resolutions = new Array<number>(22);
for (let i = 0; i < resolutions.length; ++i) {
  resolutions[i] = startResolution / Math.pow(2, i);
}

const SmhiLayer = (props: SmhiLayerProps) => {
  const { displayTime, referenceTime, layer, ...opts } = props;
  const now = shortISO(referenceTime);
  console.log('rendering smhi-layer');
  const wmsSource = new TileWMS({
    projection: 'EPSG:900913',
    params: {
      LAYERS: `pmpfrekvent:${layer}::${now}`,
      dim_reftime: now,
      time: shortISO(displayTime),
      srs: 'EPSG:900913'
    },
    tileGrid: new TileGrid({
      tileSize: 512,
      extent: projExtent,
      resolutions: resolutions
    }),
    urls: [
      'https://wts1.smhi.se/tile/',
      'https://wts2.smhi.se/tile/',
      'https://wts3.smhi.se/tile/',
      'https://wts4.smhi.se/tile/'
    ]
  });

  return <TileLayer {...opts} source={wmsSource} />;
};

export const WeatherLayer = React.memo((props: OverlayProps) => {
  return <SmhiLayer {...props} layer='wpt-overview_n-europe__' />;
});

WeatherLayer.displayName = 'WeatherLayer';

export const TemperatureLayer = React.memo((props: OverlayProps) => {
  return (
    <SmhiLayer
      layer='temperature-2m_n-europe_rainbow_'
      opacity={0.5}
      {...props}
    />
  );
});

TemperatureLayer.displayName = 'TemperatureLayer';
