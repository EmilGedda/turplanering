import React, { useContext, useEffect, useState } from 'react';
import MapContext from '../../contexts/MapContext';
import WMTS from 'ol/source/WMTS';
import { get as getProjection } from 'ol/proj';
import { Options } from 'ol/layer/Layer';
import OLTileLayer from 'ol/layer/Tile';
import TileGrid from 'ol/tilegrid/TileGrid';
import WMTSTileGrid from 'ol/tilegrid/WMTS';
import { TileWMS, TileImage as TileSource } from 'ol/source';

export type TileLayerProps = Options<WMTS>;

const lantmäterietWMTS = (layer: string) => {
  // TODO: switch on EPSG for 3006 and 3857
  return {
    layer: layer,
    format: 'image/png',
    matrixSet: '3857',
    style: 'default',
    url: 'https://minkarta.lantmateriet.se/map/topowebbcache/',
    tileGrid: new WMTSTileGrid({
      matrixIds: Array.from(Array(18).keys(), (n) => n.toString()),
      tileSize: 256,
      extent: [
        -20037508.342789, -20037508.342789, 20037508.342789, 20037508.342789
      ],
      resolutions: [
        156543.033928041, 78271.51696402048, 39135.758482010235,
        19567.87924100512, 9783.93962050256, 4891.96981025128, 2445.98490512564,
        1222.99245256282, 611.49622628141, 305.7481131407048, 152.8740565703525,
        76.43702828517624, 38.21851414258813, 19.10925707129406,
        9.554628535647032, 4.777314267823516, 2.388657133911758,
        1.194328566955879
      ]
    })
  };
};

export const topowebbSource = new WMTS(lantmäterietWMTS('topowebb'));
export const topowebbBWSource = new WMTS(lantmäterietWMTS('topowebb_nedtonad'));

export const satelliteSource = new TileWMS({
  url: 'https://minkarta.lantmateriet.se/map/ortofoto/',
  projection: getProjection('EPSG:3857'),
  params: {
    LAYERS: 'Ortofoto_0.5,Ortofoto_0.4,Ortofoto_0.25,Ortofoto_0.16',
    VERSION: '1.1.1',
    SRS: 'EPSG:3857',
    TILED: 'true',
    FORMAT: 'image/jpeg',
    TRANSPARENT: 'false'
  },
  tileGrid: new TileGrid({
    tileSize: 512,
    extent: [-1200000, 4700000, 2600000, 8500000],
    resolutions: [
      4096.0, 2048.0, 1024.0, 512.0, 256.0, 128.0, 64.0, 32.0, 16.0, 8.0, 4.0,
      2.0, 1.0, 0.5, 0.25, 0.125
    ]
  })
});

const TileLayer = <T extends TileSource>(opts: Options<T>): JSX.Element => {
  const { map } = useContext(MapContext);
  const [tileLayer, _] = useState(() => new OLTileLayer(opts));

  useEffect(() => {
    if (!map) return;

    console.log(`mounting tile-layer`);
    map.addLayer(tileLayer);

    return () => {
      if (map) {
        map.removeLayer(tileLayer);
        console.log(`unmounting tile-layer`);
      }
    };
  }, [map]);

  useEffect(() => {
    if (opts.source) tileLayer.setSource(opts.source);
  }, [opts.source]);

  return <div />;
};

export default TileLayer;
