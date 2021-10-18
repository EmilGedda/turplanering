import React, { useContext, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import WMTS from 'ol/source/WMTS';
import { Options } from 'ol/layer/Layer';
import OLTileLayer from 'ol/layer/Tile';
import TileGrid from 'ol/tilegrid/WMTS';
import TileSource from 'ol/source/Tile';

export type TileLayerProps = Options<WMTS>;

const lantmäterietWMTS = (layer: string) => {
  // TODO: switch on EPSG for 3006 and 3857
  return {
    layer: layer,
    format: 'image/png',
    matrixSet: '3857',
    style: 'default',
    url: 'https://minkarta.lantmateriet.se/map/topowebbcache/',
    tileGrid: new TileGrid({
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

const TileLayer = <T extends TileSource>(opts: Options<T>): JSX.Element => {
  const { map } = useContext(MapContext);

  useEffect(() => {
    if (!map) return;

    console.log(`mounting tile-layer`);
    const tileLayer = new OLTileLayer(opts);
    map.addLayer(tileLayer);

    return () => {
      if (map) {
        map.removeLayer(tileLayer);
        console.log(`unmounting tile-layer`);
      }
    };
  }, [map, opts]);

  return <div />;
};

export default TileLayer;
