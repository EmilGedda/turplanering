import React, { useEffect } from 'react';
import { TileWMS } from 'ol/source';
import TileLayer from './TileLayer';
import TileGrid from 'ol/tilegrid/TileGrid';
import type { TileLayerProps } from './TileLayer';

type OverlayProps = Omit<TileLayerProps, 'source'> & {
  referenceTime: Date;
  displayTime?: Date;
};

type SmhiLayerProps = OverlayProps & { layer: string };

const shortISO = (d: Date): string => d.toISOString().slice(0, -5) + 'Z';

export const SmhiLayer = React.memo(
  (props: SmhiLayerProps): JSX.Element | null => {
    const { displayTime, referenceTime, layer, ...opts } = props;

    useEffect(() => {
      console.log('mounting smhi-layer');
      return () => console.log('unmounting smhi-layer');
    }, []);

    if (!displayTime) {
      return null;
    }

    const now = shortISO(referenceTime);

    const wmsSource = new TileWMS({
      params: {
        LAYERS: `${layer}::${now}`,
        dim_reftime: now,
        time: shortISO(displayTime),
        srs: 'EPSG:900913'
      },
      tileGrid: new TileGrid({
        tileSize: 512,
        extent: [
          -20037508.342789244, -20037508.342789244, 20037508.342789244,
          20037508.342789244
        ],
        resolutions: [
          156543.03392804097, 78271.51696402048, 39135.75848201024,
          19567.87924100512, 9783.93962050256, 4891.96981025128,
          2445.98490512564, 1222.99245256282, 611.49622628141, 305.748113140705,
          152.8740565703525, 76.43702828517625, 38.21851414258813,
          19.109257071294063, 9.554628535647032, 4.777314267823516,
          2.388657133911758, 1.194328566955879, 0.5971642834779395,
          0.29858214173896974, 0.14929107086948487, 0.07464553543474244
        ]
      }),
      url: 'https://wts{1-4}.smhi.se/tile/'
    });

    return <TileLayer {...opts} source={wmsSource} />;
  }
);

SmhiLayer.displayName = 'SmhiLayer';
