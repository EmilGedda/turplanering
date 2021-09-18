import { useContext, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import OLVectorLayer from 'ol/layer/VectorTile';
import type { Options } from 'ol/layer/VectorTile';

export type VectorLayerProps = Options;

const VectorLayer: React.FC<VectorLayerProps> = (opts) => {
  const { map } = useContext(MapContext);

  useEffect(() => {
    if (!map) return;

    const vectorLayer = new OLVectorLayer(opts);
    map.addLayer(vectorLayer);

    return () => {
      if (map) {
        map.removeLayer(vectorLayer);
      }
    };
  }, [map, opts]);
  return null;
};

export default VectorLayer;
