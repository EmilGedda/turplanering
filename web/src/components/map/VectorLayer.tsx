import React, { useContext, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import { setTooltip } from './Map';
import Feature from 'ol/Feature';
import { Geometry } from 'ol/geom';
import { VectorTile } from 'ol/layer';
import { Style, Stroke } from 'ol/style';
import { MapBrowserEvent } from 'ol';
import type { Options } from 'ol/layer/VectorTile';
import RenderFeature from 'ol/render/Feature';

const getTrailID = (feature: Feature<Geometry> | RenderFeature) =>
  feature.getProperties()['trail_id'] as number;

const highlightStyleFromFeature = (id: number) => {
  return [
    new Style({
      stroke: new Stroke({
        color: 'white',
        width: 10
      })
    }),
    new Style({
      stroke: new Stroke({
        color: `hsl(${(id * 71) % 360}, 50%, 35%)`,
        lineDash: [6, 8],
        width: 6
      })
    })
  ];
};

export type TrailLayerProps = Omit<Options, 'style'>;

export const TrailLayer: React.FC<TrailLayerProps> = (opts) => {
  const { map } = useContext(MapContext);

  useEffect(() => {
    let selectedID: number | null = null;
    const vectorLayer = new VectorTile({
      ...opts,
      style: (feature, _) => {
        const id = getTrailID(feature);

        return [
          new Style({
            stroke: new Stroke({
              color: 'white',
              width: 7
            })
          }),
          new Style({
            stroke: new Stroke({
              color: `hsl(${(id * 71) % 360}, 50%, 35%)`,
              lineDash: [4, 5],
              width: 3
            })
          })
        ];
      }
    });

    const selectionLayer = new VectorTile({
      map: map,
      renderMode: 'vector',
      source: opts.source,
      style: (feature) => {
        const id = getTrailID(feature);
        if (id == selectedID) {
          return highlightStyleFromFeature(id);
        }
      }
    });

    const callback = (event: MapBrowserEvent<PointerEvent>) => {
      const prevID = selectedID;
      selectedID = null;
      let name: string | null = null;
      map?.forEachFeatureAtPixel(
        event.pixel,
        (feature) => {
          selectedID = getTrailID(feature);
          name = feature.getProperties()['name'] as string;
          return true;
        },
        { hitTolerance: 8 }
      );
      if (prevID != selectedID) {
        setTooltip(map)(!name ? '' : name);
        selectionLayer.changed();
      }
    };

    map?.on('pointermove', callback);
    map?.addLayer(vectorLayer);

    return () => {
      map?.removeLayer(vectorLayer);
      map?.removeLayer(selectionLayer);
      map?.un('pointermove', callback);
    };
  }, [map, opts]);

  return null;
};
