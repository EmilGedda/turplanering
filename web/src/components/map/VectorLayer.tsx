import React, { useContext, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import Feature from 'ol/Feature';
import { Geometry } from 'ol/geom';
import { VectorTile } from 'ol/layer';
import { Style, Stroke } from 'ol/style';
import { MapBrowserEvent } from 'ol';
import type { Options } from 'ol/layer/VectorTile';

export type TrailLayerProps = Omit<Options, 'style'>;

interface IDable {
  getId(): string | number | undefined;
}

const parseID = (feature: IDable) => {
  if (feature == null || feature.getId() == undefined) {
    return null;
  }

  const id = feature.getId() ?? -1;
  return typeof id === 'string' ? parseInt(id) : Math.round(id);
};

const highlightStyleFromFeature = (feature: IDable) => {
  const id = parseID(feature) ?? 0;

  return [
    new Style({
      stroke: new Stroke({
        color: 'white',
        width: 10
      })
    }),
    new Style({
      stroke: new Stroke({
        color: `hsl(${id % 360}, 50%, 35%)`,
        lineDash: [6, 8],
        width: 6
      })
    })
  ];
};

export const TrailLayer: React.FC<TrailLayerProps> = (opts) => {
  const { map } = useContext(MapContext);

  useEffect(() => {
    let selectedID: number | null = -1;
    const vectorLayer = new VectorTile({
      ...opts,
      style: (f, _) => {
        const feature = f as Feature<Geometry>;
        const id = parseID(feature) ?? 0;

        return [
          new Style({
            stroke: new Stroke({
              color: 'white',
              width: 7
            })
          }),
          new Style({
            stroke: new Stroke({
              color: `hsl(${id % 360}, 50%, 35%)`,
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
        if (parseID(feature) == selectedID) {
          return highlightStyleFromFeature(feature);
        }
      }
    });

    const callback = (event: MapBrowserEvent<PointerEvent>) => {
      const prevID = selectedID;
      selectedID = null;
      map?.forEachFeatureAtPixel(
        event.pixel,
        (feature) => {
          selectedID = parseID(feature);
          return true;
        },
        { hitTolerance: 8 }
      );
      if (prevID != selectedID) {
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
