import React, { useContext, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import RenderFeature from 'ol/render/Feature';
import Feature from 'ol/Feature';
import { setTooltip } from './Map';
import { Geometry } from 'ol/geom';
import { VectorTile } from 'ol/layer';
import { Style, Stroke } from 'ol/style';
import { MapBrowserEvent } from 'ol';
import { WKB } from 'ol/format';
import type { Options } from 'ol/layer/VectorTile';

const getTrailID = (feature: Feature<Geometry> | RenderFeature) =>
  feature.getProperties()['trail_id'] as number;

const highlightStyleFromFeature = (id: number, offset?: number) => {
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
        lineDashOffset: offset,
        width: 6
      })
    })
  ];
};

export type TrailLayerProps = Omit<Options, 'style'>;

const trailStyleCache = new Map<number, Style[]>();
const trailbgStyle = new Style({
  stroke: new Stroke({
    color: 'white',
    width: 7
  })
});

const getTrailStyle = (id: number) => {
  const styles = trailStyleCache.get(id);
  if (styles) {
    return styles;
  }

  const newStyles = [
    trailbgStyle,
    new Style({
      stroke: new Stroke({
        color: `hsl(${(id * 71) % 360}, 50%, 35%)`,
        lineDash: [4, 5],
        width: 3
      })
    })
  ];

  trailStyleCache.set(id, newStyles);
  return newStyles;
};

export const TrailLayer = React.memo((opts: TrailLayerProps) => {
  const { map } = useContext(MapContext);

  useEffect(() => {
    if (!map) return;
    console.log('creating new TraiLayer');
    const vectorLayer = new VectorTile({
      ...opts,
      style: (feature, _) => {
        const id = getTrailID(feature);
        return getTrailStyle(id);
      }
    });

    map.addLayer(vectorLayer);
    return () => {
      console.log('removing TraiLayer');
      map.removeLayer(vectorLayer);
    };
  }, [map, opts]);

  const tooltip = setTooltip(map);
  let selectedFeatureID: number | null = null;
  let selectionLayer: VectorTile;
  useEffect(() => {
    if (!map) return;
    let hoveredFeatureID: number | null = null;
    let offset = 0;
    setInterval(() => (offset += 0.5), 50);

    selectionLayer = new VectorTile({
      map: map,
      renderMode: 'vector',
      source: opts.source,
      style: (feature) => {
        const id = getTrailID(feature);
        if (id == selectedFeatureID) {
          const style = highlightStyleFromFeature(id, offset);
          selectionLayer.changed();
          return style;
        }

        if (id == hoveredFeatureID) {
          return highlightStyleFromFeature(id);
        }
      }
    });

    const callback = (event: MapBrowserEvent<PointerEvent>) => {
      const prevID = hoveredFeatureID;
      hoveredFeatureID = null;
      let name: string | null = null;
      map.forEachFeatureAtPixel(
        event.pixel,
        (feature) => {
          hoveredFeatureID = getTrailID(feature);
          name = feature.getProperties()['name'] as string;
          return true;
        },
        { hitTolerance: 8 }
      );

      if (prevID != hoveredFeatureID) {
        tooltip(!name ? '' : name);
        selectionLayer.changed();
      }
    };

    map.on('pointermove', callback);
    map.on('movestart', () => {
      hoveredFeatureID = null;
      tooltip('');
    });

    return () => {
      map.removeLayer(selectionLayer);
      map.un('pointermove', callback);
    };
  }, [map]);

  useEffect(() => {
    if (!map) return;
    map.on('click', (event: MapBrowserEvent<PointerEvent>) => {
      map.forEachFeatureAtPixel(
        event.pixel,
        (feat) => {
          const feature = feat as Feature<Geometry>;
          const wkb = feature.getProperties()['extent'] as string;
          const extent = new WKB().readGeometry(wkb).getExtent();
          const view = map.getView();
          selectedFeatureID = getTrailID(feature);
          selectionLayer.changed();

          view.fit(extent, {
            duration: 500,
            size: [550, 550]
          });

          return true;
        },
        { hitTolerance: 8 }
      );
    });
  }, [map]);

  return null;
});

TrailLayer.displayName = 'TrailLayer';
