import React, { useRef, useState, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import { styled, Tooltip } from '@mui/material';
import { tooltipClasses } from '@mui/material/Tooltip';
import type { TooltipProps } from '@mui/material/Tooltip';
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
  setTooltipText: React.Dispatch<React.SetStateAction<string>>;
};

const mapControls = defaultControls().extend([
  new MousePosition({
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

const nofunc = (_: string) => {
  return;
};
type StateCallBack<T> = (text: T) => void;
export const setTooltip = (map?: ol.Map): StateCallBack<string> => {
  const func = map?.get('tooltip') as StateCallBack<string> | undefined;
  return func ?? nofunc;
};

const MemoMap: React.FC<Props> = React.memo(
  ({ children, view, className, setTooltipText }) => {
    const mapRef = useRef<HTMLDivElement>(null);
    const [map, setMap] = useState<ol.Map>();

    console.log('rendering map');

    useEffect(() => {
      const { zoom, center } = view;
      const options = {
        wrapX: true,
        view: new ol.View({
          zoom,
          center
        }),
        layers: [],
        controls: mapControls,
        overlays: [],
        keyboardEventTarget: document
      };

      console.log('mounting map');
      const mapObject = new ol.Map(options);
      if (mapRef.current) {
        mapObject.setTarget(mapRef.current);
      }

      setMap(mapObject);
      mapObject.on('moveend', onMoveEnd);
      mapObject.set('tooltip', setTooltipText, true);

      return () => {
        mapObject.setTarget(undefined);
        console.log('unmounting map');
      };
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
  }
);

MemoMap.displayName = 'MemoMap';

const LargeTooltip = styled(({ className, ...props }: TooltipProps) => (
  <Tooltip {...props} classes={{ popper: className }} />
))((_) => ({
  [`& .${tooltipClasses.tooltip}`]: {
    fontSize: 16
  }
}));

const Map = React.memo((props: Omit<Props, 'setTooltipText'>) => {
  const [tooltip, setTooltip] = useState<string>('');
  return (
    <LargeTooltip
      title={tooltip}
      placement='top'
      followCursor
      open={true}
      disableFocusListener
      disableHoverListener
      disableTouchListener
    >
      <div className={props.className}>
        <MemoMap {...props} setTooltipText={setTooltip} />
      </div>
    </LargeTooltip>
  );
});

Map.displayName = 'Map';

export default Map;
