import React, { useState, useEffect, useContext, useCallback } from 'react';
import LayerSelector, { BaseLayers, Layer } from './LayerSelector';
import { styled } from '@mui/material/styles';
import Searchbar from './Searchbar';
import { Slide } from '@mui/material';
import {
  Overlaybar,
  WeatherToggleButton,
  WindToggleButton,
  TemperatureToggleButton
} from './Overlaybar';
import Map from './map/Map';
import TileLayer, {
  satelliteSource,
  topowebbBWSource,
  topowebbSource
} from './map/TileLayer';
import Timeline from './Timeline';
import Feature from 'ol/Feature';
import View from 'ol/View';
import { MVT } from 'ol/format';
import { TrailLayer } from './map/VectorLayer';
import { UrlTile, TileImage } from 'ol/source';
import { VectorTile as VectorSource } from 'ol/source';
import { fromLonLat } from 'ol/proj';
import { CoordURL, currentURLState } from '../URL';
import { ForecastTimestamps } from '../Forecast';
import { SmhiLayer } from './map/WeatherOverlays';
import { Layers, Overlays } from './map/Layers';
import EnvContext from '../contexts/EnvContext';
import { Events, MoveEnd } from './map/Events';
import * as ol from 'ol';

const PREFIX = 'App';

const classes = {
  fullscreen: `${PREFIX}-fullscreen`,
  padded: `${PREFIX}-padded`,
  topbar: `${PREFIX}-topbar`,
  hiddenBox: `${PREFIX}-hiddenBox`
};

const Div = styled('div')(({ theme }) => ({
  [`& .${classes.fullscreen}`]: {
    position: 'absolute',
    top: 0,
    right: 0,
    height: '100%',
    width: '100vw'
  },
  [`&.${classes.padded}`]: {
    [theme.breakpoints.down('md')]: {
      paddingTop: '5px',
      paddingRight: '10px',
      paddingLeft: '10px'
    },
    [theme.breakpoints.up('sm')]: {
      paddingTop: '15px',
      paddingRight: '25px',
      paddingLeft: '25px'
    }
  },
  [`&.${classes.topbar}`]: {
    position: 'relative',
    width: '100%',
    display: 'flex',
    justifyContent: 'space-between',
    pointerEvents: 'none',
    zIndex: 1001
  },
  [`&.${classes.hiddenBox}`]: {
    width: 64,
    [theme.breakpoints.down('md')]: {
      display: 'none'
    }
  }
}));

const initialView = (() => {
  const { state } = currentURLState();
  return state instanceof CoordURL
    ? { center: fromLonLat([state.lon, state.lat]), zoom: state.zoom ?? 8 }
    : { center: fromLonLat([18.07, 59.324]), zoom: 8 };
})();

const trailSource = new VectorSource({
  format: new MVT({
    featureClass: Feature,
    layerName: 'trail_sections',
    geometryName: 'geom',
    idProperty: 'gid'
  })
});

const centerTileURL = (source: UrlTile, view: View, offset: number) => {
  const center = view.getCenter();
  const zoom = view.getZoom();
  if (!center || !zoom) return;

  const coord = source
    .getTileGrid()
    .getTileCoordForCoordAndZ(center, Math.min(Math.ceil(zoom + offset), 14));
  return source.getTileUrlFunction()(coord, 1, source.getProjection());
};

const getPreviews = (view: View) => {
  return {
    topo: centerTileURL(topowebbSource, view, 1),
    topoBW: centerTileURL(topowebbBWSource, view, 1),
    satellite: centerTileURL(satelliteSource, view, -2)
  };
};

export const App = (): JSX.Element => {
  const env = useContext(EnvContext);

  const [showBar, setShowBar] = useState(false);
  const [displayIndex, setDisplayIndex] = useState<number>(0);
  const [hasForecast, setHasForecast] = useState<boolean>(false);
  const [forecast, setForecast] = useState<[string, ForecastTimestamps]>();
  const [mapSource, setMapSource] = useState<TileImage>(topowebbSource);
  const [layerPreview, setLayerPreview] = useState<{
    topo?: string;
    topoBW?: string;
    satellite?: string;
  }>({});

  const mapMount = useCallback((map: ol.Map) => {
    setLayerPreview(getPreviews(map.getView()));
  }, []);

  useEffect(() => {
    console.log('Running in ' + env.environment);
    trailSource.setUrl(env.tileURL);
  }, [env]);

  useEffect(() => {
    const timeout = setTimeout(() => setShowBar(true), 500);
    return () => clearTimeout(timeout);
  }, []);

  const displayTime = forecast
    ? forecast[1].validTimes[displayIndex]
    : undefined;

  const nextDisplayTime = forecast
    ? forecast[1].validTimes[(displayIndex + 1) % forecast[1].validTimes.length]
    : undefined;

  const showSidebar = hasForecast && showBar;

  return (
    <Div className={`${classes.padded} ${classes.fullscreen}`}>
      <Map view={initialView} className={classes.fullscreen} onMount={mapMount}>
        <Layers>
          <TileLayer source={mapSource} />
          <TrailLayer source={trailSource} />
        </Layers>

        <Overlays>
          {forecast && (
            <>
              <SmhiLayer
                layer={forecast[0]}
                opacity={0.75}
                referenceTime={forecast[1].reference}
                displayTime={displayTime}
              />

              <SmhiLayer
                layer={forecast[0]}
                opacity={0}
                referenceTime={forecast[1].reference}
                displayTime={nextDisplayTime}
              />
            </>
          )}
        </Overlays>

        <Events>
          <MoveEnd
            callback={(event) => {
              const view = event.map.getView();
              setLayerPreview(getPreviews(view));
            }}
          />
        </Events>
      </Map>

      {/*

        <WMSLayer // Hillshading layer
          url='https://minkarta.lantmateriet.se/map/hojdmodell/'
          layers='terrangskuggning'
          format='image/png'
          transparent={true}
          opacity={0.25}
        />

        <WMSLayer // Satellite
          url='https://minkarta.lantmateriet.se/map/ortofoto/'
          layers='Ortofoto_0.5,Ortofoto_0.4,Ortofoto_0.25,Ortofoto_0.16'
        />

      */}

      <Slide direction='down' in={showBar}>
        <Div className={classes.topbar}>
          <Div className={classes.hiddenBox} />

          <Searchbar
            onGPSLocate={console.log}
            onGPSTrack={console.log}
            onGPSDeactivate={console.log}
          />

          <LayerSelector>
            <BaseLayers>
              <Layer
                name='Terräng'
                defaultSelected
                previewURL={layerPreview.topo}
                onClick={() => setMapSource(topowebbSource)}
              />
              <Layer
                name='Nedtonad'
                previewURL={layerPreview.topoBW}
                onClick={() => setMapSource(topowebbBWSource)}
              />
              <Layer
                name='Flygfoto'
                previewURL={layerPreview.satellite}
                onClick={() => setMapSource(satelliteSource)}
              />
            </BaseLayers>
          </LayerSelector>
        </Div>
      </Slide>

      <Overlaybar
        onClick={setForecast}
        onForecastLoad={setHasForecast}
        shown={showSidebar}
      >
        <WeatherToggleButton layer='pmpfrekvent:wpt-overview_n-europe__' />
        <WindToggleButton layer='pmp:windspeed-windarrows-avg-10m_n-europe__' />
        <TemperatureToggleButton layer='pmpfrekvent:temperature-2m_n-europe_rainbow_' />
      </Overlaybar>

      {forecast && (
        <Timeline
          timepoints={forecast[1].validTimes}
          onChange={setDisplayIndex}
        />
      )}
    </Div>
  );
};
