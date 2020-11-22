import React, { useState, useEffect, createRef } from 'react';
import { FeatureGroup, Map, Viewport } from 'react-leaflet';
import { Slide } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';
import Searchbar from './Searchbar';
import LayerSelector from './LayerSelector';
import {
  Overlaybar,
  WeatherToggleButton,
  WindToggleButton,
  TemperatureToggleButton
} from './Overlaybar';
import Timeline from './Timeline';
import { WMSLayer } from './map/WMSTileLayer';
import { ForecastAPI, ForecastTimestamps } from '../forecast';
import {
  TrailMap,
  GPSMarker,
  WeatherLayer,
  TemperatureLayer
} from './map/TrailMap';

type Environment = {
  apiURL: string;
  environment: string;
  browser: {
    hasTouch: boolean;
  };
};

type Props = {
  env: Environment;
  forecastAPI: ForecastAPI;
};

const appStyles = makeStyles((theme) => ({
  fullscreen: {
    position: 'absolute',
    top: 0,
    right: 0,
    height: '100%',
    width: '100vw',
    [theme.breakpoints.down('sm')]: {
      paddingTop: 5,
      paddingRight: 10,
      paddingLeft: 10
    },
    [theme.breakpoints.up('sm')]: {
      paddingTop: 15,
      paddingRight: 25,
      paddingLeft: 25
    }
  },
  topbar: {
    position: 'relative',
    width: '100%',
    display: 'flex',
    justifyContent: 'space-between',
    zIndex: 1001
  },
  hiddenBox: {
    width: 64,
    [theme.breakpoints.down('sm')]: {
      display: 'none'
    }
  }
}));

const App: React.FC<Props> = (props: Props) => {
  const { env, forecastAPI } = props;
  const css = appStyles();

  useEffect(() => console.log('Running in ' + env.environment), [
    env.environment
  ]);

  const mapRef = createRef<Map>();
  const groupRef = createRef<FeatureGroup>();

  const [showBar, setShowBar] = useState(false);
  const [displayTime, setDisplayTime] = useState<Date>();

  const [forecast, setForecast] = useState<ForecastTimestamps>({
    reference: new Date(),
    validTimes: [new Date()]
  });

  const [overlays, setOverlays] = useState({
    weather: false,
    wind: false,
    temperature: false,
    layers: 0
  });

  const [position, setPosition] = useState<Coordinates>();
  const [viewport, setViewPort] = useState<Viewport>({
    center: [59.334591, 18.06324],
    zoom: 8
  });

  const hideGPSMarker = () => setPosition(undefined);

  const updatePosition: PositionCallback = ({ coords }) => {
    setPosition(coords);
  };

  const flyToPosition: PositionCallback = ({ coords }) => {
    if (!mapRef.current || !groupRef.current || !coords) return;
    const map = mapRef.current.leafletElement;
    const group = groupRef.current.leafletElement;
    if (!map || !group) return;
    setPosition(coords);
    const zoom = map.getBoundsZoom(group.getBounds());
    const { latitude, longitude } = coords;
    setViewPort({
      center: [latitude, longitude],
      zoom: zoom - 0.5
    });
  };

  useEffect(() => {
    void (async (): Promise<void> => {
      console.log('Fetching forecast...');
      const before = new Date();
      const forecast = await forecastAPI.ValidTimes();
      const duration = new Date().getTime() - before.getTime();
      console.log(
        `Fetched forecast, reference time: ${forecast.reference.toUTCString()}` +
          `, with ${forecast.validTimes.length} timestamps in ${duration}ms`
      );
      setForecast(forecast);
      setDisplayTime(forecast.validTimes[0]);
    })();
  }, [forecastAPI]);

  useEffect(() => {
    const timeout = setTimeout(() => setShowBar(true), 500);
    return () => clearTimeout(timeout);
  }, []);

  const toggleOverlay = (key: 'weather' | 'wind' | 'temperature') => {
    return (enable: boolean) => {
      const count = overlays.layers + (enable ? 1 : -1);
      const obj = { ...overlays, layers: count };
      obj[key] = enable;
      setOverlays(obj);
    };
  };

  return (
    <div className={css.fullscreen}>
      <TrailMap
        className={css.fullscreen}
        viewport={viewport}
        zoomControl={env.browser.hasTouch}
        ref={mapRef}
      >
        {/* TODO: Add WMTS layer support for faster loading
                    <WMTSLayer
                        url="https://kso.etjanster.lantmateriet.se/karta/topowebb/v1.1/wmts"
                        layers="topowebb"
                    />
                */}

        <WMSLayer // Terrain
          url='https://minkarta.lantmateriet.se/map/topowebb/'
          layers='topowebbkartan'
        />

        <WMSLayer // Hillshading layer
          url='https://minkarta.lantmateriet.se/map/hojdmodell/'
          layers='terrangskuggning'
          format='image/png'
          transparent={true}
          opacity={0.25}
        />
        {/*
                    <WMSLayer // Satellite
                      url='https://minkarta.lantmateriet.se/map/ortofoto/'
                      layers='Ortofoto_0.5,Ortofoto_0.4,Ortofoto_0.25,Ortofoto_0.16'
                    />
                */}
        {overlays.temperature && !!displayTime && (
          <TemperatureLayer
            referenceTime={forecast.reference}
            displayTime={displayTime}
          />
        )}

        {overlays.weather && !!displayTime && (
          <WeatherLayer
            referenceTime={forecast.reference}
            displayTime={displayTime}
          />
        )}

        <FeatureGroup ref={groupRef}>
          {position && <GPSMarker pos={position} />}
        </FeatureGroup>
      </TrailMap>

      <Timeline
        shown={overlays.layers > 0 && showBar}
        timepoints={forecast.validTimes}
        onChange={setDisplayTime}
      />

      <Slide direction='down' in={showBar}>
        <div className={css.topbar}>
          <div className={css.hiddenBox} />

          <Searchbar
            onGPSLocate={flyToPosition}
            onGPSTrack={updatePosition}
            onGPSDeactivate={hideGPSMarker}
          />

          <LayerSelector />
        </div>
      </Slide>

      <Overlaybar shown={!!displayTime && showBar}>
        <WeatherToggleButton onClick={toggleOverlay('weather')} />
        <WindToggleButton onClick={toggleOverlay('wind')} />
        <TemperatureToggleButton onClick={toggleOverlay('temperature')} />
      </Overlaybar>
    </div>
  );
};

export { App };
export type { Props as AppProps, Environment };
