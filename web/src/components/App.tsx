import LayerSelector from './LayerSelector';
import React, { useState, useEffect } from 'react';
import Searchbar from './Searchbar';
import { Slide } from '@mui/material';
import makeStyles from '@mui/styles/makeStyles';
import {
  Overlaybar,
  WeatherToggleButton,
  WindToggleButton,
  TemperatureToggleButton
} from './Overlaybar';
import Map from './map/Map';
import TileLayer, { wmtsCapabilities } from './map/TileLayer';
import Timeline from './Timeline';
import WMTS, { optionsFromCapabilities } from 'ol/source/WMTS';
import WMTSCapabilities from 'ol/format/WMTSCapabilities';
import { ForecastAPI, ForecastTimestamps } from '../forecast';

export type Environment = {
  apiURL: string;
  environment: string;
  browser: {
    hasTouch: boolean;
  };
};

export type AppProps = {
  env: Environment;
  forecastAPI: ForecastAPI;
};

const appStyles = makeStyles((theme) => ({
  fullscreen_no_padding: {
    position: 'absolute',
    top: 0,
    right: 0,
    height: '100%',
    width: '100vw'
  },
  fullscreen: {
    position: 'absolute',
    top: 0,
    right: 0,
    height: '100%',
    width: '100vw',
    [theme.breakpoints.down('md')]: {
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
    pointerEvents: 'none',
    zIndex: 1001
  },
  hiddenBox: {
    width: 64,
    [theme.breakpoints.down('md')]: {
      display: 'none'
    }
  }
}));

export const App: React.FC<AppProps> = (props: AppProps) => {
  const { env, forecastAPI } = props;
  const css = appStyles();

  useEffect(
    () => console.log('Running in ' + env.environment),
    [env.environment]
  );

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

  // center: [59.334591, 18.06324],
  // zoom: 8

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

  const opts = optionsFromCapabilities(
    new WMTSCapabilities().read(wmtsCapabilities),
    {
      layer: 'topowebb',
      matrixSet: '3857'
    }
  )!;

  opts.url = 'https://minkarta.lantmateriet.se/map/topowebbcache/';
  opts.urls = ['https://minkarta.lantmateriet.se/map/topowebbcache/?'];

  useEffect(() => {
    const timeout = setTimeout(() => setShowBar(true), 500);
    return () => clearTimeout(timeout);
  }, []);

  console.log(opts);

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
      <Map zoom={3} center={[18, 58]} className={css.fullscreen_no_padding}>
        <TileLayer zIndex={0} source={new WMTS(opts)} />
      </Map>
      {/*
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
                }

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
                /}
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

        */}
      <Timeline
        shown={overlays.layers > 0 && showBar}
        timepoints={forecast.validTimes}
        onChange={setDisplayTime}
      />

      <Slide direction='down' in={showBar}>
        <div className={css.topbar}>
          <div className={css.hiddenBox} />

          <Searchbar
            onGPSLocate={console.log}
            onGPSTrack={console.log}
            onGPSDeactivate={console.log}
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
