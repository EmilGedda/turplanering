import LayerSelector from './LayerSelector';
import { styled } from '@mui/material/styles';
import React, { useState, useEffect } from 'react';
import Searchbar from './Searchbar';
import { Slide } from '@mui/material';
import {
  Overlaybar,
  WeatherToggleButton,
  WindToggleButton,
  TemperatureToggleButton
} from './Overlaybar';
import Map from './map/Map';
import TileLayer, { topowebbSource } from './map/TileLayer';
import Timeline from './Timeline';
import Feature from 'ol/Feature';
import { MVT } from 'ol/format';
import { TrailLayer } from './map/VectorLayer';
import { VectorTile as VectorSource } from 'ol/source';
import { fromLonLat } from 'ol/proj';
import { CoordURL, currentURLState } from '../URL';
import { Environment } from '../contexts/EnvContext';
import { ForecastAPI, ForecastTimestamps } from '../forecast';
import { TemperatureLayer, WeatherLayer } from './map/WeatherOverlays';
import { Layers, Overlays } from './map/Layers';

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

export type AppProps = {
  env: Environment;
  forecastAPI: ForecastAPI;
};

const initialView = () => {
  const { state } = currentURLState();
  return state instanceof CoordURL
    ? { center: fromLonLat([state.lon, state.lat]), zoom: state.zoom ?? 8 }
    : { center: fromLonLat([18.07, 59.324]), zoom: 8 };
};

export const App: React.FC<AppProps> = (props: AppProps) => {
  const { env, forecastAPI } = props;

  const [showBar, setShowBar] = useState(false);
  const [displayTime, setDisplayTime] = useState<Date>();
  const [trailSource, setTrailSource] = useState<VectorSource>();

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

  useEffect(() => {
    const source =
      env.environment != 'development'
        ? undefined
        : new VectorSource({
            url: env.tileURL,
            format: new MVT({
              featureClass: Feature,
              layerName: 'trail_sections',
              geometryName: 'geom',
              idProperty: 'gid'
            })
          });
    setTrailSource(source);
  }, [env.tileURL]);

  useEffect(
    () => console.log('Running in ' + env.environment),
    [env.environment]
  );

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
    <Div className={`${classes.padded} ${classes.fullscreen}`}>
      <Map view={initialView()} className={classes.fullscreen}>
        <Layers>
          <TileLayer source={topowebbSource} />
          <TrailLayer source={trailSource} />
        </Layers>

        <Overlays>
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
        </Overlays>
      </Map>

      {/*
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
        <FeatureGroup ref={groupRef}>
          {position && <GPSMarker pos={position} />}
        </FeatureGroup>
      </TrailMap>

        */}

      <Slide direction='down' in={showBar}>
        <Div className={classes.topbar}>
          <Div className={classes.hiddenBox} />

          <Searchbar
            onGPSLocate={console.log}
            onGPSTrack={console.log}
            onGPSDeactivate={console.log}
          />

          <LayerSelector />
        </Div>
      </Slide>

      <Overlaybar shown={!!displayTime && showBar}>
        <WeatherToggleButton onClick={toggleOverlay('weather')} />
        <WindToggleButton onClick={toggleOverlay('wind')} />
        <TemperatureToggleButton onClick={toggleOverlay('temperature')} />
      </Overlaybar>

      <Timeline
        shown={overlays.layers > 0 && showBar}
        timepoints={forecast.validTimes}
        onChange={setDisplayTime}
      />
    </Div>
  );
};
