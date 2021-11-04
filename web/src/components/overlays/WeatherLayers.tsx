import {
  WeatherPouring,
  WeatherWindy,
  Thermometer
} from '@mitch528/mdi-material-ui';
import { Grid, IconButton, IconButtonProps } from '@mui/material';
import React, {
  FC,
  useCallback,
  ReactElement,
  useContext,
  useState,
  cloneElement,
  useEffect
} from 'react';
import EnvContext from '../../contexts/EnvContext';
import { ForecastTimestamps } from '../../Forecast';
import { SectionTitle } from './LayerSelector';

export type ToggleLayerCallback = (layer: string) => void;

export type LayerToggleProps = {
  onClick?: ToggleLayerCallback;
  active?: boolean;
  layer: string;
};

export type ToggleButtonProps = LayerToggleProps &
  Omit<IconButtonProps, 'onClick'>;

const ToggleButton: FC<ToggleButtonProps> = (props: ToggleButtonProps) => {
  const { active, color, layer, onClick, ...rest } = props;

  const renderColor = active ? 'primary' : color ?? 'default';
  const callback = useCallback(() => {
    if (onClick) {
      onClick(layer);
    }
  }, [onClick, layer]);

  return (
    <IconButton color={renderColor} onClick={callback} size='large' {...rest}>
      {props.children}
    </IconButton>
  );
};

const IconToggleButton = (Icon: FC): FC<ToggleButtonProps> => {
  return Object.assign(
    (props: ToggleButtonProps) => (
      <ToggleButton style={{ padding: '10px' }} {...props}>
        <Icon />
      </ToggleButton>
    ),
    { displayName: 'IconToggleButton' }
  );
};

export const WeatherOverlay = IconToggleButton(WeatherPouring);
export const WindOverlay = IconToggleButton(WeatherWindy);
export const TemperatureOverlay = IconToggleButton(Thermometer);

WeatherOverlay.displayName = 'WeatherToggleButton';
WindOverlay.displayName = 'WindToggleButton';
TemperatureOverlay.displayName = 'TemperatureToggleButton';

export type WeatherLayersProps = {
  onForecastLoad: (success: boolean) => void;
  onClick: (timestamps?: [string, ForecastTimestamps]) => void;
  children: ReactElement<ToggleButtonProps>[];
  shown?: boolean;
};

export const WeatherLayers: FC<WeatherLayersProps> = (
  props: WeatherLayersProps
) => {
  const { shown, onClick, onForecastLoad, children } = props;
  const { forecastAPI } = useContext(EnvContext);
  const [activeLayer, setActiveLayer] = useState<string>();
  const [forecasts, setForecasts] = useState<
    Record<string, ForecastTimestamps>
  >({});

  const callback = (layer: string) => {
    if (activeLayer == layer) {
      onClick(undefined);
      setActiveLayer(undefined);
    } else {
      onClick([layer, forecasts[layer]]);
      setActiveLayer(layer);
    }
  };

  const toggleButtons = React.Children.map(children, (button) => {
    return cloneElement(button, {
      onClick: callback,
      active: button.props.layer == activeLayer,
      layer: button.props.layer
    });
  });

  useEffect(() => {
    const layers = children.map((btn) => btn.props.layer);
    const before = new Date();
    let mounted = true;

    forecastAPI
      .ForecastTimes(layers)
      .then((forecasts) => {
        if (mounted) {
          const duration = new Date().getTime() - before.getTime();
          console.log(
            `Fetched forecast for layers: ${layers.join(', ')} in ${duration}ms`
          );
          setForecasts(forecasts);
          onForecastLoad(true);
        }
      })
      .catch((err) => {
        if (mounted) {
          console.log('Failed fetching forecast: ', err);
          onForecastLoad(false);
        }
      });

    return () => {
      mounted = false;
    };
  }, [forecastAPI]);

  if (!shown) return null;

  return (
    <>
      <SectionTitle>Väder</SectionTitle>
      <Grid container direction='row' justifyContent='space-around'>
        {toggleButtons}
      </Grid>
    </>
  );
};

export default WeatherLayers;
