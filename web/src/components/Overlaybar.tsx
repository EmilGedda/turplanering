import React, {
  FC,
  useState,
  ReactElement,
  cloneElement,
  useEffect,
  useContext
} from 'react';
import { styled } from '@mui/material/styles';
import {
  WeatherPouring,
  Thermometer,
  WeatherWindy
} from '@mitch528/mdi-material-ui';
import { Slide, Paper, Grid, IconButton, IconButtonProps } from '@mui/material';
import { ForecastTimestamps } from '../Forecast';
import EnvContext from '../contexts/EnvContext';

const StyledDiv = styled('div')(({ theme }) => ({
  pointerEvents: 'none',
  position: 'fixed',
  top: 0,
  bottom: 0,
  right: 0,
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  [theme.breakpoints.down('md')]: {
    marginRight: 10
  },
  [theme.breakpoints.up('sm')]: {
    marginRight: 25
  },
  zIndex: 1000
}));

export type ToggleLayerCallback = (layer: string) => void;

export type ToggleButtonProps = LayerToggleProps &
  Omit<IconButtonProps, 'onClick'>;

const ToggleButton: FC<ToggleButtonProps> = (props: ToggleButtonProps) => {
  const { active, color, layer, onClick, ...rest } = props;

  const renderColor = active ? 'primary' : color ?? 'default';
  const callback = () => {
    if (onClick) {
      onClick(layer);
    }
  };

  return (
    <IconButton color={renderColor} onClick={callback} size='large' {...rest}>
      {props.children}
    </IconButton>
  );
};

export type LayerToggleProps = {
  onClick?: ToggleLayerCallback;
  active?: boolean;
  layer: string;
};

export type OverlaybarProps = {
  shown: boolean;
  onForecastLoad: (success: boolean) => void;
  onClick: (timestamps?: [string, ForecastTimestamps]) => void;
  children: ReactElement<ToggleButtonProps>[];
};

const IconToggleButton = (Icon: FC): FC<ToggleButtonProps> => {
  return Object.assign(
    (props: ToggleButtonProps) => (
      <ToggleButton {...props}>
        <Icon />
      </ToggleButton>
    ),
    { displayName: 'IconToggleButton' }
  );
};

export const WeatherToggleButton = IconToggleButton(WeatherPouring);
export const WindToggleButton = IconToggleButton(WeatherWindy);
export const TemperatureToggleButton = IconToggleButton(Thermometer);

WeatherToggleButton.displayName = 'WeatherToggleButton';
WindToggleButton.displayName = 'WindToggleButton';
TemperatureToggleButton.displayName = 'TemperatureToggleButton';

export const Overlaybar: FC<OverlaybarProps> = (props: OverlaybarProps) => {
  const { shown, onClick, onForecastLoad, children: buttons } = props;
  const { forecastAPI } = useContext(EnvContext);
  const [activeLayer, setActiveLayer] = useState<string>();
  const [forecasts, setForecasts] = useState<
    Record<string, ForecastTimestamps>
  >({});

  const callback = (layer: string) => {
    if (activeLayer == layer) {
      setActiveLayer(undefined);
      onClick(undefined);
    } else {
      setActiveLayer(layer);
      onClick([layer, forecasts[layer]]);
    }
  };

  const toggleButtons = React.Children.map(buttons, (button) => {
    return cloneElement(button, {
      ...button.props,
      onClick: callback,
      active: button.props.layer == activeLayer,
      layer: button.props.layer
    });
  });

  useEffect(() => {
    const layers = buttons.map((btn) => btn.props.layer);
    const before = new Date();

    forecastAPI
      .ForecastTimes(layers)
      .then((forecasts) => {
        const duration = new Date().getTime() - before.getTime();
        console.log(
          `Fetched forecast for layers: ${layers.join(', ')} in ${duration}ms`
        );
        setForecasts(forecasts);
        onForecastLoad(true);
      })
      .catch((err) => {
        console.log('Failed fetching forecast: ', err);
        onForecastLoad(false);
      });
  }, [forecastAPI]);

  return (
    <Slide direction='left' in={shown}>
      <StyledDiv>
        <Paper elevation={5} square={false} style={{ pointerEvents: 'auto' }}>
          <Grid container direction='column' justifyContent='space-around'>
            {toggleButtons}
          </Grid>
        </Paper>
      </StyledDiv>
    </Slide>
  );
};
