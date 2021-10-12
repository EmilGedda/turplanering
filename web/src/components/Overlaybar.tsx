import React, {
  FC,
  useState,
  ReactElement,
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

export type ToggleLayerCallback = (layer?: string) => void;

export type ToggleButtonProps = LayerToggleProps &
  Omit<IconButtonProps, 'onClick'>;

const ToggleButton: FC<ToggleButtonProps> = (props: ToggleButtonProps) => {
  const [active, setIsActive] = useState(false);

  const color = (() => {
    if (props.disabled) return 'secondary';
    if (active) return 'primary';
    return props.color ? props.color : 'default';
  })();

  const onClick = () => {
    if (props.disabled) return;
    if (props.onClick) props.onClick(!active ? props.layer : undefined);
    setIsActive(!active);
  };

  return (
    <IconButton color={color} onClick={onClick} size='large'>
      {props.children}
    </IconButton>
  );
};

export type LayerToggleProps = {
  onClick?: ToggleLayerCallback;
  layer: string;
};

export type OverlaybarProps = {
  shown: boolean;
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
  const { shown, onClick, children: buttons } = props;
  const [forecasts, setForecasts] = useState<
    Record<string, ForecastTimestamps>
  >({});
  const { forecastAPI } = useContext(EnvContext);

  const callback = (layer?: string) => {
    onClick(layer ? [layer, forecasts[layer]] : undefined);
  };

  const toggleButtons = React.Children.map(buttons, (button) => {
    if (React.isValidElement(button)) {
      return React.cloneElement(button, { onClick: callback });
    }
    return button;
  });

  useEffect(() => {
    void (async (): Promise<void> => {
      const layers = buttons.map((btn) => btn.props.layer);
      console.log('Fetching forecast...');
      const before = new Date();
      const forecasts = await forecastAPI.ValidTimes(layers);
      const duration = new Date().getTime() - before.getTime();
      console.log(
        `Fetched forecast for layers: ${layers.join(', ')} in ${duration}ms`
      );
      setForecasts(forecasts);
    })();
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
