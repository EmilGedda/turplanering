import React from "react";
import { ForecastTimestamps } from "../../Forecast";
import {
  WeatherLayers,
  WeatherOverlay,
  WindOverlay,
  TemperatureOverlay
} from './WeatherLayers';
import { errHandler, server, WTSURL } from '../../Mocks';
import { Environment } from '../../contexts/EnvContext';
import { fireEvent, waitFor, screen, makeRender, act, RenderArgs, testEnv } from '../../setup.test';
import '@testing-library/jest-dom';


describe('WeatherLayers', () => {
  type OverlayArgs = {
    onClick:(timestamps?: [string, ForecastTimestamps]) => void,
    onForecastLoad:(success: boolean) => void,
    shown:boolean
  };

  const render = (obj?: Partial<OverlayArgs>, renderArgs?: Partial<RenderArgs>) => {
    const props: OverlayArgs = {
      onClick: (_) => {},
      onForecastLoad: (_) => {},
      shown: true
    };

    Object.assign(props, obj);

    act(() => {
      makeRender(renderArgs)(
        <WeatherLayers {...props}>
          <WeatherOverlay layer='weather-layer' title='Weather' />
          <WindOverlay layer='wind-layer' title='Wind'/>
          <TemperatureOverlay layer='temperature-layer' title='Temperature'/>
        </WeatherLayers>
      );
    });
  };

  it('buttons should be visible', () => {
    render();
    expect(screen.getByTitle('Weather')).toBeVisible();
    expect(screen.getByTitle('Wind')).toBeVisible();
    expect(screen.getByTitle('Temperature')).toBeVisible();
  });

  it('should not be visible with shown=false', () => {
    render({shown: false});
    expect(screen.queryByTitle('Weather')).toBeNull();
    expect(screen.queryByTitle('Wind')).toBeNull();
    expect(screen.queryByTitle('Temperature')).toBeNull();
  });

  it('should call callback on forecast load', async () => {
    const callback = jest.fn();

    render({ onForecastLoad: callback });
    await waitFor (() => expect(callback).toHaveBeenCalledWith(true));

    callback.mockClear();
    server.use(errHandler(WTSURL, 500));
    render({ onForecastLoad: callback });

    await waitFor (() => expect(callback).toHaveBeenCalledWith(false));
  });

  it('should treat buttons as toggles', async () => {
    const forecastResult : Record<string, ForecastTimestamps> = {
      'wind-layer': {
        validTimes: [],
        reference: new Date()
      },
      'weather-layer': {
        validTimes: [new Date()],
        reference: new Date()
      },
      'temperature-layer': {
        validTimes: [new Date(), new Date()],
        reference: new Date()
      }
    }

    const env: Environment = {
      ...testEnv,
      forecastAPI: {
        ForecastTimes: (_) => Promise.resolve(forecastResult)
      }
    };

    const callback = jest.fn();
    const hasForecasts = jest.fn();

    const color = (title: string) => {
      return window.getComputedStyle(screen.getByTitle(title)).color;
    }

    render({ onClick: callback, onForecastLoad: hasForecasts }, { env });
    await waitFor (() => expect(hasForecasts).toHaveBeenCalledWith(true));

    const defaultColor = color('Weather');

    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Weather'));
    const selectedColor = color('Weather');
    expect(selectedColor).not.toBe(defaultColor);

    expect(callback).toHaveBeenLastCalledWith(['weather-layer', forecastResult['weather-layer']]);
    expect(color('Weather')).toBe(selectedColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Weather'));
    expect(callback).toHaveBeenLastCalledWith(undefined);
    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Wind'));
    expect(callback).toHaveBeenLastCalledWith(['wind-layer', forecastResult['wind-layer']]);
    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(selectedColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Temperature'));
    expect(callback).toHaveBeenLastCalledWith(['temperature-layer', forecastResult['temperature-layer']]);
    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(selectedColor);
  });
});
