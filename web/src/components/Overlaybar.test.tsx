import React from "react";
import { ForecastTimestamps } from "../Forecast";
import {
  Overlaybar,
  WeatherToggleButton,
  WindToggleButton,
  TemperatureToggleButton
} from './Overlaybar';
import { errHandler, server, WTSURL } from '../Mocks';
import {fireEvent, waitFor, screen} from '@testing-library/react'
import { render } from '../setup.test';
import { theme } from '../Theme';
import '@testing-library/jest-dom'


describe('Overlaybar', () => {
  type OverlayArgs = {
    onClick:(timestamps?: [string, ForecastTimestamps]) => void,
    onForecastLoad:(success: boolean) => void,
    shown:boolean
  };

  const renderOverlay = (obj: Partial<OverlayArgs> = {}) => {
    const args: OverlayArgs = {
      onClick: (_) => {},
      onForecastLoad: (_) => {},
      shown: true
    };

    Object.assign(args, obj);
    render(
      <Overlaybar
        onClick={args.onClick}
        onForecastLoad={args.onForecastLoad}
        shown={args.shown}
      >
        <WeatherToggleButton layer='weather-layer' title='Weather' />
        <WindToggleButton layer='wind-layer' title='Wind'/>
        <TemperatureToggleButton layer='temperature-layer' title='Temperature'/>
      </Overlaybar>
    );
  };

  it('buttons should be visible', () => {
    renderOverlay();
    expect(screen.getByTitle('Weather')).toBeVisible();
    expect(screen.getByTitle('Wind')).toBeVisible();
    expect(screen.getByTitle('Temperature')).toBeVisible();
  });

  it('should call callback on forecast load', async () => {
    const callback = jest.fn();

    const args: Partial<OverlayArgs> = {
      onForecastLoad: callback
    };


    renderOverlay(args);
    await waitFor (() => expect(callback).toHaveBeenCalledWith(true));

    callback.mockClear();
    server.use(errHandler(WTSURL, 500));
    renderOverlay(args);

    await waitFor (() => expect(callback).toHaveBeenCalledWith(false));
  });

  it('should treat buttons as toggles', async () => {
    const callback = jest.fn();

    const color = (title: string) => {
      return window.getComputedStyle(screen.getByTitle(title)).color;
    }


    renderOverlay({ onClick: callback });
    const defaultColor = color('Weather');

    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Weather'));
    expect(callback).toHaveBeenLastCalledWith(['weather-layer', undefined]);
    const selectedColor = color('Weather');
    expect(color('Weather')).toBe(selectedColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Weather'));
    expect(callback).toHaveBeenLastCalledWith(undefined);
    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Wind'));
    expect(callback).toHaveBeenLastCalledWith(['wind-layer', undefined]);
    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(selectedColor);
    expect(color('Temperature')).toBe(defaultColor);

    fireEvent.click(screen.getByTitle('Temperature'));
    expect(callback).toHaveBeenLastCalledWith(['temperature-layer', undefined]);
    expect(color('Weather')).toBe(defaultColor);
    expect(color('Wind')).toBe(defaultColor);
    expect(color('Temperature')).toBe(selectedColor);
  });
});
