import React from "react";
import { fireEvent, screen, makeRender, act } from '../setup.test';
import Timeline, { TimelineProps } from "./Timeline";
import '@testing-library/jest-dom';

describe('Timeline', () => {
  jest.useFakeTimers();

  afterAll(() => {
    jest.useRealTimers();
  });

  const date1 = new Date(2000, 0, 1, 1, 2);
  const date2 = new Date(2000, 0, 1, 3, 4);

  const render = (obj?: Partial<TimelineProps>) => {
    const props: TimelineProps = {
      timepoints: [date1, date2],
      onChange: (_) => {}
    };

    Object.assign(props, obj);

    act(() => {
      makeRender()(<Timeline  {...props} />);
    });
  };

  const setInputValue = (val: number) => {
    act(() => {
      fireEvent.change(document.querySelector('input') as Element, { target: { value: val } });
    });
  }

  const play = () => {
    act(() => {
      fireEvent.click(screen.getByTestId('PlayCircleOutlineIcon'));
    });
  }

  const pause = () => {
    act(() => {
      fireEvent.click(screen.getByTestId('PauseCircleOutlineIcon'));
    });
  }

  const runTimers = () => {
    act(() => {
      jest.runOnlyPendingTimers();
    });
  }

  const isPaused = () => screen.queryByTestId('PlayCircleOutlineIcon')

  const isPlaying = () => screen.queryByTestId('PauseCircleOutlineIcon')

  it('should play when button clicked', () => {
    const date3 = new Date(2000, 0, 1, 4, 5);
    const onChange = jest.fn();
    render({ timepoints: [date1, date2, date3],  onChange });

    setInputValue(0);
    setInputValue(2);
    setInputValue(1);
    play();

    runTimers();
    setInputValue(0);
    expect(isPaused()).toBeTruthy();
    expect(isPlaying()).toBeFalsy();

    setInputValue(0);
    play();
    runTimers();

    expect(isPaused()).toBeFalsy();
    expect(isPlaying()).toBeTruthy();

    runTimers();
    runTimers();

    expect(isPaused()).toBeTruthy();
    expect(isPlaying()).toBeFalsy();

    setInputValue(2);
    play();
    runTimers();
    runTimers();

    expect(isPaused()).toBeTruthy();
    expect(isPlaying()).toBeFalsy();
  });

  it('should pause when button or timeline clicked when running', () => {
    render();

    expect(isPaused()).toBeTruthy();
    expect(isPlaying()).toBeFalsy();

    play();

    expect(isPaused()).toBeFalsy();
    expect(isPlaying()).toBeTruthy();

    pause();
    expect(isPaused()).toBeTruthy();
    expect(isPlaying()).toBeFalsy();

    play();
    setInputValue(0);
    setInputValue(1);

    expect(isPaused()).toBeTruthy();
    expect(isPlaying()).toBeFalsy();
  });

  it('should call onChange when timepoint changes', async () => {
    const onChange = jest.fn();
    render({ onChange });
    expect(onChange).not.toHaveBeenCalled();

    setInputValue(1);
    runTimers();
    expect(onChange).toHaveBeenLastCalledWith(1);

    setInputValue(0);
    runTimers();
    expect(onChange).toHaveBeenLastCalledWith(0);
  });

  it('should render timepoint string', () => {
    const date1 = new Date(2000, 0, 1, 1, 2);
    const date2 = new Date(2000, 0, 1, 3, 4);
    const time1 = date1.toLocaleTimeString('sv-SE', {
      hour: 'numeric',
      minute: 'numeric'
    });
    const time2 = date2.toLocaleTimeString('sv-SE', {
      hour: 'numeric',
      minute: 'numeric'
    });

    render();

    expect(screen.queryByText(time1)).toBeTruthy();
    expect(screen.queryByText(time2)).toBeFalsy

    setInputValue(1);

    expect(screen.queryByText(time1)).toBeFalsy();
    expect(screen.queryByText(time2)).toBeTruthy();
  });

});
