import React from "react";
import { LayerSelector, SectionTitle } from '.';
import { render, fireEvent, screen } from '../../setup.test';
import '@testing-library/jest-dom';


describe('SectionTitle', () => {
  it('should show text', () => {
    render(<SectionTitle>some text</SectionTitle>);
    expect(screen.getByText("some text")).toBeVisible();
  });
});

describe('LayerSelector', () => {

  beforeEach(() => {
    jest.useFakeTimers();
    render(<LayerSelector>layer selector</LayerSelector>);
    jest.runOnlyPendingTimers();
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  const open = () => fireEvent.click(screen.getByTestId('LayersIcon'));
  const close = () => fireEvent.mouseDown(document.body);

  it('should only show icon by default', () => {
    expect(screen.getByText('layer selector')).not.toBeVisible();
  });

  it('should open when clicked', () => {
    open();
    expect(screen.getByText('layer selector')).toBeVisible();
  });

  it('should close when clicked away', async () => {
    open();
    close();
    expect(screen.getByText('layer selector')).not.toBeVisible();
  });

  it('should open when icon hovered', async () => {
    fireEvent.mouseEnter(screen.getByTestId('LayersIcon'));
    expect(screen.getByText('layer selector')).toBeVisible();
  });

  it('should stay open when hovered', async () => {
    fireEvent.mouseEnter(screen.getByTestId('SelectorBox'));
    expect(screen.getByText('layer selector')).toBeVisible();
  });

  it('should close when mouse leave', async () => {
    fireEvent.mouseEnter(screen.getByTestId('LayersIcon'));
    fireEvent.mouseLeave(screen.getByTestId('SelectorBox'));
    expect(screen.getByText('layer selector')).not.toBeVisible();
  });

  it('should stay open when pinned', async () => {
    open();
    fireEvent.click(screen.getByTestId('PinIconButton'));
    close();
    expect(screen.getByText('layer selector')).toBeVisible();
  });

  it('should stay close when unpinned', async () => {
    open();
    fireEvent.click(screen.getByTestId('PinIconButton'));
    fireEvent.click(screen.getByTestId('PinIconButton'));
    close();
    expect(screen.getByText('layer selector')).not.toBeVisible();
  });
});
