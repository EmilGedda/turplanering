import React from 'react';

export type LayerProps = { children: React.ReactNode };

const circular = () => {
  const seen = new WeakSet();
  return (key: string, value: string) => {
    if (key.startsWith('_')) return; // Don't compare React's internal props.
    if (typeof value === 'object' && value !== null) {
      if (seen.has(value)) return;
      seen.add(value);
    }
    return value;
  };
};

type Obj = Record<string, unknown>;
const jsonEq = (prevProps: Obj, nextProps: Obj) => {
  const prev = JSON.stringify(prevProps, circular());
  const next = JSON.stringify(nextProps, circular());
  return prev === next;
};

export const Overlays = ({ children }: LayerProps): JSX.Element => {
  return <>{children}</>;
};

export const Layers = React.memo(Overlays, jsonEq);
Layers.displayName = 'Layers';
