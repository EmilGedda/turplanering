import React, { FC, useContext, useEffect } from 'react';
import MapContext from '../../contexts/MapContext';
import BaseEvent from 'ol/events/Event';
import * as ol from 'ol';

export const Events: FC = ({ children }): JSX.Element => {
  return <>{children}</>;
};

type EventNames = Parameters<ol.PluggableMap['on']>[0][0];

type Props<T> = {
  callback: (event: T) => void;
};

const event = <T extends BaseEvent>(name: EventNames) => {
  return ({ callback }: Props<T>): null => {
    const { map } = useContext(MapContext);

    useEffect(() => {
      // @ts-expect-error: argument forwarding to multiple overloads is hard
      map?.on(name, callback);
      // @ts-expect-error: argument forwarding to multiple overloads is hard
      return () => map?.un(name, callback);
    }, [callback]);

    return null;
  };
};

export const MoveEnd = event<ol.MapEvent>('moveend');
export const Change = event<BaseEvent>('change');
