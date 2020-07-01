import React from 'react';
import { LatLngTuple } from 'leaflet';
import { Marker } from 'react-leaflet';

const LayerContext:any = React.createContext({});

const LayerContextProvider = ({ children }: any) => {
    const [point, setPoint] = React.useState<LatLngTuple>([0, 0]);

    const defaultValue = {
        point,
        setPoint
    }

    return (
        <LayerContext.Provider value={defaultValue}>
            {children}
        </LayerContext.Provider>
    )
}

export { LayerContext, LayerContextProvider };
