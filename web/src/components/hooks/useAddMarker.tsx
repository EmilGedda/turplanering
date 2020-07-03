import React from 'react';
import { useContext, useEffect, useState, useCallback } from 'react';
import { LeafletMouseEvent } from 'leaflet';
import { useLeaflet, Marker } from 'react-leaflet';
import { LayerContext } from '../context/LayerContext';

// the hook Effect will be activated by the click on the button
function useAddMarker(selected: boolean) {

// The hook 'useLeaflet' is provided by the react-leaflet library.
// This hook allow to access to the Leaflet Context and its variables.
// It is a simple way to access the map and its content.

    const { map } = useLeaflet();

// the hook useContext is used to access to the previously defined LayerContext.
    const { setPoint } = useContext(LayerContext);

// add a state to activate the Event
    const [activate, setActivate] = useState(selected);

// define the MouseEvent with the useCallback hook
    const markerEvent = useCallback(
        (e: LeafletMouseEvent) => {
            // if you want to use any event,
            // be sure that the default is disabled.
            e.originalEvent.preventDefault();
            // create your Marker with the react leaflet component Marker
            setPoint(<Marker position={e.latlng} />);
            e.originalEvent.stopPropagation();
        }, [setPoint]);


    // activate the EventHandler with the useEffect handler
    useEffect(
        () => {
            map?.doubleClickZoom.disable();
            if (activate === true) {
                map?.on('dblclick', markerEvent);
            }
            return () => {
                map?.off('dblclick', markerEvent);
            }
        }, [map, activate, markerEvent]
    );

    return {activate, setActivate};
}

export default useAddMarker;


