import React from 'react'
import {
    Map,
    MapProps,
    AttributionControl,
    LayersControl,
    Children,
    ScaleControl,
} from 'react-leaflet'
import BufferedWMSLayer from './BufferedWMSTileLayer'

// Children optional, not required
type Props = Omit<MapProps, 'children'> & {
    hasTouch: boolean
    children?: Children
}

const TrailMap: React.FC<Props> = (props: Props) => {
    return (
        <Map
            {...props}
            useFlyTo={true}
            attributionControl={false}
            zoomControl={!props.hasTouch}
            zoomSnap={0}
        >
            <LayersControl position="topright">
                <LayersControl.BaseLayer
                    name="Lantmäteriet Webbkarta"
                    checked={true}
                >
                    <BufferedWMSLayer
                        url="https://minkarta.lantmateriet.se/map/topowebb/"
                        layers="topowebbkartan"
                    />
                </LayersControl.BaseLayer>
            </LayersControl>
            <ScaleControl position="bottomleft" />
            <AttributionControl position="bottomright" prefix={false} />
        </Map>
    )
}

export default TrailMap
