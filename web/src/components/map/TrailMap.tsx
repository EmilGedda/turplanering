import { LatLngTuple } from 'leaflet'
import React from 'react'
import {
    Map,
    MapProps,
    AttributionControl,
    LayersControl,
    Children,
    ScaleControl,
    Circle,
    CircleMarker,
} from 'react-leaflet'
import BufferedWMSLayer from './BufferedWMSTileLayer'

// Children optional, not required
type Props = Omit<MapProps, 'children'> & {
    hasTouch: boolean
    children?: Children
}

type GPSMarkerProps = { pos: Coordinates }
const GPSMarker: React.FC<GPSMarkerProps> = ({ pos }: GPSMarkerProps) => {
    const center: LatLngTuple = [pos.latitude, pos.longitude]
    return (
        <>
            <Circle radius={pos.accuracy} center={center} />
            <CircleMarker
                radius={5}
                center={center}
                fillOpacity={1.0}
                color="#0c31eb"
            />
        </>
    )
}

const TrailMap = React.forwardRef<Map, Props>((props: Props, ref) => {
    const { children, ...baseProps } = props
    return (
        <Map
            {...baseProps}
            useFlyTo={true}
            attributionControl={false}
            zoomControl={!props.hasTouch}
            zoomSnap={0}
            ref={ref}
        >
            <LayersControl position="topright">
                <LayersControl.BaseLayer
                    name="Lantmäteriet Webbkarta"
                    checked={true}
                >
                    <BufferedWMSLayer
                        url="https://minkarta.lantmateriet.se/map/topowebb/"
                        layers="topowebbkartan"
                        onload={props.onload}
                    />
                </LayersControl.BaseLayer>
            </LayersControl>
            {children}
            <ScaleControl position="bottomleft" />
            <AttributionControl position="bottomright" prefix={false} />
        </Map>
    )
})

TrailMap.displayName = 'TrailMap'

export { TrailMap, GPSMarker }
