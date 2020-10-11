import { LatLngTuple, CRS } from 'leaflet'
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

type Props = Omit<MapProps, 'children'> & {
    hasTouch: boolean
    children?: Children
    showPrecipitation?: boolean
    showTemperature?: boolean
    showWind?: boolean
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
                    />
                </LayersControl.BaseLayer>
                {props.showPrecipitation && (
                    <BufferedWMSLayer
                        url="https://wts{s}.smhi.se/tile/"
                        subdomains={['1', '2', '3', '4']}
                        transparent={true}
                        tileSize={512}
                        crs={CRS.EPSG900913}
                        // time="2020-10-11T17:00:00Z"
                        format="image/png"
                        layers="pmpfrekvent:wpt-overview_n-europe__::2020-10-11T21:00:00Z"
                    />
                )}
            </LayersControl>
            {children}
            <ScaleControl position="bottomleft" />
            <AttributionControl position="bottomright" prefix={false} />
        </Map>
    )
})

TrailMap.displayName = 'TrailMap'

export { TrailMap, GPSMarker }
