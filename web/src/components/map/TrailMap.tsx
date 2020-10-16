import React, { FC } from 'react'
import {
    Map,
    MapProps,
    AttributionControl,
    Children,
    ScaleControl,
    Circle,
    CircleMarker,
} from 'react-leaflet'
import { LatLngTuple, CRS } from 'leaflet'
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
    showTemperature?: boolean
    showWind?: boolean
}

type WeatherLayerProps = {
    referenceTime: Date
    displayTime: Date
}

const WeatherLayer: FC<WeatherLayerProps> = ({
    displayTime,
    referenceTime,
}: WeatherLayerProps) => {
    const timeStr = (d: Date): string => d.toISOString().slice(0, -5) + 'Z'
    const now = timeStr(referenceTime)

    return (
        <BufferedWMSLayer
            url="https://wts{s}.smhi.se/tile/"
            subdomains="1234"
            transparent={true}
            tileSize={512}
            crs={CRS.EPSG900913}
            format="image/png"
            dim_reftime={now}
            time={timeStr(displayTime)}
            layers={'pmpfrekvent:wpt-overview_n-europe__::' + now}
        />
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
            {children}
            <ScaleControl position="bottomleft" />
            <AttributionControl position="bottomright" prefix={false} />
        </Map>
    )
})

TrailMap.displayName = 'TrailMap'

export { TrailMap, GPSMarker, WeatherLayer }
