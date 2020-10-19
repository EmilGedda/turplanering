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
import { WMSLayer, BufferedWMSLayerProps } from './BufferedWMSTileLayer'

type GPSMarkerProps = { pos: Coordinates }

export const GPSMarker: React.FC<GPSMarkerProps> = ({
    pos,
}: GPSMarkerProps) => {
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

type OverlayProps = Omit<BufferedWMSLayerProps, 'url'> & {
    referenceTime: Date
    displayTime: Date
}

type SmhiLayerProps = OverlayProps & { layer: string }

const timeStr = (d: Date): string => d.toISOString().slice(0, -5) + 'Z'

const SmhiLayer: FC<SmhiLayerProps> = (props: SmhiLayerProps) => {
    const { displayTime, referenceTime, layer, ...baseProps } = props
    const now = timeStr(referenceTime)

    return (
        <WMSLayer
            {...baseProps}
            url="https://wts{s}.smhi.se/tile/"
            subdomains="1234"
            transparent={true}
            tileSize={512}
            crs={CRS.EPSG900913}
            format="image/png"
            dim_reftime={now}
            time={timeStr(displayTime)}
            layers={layer + '::' + now}
        />
    )
}

export const WeatherLayer: FC<OverlayProps> = (props: OverlayProps) => {
    return (
        <SmhiLayer {...props} layer={'pmpfrekvent:wpt-overview_n-europe__'} />
    )
}

export const TemperatureLayer: FC<OverlayProps> = (props: OverlayProps) => {
    return (
        <SmhiLayer
            {...props}
            opacity={0.5}
            layer={'pmpfrekvent:temperature-2m_n-europe_rainbow_'}
        />
    )
}

export const TrailMap = React.forwardRef<Map, Props>((props: Props, ref) => {
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
