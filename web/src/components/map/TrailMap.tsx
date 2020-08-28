import React from 'react'
import {
    Map,
    MapProps,
    AttributionControl,
    LayersControl,
    Children,
} from 'react-leaflet'
import BufferedTileLayer from './BufferedTileLayer'
import BufferedWMSLayer from './BufferedWMSTileLayer'

// Children optional, not required
type Props = Omit<MapProps, 'children'> & { token: string; children?: Children }

const TrailMap: React.FC<Props> = (props: Props) => {
    return (
        <Map {...props} useFlyTo={true} attributionControl={false}>
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
                <LayersControl.BaseLayer name="Lantmäteriet WMTS">
                    <BufferedTileLayer
                        url={`https://api.lantmateriet.se/open/topowebb-ccby/v1/wmts/token/${props.token}/1.0.0/topowebb/default/3857/{z}/{y}/{x}.png`}
                        bufferRadius={2}
                        maxZoom={15}
                    />
                </LayersControl.BaseLayer>
            </LayersControl>
            <AttributionControl position="bottomright" prefix={false} />
        </Map>
    )
}

export default TrailMap
