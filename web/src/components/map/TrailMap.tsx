import React from 'react'
import { useEffect, useState } from 'react'
import { Map, MapProps, AttributionControl, LayersControl, Children } from 'react-leaflet'
import BufferedTileLayer from './BufferedTileLayer'

// Children optional, not required
type Props = Omit<MapProps, "children"> & { children?: Children }

type Token = {
    access_token: string,
    expires_in: number,
    expires_at: string
}

const TrailMap: React.FC<Props> = (props: Props) => {
    const [token, setToken] = useState("")

    const refreshToken = () => {
        useEffect(() => {
          fetch("http://localhost:8080/token") // TODO: useEnv
            .then(res => res.json() as Promise<Token>)
            .then(
                (result) => {
                    setToken(result.access_token)
                    console.log(result)
                    setTimeout(refreshToken, result.expires_in / 1e6); // TODO: a bit more typesafe
                },
                (error) => console.log(error) // TODO: better error handling
            )
        })
    }

    refreshToken()

    return (
            <Map
                 {...props}
                useFlyTo={true}
                attributionControl={false}
            >
                <LayersControl position="topright">
                    <LayersControl.BaseLayer
                        name="ArcGIS.Satellite"
                        checked={true}
                    >
                        <BufferedTileLayer
                            url="https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
                            bufferRadius={2}
                        />
                    </LayersControl.BaseLayer>
                </LayersControl>
                <AttributionControl position="bottomright" prefix={false} />
            </Map>
    )
}

export default TrailMap
