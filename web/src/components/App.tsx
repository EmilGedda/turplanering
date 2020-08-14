import React from 'react'
import { useState } from 'react'
import { Map, AttributionControl, LayersControl } from 'react-leaflet'
import BufferedTileLayer from './map/BufferedTileLayer'
import Searchbar from './Searchbar'

const App: React.FC = () => {
    const [viewport, setViewport] = useState({
        center: [59.334591, 18.06324] as [number, number],
        zoom: 8,
    })

    const centerToGPS: PositionCallback = (pos) => {
        const { latitude, longitude } = pos.coords
        setViewport({
            center: [latitude, longitude],
            zoom: 13, // TODO calculate this from accuracy in pos, and draw a circle
        })
    }

    return (
        <div>
            <Searchbar onGPSTrack={centerToGPS} />
            <Map
                id="trailmap"
                viewport={viewport}
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
        </div>
    )
}

export default App
