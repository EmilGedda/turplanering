import React from 'react'
import { Map } from 'react-leaflet'
import BufferedTileLayer from './BufferedTileLayer'

const TrailMap: React.FC = () => {
    const sthlm = { lat: 59.334591, lng: 18.06324 }

    return (
        <Map id="trailmap" center={sthlm} zoom={8}>
            <BufferedTileLayer
                url="https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
                attribution='&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
            />
        </Map>
    )
    //  <TileLayer
    //      url="https://api.maptiler.com/maps/hybrid/{z}/{x}/{y}.jpg?key=GJepBetlUMcQfFZRdreA"
    //      zoomOffset={-1}
    //      tileSize={512}
    //  ></TileLayer>
    //              <TileLayer
    //                  url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    //                  attribution='&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
    //              ></TileLayer>
}

export default TrailMap
