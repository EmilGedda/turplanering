import React from 'react';
import { Map, LayerGroup } from 'react-leaflet';
import { LatLngTuple } from 'leaflet';
import { LayerContext } from './context/LayerContext';
import AddMarkerButton from './MarkerButton';
import BufferedTileLayer from './BufferedTileLayer';

const defaultLatLng: LatLngTuple = [59.334591, 18.06324];

const TrailMap: React.FC = () => {
    const { point } = React.useContext(LayerContext);

    return (
        <Map id="trailmap" center={defaultLatLng} zoom={8} preferCanvas={true}>
            <BufferedTileLayer
                url="https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
                attribution='&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
                keepBuffer={4}
            ></BufferedTileLayer>
        </Map>
    );
    // <TileLayer
    //     url="https://api.maptiler.com/maps/hybrid/{z}/{x}/{y}.jpg?key=GJepBetlUMcQfFZRdreA"
    //     zoomOffset={-1}
    //     tileSize={512}
    // ></TileLayer>
    //             <TileLayer
    //                 url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    //                 attribution='&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
    //             ></TileLayer>
};

export default TrailMap;
