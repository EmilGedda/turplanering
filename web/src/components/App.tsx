import React from 'react'
import { useState } from 'react'
import Searchbar from './Searchbar'
import TrailMap from './map/TrailMap'

type Environment = {
    apiURL: string
    environment: string
}

type Props = {
    env: Environment
}

const App: React.FC<Props> = (props: Props) => {

    React.useEffect(() => console.log("Running in " + props.env.environment))

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
            <TrailMap id="trailmap" viewport={viewport} />
        </div>
    )
}

export { App }
export type { Props as AppProps, Environment }
