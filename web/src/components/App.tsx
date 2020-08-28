import React from 'react'
import { useState, useEffect } from 'react'
import Searchbar from './Searchbar'
import TrailMap from './map/TrailMap'

type Environment = {
    apiURL: string
    environment: string
}

type Props = {
    env: Environment
}

// type Token = {
//     access_token: string
//     expires_in: number
//     expires_at: string
// }
//
// const useToken = () => {
//     const [token, setToken] = useState('')
//
//     useEffect(() => {
//         fetch('http://localhost:8080/token') // TODO: useEnv
//             .then((res) => res.json() as Promise<Token>)
//             .then(
//                 (result) => {
//                     setToken(result.access_token)
//                     console.log(result)
//                     setTimeout(useToken, result.expires_in / 1e6 - 1e3) // TODO: a bit more typesafe timing
//                 },
//                 (error) => console.log(error), // TODO: better error handling
//             )
//     })
//     return token
// }

const App: React.FC<Props> = (props: Props) => {
    useEffect(() => console.log('Running in ' + props.env.environment))

    const token = '' // await fetch('http://localhost:8080/token').then((res) => res.json() as Promise<Token>)

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
            <TrailMap id="trailmap" viewport={viewport} token={token} />
        </div>
    )
}

export { App }
export type { Props as AppProps, Environment }
