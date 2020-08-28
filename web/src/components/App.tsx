import React from 'react'
import { useState, useEffect } from 'react'
import Searchbar from './Searchbar'
import TrailMap from './map/TrailMap'
import { Fab } from '@material-ui/core'
import AddIcon from '@material-ui/icons/Add'
import { makeStyles } from '@material-ui/core/styles'

type Environment = {
    apiURL: string
    environment: string
}

type Props = {
    env: Environment
}

const appStyles = makeStyles((theme) => ({
    map: {
        position: 'absolute',
        top: 0,
        right: 0,
        height: '100vh',
        width: '100vw',
        zIndex: -1,
    },
    add: {
        margin: theme.spacing(4),
        position: 'absolute',
        bottom: 0,
        right: 0,
    },
}))

const App: React.FC<Props> = (props: Props) => {
    useEffect(() => console.log('Running in ' + props.env.environment))

    const styles = appStyles()

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
            <Fab className={styles.add} color="primary">
                <AddIcon />
            </Fab>
            <TrailMap
                className={styles.map}
                viewport={viewport}
                token={token}
            />
        </div>
    )
}

export { App }
export type { Props as AppProps, Environment }
