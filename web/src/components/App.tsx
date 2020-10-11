import React, { useState, useEffect, createRef } from 'react'
import { FeatureGroup, Map, Viewport } from 'react-leaflet'
import { makeStyles } from '@material-ui/core/styles'
import Searchbar from './Searchbar'
import Overlaybar from './Overlaybar'
import { TrailMap, GPSMarker } from './map/TrailMap'

type Environment = {
    apiURL: string
    environment: string
    browser: {
        hasTouch: boolean
    }
}

type Props = {
    env: Environment
}

const appStyles = makeStyles(() => ({
    fullscreen: {
        position: 'absolute',
        top: 0,
        right: 0,
        height: '100%',
        width: '100vw',
    },
}))

const App: React.FC<Props> = (props: Props) => {
    useEffect(() => console.log('Running in ' + env.environment))

    const css = appStyles(),
        { env } = props

    const mapRef = createRef<Map>(),
        groupRef = createRef<FeatureGroup>()

    const [showWeather, setShowWeather] = useState(false),
        [showBar, setShowBar] = useState(false),
        [position, setPosition] = useState<Coordinates | undefined>(undefined),
        [viewport, setViewPort] = useState<Viewport>({
            center: [59.334591, 18.06324],
            zoom: 8,
        })

    const hideGPSMarker = () => setPosition(undefined)

    const updatePosition: PositionCallback = ({ coords }) => {
        setPosition(coords)
    }

    const flyToPosition: PositionCallback = ({ coords }) => {
        if (!mapRef.current || !groupRef.current) return
        const map = mapRef.current.leafletElement
        const group = groupRef.current.leafletElement
        setPosition(coords)
        map.fitBounds(group.getBounds())
        const { lat, lng } = map.getCenter()
        setViewPort({
            center: [lat, lng],
            zoom: map.getZoom(),
        })
    }

    useEffect(() => {
        const timeout = setTimeout(setShowBar, 500, true)
        return () => clearTimeout(timeout)
    }, [])

    return (
        <div className={css.fullscreen}>
            <TrailMap
                className={css.fullscreen}
                viewport={viewport}
                hasTouch={env.browser.hasTouch}
                ref={mapRef}
                showPrecipitation={showWeather}
            >
                <FeatureGroup ref={groupRef}>
                    {position && <GPSMarker pos={position} />}
                </FeatureGroup>
            </TrailMap>
            <Overlaybar
                shown={showBar}
                precipitation={{ onClick: setShowWeather }}
            />
            <Searchbar
                shown={showBar}
                onGPSLocate={flyToPosition}
                onGPSTrack={updatePosition}
                onGPSDeactivate={hideGPSMarker}
            />
            {/*
            <Fab className={styles.add} color="primary">
                <AddIcon />
            </Fab>
            */}
        </div>
    )
}

export { App }
export type { Props as AppProps, Environment }
