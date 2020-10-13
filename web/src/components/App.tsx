import React, { useState, useEffect, createRef } from 'react'
import { FeatureGroup, Map, Viewport } from 'react-leaflet'
import { makeStyles } from '@material-ui/core/styles'
import Searchbar from './Searchbar'
import {
    Overlaybar,
    WeatherToggleButton,
    WindToggleButton,
    TemperatureToggleButton,
} from './Overlaybar'
import Timeline from './Timeline'
import BufferedWMSLayer from './map/BufferedWMSTileLayer'
import { TrailMap, GPSMarker, WeatherLayer } from './map/TrailMap'

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

type BooleanToggle = (show: boolean) => void

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
    const css = appStyles(),
        { env } = props

    useEffect(() => console.log('Running in ' + env.environment), [
        env.environment,
    ])

    const mapRef = createRef<Map>(),
        groupRef = createRef<FeatureGroup>()

    const [showWeather, setShowWeather] = useState(false),
        [showWind, setShowWind] = useState(false),
        [showTemperature, setShowTemperature] = useState(false),
        [showBar, setShowBar] = useState(false),
        [layerCount, setLayerCount] = useState(0),
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
        if (!mapRef.current || !groupRef.current || !coords) return
        const map = mapRef.current.leafletElement
        const group = groupRef.current.leafletElement
        if (!map || !group) return
        setPosition(coords)
        const zoom = map.getBoundsZoom(group.getBounds())
        const { latitude, longitude } = coords
        setViewPort({
            center: [latitude, longitude],
            zoom: zoom - 0.5,
        })
    }

    useEffect(() => {
        const timeout = setTimeout(setShowBar, 500, true)
        return () => clearTimeout(timeout)
    }, [])

    const layerToggle = (toggle: BooleanToggle): BooleanToggle => {
        return (show: boolean): void => {
            setLayerCount(layerCount + (show ? 1 : -1))
            toggle(show)
        }
    }

    const referenceTime = new Date('2020-10-12T21:00:00Z')

    return (
        <div className={css.fullscreen}>
            <TrailMap
                className={css.fullscreen}
                viewport={viewport}
                hasTouch={env.browser.hasTouch}
                ref={mapRef}
            >
                <BufferedWMSLayer
                    url="https://minkarta.lantmateriet.se/map/topowebb/"
                    layers="topowebbkartan"
                />

                {showWeather && <WeatherLayer referenceTime={referenceTime} />}
                {showWind && <WeatherLayer referenceTime={referenceTime} />}
                {showTemperature && (
                    <WeatherLayer referenceTime={referenceTime} />
                )}

                <FeatureGroup ref={groupRef}>
                    {position && <GPSMarker pos={position} />}
                </FeatureGroup>
            </TrailMap>

            <Timeline shown={layerCount > 0 && showBar} timepoints={[]} />

            <Overlaybar shown={showBar}>
                <WeatherToggleButton onClick={layerToggle(setShowWeather)} />
                <WindToggleButton onClick={layerToggle(setShowWind)} />
                <TemperatureToggleButton
                    onClick={layerToggle(setShowTemperature)}
                />
            </Overlaybar>
            <Searchbar
                shown={showBar}
                onGPSLocate={flyToPosition}
                onGPSTrack={updatePosition}
                onGPSDeactivate={hideGPSMarker}
            />
        </div>
    )
}

export { App }
export type { Props as AppProps, Environment }
