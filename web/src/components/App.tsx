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
import { ForecastAPI, ForecastTimestamps } from '../forecast'
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
    forecastAPI: ForecastAPI
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
    const css = appStyles(),
        { env, forecastAPI } = props

    useEffect(() => console.log('Running in ' + env.environment), [
        env.environment,
    ])

    const mapRef = createRef<Map>(),
        groupRef = createRef<FeatureGroup>()

    const [showBar, setShowBar] = useState(false)
    const [hasForecast, setHasForecast] = useState(false)
    const [displayTime, setDisplayTime] = useState<Date | undefined>()
    const [referenceTime, setReferenceTime] = useState<Date | undefined>()

    const [forecast, setForecast] = useState<ForecastTimestamps>({
        reference: new Date(),
        validTimes: [new Date()],
    })

    const [overlays, setOverlays] = useState({
        weather: false,
        wind: false,
        temperature: false,
        layers: 0,
    })

    const [position, setPosition] = useState<Coordinates>()
    const [viewport, setViewPort] = useState<Viewport>({
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
        void (async (): Promise<void> => {
            console.log('Fetching forecast...')
            const before = new Date()
            const forecast = await forecastAPI.ValidTimes()
            const duration = new Date().getTime() - before.getTime()
            console.log(
                `Fetched forecast, reference time: ${forecast.reference.toUTCString()}` +
                    `, with ${forecast.validTimes.length} timestamps in ${duration}ms`
            )
            setForecast(forecast)
            setReferenceTime(forecast.reference)
            setDisplayTime(forecast.validTimes[0])
            setHasForecast(true)
        })()
    }, [forecastAPI])

    useEffect(() => {
        const timeout = setTimeout(setShowBar, 500, true)
        return () => clearTimeout(timeout)
    }, [])

    const toggleOverlay = (key: 'weather' | 'wind' | 'temperature') => {
        return (enable: boolean) => {
            const count = overlays.layers + (enable ? 1 : -1)
            const obj = { ...overlays, layers: count }
            obj[key] = enable
            setOverlays(obj)
        }
    }

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

                {overlays.weather && displayTime && referenceTime && (
                    <WeatherLayer
                        referenceTime={referenceTime}
                        displayTime={displayTime}
                    />
                )}

                <FeatureGroup ref={groupRef}>
                    {position && <GPSMarker pos={position} />}
                </FeatureGroup>
            </TrailMap>

            <Timeline
                shown={overlays.layers > 0 && showBar}
                timepoints={forecast.validTimes}
                onChange={setDisplayTime}
            />

            <Overlaybar shown={hasForecast && showBar}>
                <WeatherToggleButton onClick={toggleOverlay('weather')} />
                <WindToggleButton onClick={toggleOverlay('wind')} />
                <TemperatureToggleButton
                    onClick={toggleOverlay('temperature')}
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
