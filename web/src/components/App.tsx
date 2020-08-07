import React from 'react'
import { useState } from 'react'
import { Map } from 'react-leaflet'
import {
    TextField,
    Box,
    InputAdornment,
    Button,
    makeStyles,
} from '@material-ui/core'
import SearchIcon from '@material-ui/icons/Search'
import GpsFixedIcon from '@material-ui/icons/GpsFixed'
import BufferedTileLayer from './map/BufferedTileLayer'

const useStyles = makeStyles((theme) => ({
    root: {
        display: 'flex',
        flexWrap: 'wrap',
    },
    margin: {
        margin: theme.spacing(1),
    },
    textSize: {
        fontSize: '15pt',
    },
}))

const App: React.FC = () => {
    const classes = useStyles()

    const [viewport, setViewport] = useState({
        center: [59.334591, 18.06324] as [number, number],
        zoom: 8,
    })

    const centerToGPS = () => {
        navigator.geolocation.getCurrentPosition((pos) => {
            const { latitude, longitude } = pos.coords
            console.log(pos)
            setViewport({
                center: [latitude, longitude],
                zoom: 13,
            })
        })
    }
    return (
        <div>
            <Box
                bgcolor="white"
                borderRadius={4}
                style={{
                    border: '2px solid #ccc',
                    display: 'inline-block',
                    margin: '50px',
                }}
            >
                <TextField
                    type="search"
                    placeholder="Sök"
                    style={{
                        margin: '0px',
                    }}
                    InputProps={{
                        classes: {
                            input: classes.textSize,
                        },
                        startAdornment: (
                            <InputAdornment position="start">
                                <SearchIcon
                                    style={{
                                        color: 'rgb(120, 120, 120)',
                                        marginTop: '-1px',
                                        marginLeft: '5px',
                                    }}
                                />
                            </InputAdornment>
                        ),
                        disableUnderline: true,
                    }}
                />
            </Box>
            <Button variant="contained" size="small" onClick={centerToGPS}>
                <GpsFixedIcon />
            </Button>
            <Map id="trailmap" viewport={viewport} useFlyTo={true}>
                <BufferedTileLayer
                    url="https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
                    attribution='&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
                    bufferRadius={2}
                />
            </Map>
        </div>
    )
}

export default App
