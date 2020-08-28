import React from 'react'
import { useState, FC } from 'react'
import { makeStyles } from '@material-ui/core/styles'

import Divider from '@material-ui/core/Divider'
import InputBase from '@material-ui/core/InputBase'
import Paper from '@material-ui/core/Paper'

import AccountCircleIcon from '@material-ui/icons/AccountCircle'
import GpsFixedIcon from '@material-ui/icons/GpsFixed'
import GpsNotFixedIcon from '@material-ui/icons/GpsNotFixed'
import IconButton from '@material-ui/core/IconButton'
import SearchIcon from '@material-ui/icons/Search'

const searchbarCSS = makeStyles((theme) => ({
    searchbar: {
        padding: '2px 4px',
        display: 'flex',
        alignItems: 'center',
        width: '95%',
        margin: '0 auto',
        marginTop: '15px',
        maxWidth: '800px',
    },
    input: {
        marginLeft: theme.spacing(1),
        flex: 1,
    },
    iconButton: {
        padding: 10,
    },
    divider: {
        height: 28,
        margin: 4,
    },
}))

type Props = { onGPSTrack: PositionCallback }

const Searchbar: FC<Props> = ({ onGPSTrack }: Props) => {
    const classes = searchbarCSS(),
        [watchID, setWatchID] = useState(0),
        [isTracking, setIsTracking] = useState(false), // TODO: switch to enum
        gps = navigator.geolocation

    const stopTracking = () => {
        setIsTracking(false)
        gps.clearWatch(watchID)
    }

    const handleGPSError: PositionErrorCallback = (err: PositionError) => {
        // TODO: update the gps icon to corresponding error
        console.log('GPS error:')
        console.log(err)
        stopTracking()
    }

    const watchGPS = () => {
        setIsTracking(true)
        setWatchID(
            gps.watchPosition(onGPSTrack, handleGPSError, {
                enableHighAccuracy: true,
            }),
        )
    }

    return (
        <Paper component="form" className={classes.searchbar} elevation={5}>
            <IconButton className={classes.iconButton}>
                <AccountCircleIcon />
            </IconButton>
            <Divider className={classes.divider} orientation="vertical" />
            <InputBase className={classes.input} placeholder="Sök" />
            <IconButton type="submit" className={classes.iconButton}>
                <SearchIcon />
            </IconButton>
            <Divider className={classes.divider} orientation="vertical" />
            <IconButton
                color={isTracking ? 'primary' : undefined}
                className={classes.iconButton}
                onClick={isTracking ? stopTracking : watchGPS}
            >
                {isTracking ? <GpsFixedIcon /> : <GpsNotFixedIcon />}
            </IconButton>
        </Paper>
    )
}

export default Searchbar
