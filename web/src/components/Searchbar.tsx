import React from 'react'
import { useState, FC } from 'react'
import { makeStyles } from '@material-ui/core/styles'

import Divider from '@material-ui/core/Divider'
import InputBase from '@material-ui/core/InputBase'
import Paper from '@material-ui/core/Paper'

import AccountCircleIcon from '@material-ui/icons/AccountCircle'
import AddCircleIcon from '@material-ui/icons/AddCircle'
import GpsFixedIcon from '@material-ui/icons/GpsFixed'
import GpsNotFixedIcon from '@material-ui/icons/GpsNotFixed'
import HelpIcon from '@material-ui/icons/Help'
import IconButton from '@material-ui/core/IconButton'
import SearchIcon from '@material-ui/icons/Search'

const useStyles = makeStyles((theme) => ({
    root: {
        padding: '2px 4px',
        margin: 90,
        display: 'flex',
        alignItems: 'center',
        width: 600,
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
    const classes = useStyles(),
        [watchID, setWatchID] = useState(0),
        [isTracking, setIsTracking] = useState(false), // TODO: switch to enum
        gps = navigator.geolocation

    const stopTracking = () => {
        setIsTracking(false)
        gps.clearWatch(watchID)
    }

    const handleGPSError: PositionErrorCallback = (err: PositionError) => {
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
        <Paper component="form" className={classes.root} elevation={5}>
            <IconButton className={classes.iconButton}>
                <HelpIcon />
            </IconButton>
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
            <IconButton className={classes.iconButton}>
                <AddCircleIcon />
            </IconButton>
        </Paper>
    )
}

export default Searchbar
