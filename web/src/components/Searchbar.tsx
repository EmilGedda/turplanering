import React, { useState, FC, useEffect } from 'react'
import {
    AccountCircle,
    GpsFixed,
    GpsNotFixed,
    GpsOff,
    Search,
} from '@material-ui/icons'
import {
    Slide,
    Divider,
    InputBase,
    Paper,
    IconButton,
    IconButtonProps,
} from '@material-ui/core'
import { makeStyles } from '@material-ui/core/styles'

const searchbarCSS = makeStyles((theme) => ({
    searchbar: {
        position: 'relative',
        padding: '2px 4px',
        display: 'flex',
        alignItems: 'center',
        width: '95%',
        margin: '0 auto',
        [theme.breakpoints.down('sm')]: {
            marginTop: '5px',
        },
        [theme.breakpoints.up('sm')]: {
            marginTop: '15px',
        },
        maxWidth: '800px',
        zIndex: 1000,
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

type GPSProps = { status: GPSStatus } & IconButtonProps

enum GPSStatus {
    Disabled,
    Tracking,
    Loading,
    Inactive,
}

const GPSButton: FC<GPSProps> = ({ status, ...props }: GPSProps) => {
    switch (status) {
        case GPSStatus.Inactive:
            return (
                <IconButton {...props}>
                    <GpsNotFixed />
                </IconButton>
            )
        case GPSStatus.Loading:
            return (
                <IconButton {...props} color="primary">
                    <GpsNotFixed />
                </IconButton>
            )
        case GPSStatus.Tracking:
            return (
                <IconButton {...props} color="primary">
                    <GpsFixed />
                </IconButton>
            )
        case GPSStatus.Disabled:
            return (
                <IconButton {...props} color="secondary">
                    <GpsOff />
                </IconButton>
            )
    }
}

type Props = {
    shown: boolean
    onGPSTrack: PositionCallback
    onGPSLocate: PositionCallback
    onGPSDeactivate: VoidFunction
}

const Searchbar: FC<Props> = (props: Props) => {
    const { onGPSTrack } = props,
        classes = searchbarCSS(),
        [watchID, setWatchID] = useState(0),
        [gpsStatus, setGPSStatus] = useState(GPSStatus.Inactive),
        gps = navigator.geolocation

    const stopTracking = () => {
        gps.clearWatch(watchID)
        setGPSStatus(GPSStatus.Inactive)
        props.onGPSDeactivate()
    }

    const disableGPS = () => setGPSStatus(GPSStatus.Disabled)

    const watchGPS = () => {
        const opts = { enableHighAccuracy: true }
        setGPSStatus(GPSStatus.Loading)
        gps.getCurrentPosition(
            (coords) => {
                setGPSStatus(GPSStatus.Tracking)
                props.onGPSLocate(coords)
            },
            disableGPS,
            opts
        )
        setWatchID(gps.watchPosition(onGPSTrack, disableGPS, opts))
    }

    useEffect(() => {
        return () => gps.clearWatch(watchID)
    }, [gps, watchID])

    return (
        <Slide direction="down" in={props.shown} mountOnEnter unmountOnExit>
            <Paper className={classes.searchbar} elevation={5}>
                <IconButton className={classes.iconButton}>
                    <AccountCircle />
                </IconButton>
                <Divider className={classes.divider} orientation="vertical" />
                <InputBase className={classes.input} placeholder="Sök" />
                <IconButton type="submit" className={classes.iconButton}>
                    <Search />
                </IconButton>
                <Divider className={classes.divider} orientation="vertical" />
                <GPSButton
                    status={gps ? gpsStatus : GPSStatus.Disabled}
                    className={classes.iconButton}
                    onClick={
                        gpsStatus == GPSStatus.Tracking
                            ? stopTracking
                            : watchGPS
                    }
                />
            </Paper>
        </Slide>
    )
}

export default Searchbar
