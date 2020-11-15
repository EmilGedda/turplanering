import React, { useState, FC, useEffect } from 'react';
import {
  AccountCircle,
  GpsFixed,
  GpsNotFixed,
  GpsOff,
  Search,
} from '@material-ui/icons';
import {
  Divider,
  IconButton,
  IconButtonProps,
  InputBase,
  Paper,
  Slide,
} from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';

const searchbarCSS = makeStyles((theme) => ({
  searchbar: {
    position: 'fixed',
    left: 0,
    right: 0,
    padding: '2px 4px',
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
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
}));

type GPSProps = { status: GPSStatus } & IconButtonProps;

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
      );
    case GPSStatus.Loading:
      return (
        <IconButton {...props} color='primary'>
          <GpsNotFixed />
        </IconButton>
      );
    case GPSStatus.Tracking:
      return (
        <IconButton {...props} color='primary'>
          <GpsFixed />
        </IconButton>
      );
    case GPSStatus.Disabled:
      return (
        <IconButton {...props} color='secondary'>
          <GpsOff />
        </IconButton>
      );
  }
};

type Props = {
  shown: boolean;
  onGPSTrack: PositionCallback;
  onGPSLocate: PositionCallback;
  onGPSDeactivate: VoidFunction;
};

const Searchbar: FC<Props> = (props: Props) => {
  const { onGPSTrack, onGPSDeactivate, onGPSLocate, shown } = props,
    [watchID, setWatchID] = useState(0),
    [gpsStatus, setGPSStatus] = useState(GPSStatus.Inactive),
    css = searchbarCSS(),
    gps = navigator.geolocation;

  const stopTracking = () => {
    gps.clearWatch(watchID);
    setGPSStatus(GPSStatus.Inactive);
    onGPSDeactivate();
  };

  const disableGPS = () => setGPSStatus(GPSStatus.Disabled);

  const watchGPS = () => {
    const opts = { enableHighAccuracy: true };
    setGPSStatus(GPSStatus.Loading);
    gps.getCurrentPosition(
      (coords) => {
        setGPSStatus(GPSStatus.Tracking);
        onGPSLocate(coords);
      },
      disableGPS,
      opts
    );
    setWatchID(gps.watchPosition(onGPSTrack, disableGPS, opts));
  };

  useEffect(() => {
    return () => gps.clearWatch(watchID);
  }, [gps, watchID]);

  return (
    <Slide direction='down' in={shown} mountOnEnter unmountOnExit>
      <Paper className={css.searchbar} elevation={5}>
        <IconButton className={css.iconButton}>
          <AccountCircle />
        </IconButton>
        <Divider className={css.divider} orientation='vertical' />
        <InputBase className={css.input} placeholder='Sök' />
        <IconButton type='submit' className={css.iconButton}>
          <Search />
        </IconButton>
        <Divider className={css.divider} orientation='vertical' />
        <GPSButton
          status={gps ? gpsStatus : GPSStatus.Disabled}
          className={css.iconButton}
          onClick={gpsStatus == GPSStatus.Tracking ? stopTracking : watchGPS}
        />
      </Paper>
    </Slide>
  );
};

export default Searchbar;
