import React, { useState, FC, useEffect } from 'react';
import {
  AccountCircle,
  GpsFixed,
  GpsNotFixed,
  GpsOff,
  Search
} from '@material-ui/icons';
import {
  Divider,
  IconButton,
  IconButtonProps,
  InputBase,
  Paper
} from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';

const searchbarCSS = makeStyles((theme) => ({
  searchbar: {
    maxWidth: '800px',
    width: '95%',
    zIndex: 1000,
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    height: 48,
    padding: '0px 4px'
  },
  input: {
    marginLeft: theme.spacing(1),
    flex: 1
  },
  iconButton: {
    padding: 8
  },
  divider: {
    height: 28,
    margin: 2
  }
}));

type GPSProps = { status: GPSStatus } & IconButtonProps;

enum GPSStatus {
  Disabled,
  Tracking,
  Loading,
  Inactive
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
  onGPSTrack: PositionCallback;
  onGPSLocate: PositionCallback;
  onGPSDeactivate: VoidFunction;
};

const Searchbar: FC<Props> = (props: Props) => {
  const { onGPSTrack, onGPSDeactivate, onGPSLocate } = props,
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
  );
};

export default Searchbar;
