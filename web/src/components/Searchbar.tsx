import React, { useState, FC, useEffect } from 'react';
import { styled } from '@mui/material/styles';
import {
  AccountCircle,
  GpsFixed,
  GpsNotFixed,
  GpsOff,
  Search
} from '@mui/icons-material';
import { Divider, IconButton, InputBase, Paper } from '@mui/material';

const StyledPaper = styled(Paper)((_) => ({
  maxWidth: '800px',
  width: '95%',
  zIndex: 1000,
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  height: 48,
  padding: '0px 4px',
  pointerEvents: 'auto'
}));

const StyledInput = styled(InputBase)(({ theme }) => ({
  marginLeft: theme.spacing(1),
  flex: 1
}));

const StyledDivider = styled(Divider)((_) => ({
  height: 28,
  margin: 2
}));

const StyledIconButton = styled(IconButton)((_) => ({
  padding: 8
}));

export type GPSProps = { status: GPSStatus; onClick: () => void };

export enum GPSStatus {
  Disabled,
  Tracking,
  Loading,
  Inactive
}

const GPSButton: FC<GPSProps> = ({ status, ...props }: GPSProps) => {
  switch (status) {
    case GPSStatus.Inactive:
      return (
        <StyledIconButton {...props} size='large'>
          <GpsNotFixed />
        </StyledIconButton>
      );
    case GPSStatus.Loading:
      return (
        <StyledIconButton {...props} color='primary' size='large'>
          <GpsNotFixed />
        </StyledIconButton>
      );
    case GPSStatus.Tracking:
      return (
        <StyledIconButton {...props} color='primary' size='large'>
          <GpsFixed />
        </StyledIconButton>
      );
    case GPSStatus.Disabled:
      return (
        <StyledIconButton {...props} color='secondary' size='large'>
          <GpsOff />
        </StyledIconButton>
      );
  }
};

export type SearchbarProps = {
  onGPSTrack: PositionCallback;
  onGPSLocate: PositionCallback;
  onGPSDeactivate: VoidFunction;
};

const Searchbar: FC<SearchbarProps> = (props: SearchbarProps) => {
  const { onGPSTrack, onGPSDeactivate, onGPSLocate } = props,
    [watchID, setWatchID] = useState(0),
    [gpsStatus, setGPSStatus] = useState(GPSStatus.Inactive),
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
    <StyledPaper elevation={5}>
      <StyledIconButton size='large'>
        <AccountCircle />
      </StyledIconButton>
      <StyledDivider orientation='vertical' />
      <StyledInput placeholder='Sök' />
      <StyledIconButton type='submit' size='large'>
        <Search />
      </StyledIconButton>
      <StyledDivider orientation='vertical' />
      <GPSButton
        status={gps ? gpsStatus : GPSStatus.Disabled}
        onClick={gpsStatus == GPSStatus.Tracking ? stopTracking : watchGPS}
      />
    </StyledPaper>
  );
};

export default Searchbar;
