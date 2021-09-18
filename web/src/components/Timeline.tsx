import React, { FC, useState, useEffect } from 'react';
import {
  Divider,
  Grid,
  IconButton,
  IconButtonProps,
  Paper,
  Slide,
  Slider,
  Typography
} from '@mui/material';
import { PlayCircleOutline, PauseCircleOutline } from '@mui/icons-material';
import makeStyles from '@mui/styles/makeStyles';
import { common } from '@mui/material/colors';

type LabelProps = {
  timepoint: Date;
};

const capitalize = (str: string) => str.charAt(0).toUpperCase() + str.slice(1);

const labelCSS = makeStyles((theme) => ({
  time: {
    fontSize: theme.typography.pxToRem(24)
  },
  date: {
    fontStyle: 'italic',
    color: 'grey',
    marginLeft: '12px',
    fontSize: theme.typography.pxToRem(20)
  }
}));

const TimepointLabel: FC<LabelProps> = ({ timepoint }) => {
  const locale = 'sv-SE';
  const css = labelCSS();

  const time = timepoint.toLocaleTimeString(locale, {
    hour: 'numeric',
    minute: 'numeric'
  });

  const date = timepoint.toLocaleDateString(locale, {
    weekday: 'long',
    month: 'long',
    day: 'numeric'
  });

  return (
    <div>
      <Typography variantMapping={{ body1: 'span' }} className={css.time}>
        {time}
      </Typography>
      <Typography variantMapping={{ body1: 'span' }} className={css.date}>
        {capitalize(date)}
      </Typography>
    </div>
  );
};

export type TimelineProps = {
  shown: boolean;
  timepoints: Date[];
  onChange: (timepoint: Date) => void;
};

export type SliderCallback = (event: Event, value: number | number[]) => void;

const Timeline: FC<TimelineProps> = (props: TimelineProps) => {
  const { shown, timepoints, onChange } = props;
  const css = timelineCSS();
  const [currentIndex, setCurrentIndex] = useState(0);
  const [running, setRunning] = useState(false);
  const [watchID, setWatchID] = useState(-1);

  const start = (run: boolean) => {
    if (run && currentIndex == timepoints.length - 1) {
      setCurrentIndex(0);
    }
    setRunning(run);
  };

  const updateTime = (f: (timepoint: Date) => void): SliderCallback => {
    return (_, value) => {
      const v = value as number;
      if (v == currentIndex || v < 0 || v >= timepoints.length) return;

      if (running) {
        setRunning(false);
      }

      if (watchID != -1) {
        clearTimeout(watchID);
      }

      setCurrentIndex(v);
      setWatchID(
        window.setTimeout(
          (idx: number) => {
            setWatchID(-1);
            f(timepoints[idx]);
          },
          300,
          v
        )
      );
    };
  };

  useEffect(() => {
    if (running) {
      const id = window.setTimeout(() => {
        onChange(timepoints[currentIndex + 1]);
        setCurrentIndex(currentIndex + 1);
        if (currentIndex + 1 == timepoints.length - 1) {
          setWatchID(-1);
          setRunning(false);
        }
      }, 2500);
      setWatchID(id);
      return () => clearInterval(id);
    }
  }, [running, currentIndex, onChange, timepoints]);

  if (!shown && watchID != -1) {
    clearTimeout(watchID);
    setWatchID(-1);
    setRunning(false);
  }

  return (
    <Slide direction='up' in={shown} mountOnEnter>
      <div className={css.outer}>
        <Paper elevation={5} square={false} className={css.bar}>
          <Grid
            container
            direction='row'
            justifyContent='flex-start'
            alignItems='center'
          >
            <PlayButton onClick={start} isplaying={running} />
            <Divider className={css.divider} orientation='vertical' />
            <div style={{ flex: 1, marginLeft: 2 }}>
              <TimepointLabel timepoint={timepoints[currentIndex]} />
              <Slider
                max={timepoints.length - 1}
                value={currentIndex}
                onChange={updateTime(onChange)}
              />
            </div>
          </Grid>
        </Paper>
      </div>
    </Slide>
  );
};

type PlayButtonProps = {
  isplaying: boolean;
  onClick: (play: boolean) => void;
} & Omit<IconButtonProps, 'onClick'>;

const PlayButton: FC<PlayButtonProps> = (props: PlayButtonProps) => {
  const { onClick: callback, isplaying, ...baseProps } = props;
  const css = timelineCSS();
  const color = iconColor();

  const onClick = () => callback(!isplaying);

  return (
    <IconButton
      color='primary'
      onClick={onClick}
      className={css.icon}
      classes={color}
      edge='end'
      {...baseProps}
      size='large'
    >
      {isplaying ? (
        <PauseCircleOutline className={css.icon} />
      ) : (
        <PlayCircleOutline className={css.icon} />
      )}
    </IconButton>
  );
};

const iconColor = makeStyles((theme) => ({
  colorPrimary: {
    color:
      theme.palette.mode == 'dark' ? common.white : theme.palette.primary.main
  }
}));

const timelineCSS = makeStyles((theme) => ({
  outer: {
    position: 'fixed',
    left: 0,
    right: 0,
    bottom: 0,
    padding: '2px 4px',
    maxWidth: '95%',
    margin: '0 auto',
    [theme.breakpoints.down('md')]: {
      marginBottom: '5px'
    },
    [theme.breakpoints.up('sm')]: {
      marginBottom: '15px'
    },
    width: 600,
    zIndex: 1001
  },
  bar: {
    width: '100%',
    paddingRight: 32
  },
  center: {
    width: '100%',
    left: 0,
    right: 0
  },
  divider: {
    height: 60,
    margin: 12
  },
  icon: {
    fontSize: 60
  }
}));

export default Timeline;
