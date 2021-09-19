import React, { FC, useState, useEffect } from 'react';
import { styled } from '@mui/styles';
import {
  Divider,
  Grid,
  IconButton,
  Paper,
  Slide,
  Slider,
  Typography
} from '@mui/material';
import { PlayCircleOutline, PauseCircleOutline } from '@mui/icons-material';
import { common } from '@mui/material/colors';

const PREFIX = 'Timeline';

const classes = {
  time: `${PREFIX}-time`,
  date: `${PREFIX}-date`
};

const StyledTypography = styled(Typography)(({ theme }) => ({
  [`&.${classes.time}`]: {
    fontSize: theme.typography.pxToRem(24)
  },
  [`&.${classes.date}`]: {
    fontStyle: 'italic',
    color: 'grey',
    marginLeft: '12px',
    fontSize: theme.typography.pxToRem(20)
  }
}));

type LabelProps = {
  timepoint: Date;
};

const capitalize = (str: string) => str.charAt(0).toUpperCase() + str.slice(1);

const TimepointLabel: FC<LabelProps> = ({ timepoint }) => {
  const locale = 'sv-SE';

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
      <StyledTypography
        variantMapping={{ body1: 'span' }}
        className={classes.time}
      >
        {time}
      </StyledTypography>
      <StyledTypography
        variantMapping={{ body1: 'span' }}
        className={classes.date}
      >
        {capitalize(date)}
      </StyledTypography>
    </div>
  );
};

export type TimelineProps = {
  shown: boolean;
  timepoints: Date[];
  onChange: (timepoint: Date) => void;
};

export type SliderCallback = (event: Event, value: number | number[]) => void;

const OuterDiv = styled('div')(({ theme }) => ({
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
}));

const StyledPaper = styled(Paper)((_) => ({
  width: '100%',
  paddingRight: 32
}));

const StyledDivider = styled(Divider)((_) => ({
  height: 60,
  margin: 12
}));

const Timeline: FC<TimelineProps> = (props: TimelineProps) => {
  const { shown, timepoints, onChange } = props;
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
      <OuterDiv>
        <StyledPaper elevation={5} square={false}>
          <Grid
            container
            direction='row'
            justifyContent='flex-start'
            alignItems='center'
          >
            <PlayButton onClick={start} isplaying={running} />
            <StyledDivider orientation='vertical' />
            <div style={{ flex: 1, marginLeft: 2 }}>
              <TimepointLabel timepoint={timepoints[currentIndex]} />
              <Slider
                max={timepoints.length - 1}
                value={currentIndex}
                onChange={updateTime(onChange)}
              />
            </div>
          </Grid>
        </StyledPaper>
      </OuterDiv>
    </Slide>
  );
};

const StyledIconButton = styled(IconButton)(({ theme }) => ({
  colorPrimary: {
    color:
      theme.palette.mode == 'dark' ? common.white : theme.palette.primary.main
  },
  fontSize: '60px'
}));

type PlayButtonProps = {
  isplaying: boolean;
  onClick: (play: boolean) => void;
};

const PlayButton: FC<PlayButtonProps> = (props: PlayButtonProps) => {
  const { onClick: callback, isplaying } = props;

  const onClick = () => callback(!isplaying);

  return (
    <StyledIconButton color='primary' onClick={onClick} edge='end' size='large'>
      {isplaying ? (
        <PauseCircleOutline style={{ fontSize: '60px' }} />
      ) : (
        <PlayCircleOutline style={{ fontSize: '60px' }} />
      )}
    </StyledIconButton>
  );
};

export default Timeline;
