import React, { FC, useState } from 'react';
import { Layers } from '@material-ui/icons';
import {
  Fab,
  Grow,
  ClickAwayListener,
  Paper,
  Typography,
  Grid
} from '@material-ui/core';
import type { TypographyProps } from '@material-ui/core';
import { makeStyles, emphasize } from '@material-ui/core/styles';

const selectorCSS = makeStyles((theme) => ({
  container: {
    position: 'static',
    pointerEvents: 'auto'
  },
  icon: {
    color: theme.palette.action.active,
    width: '26px',
    height: '26px'
  },
  button: {
    right: 0,
    position: 'relative',
    [theme.breakpoints.down('sm')]: {
      marginLeft: 10
    },
    [theme.breakpoints.up('sm')]: {
      marginLeft: 25
    },
    width: '48px',
    height: '48px',
    zIndex: 1001,
    borderRadius: 4,
    backgroundColor: theme.palette.background.paper,
    transition: theme.transitions.create('background-color', {
      duration: theme.transitions.duration.shortest
    }),
    '&:hover': {
      backgroundColor: emphasize(theme.palette.background.paper)
    }
  },
  selector: {
    zIndex: 1002,
    position: 'absolute',
    padding: '8px 20px',
    right: 0
  },
  sectionTitle: {
    color: 'grey',
    fontWeight: 'bold',
    fontSize: theme.typography.pxToRem(11)
  },
  layer: {
    position: 'relative',
    width: '64px',
    height: '64px'
  }
}));

const SectionTitle: FC<TypographyProps> = (props) => {
  const css = selectorCSS();

  return (
    <>
      <Typography variant='overline' className={css.sectionTitle}>
        {props.children}
      </Typography>
      <br />
    </>
  );
};

const LayerSelector: FC = () => {
  const css = selectorCSS();
  const [shown, setShown] = useState(false);
  const [hover, setHover] = useState(false);

  return (
    <ClickAwayListener
      onClickAway={() => setShown(false)}
      mouseEvent='onMouseDown'
    >
      <div className={css.container}>
        <Grow in={hover || shown} style={{ transformOrigin: 'top right' }}>
          <Paper
            elevation={4}
            className={css.selector}
            onMouseLeave={() => setHover(false)}
          >
            <SectionTitle>Karta</SectionTitle>
            <Grid
              container
              spacing={3}
              justify='flex-start'
              alignItems='center'
              style={{ marginBottom: '4px' }}
            >
              <Grid item>
                <Paper className={css.layer} />
              </Grid>
              <Grid item>
                <Paper className={css.layer} />
              </Grid>
              <Grid item>
                <Paper className={css.layer} />
              </Grid>
            </Grid>
            <SectionTitle>Variant</SectionTitle>
            <Grid
              container
              spacing={3}
              justify='flex-start'
              alignItems='center'
              style={{ marginBottom: '4px' }}
            >
              <Grid item>
                <Paper className={css.layer} />
              </Grid>
              <Grid item>
                <Paper className={css.layer} />
              </Grid>
              <Grid item>
                <Paper className={css.layer} />
              </Grid>
            </Grid>
            <SectionTitle>Väder</SectionTitle>
            <SectionTitle>Detaljer</SectionTitle>
          </Paper>
        </Grow>
        <Fab
          className={css.button}
          onClick={() => setShown(true)}
          onMouseEnter={() => setHover(true)}
        >
          <Layers className={css.icon} />
        </Fab>
      </div>
    </ClickAwayListener>
  );
};

export default LayerSelector;
