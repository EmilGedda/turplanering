import React, { FC, useState } from 'react';
import { Layers } from '@material-ui/icons';
import { Fab, Grow, ClickAwayListener, Paper } from '@material-ui/core';
import { makeStyles, emphasize } from '@material-ui/core/styles';

const selectorCSS = makeStyles((theme) => ({
  container: {
    display: 'flex',
    position: 'relative'
  },
  icon: {
    color: theme.palette.action.active
  },
  button: {
    right: 0,
    position: 'absolute',
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
    zIndex: 1002
  },
  svg: {
    width: 100,
    height: 100,
    float: 'left'
  },
  polygon: {
    fillOpacity: 0,
    stroke: theme.palette.divider,
    strokeWidth: 1
  }
}));

const LayerSelector: FC = () => {
  const css = selectorCSS();
  const [shown, setShown] = useState(false);
  // const [hover, setHover] = useState(false);

  return (
    <ClickAwayListener
      onClickAway={() => setShown(false)}
      mouseEvent='onMouseDown'
    >
      <div className={css.container}>
        <Grow
          in={/* hover || */ shown}
          style={{ transformOrigin: 'top right' }}
        >
          <Paper
            elevation={4}
            className={css.selector}
            // onMouseLeave={() => setHover(false)}
          >
            <svg className={css.svg}>
              <polygon points='0,100 50,00, 100,100' className={css.polygon} />
              <polygon points='0,0 50,100, 100,0' className={css.polygon} />
              <polygon points='0,0 100,50, 0,100' className={css.polygon} />
              <polygon points='100,0 0,50, 100,100' className={css.polygon} />
            </svg>
          </Paper>
        </Grow>
        <Fab
          className={css.button}
          onClick={() => setShown(true)}
          //  onMouseEnter={() => setHover(true)}
        >
          <Layers className={css.icon} />
        </Fab>
      </div>
    </ClickAwayListener>
  );
};

export default LayerSelector;
