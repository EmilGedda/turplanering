import React, { FC } from 'react';
import { Layers } from '@material-ui/icons';
import { Fab } from '@material-ui/core';
import { makeStyles, emphasize } from '@material-ui/core/styles';

const selectorCSS = makeStyles((theme) => ({
  root: {
    height: 380,
    [theme.breakpoints.down('sm')]: {
      marginLeft: 10
    },
    [theme.breakpoints.up('sm')]: {
      marginLeft: 25
    }
  },
  icon: {
    color: theme.palette.action.active
  },
  fab: {
    'borderRadius': 4,
    'width': '48px',
    'height': '48px',
    'zIndex': 1001,
    'backgroundColor': theme.palette.background.paper,
    'transition': theme.transitions.create('background-color', {
      duration: theme.transitions.duration.shortest
    }),
    '&:hover': {
      backgroundColor: emphasize(theme.palette.background.paper)
    }
  }
}));

type IconProps = { className: string };

const LayerIcon: FC<IconProps> = ({ className }) => {
  return <Layers className={className} />;
};

const LayerSelectorButton: FC = () => {
  const css = selectorCSS();

  return (
    <div className={css.root}>
      <Fab className={css.fab}>
        <LayerIcon className={css.icon} />
      </Fab>
    </div>
  );
};

export default LayerSelectorButton;
