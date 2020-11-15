import React, { useState, FC } from 'react';
import { Layers, LayersOutlined } from '@material-ui/icons';
import { SpeedDial, SpeedDialAction } from '@material-ui/lab';
import { makeStyles, emphasize } from '@material-ui/core/styles';

const selectorCSS = makeStyles((theme) => ({
  root: {
    height: 380,
    flexGrow: 1,
    zIndex: 1000,
  },
  speedDial: {
    zIndex: 1000,
    position: 'absolute',
    top: theme.spacing(2),
    right: theme.spacing(2),
  },
  icon: {
    color: theme.palette.action.active,
  },
  fab: {
    'borderRadius': 4,
    'width': '48px',
    'height': '48px',
    'zIndex': 1000,
    'backgroundColor': theme.palette.background.paper,
    'transition': theme.transitions.create('background-color', {
      duration: theme.transitions.duration.shortest,
    }),
    '&:hover': {
      backgroundColor: emphasize(theme.palette.background.paper),
    },
    '&$disabled': {
      backgroundColor: 'transparent',
      color: theme.palette.action.disabled,
    },
  },
}));

type IconProps = { className: string };

const LayerIcon: FC<IconProps> = ({ className }) => {
  return <Layers className={className} />;
};

const LayerSelectorButton: FC = () => {
  const actions = [
    { icon: <LayersOutlined />, name: 'Lantmäteriet skuggad' },
    { icon: <LayersOutlined />, name: 'Lantmäteriet' },
    { icon: <LayersOutlined />, name: 'OpenStreetMap' },
  ];

  const css = selectorCSS();
  const [open, setOpen] = useState(false);

  const handleOpen = () => {
    setOpen(true);
  };

  const handleClose = () => {
    setOpen(false);
  };

  return (
    <div className={css.root}>
      <SpeedDial
        ariaLabel='Layer selector button'
        className={css.speedDial}
        icon={<LayerIcon className={css.icon} />}
        onClose={handleClose}
        onOpen={handleOpen}
        open={open}
        direction='down'
        FabProps={{ className: css.fab }}
      >
        {actions.map((action) => (
          <SpeedDialAction
            key={action.name}
            icon={action.icon}
            tooltipTitle={action.name}
            onClick={handleClose}
            tooltipOpen={true}
            // TODO: MUI 5.0: maxWidth: none
          />
        ))}
      </SpeedDial>
    </div>
  );
};

export default LayerSelectorButton;
