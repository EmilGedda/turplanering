import React, {
  createRef,
  FC,
  useState,
  ReactElement,
  cloneElement,
  useEffect,
  useCallback
} from 'react';
import { styled } from '@mui/material/styles';
import { Layers } from '@mui/icons-material';
import { Pin, PinOutline } from '@mitch528/mdi-material-ui';
import type { TypographyProps } from '@mui/material';
import { emphasize } from '@mui/material/styles';
import {
  Fab,
  Grow,
  ClickAwayListener,
  Paper,
  Typography,
  Grid,
  IconButton,
  Switch,
  SwitchProps,
  Divider
} from '@mui/material';
import { orange } from '@mui/material/colors';

const StyledIcon = styled(Layers)(({ theme }) => ({
  color: theme.palette.action.active,
  width: '26px',
  height: '26px'
}));

const StyledFab = styled(Fab)(({ theme }) => ({
  right: 0,
  position: 'relative',
  [theme.breakpoints.down('md')]: {
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
  backgroundImage:
    'linear-gradient(rgba(255, 255, 255, 0.1), rgba(255, 255, 255, 0.1))',
  transition: theme.transitions.create('background-color', {
    duration: theme.transitions.duration.shortest
  }),
  '&:hover': {
    backgroundColor: emphasize(theme.palette.background.paper)
  }
}));

const StyledTypography = styled(Typography)(({ theme }) => ({
  color: 'grey',
  fontWeight: 'bold',
  fontSize: theme.typography.pxToRem(12)
}));

const SelectorBox = styled(Paper)({
  zIndex: 1002,
  position: 'absolute',
  padding: '15px 20px',
  right: 0
});

const BaseLayerButton = styled(Paper, {
  shouldForwardProp: (prop) => prop !== 'selected'
})<{ selected?: boolean }>(({ selected, theme }) => ({
  position: 'relative',
  width: '80px',
  height: '80px',
  marginBottom: '4px',
  ...(selected && {
    boxShadow: `0px 0px 10px 3px ${
      theme.palette.mode === 'dark' ? orange[500] : '#418DE2'
    }`
  })
}));

const StyledDiv = styled('div')({
  position: 'static',
  pointerEvents: 'auto'
});

const BaseLayerName = styled(Typography)({
  textAlign: 'center',
  color: 'grey'
});

const LayerPreview = styled('img')({
  border: '0px',
  objectFit: 'none',
  objectPosition: 'center',
  height: '100%',
  width: '100%',
  borderRadius: 'inherit'
});

const InlineSwitch = styled(Switch)(({ theme }) => ({
  width: 28,
  height: 16,
  padding: 0,
  display: 'flex',
  '&:active': {
    '& .MuiSwitch-thumb': {
      width: 15
    },
    '& .MuiSwitch-switchBase.Mui-checked': {
      transform: 'translateX(9px)'
    }
  },
  '& .MuiSwitch-switchBase': {
    padding: 2,
    '&.Mui-checked': {
      transform: 'translateX(12px)',
      color: '#fff',
      '& + .MuiSwitch-track': {
        opacity: 1,
        backgroundColor: theme.palette.mode === 'dark' ? orange[700] : '#1890ff'
      }
    }
  },
  '& .MuiSwitch-thumb': {
    boxShadow: '0 2px 4px 0 rgb(0 35 11 / 20%)',
    width: 12,
    height: 12,
    borderRadius: 6,
    transition: theme.transitions.create(['width'], {
      duration: 200
    })
  },
  '& .MuiSwitch-track': {
    borderRadius: 16 / 2,
    opacity: 1,
    backgroundColor:
      theme.palette.mode === 'dark'
        ? 'rgba(255,255,255,.35)'
        : 'rgba(0,0,0,.25)',
    boxSizing: 'border-box'
  }
}));

const SectionTitle: FC<TypographyProps> = (props) => {
  const { children, ...rest } = props;
  return (
    <StyledTypography variant='overline' {...rest}>
      {children}
    </StyledTypography>
  );
};

const HillshaderSwitch = (props: SwitchProps): JSX.Element => {
  const ref = createRef<HTMLElement>();

  const labelClick = () => {
    ref.current?.click();
  };

  return (
    <div
      style={{
        display: 'flex',
        justifyContent: 'end'
      }}
    >
      <SectionTitle
        onClick={labelClick}
        style={{
          marginRight: '5px',
          fontWeight: 500,
          userSelect: 'none',
          cursor: 'pointer'
        }}
      >
        Höjdskuggning
      </SectionTitle>
      <div style={{ marginTop: '6px' }}>
        <InlineSwitch inputRef={ref} {...props} />
      </div>
    </div>
  );
};

type LayerProps = {
  name: string;
  previewURL?: string;
  defaultSelected?: boolean;
  selected?: boolean;
  onClick?: () => void;
};

export const Layer = (props: LayerProps): JSX.Element => {
  const { name, selected, onClick, previewURL } = props;
  const ref = createRef<HTMLImageElement>();
  const [image, setImage] = useState<string>();

  useEffect(() => {
    if (ref?.current) ref.current.style.opacity = '0';
  }, [previewURL]);

  return (
    <Grid
      item
      style={{
        paddingTop: '0px',
        marginBottom: '5px'
      }}
    >
      <div
        onClick={onClick}
        style={{
          display: 'flex',
          flexDirection: 'column',
          cursor: 'pointer'
        }}
      >
        <BaseLayerButton selected={selected} elevation={5}>
          <LayerPreview
            src={image}
            style={{
              position: 'relative'
            }}
            onLoad={() => {
              if (ref?.current) ref.current.style.opacity = '0';
            }}
          />
          <LayerPreview
            ref={ref}
            src={previewURL}
            style={{
              position: 'absolute',
              left: '0px',
              opacity: 0,
              transition: 'opacity 0.3s ease-in-out'
            }}
            onLoad={(e) => {
              e.currentTarget.style.opacity = '1';
            }}
            onTransitionEnd={(e) => {
              if (e.currentTarget.style.opacity != '1' || !previewURL) return;
              setImage(previewURL);
            }}
          />
        </BaseLayerButton>
        <BaseLayerName variant='body1'>{name}</BaseLayerName>
      </div>
    </Grid>
  );
};

type BaseLayersProps = {
  children: ReactElement<LayerProps> | ReactElement<LayerProps>[];
};

export const BaseLayers = (props: BaseLayersProps): JSX.Element => {
  const { children } = props;
  const [selectedLayer, setSelectedLayer] = useState<string>(() => {
    if (Array.isArray(children)) {
      for (const layer of children) {
        if (layer.props.defaultSelected) return layer.props.name;
      }
      return children[0].props.name;
    }
    return children.props.name;
  });

  const baseLayers = React.Children.map(children, (layer) => {
    return cloneElement(layer, {
      selected: selectedLayer == layer.props.name,
      onClick: () => {
        const { onClick } = layer.props;
        setSelectedLayer(layer.props.name);
        if (onClick && selectedLayer != layer.props.name) onClick();
      }
    });
  });

  return (
    <Grid
      container
      spacing={3}
      justifyContent='flex-start'
      alignItems='center'
      style={{ marginBottom: '4px', marginTop: '5px' }}
    >
      {baseLayers}
    </Grid>
  );
};

type LayerSelectorProps = {
  children: ReactElement<BaseLayersProps> | ReactElement<BaseLayersProps>[];
  disableHillshading?: boolean;
  onHillshadingChange: (checked: boolean) => void;
};

const LayerSelector = (props: LayerSelectorProps): JSX.Element => {
  const { children, onHillshadingChange, disableHillshading } = props;
  const [hillshading, setHillshading] = useState(false);
  const [shown, setShown] = useState(false);
  const [pinned, setPinned] = useState(false);

  const hillshadingCallback = useCallback(
    (_, checked) => {
      onHillshadingChange(checked);
      setHillshading(checked);
    },
    [onHillshadingChange]
  );

  useEffect(() => {
    if (disableHillshading) {
      onHillshadingChange(false);
    } else {
      onHillshadingChange(hillshading);
    }
  }, [disableHillshading, onHillshadingChange]);

  return (
    <ClickAwayListener
      onClickAway={() => setShown(false)}
      mouseEvent='onMouseDown'
    >
      <StyledDiv>
        <Grow in={shown || pinned} style={{ transformOrigin: 'top right' }}>
          <SelectorBox
            elevation={4}
            onMouseLeave={() => setShown(false)}
            onMouseEnter={() => setShown(true)}
          >
            <SectionTitle>Karta</SectionTitle>
            <IconButton
              size='small'
              color={pinned ? 'primary' : 'default'}
              style={{
                right: 0,
                top: 0,
                position: 'absolute'
              }}
              onClick={() => setPinned(!pinned)}
            >
              {pinned ? (
                <Pin style={{ fontSize: '18px' }} />
              ) : (
                <PinOutline style={{ fontSize: '18px' }} />
              )}
            </IconButton>
            {children}
            <HillshaderSwitch
              onChange={hillshadingCallback}
              disabled={disableHillshading}
              checked={hillshading && !disableHillshading}
            />
            <Divider
              variant='middle'
              style={{
                margin: '5px 0 10px 0'
              }}
            />
            <SectionTitle>Väder</SectionTitle>
            <br />
            <SectionTitle>Detaljer</SectionTitle>
            <br />
          </SelectorBox>
        </Grow>
        <StyledFab
          onClick={() => setShown(true)}
          onMouseEnter={() => setShown(true)}
        >
          <StyledIcon />
        </StyledFab>
      </StyledDiv>
    </ClickAwayListener>
  );
};

export default LayerSelector;
