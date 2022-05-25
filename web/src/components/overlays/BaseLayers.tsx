import React, {
  cloneElement,
  createRef,
  ReactElement,
  useCallback,
  useEffect,
  useState
} from 'react';
import {
  Divider,
  Grid,
  Paper,
  styled,
  Switch,
  SwitchProps,
  Typography
} from '@mui/material';
import { orange } from '@mui/material/colors';
import { SectionTitle } from './LayerSelector';

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
  disableHillshading?: boolean;
  onHillshadingChange: (checked: boolean) => void;
};

export const BaseLayers = (props: BaseLayersProps): JSX.Element => {
  const { children, onHillshadingChange, disableHillshading } = props;
  const [hillshading, setHillshading] = useState(false);

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

  const hillshadingCallback = useCallback(
     (_:any, checked: boolean) => {
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
    <>
      <SectionTitle>Karta</SectionTitle>
      <Grid
        container
        spacing={3}
        justifyContent='flex-start'
        alignItems='center'
        style={{ marginBottom: '4px', marginTop: '5px' }}
      >
        {baseLayers}
      </Grid>
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
    </>
  );
};
