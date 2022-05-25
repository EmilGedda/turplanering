import React, { FC, useState } from 'react';
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
  IconButton
} from '@mui/material';

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

const StyledDiv = styled('div')({
  position: 'static',
  pointerEvents: 'auto'
});

export const SectionTitle: FC<TypographyProps> = (props) => {
  const { children, ...rest } = props;
  return (
    <StyledTypography variant='overline' {...rest}>
      {children}
    </StyledTypography>
  );
};

type Props = {
  children: React.ReactNode
};

const LayerSelector: React.FC<Props> = ({children}: Props): JSX.Element => {
  const [shown, setShown] = useState(false);
  const [pinned, setPinned] = useState(false);

  return (
    <ClickAwayListener
      onClickAway={() => setShown(false)}
      mouseEvent='onMouseDown'
      touchEvent='onTouchStart'
    >
      <StyledDiv>
        <Grow in={shown || pinned} style={{ transformOrigin: 'top right' }}>
          <SelectorBox
            elevation={4}
            onMouseLeave={() => setShown(false)}
            onMouseEnter={() => setShown(true)}
            data-testid='SelectorBox'
          >
            <IconButton
              size='small'
              color={pinned ? 'primary' : 'default'}
              onClick={() => setPinned(!pinned)}
              data-testid='PinIconButton'
              style={{
                right: 0,
                top: 0,
                position: 'absolute'
              }}
            >
              {pinned ? (
                <Pin style={{ fontSize: '18px' }} />
              ) : (
                <PinOutline style={{ fontSize: '18px' }} />
              )}
            </IconButton>
            {children}
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
