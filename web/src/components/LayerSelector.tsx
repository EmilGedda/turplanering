import React, { FC, useState } from 'react';
import { styled } from '@mui/styles';
import { Layers } from '@mui/icons-material';
import {
  Fab,
  Grow,
  ClickAwayListener,
  Paper,
  Typography,
  Grid
} from '@mui/material';
import type { TypographyProps } from '@mui/material';
import { emphasize } from '@mui/material/styles';

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
  fontSize: theme.typography.pxToRem(11)
}));

const SelectorBox = styled(Paper)((_) => ({
  zIndex: 1002,
  position: 'absolute',
  padding: '8px 20px',
  right: 0
}));

const StyledLayers = styled(Paper)((_) => ({
  position: 'relative',
  width: '64px',
  height: '64px'
}));

const StyledDiv = styled('div')((_) => ({
  position: 'static',
  pointerEvents: 'auto'
}));

const SectionTitle: FC<TypographyProps> = (props) => {
  return (
    <>
      <StyledTypography variant='overline'>{props.children}</StyledTypography>
      <br />
    </>
  );
};

const LayerSelector: FC = () => {
  const [shown, setShown] = useState(false);
  const [hover, setHover] = useState(false);

  return (
    <ClickAwayListener
      onClickAway={() => setShown(false)}
      mouseEvent='onMouseDown'
    >
      <StyledDiv>
        <Grow in={hover || shown} style={{ transformOrigin: 'top right' }}>
          <SelectorBox elevation={4} onMouseLeave={() => setHover(false)}>
            <SectionTitle>Karta</SectionTitle>
            <Grid
              container
              spacing={3}
              justifyContent='flex-start'
              alignItems='center'
              style={{ marginBottom: '4px' }}
            >
              <Grid item>
                <StyledLayers />
              </Grid>
              <Grid item>
                <StyledLayers />
              </Grid>
              <Grid item>
                <StyledLayers />
              </Grid>
            </Grid>
            <SectionTitle>Variant</SectionTitle>
            <Grid
              container
              spacing={3}
              justifyContent='flex-start'
              alignItems='center'
              style={{ marginBottom: '4px' }}
            >
              <Grid item>
                <StyledLayers />
              </Grid>
              <Grid item>
                <StyledLayers />
              </Grid>
              <Grid item>
                <StyledLayers />
              </Grid>
            </Grid>
            <SectionTitle>Väder</SectionTitle>
            <SectionTitle>Detaljer</SectionTitle>
          </SelectorBox>
        </Grow>
        <StyledFab
          onClick={() => setShown(true)}
          onMouseEnter={() => setHover(true)}
        >
          <StyledIcon />
        </StyledFab>
      </StyledDiv>
    </ClickAwayListener>
  );
};

export default LayerSelector;
