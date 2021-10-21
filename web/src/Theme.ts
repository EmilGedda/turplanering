import { alpha, createTheme, Theme } from '@mui/material/styles';
import { orange, common, grey } from '@mui/material/colors';

declare module '@mui/material/styles' {
  // eslint-disable-next-line
  interface DefaultTheme extends Theme{}
}

export const theme = {
  light: createTheme({
    palette: {
      mode: 'light'
    }
  }),
  dark: createTheme({
    palette: {
      mode: 'dark'
    },
    components: {
      MuiSlider: {
        styleOverrides: {
          // drag color and opacity
          track: { backgroundColor: grey[500] },
          rail: { backgroundColor: grey[600] },
          thumb: {
            backgroundColor: 'white',
            '&.Mui-focusVisible,&:hover': {
              boxShadow: `0px 0px 0px 8px ${alpha(common.white, 0.16)}`,
              '@media (hover: none)': {
                boxShadow: 'none'
              }
            },
            '&.Mui-active': {
              boxShadow: `0px 0px 0px 14px ${alpha(common.white, 0.16)}`
            }
          }
        }
      },
      MuiIconButton: {
        styleOverrides: {
          colorPrimary: {
            color: orange[500]
          }
        }
      }
    }
  })
};
