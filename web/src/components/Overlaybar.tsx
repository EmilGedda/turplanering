import React, { FC, useState } from 'react'
import { WeatherPouring, Thermometer, WeatherWindy } from 'mdi-material-ui'
import {
    Slide,
    Paper,
    Grid,
    IconButton,
    IconButtonProps,
} from '@material-ui/core'
import { makeStyles } from '@material-ui/core/styles'

type ToggleLayerCallback = (shown: boolean) => void

type ToggleButtonProps = LayerToggleProps &
    Omit<IconButtonProps, 'onClick' | 'disabled'>

const ToggleButton: FC<ToggleButtonProps> = (props: ToggleButtonProps) => {
    const [active, setIsActive] = useState(false)

    const color = (() => {
        if (props.disabled) return 'secondary'
        if (active) return 'primary'
        return props.color ? props.color : 'default'
    })()

    const onClick = () => {
        if (props.disabled) return
        if (props.onClick) props.onClick(!active)
        setIsActive(!active)
    }

    return (
        <IconButton color={color} onClick={onClick}>
            {props.children}
        </IconButton>
    )
}

type LayerToggleProps = {
    onClick?: ToggleLayerCallback
    disabled?: boolean
}

type Props = {
    shown: boolean
    precipitation?: LayerToggleProps
    wind?: LayerToggleProps
    temperature?: LayerToggleProps
}

const Overlaybar: FC<Props> = (props: Props) => {
    const { precipitation, wind, temperature } = props
    const css = overlaybarCSS()

    return (
        <Slide direction="left" in={props.shown}>
            <div className={css.outer}>
                <Paper elevation={5} square={false}>
                    <Grid
                        container
                        direction="column"
                        justify="space-around"
                        alignItems="flex-end"
                    >
                        <ToggleButton {...precipitation}>
                            <WeatherPouring />
                        </ToggleButton>
                        <ToggleButton {...wind}>
                            <WeatherWindy />
                        </ToggleButton>
                        <ToggleButton {...temperature}>
                            <Thermometer />
                        </ToggleButton>
                    </Grid>
                </Paper>
            </div>
        </Slide>
    )
}

const overlaybarCSS = makeStyles((theme) => ({
    outer: {
        position: 'fixed',
        top: 0,
        bottom: 0,
        right: 0,
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        [theme.breakpoints.down('sm')]: {
            marginRight: 10,
        },
        [theme.breakpoints.up('sm')]: {
            marginRight: 25,
        },
        zIndex: 1000,
    },
}))

export default Overlaybar
