import React, { FC, useState } from 'react'
import {
    Divider,
    Grid,
    IconButton,
    IconButtonProps,
    Paper,
    Slide,
    Slider,
    Typography,
} from '@material-ui/core'
import { PlayCircleOutline, PauseCircleOutline } from '@material-ui/icons'
import { makeStyles } from '@material-ui/core/styles'

type Props = {
    shown: boolean
    timepoints: Date[]
    onChange: (timepoint: Date) => void
}

type SliderCallback = (
    event: React.ChangeEvent<unknown>,
    value: number | number[]
) => void

const Timeline: FC<Props> = (props: Props) => {
    const { shown, timepoints, onChange } = props
    const css = timelineCSS()
    const [currentIdx, setCurrentIdx] = useState(0)
    const [watchID, setWatchID] = useState(-1)

    const updateTime = (f: (timepoint: Date) => void): SliderCallback => {
        return (_, value) => {
            const v = value as number
            if (value == currentIdx) return
            if (watchID != -1) {
                clearTimeout(watchID)
            }

            if (v >= 0 && v < timepoints.length) setCurrentIdx(v)
            setWatchID(
                window.setTimeout(() => {
                    f(timepoints[v])
                    setWatchID(-1)
                }, 300)
            )
        }
    }
    console.log(timepoints)

    return (
        <Slide direction="up" in={shown} mountOnEnter>
            <div className={css.outer}>
                <Paper elevation={5} square={false} className={css.bar}>
                    <Grid
                        container
                        direction="row"
                        justify="flex-start"
                        alignItems="center"
                    >
                        <PlayButton />
                        <Divider
                            className={css.divider}
                            orientation="vertical"
                        />
                        <div style={{ flex: 1, marginLeft: 2 }}>
                            <Typography>
                                {timepoints[currentIdx].toString()}
                            </Typography>
                            <Slider
                                max={timepoints.length - 1}
                                onChange={updateTime(onChange)}
                            />
                        </div>
                    </Grid>
                </Paper>
            </div>
        </Slide>
    )
}

type PlayButtonProps = { onClick?: (play: boolean) => void } & IconButtonProps
const PlayButton: FC<PlayButtonProps> = (props: PlayButtonProps) => {
    const [active, setIsActive] = useState(false)
    const { onClick: callback, ...baseProps } = props
    const css = timelineCSS()

    const onClick = () => {
        if (callback) callback(!active)
        setIsActive(!active)
    }

    return (
        <IconButton
            color="primary"
            onClick={onClick}
            className={css.icon}
            edge="end"
            {...baseProps}
        >
            {active ? (
                <PauseCircleOutline className={css.icon} />
            ) : (
                <PlayCircleOutline className={css.icon} />
            )}
        </IconButton>
    )
}

const timelineCSS = makeStyles((theme) => ({
    outer: {
        position: 'fixed',
        left: 0,
        right: 0,
        bottom: 0,
        padding: '2px 4px',
        maxWidth: '95%',
        margin: '0 auto',
        [theme.breakpoints.down('sm')]: {
            marginBottom: '5px',
        },
        [theme.breakpoints.up('sm')]: {
            marginBottom: '15px',
        },
        width: 600,
        zIndex: 1001,
    },
    bar: {
        width: '100%',
        paddingRight: 32,
    },
    center: {
        width: '100%',
        left: 0,
        right: 0,
    },
    divider: {
        height: 60,
        margin: 12,
    },
    icon: {
        fontSize: 60,
    },
}))

export default Timeline
