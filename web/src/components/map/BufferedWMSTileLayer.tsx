import { TileLayer, Bounds, point } from 'leaflet'
import { withLeaflet, GridLayer, WMSTileLayerProps } from 'react-leaflet'

type Props = WMSTileLayerProps & {
    bufferRadius?: number
    dim_reftime?: string
    time?: string
}

type WMSLayer = TileLayer.WMS

type WMSParams = {
    dim_reftime?: string
    time?: string
    format?: string
    layers?: string
    request?: string
    service?: string
    styles?: string
    version?: string
    transparent?: boolean
    width?: number
    height?: number
}

class BufferedWMSLayer extends GridLayer<Props, WMSLayer> {
    updateLeafletElement(fromProps: Props, toProps: Props) {
        super.updateLeafletElement(fromProps, toProps)
        if (toProps.url !== fromProps.url) {
            this.leafletElement.setUrl(toProps.url)
        }
        if (
            toProps.dim_reftime !== fromProps.dim_reftime ||
            toProps.time !== fromProps.time
        ) {
            this.leafletElement.setParams(this.getParams(toProps))
        }
    }

    /* eslint-disable */
    createLeafletElement(props: Props): WMSLayer {
        const { bufferRadius, ...params } = props,
              { url, ...options } = this.getOptions(params)
        const getRadius = () => bufferRadius || 2

        return new (class extends TileLayer.WMS {
            _pxBoundsToTileRange(pixels: Bounds): Bounds {
                // @ts-ignore: Hack to wrap undeclared function
                const viewport = super._pxBoundsToTileRange(pixels) as Bounds,
                    padding = Math.max(getRadius(), 0),
                    margin = point(padding, padding)
                return new Bounds(
                    viewport.min!.subtract(margin),
                    viewport.max!.add(margin),
                )
            }
        })(url, options)
    }

    getOptions(props: any): any {
        const keys = [
            "addBaseLayer", "addOverlay", "removeLayer",
            "removeLayerControl", "bufferRadius", "leaflet"
        ]

        for (let key of keys) {
            delete props[key]
        }

        return props
    }

    getParams(props: any): any {
        const keys: Array<keyof WMSParams> = [
            "dim_reftime", "time",
            "format", "layers",
            "request", "service",
            "styles", "version",
            "width", "height",
            "transparent",
        ]

        let params:WMSParams = {}

        for (let key of keys) {
            if (props[key]) {
                params[key] = props[key]
            }
        }

        return params
    }
    /* eslint-enable */
}

export default withLeaflet<Props>(BufferedWMSLayer)
