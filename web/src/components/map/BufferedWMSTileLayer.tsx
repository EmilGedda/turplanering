import { TileLayer, Bounds, point } from 'leaflet'
import { withLeaflet, GridLayer, WMSTileLayerProps } from 'react-leaflet'

type Props = WMSTileLayerProps & {
    bufferRadius?: number
    dim_reftime?: string
    time?: string
}

type WMSLayer = TileLayer.WMS

class BufferedWMSLayer extends GridLayer<Props, WMSLayer> {
    updateLeafletElement(fromProps: Props, toProps: Props) {
        super.updateLeafletElement(fromProps, toProps)
        if (toProps.url !== fromProps.url) {
            this.leafletElement.setUrl(toProps.url)
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
            "addBaseLayer",
            "addOverlay", "removeLayer",
            "removeLayerControl"
        ]

        for (let key of keys) {
            delete props[key]
        }

        return props
    }
    /* eslint-enable */
}

export default withLeaflet<Props>(BufferedWMSLayer)
