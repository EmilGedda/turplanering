import { TileLayer, Bounds, point } from 'leaflet'
import { withLeaflet, GridLayer, GridLayerProps } from 'react-leaflet'

type Props = { url: string; bufferRadius?: number } & GridLayerProps

class BufferedTileLayer extends GridLayer<Props, TileLayer> {
    updateLeafletElement(fromProps: Props, toProps: Props) {
        super.updateLeafletElement(fromProps, toProps)
        if (toProps.url !== fromProps.url) {
            this.leafletElement.setUrl(toProps.url)
        }
    }

    createLeafletElement(props: Props): TileLayer {
        const getRadius = () => this.props.bufferRadius || 2
        return new (class extends TileLayer {
            _pxBoundsToTileRange(pixels: Bounds): Bounds {
                /* eslint-disable */
                // @ts-ignore: Hack to wrap undeclared function
                const viewport = super._pxBoundsToTileRange(pixels) as Bounds,
                    padding = Math.max(getRadius(), 0),
                    margin = point(padding, padding)
                return new Bounds(
                    viewport.min!.subtract(margin),
                    viewport.max!.add(margin),
                )
                /* eslint-enable */
            }
        })(props.url, this.getOptions(props))
    }
}

export default withLeaflet<Props>(BufferedTileLayer)
