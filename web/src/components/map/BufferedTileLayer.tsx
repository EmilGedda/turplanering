//import { TileLayer, Bounds, point } from 'leaflet'
//import { withLeaflet, GridLayer, GridLayerProps } from 'react-leaflet'
//
//type Props = { url: string; bufferRadius?: number } & GridLayerProps
//
//class BufferedTileLayer extends GridLayer<Props, TileLayer> {
//    createLeafletElement(props: Props): TileLayer {
//        return new (class extends TileLayer {
//            _pxBoundsToTileRange(pixels: Bounds): Bounds {
//                // @ts-ignore: Hack to wrap undeclared function
//                const viewport = super._pxBoundsToTileRange(pixels) as Bounds,
//                    padding = props.bufferRadius || 2,
//                    margin = point(padding, padding)
//                return new Bounds(
//                    viewport.min.subtract(margin),
//                    viewport.max.add(margin),
//                )
//            }
//        })(props.url, this.getOptions(props))
//    }
//}
//
//export default withLeaflet(BufferedTileLayer)
//export type { Props as BufferedTileLayerProps }
