import { TileLayer as LeafletTileLayer, Bounds } from 'leaflet';
import { withLeaflet, GridLayer, GridLayerProps } from 'react-leaflet';

type LeafletElement = LeafletTileLayer;
type Props = { url: string; bufferRadius?: number } & GridLayerProps;

class BufferedTileLayer extends GridLayer<Props, LeafletElement> {
    createLeafletElement(props: Props): LeafletElement {
        return new (class extends LeafletTileLayer {
            _pxBoundsToTileRange(bounds) {
                const bounds = super._pxBoundsToTileRange(bounds),
                    margin = props.bufferRadius || 2,
                    padding = [margin, margin];
                return new Bounds(
                    bounds.min.subtract(padding),
                    bounds.max.add(padding),
                );
            }
        })(props.url, this.getOptions(props));
    }
}

export default withLeaflet(BufferedTileLayer);
