import { rest } from 'msw';
import type { DefaultRequestBody } from 'msw';
import { setupServer } from 'msw/native';

export const mockWTSXMLResponse = (layers: string[]): string => {
  const layerStr: string[] = [];

  for (const layer of layers) {
    layerStr.push(`
      <Layer queryable="0" opaque="0" cascaded="1">
        <Name>${layer}</Name>
        <Title>${layer}</Title>
        <LatLonBoundingBox minx="-4.646" miny="48.833" maxx="37.968" maxy="70.144" />
        <BoundingBox SRS="EPSG:900913" minx="-517211.938776" miny="6246555.474468" maxx="4226612.825958" maxy="11115673.143021" />
        <BoundingBox SRS="EPSG:3021" minx="3341.532695" miny="5615685.377002" maxx="2324067.838945" maxy="7936411.683252" />
        <BoundingBox SRS="EPSG:3006" minx="-937499.768594" miny="5597876.889369" maxx="1352571.208711" maxy="7945323.035330" />
        <BoundingBox SRS="EPSG:4326" minx="-4.646194" miny="48.832900" maxx="37.968309" maxy="70.143775" />
        <Dimension name="time" units="ISO8601" unitSymbol="T" />
        <Dimension name="reftime" units="ISO8601" unitSymbol="T" />
        <Extent name="time">2021-10-11T06:00:00Z/2021-10-14T12:00:00Z/PT6H, 2021-10-15T00:00:00Z/2021-10-18T00:00:00Z/PT12H</Extent>
        <Extent name="reftime">2021-10-08T20:00:00Z</Extent>
      </Layer>`);
  }

  return `<?xml version="1.0" encoding="UTF-8"?>
<WMT_MS_Capabilities version="1.1.1">
  <Service>
    <Name>OGC:WMS</Name>
    <Title />
    <OnlineResource xmlns:xlink="http://www.w3.org/1999/xlink" xlink:href="http://wts.smhi.se/tile/pmp:windspeed-windarrows-avg-10m_n-europe__,pmpfrekvent:wpt-overview_n-europe__,pmpfrekvent:temperature-2m_n-europe_rainbow_?" />
  </Service>
  <Capability>
    <Request>
      <GetCapabilities>
        <Format>application/vnd.ogc.wms_xml</Format>
        <DCPType>
          <HTTP>
            <Get>
              <OnlineResource xmlns:xlink="http://www.w3.org/1999/xlink" xlink:href="http://wts.smhi.se/tile/pmp:windspeed-windarrows-avg-10m_n-europe__,pmpfrekvent:wpt-overview_n-europe__,pmpfrekvent:temperature-2m_n-europe_rainbow_?" />
            </Get>
          </HTTP>
        </DCPType>
      </GetCapabilities>
      <GetMap>
        <Format>image/png</Format>
        <DCPType>
          <HTTP>
            <Get>
              <OnlineResource xmlns:xlink="http://www.w3.org/1999/xlink" xlink:href="http://wts.smhi.se/tile/pmp:windspeed-windarrows-avg-10m_n-europe__,pmpfrekvent:wpt-overview_n-europe__,pmpfrekvent:temperature-2m_n-europe_rainbow_?" />
            </Get>
          </HTTP>
        </DCPType>
      </GetMap>
    </Request>
    <Exception>
      <Format>text/plain</Format>
    </Exception>
    <UserDefinedSymbolization SupportSLD="0" UserLayer="0" UserStyle="0" RemoteWFS="0" />
    <Layer>
      <Title>Picasso Layers</Title>
      <SRS>EPSG:900913</SRS>
      <SRS>EPSG:3021</SRS>
      <SRS>EPSG:3006</SRS>
      <SRS>EPSG:4326</SRS>
${layerStr.join('\n')}
    </Layer>
  </Capability>
</WMT_MS_Capabilities>
`;
};

export const WTSURL = 'https://wts.smhi.se/tile/:layers';

type WTSLayersParams = {
  layers: string;
};

export const handlers = [
  rest.get<DefaultRequestBody, {}, WTSLayersParams>(WTSURL, (req, res, ctx) => {
    const { layers } = req.params;
    return res(ctx.xml(mockWTSXMLResponse(layers.split(','))));
  })
];

export const errHandler = (
  url: string,
  code: number,
  headers?: Record<string, string | string[]>
) => {
  return rest.get(url, (_, res, ctx) => {
    return res.once(ctx.set(headers ?? {}), ctx.status(code));
  });
};
// Setup requests interception using the given handlers.
export const server = setupServer(...handlers);
