/**
 * URL schema:
 * /coord/LAT,LON[,ZOOM]
 * /trail/TRAIL[-SECTION]
 * /attributions
 *
 * Additional: ?overlays=temp,weather&when=TIMESTAMP
 */

enum URLStateType {
  Trail,
  Coord,
  Attribution
}

interface URLType {
  type: URLStateType
}

interface URLSegment {
  toURL(): string
}

type URLPart = URLSegment & URLType;

export class TrailURL implements URLPart {
  type = URLStateType.Trail

  readonly trail: string
  readonly section?: string

  public constructor(trail: string, section?: string) {
    this.trail = trail;
    this.section = section;
  }

  readonly toURL = () => {
    return '/trail/' +
      !this.section
        ? this.trail
        : `${this.trail}:${this.section}`;
  }
}

export class CoordURL implements URLPart {
  type = URLStateType.Coord
  readonly lat: number
  readonly lon: number
  readonly zoom?: number

  public constructor(lat: number, lon: number, zoom?: number) {
    this.lat = lat;
    this.lon = lon;
    this.zoom = zoom;
  }

  readonly toURL = () => {
    return `/coord/${this.lat.toFixed(4)},${this.lon.toFixed(4)}`
      + (this.zoom ? `:${this.zoom.toFixed(1)}` : '');
  }
}

export class AttributionsURL implements URLPart {
  type = URLStateType.Coord
  readonly toURL = () => {
    return "/attributions";
  }
}

export class OverlayState implements URLSegment {
  readonly temperature: boolean = false
  readonly weather: boolean = false
  readonly when?: string

  public constructor(queryArgs: Map<string, string>) {
      const layers = queryArgs.get("overlays")
      if (layers) {
          const l = layers.split(',');
          this.temperature = l.includes("temp");
          this.weather = l.includes("weather");
          this.when = queryArgs.get("when");
      }
  }

  readonly toURL = () => {
    if (!this.temperature && !this.weather) {
      return '';
    }

    let url = '?overlays=';
    let layers = []

    this.temperature && layers.push("temp");
    this.weather     && layers.push("weather");

    url += layers.join(',');

    if (this.when) {
      url += `&when=${this.when}`
    }

    return url;
  }
}

export class URLState implements URLSegment {
  readonly state?: URLPart
  readonly overlays: OverlayState

  public constructor(state: URLPart | undefined, overlays: OverlayState) {
    this.state = state;
    this.overlays = overlays;
  }

  readonly toURL = () => {
    if (!this.state) {
      return "/";
    }
    return `${this.state.toURL()}${this.overlays.toURL()}`
  }
}


export const parseQueryArgs = (str: string): Map<string, string> => {
  if (str.startsWith("?")) {
    str = str.slice(1);
  }

  let map = new Map<string, string>();
  const args = str.split('&');

  for (const arg of args) {
    const equals = arg.indexOf('=');
    map.set(
      arg.slice(0, equals),
      decodeURIComponent(arg.slice(equals + 1))
    );
  }

  return map;
}

// TODO: history pop current state?
export const currentURLState = ( path: string[] = window.location.pathname.slice(1).split("/")
                               , queryArgs: string = window.location.search): URLState => {
  const overlays = new OverlayState(parseQueryArgs(queryArgs));
  let state: URLPart | undefined = undefined;

  switch(path[0]) {
    case "coord": {
      if (path.length < 2) {
        break;
      }

      const data = path[1].split(/[,:]/);
      if (data.length < 2) {
        break;
      }

      const zoom = parseFloat(data[2]);

      state = new CoordURL(
        parseFloat(data[0]),
        parseFloat(data[1]),
        isNaN(zoom) ? undefined : zoom
      );

      break;
    }

    case "trail": {
      const data = path[1].split(":");
      if (data.length < 1) {
        break;
      }

      state = new TrailURL(data[0], data[1]);
      break;
    }

    case "attributions": {
      state = new AttributionsURL();
      break;
    }
  }

  return new URLState(state, overlays)
}

export const updateURL = (givenState: URLPart):void => {
  const {state, overlays} = currentURLState();
  const updateState
    = state && state.type == givenState.type
                ? window.history.replaceState
                : window.history.pushState;

  const newState = new URLState(givenState, overlays)
  updateState.bind(window.history)({}, "", newState.toURL());
}

