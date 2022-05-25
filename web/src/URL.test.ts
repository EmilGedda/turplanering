import {
  AttributionsURL,
  CoordURL,
  currentURLState,
  OverlayState,
  parseQueryArgs,
  updateURL,
  TrailURL,
  URLState
} from './URL';

describe('TrailURL', () => {
  it('toURL()', () => {
    const url = new TrailURL('footrail', 'sectionbar').toURL();
    expect(url).toContain('/trail/');
    expect(url).toContain('footrail');
    expect(url).toContain('sectionbar');
  });
});

describe('CoordURL', () => {
  it('toURL()', () => {
    var url = new CoordURL(12.3, 45.6, 7.89).toURL();
    expect(url).toContain('12.30');
    expect(url).toContain('45.60');
    expect(url).toContain('7.9');

    var url = new CoordURL(11.1, 22.2).toURL();
    expect(url).not.toContain(':');
  });
});

describe('AttributionsURL', () => {
  it('toURL()', () => {
    expect(new AttributionsURL().toURL()).toEqual('/attributions');
  });
});

describe('URLState', () => {
  it('toURL()', () => {
    const attribution = new AttributionsURL();
    const overlays = new OverlayState({
      temperature: true,
      weather: true
    });

    const urlState = new URLState(attribution, overlays);
    expect(urlState.toURL()).toContain(attribution.toURL());
    expect(urlState.toURL()).toContain(overlays.toURL());

    expect(new URLState(undefined, new OverlayState()).toURL()).toEqual('/');
  });
});

describe('OverlayState', () => {
  it('empty initialization', () => {
    const { toURL, ...os } = new OverlayState();

    expect(os).toEqual({
      temperature: false,
      weather: false
    });

    expect(toURL()).toEqual('');
  });

  it('partial initialization', () => {
    const { toURL, ...os } = new OverlayState({
      temperature: true,
      weather: true,
      when: 'date'
    });

    expect(os).toEqual({
      temperature: true,
      weather: true,
      when: 'date'
    });

    expect(toURL()).toContain('temp');
    expect(toURL()).toContain('weather');
    expect(toURL()).toContain('when');
    expect(toURL()).toContain('date');
  });

  it('query args initialization', () => {
    const args = new Map<string, string>([
      ['overlays', 'temp,weather'],
      ['when', 'date']
    ]);

    var { toURL, ...os } = OverlayState.fromQueryArgs(args);

    expect(os).toEqual({
      temperature: true,
      weather: true,
      when: 'date'
    });

    expect(toURL()).toContain('temp');
    expect(toURL()).toContain('weather');
    expect(toURL()).toContain('when');
    expect(toURL()).toContain('date');

    var { toURL, ...os } = OverlayState.fromQueryArgs(new Map());
    expect(os).toEqual({
      temperature: false,
      weather: false
    });
  });
});

describe('parseQueryArgs', () => {
  it('forms an isomorphism with OverlayState', () => {
    const args1 = new Map<string, string>([
      ['overlays', 'temp,weather'],
      ['when', 'date']
    ]);

    const state1 = OverlayState.fromQueryArgs(args1);
    const args2 = parseQueryArgs(state1.toURL());
    expect(args2).toEqual(args1);
    expect(OverlayState.fromQueryArgs(args2).toString()).toEqual(
      state1.toString()
    );
  });
});

describe('currentURLState', () => {
  const overlays = new OverlayState({
    temperature: true,
    weather: true,
    when: 'timestamp'
  });

  const urlState = (path: string) => {
    const url = new URL(path, 'http://localhost');
    return currentURLState(url.pathname.slice(1).split('/'), overlays.toURL());
  };

  it('from correct CoordURL', () => {
    const coord = new CoordURL(12.3, 45.6, 7.8);
    const url = urlState(coord.toURL());
    expect(url.toURL()).toEqual(new URLState(coord, overlays).toURL());
    expect(JSON.stringify(url.state)).toEqual(JSON.stringify(coord));
  });

  it('from invalid CoordURL', () => {
    var url = urlState('/coord');
    expect(url.toURL()).toEqual(new URLState(undefined, overlays).toURL());
    expect(url.state).toBeUndefined();

    var url = urlState('/coord/12');
    expect(url.toURL()).toEqual(new URLState(undefined, overlays).toURL());
    expect(url.state).toBeUndefined();

    var url = urlState('/coord/1,2:foo');
    expect(url.state).toMatchObject({
      lat: 1,
      lon: 2,
      zoom: undefined
    });
  });

  it('from TrailURL', () => {
    var trail = new TrailURL('fooo', 'bar');
    var url = urlState(trail.toURL());
    expect(url.toURL()).toEqual(new URLState(trail, overlays).toURL());
    expect(url.state).toMatchObject({
      trail: 'fooo',
      section: 'bar'
    });

    var trail = new TrailURL('traaail');
    var url = urlState(trail.toURL());
    expect(url.toURL()).toEqual(new URLState(trail, overlays).toURL());
    expect(url.state).toMatchObject({
      trail: 'traaail',
      section: undefined
    });
  });

  it('from AttributionsURL', () => {
    const attributions = new AttributionsURL();
    const url = urlState(attributions.toURL());
    expect(url.toURL()).toEqual(new URLState(attributions, overlays).toURL());
    expect(JSON.stringify(url.state)).toEqual(JSON.stringify(attributions));
  });
});

describe('updateURL', () => {
  const oldHistory = { ...window.history };
  Object.assign(window.history, {
    replaceState: jest.fn(),
    pushState: jest.fn()
  });

  // current url is: '/' == undefined
  it('pushes new state', () => {
    // @ts-ignore
    jsdom.reconfigure({
      url: `${window.location.protocol}//${window.location.host}/`
    });
    const attr = new AttributionsURL();
    updateURL('', attr);
    expect(window.history.pushState).toHaveBeenCalledWith({}, '', attr.toURL());
    expect(window.history.replaceState).toHaveBeenCalledTimes(0);
  });

  it('replaces state', () => {
    const coord = new CoordURL(12.3, 45.6, 7.8);
    // @ts-ignore
    jsdom.reconfigure({
      url: `${window.location.protocol}//${
        window.location.host
      }${coord.toURL()}`
    });
    updateURL('', coord);
    expect(window.history.replaceState).toHaveBeenCalledWith(
      {},
      '',
      coord.toURL()
    );
    expect(window.history.pushState).toHaveBeenCalledTimes(1);
  });

  Object.assign(window.history, oldHistory);
});
