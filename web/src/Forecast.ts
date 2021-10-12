export type ForecastTimestamps = {
  reference: Date;
  validTimes: Date[];
};

export type ForecastAPI = {
  ValidTimes(layers: string[]): Promise<Record<string, ForecastTimestamps>>;
};

type timeType = {
  time: string;
  length: number;
};

export const parsePeriodicity = (str: string): timeType[] => {
  const times: timeType[] = [];
  let num = 0;
  for (const c of str) {
    const digit = parseFloat(c);
    if (isNaN(digit)) {
      times.push({
        length: num,
        time: c
      });
      num = 0;
    } else {
      num = num * 10 + digit;
    }
  }
  return times;
};

export const dateIncrementer = (str: string): ((d: Date) => Date) => {
  const incrementer: Record<string, (d: Date, v: number) => void> = {
    Y: (d: Date, value: number) => d.setUTCFullYear(d.getUTCFullYear() + value),
    // no month support due to ambiguity with minutes
    D: (d: Date, value: number) => d.setUTCDate(d.getUTCDate() + value),
    H: (d: Date, value: number) => d.setUTCHours(d.getUTCHours() + value),
    M: (d: Date, value: number) => d.setUTCMinutes(d.getUTCMinutes() + value),
    S: (d: Date, value: number) => d.setUTCSeconds(d.getUTCSeconds() + value)
  };

  if (str.length < 0 || str[0] != 'P') {
    throw new Error(`invalid date periodicity: ${str}`);
  }

  const periods = parsePeriodicity(str.slice(1).replace('T', ''));

  return (date: Date) => {
    for (const period of periods) {
      incrementer[period.time](date, period.length);
    }
    return date;
  };
};

export const parseTimestamps = (wmsTime: string): Date[] => {
  const ranges = wmsTime.split(/, ?/);
  const timestamps = new Array<Date>();
  const addTimestamp = (date: Date) => {
    timestamps.push(new Date(date.getTime()));
  };

  for (const range of ranges) {
    const timeValues = range.split('/');
    const date = new Date(timeValues[0]);
    if (isNaN(date.getTime())) {
      throw new Error(
        `unable to parse start date in WMS Time range: '${timeValues[0]}' in '${range}'`
      );
    }

    addTimestamp(date);

    if (timeValues.length == 3) {
      const end = new Date(timeValues[1]);
      const next = dateIncrementer(timeValues[2]);

      while (date < end) {
        addTimestamp(next(date));
      }
    } else if (timeValues.length != 1) {
      throw new Error(
        `unable to parse WMS Time range: '${range}' in '${wmsTime}'`
      );
    }
  }

  return timestamps;
};

export const parseWMSXMLTimeRange = (
  layers: string[],
  xml: string
): Record<string, ForecastTimestamps> => {
  const doc = new DOMParser().parseFromString(xml, 'application/xml');

  const xpath = (layer: string, extent: string) => {
    const query = `//Layer[./Name='${layer}']/Extent[@name='${extent}']/text()`;
    return doc.evaluate(query, doc, null, XPathResult.STRING_TYPE).stringValue;
  };

  const timepoints: Record<string, ForecastTimestamps> = {};

  for (const layer of layers) {
    timepoints[layer] = {
      reference: new Date(xpath(layer, 'reftime')),
      validTimes: parseTimestamps(xpath(layer, 'time'))
    };
  }

  return timepoints;
};

export const fetchWMSTimes = async (
  layers: string[]
): Promise<Record<string, ForecastTimestamps>> => {
  const query = layers.join(',');

  const capabilities = '?SERVICE=WMS&REQUEST=GetCapabilities';
  const smhiDomain = 'https://wts.smhi.se/tile/';

  const url = smhiDomain + query + capabilities;
  const response = await fetch(url);

  if (response.status != 200) {
    throw new Error(
      `failed to fetch forecast times: got status '${response.statusText}'`
    );
  }

  const contentType = response.headers.get('Content-Type');
  if (contentType != 'application/xml' && contentType != 'text/xml') {
    throw new Error(
      `failed to fetch forecast times: got content type ${contentType}`
    );
  }

  const xml = await response.text();
  return parseWMSXMLTimeRange(layers, xml);
};

export const Smhi: ForecastAPI = {
  ValidTimes: fetchWMSTimes
};
