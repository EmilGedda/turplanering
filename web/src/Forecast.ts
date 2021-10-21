type TimeType = {
  time: string;
  length: number;
};

export const parsePeriodicity = (str: string): TimeType[] => {
  const times: TimeType[] = [];
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

type Forecast<T, U = T> = {
  reference: T;
  validTimes: U;
};

export type ForecastResponse = {
  forecast: Record<string, Forecast<string>>;
};

export type ForecastTimestamps = Forecast<Date, Date[]>;

export const fetchWMSTimes = async (
  baseURL: string,
  layers: string[]
): Promise<Record<string, ForecastTimestamps>> => {
  const response = await fetch(baseURL + layers.join(','));

  if (response.status != 200) {
    throw new Error(
      `failed to fetch forecast times: got status '${response.statusText}'`
    );
  }

  const contentType = response.headers.get('Content-Type');
  if (!contentType || !contentType.startsWith('application/json')) {
    throw new Error(
      `failed to fetch forecast times: got content type ${String(contentType)}`
    );
  }

  const { forecast } = (await response.json()) as ForecastResponse;
  const timepoints: Record<string, ForecastTimestamps> = {};

  for (const key in forecast) {
    const layer = forecast[key];
    timepoints[key] = {
      reference: new Date(layer.reference),
      validTimes: parseTimestamps(layer.validTimes)
    };
  }

  return timepoints;
};

export interface ForecastAPI {
  ForecastTimes(layers: string[]): Promise<Record<string, ForecastTimestamps>>;
}

export class Smhi implements ForecastAPI {
  baseURL = 'http://localhost:4000';

  constructor(args?: Partial<Smhi>) {
    Object.assign(this, args);
  }

  ForecastTimes(layers: string[]): Promise<Record<string, ForecastTimestamps>> {
    return fetchWMSTimes(this.baseURL + '/forecast/', layers);
  }
}

// export const Smhi: ForecastAPI = {
//   ValidTimes: fetchWMSTimes
// };
