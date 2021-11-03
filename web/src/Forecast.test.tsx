import { WTSURL, server, errHandler, mockForecastResponse } from './Mocks';
import {
  parsePeriodicity,
  dateIncrementer,
  parseTimestamps,
  fetchWMSTimes,
  Smhi
} from './Forecast';

describe('parsePeriodicity', () => {
  it('parses time units properly', () => {
    expect(parsePeriodicity('1H')).toStrictEqual([{ time: 'H', length: 1 }]);

    expect(parsePeriodicity('1M')).toStrictEqual([{ time: 'M', length: 1 }]);

    expect(parsePeriodicity('2H4M')).toStrictEqual([
      { time: 'H', length: 2 },
      { time: 'M', length: 4 }
    ]);

    expect(parsePeriodicity('10Y20D30H40M50S')).toStrictEqual([
      { time: 'Y', length: 10 },
      { time: 'D', length: 20 },
      { time: 'H', length: 30 },
      { time: 'M', length: 40 },
      { time: 'S', length: 50 }
    ]);
  });
});

describe('dateIncrementer', () => {
  it("returns exception if string does not start with 'P'", () => {
    expect(() => dateIncrementer('foo')).toThrow(/invalid date periodicity.*/);
  });

  const incrementer = (str: string): Date =>
    dateIncrementer(str)(new Date(2001, 1, 1));
  it('create a incrementing function given a periodicity string', () => {
    expect(incrementer('P1Y1D')).toStrictEqual(new Date(2002, 1, 2));
    expect(incrementer('P365D')).toStrictEqual(new Date(2002, 1, 1));
    expect(incrementer('P25H15M20S')).toStrictEqual(
      new Date(2001, 1, 2, 1, 15, 20)
    );
    expect(incrementer('P1500M')).toStrictEqual(new Date(2001, 1, 2, 1));
  });
});

describe('parseTimestamps', () => {
  it('should parse single timestamp', () => {
    expect(parseTimestamps('2000-01-01Z')).toStrictEqual([
      new Date(Date.UTC(2000, 0, 1))
    ]);

    expect(parseTimestamps('2345-02-04T12:34:56Z')).toStrictEqual([
      new Date(Date.UTC(2345, 1, 4, 12, 34, 56))
    ]);
  });

  it('should parse timestamp ranges', () => {
    expect(parseTimestamps('2000-01-01Z/2000-01-03/P1D')).toStrictEqual([
      new Date(Date.UTC(2000, 0, 1)),
      new Date(Date.UTC(2000, 0, 2)),
      new Date(Date.UTC(2000, 0, 3))
    ]);

    expect(
      parseTimestamps('2000-01-01T00:00:00Z/2000-01-01T01:00:00Z/P15M')
    ).toStrictEqual([
      new Date(Date.UTC(2000, 0, 1, 0, 0)),
      new Date(Date.UTC(2000, 0, 1, 0, 15)),
      new Date(Date.UTC(2000, 0, 1, 0, 30)),
      new Date(Date.UTC(2000, 0, 1, 0, 45)),
      new Date(Date.UTC(2000, 0, 1, 1, 0))
    ]);

    expect(
      parseTimestamps('2000-01-01Z/2000-01-02/P1D, 2001-01-01Z/2001-01-02/P12H')
    ).toStrictEqual([
      new Date(Date.UTC(2000, 0, 1)),
      new Date(Date.UTC(2000, 0, 2)),
      new Date(Date.UTC(2001, 0, 1, 0)),
      new Date(Date.UTC(2001, 0, 1, 12)),
      new Date(Date.UTC(2001, 0, 2, 0))
    ]);
  });

  it('should throw error on invalid format', () => {
    expect(() => parseTimestamps('/')).toThrow(/unable to parse/);
    expect(() => parseTimestamps('///')).toThrow(/unable to parse/);
    expect(() => parseTimestamps('foo')).toThrow(/unable to parse/);
    expect(() => parseTimestamps('a/b/c')).toThrow(/unable to parse/);
    expect(() => parseTimestamps('2001/b/c/d')).toThrow(/unable to parse/);
    expect(() => parseTimestamps('2000-01-01Z,foo')).toThrow(/unable to parse/);
  });
});

describe('fetchWMSTimes', () => {
  it('should return timestamps on proper JSON', async () => {
    expect.assertions(2);
    await expect(fetchWMSTimes('/forecast/', ['layer'])).resolves.toHaveProperty('layer');
    server.use(mockForecastResponse({
      forecast: {
        'temp': {
          validTimes: '2123-01-01Z',
          reference: '2001-01-01Z'
        },
        'wind': {
          validTimes: '2002-02-02Z',
          reference: '2002-02-02Z'
        }
      }
    }));
    await expect(fetchWMSTimes('/forecast/', ['temp'])).resolves.toMatchObject({
      'temp': {
        validTimes: [new Date('2123-01-01Z')],
        reference: new Date('2001-01-01Z')
      }
    })
  });

  it('should error on abnormal response', async () => {
    expect.assertions(2);
    server.use(errHandler(WTSURL, 200, { 'Content-Type': 'foo' }));
    await expect(() => fetchWMSTimes('/forecast/', ['layer'])).rejects.toThrowError(
      /got content/
    );

    server.use(errHandler(WTSURL, 500));
    await expect(() => fetchWMSTimes('/forecast/', ['layer'])).rejects.toThrowError(
      /got status/
    );
  });
});