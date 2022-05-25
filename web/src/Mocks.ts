import { rest } from 'msw';
import type { DefaultBodyType } from 'msw';
import { setupServer } from 'msw/native';
import { ForecastResponse } from './Forecast';

export const mockForecastResponse = (mock: ForecastResponse) => {
  return rest.get<DefaultBodyType, WTSLayersParams>(WTSURL, (req, res, ctx) => {
    const { layers } = req.params;
    const response: ForecastResponse = { forecast: {} };

    for(const layer of layers.split(',')) {
      response.forecast[layer] = mock.forecast[layer];
    }
    return res(
      ctx.json(response)
    );
  })
};

export const WTSURL = '/forecast/:layers';

type WTSLayersParams = {
  layers: string;
};

export const handlers = [
  rest.get<DefaultBodyType, WTSLayersParams>(WTSURL, (req, res, ctx) => {
    const { layers } = req.params;
    const response: ForecastResponse = { forecast: {} };
    for(const layer of layers.split(',')) {
      response.forecast[layer] = {
            validTimes: "2021-10-18T14:00:00Z/2021-10-20T12:00:00Z/PT1H, 2021-10-20T15:00:00Z/2021-10-21T00:00:00Z/PT3H, 2021-10-21T06:00:00Z/2021-10-24T00:00:00Z/PT6H, 2021-10-24T12:00:00Z/2021-10-28T00:00:00Z/PT12H",
            reference: "2021-10-18T13:00:00Z"
      }
    }

    return res(
      ctx.json(response)
    );
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
