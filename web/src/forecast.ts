export type ForecastTimestamps = {
    reference: Date
    validTimes: Date[]
}

export type ForecastAPI = {
    ValidTimes(): Promise<ForecastTimestamps>
}

const fetchTimes = async (): Promise<ForecastTimestamps> => {
    const api = {
        category: "pmp3g",
        version: 2
    }

    const url = "https://opendata-download-metfcst.smhi.se" +
        `/api/category/${api.category}/version/${api.version}`

    const requests = ["/validtime.json", "/approvedtime.json"].map(ep => fetch(url + ep))
    const [validTimeResponse, approvedTimeResponse] = await Promise.allSettled(requests)

    // TODO: check encoding == application/json
    if (approvedTimeResponse.status == "rejected") {
        throw new Error("failed to fetch approved time for weather")
    }

    if (validTimeResponse.status == "rejected") {
        throw new Error("failed to fetch approved time for weather")
    }

    const [validTimesJson, approvedTimeJson]
        = await Promise.all([validTimeResponse.value.json() as Promise<{validTime: string[]}>
            , approvedTimeResponse.value.json() as Promise<{referenceTime: string}>])

    return {
        reference: new Date(approvedTimeJson.referenceTime),
        validTimes: validTimesJson.validTime.map(d => new Date(d))
    }
}

export const Smhi: ForecastAPI = {
    ValidTimes: fetchTimes
}
