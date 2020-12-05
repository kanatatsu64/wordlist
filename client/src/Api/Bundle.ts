import axios, { AxiosResponse } from 'axios'

import { Bundle, BundleID, BundleInfo } from 'Types'

const BaseURL = 'http://localhost:3000/'
const BaseApiURL = BaseURL + 'api/'
const BundleApiURL = BaseApiURL + 'bundle/'

export const create = async (name: string, desc: string) => {
    await axios.post(BundleApiURL, null, { params: { name, desc } })
}

export const load = async (bundleId: BundleID) => {
    type DataType = Bundle
    const res: AxiosResponse<DataType> = await axios.get(BundleApiURL + bundleId)
    return res.data
}

export const loadInfos = async () => {
    type DataType = BundleInfo[]
    const res: AxiosResponse<DataType> = await axios.get(BundleApiURL + 'list/name')
    return res.data
}
