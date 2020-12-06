import axios, { AxiosResponse } from 'axios'

import { Bundle, BundleID, BundleInfo, PluginID } from 'Types'
import { BaseApiURL } from 'Api/Config'

const BundleApiURL = BaseApiURL + 'bundle/'

export const create = async (name: string, desc: string) => {
    await axios.post(BundleApiURL, null, { params: { name, desc } })
}

export const upload = async (bundleId: BundleID, file: File, pluginId: PluginID) => {
    const data = new FormData()
    data.append("csv", file)
    await axios.post(BundleApiURL + bundleId, data, { params: { plugin: pluginId } })
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
