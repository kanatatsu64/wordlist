import axios, { AxiosResponse } from 'axios'

import { PluginInfo, PluginID } from 'Types'
import { BaseApiURL } from 'Api/Config'

const BasePluginURL = BaseApiURL + 'plugin/'

export const loadInfo = async (pluginId: PluginID) => {
    type DataType = PluginInfo
    const res: AxiosResponse<DataType> = await axios.get(BasePluginURL + 'info/' + pluginId)
    return res.data
}
