import axios, { AxiosResponse } from 'axios'

import { Bundle, BundleID, BundleInfo, CardID, PluginID, PseudoID } from 'Types'
import * as CardApi from 'Api/Card'
import { BaseApiURL } from 'Api/Config'

const BundleApiURL = BaseApiURL + 'bundle/'

export const create = async (name: string, desc: string) => {
    const bundleid = PseudoID
    const cardids = []
    await axios.post(BundleApiURL, { bundleid, name, desc, cardids })
}

export const upload = async (bundleId: BundleID, file: File, pluginId: PluginID) => {
    const cardIds = await CardApi.upload(file, pluginId)
    await addCards(bundleId, cardIds)
}

export const addCards = async (bundleId: BundleID, cardIds: CardID[]) => {
    await axios.post(BundleApiURL + bundleId + '/cards', cardIds)
}

export const deleteCards = async (bundleId: BundleID, cardIds: CardID[]) => {
    await axios.delete(BundleApiURL + bundleId + '/cards', { data: cardIds })
}

export const update = async (bundle: Bundle) => {
    const { bundleid, name, desc, cards } = bundle
    const cardids = cards.map(card => card.cardid)
    await axios.put(BundleApiURL, { bundleid, name, desc, cardids })
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

export const delete_ = async (bundleId: BundleID) => {
    await axios.delete(BundleApiURL + bundleId)
}
