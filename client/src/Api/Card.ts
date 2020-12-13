import axios, { AxiosResponse } from 'axios'

import { CardID, Card, PluginID } from 'Types'
import { BaseApiURL } from 'Api/Config'

const BaseCardURL = BaseApiURL + 'card/'

export const load = async (cardId: CardID) => {
    type DataType = Card
    const res: AxiosResponse<DataType> = await axios.get(BaseCardURL + cardId)
    return res.data
}

export const upload = async (file: File, pluginId: PluginID) => {
    type DataType = CardID[]
    const data = new FormData()
    data.append("csv", file)
    const res: AxiosResponse<DataType> = await axios.post(BaseCardURL + 'list', data, { params: { plugin: pluginId } })
    return res.data
}

export const update = async (card: Card) => {
    await axios.put(BaseCardURL, card)
}

export const delete_ = async (cardId: CardID) => {
    await axios.delete(BaseCardURL + cardId)
}
