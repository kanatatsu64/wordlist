import axios, { AxiosResponse } from 'axios'

import { CardID, Card } from 'Types'
import { BaseApiURL } from 'Api/Config'

const BaseCardURL = BaseApiURL + 'card/'

export const load = async (cardId: CardID) => {
    type DataType = Card
    const res: AxiosResponse<DataType> = await axios.get(BaseCardURL + cardId)
    return res.data
}
