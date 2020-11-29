import axios, { AxiosResponse } from 'axios'

import { Card } from 'Api/Types'

export type CSV = {
    name: string,
    cards: Card[]
}

const BaseURL = 'http://localhost:3000/'
const BaseApiURL = BaseURL + 'api/'
const CsvApiURL = BaseApiURL + 'csv/'

export const getByName = async (name: string) => {
    type DataType = CSV
    const res: AxiosResponse<DataType> = await axios.get(CsvApiURL + name)
    return res.data
}

export const getList = async () => {
    type DataType = CSV[]
    const res: AxiosResponse<DataType> = await axios.get(CsvApiURL + 'list/csv')
    return res.data
}

export const getNameList = async () => {
    type DataType = string[]
    const res: AxiosResponse<DataType> = await axios.get(CsvApiURL + 'list/name')
    return res.data
}
