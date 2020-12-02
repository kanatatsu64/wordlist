import axios, { AxiosResponse } from 'axios'

import { Bundle } from 'Types'

const BaseURL = 'http://localhost:3000/'
const BaseApiURL = BaseURL + 'api/'
const BundleApiURL = BaseApiURL + 'bundle/'

export const getByName = async (name: string) => {
    type DataType = Bundle
    const res: AxiosResponse<DataType> = await axios.get(BundleApiURL + 'name/' + name)
    return res.data
}

export const getList = async () => {
    type DataType = Bundle[]
    const res: AxiosResponse<DataType> = await axios.get(BundleApiURL + 'list')
    return res.data
}

export const getNameList = async () => {
    type DataType = string[]
    const res: AxiosResponse<DataType> = await axios.get(BundleApiURL + 'list/name')
    return res.data
}
