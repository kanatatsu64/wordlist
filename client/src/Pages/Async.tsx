import Loadable from 'react-loadable'

import { Loading } from 'Lib/Loading'

export const BundleTablePageAsync = Loadable({
    loader: async () => {
        const module = await import('./BundleTablePage')
        return module.BundleTablePage
    },
    loading: Loading
})

export const CardPageAsync = Loadable({
    loader: async () => {
        const module = await import('./CardPage')
        return module.CardPage
    },
    loading: Loading
})

export const TopPageAsync = Loadable({
    loader: async () => {
        const module = await import('./TopPage')
        return module.TopPage
    },
    loading: Loading
})
