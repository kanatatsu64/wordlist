import Loadable from 'react-loadable'

import { Loading } from 'Lib/Loading'

export const BundlePageAsync = Loadable({
    loader: async () => {
        const module = await import('./BundlePage')
        return module.BundlePage
    },
    loading: Loading
})

export const CardPageAsync = Loadable({
    loader: async () => {
        const module = await import ('./CardPage')
        return module.CardPage
    },
    loading: Loading
})

export const LearnPageAsync = Loadable({
    loader: async () => {
        const module = await import('./LearnPage')
        return module.LearnPage
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
