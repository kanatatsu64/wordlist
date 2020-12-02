import Loadable from 'react-loadable'

import { ReactTag, PluginID, Card } from 'Types'
import { Loading } from 'Lib/Loading'

/*
    export const Plugin = {
        ViewUpside: ...,
        ViewDownside: ...
    }
 */

const loadModule = async (pluginid: string): Promise<PluginModuleType> => {
    switch (pluginid) {
        case 'c2cc10e1-57d6-4b6f-9899-38d972112d8c': return import('Plugins/German/Plugin')
    }
}

export type ViewPropsType = {
    card: Card
}

export type PluginType = {
    ViewUpside: ReactTag<ViewPropsType>,
    ViewDownside: ReactTag<ViewPropsType>
}

export type PluginModuleType = {
    Plugin: PluginType
}

const cache: { [pluginid: string]: PluginModuleType} = {}

const loadModuleWithCache = async (pluginid: string): Promise<PluginModuleType> => (
    pluginid in cache ? Promise.resolve(cache[pluginid]) : loadModule(pluginid)
)

export type PluginTypeAsync = {
    ViewUpside: ReactTag<ViewPropsType> & Loadable.LoadableComponent
    ViewDownside: ReactTag<ViewPropsType> & Loadable.LoadableComponent
}

const wrap = (promise: Promise<PluginModuleType>): PluginTypeAsync => (
    {
        ViewUpside: Loadable({
            loader: async () => {
                const module = await promise
                return module.Plugin.ViewUpside
            },
            loading: Loading
        }),
        ViewDownside: Loadable({
            loader: async () => {
                const module = await promise
                return module.Plugin.ViewDownside
            },
            loading: Loading
        })
    }
)

export const loadPlugin = (pluginid: PluginID): PluginTypeAsync => {
    const promise = loadModuleWithCache(pluginid)
    const pluginAsync = wrap(promise)
    return pluginAsync
}
