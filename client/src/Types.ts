import React from 'react'

export type ReactTag<P={}, S={}> = React.ComponentClass<P, S> | React.FC<P>

export type UUID = string

export type PluginID = UUID

export type BundleID = UUID

export type CardID = UUID

export type Language =
    'Japanese' |
    'English' |
    'Chinese' |
    'French' |
    'German'

export const shorten = (language: Language) => {
    switch (language) {
        case 'Japanese': return 'JP'
        case 'English': return 'EN'
        case 'Chinese': return 'CH'
        case 'French': return 'FR'
        case 'German': return 'GR'
    }
    return ''
}

export type Example = {
    original: string
    translation: string
}

export type Bundle = {
    bundleid: BundleID
    name: string
    desc: string
    cards: Card[]
}

export type BundleInfo = {
    bundleid: BundleID
    name: string
    desc: string
}

export type Card = {
    cardid: CardID
    pluginid: PluginID
    language: Language
    word: string
    meaning: string
    attrs: string[]
    note: string
    examples: Example[]
}

export type PluginInfo = {
    pluginid: PluginID
    name: string
    desc: string
}
