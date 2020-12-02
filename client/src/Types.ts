import React from 'react'

export type ReactTag<P={}, S={}> = React.ComponentClass<P, S> | React.FC<P>

export type UUID = string

export type PluginID = UUID

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

export type Bundle = {
    name: string,
    desc: string,
    cards: Card[]
}

export type Card = {
    cardid: CardID,
    language: Language,
    word: string,
    meaning: string,
    attributes: string[]
}
