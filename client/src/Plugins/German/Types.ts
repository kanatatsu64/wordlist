import { Card, CardID } from 'Types'

export const uuid = 'c2cc10e1-57d6-4b6f-9899-38d972112d8c'

export type Part =
    'Noun' |
    'Verb' |
    'Adjective' |
    'Adverb' |
    'Conjunction'

export const convertPart = (part: string): Part => {
    switch (part) {
        case 'N.': return 'Noun'
        case 'V.': return 'Verb'
        case 'Adj.': return 'Adjective'
        case 'Adv.': return 'Adverb'
        case 'Kon.': return 'Conjunction'
    }
}

export const revertPart = (part: Part): string => {
    switch (part) {
        case 'Noun': return 'N.'
        case 'Verb': return 'V.'
        case 'Adjective': return 'Adj.'
        case 'Adverb': return 'Adv.'
        case 'Conjunction': return 'Kon.'
    }
}

export const shortenPart = (part: Part): string => {
    switch (part) {
        case 'Noun': return 'N.'
        case 'Verb': return 'V.'
        case 'Adjective': return 'Adj.'
        case 'Adverb': return 'Adv.'
        case 'Conjunction': return 'Kon.'
    }
}

export type Plural = string

export type Genre = 'Male' | 'Female' | 'Neuter'

export const convertGenre = (genre: string): Genre => {
    switch (genre) {
        case 'M.': return 'Male'
        case 'F.': return 'Female'
        case 'N.': return 'Neuter'
    }
}

export const revertGenre = (genre: Genre): string => {
    switch (genre) {
        case 'Male': return 'M.'
        case 'Female': return 'F.'
        case 'Neuter': return 'N.'
    }
}

export const shortenGenre = (genre: Genre): string => {
    switch (genre) {
        case 'Male': return 'M.'
        case 'Female': return 'F.'
        case 'Neuter': return 'N.'
    }
}

export type Kind = 'Intransitive' | 'Transitive'

export const convertKind = (kind: string): Kind => {
    switch (kind) {
        case 'I.': return 'Intransitive'
        case 'T.': return 'Transitive'
    }
}

export const revertKind = (kind: Kind): string => {
    switch (kind) {
        case 'Intransitive': return 'I.'
        case 'Transitive': return 'T.'
    }
}

export const shortenKind = (kind: Kind): string => {
    switch (kind) {
        case 'Intransitive': return 'I.'
        case 'Transitive': return 'T.'
    }
}

export type Form = string

export type Comparative = string
export type Superlative = string

export type Example = {
    original: string
    translation: string
}

export type Attrs = [string, string]

export type GermanCard = {
    cardid: CardID
    part: Part
    word: string
    attrs: Attrs
    meaning: string
    note: string
    examples: Example[]
}

export const convert = (card: Card): GermanCard => {
    const {
        cardid,
        language,
        word,
        meaning,
        attrs,
        note,
        examples
    } = card

    const part = convertPart(attrs[0])
    const _attrs = attrs.slice(1, 3) as [string, string]

    return {
        cardid,
        part,
        word,
        attrs: _attrs,
        meaning,
        note,
        examples
    }
}

export const revert = (card: GermanCard): Card => {
    const {
        cardid,
        part,
        word,
        attrs,
        meaning,
        note,
        examples
    } = card

    const _part = revertPart(part)
    const _attrs = [_part, ...attrs]

    return {
        cardid,
        pluginid: uuid,
        language: 'German',
        word,
        attrs: _attrs,
        meaning,
        note,
        examples
    }
}

export type NounAttrs = [Plural | null, Genre]

export const convertNounAttrs = (attrs: Attrs): NounAttrs => {
    const [pl, gen] = attrs
    const gen_ = convertGenre(gen)
    return pl == '' ? [null, gen_] : [pl, gen_]
}

export type VerbAttrs = [Kind, Form | null]

export const convertVerbAttrs = (attrs: Attrs): VerbAttrs => {
    const [kind, form] = attrs
    const kind_ = convertKind(kind)
    switch (kind_) {
        case 'Intransitive': return [kind_, null]
        case 'Transitive': return [kind_, form]
    }
}

export type AdjectiveAttrs = [Comparative, Superlative]

export const convertAdjectiveAttrs = (attrs: Attrs): AdjectiveAttrs => {
    const [comp, sup] = attrs
    return [comp, sup]
}

export type AdverbAttrs = null

export const convertAdverbAttrs = (attrs: Attrs): AdverbAttrs => {
    return null
}

export type ConjunctionAttrs = null

export const convertConjunctionAttrs = (attrs: Attrs): ConjunctionAttrs => {
    return null
}

export const buildPartAttr = (card: GermanCard): string => {
    const { part } = card

    const buildVerbPartAttr = (card: GermanCard): string => {
        const { attrs: [kind] } = card
        const kind_: Kind = convertKind(kind)
        return shortenKind(kind_)
    }

    return part == 'Verb' ? buildVerbPartAttr(card) : shortenPart(part)
}
