import React from 'react'

import {
    convert,
    revert,
    Part,
    Genre,
    Kind,
    Form,
    Comparative,
    Superlative,
    Attrs,
    Example,
    shortenPart,
    shortenGenre,
    shortenKind,
    GermanCard
} from './Types'
import { Card } from 'Types'
import { EditPropsType } from 'Plugin'

type Draft = {
    part: Part
    word: string
    attrs: Attrs
    meaning: string
    note: string
    examples: Example[]
}

export const Edit: React.FC<EditPropsType> = props => {
    const card = convert(props.card)

    const initDraft = React.useMemo<Draft>(() => (
        {
            part: card.part,
            word: card.word,
            attrs: [ ...card.attrs ],
            meaning: card.meaning,
            note: card.note,
            examples: card.examples.map(example => ({ ...example }))
        }
    ), [card])
    const [draft, setDraft] = React.useState(initDraft)

    const onSubmit = (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault()
        const _gcard: GermanCard = { ...card, ...draft }
        const _card: Card = revert(_gcard)
        props.update(_card)
    }

    const nounAttrs = React.useMemo(() => {
        const plural = draft.attrs[0]
        const genre: Genre = draft.attrs[1] as Genre

        const onChangePlural = (event: React.ChangeEvent<HTMLInputElement>) => {
            const input = event.target.value
            const updated: Draft = { ...draft, attrs: [input, genre]}
            setDraft(updated)
        }
        const onChangeGenre = (event: React.ChangeEvent<HTMLSelectElement>) => {
            const input = event.target.value
            const updated: Draft = { ...draft, attrs: [plural, input]}
            setDraft(updated)
        }

        return (
            <>
                <label>
                    Plural
                    <input onChange={onChangePlural} value={plural}></input>
                </label>
                <label>
                    Genre
                    <select onChange={onChangeGenre} value={genre}>
                        <option value="Male">{ shortenGenre('Male') }</option>
                        <option value="Female">{ shortenGenre('Female') }</option>
                        <option value="Neuter">{ shortenGenre('Neuter') }</option>
                    </select>
                </label>
            </>
        )
    }, [draft])

    const verbAttrs = React.useMemo(() => {
        const kind: Kind = draft.attrs[0] as Kind
        const form: Form = draft.attrs[1]

        const onChangeKind = (event: React.ChangeEvent<HTMLSelectElement>) => {
            const input: Kind = event.target.value as Kind
            let update: Draft
            switch (input) {
                case 'Intransitive':
                    update = { ...draft, attrs: [input, ""] }
                case 'Transitive':
                    update = { ...draft, attrs: [input, form] }
            }
            setDraft(update)
        }
        const onChangeForm = (event: React.ChangeEvent<HTMLInputElement>) => {
            const input: Form = event.target.value as Form
            const update: Draft = { ...draft, attrs: [kind, input] }
            setDraft(update)
        }

        return (
            <>
                <label>
                    Kind
                    <select onChange={ onChangeKind } value={ kind }>
                        <option value="Intransitive">{ shortenKind('Intransitive') }</option>
                        <option value="Transitive">{ shortenKind('Transitive') }</option>
                    </select>
                </label>
            {kind == 'Transitive' || (
                <label>
                    Form
                    <input onChange={ onChangeForm } value={ form }></input>
                </label>
            )}
            </>
        )
    }, [draft])

    const adjectiveAttrs = React.useMemo(() => {
        const comp: Comparative = draft.attrs[0]
        const sup: Superlative = draft.attrs[1]

        const onChangeComp = (event: React.ChangeEvent<HTMLInputElement>) => {
            const input: Comparative = event.target.value
            const update: Draft = { ...draft, attrs: [input, sup] }
            setDraft(update)
        }
        const onChangeSup = (event: React.ChangeEvent<HTMLInputElement>) => {
            const input: Superlative = event.target.value
            const update: Draft = { ...draft, attrs: [input, sup] }
            setDraft(update)
        }

        return (
            <>
                <label>
                    Comparative
                    <input onChange={ onChangeComp } value={ comp }></input>
                </label>
                <label>
                    Superlative
                    <input onChange={ onChangeSup } value={ sup }></input>
                </label>
            </>
        )
    }, [draft])

    const editPart = React.useMemo(() => {
        const part: Part = draft.part

        const onChangePart = (event: React.ChangeEvent<HTMLSelectElement>) => {
            const input: Part = event.target.value as Part
            const update: Draft = { ...draft, part: input }
            setDraft(update)
        }

        return (
            <label>
                Part
                <select onChange={ onChangePart } value={ part }>
                    <option value="Noun">{ shortenPart('Noun') }</option>
                    <option value="Verb">{ shortenPart('Verb') }</option>
                    <option value="Adjective">{ shortenPart('Adjective') }</option>
                    <option value="Adverb">{ shortenPart('Adverb') }</option>
                    <option value="Conjunction">{ shortenPart('Conjunction') }</option>
                </select>
            </label>
        )
    }, [draft])

    const editWord = React.useMemo(() => {
        const word: string = draft.word

        const onChangeWord = (event: React.ChangeEvent<HTMLInputElement>) => {
            const input: string = event.target.value
            const update = { ...draft, word: input }
            setDraft(update)
        }

        return (
            <label>
                Word
                <input onChange={ onChangeWord } value={ word }></input>
            </label>
        )
    }, [draft])

    const editAttrs = React.useMemo(() => {
        switch (draft.part) {
            case 'Noun': return nounAttrs
            case 'Verb': return verbAttrs
            case 'Adjective': return adjectiveAttrs
            default: return
        }
    }, [draft])

    const editMeaning = React.useMemo(() => {
        const meaning: string = draft.meaning

        const onChangeMeaning = (event: React.ChangeEvent<HTMLInputElement>) => {
            const input: string = event.target.value
            const update = { ...draft, meaning: input }
            setDraft(update)
        }

        return (
            <label>
                Meaning
                <input onChange={ onChangeMeaning } value={ meaning }></input>
            </label>
        )
    }, [draft])

    const editNote = React.useMemo(() => {
        const note: string = draft.note

        const onChangeNote = (event: React.ChangeEvent<HTMLInputElement>) => {
            const input: string = event.target.value
            const update = { ...draft, note: input }
            setDraft(update)
        }

        return (
            <label>
                Note
                <input onChange={ onChangeNote } value={ note }></input>
            </label>
        )
    }, [draft])

    return (
        <form onSubmit={ onSubmit }>
            { editPart }
            { editWord }
            { editAttrs }
            { editMeaning }
            { editNote }
            <input type="submit" value="Save"></input>
        </form>
    )
}
