import React from 'react'

import { Card } from 'Api/Types'

type PropsType = {
    card: Card,
    onSelect: (card: Card) => void,
    onDelete: (card: Card) => void
}

export const Item: React.FC<PropsType> = props => {
    const { card, card: { word, meaning } } = props

    const onSelect = () => props.onSelect(card)
    const onDelete = () => props.onDelete(card)

    return (
        <div onClick={ onSelect }>
            <span>{ word }</span>
            <span>{ meaning }</span>
            <button onClick={ onDelete }>delete</button>
        </div>
    )
}
