import React from 'react'

import { Bundle, Card } from 'Types'
import { Item } from 'Card/Item'

type PropsType = {
    bundle: Bundle,
    onSelect: (card: Card) => void,
    onDelete: (card: Card) => void
}

export const Table : React.FC<PropsType> = props => {
    const { bundle: { name, desc, cards } } = props

    const onSelect = props.onSelect
    const onDelete = props.onDelete

    const items = React.useMemo(() => (
        cards.map(card => (
            <li><Item card={ card } onSelect={ onSelect } onDelete={ onDelete }></Item></li>
        ))
    ), [cards])
    const title = name + (desc ? '('+ desc +')' : '')

    return (
        <div>
            <h1>{ title }</h1>
            <ul>{ items }</ul>
        </div>
    )
}
