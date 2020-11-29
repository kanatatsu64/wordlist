import React from 'react'

import { CSV } from 'Api/CSV'
import { Item } from 'Card/Item'

type PropsType = { csv: CSV, onSelect: (CSV) => void, onDelete: (CSV) => void }

export const Table : React.FC<PropsType> = props => {
    const { csv: { name, cards } } = props

    const onSelect = props.onSelect
    const onDelete = props.onDelete

    const items = React.useMemo(() => (
        cards.map(card => (
            <li><Item card={ card } onSelect={ onSelect } onDelete={ onDelete }></Item></li>
        ))
    ), [cards])

    return (
        <div>
            <h1>{ name }</h1>
            <ul>{ items }</ul>
        </div>
    )
}
