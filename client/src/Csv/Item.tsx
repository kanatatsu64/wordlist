import React from 'react'

import style from './ItemStyle.scss'
import { Delete } from 'Lib/Icon'

type PropsType = {
    name: String,
    onSelect: (name: String) => void,
    onDelete: (name: String) => void
}

export const Item: React.FC<PropsType> = props => {
    const { name } = props

    const onDelete = () => props.onDelete(name)
    const onSelect = () => props.onSelect(name)

    return (
        <div className={ style.item } onClick={ onSelect }>
            <div className={ style.name }>
                <span>{ name }</span>
            </div>
            <div className={ style.control }>
                <Delete onClick={ onDelete }></Delete>
            </div>
        </div>
    )
}
