import React from 'react'
import style from '.ListStyle.scss'

type PropsType = {
    records: [{
        item: React.ReactElement,
        key: any
    }]
}

export const List: React.FC<PropsType> = props => {
    const { records } = props

    return (
        <ul className={ style.list }>
            {records.map(record => {
                const { item, key } = record
                return (
                    <li className={ style.item } key={ key }>{ item }</li>
                )
            })}
        </ul>
    )
}
