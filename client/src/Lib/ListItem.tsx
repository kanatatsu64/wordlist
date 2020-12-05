import React from 'react'

import style from './ListItemStyle.scss'

type PropsType = { }

export const ListItem: React.FC<PropsType> = props => {
    return (
        <li className={ style.item }>
            { props.children }
        </li>
    )
}
